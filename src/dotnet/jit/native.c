#include "native.h"

#include <stdatomic.h>

#include "dotnet/loader.h"
#include "dotnet/types.h"
#include "tomatodotnet/tdn.h"
#include "tomatodotnet/types/type.h"
#include "util/except.h"
#include "util/string.h"

typedef struct native_function {
    const char* namespace;
    const char* class;
    const char* name;
    void* function;
    spidir_value_type_t* args;
} native_function_t;

static double math_sqrt_double(double value) {
    return __builtin_sqrt(value);
}

static String string_allocate(int len) {
    String new_str = tdn_gc_new(tString, sizeof(struct String) + len * 2);
    ASSERT(new_str != NULL);
    new_str->Length = len;
    return new_str;
}

static void buffer_bulk_move_with_write_barrier(void* _dest, void* _src, size_t len) {
    // TODO: something faster, this is done to ensure that there can't be data races
    ASSERT((len % 4) == 0);
    uint64_t* dest = (uint64_t*)_dest;
    uint64_t* src = (uint64_t*)_src;
    for (size_t i = 0; i < len / 4; i++) {
        dest[i] = src[i];
    }
}

static void buffer_memmove(void* dest, void* src, size_t len) {
    __builtin_memmove(dest, src, len);
}

static void span_helpers_clear_without_references(void* b, size_t byteLength) {
    __builtin_memset(b, 0, byteLength);
}

static void span_helpers_clear_with_references(void* b, size_t pointerSizeLength) {
    uint64_t* dest = (uint64_t*)b;
    for (size_t i = 0; i < pointerSizeLength; i++) {
        dest[i] = 0;
    }
}

static void debug_provider_fail_core(String stackTrace, String message, String detailMessage, String errorSource) {
    ASSERT(!"Debug Assertion Failed", "%U - %U - %U", message, detailMessage, errorSource);
}

static void debug_provider_write_core(String message) {
    tdn_host_printf("%U", message);
}

static uint32_t bit_operations_leading_zero_count_i32(uint32_t value) { return __builtin_clz(value | 1); }
static uint32_t bit_operations_leading_zero_count_i64(uint64_t value) { return __builtin_clzll(value | 1); }

// TODO: throw exceptions or something
static void runtime_helpers_initialize_array(Array array, RuntimeFieldInfo* field_handle) {
    RuntimeFieldInfo field = *field_handle;
    ASSERT(field->Attributes.Static);
    ASSERT(field->JitFieldPtr != NULL);

    // ensure that the size of the array is the same as the data size in the struct
    RuntimeTypeInfo array_type = array->VTable->Type->ElementType;
    ASSERT(field->FieldType->StackSize == array->Length * array_type->StackSize);

    // and now copy it
    size_t data_offset = tdn_get_array_elements_offset(array_type);
    memcpy((void*)array + data_offset, field->JitFieldPtr, field->FieldType->StackSize);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Signatures
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum arg_kind : uint8_t {
    KIND_UNKNOWN,
    KIND_INT32,
    KIND_INT64,
    KIND_NATIVE_INT,
    KIND_FLOAT,
    KIND_BY_REF,
    KIND_OBJ_REF,
    KIND_VALUE_TYPE,
} arg_kind_t;

// helper to define the signature in a nicer way
#define MAKE_SIG(x) KIND_##x
#define SIG(...) \
    (arg_kind_t[]){ MAP(MAKE_SIG, COMMA, ## __VA_ARGS__, UNKNOWN) }

static native_function_t m_native_functions[] = {
    {
        "System", "Math", "Sqrt",
        math_sqrt_double,
        SIG(FLOAT)
    },
    {
        "System", "String", "FastAllocateString",
        string_allocate,
        SIG(INT32)
    },
    {
        "System", "Buffer", "BulkMoveWithWriteBarrier",
        buffer_bulk_move_with_write_barrier,
        SIG(BY_REF, BY_REF, NATIVE_INT)
    },
    {
        "System", "Buffer", "Memmove",
        buffer_memmove,
        SIG(BY_REF, BY_REF, NATIVE_INT)
    },
    {
        "System", "SpanHelpers", "ClearWithoutReferences",
        span_helpers_clear_without_references,
        SIG(BY_REF, NATIVE_INT)
    },
    {
        "System", "SpanHelpers", "ClearWithReferences",
        span_helpers_clear_with_references,
        SIG(BY_REF, NATIVE_INT)
    },
    {
        "System.Diagnostics", "DebugProvider", "WriteCore",
        debug_provider_write_core,
        SIG(OBJ_REF)
    },
    {
        "System.Diagnostics", "DebugProvider", "FailCore",
        debug_provider_fail_core,
        SIG(OBJ_REF, OBJ_REF, OBJ_REF, OBJ_REF)
    },
    {
        "System.Numerics", "BitOperations", "LeadingZeroCount",
        bit_operations_leading_zero_count_i32,
        SIG(INT32)
    },
    {
        "System.Numerics", "BitOperations", "LeadingZeroCount",
        bit_operations_leading_zero_count_i64,
        SIG(INT64)
    },
    {
        "System.Runtime.CompilerServices", "RuntimeHelpers", "InitializeArray",
        runtime_helpers_initialize_array,
        SIG(OBJ_REF, VALUE_TYPE)
    },
};

static arg_kind_t get_type_kind(RuntimeTypeInfo type) {
    if (type == NULL) {
        return KIND_OBJ_REF;
    } else if (
        type == tBoolean ||
        type == tChar ||
        type == tSByte ||
        type == tByte ||
        type == tInt16 ||
        type == tUInt16 ||
        type == tInt32 ||
        type == tUInt32
    ) {
        return KIND_INT32;

    } else if (type == tInt64 || type == tUInt64) {
        return KIND_INT64;

    } else if (type == tDouble || type == tSingle) {
        return KIND_FLOAT;

    } else if (type == tIntPtr || type == tUIntPtr || type->IsPointer) {
        return KIND_NATIVE_INT;

    } else if (type->BaseType == tEnum) {
        return get_type_kind(type->EnumUnderlyingType);

    } else if (type->IsByRef) {
        return KIND_BY_REF;

    } else if (tdn_type_is_valuetype(type)) {
        return KIND_VALUE_TYPE;

    } else {
        ASSERT(tdn_type_is_referencetype(type));
        return KIND_OBJ_REF;
    }
}

void* jit_get_native_method(RuntimeMethodInfo info) {
    // only go here for native implemented methods
    if (info->MethodImplFlags.CodeType != TDN_METHOD_IMPL_CODE_TYPE_NATIVE) {
        return NULL;
    }

    // only if its the corelib, or we are before the corelib was initialized
    if (gCoreAssembly != NULL && info->Module->Assembly != gCoreAssembly) {
        return NULL;
    }

    // search for a native method that implements this
    // TODO: type check of the arguments
    RuntimeTypeInfo type = info->DeclaringType;
    for (int i = 0; i < ARRAY_LENGTH(m_native_functions); i++) {
        native_function_t* func = &m_native_functions[i];
        if (!tdn_compare_string_to_cstr(type->Namespace, func->namespace)) continue;
        if (!tdn_compare_string_to_cstr(type->Name, func->class)) continue;
        if (!tdn_compare_string_to_cstr(info->Name, func->name)) continue;

        size_t arg_count = 0;
        for (arg_kind_t arg = func->args[0]; arg != KIND_UNKNOWN; arg = func->args[++arg_count]) {
            if (info->Parameters->Length < arg_count) {
                break;
            }

            // compare the kind
            arg_kind_t kind = get_type_kind(info->Parameters->Elements[arg_count]->ParameterType);
            if (kind != func->args[arg_count]) {
                break;
            }
        }
        if (info->Parameters->Length != arg_count) {
            continue;
        }

        return func->function;
    }

    return NULL;
}
