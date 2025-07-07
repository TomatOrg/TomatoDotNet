#include "jit_native.h"

#include "jit_function.h"
#include "jit_type.h"
#include "dotnet/loader.h"
#include "tomatodotnet/types/type.h"
#include "util/except.h"
#include "util/string.h"

typedef struct native_function {
    const char* namespace;
    const char* class;
    const char* name;
    void* function;
    jit_stack_value_kind_t* args;
} native_function_t;

static double math_sqrt_double(double value) {
    return __builtin_sqrt(value);
}

static uint32_t bit_operations_leading_zero_count_i32(uint32_t value) {
    return __builtin_clz(value | 1);
}

// TODO: throw exceptions or something
static void runtime_helpers_initialize_array(Array array, RuntimeFieldInfo* field_handle) {
    RuntimeFieldInfo field = *field_handle;
    ASSERT(field->Attributes.Static);
    ASSERT(field->JitFieldPtr != NULL);

    // ensure that the size of the array is the same as the data size in the struct
    RuntimeTypeInfo array_type = array->VTable->Type->ElementType;
    ASSERT(field->FieldType->StackSize == array->Length * array_type->StackSize);

    // and now copy it
    size_t data_offset = jit_get_array_elements_offset(array_type);
    memcpy((void*)array + data_offset, field->JitFieldPtr, field->FieldType->StackSize);
}

// helper to define the signature in a nicer way
#define MAKE_SIG(x) JIT_KIND_##x
#define SIG(...) \
    (jit_stack_value_kind_t[]){ MAP(MAKE_SIG, COMMA, ## __VA_ARGS__, UNKNOWN) }

static native_function_t m_native_functions[] = {
    {
        "System", "Math", "Sqrt",
        math_sqrt_double,
        SIG(FLOAT)
    },
    {
        "System.Numerics", "BitOperations", "LeadingZeroCount",
        bit_operations_leading_zero_count_i32,
        SIG(INT32)
    },
    {
        "System.Runtime.CompilerServices", "RuntimeHelpers", "InitializeArray",
        runtime_helpers_initialize_array,
        SIG(OBJ_REF, VALUE_TYPE)
    },
};

void* jit_get_native_method(RuntimeMethodInfo info) {
    // only go here for native implemented methods
    if (info->MethodImplFlags.CodeType != TDN_METHOD_IMPL_CODE_TYPE_NATIVE) {
        return NULL;
    }

    // only if its the corelib
    if (info->Module->Assembly != gCoreAssembly) {
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
        for (jit_stack_value_kind_t arg = func->args[0]; arg != JIT_KIND_UNKNOWN; arg = func->args[++arg_count]) {
            if (info->Parameters->Length < arg_count) {
                break;
            }

            // compare the kind
            jit_stack_value_kind_t kind = jit_get_type_kind(info->Parameters->Elements[arg_count]->ParameterType);
            if (kind != func->args[arg_count]) {
                TRACE("FAILED AT %d", arg_count);
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
