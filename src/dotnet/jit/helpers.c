#include "helpers.h"

#include <stdbool.h>
#include <tomatodotnet/types/basic.h>
#include <util/defs.h>
#include <util/except.h>
#include "tomatodotnet/util/stb_ds.h"
#include <util/string_builder.h>

#include "jit.h"
#include "emit.h"
#include "dotnet/types.h"
#include "tomatodotnet/tdn.h"

#undef memcpy
void* memcpy(void* dest, const void* src, size_t n);

static void* bzero(void* s, size_t n) {
    return __builtin_memset(s, 0, n);
}


static void* gc_bzero(void* dest, size_t n) {
    void** d = dest;
    n /= sizeof(void*);
    while (n--) {
        *d++ = NULL;
    }
    return dest;
}

static void* gc_memcpy(void* dest, const void* src, size_t n) {
    void** d = dest;
    void* const* s = src;
    n /= sizeof(void*);
    while (n--) {
        *d++ = *s++;
    }
    return dest;
}

static void jit_throw(Exception exception) {
    if (exception == NULL) {
        ASSERT(!"jit_throw: <null exception>");
    } else {
        ASSERT(!"jit_throw", "%U", exception->Message);
    }
}

static void jit_throw_out_of_memory(void) {
    ASSERT(!"jit_throw_out_of_memory");
}

static void jit_throw_null_reference(void) {
    ASSERT(!"jit_throw_null_reference");
}

static void jit_throw_index_out_of_range(void) {
    ASSERT(!"jit_throw_index_out_of_range");
}

static void jit_throw_overflow(void) {
    ASSERT(!"jit_throw_overflow");
}

static void jit_throw_invalid_cast(void) {
    ASSERT(!"jit_throw_invalid_cast");
}

static Object jit_newobj(RuntimeTypeInfo type) {
    size_t size = type->HeapSize;
    if (tdn_type_is_valuetype(type)) {
        size += tdn_get_boxed_value_offset(type);
    }
    void* ptr = tdn_gc_new(type, size);
    if (ptr == NULL) {
        jit_throw_out_of_memory();
    }
    return ptr;
}

static Object jit_newstr(uint32_t char_count) {
    void* ptr = tdn_gc_new(tString, sizeof(struct String) + (int64_t)char_count * 2);
    if (ptr == NULL) {
        jit_throw_out_of_memory();
    }
    return ptr;
}

static Object jit_newarr(RuntimeTypeInfo arrType, int64_t num_elements) {
    RuntimeTypeInfo element_type = arrType->ElementType;

    // calculate the total length, make sure to take into account overflows
    size_t total_length;
    if (num_elements < 0) jit_throw_overflow();
    if (num_elements > INT32_MAX) jit_throw_out_of_memory();
    if (__builtin_mul_overflow(num_elements, element_type->StackSize, &total_length)) jit_throw_out_of_memory();
    if (__builtin_add_overflow(total_length, ALIGN_UP(sizeof(struct Array), element_type->StackAlignment), &total_length)) jit_throw_out_of_memory();

    // and allocate it
    void* ptr = tdn_gc_new(arrType, total_length);
    if (ptr == NULL) {
        jit_throw_out_of_memory();
    }
    return ptr;
}

static void* jit_get_interface_vtable(Object object, RuntimeTypeInfo to_type) {
    RuntimeTypeInfo from_type = object->VTable->Type;
    int idx = hmgeti(from_type->InterfaceImpls, to_type);
    ASSERT(idx >= 0, "Invalid jit_get_interface_vtable(%T, %T)", from_type, to_type);
    int offset = from_type->InterfaceImpls[idx].value;
    return &object->VTable->Functions[offset];
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Helper abstraction
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct jit_helper {
    spidir_funcref_t function;
    const char* name;
    void* func;
    bool created;
} jit_helper_t;

#define JIT_HELPER(_func) \
    { .func = _func, .name = #_func }

static jit_helper_t m_jit_helpers[] = {
    [JIT_HELPER_BZERO] = JIT_HELPER(bzero),
    [JIT_HELPER_MEMCPY] = JIT_HELPER(memcpy),
    [JIT_HELPER_GC_BZERO] = JIT_HELPER(gc_bzero),
    [JIT_HELPER_GC_MEMCPY] = JIT_HELPER(gc_memcpy),
    [JIT_HELPER_THROW] = JIT_HELPER(jit_throw),
    [JIT_HELPER_THROW_OUT_OF_MEMORY] = JIT_HELPER(jit_throw_out_of_memory),
    [JIT_HELPER_THROW_NULL_REFERENCE] = JIT_HELPER(jit_throw_null_reference),
    [JIT_HELPER_THROW_INDEX_OUT_OF_RANGE] = JIT_HELPER(jit_throw_index_out_of_range),
    [JIT_HELPER_THROW_OVERFLOW] = JIT_HELPER(jit_throw_overflow),
    [JIT_HELPER_THROW_INVALID_CAST] = JIT_HELPER(jit_throw_invalid_cast),
    [JIT_HELPER_NEWOBJ] = JIT_HELPER(jit_newobj),
    [JIT_HELPER_NEWSTR] = JIT_HELPER(jit_newstr),
    [JIT_HELPER_NEWARR] = JIT_HELPER(jit_newarr),
    [JIT_HELPER_GET_INTERFACE_VTABLE] = JIT_HELPER(jit_get_interface_vtable),
};

spidir_funcref_t jit_get_helper(spidir_module_handle_t module, jit_helper_type_t helper) {

    if (m_jit_helpers[helper].created) {
        return m_jit_helpers[helper].function;
    }

    spidir_extern_function_t func;
    switch (helper) {
        case JIT_HELPER_BZERO:
            func = spidir_module_create_extern_function(module,
                "jit_bzero", SPIDIR_TYPE_PTR, 2,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR, SPIDIR_TYPE_I64 }); break;

        case JIT_HELPER_MEMCPY:
            func = spidir_module_create_extern_function(module,
                "jit_memcpy", SPIDIR_TYPE_PTR, 3,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR, SPIDIR_TYPE_PTR, SPIDIR_TYPE_I64 }); break;

        case JIT_HELPER_GC_BZERO:
            func = spidir_module_create_extern_function(module,
                "jit_gc_bzero", SPIDIR_TYPE_PTR, 2,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR, SPIDIR_TYPE_I64 }); break;

        case JIT_HELPER_GC_MEMCPY:
            func = spidir_module_create_extern_function(module,
                "jit_gc_memcpy", SPIDIR_TYPE_PTR, 3,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR, SPIDIR_TYPE_PTR, SPIDIR_TYPE_I64 }); break;

        case JIT_HELPER_THROW:
            func = spidir_module_create_extern_function(module,
                "jit_throw", SPIDIR_TYPE_NONE, 1,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR }); break;

        case JIT_HELPER_THROW_OUT_OF_MEMORY:
            func = spidir_module_create_extern_function(module,
                "jit_throw_out_of_memory", SPIDIR_TYPE_NONE, 0, NULL); break;

        case JIT_HELPER_THROW_NULL_REFERENCE:
            func = spidir_module_create_extern_function(module,
                "jit_throw_null_reference", SPIDIR_TYPE_NONE, 0, NULL); break;

        case JIT_HELPER_THROW_INDEX_OUT_OF_RANGE:
            func = spidir_module_create_extern_function(module,
                "jit_throw_index_out_of_range", SPIDIR_TYPE_NONE, 0, NULL); break;

        case JIT_HELPER_THROW_OVERFLOW:
            func = spidir_module_create_extern_function(module,
                "jit_throw_overflow", SPIDIR_TYPE_NONE, 0, NULL); break;

        case JIT_HELPER_THROW_INVALID_CAST:
            func = spidir_module_create_extern_function(module,
                "jit_throw_invalid_cast", SPIDIR_TYPE_NONE, 0, NULL); break;

        case JIT_HELPER_NEWOBJ:
            func = spidir_module_create_extern_function(module,
                "jit_newobj", SPIDIR_TYPE_PTR, 1,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR }); break;

        case JIT_HELPER_NEWSTR:
            func = spidir_module_create_extern_function(module,
                "jit_newstr", SPIDIR_TYPE_PTR, 1,
                (spidir_value_type_t[]){ SPIDIR_TYPE_I32 }); break;

        case JIT_HELPER_NEWARR:
            func = spidir_module_create_extern_function(module,
                "jit_newarr", SPIDIR_TYPE_PTR, 2,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR, SPIDIR_TYPE_I64 }); break;

        case JIT_HELPER_GET_INTERFACE_VTABLE:
            func = spidir_module_create_extern_function(module,
                "jit_get_interface_vtable", SPIDIR_TYPE_PTR, 2,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR, SPIDIR_TYPE_PTR }); break;

        default:
            ASSERT(!"Invalid jit helper");
    }

    m_jit_helpers[helper].function = spidir_funcref_make_external(func);
    m_jit_helpers[helper].created = true;
    return m_jit_helpers[helper].function;
}

void* jit_get_helper_ptr(spidir_funcref_t function) {
    for (int i = 0; i < ARRAY_LENGTH(m_jit_helpers); i++) {
        if (m_jit_helpers[i].created && function.id == m_jit_helpers[i].function.id) {
            return m_jit_helpers[i].func;
        }
    }
    return NULL;
}

const char* jit_get_helper_name(spidir_funcref_t function) {
    for (int i = 0; i < ARRAY_LENGTH(m_jit_helpers); i++) {
        if (m_jit_helpers[i].created && function.id == m_jit_helpers[i].function.id) {
            return m_jit_helpers[i].name;
        }
    }
    return NULL;
}

static struct {
    spidir_funcref_t key;
    RuntimeMethodBase value;
}* m_thunk_method_lookup;


static struct {
    RuntimeMethodBase key;
    spidir_funcref_t value;
}* m_method_thunk_lookup;

RuntimeMethodBase jit_get_thunk_method(spidir_funcref_t function) {
    return hmget(m_thunk_method_lookup, function);
}

static void jit_emit_static_delegate_thunk(spidir_builder_handle_t builder, void* _ctx) {
    RuntimeMethodBase method = _ctx;
    spidir_funcref_t function = jit_get_function(spidir_builder_get_module(builder), method);

    // setup the call
    spidir_block_t entry = spidir_builder_create_block(builder);
    spidir_builder_set_block(builder, entry);
    spidir_builder_set_entry_block(builder, entry);

    // load all the arguments
    spidir_value_t* args = NULL;
    for (int i = 0; i < method->Parameters->Length; i++) {
        arrpush(args, spidir_builder_build_param_ref(builder, i + 1));
    }

    // perform the indirect call
    spidir_value_t result = spidir_builder_build_call(
        builder,
        function,
        arrlen(args), args
    );

    // and return it
    spidir_builder_build_return(builder, result);

    arrfree(args);
}

spidir_funcref_t jit_generate_static_delegate_thunk(spidir_module_handle_t module, RuntimeMethodBase method) {
    spidir_value_type_t* args = NULL;

    // check if we already generated it this round, if so return it
    int idx = hmgeti(m_method_thunk_lookup, method);
    if (idx >= 0) {
        return m_method_thunk_lookup[idx].value;
    }

    if (method->ThunkPtr != NULL) {
        ASSERT(!"Don't generate a new thunk, reuse existing one");
    }

    // build the name
    string_builder_t builder = {};
    string_builder_push_method_signature(&builder, method, true);
    string_builder_push_cstr(&builder, " [static-delegate-thunk]");
    const char* name = string_builder_build(&builder);

    // build the arg types, insert a dummy ptr to the first argument
    // to simulate the thiscall
    args = jit_get_spidir_arg_types(method);
    arrins(args, 0, SPIDIR_TYPE_PTR);

    // create the function
    spidir_function_t thunk = spidir_module_create_function(
        module,
        name,
        jit_get_spidir_ret_type(method),
        arrlen(args), args
    );
    string_builder_free(&builder);
    arrfree(args);

    // build the function
    spidir_module_build_function(
        module,
        thunk,
        jit_emit_static_delegate_thunk,
        method
    );

    spidir_funcref_t funcref = spidir_funcref_make_internal(thunk);
    hmput(m_thunk_method_lookup, funcref, method);
    hmput(m_method_thunk_lookup, method, funcref);

    return funcref;
}

void jit_clean_helpers() {
    for (int i = 0; i < ARRAY_LENGTH(m_jit_helpers); i++) {
        m_jit_helpers[i].created = false;
    }
    hmfree(m_thunk_method_lookup);
    hmfree(m_method_thunk_lookup);
}
