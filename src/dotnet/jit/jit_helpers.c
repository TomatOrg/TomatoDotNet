#include "jit_helpers.h"

#include <stdbool.h>
#include <dotnet/gc/gc.h>
#include <tomatodotnet/types/basic.h>
#include <util/defs.h>
#include <util/except.h>

void* memcpy(void* dest, const void* src, size_t n);

static void* bzero(void* s, size_t n) {
    return __builtin_memset(s, 0, n);
}


static void* gc_bzero(void* dest, int c, size_t n) {
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

static void jit_throw(Object exception) {
    if (exception == NULL) {
        TRACE("jit_throw: <null exception>");
    } else {
        TRACE("jit_throw: %T", exception->VTable->Type);
    }
}

static Object jit_new(RuntimeTypeInfo type) {
    return gc_new(type, type->HeapSize);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Helper abstraction
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct jit_helper {
    spidir_function_t function;
    void* func;
    bool created;
} jit_helper_t;

static jit_helper_t m_jit_helpers[] = {
    [JIT_HELPER_BZERO] = { .func = bzero },
    [JIT_HELPER_MEMCPY] = { .func = memcpy },
    [JIT_HELPER_GC_BZERO] = { .func = gc_bzero },
    [JIT_HELPER_GC_MEMCPY] = { .func = gc_memcpy },
    [JIT_HELPER_THROW] = { .func = jit_throw },
    [JIT_HELPER_GC_NEW] = { .func = jit_new },
};

spidir_function_t jit_helper_get(spidir_module_handle_t module, jit_helper_type_t helper) {
    if (m_jit_helpers[helper].created) {
        return m_jit_helpers[helper].function;
    }

    switch (helper) {
        case JIT_HELPER_BZERO:
            m_jit_helpers[helper].function = spidir_module_create_extern_function(module,
                "jit_bzero", SPIDIR_TYPE_PTR, 2,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR, SPIDIR_TYPE_I64 }); break;

        case JIT_HELPER_MEMCPY:
            m_jit_helpers[helper].function = spidir_module_create_extern_function(module,
                "jit_memcpy", SPIDIR_TYPE_PTR, 3,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR, SPIDIR_TYPE_PTR, SPIDIR_TYPE_I64 }); break;

        case JIT_HELPER_GC_BZERO:
            m_jit_helpers[helper].function = spidir_module_create_extern_function(module,
                "jit_gc_bzero", SPIDIR_TYPE_PTR, 2,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR, SPIDIR_TYPE_I64 }); break;

        case JIT_HELPER_GC_MEMCPY:
            m_jit_helpers[helper].function = spidir_module_create_extern_function(module,
                "jit_gc_memcpy", SPIDIR_TYPE_PTR, 3,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR, SPIDIR_TYPE_PTR, SPIDIR_TYPE_I64 }); break;

        case JIT_HELPER_THROW:
            m_jit_helpers[helper].function = spidir_module_create_extern_function(module,
                "jit_throw", SPIDIR_TYPE_NONE, 1,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR }); break;

        case JIT_HELPER_GC_NEW:
            m_jit_helpers[helper].function = spidir_module_create_extern_function(module,
                "jit_gc_new", SPIDIR_TYPE_PTR, 1,
                (spidir_value_type_t[]){ SPIDIR_TYPE_PTR }); break;

        default:
            ASSERT(!"Invalid jit helper");
    }

    return m_jit_helpers[helper].function;
}

void* jit_helper_get_ptr(spidir_function_t function) {
    for (int i = 0; i < ARRAY_LENGTH(m_jit_helpers); i++) {
        if (m_jit_helpers[i].created && function.id == m_jit_helpers[i].function.id) {
            return m_jit_helpers[i].func;
        }
    }
    return NULL;
}

void jit_helper_clean() {
    for (int i = 0; i < ARRAY_LENGTH(m_jit_helpers); i++) {
        m_jit_helpers[i].created = false;
    }
}
