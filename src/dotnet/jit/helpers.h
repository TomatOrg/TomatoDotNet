#pragma once

#include <spidir/module.h>
#include <tomatodotnet/types/type.h>

typedef enum jit_helper_type {
    JIT_HELPER_BZERO,
    JIT_HELPER_MEMCPY,

    JIT_HELPER_GC_BZERO,
    JIT_HELPER_GC_MEMCPY,

    JIT_HELPER_THROW,
    JIT_HELPER_THROW_OUT_OF_MEMORY,
    JIT_HELPER_THROW_NULL_REFERENCE,
    JIT_HELPER_THROW_INDEX_OUT_OF_RANGE,
    JIT_HELPER_THROW_OVERFLOW,
    JIT_HELPER_THROW_INVALID_CAST,

    JIT_HELPER_NEWOBJ,
    JIT_HELPER_NEWSTR,
    JIT_HELPER_NEWARR,

    JIT_HELPER_GET_INTERFACE_VTABLE,

    JIT_HELPER_F32_REM,
    JIT_HELPER_F64_REM,
} jit_helper_type_t;

/**
 * Get (or create) a helper for the given helper type
 */
spidir_funcref_t jit_get_helper(spidir_module_handle_t module, jit_helper_type_t helper);

/**
 * Get the helper pointer (assuming its an helper) or NULL if not found
 */
void* jit_get_helper_ptr(spidir_funcref_t function);

/**
 * Get the helper pointer (assuming its an helper) or NULL if not found
 */
const char* jit_get_helper_name(spidir_funcref_t function);

/**
 * Get the method that the thunk belongs to
 */
RuntimeMethodBase jit_get_thunk_method(spidir_funcref_t function);

/**
 * Generate a static delegate thunk for the given method
 */
spidir_funcref_t jit_generate_static_delegate_thunk(spidir_module_handle_t module, RuntimeMethodBase method);

/**
 * Clean all the helpers for this run
 */
void jit_clean_helpers(void);
