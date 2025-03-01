#pragma once

#include <spidir/module.h>

typedef enum jit_helper_type {
    JIT_HELPER_BZERO,
    JIT_HELPER_MEMCPY,

    JIT_HELPER_GC_BZERO,
    JIT_HELPER_GC_MEMCPY,

    JIT_HELPER_MAX,
} jit_helper_type_t;

/**
 * Get (or create) a helper for the given helper type
 */
spidir_function_t jit_helper_get(spidir_module_handle_t module, jit_helper_type_t helper);

/**
 * Get the helper pointer (assuming its an helper) or NULL if not found
 */
void* jit_helper_get_ptr(spidir_function_t function);

/**
 * Clean all the helpers for this run
 */
void jit_helper_clean(void);
