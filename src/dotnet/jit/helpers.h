#pragma once

#include <spidir/module.h>
#include <tomatodotnet/types/type.h>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Runtime helpers, don't represent a C# method but a C# opcode
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Thunk generation
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Get the method that the thunk belongs to
 */
RuntimeMethodBase jit_get_thunk_method(spidir_funcref_t function);

/**
 * Generate a static delegate thunk for the given method
 */
spidir_funcref_t jit_generate_static_delegate_thunk(spidir_module_handle_t module, RuntimeMethodBase method);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Runtime intrinsic like implementations, things that are easy to implement in
// spidir but not in IL
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef spidir_value_t (*jit_builtin_emitter_t)(
    spidir_builder_handle_t builder,
    RuntimeMethodBase method,
    spidir_value_t* args
);

typedef struct jit_builtin_context {
    RuntimeMethodBase method;
    tdn_err_t err;
} jit_builtin_context_t;

/**
 * Get the emitter callback, use this to check if a method has an emitter at all
 */
jit_builtin_emitter_t jit_get_builtin_emitter(RuntimeMethodBase method);

/**
 * Emit a builtin, this is a spidir callback that takes in the jit_builtin_context_t
 */
void jit_emit_builtin(spidir_builder_handle_t builder, void* ctx);

/**
 * Clean all the helpers for this run
 */
void jit_clean_helpers(void);