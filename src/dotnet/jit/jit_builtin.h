#pragma once

#include <spidir/module.h>
#include <tomatodotnet/except.h>
#include <tomatodotnet/types/reflection.h>

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
