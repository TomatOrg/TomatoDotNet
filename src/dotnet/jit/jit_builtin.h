#pragma once

#include <spidir/module.h>
#include <tomatodotnet/except.h>
#include <tomatodotnet/types/reflection.h>

typedef spidir_value_t (*jit_builtin_emitter_t)(
    spidir_builder_handle_t handle,
    RuntimeMethodBase method,
    spidir_value_t* args
);

jit_builtin_emitter_t jit_get_builtin_emitter(RuntimeMethodBase method);

typedef struct jit_builtin_context {
    RuntimeMethodBase method;
    tdn_err_t err;
} jit_builtin_context_t;

void jit_emit_builtin(spidir_builder_handle_t handle, void* ctx);
