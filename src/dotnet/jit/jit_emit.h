#pragma once

#include <tomatodotnet/except.h>

#include "jit_internal.h"

/**
 * Initialize the emit backend
 */
tdn_err_t jit_init_emit();

/**
 * Queue a type for vtable fixes
 */
tdn_err_t jit_queue_emit_method(spidir_module_handle_t module, RuntimeMethodBase method);

/**
 * Wait until emitting is finished, only call this once you finished
 * queueing all of the methods required
 */
tdn_err_t jit_emit(spidir_module_handle_t module);

/**
 * Clean everything to do with the jit init
 */
void jit_emit_clean(void);

spidir_value_type_t jit_get_spidir_type(RuntimeTypeInfo type);
spidir_value_type_t jit_get_spidir_ret_type(RuntimeMethodBase method);
spidir_value_type_t* jit_get_spidir_arg_types(RuntimeMethodBase method);
