#pragma once

#include <tomatodotnet/except.h>

#include "jit_internal.h"

/**
 * Initialize the emit backend
 */
tdn_err_t jit_init_emit();

/**
 * Queue the method to get emitted
 */
void jit_queue_emit(jit_method_t* method);

/**
 * Queue an external method
 */
void jit_queue_emit_extern(jit_method_t* method);

/**
 * Queue a type for vtable fixes
 */
void jit_queue_emit_type(RuntimeTypeInfo type);

/**
 * Wait until emitting is finished, only call this once you finished
 * queueing all of the methods required
 */
tdn_err_t jit_emit(void);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit emitting helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

spidir_value_type_t jit_get_spidir_ret_type(RuntimeMethodBase method);
spidir_value_type_t* jit_get_spidir_arg_types(RuntimeMethodBase method);
