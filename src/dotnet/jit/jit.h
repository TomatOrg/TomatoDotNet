#pragma once

#include "spidir/module.h"
#include "tomatodotnet/except.h"
#include "tomatodotnet/types/reflection.h"

tdn_err_t tdn_jit_init();

/**
 * Lookup for the spidir function from the method
 */
spidir_funcref_t jit_get_function(spidir_module_handle_t module, RuntimeMethodBase method);

/**
 * Get the dotnet method from the spidir function
 */
RuntimeMethodBase jit_get_method_from_function(spidir_funcref_t function);

/**
 * Queue a type to be initialized
 */
void jit_queue_type(spidir_module_handle_t module, RuntimeTypeInfo type);

/**
 * Queue a cctor if required
 */
void jit_queue_cctor(spidir_module_handle_t module, RuntimeTypeInfo type);
