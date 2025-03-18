#pragma once

#include <spidir/module.h>
#include <tomatodotnet/jit/jit.h>

/**
 * Lookup for the spidir function from the method
 */
spidir_function_t jit_get_function(spidir_module_handle_t module, RuntimeMethodBase method);

/**
 * Get the dotnet method from the spidir function
 */
RuntimeMethodBase jit_get_method_from_function(spidir_function_t function);

/**
 * Queue a type to be initialized
 */
void jit_queue_type(spidir_module_handle_t module, RuntimeTypeInfo type);

/**
 * Initialize anything that needs to be initialized
 */
tdn_err_t tdn_jit_init();
