#pragma once

#include <spidir/module.h>
#include <tomatodotnet/jit/jit.h>

/**
 * Lookup for the spidir function from the method
 */
spidir_function_t jit_get_function(spidir_module_handle_t module, RuntimeMethodBase method);

/**
 * Initialize anything that needs to be initialized
 */
tdn_err_t tdn_jit_init();
