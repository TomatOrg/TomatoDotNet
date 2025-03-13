#pragma once
#include <spidir/module.h>
#include <tomatodotnet/except.h>
#include <tomatodotnet/types/reflection.h>

/**
 * Initialize the basic codegen
 */
void jit_codegen_init(void);

/**
 * Queue a method to be emitted
 */
void jit_codegen_queue(RuntimeMethodBase method, spidir_function_t function);

/**
 * Perform the full codegen
 */
tdn_err_t jit_codegen(spidir_module_handle_t module);

/**
 * Cleanup after the codegen
 */
void jit_codgen_cleanup();
