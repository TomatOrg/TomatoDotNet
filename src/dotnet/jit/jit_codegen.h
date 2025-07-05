#pragma once
#include <spidir/module.h>
#include <tomatodotnet/except.h>
#include <tomatodotnet/types/reflection.h>

#include "spidir/codegen.h"


typedef struct jit_codegen_entry {
    spidir_function_t key;
    spidir_codegen_blob_handle_t blob;
    RuntimeMethodBase method;
    bool thunk;
} jit_codegen_entry_t;

/**
 * Initialize the basic codegen
 */
void jit_codegen_init(void);

/**
 * Queue a method to be emitted
 */
void jit_codegen_queue(RuntimeMethodBase method, spidir_function_t function, bool thunk);

/**
 * Perform the full codegen
 */
tdn_err_t jit_codegen(spidir_module_handle_t module);

/**
 * Cleanup after the codegen
 */
void jit_codgen_cleanup();
