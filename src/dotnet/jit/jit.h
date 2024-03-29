#pragma once

#include "../types.h"

#include <util/except.h>

/**
 * Initialize the jit itself
 */
err_t init_jit();

/**
 * Jit a type assuming it is going to be instantiated
 */
err_t jit_type(System_Type type);

/**
 * Jit a method assuming it is going to be called
 */
err_t jit_method(System_Reflection_MethodInfo method);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit API
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct jit_generic_extern_hook {
    bool(*can_gen)(System_Reflection_MethodInfo method);
    err_t(*gen)(MIR_context_t ctx, System_Reflection_MethodInfo method);
} jit_generic_extern_hook_t;

/**
 * Allows to add hooks to the generic extern method instantiation
 */
void jit_add_generic_extern_hook(jit_generic_extern_hook_t* hook);

/**
 * Add an assembly to the whitelist of assemblies that can use extern
 */
void jit_add_extern_whitelist(const char* assembly);

/**
 * Get the global MIR context, should be used to add externs to the runtime.
 *
 * NOTE: this takes the jit recursive mutex until you release it
 */
MIR_context_t jit_get_mir_context();

/**
 * Release the global mir context
 */
void jit_release_mir_context();

/**
 * Must be called when a thread is destroyed to remove all the thread
 * locals created by the thread
 */
void jit_free_thread_locals();

/**
 * Dump the MIR of a specific method
 */
void jit_dump_method(System_Reflection_MethodInfo method);

/**
 * Dump the whole global context
 */
void jit_dump_context();

typedef struct method_result {
    System_Exception exception;
    uintptr_t value;
} method_result_t;
