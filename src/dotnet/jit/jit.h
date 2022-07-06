#pragma once

#include "../types.h"

#include <util/except.h>

/**
 * Initialize the jit itself
 */
err_t init_jit();

/**
 * Fully jit a type, and all the types that reference this type
 */
err_t jit_type(System_Type method);

/**
 * Get the global MIR context, should be used to add externs to the runtime
 */
MIR_context_t jit_get_mir_context();

/**
 * Release the global mir context
 */
void jit_release_mir_context();

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
