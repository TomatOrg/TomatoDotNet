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

void jit_dump_method(System_Reflection_MethodInfo method);

void jit_dump_context();

typedef struct method_result {
    System_Exception exception;
    uintptr_t value;
} method_result_t;
