#pragma once

#include "util/except.h"
#include "types.h"

/**
 * Fully fill this type, and all other types which this type references so
 * whatever we can access from it is ready to be tested
 */
err_t filler_fill_type(System_Type type);

/**
 * Initialize the stack size of this type, useful in places where you just want to know
 * the stack size of the type but don't want to start any other form of initialization
 */
err_t filler_fill_stack_size(System_Type type);
