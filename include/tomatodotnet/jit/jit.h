#pragma once

#include "tomatodotnet/except.h"
#include "tomatodotnet/types/reflection.h"

/**
 * Jit a specific method, this is usually needed when you want to call a non-virtual method
 * be it static or instance.
 */
tdn_err_t tdn_jit_method(RuntimeMethodBase methodInfo);

/**
 * Jit a type instance, this is needed when an instance of a type is created to make sure
 * that the vtable/itables are filled properly
 */
tdn_err_t tdn_jit_type(RuntimeTypeInfo type);
