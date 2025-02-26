#pragma once

#include <tomatodotnet/except.h>

#include "jit_internal.h"

/**
 * Verify a specific method
 */
tdn_err_t jit_verify_method(jit_method_t* method);
