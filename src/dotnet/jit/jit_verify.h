#pragma once

#include <tomatodotnet/except.h>

#include "jit_internal.h"

tdn_err_t jit_verify_prepare_method(jit_method_t* jmethod);

/**
 * Verify a specific method
 */
tdn_err_t jit_verify_method(jit_method_t* method);
