#pragma once

#include "tomatodotnet/except.h"
#include "tomatodotnet/types/reflection.h"

/**
 * Fully verify the function is safe to be jitted
 */
tdn_err_t verifier_verify_method(RuntimeMethodBase method);
