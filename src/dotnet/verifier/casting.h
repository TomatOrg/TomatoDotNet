#pragma once

#include "tomatodotnet/types/type.h"

/**
 * Get the underlying type (for enum returns the instance type)
 */
RuntimeTypeInfo verifier_get_underlying_type(RuntimeTypeInfo type);

/**
 * Can the given type be casted into another type
 */
bool verifier_can_cast_to(RuntimeTypeInfo this_type, RuntimeTypeInfo other_type);
