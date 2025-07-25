#pragma once

#include <tomatodotnet/types/reflection.h>

/**
 * Check the given type can access the target type
 */
bool verifier_can_access_type(RuntimeTypeInfo current_class, RuntimeTypeInfo target_class);

/**
 * Check the given type can access the field on the given instance
 */
bool verifier_can_access_field(RuntimeTypeInfo current_type, RuntimeFieldInfo field, RuntimeTypeInfo instance);

/**
 * Check the given type can access the method on the given instance
 */
bool verifier_can_access_method(RuntimeTypeInfo current_type, RuntimeMethodBase target_method, RuntimeTypeInfo instance);
