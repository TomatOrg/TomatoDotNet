#pragma once

#include "../types.h"

/**
 * Same as heap_find but does not check if the object is mapped
 * or not, and assumes that if it is in the range it is mapped
 */
System_Object heap_find_fast(void* ptr);
