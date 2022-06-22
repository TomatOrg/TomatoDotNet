#pragma once

#include "../types.h"

#include <stddef.h>

typedef struct internal_call {
    const char* target;
    void* impl;
} internal_call_t;

extern internal_call_t g_internal_calls[];

extern size_t g_internal_calls_count;

/**
 * memcpy a struct to an heap object, taking care of any memory barrier that should happen
 * while we are copying it
 */
void managed_memcpy(System_Object this, System_Type struct_type, size_t offset, void* from);
