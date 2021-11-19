#pragma once

#include <stddef.h>

#include "type.h"

void* gc_alloc(type_t* type);

void* gc_alloc_from_token(assembly_t* assembly, token_t token);
