#pragma once

#include <stddef.h>
#include "except.h"
#include "tinydotnet/types/reflection.h"

tdn_err_t tdn_load_assembly_from_memory(const void* buffer, size_t buffer_size, RuntimeAssembly* assembly);
