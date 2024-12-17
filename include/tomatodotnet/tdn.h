#pragma once

#include <stddef.h>
#include "except.h"
#include "tomatodotnet/types/reflection.h"

tdn_err_t tdn_load_assembly_from_memory(const void* buffer, size_t buffer_size, RuntimeAssembly* assembly);

tdn_err_t tdn_load_assembly_from_file(tdn_file_t file, RuntimeAssembly* assembly);
