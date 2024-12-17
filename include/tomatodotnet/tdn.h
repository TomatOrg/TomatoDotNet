#pragma once

#include <stddef.h>

#include "types/reflection.h"
#include "except.h"
#include "host.h"


tdn_err_t tdn_load_assembly_from_memory(const void* buffer, size_t buffer_size, RuntimeAssembly* assembly);

tdn_err_t tdn_load_assembly_from_file(tdn_file_t file, RuntimeAssembly* assembly);
