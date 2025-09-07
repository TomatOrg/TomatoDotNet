#pragma once

#include <stddef.h>

#include "codegen.h"

void jit_gdb_register(
    jit_codegen_entry_t* funcs,
    void* code, size_t code_size
);
