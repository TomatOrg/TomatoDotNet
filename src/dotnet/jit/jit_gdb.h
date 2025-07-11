#pragma once

#include <stddef.h>

#include "jit_codegen.h"

void jit_gdb_register(
    jit_codegen_entry_t* funcs,
    void* code, size_t code_size
);
