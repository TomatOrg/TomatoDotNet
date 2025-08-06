#pragma once

#include <stddef.h>

#include "codegen.h"
#include "spidir/codegen.h"

void jit_elf_start_emit();
void jit_elf_add_entry(jit_codegen_entry_t* entry);
void jit_elf_emit();
