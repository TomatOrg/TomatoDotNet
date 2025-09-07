#pragma once

#include <tomatodotnet/disasm.h>
#include <tomatodotnet/except.h>

#include "emit.h"
#include "function.h"

typedef tdn_err_t (*type_instruction_t)(
    jit_function_t* function,
    jit_block_t* block,
    tdn_il_inst_t* inst,
    jit_stack_value_t* stack
);

extern type_instruction_t g_type_dispatch_table[];
extern size_t g_type_dispatch_table_size;

tdn_err_t type_on_block_fallthrough(jit_function_t* function, jit_block_t* from, jit_block_t* block);
