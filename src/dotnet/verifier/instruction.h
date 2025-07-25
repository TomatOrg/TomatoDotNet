#pragma once

#include "internal.h"
#include "tomatodotnet/disasm.h"
#include "util/defs.h"

typedef tdn_err_t (*verify_instruction_t)(
    function_t* function,
    block_t* block,
    tdn_il_inst_t* inst,
    stack_value_t* stack
);

extern verify_instruction_t g_verify_dispatch_table[];
extern size_t g_verify_dispatch_table_size;
