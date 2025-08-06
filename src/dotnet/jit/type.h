#pragma once

#include "util/except.h"
#include "function.h"
#include <stddef.h>

#include "tomatodotnet/disasm.h"

typedef tdn_err_t (*type_instruction_t)(
    function_t* function,
    block_t* block,
    tdn_il_inst_t* inst,
    stack_value_t* stack
);

extern const type_instruction_t g_type_dispatch_table[];
extern const size_t g_type_dispatch_table_size;

tdn_err_t type_on_block_fallthrough(function_t* function, block_t* from, block_t* block);
