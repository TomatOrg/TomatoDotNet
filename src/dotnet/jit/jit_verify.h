#pragma once

#include <tomatodotnet/disasm.h>
#include <tomatodotnet/except.h>

#include "jit_basic_block.h"
#include "jit_emit.h"
#include "jit_function.h"

#define JIT_VERBOSE_VERIFY

typedef tdn_err_t (*verify_instruction_t)(
    jit_function_t* function,
    jit_block_t* block,
    tdn_il_inst_t* inst,
    jit_stack_item_t* stack
);

extern verify_instruction_t g_verify_dispatch_table[];
extern size_t g_verify_dispatch_table_size;

tdn_err_t verifier_on_block_fallthrough(jit_function_t* function, jit_block_t* from, jit_block_t* block);
