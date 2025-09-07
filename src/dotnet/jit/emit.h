#pragma once

#include <spidir/module.h>

#include "function.h"

typedef tdn_err_t (*emit_instruction_t)(
    jit_function_t* function,
    spidir_builder_handle_t builder,
    jit_block_t* block,
    tdn_il_inst_t* inst,
    jit_stack_value_t* stack
);

extern emit_instruction_t g_emit_dispatch_table[];
extern size_t g_emit_dispatch_table_size;

tdn_err_t emitter_on_entry_block(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block);

tdn_err_t emitter_on_block_fallthrough(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* from, jit_block_t* block);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Useful spidir helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

spidir_value_type_t jit_get_spidir_ret_type(RuntimeMethodBase method);
spidir_value_type_t* jit_get_spidir_arg_types(RuntimeMethodBase method);
