#pragma once

#include <spidir/module.h>

#include "function.h"
#include "tomatodotnet/disasm.h"

typedef tdn_err_t (*emit_instruction_t)(
    function_t* function,
    spidir_builder_handle_t builder,
    block_t* block,
    tdn_il_inst_t* inst,
    stack_value_t* stack
);

extern const emit_instruction_t g_emit_dispatch_table[];
extern const size_t g_emit_dispatch_table_size;

tdn_err_t emitter_on_entry_block(function_t* function, spidir_builder_handle_t builder, block_t* block);

tdn_err_t emitter_on_block_fallthrough(function_t* function, spidir_builder_handle_t builder, block_t* from, block_t* block);
