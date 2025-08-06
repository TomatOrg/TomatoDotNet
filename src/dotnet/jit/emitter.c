#include "emitter.h"

#include "spidir.h"
#include "util/defs.h"
#include "util/except.h"

const emit_instruction_t g_emit_dispatch_table[] = {

};
const size_t g_emit_dispatch_table_size = ARRAY_LENGTH(g_emit_dispatch_table);

tdn_err_t emitter_on_entry_block(function_t* function, spidir_builder_handle_t builder, block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;


    // if we are not coming from an inline create the entry block, otherwise
    // assume the caller is in charge of that
    if (!function->inline_depth) {
        block->jit_block = spidir_builder_create_block(builder);
        spidir_builder_set_entry_block(builder, block->jit_block);
    }
    spidir_builder_set_block(builder, block->jit_block);

    //
    // initialize and spill the arguments
    //
    for (int i = 0; i < arrlen(block->args); i++) {
        block_local_t* arg = &block->args[i];
        local_t* func_arg = &function->args[i];

        // get the param for this slot, if we are inlining assume the value
        // is already stored inside
        stack_value_t value = {};
        if (!function->inline_depth) {
            value.type = function->args->type;
            value.kind = jit_get_spidir_type(value.type);
            value.value = spidir_builder_build_param_ref(builder, i);
        } else {
            value = block->args[i].value;
        }

        // if we need to spill, spill it now
        if (function->args[i].spilled) {
            spidir_value_t stackslot = spidir_builder_build_stackslot(builder,
                func_arg->type->StackSize, func_arg->type->StackAlignment);

            // TODO: spill the value into the allocated stack slot
            CHECK_FAIL();
        }

        // and store it
        arg->value = value;
    }

cleanup:
    return err;
}

tdn_err_t emitter_on_block_fallthrough(function_t* function, spidir_builder_handle_t builder, block_t* from, block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: perform the merge

    // and branch into the new block
    spidir_builder_build_branch(builder, block->jit_block);

cleanup:
    return err;
}
