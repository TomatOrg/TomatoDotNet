#include "jit_function.h"

#include <util/except.h>
#include <util/stb_ds.h>
#include <util/string.h>
#include <util/string_builder.h>

#include "jit_type.h"
#include "jit_verify.h"

#define VarPop  (-1)
#define Pop0    0
#define Pop1    1
#define PopRef  Pop1
#define PopI    Pop1
#define PopI8   Pop1
#define PopR4   Pop1
#define PopR8   Pop1

#define VarPush     (-1)
#define Push0       0
#define Push1       1
#define PushRef     Push1
#define PushI       Push1
#define PushI8      Push1
#define PushR4      Push1
#define PushR8      Push1

typedef struct il_stack_behavior {
    int pop;
    int push;
} il_stack_behavior_t;

/**
 * Metadata for all the opcodes, used for decoding, this is turned
 * into another struct which is a bit more useful
 */
static il_stack_behavior_t m_il_stack_behavior[] = {
#define OPDEF_REAL_OPCODES_ONLY
#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) [c] = { pop, push },
#include "tomatodotnet/opcode.def"
#undef OPDEF
#undef OPDEF_REAL_OPCODES_ONLY
};

static void jit_clone_block(jit_block_t* in, jit_block_t* out) {
    out->block = in->block;

    arrsetlen(out->args, arrlen(in->args));
    memcpy(out->args, in->args, arrlen(in->args) * sizeof(*in->args));

    arrsetlen(out->locals, arrlen(in->locals));
    memcpy(out->locals, in->locals, arrlen(in->locals) * sizeof(*in->locals));

    arrsetlen(out->stack, arrlen(in->stack));
    memcpy(out->stack, in->stack, arrlen(in->stack) * sizeof(*in->stack));
}

static tdn_err_t jit_visit_basic_block(jit_function_t* verifier, jit_block_t* in_block, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = verifier->method;
    RuntimeMethodBody body = method->MethodBody;
    jit_stack_item_t* stack_items = NULL;

    // clone the block into the current frame, so we can modify it
    jit_block_t block = {};
    jit_clone_block(in_block, &block);

#ifdef JIT_VERBOSE_VERIFY
    int indent = 0;
#endif

    // get the pc
    tdn_il_inst_t inst = { .control_flow = TDN_IL_CF_FIRST };
    uint32_t pc = block.block.start;
    while (pc < block.block.end) {
        // can only parse more instructions if we had no block
        // terminating instruction
        CHECK(
            inst.control_flow == TDN_IL_CF_FIRST ||
            inst.control_flow == TDN_IL_CF_NEXT ||
            inst.control_flow == TDN_IL_CF_META ||
            inst.control_flow == TDN_IL_CF_CALL
        );

        // get the instruction
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));

#ifdef JIT_VERBOSE_VERIFY
        indent = tdn_disasm_print_start(body, pc, inst, indent);
#endif

        // normalize the instruction for easier processing
        tdn_normalize_inst(&inst);
        uint32_t current_pc = pc;
        pc += inst.length;

        // TODO: verify the stack push

        //
        // figure how much we need to pop from the stack
        //
        il_stack_behavior_t stack_behavior = m_il_stack_behavior[inst.opcode];

        if (stack_behavior.push < 0) {
            // TODO: calculate how many needed
            CHECK_FAIL();
        }

        if (stack_behavior.pop < 0) {
            switch (inst.opcode) {
                case CEE_RET: {
                    if (method->ReturnParameter->ParameterType != tVoid) {
                        stack_behavior.pop = 1;
                    } else {
                        stack_behavior.pop = 0;
                    }
                } break;

                default:
                    CHECK_FAIL("Invalid PopVar for %s", tdn_get_opcode_name(inst.opcode));
            }
        }

        arrsetlen(stack_items, stack_behavior.pop);

        //
        // pop from the stack
        //
        CHECK(arrlen(block.stack) >= arrlen(stack_items));
        for (int i = arrlen(stack_items) - 1; i >= 0; i--) {
            stack_items[i] = arrpop(block.stack);
        }

        //
        // Ensure we can push to the stack enough items
        //
        CHECK(arrlen(block.stack) + stack_behavior.push <= body->MaxStackSize);
        size_t wanted_stack_size = arrlen(block.stack) + stack_behavior.push;

        // ensure we have a verifier for this opcode
        CHECK(inst.opcode < g_verify_dispatch_table_size && g_verify_dispatch_table[inst.opcode] != NULL,
            "Unknown opcode %s", tdn_get_opcode_name(inst.opcode));
        CHECK_AND_RETHROW(g_verify_dispatch_table[inst.opcode](verifier, &block, &inst, stack_items));

        // call the emitter (if need be)
        if (builder != NULL) {
            CHECK(inst.opcode < g_emit_dispatch_table_size && g_emit_dispatch_table[inst.opcode] != NULL,
                "Unknown opcode %s", tdn_get_opcode_name(inst.opcode));
            CHECK_AND_RETHROW(g_emit_dispatch_table[inst.opcode](verifier, builder, &block, &inst, stack_items));
        }

        // ensure that the instruction was executed correctly
        CHECK(arrlen(block.stack) == wanted_stack_size);

#ifdef JIT_VERBOSE_VERIFY
        indent = tdn_disasm_print_end(body, pc, indent);
#endif
    }

    // if we have a fallthrough, then we need to merge to the next block
    if (inst.control_flow == TDN_IL_CF_NEXT || inst.control_flow == TDN_IL_CF_CALL) {
        CHECK_AND_RETHROW(verifier_on_block_fallthrough(verifier, &block, &verifier->blocks[block.block.index + 1]));
        if (builder != NULL) {
            CHECK_AND_RETHROW(emitter_on_block_fallthrough(verifier, builder, &block, &verifier->blocks[block.block.index + 1]));
        }
    }

    // last must be a valid instruction
    CHECK(
        inst.control_flow != TDN_IL_CF_FIRST &&
        inst.control_flow != TDN_IL_CF_META
    );

cleanup:
    // free the block data
    arrfree(block.args);
    arrfree(block.locals);
    arrfree(block.stack);

    arrfree(stack_items);

    return err;
}

tdn_err_t jit_visit_blocks(jit_function_t* function, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;

    // for the emitter we need a pass in here to initialize all of the block
    // locals before we enter the real entry block
    if (builder != NULL) {
        CHECK_AND_RETHROW(emitter_on_entry_block(function, builder, &function->entry_block));
    }

    // and now we need to merge with the entry block, which contains
    // all of the initial information about the function entry
    CHECK_AND_RETHROW(verifier_on_block_fallthrough(function, &function->entry_block, &function->blocks[0]));
    if (builder != NULL) {
        CHECK_AND_RETHROW(emitter_on_block_fallthrough(function, builder, &function->entry_block, &function->blocks[0]));
    }

    // and run until all blocks are verifier
    while (arrlen(function->queue) != 0) {
        jit_block_t* block = arrpop(function->queue);
        block->in_queue = false;

#ifdef JIT_VERBOSE_VERIFY
        TRACE("\tBasic block %d (IL_%04x):", block->block.index, block->block.start);
#endif

        CHECK_AND_RETHROW(jit_visit_basic_block(function, block, builder));
    }

cleanup:
    arrfree(function->queue);

    return err;
}

tdn_err_t jit_function_init(jit_function_t* function, RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure this method has a body
    CHECK(method->MethodBody != NULL);
    function->method = method;

    // find all of the basic blocks
    CHECK_AND_RETHROW(jit_find_basic_blocks(method, &function->labels));

    // setup the blocks array
    arrsetlen(function->blocks, hmlen(function->labels));
    memset(function->blocks, 0, arrlen(function->blocks) * sizeof(*function->blocks));

    // copy over the block information
    for (int i = 0; i < hmlen(function->labels); i++) {
        function->blocks[function->labels[i].value.index].block = function->labels[i].value;
    }

    jit_block_t* entry_block = &function->entry_block;

    //
    // Initialize the arguments
    //

    // handle the this parameter if non-static
    if (!method->Attributes.Static) {
        jit_block_local_t local = {
            .stack = {
                .type = jit_get_method_this_type(method),
            },
        };

        // TODO: unscoped support

        // if this is a readonly struct, then the ref is readonly as well
        if (method->DeclaringType->IsReadOnly) {
            local.stack.readonly_ref = true;
        }

        // this of a ref-struct is only non-local in its ref arguments
        if (method->DeclaringType->IsByRefStruct) {
            local.stack.readonly_ref = true;
        }

        arrpush(entry_block->args, local);
    }

    // now do the same for the parameters
    for (int i = 0; i < method->Parameters->Length; i++) {
        ParameterInfo parameter = method->Parameters->Elements[i];
        RuntimeTypeInfo type = parameter->ParameterType;
        jit_block_local_t local = {
            .stack = {
                .type = type
            },
        };

        // if this is a readonly parameter then mark it as such
        if (parameter->IsReadOnly) {
            CHECK(type->IsByRef);
            local.stack.readonly_ref = true;
        }

        // TODO: scoped references

        // references are non-local since they come from the outside
        // same is true for the ref-structs
        if (type->IsByRef) {
            local.stack.non_local_ref = true;

            if (type->ElementType->IsByRefStruct) {
                local.stack.non_local_ref_struct = true;
            }
        } else if (type->IsByRefStruct) {
            local.stack.non_local_ref_struct = true;
        }

        arrpush(entry_block->args, local);
    }

    //
    // Initialize the locals
    //

    // the entry block context
    arrsetlen(entry_block->locals, method->MethodBody->LocalVariables->Length);
    memset(entry_block->locals, 0, arrlen(entry_block->locals) * sizeof(*entry_block->locals));
    for (int i = 0; i < arrlen(entry_block->locals); i++) {
        entry_block->locals[i].stack.type = method->MethodBody->LocalVariables->Elements[i]->LocalType;
    }

    //
    // Set the global context
    //

    arrsetlen(function->locals, arrlen(entry_block->locals));
    memset(function->locals, 0, arrlen(function->locals) * sizeof(*function->locals));
    for (int i = 0; i < arrlen(function->locals); i++) {
        function->locals[i].type = entry_block->locals[i].stack.type;
    }

    arrsetlen(function->args, arrlen(entry_block->args));
    memset(function->args, 0, arrlen(function->args) * sizeof(*function->args));
    for (int i = 0; i < arrlen(function->args); i++) {
        function->args[i].type = entry_block->args[i].stack.type;
    }


cleanup:
    return err;
}

tdn_err_t jit_function(jit_function_t* function, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;

    TRACE("========================================");
    string_builder_t str_builder = {};
    string_builder_push_method_signature(&str_builder, function->method, true);
    const char* name = string_builder_build(&str_builder);
    TRACE("%s", name);
    string_builder_free(&str_builder);

    TRACE("----------------------------------------");
    // start with verifying the function fully
    CHECK_AND_RETHROW(jit_visit_blocks(function, NULL));

    TRACE("----------------------------------------");

    // prepare all the blocks for another pass, this time with a spidir
    // block ready so it can be jumped to
    for (int i = 0; i < arrlen(function->blocks); i++) {
        function->blocks[i].visited = false;
        function->blocks[i].spidir_block = spidir_builder_create_block(builder);
    }

    // now we can do the second pass of emitting
    CHECK_AND_RETHROW(jit_visit_blocks(function, builder));

cleanup:
    return err;
}

void jit_function_destroy(jit_function_t* function) {
    if (function == NULL)
        return;

    for (int i = 0; i < arrlen(function->blocks); i++) {
        arrfree(function->blocks[i].args);
        arrfree(function->blocks[i].locals);
        arrfree(function->blocks[i].stack);
    }
    arrfree(function->blocks);

    arrfree(function->locals);
    arrfree(function->args);

    hmfree(function->labels);
    arrfree(function->queue);
}
