#include "function.h"

#include "emitter.h"
#include "type.h"
#include "dotnet/disasm.h"
#include "dotnet/types.h"
#include "dotnet/verifier/basic_block.h"
#include "spidir/log.h"
#include "tomatodotnet/disasm.h"
#include "tomatodotnet/tdn.h"
#include "tomatodotnet/util/stb_ds.h"
#include "util/except.h"
#include "util/string.h"
#include "util/string_builder.h"

block_t* jit_function_get_block(function_t* function, uint32_t target_pc, uint32_t* leave_target_stack) {
    basic_block_entry_t* b = hmgetp_null(function->labels, target_pc);
    if (b == NULL) {
        return NULL;
    }

    // get the real block
    block_t* block = &function->blocks[b->value.index];

    // do we have a leave target we are after?
    if (arrlen(leave_target_stack) != 0) {
        leave_block_key_t key = {
            .block = block,
            .leave_target = arrlast(leave_target_stack)
        };
        block_t* leave_block = hmget(function->leave_blocks, key);

        // if we don't have a block in the leave chain to this target
        // then create one right now (only if not emitting)
        if (leave_block == NULL && !function->emitting) {
            leave_block = tdn_host_mallocz(sizeof(block_t), _Alignof(block_t));
            if (leave_block == NULL) {
                return NULL;
            }

            // copy the info into the block
            leave_block->block = b->value;
            leave_block->leave_target_stack = jit_copy_leave_targets(leave_target_stack);

            // store it
            hmput(function->leave_blocks, key, leave_block);
        }

        block = leave_block;
    }

    return block;
}

static void jit_clone_block(block_t* in, block_t* out) {
    out->block = in->block;
    out->jit_block = in->jit_block;

    // steal the leave target stack, its not needed anymore
    out->leave_target_stack = jit_copy_leave_targets(in->leave_target_stack);

    arrsetlen(out->args, arrlen(in->args));
    memcpy(out->args, in->args, arrlen(in->args) * sizeof(*in->args));

    arrsetlen(out->locals, arrlen(in->locals));
    memcpy(out->locals, in->locals, arrlen(in->locals) * sizeof(*in->locals));

    arrsetlen(out->stack, arrlen(in->stack));
    memcpy(out->stack, in->stack, arrlen(in->stack) * sizeof(*in->stack));
}

static void jit_destroy_block(block_t* block) {
    arrfree(block->stack);
    arrfree(block->stack_phis);
    arrfree(block->locals);
    arrfree(block->args);
    arrfree(block->leave_target_stack);
}

// we need this purely for the length, in theory if we had an alloca in spidir then
// we would be able to use it instead of this
typedef enum localloc_state_machine {
    LOCALLOC_PATTERN__NONE,
    LOCALLOC_PATTERN__FOUND_LDC_I4,
    LOCALLOC_PATTERN__FOUND_CONV_U,
    LOCALLOC_PATTERN__FOUND_SIZEOF,
    LOCALLOC_PATTERN__FOUND_MUL_OVF_UN,
} localloc_state_machine_t;

static tdn_err_t jit_visit_basic_block(function_t* function, block_t* in_block, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = function->method;
    RuntimeMethodBody body = method->MethodBody;
    stack_value_t* stack_items = NULL;
    tdn_il_inst_t inst = { .control_flow = TDN_IL_CF_FIRST };
    bool trace = (function->emitting && tdn_get_config()->jit_emit_trace) || (!function->emitting && tdn_get_config()->jit_type_prop_trace);

    // clone the block into the current frame, so we can modify it
    block_t block = {};
    jit_clone_block(in_block, &block);

    // if we are in emit set the current block to
    // the basic block we are jitting
    if (builder != NULL) {
        spidir_builder_set_block(builder, block.jit_block);
    }

    int indent = 0;
    bool allow_unsafe = method->Module->Assembly->AllowUnsafe;

    // the localloc verification
    localloc_state_machine_t localloc_state = LOCALLOC_PATTERN__NONE;
    int localloc_size = 0;

    // get the pc
    tdn_il_prefix_t prefix = 0;
    uint32_t pc = block.block.start;
    RuntimeTypeInfo constrained = NULL;
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

        if (trace) {
            indent = tdn_disasm_print_start(body, pc, inst, indent);
        }

        // normalize the instruction for easier processing
        tdn_normalize_inst(&inst);
        pc += inst.length;

        // handle prefixes right away
        if (inst.opcode == CEE_VOLATILE) {
            prefix |= TDN_IL_PREFIX_VOLATILE;
            continue;
        } else if (inst.opcode == CEE_READONLY) {
            prefix |= TDN_IL_PREFIX_READONLY;
            continue;
        } else if (inst.opcode == CEE_CONSTRAINED) {
            prefix |= TDN_IL_PREFIX_CONSTRAINED;
            constrained = inst.operand.type;
            continue;

        } else {
            inst.prefixes = prefix;

            // constrained is only allowed on a callvirt
            if (prefix & TDN_IL_PREFIX_CONSTRAINED) {
                CHECK(inst.opcode == CEE_CALLVIRT || inst.opcode == CEE_CALL);
                inst.constrained = constrained;
                constrained = NULL;
            }

            prefix = 0;
        }

        //
        // figure how much we need to pop from the stack
        //
        il_stack_behavior_t stack_behavior = g_il_stack_behavior[inst.opcode];

        // ensure we know how much to push
        if (stack_behavior.push < 0) {
            CHECK(inst.opcode == CEE_CALL || inst.opcode == CEE_CALLVIRT,
                "Invalid PushVar for `%s`", tdn_get_opcode_name(inst.opcode));
            if (inst.operand.method->ReturnParameter->ParameterType != tVoid) {
                stack_behavior.push = 1;
            } else {
                stack_behavior.push = 0;
            }
        }

        // ensure we know how much to pop
        if (stack_behavior.pop < 0) {
            if (inst.opcode == CEE_RET) {
                if (method->ReturnParameter->ParameterType != tVoid) {
                    stack_behavior.pop = 1;
                } else {
                    stack_behavior.pop = 0;
                }

            } else if (inst.opcode == CEE_CALL || inst.opcode == CEE_CALLVIRT || inst.opcode == CEE_NEWOBJ) {
                stack_behavior.pop = inst.operand.method->Parameters->Length;
                if (inst.opcode != CEE_NEWOBJ && !inst.operand.method->Attributes.Static) {
                    stack_behavior.pop++;
                }
            } else {
                CHECK_FAIL("Invalid VarPop for `%s`", tdn_get_opcode_name(inst.opcode));
            }
        }

        arrsetlen(stack_items, stack_behavior.pop);

        //
        // pop from the stack, the weird error
        // condition is to make the ilverify tests
        // happy
        //
        CHECK(arrlen(block.stack) >= arrlen(stack_items));
        for (int i = arrlen(stack_items) - 1; i >= 0; i--) {
            stack_items[i] = arrpop(block.stack);
        }

        // check the localloc state machine, the patterns we are matching are:
        //
        //  ldc.i4 <total size>
        //  conv.u
        //  localloc
        //
        // or (when there are generics)
        //  ldc.i4 <len>
        //  conv.u
        //  sizeof !!T
        //  mul.ovf.un
        //  localloc
        //
        // in both cases we will set a constant value for localloc
        // in the case of safe assemblies we will also verify that
        // it continues with:
        //  ldc.i4 <len>
        //  newobj Span<T>(void*,int)
        //
        // we will explicitly check that the len * sizeof(T) matches
        // the allocated size
        //
        switch (localloc_state) {
            case LOCALLOC_PATTERN__NONE: {
                if (inst.opcode == CEE_LDC_I4) {
                    localloc_state = LOCALLOC_PATTERN__FOUND_LDC_I4;
                    localloc_size = inst.operand.int32;
                } else {
                    localloc_state = LOCALLOC_PATTERN__NONE;
                }
            } break;

            case LOCALLOC_PATTERN__FOUND_LDC_I4: {
                if (inst.opcode == CEE_CONV_U) {
                    localloc_state = LOCALLOC_PATTERN__FOUND_CONV_U;
                } else {
                    localloc_state = LOCALLOC_PATTERN__NONE;
                }
            } break;

            case LOCALLOC_PATTERN__FOUND_CONV_U: {
                if (inst.opcode == CEE_LOCALLOC) {
                    inst.operand_type = TDN_IL_INT32;
                    inst.operand.int32 = localloc_size;
                    localloc_state = LOCALLOC_PATTERN__NONE;

                } else if (inst.opcode == CEE_SIZEOF) {
                    // TODO: overflow checking or something
                    localloc_size *= inst.operand.type->StackSize;
                    localloc_state = LOCALLOC_PATTERN__FOUND_SIZEOF;

                } else {
                    localloc_state = LOCALLOC_PATTERN__NONE;
                }
            } break;

            case LOCALLOC_PATTERN__FOUND_SIZEOF: {
                if (inst.opcode == CEE_MUL_OVF_UN) {
                    localloc_state = LOCALLOC_PATTERN__FOUND_MUL_OVF_UN;
                } else {
                    localloc_state = LOCALLOC_PATTERN__NONE;
                }
            } break;

            case LOCALLOC_PATTERN__FOUND_MUL_OVF_UN: {
                if (inst.opcode == CEE_LOCALLOC) {
                    inst.operand_type = TDN_IL_INT32;
                    inst.operand.int32 = localloc_size;
                    localloc_state = LOCALLOC_PATTERN__NONE;

                } else {
                    localloc_state = LOCALLOC_PATTERN__NONE;
                }
            } break;
        }

        //
        // Ensure we can push to the stack enough items
        //
        CHECK_ERROR(arrlen(block.stack) + stack_behavior.push <= body->MaxStackSize,
            TDN_ERROR_VERIFIER_STACK_OVERFLOW);
        size_t wanted_stack_size = arrlen(block.stack) + stack_behavior.push;

        // ensure we have a verifier for this opcode
        CHECK(inst.opcode < g_type_dispatch_table_size && g_type_dispatch_table[inst.opcode] != NULL,
            "Unimplemented opcode `%s` in type-propogation", tdn_get_opcode_name(inst.opcode));
        CHECK_AND_RETHROW(g_type_dispatch_table[inst.opcode](function, &block, &inst, stack_items));

        // call the emitter (if need be)
        if (builder != NULL) {
            CHECK(inst.opcode < g_emit_dispatch_table_size && g_emit_dispatch_table[inst.opcode] != NULL,
                "Unimplemented opcode `%s` in emitter", tdn_get_opcode_name(inst.opcode));
            CHECK_AND_RETHROW(g_emit_dispatch_table[inst.opcode](function, builder, &block, &inst, stack_items));
        }

        // ensure that the instruction was executed correctly
        CHECK(arrlen(block.stack) == wanted_stack_size);

        if (trace) {
            indent = tdn_disasm_print_end(body, pc, indent);
        }

        tdn_free_inst(&inst);
    }

    // if we have a fallthrough, then we need to merge to the next block
    if (inst.control_flow == TDN_IL_CF_NEXT || inst.control_flow == TDN_IL_CF_CALL) {
        block_t* next = jit_function_get_block(function, block.block.end, block.leave_target_stack);
        CHECK(next != NULL);

        CHECK_AND_RETHROW(type_on_block_fallthrough(function, &block, next));
        if (builder != NULL) {
            CHECK_AND_RETHROW(emitter_on_block_fallthrough(function, builder, &block, next));
        }
    }

    // last must be a valid instruction
    CHECK(
        inst.control_flow != TDN_IL_CF_FIRST &&
        inst.control_flow != TDN_IL_CF_META
    );

cleanup:
    // free the block data
    jit_destroy_block(&block);
    tdn_free_inst(&inst);

    arrfree(stack_items);

    return err;
}


tdn_err_t jit_visit_blocks(function_t* function, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;
    bool trace = (function->emitting && tdn_get_config()->jit_emit_trace) || (!function->emitting && tdn_get_config()->jit_verify_trace);

    // for the emitter we need a pass in here to initialize all of the block
    // locals before we enter the real entry block
    if (builder != NULL) {
        CHECK_AND_RETHROW(emitter_on_entry_block(function, builder, &function->entry_block));
    }

    // and now we need to merge with the entry block, which contains
    // all of the initial information about the function entry
    CHECK_AND_RETHROW(type_on_block_fallthrough(function, &function->entry_block, &function->blocks[0]));
    if (builder != NULL) {
        CHECK_AND_RETHROW(emitter_on_block_fallthrough(function, builder, &function->entry_block, &function->blocks[0]));
    }

    // and run until all blocks are verifier
    while (arrlen(function->queue) != 0) {
        block_t* block = arrpop(function->queue);
        block->in_queue = false;

        if (trace) {
            TRACE("\tBasic block (IL_%04x):", block->block.start);
        }

        CHECK_AND_RETHROW(jit_visit_basic_block(function, block, builder));
    }

cleanup:
    arrfree(function->queue);

    return err;
}

tdn_err_t jit_function_init(function_t* function, RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure this method has a body
    CHECK(method->MethodBody != NULL);
    function->method = method;

    // find all of the basic blocks
    CHECK_AND_RETHROW(verifier_find_basic_blocks(method, &function->labels, NULL));

    // setup the blocks array
    arrsetlen(function->blocks, hmlen(function->labels));
    memset(function->blocks, 0, arrlen(function->blocks) * sizeof(*function->blocks));

    // copy over the block information
    for (int i = 0; i < hmlen(function->labels); i++) {
        function->blocks[function->labels[i].value.index].block = function->labels[i].value;
    }

    block_t* entry_block = &function->entry_block;

    //
    // Initialize the arguments
    //

    // handle the this parameter if non-static
    if (!method->Attributes.Static) {
        block_local_t local = {};
        arrpush(entry_block->args, local);

        local_t arg = {
            .type = method->DeclaringType,
        };
        if (tdn_type_is_valuetype(method->DeclaringType)) {
            CHECK_AND_RETHROW(tdn_get_byref_type(arg.type, &arg.type));
        }
        arrpush(function->args, arg);
    }

    // now do the same for the parameters
    for (int i = 0; i < method->Parameters->Length; i++) {
        ParameterInfo parameter = method->Parameters->Elements[i];
        RuntimeTypeInfo type = parameter->ParameterType;

        block_local_t local = {};
        arrpush(entry_block->args, local);

        local_t arg = { .type = type };
        arrpush(function->args, arg);
    }

    //
    // Initialize the locals
    //

    // the entry block context
    if (method->MethodBody->LocalVariables != NULL) {
        // initialize the local types
        arrsetlen(function->locals, method->MethodBody->LocalVariables->Length);
        memset(function->locals, 0, arrlen(function->locals) * sizeof(*function->locals));
        for (int i = 0; i < method->MethodBody->LocalVariables->Length; i++) {
            function->locals[i].type = method->MethodBody->LocalVariables->Elements[i]->LocalType;
        }

        // initialize the entry block types
        arrsetlen(entry_block->locals, method->MethodBody->LocalVariables->Length);
        memset(entry_block->locals, 0, arrlen(entry_block->locals) * sizeof(*entry_block->locals));
    }

cleanup:
    return err;
}

tdn_err_t jit_function(function_t* function, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;
    bool trace = tdn_get_config()->jit_emit_trace || tdn_get_config()->jit_verify_trace;
    spidir_log_level_t orig_level = spidir_log_get_max_level();

    if (trace) {
        // set to trace so we can see the generated nodes
        if (tdn_get_config()->jit_emit_trace) {
            spidir_log_set_max_level(SPIDIR_LOG_LEVEL_TRACE);
        }

        TRACE("========================================");
        string_builder_t str_builder = {};
        string_builder_push_method_signature(&str_builder, function->method, true);
        const char* name = string_builder_build(&str_builder);
        TRACE("%s", name);
        string_builder_free(&str_builder);
        TRACE("----------------------------------------");
    }

    // start with verifying the function fully
    CHECK_AND_RETHROW(jit_visit_blocks(function, NULL));

    if (trace) {
        TRACE("----------------------------------------");
    }

    // don't allow
    function->emitting = true;

    // prepare all the blocks for another pass, this time with a spidir
    // block ready so it can be jumped to
    for (int i = 0; i < arrlen(function->blocks); i++) {
        block_t* block = &function->blocks[i];
        block->visited = false;
        block->jit_block = spidir_builder_create_block(builder);
    }

    // prepare all the leave blocks as well
    for (int i = 0; i < hmlen(function->leave_blocks); i++) {
        block_t* block = function->leave_blocks[i].value;
        block->visited = false;
        block->jit_block = spidir_builder_create_block(builder);
    }

    // now we can do the second pass of emitting
    CHECK_AND_RETHROW(jit_visit_blocks(function, builder));

cleanup:
    spidir_log_set_max_level(orig_level);

    return err;
}

void jit_function_destroy(function_t* function) {
    if (function == NULL)
        return;

    // free the entry block
    arrfree(function->entry_block.args);
    arrfree(function->entry_block.locals);
    arrfree(function->entry_block.stack);

    // free the rest of the blocks
    for (int i = 0; i < arrlen(function->blocks); i++) {
        jit_destroy_block(&function->blocks[i]);
    }
    arrfree(function->blocks);

    for (int i = 0; i < hmlen(function->leave_blocks); i++) {
        jit_destroy_block(function->leave_blocks[i].value);
        tdn_host_free(function->leave_blocks[i].value);
    }
    hmfree(function->leave_blocks);

    arrfree(function->locals);
    arrfree(function->args);

    hmfree(function->labels);
    arrfree(function->queue);
}

