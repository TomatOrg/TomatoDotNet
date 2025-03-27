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

jit_block_t* jit_function_get_block(jit_function_t* function, uint32_t target_pc, uint32_t* leave_target_stack) {
    jit_basic_block_entry_t* b = hmgetp_null(function->labels, target_pc);
    if (b == NULL) {
        return NULL;
    }

    // get the real block
    jit_block_t* block = &function->blocks[b->value.index];

    // do we have a leave target we are after?
    if (arrlen(leave_target_stack) != 0) {
        jit_leave_block_key_t key = {
            .block = block,
            .leave_target = arrlast(leave_target_stack)
        };
        jit_block_t* leave_block = hmget(function->leave_blocks, key);

        // if we don't have a block in the leave chain to this target
        // then create one right now (only if not emitting)
        if (leave_block == NULL && !function->emitting) {
            leave_block = tdn_host_mallocz(sizeof(jit_block_t), _Alignof(jit_block_t));
            if (leave_block == NULL) {
                return NULL;
            }

            // copy the info into the block
            leave_block->start = b->value.start;
            leave_block->end = b->value.end;
            leave_block->leave_target_stack = jit_copy_leave_targets(leave_target_stack);

            // store it
            hmput(function->leave_blocks, key, leave_block);
        }

        block = leave_block;
    }

    return block;
}

static void jit_clone_block(jit_block_t* in, jit_block_t* out) {
    out->start = in->start;
    out->end = in->end;
    out->spidir_block = in->spidir_block;

    // steal the leave target stack, its not needed anymore
    out->leave_target_stack = jit_copy_leave_targets(in->leave_target_stack);

    arrsetlen(out->args, arrlen(in->args));
    memcpy(out->args, in->args, arrlen(in->args) * sizeof(*in->args));

    arrsetlen(out->locals, arrlen(in->locals));
    memcpy(out->locals, in->locals, arrlen(in->locals) * sizeof(*in->locals));

    arrsetlen(out->stack, arrlen(in->stack));
    memcpy(out->stack, in->stack, arrlen(in->stack) * sizeof(*in->stack));
}

static void jit_destroy_block(jit_block_t* block) {
    arrfree(block->stack);
    arrfree(block->stack_phis);
    arrfree(block->locals);
    arrfree(block->args);
    arrfree(block->leave_target_stack);
}

static tdn_err_t jit_visit_basic_block(jit_function_t* function, jit_block_t* in_block, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = function->method;
    RuntimeMethodBody body = method->MethodBody;
    jit_stack_item_t* stack_items = NULL;
    bool trace = (function->emitting && tdn_get_config()->jit_emit_trace) || (!function->emitting && tdn_get_config()->jit_verify_trace);

    // clone the block into the current frame, so we can modify it
    jit_block_t block = {};
    jit_clone_block(in_block, &block);

    // if we are in emit set the current block to
    // the basic block we are jitting
    if (builder != NULL) {
        spidir_builder_set_block(builder, block.spidir_block);
    }

    int indent = 0;

    // get the pc
    tdn_il_prefix_t prefix = 0;
    tdn_il_inst_t inst = { .control_flow = TDN_IL_CF_FIRST };
    tdn_il_inst_t last_inst;
    uint32_t pc = block.start;
    while (pc < block.end) {
        last_inst = inst;

        // can only parse more instructions if we had no block
        // terminating instruction
        CHECK_ERROR(
            inst.control_flow == TDN_IL_CF_FIRST ||
            inst.control_flow == TDN_IL_CF_NEXT ||
            inst.control_flow == TDN_IL_CF_META ||
            inst.control_flow == TDN_IL_CF_CALL,
            TDN_ERROR_VERIFIER_BAD_JUMP_TARGET
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
        } else {
            inst.prefixes = prefix;
            prefix = 0;

            // ensure that the prefix matches
            if (prefix & TDN_IL_PREFIX_VOLATILE) {
                switch (inst.opcode) {
                    case CEE_LDIND_I1:
                    case CEE_LDIND_U1:
                    case CEE_LDIND_I2:
                    case CEE_LDIND_U2:
                    case CEE_LDIND_I4:
                    case CEE_LDIND_U4:
                    case CEE_LDIND_I8:
                    case CEE_LDIND_I:
                    case CEE_LDIND_R4:
                    case CEE_LDIND_R8:
                    case CEE_LDIND_REF:
                    case CEE_STIND_REF:
                    case CEE_STIND_I1:
                    case CEE_STIND_I2:
                    case CEE_STIND_I4:
                    case CEE_STIND_I8:
                    case CEE_STIND_R4:
                    case CEE_STIND_R8:
                    case CEE_LDFLD:
                    case CEE_STFLD:
                    case CEE_LDSFLD:
                    case CEE_STSFLD:
                    case CEE_LDOBJ:
                    case CEE_STOBJ:
                    case CEE_INITBLK:
                    case CEE_CPBLK:
                        break;
                    default:
                        CHECK_FAIL();
                }
            }
        }

        //
        // figure how much we need to pop from the stack
        //
        il_stack_behavior_t stack_behavior = m_il_stack_behavior[inst.opcode];

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
        // pop from the stack
        //
        CHECK(arrlen(block.stack) >= arrlen(stack_items));
        for (int i = arrlen(stack_items) - 1; i >= 0; i--) {
            stack_items[i] = arrpop(block.stack);
        }

        // if the last opcode is an ldftn or ldvirtftn then
        // this must be a newobj
        if (last_inst.opcode == CEE_LDFTN || last_inst.opcode == CEE_LDVIRTFTN) {
            CHECK(inst.opcode == CEE_NEWOBJ);
        }

        // before ldvirtftn we must have a dup
        if (inst.opcode == CEE_LDVIRTFTN) {
            CHECK(last_inst.opcode == CEE_DUP);
        }

        //
        // Ensure we can push to the stack enough items
        //
        CHECK(arrlen(block.stack) + stack_behavior.push <= body->MaxStackSize);
        size_t wanted_stack_size = arrlen(block.stack) + stack_behavior.push;

        // ensure we have a verifier for this opcode
        CHECK(inst.opcode < g_verify_dispatch_table_size && g_verify_dispatch_table[inst.opcode] != NULL,
            "Unimplemented opcode %s in verifier", tdn_get_opcode_name(inst.opcode));
        CHECK_AND_RETHROW(g_verify_dispatch_table[inst.opcode](function, &block, &inst, stack_items));

        // call the emitter (if need be)
        if (builder != NULL) {
            CHECK(inst.opcode < g_emit_dispatch_table_size && g_emit_dispatch_table[inst.opcode] != NULL,
                "Unimplemented opcode %s in emitter", tdn_get_opcode_name(inst.opcode));
            CHECK_AND_RETHROW(g_emit_dispatch_table[inst.opcode](function, builder, &block, &inst, stack_items));
        }

        // ensure that the instruction was executed correctly
        CHECK(arrlen(block.stack) == wanted_stack_size);

        if (trace) {
            indent = tdn_disasm_print_end(body, pc, indent);
        }
    }

    // if we have a fallthrough, then we need to merge to the next block
    if (inst.control_flow == TDN_IL_CF_NEXT || inst.control_flow == TDN_IL_CF_CALL) {
        jit_block_t* next = jit_function_get_block(function, block.end, block.leave_target_stack);
        CHECK(next != NULL);

        CHECK_AND_RETHROW(verifier_on_block_fallthrough(function, &block, next));
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

    arrfree(stack_items);

    return err;
}

tdn_err_t jit_visit_blocks(jit_function_t* function, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;
    bool trace = (function->emitting && tdn_get_config()->jit_emit_trace) || (!function->emitting && tdn_get_config()->jit_verify_trace);

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

        if (trace) {
            TRACE("\tBasic block (IL_%04x):", block->start);
        }

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
        function->blocks[function->labels[i].value.index].start = function->labels[i].value.start;
        function->blocks[function->labels[i].value.index].end = function->labels[i].value.end;
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
                .is_this = true,
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
    if (method->MethodBody->LocalVariables != NULL) {
        arrsetlen(entry_block->locals, method->MethodBody->LocalVariables->Length);
        memset(entry_block->locals, 0, arrlen(entry_block->locals) * sizeof(*entry_block->locals));
        for (int i = 0; i < arrlen(entry_block->locals); i++) {
            entry_block->locals[i].stack.type = method->MethodBody->LocalVariables->Elements[i]->LocalType;
        }
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
    bool trace = tdn_get_config()->jit_emit_trace || tdn_get_config()->jit_verify_trace;

    if (trace) {
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
        jit_block_t* block = &function->blocks[i];
        block->visited = false;
        block->spidir_block = spidir_builder_create_block(builder);
    }

    // prepare all the leave blocks as well
    for (int i = 0; i < hmlen(function->leave_blocks); i++) {
        jit_block_t* block = function->leave_blocks[i].value;
        block->visited = false;
        block->spidir_block = spidir_builder_create_block(builder);
    }

    // now we can do the second pass of emitting
    CHECK_AND_RETHROW(jit_visit_blocks(function, builder));

cleanup:
    return err;
}

void jit_function_destroy(jit_function_t* function) {
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

RuntimeExceptionHandlingClause jit_get_enclosing_try_clause(jit_function_t* function, uint32_t pc, int type, RuntimeExceptionHandlingClause previous) {
    RuntimeExceptionHandlingClause_Array arr = function->method->MethodBody->ExceptionHandlingClauses;

    if (arr == NULL) {
        return NULL;
    }

    bool found_previous = previous == NULL;
    for (int i = 0; i < arr->Length; i++) {
        RuntimeExceptionHandlingClause clause = arr->Elements[i];
        if (previous == clause) {
            found_previous = true;
            continue;
        }

        // if we have not found the previous yet continue
        if (!found_previous) {
            continue;
        }

        if (type != -1 && clause->Flags != type) {
            continue;
        }

        if (clause->TryOffset <= pc && pc < clause->TryOffset + clause->TryLength) {
            return clause;
        }
    }

    return NULL;
}

RuntimeExceptionHandlingClause jit_get_enclosing_handler_clause(jit_function_t* function, uint32_t pc, int type, RuntimeExceptionHandlingClause previous) {
    RuntimeExceptionHandlingClause_Array arr = function->method->MethodBody->ExceptionHandlingClauses;

    if (arr == NULL) {
        return NULL;
    }

    bool found_previous = previous == NULL;
    for (int i = 0; i < arr->Length; i++) {
        RuntimeExceptionHandlingClause clause = arr->Elements[i];
        if (previous == clause) {
            found_previous = true;
            continue;
        }

        // if we have not found the previous yet continue
        if (!found_previous) {
            continue;
        }

        if (type != -1 && clause->Flags != type) {
            continue;
        }

        if (clause->HandlerOffset <= pc && pc < clause->HandlerOffset + clause->HandlerLength) {
            return clause;
        }
    }

    return NULL;
}

