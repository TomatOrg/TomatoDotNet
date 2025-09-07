#include "verifier.h"

#include <stdbool.h>
#include <stdint.h>

#include "basic_block.h"
#include "internal.h"
#include "control_flow.h"
#include "instruction.h"
#include "localloc.h"
#include "dotnet/disasm.h"
#include "dotnet/types.h"
#include "tomatodotnet/disasm.h"
#include "tomatodotnet/tdn.h"
#include "util/except.h"
#include "util/string.h"
#include "util/string_builder.h"

static void clone_block(block_t* in, block_t* out) {
    out->block = in->block;
    out->this_initialized = in->this_initialized;

    arrsetlen(out->args, arrlen(in->args));
    memcpy(out->args, in->args, arrlen(in->args) * sizeof(*in->args));

    arrsetlen(out->locals, arrlen(in->locals));
    memcpy(out->locals, in->locals, arrlen(in->locals) * sizeof(*in->locals));

    arrsetlen(out->stack, arrlen(in->stack));
    memcpy(out->stack, in->stack, arrlen(in->stack) * sizeof(*in->stack));
}

static void destroy_block(block_t* block) {
    arrfree(block->stack);
    arrfree(block->locals);
    arrfree(block->args);
}

static tdn_err_t verifier_visit_basic_block(function_t* function, block_t* in_block) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = function->method;
    RuntimeMethodBody body = method->MethodBody;
    stack_value_t* stack_items = NULL;
    tdn_il_inst_t inst = { .control_flow = TDN_IL_CF_FIRST };
    bool trace = tdn_get_config()->jit_verify_trace;

    // clone the block into the current frame, so we can modify it
    block_t block = {};
    clone_block(in_block, &block);

    int indent = 0;
    bool allow_unsafe = method->Module->Assembly->AllowUnsafe;

    // the localloc verification
    localloc_verifier_t localloc_verifier = {};

    // get the pc
    tdn_il_prefix_t prefix = 0;
    tdn_il_opcode_t last_opcode;
    uint32_t pc = block.block.start;
    RuntimeTypeInfo constrained = NULL;
    while (pc < block.block.end) {
        last_opcode = inst.opcode;

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
        } else if (inst.opcode == CEE_CONSTRAINED) {
            prefix |= TDN_IL_PREFIX_CONSTRAINED;
            constrained = inst.operand.type;
            continue;

        } else {
            inst.prefixes = prefix;

            // ensure that the prefix matches
            if (prefix & TDN_IL_PREFIX_VOLATILE || prefix & TDN_IL_PREFIX_UNALIGNED) {
                switch (inst.opcode) {
                    case CEE_LDIND_I1: case CEE_LDIND_U1: case CEE_LDIND_I2: case CEE_LDIND_U2:
                    case CEE_LDIND_I4: case CEE_LDIND_U4: case CEE_LDIND_I8: case CEE_LDIND_I:
                    case CEE_LDIND_R4: case CEE_LDIND_R8: case CEE_LDIND_REF: case CEE_STIND_REF:
                    case CEE_STIND_I1: case CEE_STIND_I2: case CEE_STIND_I4: case CEE_STIND_I8:
                    case CEE_STIND_R4: case CEE_STIND_R8: case CEE_LDFLD: case CEE_STFLD:
                    case CEE_LDOBJ: case CEE_STOBJ:
                    case CEE_INITBLK: case CEE_CPBLK:
                        break;
                    default:
                        // only volatile prefix is allowed on ldsfld and stsfld
                        CHECK(prefix & TDN_IL_PREFIX_VOLATILE && (inst.opcode == CEE_LDSFLD || inst.opcode == CEE_STSFLD));
                }
            }

            // readonly is only allowed on ldelema
            if (prefix & TDN_IL_PREFIX_READONLY) {
                CHECK(inst.opcode == CEE_LDELEMA);
            }

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
        CHECK(inst.opcode <= g_il_stack_behavior_count);
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
        CHECK_ERROR(arrlen(block.stack) >= arrlen(stack_items),
            inst.opcode == CEE_RET ? TDN_ERROR_VERIFIER_RETURN_MISSING : TDN_ERROR_VERIFIER_STACK_UNDERFLOW);
        for (int i = arrlen(stack_items) - 1; i >= 0; i--) {
            stack_items[i] = arrpop(block.stack);
        }

        // if the last opcode is an ldftn or ldvirtftn then
        // this must be a newobj
        if (last_opcode == CEE_LDFTN || last_opcode == CEE_LDVIRTFTN) {
            CHECK_ERROR(inst.opcode == CEE_NEWOBJ,
                TDN_ERROR_VERIFIER_DELEGATE_PATTERN);
        }

        // before ldvirtftn we must have a dup
        if (inst.opcode == CEE_LDVIRTFTN) {
            CHECK_ERROR(last_opcode == CEE_DUP,
                TDN_ERROR_VERIFIER_DELEGATE_PATTERN);
        }

        // check for localloc patterns
        CHECK_AND_RETHROW(localloc_verifier_check(&localloc_verifier, &inst, allow_unsafe));

        //
        // Ensure we can push to the stack enough items
        //
        CHECK_ERROR(arrlen(block.stack) + stack_behavior.push <= body->MaxStackSize,
            TDN_ERROR_VERIFIER_STACK_OVERFLOW);
        size_t wanted_stack_size = arrlen(block.stack) + stack_behavior.push;

        // ensure we have a verifier for this opcode
        CHECK(inst.opcode < g_verify_dispatch_table_size && g_verify_dispatch_table[inst.opcode] != NULL,
            "Unimplemented opcode %s in verifier", tdn_get_opcode_name(inst.opcode));
        CHECK_AND_RETHROW(g_verify_dispatch_table[inst.opcode](function, &block, &inst, stack_items));

        // ensure that the instruction was executed correctly
        CHECK(arrlen(block.stack) == wanted_stack_size);

        if (trace) {
            indent = tdn_disasm_print_end(body, pc, indent);
        }

        tdn_free_inst(&inst);
    }

    // if we have a fallthrough, then we need to merge to the next block
    if (inst.control_flow == TDN_IL_CF_NEXT || inst.control_flow == TDN_IL_CF_CALL) {
        block_t* next = verifier_get_block(function, block.block.end);
        CHECK_ERROR(next != NULL, TDN_ERROR_VERIFIER_METHOD_FALLTHROUGH);

        CHECK_AND_RETHROW(verifier_propagate_control_flow(function, &block, next, true));
    }

    // last must be a valid instruction
    CHECK(
        inst.control_flow != TDN_IL_CF_FIRST &&
        inst.control_flow != TDN_IL_CF_META
    );

cleanup:
    // free the block data
    destroy_block(&block);
    tdn_free_inst(&inst);

    arrfree(stack_items);

    return err;
}

static tdn_err_t verifier_start_block(function_t* function, block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;
    basic_block_t* basic_block = &block->block;

    if (basic_block->try_start) {
        CHECK_ERROR(arrlen(block->stack) == 0, TDN_ERROR_VERIFIER_TRY_NON_EMPTY_STACK);

        RuntimeExceptionHandlingClause_Array clauses = function->method->MethodBody->ExceptionHandlingClauses;
        for (int i = 0; clauses != NULL && i < clauses->Length; i++) {
            RuntimeExceptionHandlingClause r = clauses->Elements[i];

            if (basic_block->start != r->TryOffset) {
                continue;
            }

            if (r->Flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
                block_t* filter_block = verifier_get_block(function, r->FilterOffset);
                CHECK(filter_block != NULL);
                CHECK_AND_RETHROW(verifier_propagate_this_state(function, block, filter_block));
            }

            block_t* handler_block = verifier_get_block(function, r->HandlerOffset);
            CHECK(handler_block != NULL);
            CHECK_AND_RETHROW(verifier_propagate_this_state(function, block, handler_block));
        }
    }

    if (basic_block->filter_start || basic_block->handler_start) {
        RuntimeExceptionHandlingClause r = NULL;
        if (basic_block->handler_clause != NULL) {
            r = basic_block->handler_clause;
        } else if (basic_block->filter_clause != NULL) {
            r = basic_block->filter_clause;
        } else {
            ASSERT(!"Should not happen");
        }

        if (r->Flags == COR_ILEXCEPTION_CLAUSE_FILTER || r->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
            // // stack must be uninit or 1 (exception object)
            // CHECK_ERROR(arrlen(block->stack) == 1, TDN_ERROR_VERIFIER_FILTER_OR_CATCH_UNEXPECTED_STACK);
            //
            // // TODO: initialize the stack right now
            //
            // if (r->Flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
            //     // TODO: should have an object on the stack
            // } else if (r->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
            //     RuntimeTypeInfo exception_type = r->CatchType;
            //     CHECK_ERROR(!exception_type->IsByRef, TDN_ERROR_VERIFIER_CATCH_BYREF);
            //     // TODO: set as the entry object
            // }
        } else {
            // stack must be empty
            CHECK_ERROR(arrlen(block->stack) == 0, TDN_ERROR_VERIFIER_FIN_OR_FAULT_NON_EMPTY_STACK);
        }
    }

    // NOTE: in theory we would check for backward branch constraint, but new C#
    //       versions have amended it and don't require that constraint anymore,
    //       so we won't be checking for it, besides our code can handle it

cleanup:
    return err;
}

static tdn_err_t verifier_visit_blocks(function_t* function) {
    tdn_err_t err = TDN_NO_ERROR;
    bool trace = tdn_get_config()->jit_verify_trace;

    // and now we need to merge with the entry block, which contains
    // all of the initial information about the function entry
    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, &function->entry_block, &function->blocks[0], true));

    // and run until all blocks are verifier
    while (arrlen(function->queue) != 0) {
        block_t* block = arrpop(function->queue);
        block->in_queue = false;

        if (trace) {
            TRACE("\tBasic block (IL_%04x):", block->block.start);
        }

        CHECK_AND_RETHROW(verifier_start_block(function, block));
        CHECK_AND_RETHROW(verifier_visit_basic_block(function, block));
    }

    cleanup:
        arrfree(function->queue);

    return err;
}

static tdn_err_t verifier_function_init(function_t* function, RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;

    // check if we allow unsafe operations on this assembly
    function->allow_unsafe = method->Module->Assembly->AllowUnsafe;

    // ctor bypass readonly structs and fields
    bool is_ctor = tdn_compare_string_to_cstr(method->Name, ".ctor") ||
                    tdn_compare_string_to_cstr(method->Name, ".cctor");

    // ensure this method has a body
    CHECK(method->MethodBody != NULL);
    function->method = method;

    // find all of the basic blocks
    CHECK_AND_RETHROW(verifier_find_basic_blocks(method, &function->labels, &function->modifies_this_type));

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
        block_local_t local = {
            .flags ={
                // we only want this to represent a `this` ptr if the
                // method does not take the this by-ref
                .this_ptr = !function->modifies_this_type
            }
        };

        // TODO: unscoped support

        // if this is a readonly struct, then the ref is readonly as well
        if (method->DeclaringType->IsReadOnly && !is_ctor) {
            local.flags.ref_read_only = true;
        }

        // unlike the this not being a non-local ref, its contents as a ref-struct
        // are considered non-local
        if (method->DeclaringType->IsByRefStruct) {
            local.flags.ref_struct_non_local = true;
        }

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

        // if this is a readonly parameter then mark it as such
        if (parameter->ReferenceIsReadOnly && !is_ctor) {
            CHECK(type->IsByRef);
            local.flags.ref_read_only = true;
        }

        // TODO: scoped references

        // references are non-local since they come from the outside
        // same is true for the ref-structs
        if (type->IsByRef) {
            local.flags.ref_non_local = true;
        }

        // ref-struct that is incoming will only contain non-local references
        // so mark it as such
        if (type->IsByRefStruct) {
            local.flags.ref_struct_non_local = true;
        }

        arrpush(entry_block->args, local);

        local_t arg = {
            .type = type,
        };
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

    // if this is a ctor, we need to track the ctor state to ensure that the parent
    // ctor is called properly
    if (
        !function->method->Attributes.Static &&
        !tdn_type_is_valuetype(function->method->DeclaringType) &&
        tdn_compare_string_to_cstr(function->method->Name, ".ctor")
    ) {
        function->track_ctor_state = true;
    }

cleanup:
    return err;
}

tdn_err_t verifier_verify_method(RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;
    bool trace = tdn_get_config()->jit_verify_trace;
    function_t function = {};

    // if already verified we can ignore it
    if (method->IsVerified) {
        goto cleanup;
    }

    // we need to always verify the generic version of the method
    // and not the expanded one, this ensures that generic parameters
    // are always accessible but are checked against the generic of it
    // and that you don't perform any weird non-generic access on the type
    // (as the verifier would fail on such a thing)
    RuntimeMethodBase method_def = verifier_get_typical_method_definition(method);
    if (method_def->IsVerified) {
        method->IsVerified = true;
        goto cleanup;
    }

    if (trace) {
        TRACE("========================================");
        string_builder_t str_builder = {};
        string_builder_push_method_signature(&str_builder, method_def, true);
        const char* name = string_builder_build(&str_builder);
        TRACE("%s", name);
        string_builder_free(&str_builder);
        TRACE("----------------------------------------");
    }

    // initialize our context
    CHECK_AND_RETHROW(verifier_function_init(&function, method_def));

    // start with verifying the function fully
    CHECK_AND_RETHROW(verifier_visit_blocks(&function));

    if (trace) {
        TRACE("----------------------------------------");
    }

    // mark both the method def and the method as verified
    method_def->IsVerified = true;
    method->IsVerified = true;

cleanup:
    destroy_block(&function.entry_block);
    for (int i = 0; i < arrlen(function.blocks); i++) {
        destroy_block(&function.blocks[i]);
    }
    arrfree(function.blocks);
    arrfree(function.locals);
    arrfree(function.args);
    hmfree(function.labels);
    arrfree(function.queue);

    return err;
}


