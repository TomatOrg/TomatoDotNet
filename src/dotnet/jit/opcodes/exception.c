#include "../jit_internal.h"
#include "util/except.h"

#ifdef JIT_TRACE_MIR
#define MIR_append_insn(...) MIR_append_insn_output(__VA_ARGS__)
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Exception related opcodes:
//      - leave
//      - endfinally
//      - endfilter
//      - throw
//      - rethrow
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

err_t jit_emit_leave(jit_method_context_t* ctx) {
    err_t err = NO_ERROR;

    CHECK(ctx->filter_clause == NULL);

    // resolve the label
    MIR_label_t target_label;
    CHECK_AND_RETHROW(jit_resolve_branch(ctx, operand_i32, &target_label));

    int last_clausi = -1;

    // we found a leave, we are going to find every finally clause that we are in, and build
    // up a chain of where to go next, if we already have a clause with an entry to go to, we
    // are going to make sure it goes to the same place
    bool in_a_protected_block = false;
    System_Reflection_ExceptionHandlingClause_Array exceptions = body->ExceptionHandlingClauses;
    for (int i = 0; i < exceptions->Length; i++) {
        System_Reflection_ExceptionHandlingClause clause = exceptions->Data[i];

        if (clause->HandlerOffset <= ctx->il_offset && ctx->il_offset < clause->HandlerOffset + clause->HandlerLength) {
            // we are in a handler region, this means that the exception has been dealt with and
            // we should clear it out so the finally nodes won't think that it might need to do
            // something with it
            in_a_protected_block = true;

            // reset the exception value
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                         MIR_new_int_op(mir_ctx, 0)));
        }

        // make sure we are in this try
        if (clause->TryOffset > ctx->il_offset || ctx->il_offset >= clause->TryOffset + clause->TryLength)
            continue;

        // we are in a try block
        in_a_protected_block = true;

        // make sure we are getting a final block
        if (clause->Flags != COR_ILEXCEPTION_CLAUSE_FINALLY)
            continue;

        // lets get the clause label and offset
        int clausei = hmgeti(ctx->clause_to_label, clause);
        CHECK(clausei != -1);
        MIR_label_t finally_label = ctx->clause_to_label[clausei].value;

        // the current finally clause is going to jump into the target label
        // (unless it is nested in someone else)
        ctx->clause_to_label[clausei].next_clause = target_label;
        ctx->clause_to_label[clausei].last_in_chain = true;

        if (last_clausi == -1) {
            // jump to the first finally we see, don't forget to zero out the
            // exception register before doing so (given we have not thrown an exception)
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                         MIR_new_int_op(mir_ctx, 0)));
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_JMP,
                                         MIR_new_label_op(mir_ctx, finally_label)));
        } else {
            // the last clause is going to actually jump to us
            ctx->clause_to_label[last_clausi].next_clause = finally_label;
            ctx->clause_to_label[last_clausi].last_in_chain = false;
        }

        last_clausi = clausei;
    }

    // make sure we are in a try region
    CHECK(in_a_protected_block);

    if (last_clausi == -1) {
        // there is no finally around us, we can
        // safely jump to the target
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_JMP,
                                     MIR_new_label_op(mir_ctx, target_label)));

    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------

err_t jit_emit_endfinally(jit_method_context_t* ctx) {
    err_t err = NO_ERROR;

    // find the finally block we are in
    bool found = false;
    System_Reflection_ExceptionHandlingClause_Array exceptions = body->ExceptionHandlingClauses;
    for (int i = exceptions->Length - 1; i >= 0; i--) {
        System_Reflection_ExceptionHandlingClause clause = exceptions->Data[i];

        // make sure we are in this try
        if (clause->HandlerOffset > ctx->il_offset || ctx->il_offset >= clause->HandlerOffset + clause->HandlerLength)
            continue;

        // make sure we are getting a final block
        CHECK (
                clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY ||
                clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT
        );

        // lets get the clause label and offset
        int clausei = hmgeti(ctx->clause_to_label, clause);
        CHECK(clausei != -1);
        MIR_label_t next_clause = ctx->clause_to_label[clausei].next_clause;

        size_t nres = 1;
        if (ctx->method->ReturnType != NULL) {
            MIR_type_t mtype = jit_get_mir_type(ctx->method->ReturnType);
            if (mtype != MIR_T_BLK) {
                nres = 2;
            }
        }

        //
        // clause will be null if the try related to this finally does not actually have
        // any leave instruction, the flow can happen when an exception is thrown
        // unconditionally from a try-clause. in this case we are going to just
        // assume that we need to always return the exception
        //
        if (next_clause == NULL) {
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_ret_insn(mir_ctx, nres,
                                             MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                             MIR_new_int_op(mir_ctx, 0)));

            found = true;
            break;
        }

        // next_clause will either continue to the next finally if there was an exception, or it will
        // continue to the next instruction if there was no exception.
        MIR_label_t skip = MIR_new_label(mir_ctx);

        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_BF,
                                     MIR_new_label_op(mir_ctx, skip),
                                     MIR_new_reg_op(mir_ctx, ctx->exception_reg)));

        if (ctx->clause_to_label[clausei].last_in_chain) {
            //
            // This is the last finally in the function and in the chain, so we can return
            // right away from this function with the error.
            //
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_ret_insn(mir_ctx, nres,
                                             MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                             MIR_new_int_op(mir_ctx, 0)));

        } else {
            // The function has another clause in the
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_JMP,
                                         MIR_new_label_op(mir_ctx, next_clause)));
        }

        // insert the skip label
        MIR_append_insn(mir_ctx, mir_func, skip);

        found = true;
        break;
    }

    CHECK(found);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------

err_t jit_emit_endfilter(jit_method_context_t* ctx) {
    err_t err = NO_ERROR;

    CHECK(ctx->filter_clause != NULL);

    JIT_TRACE(
        TRACE("%*s} {", ctx->jit_trace_indent, "");
    );

    // get the value
    MIR_reg_t value_reg;
    System_Type value_type;
    CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg, NULL));

    // must be an int32
    CHECK(value_type == tSystem_Int32);

    // get the handler location
    int idx = hmgeti(ctx->pc_to_stack_snapshot, ctx->filter_clause->HandlerOffset);
    CHECK(idx != -1);
    MIR_label_t handler_label = ctx->pc_to_stack_snapshot[idx].label;

    // move the exception to it (its fine if we don't end up jumping to here)
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_MOV,
                                 MIR_new_reg_op(mir_ctx, ctx->ireg.regs[0]),
                                 MIR_new_reg_op(mir_ctx, ctx->exception_reg)));

    // check the result of this operation, if its non-zero go to the handler
    // otherwise rethrow
    // NOTE: in theory it should be 1 for handler, but any other value is unspecified
    //       so we can just do this
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_BT,
                                 MIR_new_label_op(mir_ctx, handler_label),
                                 MIR_new_reg_op(mir_ctx, value_reg)));

    // continue with the rethrow, exception_reg should still have it
    CHECK_AND_RETHROW(jit_throw(ctx, NULL, true));

    // we are outside the handler now
    ctx->filter_clause = NULL;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------

err_t jit_emit_throw(jit_method_context_t* ctx) {
    err_t err = NO_ERROR;

    // get the return argument
    MIR_reg_t obj_reg;
    System_Type obj_type;
    CHECK_AND_RETHROW(stack_pop(ctx, &obj_type, &obj_reg, NULL));

    // free this entirely
            arrfree(ctx->stack.entries);
    ctx->ireg.depth = 0;
    ctx->freg.depth = 0;
    ctx->dreg.depth = 0;

    // check the object is not null
    CHECK_AND_RETHROW(jit_null_check(ctx, obj_reg, obj_type));

    // append the instruction itself
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_MOV,
                                 MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                 MIR_new_reg_op(mir_ctx, obj_reg)));

    // throw it
    CHECK_AND_RETHROW(jit_throw(ctx, obj_type, false));

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------

err_t jit_emit_rethrow(jit_method_context_t* ctx) {
    err_t err = NO_ERROR;

    // only permitted inside a catch handler
    bool found_handler = true;
    System_Type catchType = NULL;
    System_Reflection_ExceptionHandlingClause_Array exceptions = body->ExceptionHandlingClauses;
    for (int i = 0; i < exceptions->Length; i++) {
        System_Reflection_ExceptionHandlingClause clause = exceptions->Data[i];

        // check if we are in this handler
        if (clause->HandlerOffset <= ctx->il_offset && ctx->il_offset < clause->HandlerOffset + clause->HandlerLength) {
            // catch clause
            if (clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
                catchType = clause->CatchType;
                found_handler = true;
                break;
            }

            // filter clause
            if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
                found_handler = true;
                break;
            }
        }
    }

    CHECK(found_handler);

    // we should already have the exception_reg filled

    // emit the rethrow
    CHECK_AND_RETHROW(jit_throw(ctx, catchType, true));

cleanup:
    return err;
}


