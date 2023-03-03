#include "../jit_internal.h"
#include "util/except.h"
#include "dotnet/exception.h"

#ifdef JIT_TRACE_MIR
#define MIR_append_insn(...) MIR_append_insn_output(__VA_ARGS__)
#endif

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Actual exception throwing code
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Resolves the label of the exception clause, making sure it looks valid
 */
static err_t jit_resolve_address_raw(jit_method_context_t* ctx, int offset, MIR_label_t* label) {
    err_t err = NO_ERROR;

    // get the stack frame
    int i = hmgeti(ctx->pc_to_stack_snapshot, offset);
    CHECK(i != -1);
    *label = ctx->pc_to_stack_snapshot[i].label;

cleanup:
    return err;
}

err_t jit_emit_try(jit_method_context_t* ctx, System_Reflection_ExceptionHandlingClause clause) {
    err_t err = NO_ERROR;

    // push a new frame
    // TODO: maybe have the frame know what the clause it is doing is, and just
    MIR_reg_t frame = jit_new_temp_reg(ctx, tSystem_UIntPtr);
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_ALLOCA,
                                 MIR_new_reg_op(mir_ctx, frame),
                                 MIR_new_int_op(mir_ctx, sizeof(exception_frame_t))));
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_call_insn(mir_ctx, 4,
                                      MIR_new_ref_op(mir_ctx, m_exception_set_frame_proto),
                                      MIR_new_ref_op(mir_ctx, m_exception_set_frame_func),
                                      MIR_new_reg_op(mir_ctx, frame),
                                      MIR_new_reg_op(mir_ctx, frame)));

    // if the frame returned 0 then we don't need to run the exception
    // handling code, otherwise we do need to run it
    MIR_label_t no_exception = MIR_new_label(mir_ctx);
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_BF,
                                 MIR_new_label_op(mir_ctx, no_exception),
                                 MIR_new_reg_op(mir_ctx, frame)));

    switch (clause->Flags) {
        case COR_ILEXCEPTION_CLAUSE_FAULT:
        case COR_ILEXCEPTION_CLAUSE_FINALLY: {
                // for fault/finally just jump to the handler
                MIR_label_t label;
                CHECK_AND_RETHROW(jit_resolve_address_raw(ctx, clause->HandlerOffset, &label));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_JMP,
                                             MIR_new_label_op(mir_ctx, label)));
        } break;

        case COR_ILEXCEPTION_CLAUSE_FILTER: {
//                // for the filter just jump to the filter
//                // itself, the endfilter will optionally jump to the handler if needed
            MIR_label_t label;
            CHECK_AND_RETHROW(jit_resolve_address_raw(ctx, clause->FilterOffset, &label));
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_JMP,
                                         MIR_new_label_op(mir_ctx, label)));
        } break;

        // TODO: in theory the exception_throw can do this for us, so we won't
        //       have to check for the different cases on our own, but that will
        //       be done at another time
        case COR_ILEXCEPTION_CLAUSE_EXCEPTION: {
            // get the current exception so we can handle it, put
            // it in the exception register so if it is the code
            // in the handler will already have access to it
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_call_insn(mir_ctx, 3,
                                              MIR_new_ref_op(mir_ctx, m_exception_get_proto),
                                              MIR_new_ref_op(mir_ctx, m_exception_get_func),
                                              MIR_new_reg_op(mir_ctx, ctx->exception_reg)));

            // check if the exception is something we want to handle or not
            MIR_reg_t result = jit_new_temp_reg(ctx, tSystem_Boolean);
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_call_insn(mir_ctx, 5,
                                              MIR_new_ref_op(mir_ctx, m_is_instance_proto),
                                              MIR_new_ref_op(mir_ctx, m_is_instance_func),
                                              MIR_new_reg_op(mir_ctx, result),
                                              MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                              MIR_new_ref_op(mir_ctx, clause->CatchType->MirType)));

            // now jump only if the is instance was true
            MIR_label_t label;
            CHECK_AND_RETHROW(jit_resolve_address_raw(ctx, clause->HandlerOffset, &label));
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_BT,
                                         MIR_new_label_op(mir_ctx, label),
                                         MIR_new_reg_op(mir_ctx, result)));

            // if it was false, just rethrow, which will pop the current
            // frame and pass the exception to the next one
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_call_insn(mir_ctx, 2,
                                              MIR_new_ref_op(mir_ctx, m_exception_rethrow_proto),
                                              MIR_new_ref_op(mir_ctx, m_exception_rethrow_func)));
        } break;
    }

    // skip all the exception throwing on the non-throwing path
    MIR_append_insn(mir_ctx, mir_func, no_exception);


cleanup:
    return err;
}

err_t jit_throw_new(jit_method_context_t* ctx, System_Type type) {
    err_t err = NO_ERROR;

    // call the default ctor
    System_Reflection_MethodInfo ctor = NULL;
    for (int i = 0; i < type->Methods->Length; i++) {
        System_Reflection_MethodInfo mi = type->Methods->Data[i];
        if (method_is_static(mi)) continue;
        if (!method_is_special_name(mi) || !method_is_rt_special_name(mi)) continue;
        if (!string_equals_cstr(mi->Name, ".ctor")) continue;
        if (mi->Parameters->Length != 0) continue;
        if (mi->ReturnType != NULL) continue;
        ctor = mi;
        break;
    }
    CHECK(ctor != NULL);

    // prepare the ctor
    CHECK_AND_RETHROW(jit_prepare_method(ctx->ctx, ctor));

    // the temp reg for the new obejct
    MIR_reg_t exception_obj = jit_new_temp_reg(ctx, type);

    // allocate the new object
    CHECK_AND_RETHROW(jit_new(ctx, exception_obj, type, MIR_new_int_op(mir_ctx, type->ManagedSize)));

    // call the ctor for it
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_call_insn(mir_ctx, 3,
                                      MIR_new_ref_op(mir_ctx, ctor->MirProto),
                                      MIR_new_ref_op(mir_ctx, ctor->MirFunc),
                                      MIR_new_reg_op(mir_ctx, exception_obj)));

    // throw the newly created exception
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_call_insn(mir_ctx, 3,
                                      MIR_new_ref_op(mir_ctx, m_exception_throw_proto),
                                      MIR_new_ref_op(mir_ctx, m_exception_throw_func),
                                      MIR_new_reg_op(mir_ctx, exception_obj)));

cleanup:
    return err;
}

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
    bool cleared_exception = false;

    CHECK(ctx->filter_clause == NULL);

    // first find where we are coming from
    int our_clause = -1;
    for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
        System_Reflection_ExceptionHandlingClause clause = body->ExceptionHandlingClauses->Data[i];

        // we are inside this handler, so we are exiting from a handler
        if (clause->HandlerOffset <= ctx->il_offset && ctx->il_offset < clause->HandlerOffset + clause->HandlerLength) {
            CHECK(clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION || clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER);

            // we found where we are
            our_clause = i;
            break;
        }

        // we are inside the try region, so this is where we are exiting from
        if (clause->TryOffset <= ctx->il_offset && ctx->il_offset < clause->TryOffset + clause->TryLength) {
            our_clause = i;
            break;
        }
    }

    // make sure we found it
    CHECK(our_clause != -1);

    if (body->ExceptionHandlingClauses->Data[our_clause]->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
        // Clear the current exception and pop our frame only if we use the leave to exit
        // from a catch block, if we exit from a try block we don't care *yet*
        cleared_exception = true;
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_call_insn(mir_ctx, 2,
                                          MIR_new_ref_op(mir_ctx, m_exception_clear_proto),
                                          MIR_new_ref_op(mir_ctx, m_exception_clear_func)));
    }

    // resolve the label we are going to
    MIR_label_t label = NULL;
    int stacki = hmgeti(ctx->pc_to_stack_snapshot, operand_i32);
    if (stacki == -1) {
        // no one goes here yet, set an empty stack entry
        label = MIR_new_label(mir_ctx);
        stack_snapshot_t snapshot = {
            .key = operand_i32,
            .label = label,
            .stack = { NULL },
            .ireg_depth = 0,
            .freg_depth = 0,
            .dreg_depth = 0,
        };
        hmputs(ctx->pc_to_stack_snapshot, snapshot);
    } else {
        // someone already is in here, make sure the stack is empty
        CHECK(arrlen(ctx->pc_to_stack_snapshot[stacki].stack.entries) == 0);
        label = ctx->pc_to_stack_snapshot[stacki].label;
    }

    // now we need to find to which finally which are jumping out of
    System_Reflection_ExceptionHandlingClause first_clause = NULL;
    System_Reflection_ExceptionHandlingClause last_clause = NULL;
    for (int i = our_clause; i < body->ExceptionHandlingClauses->Length; i++) {
        System_Reflection_ExceptionHandlingClause clause = body->ExceptionHandlingClauses->Data[i];

        // not a finally, we don't care
        if (clause->Flags != COR_ILEXCEPTION_CLAUSE_FINALLY) {
            continue;
        }

        bool source_in_try = clause->TryOffset <= ctx->il_offset && ctx->il_offset < clause->TryOffset + clause->TryLength;
        bool target_in_try = clause->TryOffset <= operand_i32 && operand_i32 < clause->TryOffset + clause->TryLength;

        // we are going inside the same finally try block, meaning we don't care about
        // this and we can exit right away
        if (source_in_try && target_in_try)
            break;

        // this is not a finally we care about, we looked through everything
        // we do care about
        if (!source_in_try)
            break;

        // now we are in a position where we know that the source is inside the try and the target
        // is not inside the try, meaning we need to run this exception handler

        if (last_clause != NULL) {
            // we have a last clause, it should jump
            // to this clause instead of where it was
            // supposed to be before that
            MIR_label_t current_clause_label;
            CHECK_AND_RETHROW(jit_resolve_address_raw(ctx, clause->HandlerOffset, &current_clause_label));

            hmgets(ctx->finally_chain, last_clause).new_label = current_clause_label;
        }

        // resolve the label of the finally handler
        int clausei = hmgeti(ctx->finally_chain, clause);
        if (clausei == -1) {
            char reg_name[128];
            snprintf(reg_name, sizeof(reg_name), "selector%d", i);

            // this finally does not know where to go
            // next, set it
            finally_chain_t chain = (finally_chain_t){
                .key = clause,
                .labels = NULL,
                .selector = MIR_new_func_reg(mir_ctx, mir_func->u.func, MIR_T_I64, reg_name),
                .new_label = label,
            };
            hmputs(ctx->finally_chain, chain);
        } else {
            // this exists, set that the label should be something new
            CHECK(ctx->finally_chain[clausei].new_label == NULL);
            ctx->finally_chain[clausei].new_label = label;
        }

        // this is the one we actually want to jump to
        if (first_clause == NULL) {
            first_clause = clause;
        }

        // set this as the last handler
        last_clause = clause;
   }

    // now that we have figured all of this out, we just need to make sure that all of the
    if (first_clause != NULL) {
        // merge all the should be entries
        for (int i = our_clause; i < body->ExceptionHandlingClauses->Length; i++) {
            finally_chain_t* chain = hmgetp_null(ctx->finally_chain, body->ExceptionHandlingClauses->Data[i]);
            if (chain == NULL || chain->new_label == NULL) {
                continue;
            }

            // check which of these we need for this one
            int at = -1;
            for (int j = 0; j < arrlen(chain->labels); j++) {
                if (chain->labels[j] == chain->new_label) {
                    at = j;
                    break;
                }
            }

            // this is a new entry, add it
            if (at == -1) {
                CHECK(!chain->done);
                at = arrlen(chain->labels);
                arrpush(chain->labels, chain->new_label);
            }

            // set the selector
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_reg_op(mir_ctx, chain->selector),
                                         MIR_new_int_op(mir_ctx, at)));

            // we handled this merge
            chain->new_label = NULL;
        }

        // now resolve the jump to the first clause instead
        // of going directly outside
        CHECK_AND_RETHROW(jit_resolve_address_raw(ctx, first_clause->HandlerOffset, &label));
    } else if (!cleared_exception) {
        // if we are jumping out of a try-block but we have no surrounding finally block
        // then we do need to clear our own exception
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_call_insn(mir_ctx, 2,
                                          MIR_new_ref_op(mir_ctx, m_exception_clear_proto),
                                          MIR_new_ref_op(mir_ctx, m_exception_clear_func)));
    }

    // finally jump to the label for the next clause/to
    // go outside
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_JMP,
                                 MIR_new_label_op(mir_ctx, label)));

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------

err_t jit_emit_endfinally(jit_method_context_t* ctx) {
    err_t err = NO_ERROR;
    MIR_op_t* switch_ops = NULL;

    // we are done, first of all check pop our frame and try to
    // rethrow the current exception if any, if we don't have any
    // then we will simply go on to go to the next finally
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_call_insn(mir_ctx, 2,
                                      MIR_new_ref_op(mir_ctx, m_exception_rethrow_proto),
                                      MIR_new_ref_op(mir_ctx, m_exception_rethrow_func)));

    // we need to figure the surrounding catch handler, and figure what to do with it when the exception is thrown
    bool found = false;
    finally_chain_t* chain = NULL;
    for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
        System_Reflection_ExceptionHandlingClause clause = body->ExceptionHandlingClauses->Data[i];

        // skip any clause we are not inside
        if (clause->HandlerOffset > ctx->il_offset || clause->HandlerOffset + clause->HandlerLength < ctx->il_offset) {
            continue;
        }

        // we are inside of this handler, it must be either finally or fault
        CHECK(
            clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT ||
            clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY
        );

        // we found our handler!
        found = true;

        // try to see if the success has somewhere to go to
        chain = hmgetp_null(ctx->finally_chain, clause);

        break;
    }

    // check we found our handler
    CHECK(found);

    // for finally we have the success case which we need to handle, but
    // even for that we have cases that there is an unconditional throw inside
    // the try-finally handler, so we might unconditionally rethrow
    if (chain != NULL) {
        if (chain->labels != NULL) {
            if (arrlen(chain->labels) == 1) {
                // there is a single entry, no need for a switch
                // go to the success label
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_JMP,
                                             MIR_new_label_op(mir_ctx, chain->labels[0])));
            } else {
                // we have multiple possibilities, use a switch
                // allocate enough space for the ops
                switch_ops = malloc((arrlen(chain->labels) + 1) * sizeof(MIR_op_t));

                // branch selector
                switch_ops[0] = MIR_new_reg_op(mir_ctx, chain->selector);

                // all the locations
                for (int i = 0; i < arrlen(chain->labels); i++) {
                    switch_ops[i + 1] = MIR_new_label_op(mir_ctx, chain->labels[i]);
                }

                // and do it
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn_arr(mir_ctx, MIR_SWITCH,
                                                 arrlen(chain->labels) + 1,
                                                 switch_ops));
            }
        }

        // this can not be changed anymore
        chain->done = true;
    }

cleanup:
    SAFE_FREE(switch_ops);

    return err;
}

//----------------------------------------------------------------------------------------------------------------------

err_t jit_emit_endfilter(jit_method_context_t* ctx) {
    err_t err = NO_ERROR;

    CHECK(ctx->filter_clause != NULL);

    JIT_TRACE(
        TRACE("%*s} // end filter", ctx->jit_trace_indent, "");
        ctx->jit_trace_indent -= 4;
    );

    // pop from the stack the result
    System_Type value_type;
    MIR_reg_t value_reg;
    CHECK_AND_RETHROW(jit_stack_pop(ctx, &value_type, &value_reg, NULL));

    // make sure this is valid
    CHECK(value_type == tSystem_Int32);

    // resolve the handler
    MIR_label_t label;
    CHECK_AND_RETHROW(jit_resolve_address_raw(ctx, ctx->filter_clause->HandlerOffset, &label));

    // if the value is not zero, jump to the handler, otherwise we are going to rethrow,
    // NOTE: in theory we should only jump to the handler when it is equals to 1, but we are
    //       going to use the fact that "The result of using any other integer value is unspecified"
    //       and just make it jump on not zero
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_BTS,
                                 MIR_new_label_op(mir_ctx, label),
                                 MIR_new_reg_op(mir_ctx, value_reg)));

    // if we got to here then we got a zero, meaning we need to rethrow
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_call_insn(mir_ctx, 2,
                                      MIR_new_ref_op(mir_ctx, m_exception_rethrow_proto),
                                      MIR_new_ref_op(mir_ctx, m_exception_rethrow_func)));

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
    CHECK_AND_RETHROW(jit_stack_pop(ctx, &obj_type, &obj_reg, NULL));

    // append the instruction itself
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_call_insn(mir_ctx, 3,
                                      MIR_new_ref_op(mir_ctx, m_exception_throw_proto),
                                      MIR_new_ref_op(mir_ctx, m_exception_throw_func),
                                      MIR_new_reg_op(mir_ctx, obj_reg)));

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------

err_t jit_emit_rethrow(jit_method_context_t* ctx) {
    err_t err = NO_ERROR;

    // only permitted inside a catch handler
    bool found_handler = true;
    for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
        System_Reflection_ExceptionHandlingClause clause = body->ExceptionHandlingClauses->Data[i];

        // check if we are in this handler
        if (clause->HandlerOffset <= ctx->il_offset && ctx->il_offset < clause->HandlerOffset + clause->HandlerLength) {
            // catch clause
            if (clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
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

    // emit the rethrow
    MIR_append_insn(mir_ctx, mir_func,
                   MIR_new_call_insn(mir_ctx, 2,
                                     MIR_new_ref_op(mir_ctx, m_exception_rethrow_proto),
                                     MIR_new_ref_op(mir_ctx, m_exception_rethrow_func)));

cleanup:
    return err;
}


