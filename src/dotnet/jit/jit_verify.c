//
// Created by tomato on 10/4/24.
//

#include "jit_verify.h"

#include <stdlib.h>
#include <dotnet/types.h>
#include <tomatodotnet/disasm.h>
#include <util/except.h>
#include <util/stb_ds.h>
#include <util/string.h>

#include "jit_basic_block.h"
#include "jit_emit.h"

/**
 * The methods left to be verified
 */
static jit_method_t** m_methods_to_verify = NULL;

static void jit_queue_verify(jit_method_t* method) {
    if (!method->verifying) {
        method->verifying = true;

        arrpush(m_methods_to_verify, method);
    }
}

static tdn_err_t jit_queue_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // if we already jitted the instance then we can ignore this
    if (type->JitStartedInstance) {
        goto cleanup;
    }
    type->JitStartedInstance = true;

    // if not we are going to queue all the methods
    for (int i = 0; i < type->VTable->Length; i++) {
        jit_method_t* jit_method;
        CHECK_AND_RETHROW(jit_get_or_create_method((RuntimeMethodBase)type->VTable->Elements[i], &jit_method));
        jit_queue_verify(jit_method);
    }

    // and finally the type itself
    jit_queue_emit_type(type);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Verify types match
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static bool verifier_is_assignable(RuntimeTypeInfo src, RuntimeTypeInfo dst) {
    if (src == dst) {
        return true;
    }

    if (tdn_type_is_referencetype(src)) {
        if (!tdn_type_is_referencetype(dst)) {
            return false;
        }

        if (src == NULL) {
            return true;
        }

        // if the dst is an interface, check that we can
        if (dst->Attributes.Interface) {
            return (src->JitVTable->InterfaceProduct % dst->TypeId) == 0;
        }

        // check that the object can be lowered to the dest object
        // TODO: when we implemented the normal type ids then use that instead
        do {
            if (dst == src) {
                return true;
            }
            src = src->BaseType;
        } while (src != NULL);
    }

    return false;
}

static bool verifier_is_binary_comparable(RuntimeTypeInfo src, RuntimeTypeInfo dst, tdn_il_opcode_t op) {
    if (tdn_type_is_referencetype(src)) {
        // can only compare reference type to another one
        if (!tdn_type_is_referencetype(dst)) {
            return false;
        }

        return op == CEE_BEQ || op == CEE_BNE_UN || op == CEE_CEQ || op == CEE_CGT_UN;

    }

    if (src->IsByRef) {
        // can only compare refs with another ref
        if (!dst->IsByRef) {
            return false;
        }

        return true;
    }

    // can compare int32 only to native int and itself
    if (src == tInt32) {
        return dst == tInt32 || dst == tIntPtr;
    }

    // can only compare int64 to int64
    if (src == tInt64) {
        return dst == tInt64;
    }

    // can compare intptr to int32 and intptr
    if (src == tIntPtr) {
        return dst == tInt32 || dst == tIntPtr;
    }

    // anything else is not comparable
    return false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Merge basic blocks
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static bool jit_merge_attrs(jit_item_attrs_t* wanted, jit_item_attrs_t* actual) {
    bool modified = false;

    // if known type changed then change it and mark for another verification run
    if (wanted->known_type != NULL) {
        if (wanted->known_type != actual->known_type) {
            wanted->known_type = NULL;
            modified = true;
        }
    }

    // wanted readable, got non-readable, re-verify
    if (wanted->readonly && !actual->readonly) {
        wanted->readonly = false;
        modified = true;
    }

    // and same for non-local
    if (wanted->nonlocal_ref && !actual->nonlocal_ref) {
        wanted->nonlocal_ref = false;
        modified = true;
    }

    return modified;
}

static void verify_queue_basic_block(jit_method_t* ctx, jit_basic_block_t* block) {
    if (block->state != JIT_BLOCK_PENDING_VERIFY) {
        block->state = JIT_BLOCK_PENDING_VERIFY;

        long bi = block - ctx->basic_blocks;
        arrpush(ctx->block_queue, bi);
    }
}

static tdn_err_t verify_merge_basic_block(jit_method_t* method, uint32_t target_pc, jit_stack_value_t* stack, jit_item_attrs_t* locals) {
    tdn_err_t err = TDN_NO_ERROR;

    int bi = hmgeti(method->labels, target_pc);
    CHECK(bi != 0);
    jit_basic_block_t* target = &method->basic_blocks[method->labels[bi].value];

    // if not initialized yet then initialize it now
    if (!target->initialized) {
        // copy locals state
        arrsetlen(target->locals, arrlen(locals));
        memcpy(target->locals, locals, sizeof(*locals) * arrlen(locals));

        // copy stack state
        arrsetlen(target->stack, arrlen(stack));
        memcpy(target->stack, stack, arrlen(stack) * sizeof(*target->stack));

        // queue it
        verify_queue_basic_block(method, target);

        target->initialized = true;
    } else {

        // make sure both have the same stack length
        CHECK(arrlen(stack) == arrlen(target->stack));

        // already initialized, make sure the state is consistent, if not
        // mark the target for another pass
        for (int i = 0; i < arrlen(stack); i++) {
            jit_stack_value_t* wanted = &target->stack[i];
            jit_stack_value_t* actual = &stack[i];

            // make sure both have the same type on the stack
            // TODO: support finding the common of the two
            CHECK(wanted->type == actual->type);

            // merge the attributes
            if (jit_merge_attrs(&wanted->attrs, &actual->attrs)) {
                verify_queue_basic_block(method, target);
            }
        }

        target->needs_phi = true;

        // merge the locals attributes
        for (int i = 0; i < arrlen(locals); i++) {
            jit_item_attrs_t* wanted = &target->locals[i];
            jit_item_attrs_t* actual = &locals[i];
            if (jit_merge_attrs(wanted, actual)) {
                verify_queue_basic_block(method, target);
            }
        }
    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Perform verification on a single basic block
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static bool verifier_assignable_to(RuntimeTypeInfo Q, RuntimeTypeInfo R) {
    RuntimeTypeInfo T = tdn_get_verification_type(Q);
    RuntimeTypeInfo U = tdn_get_verification_type(R);

    // 9. T is null type, and U is reference type, we perform
    //    this first to make sure that no null deref happens from
    //    the null type
    if (T == NULL) {
        if (tdn_type_is_referencetype(U)) {
            return true;
        }

        return false;
    }

    return tdn_type_assignable_to(T, U);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Perform verification on a single basic block
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum il_prefix {
    IL_PREFIX_CONSTRAINED = BIT0,
    IL_PREFIX_VOLATILE = BIT1,
} il_prefix_t;

#define EVAL_STACK_PUSH(...) \
    do { \
        CHECK(arrlen(stack) < body->MaxStackSize); \
        jit_stack_value_t __item = { __VA_ARGS__ }; \
        arrpush(stack, __item); \
    } while (0)

#define EVAL_STACK_POP() \
    ({ \
        CHECK(arrlen(stack) > 0); \
        arrpop(stack); \
    })

#define GET_ARG_TYPE(_index) \
    ({ \
        typeof(_index) __index = _index; \
        RuntimeTypeInfo __arg_type; \
        if (this_type != NULL) { \
            if (__index == 0) { \
                __arg_type = this_type; \
            } else { \
                __index--; \
                CHECK(__index < method->Parameters->Length); \
                __arg_type = method->Parameters->Elements[__index]->ParameterType; \
            } \
        } else { \
            CHECK(__index < method->Parameters->Length); \
            __arg_type = method->Parameters->Elements[__index]->ParameterType; \
        } \
        __arg_type; \
    })

#define GET_ARG_PARAMETER(_index) \
    ({ \
        typeof(_index) __index = _index; \
        ParameterInfo __arg; \
        if (this_type != NULL) { \
            if (__index == 0) { \
                __arg = NULL; \
            } else { \
                __index--; \
                CHECK(__index < method->Parameters->Length); \
                __arg = method->Parameters->Elements[__index]; \
            } \
        } else { \
            CHECK(__index < method->Parameters->Length); \
            __arg = method->Parameters->Elements[__index]; \
        } \
        __arg; \
    })


static tdn_err_t verify_basic_block(jit_method_t* jmethod, jit_basic_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = jmethod->method;
    RuntimeMethodBody body = method->MethodBody;

    // pre-mark as verified, if we jump to ourselves we might need
    // to re-queue the block
    block->state = JIT_BLOCK_VERIFIED;

    // the context
    jit_stack_value_t* stack = NULL;
    jit_item_attrs_t* locals = NULL;
    RuntimeTypeInfo this_type = NULL;

    // figure the this type if this is a non-static method
    if (!method->Attributes.Static) {
        this_type = jmethod->args[0].type;
    }

    // initialize the context
    // if this block was jumped to before then the locals would be non-null and we would need
    // to initialize from it
    if (body->LocalVariables != NULL) {
        arrsetlen(locals, body->LocalVariables->Length);
    }

    if (block->initialized) {
        // copy the initial locals state
        memcpy(locals, block->locals, sizeof(*locals) * arrlen(locals));

        // copy the initial stack
        arrsetlen(stack, arrlen(block->stack));
        memcpy(stack, block->stack, arrlen(block->stack) * sizeof(*block->stack));
    } else {
        // start with no attributes on any local
        memset(locals, 0, sizeof(*locals) * arrlen(locals));

        // also initialize the locals in the block
        arrsetlen(block->locals, arrlen(locals));
        memset(block->locals, 0, sizeof(*block->locals) * arrlen(block->locals));
    }
    block->initialized = true;

#ifdef JIT_VERBOSE_VERIFY
    int indent = 0;
#endif

    // the pending prefixes
    il_prefix_t pending_prefix = 0;

    // get the pc
    tdn_il_inst_t inst = { .control_flow = TDN_IL_CF_FIRST };
    uint32_t pc = block->start;
    while (pc < block->end) {
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

        tdn_normalize_inst(&inst);
        pc += inst.length;

        switch (inst.opcode) {

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arguments
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STARG: {
                int index = inst.operand.variable;
                RuntimeTypeInfo arg_type = GET_ARG_TYPE(index);

                // verify the type
                jit_stack_value_t value = EVAL_STACK_POP();
                CHECK(verifier_assignable_to(value.type, arg_type));

                // don't allow to modify this
                if (this_type != NULL) {
                    CHECK(index != 0);
                }

                // for simplicity, don't allow to
                // store to a byref parameter
                CHECK(!arg_type->IsByRef);
            }  break;

            case CEE_LDARG: {
                int index = inst.operand.variable;
                RuntimeTypeInfo arg_type = GET_ARG_TYPE(index);
                ParameterInfo arg_param = GET_ARG_PARAMETER(index);
                arg_type = tdn_get_intermediate_type(arg_type);

                // figure the attributes for the
                // argument, if it has a
                jit_item_attrs_t attrs = {};

                // if this is a ref argument then it is nonlocal
                if (arg_type->IsByRef) {
                    attrs.nonlocal_ref = true;

                    // check if the reference is readonly
                    if (arg_param != NULL) {
                        attrs.readonly = arg_param->IsReadonly;
                    } else {
                        attrs.readonly = method->DeclaringType->IsReadOnly;
                    }
                }

                EVAL_STACK_PUSH(arg_type, attrs);
            }  break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Locals
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STLOC: {
                int index = inst.operand.variable;
                CHECK(index < arrlen(locals));
                RuntimeLocalVariableInfo local = body->LocalVariables->Elements[index];
                jit_item_attrs_t attrs = locals[index];

                // verify the type
                jit_stack_value_t value = EVAL_STACK_POP();
                CHECK(verifier_assignable_to(value.type, local->LocalType));

                // set the new local attributes
                locals[index] = attrs;
            } break;

            case CEE_LDLOC: {
                int index = inst.operand.variable;
                CHECK(index < arrlen(locals));
                RuntimeLocalVariableInfo local = body->LocalVariables->Elements[index];
                jit_item_attrs_t attrs = locals[index];

                RuntimeTypeInfo type = tdn_get_intermediate_type(local->LocalType);
                EVAL_STACK_PUSH(type, attrs);
            } break;

            case CEE_LDLOCA: {
                int index = inst.operand.variable;
                CHECK(index < arrlen(locals));
                RuntimeLocalVariableInfo local = body->LocalVariables->Elements[index];

                RuntimeTypeInfo type = tdn_get_verification_type(local->LocalType);
                CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));

                // its only writable if the struct
                // is writable in the first place
                // its always readable because its
                // coming from the stack
                jit_item_attrs_t attrs = {
                    .readonly = type->IsReadOnly,
                };

                EVAL_STACK_PUSH(type, attrs);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Stack manipulation
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDC_I4: {
                EVAL_STACK_PUSH(tInt32);
            } break;

            case CEE_LDC_I8: {
                EVAL_STACK_PUSH(tInt64);
            } break;

            case CEE_LDNULL: {
                EVAL_STACK_PUSH(NULL);
            } break;

            case CEE_LDSTR: {
                EVAL_STACK_PUSH(tString);
            } break;

            case CEE_POP: {
                EVAL_STACK_POP();
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Math
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_CEQ:
            case CEE_CGT:
            case CEE_CGT_UN:
            case CEE_CLT:
            case CEE_CLT_UN: {
                jit_stack_value_t value2 = EVAL_STACK_POP();
                jit_stack_value_t value1 = EVAL_STACK_POP();
                CHECK(verifier_is_binary_comparable(value1.type, value2.type, inst.opcode));
                EVAL_STACK_PUSH(tInt32);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Branching
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_BEQ:
            case CEE_BGE:
            case CEE_BGT:
            case CEE_BLE:
            case CEE_BLT:
            case CEE_BNE_UN:
            case CEE_BGE_UN:
            case CEE_BGT_UN:
            case CEE_BLE_UN:
            case CEE_BLT_UN: {
                jit_stack_value_t value2 = EVAL_STACK_POP();
                jit_stack_value_t value1 = EVAL_STACK_POP();
                CHECK(verifier_is_binary_comparable(value1.type, value2.type, inst.opcode));

                CHECK_AND_RETHROW(verify_merge_basic_block(
                    jmethod,
                    inst.operand.branch_target,
                    stack, locals));

                CHECK_AND_RETHROW(verify_merge_basic_block(
                    jmethod,
                    pc,
                    stack, locals));
            } break;

            case CEE_BRTRUE:
            case CEE_BRFALSE: {
                jit_stack_value_t value = EVAL_STACK_POP();
                CHECK(
                    value.type == tInt32 ||
                    value.type == tInt64 ||
                    value.type == tIntPtr ||
                    tdn_type_is_referencetype(value.type)
                );

                CHECK_AND_RETHROW(verify_merge_basic_block(
                    jmethod,
                    inst.operand.branch_target,
                    stack, locals));

                CHECK_AND_RETHROW(verify_merge_basic_block(
                    jmethod,
                    pc,
                    stack, locals));
            } break;

            case CEE_BR: {
                CHECK_AND_RETHROW(verify_merge_basic_block(
                    jmethod,
                    inst.operand.branch_target,
                    stack, locals));
            } break;

            case CEE_RET: {
                RuntimeTypeInfo type = method->ReturnParameter->ParameterType;

                if (type != tVoid) {
                    jit_stack_value_t ret_val = EVAL_STACK_POP();

                    // if we return a non-readonly then make sure
                    // that the return is also not readonly
                    if (!method->ReturnParameter->IsReadonly) {
                        CHECK(!ret_val.attrs.readonly);
                    }

                    // check the type is the same
                    CHECK(verifier_assignable_to(ret_val.type, type));
                }

                CHECK(arrlen(stack) == 0);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Misc
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // nothing to do
            case CEE_NOP: break;

            default: CHECK_FAIL("Unknown opcode `%s`", tdn_get_opcode_name(inst.opcode));
        }

        // if this was not a meta instruction (prefix) then make sure we have
        // no pending prefixes
        if (inst.control_flow != TDN_IL_CF_META) {
            CHECK(pending_prefix == 0);
        }

#ifdef JIT_VERBOSE_VERIFY
        indent = tdn_disasm_print_end(body, pc, indent);
#endif
    }

    // we have a fallthrough
    if (inst.control_flow == TDN_IL_CF_NEXT) {
        CHECK_AND_RETHROW(verify_merge_basic_block(jmethod, pc, stack, locals));
    }

    // last must be a valid instruction
    CHECK(
        inst.control_flow != TDN_IL_CF_FIRST &&
        inst.control_flow != TDN_IL_CF_META
    );

cleanup:
    arrfree(stack);
    arrfree(locals);

    return err;
}

static tdn_err_t prepare_method(jit_method_t* jmethod) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = jmethod->method;
    RuntimeMethodBody body = method->MethodBody;

    // start by finding all the basic blocks so we can verify the method
    CHECK_AND_RETHROW(jit_find_basic_blocks(jmethod));

    // if we have a this add it to the local
    if (!method->Attributes.Static) {
        RuntimeTypeInfo this_type = method->DeclaringType;
        if (tdn_type_is_valuetype(this_type)) {
            CHECK_AND_RETHROW(tdn_get_byref_type(this_type, &this_type));
        }
        arrpush(jmethod->args, (jit_arg_t){ .type = this_type });
    }

    // now prepare the rest of the arguments
    for (int i = 0; i < method->Parameters->Length; i++) {
        ParameterInfo info = method->Parameters->Elements[i];
        arrpush(jmethod->args, (jit_arg_t){ .type = info->ParameterType });
    }

    // and now prepare the locals
    if (body->LocalVariables != NULL) {
        for (int i = 0; i < body->LocalVariables->Length; i++) {
            RuntimeLocalVariableInfo info = body->LocalVariables->Elements[i];
            arrpush(jmethod->locals, (jit_local_t){ .type = info->LocalType });
        }
    }

cleanup:
    return err;
}

static tdn_err_t verify_method(jit_method_t* method) {
    tdn_err_t err = TDN_NO_ERROR;

#ifdef JIT_DEBUG_VERIFY
    TRACE("%T::%U", method->method->DeclaringType, method->method->Name);
#endif

    CHECK_AND_RETHROW(prepare_method(method));

    // push the first block
    verify_queue_basic_block(method, &method->basic_blocks[0]);

    while (arrlen(method->block_queue) > 0) {
        int bi = arrpop(method->block_queue);

#ifdef JIT_VERBOSE_VERIFY
        TRACE("\tBlock %d", bi);
#endif
        CHECK_AND_RETHROW(verify_basic_block(method, &method->basic_blocks[bi]));
    }

cleanup:
    arrfree(method->block_queue);

    return err;
}

static tdn_err_t verify_all_methods(void) {
    tdn_err_t err = TDN_NO_ERROR;

    while (arrlen(m_methods_to_verify) > 0) {
        jit_method_t* method = arrpop(m_methods_to_verify);
        CHECK_AND_RETHROW(verify_method(method));
    }

cleanup:
    return err;
}

tdn_err_t jit_verify_method(RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;

    // queue the method
    jit_method_t* jit_method;
    CHECK_AND_RETHROW(jit_get_or_create_method(method, &jit_method));
    jit_queue_verify(jit_method);

    // verify it and all the called methods
    CHECK_AND_RETHROW(verify_all_methods());

cleanup:
    return err;
}

tdn_err_t jit_verify_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // queue all the instance methods
    jit_queue_type(type);

    // verify it and all the called methods
    CHECK_AND_RETHROW(verify_all_methods());

cleanup:
    return err;
}
