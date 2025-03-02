#include "jit_verify.h"

#include <dotnet/types.h>
#include <tomatodotnet/disasm.h>
#include <util/alloc.h>
#include <util/except.h>
#include <util/stb_ds.h>
#include <util/string.h>

#include "jit_basic_block.h"
#include "jit_emit.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Merge basic blocks
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static uint32_t* copy_leave_targets(uint32_t* targets) {
    if (targets == NULL) {
        return NULL;
    }

    uint32_t* new = NULL;
    arrsetlen(new, arrlen(targets));
    memcpy(new, targets, sizeof(uint32_t) * arrlen(targets));
    return new;
}

static jit_basic_block_t* jit_verify_get_basic_block(
    jit_method_t* method,
    long target_pc,
    uint32_t* leave_target_stack
) {
    int bi = hmgeti(method->labels, target_pc);
    if (bi < 0) {
        return NULL;
    }
    jit_basic_block_t* block = method->labels[bi].value;

    // lookup based on the leave target
    if (leave_target_stack != NULL) {
        jit_leave_block_key_t key = {
            .block = block,
            .leave_target = arrlast(leave_target_stack)
        };

        int bi = hmgeti(method->leave_blocks, key);
        if (bi >= 0) {
            block = method->leave_blocks[bi].value;
        } else {
            jit_basic_block_t* new_block = tdn_mallocz(sizeof(jit_basic_block_t));
            if (new_block == NULL) {
                return NULL;
            }

            new_block->start = block->start;
            new_block->end = block->end;
            new_block->leave_target_stack = leave_target_stack;

            // save the new block
            hmput(method->leave_blocks, key, new_block);

            // and return the new block instead
            // of the original block
            block = new_block;
        }
    }

    return block;
}

static void jit_verify_queue_basic_block(jit_method_t* ctx, jit_basic_block_t* block) {
    if (!block->in_queue) {
        arrpush(ctx->block_queue, block);
        block->in_queue = true;
    }
}

/**
 * Merge the attributes of a single jit value
 *
 * @param current   [IN] The current attributes
 * @param incoming  [IN] The incoming attributes
 */
static bool jit_merge_attrs(jit_value_attrs_t* current, jit_value_attrs_t* incoming) {
    bool modified = false;

    // the only case we get a method in a merge is if we have a delegate with a known
    // type, so we are going to attempt to de-virt it, but if we got something else
    // then we need to re-verify
    if (current->method != NULL) {
        if (current->method != incoming->method) {
            current->method = NULL;
            modified = true;
        }
    }


    // if known type changed then change it and mark for another verification run
    // this also handles the case of two known methods not being the same
    if (current->known_type != NULL) {
        if (current->known_type != incoming->known_type) {
            current->known_type = NULL;
            modified = true;
        }
    }

    // wanted readonly, got non-readonly, re-verify
    if (current->readonly && !incoming->readonly) {
        current->readonly = false;
        modified = true;
    }

    // and same for non-local
    if (current->nonlocal_ref && !incoming->nonlocal_ref) {
        current->nonlocal_ref = false;
        modified = true;
    }

    // and same for ref-structs
    if (current->nonlocal_ref_struct && !incoming->nonlocal_ref_struct) {
        current->nonlocal_ref_struct = false;
        modified = true;
    }

    // and same for this ptr
    if (current->this_ptr && !incoming->this_ptr) {
        current->this_ptr = false;
        modified = true;
    }

    return modified;
}

static tdn_err_t jit_merge_values(jit_value_t* current, jit_value_t* incoming, bool* modified) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(arrlen(current) == arrlen(incoming));

    for (int i = 0; i < arrlen(current); i++) {
        // verify the basic type is exactly the same
        // TODO: do we need something stronger
        CHECK (current[i].type == incoming[i].type);

        // merge the attributes, if they require a re-verification then mark it as such
        if (jit_merge_attrs(&current[i].attrs, &incoming->attrs)) {
            *modified = true;
        }
    }

cleanup:
    return err;
}

static tdn_err_t jit_verify_merge_basic_block(
    jit_method_t* method,
    uint32_t target_pc,
    jit_value_t* stack, jit_value_t* locals, jit_value_t* args,
    uint32_t* leave_target_stack
) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_basic_block_t* target = jit_verify_get_basic_block(method, target_pc, leave_target_stack);
    CHECK(target != NULL);

    if (target->initialized) {
        // already initialized, verify the merge is valid, if not we will requeue the code
        bool modified = false;
        CHECK_AND_RETHROW(jit_merge_values(target->stack, stack, &modified));
        CHECK_AND_RETHROW(jit_merge_values(target->locals, locals, &modified));
        CHECK_AND_RETHROW(jit_merge_values(target->args, args, &modified));

        // mark that we saw it multiple times, meaning that we will
        // need to generate phis for everything
        target->need_phis = true;

        // if the state was modified, then re-verify it
        if (modified) {
            jit_verify_queue_basic_block(method, target);
        }
    } else {
        // first time we see this block, copy everything over

        arrsetlen(target->stack, arrlen(stack));
        memcpy(target->stack, stack, arrlen(stack) * sizeof(*stack));

        arrsetlen(target->locals, arrlen(locals));
        memcpy(target->locals, locals, arrlen(locals) * sizeof(*locals));

        arrsetlen(target->args, arrlen(args));
        memcpy(target->args, args, arrlen(args) * sizeof(*args));

        // properly queue it
        jit_verify_queue_basic_block(method, target);

        // mark as initialized
        target->initialized = true;
    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Type verifications
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static RuntimeTypeInfo direct_base_class(RuntimeTypeInfo T) {
    if (T == NULL) {
        return NULL;
    }

    if (T->IsArray) {
        return tArray;
    }

    if (T->Attributes.Interface) {
        return tObject;
    }

    if (!tdn_type_is_valuetype(T->BaseType) && T->BaseType != NULL) {
        return T->BaseType;
    }

    return NULL;
}

static bool compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U) {
    // 1. T is identical to U.
    if (T == U) {
        return true;
    }

    // 2. There exists some V such that T is compatible-to V and V is compatible-to U
    RuntimeTypeInfo V = direct_base_class(T);
    if (V != NULL) {
        return compatible_with(V, U);
    }

    // 3. T is a reference type, and U is the direct base class of T.
    if (tdn_type_is_referencetype(T) && U == direct_base_class(T)) {
        return true;
    }

    // 4. T is a reference type, and U is an interface directly implemented by T.
    if (T != NULL && U != NULL && tdn_type_is_referencetype(T) && U->Attributes.Interface) {
        for (int i = 0; i < arrlen(T->InterfaceImpls); i++) {
            if (T->InterfaceImpls[i].key == U) {
                return true;
            }
        }
    }

    // TODO: 5.

    // TODO: 6.

    // TODO: 7.

    // TODO: 8.

    // TODO: 9.

    return false;
}

static bool assignable_to(RuntimeTypeInfo T, RuntimeTypeInfo U) {
    // 1. T is identical to U.
    if (T == U) {
        return true;
    }

    // TODO: 2. There exists some V such that T is assignable-to V and V is assignable-to U

    // 3. T has intermediate type V, U has intermediate type W, and V is identical to W.
    RuntimeTypeInfo V = jit_get_intermediate_type(T);
    RuntimeTypeInfo W = jit_get_intermediate_type(U);
    if (V == W) {
        return true;
    }

    // 4. T has intermediate type native int and U has intermediate type int32, or vice-versa.
    if (
        (V == tIntPtr && W == tInt32) ||
        (V == tInt32 && W == tIntPtr)
    ) {
        return true;
    }

    // 5. T is compatible-with U.
    if (compatible_with(T, U)) {
        return true;
    }

    return false;
}

static bool verifier_assignable_to(RuntimeTypeInfo Q, RuntimeTypeInfo R) {
    RuntimeTypeInfo T = jit_get_verification_type(Q);
    RuntimeTypeInfo U = jit_get_verification_type(R);

    // 1. T is identical to U
    if (T == U) {
        return true;
    }

    // TODO: 2. There exists some V such that T is verifier-assignable-to V and V is verifier-assignable-to U

    // 3. T is assignable-to U
    if (assignable_to(T, U)) {
        return true;
    }

    // TODO: controlled-mutability pointer

    // TODO: boxed variations

    // 9. T is the null type, and U is a reference type.
    if (T == NULL && tdn_type_is_referencetype(U)) {
        return true;
    }

    return false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Perform verification on a single basic block
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum il_prefix {
    IL_PREFIX_CONSTRAINED = BIT0,
    IL_PREFIX_VOLATILE = BIT1,
    IL_PREFIX_UNALIGNED = BIT2,
} il_prefix_t;

#define EVAL_STACK_PUSH(...) \
    do { \
        CHECK(arrlen(stack) < body->MaxStackSize); \
        jit_value_t __item = { __VA_ARGS__ }; \
        arrpush(stack, __item); \
    } while (0)

#define EVAL_STACK_POP() \
    ({ \
        CHECK(arrlen(stack) > 0); \
        arrpop(stack); \
    })

static tdn_err_t jit_verify_binary_comparison_or_branch(RuntimeTypeInfo value1, RuntimeTypeInfo value2, uint32_t opcode) {
    tdn_err_t err = TDN_NO_ERROR;

    if (value1 == tInt32) {
        CHECK(value2 == tInt32);
        // TODO: allow IntPtr

    } else if (value1 == tInt64) {
        CHECK(value2 == tInt64);

    } else if (value1 == tIntPtr) {
        CHECK(value2 == tIntPtr);
        // TODO: allow Int32

    } else if (tdn_type_is_referencetype(value1)) {
        CHECK(opcode == CEE_CEQ || opcode == CEE_CGT_UN || opcode == CEE_BEQ || opcode == CEE_BNE_UN);
        CHECK(tdn_type_is_referencetype(value2));

    } else if (value1->IsByRef) {
        CHECK(value2->IsByRef);
    }

cleanup:
    return err;
}

static tdn_err_t jit_verify_basic_block(jit_method_t* jmethod, jit_basic_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = jmethod->method;
    RuntimeMethodBody body = method->MethodBody;

    // the context
    jit_value_t* stack = NULL;
    jit_value_t* locals = NULL;
    jit_value_t* args = NULL;
    RuntimeTypeInfo this_type = NULL;

    // figure the this type if this is a non-static method
    if (!method->Attributes.Static) {
        this_type = jmethod->args[0].type;
    }

    // ensure that the block we are seeing is already initialized
    // and copy over all the arrays
    CHECK(block->initialized);

    // copy the locals state
    arrsetlen(stack, arrlen(block->stack));
    memcpy(stack, block->stack, arrlen(stack) * sizeof(*stack));
    arrsetlen(locals, arrlen(block->locals));
    memcpy(locals, block->locals, arrlen(locals) * sizeof(*locals));
    arrsetlen(args, arrlen(block->args));
    memcpy(args, block->args, arrlen(args) * sizeof(*args));

#ifdef JIT_VERBOSE_VERIFY
    int indent = 0;
#endif

    // the pending prefixes
    il_prefix_t pending_prefix = 0;
    bool must_be_newobj = false;
    tdn_il_opcode_t last_opcode;

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

        // save the last opcode
        last_opcode = inst.opcode;

        // get the instruction
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));

#ifdef JIT_VERBOSE_VERIFY
        indent = tdn_disasm_print_start(body, pc, inst, indent);
#endif

        tdn_normalize_inst(&inst);
        uint32_t current_pc = pc;
        pc += inst.length;

        // we got a delegate creation sequence, so this
        // opcode must be newobj
        if (must_be_newobj) {
            CHECK(inst.opcode == CEE_NEWOBJ);
            must_be_newobj = false;
        }

        switch (inst.opcode) {

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arithmetic
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_CEQ:
            case CEE_CGT:
            case CEE_CGT_UN:
            case CEE_CLT:
            case CEE_CLT_UN: {
                jit_value_t value2 = EVAL_STACK_POP();
                jit_value_t value1 = EVAL_STACK_POP();

                // check for compatibility
                CHECK_AND_RETHROW(jit_verify_binary_comparison_or_branch(
                    value1.type, value2.type, inst.opcode
                ));

                EVAL_STACK_PUSH(tInt32);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Stack manipulation
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDNULL: {
                EVAL_STACK_PUSH(NULL);
            } break;

            case CEE_LDC_I4: {
                EVAL_STACK_PUSH(tInt32);
            } break;

            case CEE_LDC_I8: {
                EVAL_STACK_PUSH(tInt64);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arguments
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDARG: {
                CHECK(inst.operand.variable < arrlen(args));
                RuntimeTypeInfo type = jit_get_intermediate_type(args[inst.operand.variable].type);
                EVAL_STACK_PUSH(type, args[inst.operand.variable].attrs);
            } break;

            case CEE_STARG: {
                CHECK(inst.operand.variable < arrlen(args));
                jit_value_t value = EVAL_STACK_POP();

                CHECK(verifier_assignable_to(value.type, args[inst.operand.variable].type),
                    "%T verifier-assignable-to %T", value.type, args[inst.operand.variable].type);

                // just remember the attributes
                args[inst.operand.variable].attrs = value.attrs;
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Locals
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDLOC: {
                CHECK(inst.operand.variable < arrlen(locals));
                jit_value_t* local = &locals[inst.operand.variable];
                RuntimeTypeInfo type = jit_get_intermediate_type(local->type);

                // if not assigned yet, mark to zero init the variable
                // at the start of the function
                if (!local->attrs.is_assigned) {
                    local->attrs.is_assigned = true;
                    jmethod->locals[inst.operand.variable].attrs.needs_init = true;
                }

                EVAL_STACK_PUSH(type, local->attrs);
            } break;

            case CEE_STLOC: {
                CHECK(inst.operand.variable < arrlen(locals));
                jit_value_t value = EVAL_STACK_POP();

                CHECK(verifier_assignable_to(value.type, locals[inst.operand.variable].type),
                    "%T verifier-assignable-to %T", value.type, locals[inst.operand.variable].type);

                // just remember the attributes
                locals[inst.operand.variable].attrs = value.attrs;
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Field access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDFLD: {
                jit_value_t value = EVAL_STACK_POP();
                CHECK(
                    tdn_type_is_referencetype(value.type) ||
                    tdn_type_is_valuetype(value.type) ||
                    value.type->IsByRef ||
                    (method->Module->Assembly->AllowUnsafe && value.type->IsPointer)
                );

                RuntimeFieldInfo field = inst.operand.field;

                // ensure the type has the field inside of it
                bool found = false;
                RuntimeTypeInfo owner = value.type;
                if (owner->IsByRef || owner->IsPointer) {
                    owner = owner->ElementType;
                }
                for (; owner != NULL; owner = owner->BaseType) {
                    if (owner == field->DeclaringType) {
                        found = true;
                        break;
                    }
                }
                CHECK(found);

                RuntimeTypeInfo stack_type = jit_get_intermediate_type(field->FieldType);
                jit_value_attrs_t attrs = {};

                // check if this is a non-local reference
                if ((value.type->IsByRef && value.type->ElementType->IsByRefStruct) || value.type->IsByRefStruct) {
                    if (value.attrs.nonlocal_ref_struct) {
                        // the ref-struct is non-local, meaning that the refs inside of it are not local as well
                        attrs.nonlocal_ref = true;

                        // loading a by-ref struct from a by-ref struct, so it must be non-local as well
                        if (value.type->IsByRefStruct) {
                            attrs.nonlocal_ref_struct = true;
                        }
                    }
                }

                EVAL_STACK_PUSH(stack_type, attrs);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Calls
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_CALL:
            case CEE_CALLVIRT: {
                RuntimeMethodBase callee = inst.operand.method;
                RuntimeTypeInfo callee_this = NULL;

                // TODO: check accessibility

                if (inst.opcode == CEE_CALL) {
                    // ensure we don't attempt to call an abstract method
                    CHECK(!callee->Attributes.Abstract);
                }

                if (!callee->Attributes.Static) {
                    callee_this = method->DeclaringType;
                    if (tdn_type_is_valuetype(callee_this)) {
                        CHECK_AND_RETHROW(tdn_get_byref_type(callee_this, &callee_this));
                    }
                }

                // TODO: deal with ref-structs

                // pop the arguments and verify them
                bool might_return_local_ref = false;
                for (int i = callee->Parameters->Length - 1; i >= 0; i--) {
                    ParameterInfo parameter = callee->Parameters->Elements[i];
                    jit_value_t value = EVAL_STACK_POP();
                    RuntimeTypeInfo param_type = parameter->ParameterType;

                    // if this is a by-ref check for byref requirements
                    if (param_type->IsByRef) {
                        // does not want a readonly, ensure we don't give it a readonly
                        if (!param_type->IsReadOnly) {
                            CHECK(!value.attrs.readonly);
                        }

                        // if this is a local ref, the returned value might also be
                        // a local ref, remember that so we don't reattach it
                        // TODO: scoped references
                        if (!value.attrs.nonlocal_ref) {
                            might_return_local_ref = true;
                        }

                        // if this is a ref to a ref-struct, and the elements of the ref-struct
                        // might contain local references, then also mark that we might leak
                        // a reference
                        if (param_type->ElementType->IsByRefStruct) {
                            if (!value.attrs.nonlocal_ref_struct) {
                                might_return_local_ref = true;
                            }
                        }
                    } else if (param_type->ElementType->IsByRefStruct) {
                        // same as above, but for by-value nonlocal
                        if (!value.attrs.nonlocal_ref_struct) {
                            might_return_local_ref = true;
                        }
                    }

                    // verify we can perform the call
                    CHECK(verifier_assignable_to(value.type, param_type),
                        "%T verifier-assignable-to %T",value.type, param_type);
                }

                // check the this type
                if (callee_this != NULL) {
                    jit_value_t value = EVAL_STACK_POP();
                    CHECK(verifier_assignable_to(value.type, callee_this),
                        "%T verifier-assignable-to %T",value.type, callee_this);

                    // if this is a by-ref check for byref requirements
                    if (callee_this->IsByRef) {
                        // does not want a readonly, ensure we don't give it a readonly
                        if (!callee->IsReadOnly) {
                            CHECK(!value.attrs.readonly);
                        }
                    }

                    // TODO: unscoped references, until then a ref the the this can't be leaked
                    //       to the outside since its marked as a local on its own

                    // Ensure that the code doesn't bypass overrides via
                    // inheritance unless its calling on the current this type
                    if (inst.opcode == CEE_CALL) {
                        CHECK(value.attrs.this_ptr);
                    }
                }

                // push the return value
                RuntimeTypeInfo ret_type = callee->ReturnParameter->ParameterType;
                if (ret_type != tVoid) {
                    jit_value_attrs_t attrs = {};

                    // if the return is a readonly, mark it as such
                    if (callee->ReturnParameter->IsReadOnly) {
                        attrs.readonly = true;
                    }

                    // we will not leak a by-ref, check if we need to mark anything
                    if (!might_return_local_ref) {
                        // the return is a byref
                        if (ret_type->IsByRef) {
                            attrs.nonlocal_ref = true;

                            // its not possible to return a ref to a by-ref struct unless we passed on
                            // by reference as well, since it can't be alive anywhere else
                            CHECK(!ret_type->ElementType->IsByRefStruct);

                        } else if (ret_type->IsByRefStruct) {
                            // the ref-struct contains non-nonlocal fields if it was
                            // returned and we could not have passed anything
                            attrs.nonlocal_ref_struct = true;
                        }
                    }

                    EVAL_STACK_PUSH(ret_type, attrs);
                }
            } break;

            case CEE_RET: {
                RuntimeTypeInfo type = method->ReturnParameter->ParameterType;

                if (type != tVoid) {
                    jit_value_t value = EVAL_STACK_POP();

                    // ensure we don't return a readonly from a non-readonly
                    if (!method->ReturnParameter->IsReadOnly) {
                        CHECK(!value.attrs.readonly);
                    }

                    // don't leak non-local references
                    if (type->IsByRef) {
                        CHECK(value.attrs.nonlocal_ref);

                        // ref to ref-struct, ensure we don't have anything weird about it
                        if (type->ElementType->IsByRefStruct) {
                            CHECK(value.attrs.nonlocal_ref_struct);
                        }
                    } else if (type->IsByRefStruct) {
                        CHECK(value.attrs.nonlocal_ref_struct);
                    }

                    // verify we can actually return this
                    CHECK(verifier_assignable_to(value.type, type),
                        "%T verifier-assignable-to %T", value.type, type);
                }

                CHECK(arrlen(stack) == 0);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Control flow
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_BR: {
                CHECK_AND_RETHROW(jit_verify_merge_basic_block(
                    jmethod,
                    inst.operand.branch_target,
                    stack, locals, args,
                    copy_leave_targets(block->leave_target_stack)));
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Exceptions
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_THROW: {
                jit_value_t value = EVAL_STACK_POP();
                // TODO: must be an exception class, not just a reference
                CHECK(tdn_type_is_referencetype(value.type));
                arrsetlen(stack, 0);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Misc
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_NOP: {
            } break;

            default: CHECK_FAIL("Unknown opcode `%s`", tdn_get_opcode_name(inst.opcode));
        }

        // if this was not a meta instruction (prefix) then make sure we have
        // no pending prefixes
        if (inst.control_flow != TDN_IL_CF_META) {
            if (pending_prefix & IL_PREFIX_CONSTRAINED) ERROR("- Unhandled `.constrained`");
            if (pending_prefix & IL_PREFIX_VOLATILE) ERROR("- Unhandled `.volatile`");
            if (pending_prefix & IL_PREFIX_UNALIGNED) ERROR("- Unhandled `.unaglined`");
            CHECK(pending_prefix == 0, "Some prefixes not handled");
        }

#ifdef JIT_VERBOSE_VERIFY
        indent = tdn_disasm_print_end(body, pc, indent);
#endif
    }

    // we have a fallthrough
    if (inst.control_flow == TDN_IL_CF_NEXT || inst.control_flow == TDN_IL_CF_CALL) {
        CHECK_AND_RETHROW(jit_verify_merge_basic_block(
            jmethod,
            pc,
            stack, locals, args,
            copy_leave_targets(block->leave_target_stack)));
    }

    // last must be a valid instruction
    CHECK(
        inst.control_flow != TDN_IL_CF_FIRST &&
        inst.control_flow != TDN_IL_CF_META
    );

cleanup:
    arrfree(stack);
    arrfree(locals);
    arrfree(args);

    return err;
}

static tdn_err_t jit_verify_prepare_method(jit_method_t* jmethod) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = jmethod->method;
    RuntimeMethodBody body = method->MethodBody;

    // if we have a this add it to the local
    if (!method->Attributes.Static) {
        // mark it as a this_ptr
        jit_value_attrs_t attrs = {
            .this_ptr = true
        };

        RuntimeTypeInfo this_type = method->DeclaringType;
        if (tdn_type_is_valuetype(this_type)) {
            // we know the exact type either way
            attrs.known_type = this_type;

            // if this is a readonly method then we are going
            // to have a readonly this as well
            if (method->IsReadOnly) {
                attrs.readonly = true;
            }

            // if the this is a byref struct mark it a non-local
            // meaning that we can return the refs from inside of it
            if (this_type->IsByRefStruct) {
                attrs.nonlocal_ref_struct = true;
            }

            // TODO: support for unscoped attribute

            CHECK_AND_RETHROW(tdn_get_byref_type(this_type, &this_type));
        }

        arrpush(jmethod->args, ((jit_value_t){ .type = this_type, attrs = attrs }));
    }

    // now prepare the rest of the arguments
    for (int i = 0; i < method->Parameters->Length; i++) {
        ParameterInfo info = method->Parameters->Elements[i];

        // TODO: support for scoped attribute

        // if its a sealed type, push it correctly
        jit_value_attrs_t attrs = {
            .spilled = jit_is_struct_like(info->ParameterType)
        };

        // mark as readonly if need be
        if (info->IsReadOnly) {
            attrs.readonly = true;
        }

        // mark as a non-local ref
        if (info->ParameterType->IsByRef) {
            attrs.nonlocal_ref = true;

            // ref to a by-ref struct, the fields are nonlocal as well
            if (info->ParameterType->ElementType->IsByRefStruct) {
                attrs.nonlocal_ref_struct = true;
            }
        }

        // mark the struct as a non-local
        if (info->ParameterType->IsByRefStruct) {
            attrs.nonlocal_ref_struct = true;
        }

        arrpush(jmethod->args, ((jit_value_t){ .type = info->ParameterType, .attrs = attrs }));
    }

    // and now prepare the locals
    if (body->LocalVariables != NULL) {
        for (int i = 0; i < body->LocalVariables->Length; i++) {
            RuntimeLocalVariableInfo info = body->LocalVariables->Elements[i];
            jit_value_attrs_t attrs = {
                .spilled = jit_is_struct_like(info->LocalType)
            };
            arrpush(jmethod->locals, ((jit_value_t){ .type = info->LocalType, .attrs = attrs }));
        }
    }

    // and now find all basic blocks
    CHECK_AND_RETHROW(jit_find_basic_blocks(jmethod));

cleanup:
    return err;
}

tdn_err_t jit_verify_method(jit_method_t* method) {
    tdn_err_t err = TDN_NO_ERROR;

#ifdef JIT_DEBUG_VERIFY
    TRACE("VERIFY: %T::%U", method->method->DeclaringType, method->method->Name);
#endif

    // prepare the method, setting up the initial locals, arguments and so on
    CHECK_AND_RETHROW(jit_verify_prepare_method(method));

    // "merge" with the first block, giving it the initial pc for everything
    CHECK_AND_RETHROW(jit_verify_merge_basic_block(method, 0, NULL, method->locals, method->args, NULL));

    while (arrlen(method->block_queue) > 0) {
        jit_basic_block_t* block = arrpop(method->block_queue);
        block->in_queue = false;

#ifdef JIT_VERBOSE_VERIFY
        TRACE("\tBlock (IL_%04x)", block->start);
#endif
        CHECK_AND_RETHROW(jit_verify_basic_block(method, block));
    }

cleanup:
    arrfree(method->block_queue);

    return err;
}
