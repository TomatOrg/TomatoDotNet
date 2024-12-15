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

static tdn_err_t jit_queue_cctor(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeMethodBase cctor = (RuntimeMethodBase)type->TypeInitializer;
    if (cctor != NULL) {
        jit_method_t* jit_method;
        CHECK_AND_RETHROW(jit_get_or_create_method(cctor, &jit_method));
        jit_queue_verify(jit_method);
    }

cleanup:
    return err;
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

    // and same for ref-structs
    if (wanted->nonlocal_ref_struct && !actual->nonlocal_ref_struct) {
        wanted->nonlocal_ref_struct = false;
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
        CHECK(arrlen(stack) == arrlen(target->stack),
            "incoming %d, wanted %d", arrlen(stack), arrlen(target->stack));

        // already initialized, make sure the state is consistent, if not
        // mark the target for another pass
        for (int i = 0; i < arrlen(stack); i++) {
            jit_stack_value_t* wanted = &target->stack[i];
            jit_stack_value_t* actual = &stack[i];

            // make sure both have the same type on the stack
            // TODO: support finding the common of the two
            CHECK(wanted->type == actual->type);

            // make sure we don't have weird cases with a dangling method
            CHECK(!wanted->attrs.is_method);
            CHECK(!actual->attrs.is_method);

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
    // 9. T is null type, and U is reference type, we perform
    //    this first to make sure that no null deref happens from
    //    the null type
    if (Q == NULL) {
        if (tdn_type_is_referencetype(R)) {
            return true;
        }

        return false;
    }

    RuntimeTypeInfo T = tdn_get_verification_type(Q);
    RuntimeTypeInfo U = tdn_get_verification_type(R);

    return tdn_type_assignable_to(T, U);
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

    if (block->initialized) {
        // copy the initial stack
        arrsetlen(stack, arrlen(block->stack));
        memcpy(stack, block->stack, arrlen(block->stack) * sizeof(*block->stack));

    } else {
        //
        // This is the first block, should only really happen
        // on the entry block
        //

        // set initial local state
        if (body->LocalVariables != NULL) {
            arrsetlen(block->locals, body->LocalVariables->Length);
            for (int i = 0; i < arrlen(block->locals); i++) {
                RuntimeTypeInfo type = jmethod->locals[i].type;

                // we need to initialize locals to be non-local since they start with
                // all zeroes default state, and a NULL ref is considered a non-local ref
                block->locals[i] = (jit_item_attrs_t){
                    .nonlocal_ref_struct = type->IsByRefStruct,
                    .nonlocal_ref = type->IsByRef,
                };
            }
        }
    }
    block->initialized = true;

    // copy the locals state
    arrsetlen(locals, arrlen(block->locals));
    memcpy(locals, block->locals, sizeof(*locals) * arrlen(locals));

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
        pc += inst.length;

        // we got a delegate creation sequence, so this
        // opcode must be newobj
        if (must_be_newobj) {
            CHECK(inst.opcode == CEE_NEWOBJ);
            must_be_newobj = false;
        }

        switch (inst.opcode) {

            case CEE_VOLATILE: {
                pending_prefix |= IL_PREFIX_VOLATILE;
            } break;

            case CEE_CONSTRAINED: {
                pending_prefix |= IL_PREFIX_CONSTRAINED;
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arguments
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STARG: {
                int index = inst.operand.variable;
                RuntimeTypeInfo arg_type = GET_ARG_TYPE(index);

                // verify the type
                jit_stack_value_t value = EVAL_STACK_POP();
                CHECK(verifier_assignable_to(value.type, arg_type));

                // need to be spilled like a local variable
                if (!jit_is_struct_like(arg_type)) {
                    jmethod->args[index].spill_required = true;
                }

                // don't allow to modify this
                if (this_type != NULL) {
                    CHECK(index != 0);
                }

                // for simplicity, don't allow to
                // store to a byref parameter
                if (arg_type->IsByRef) CHECK(value.attrs.nonlocal_ref);
                if (arg_type->IsByRefStruct) CHECK(value.attrs.nonlocal_ref_struct);
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
                    // check if the reference is readonly
                    if (arg_param != NULL) {
                        attrs.readonly = arg_param->IsReadOnly;

                        // a ref that comes from the outside, so its a non-local ref
                        attrs.nonlocal_ref = true;
                    } else {
                        // if this is a readonly method and we are loading
                        // the `this` pointer then its a non-local reference
                        attrs.readonly = method->IsReadOnly;

                        if (arg_type->ElementType->IsByRefStruct) {
                            // when passing a ref struct, the ref struct itself is considered
                            // non-local, even tho its reference is considered local (because
                            // of the default scoping rules), this doesn't allow ldflda to work
                            // but allows ldfld of references to work
                            attrs.nonlocal_ref_struct = true;
                        }

                        // TODO: UnscopedRefAttribute
                    }

                } else if (arg_type->IsByRefStruct) {
                    // a ref-struct that comes from the outside, quick check to make sure we are
                    // not returning a `this` type with this
                    CHECK(arg_param != NULL);
                    attrs.nonlocal_ref_struct = true;
                }

                EVAL_STACK_PUSH(arg_type, attrs);
            }  break;

            case CEE_LDARGA: {
                int index = inst.operand.variable;
                RuntimeTypeInfo arg_type = GET_ARG_TYPE(index);

                // don't allow byref of the `this`, so
                // it can't be overriden
                if (this_type != NULL) {
                    CHECK(index != 0);
                }

                // mark that we need a spill
                if (!jit_is_struct_like(arg_type)) {
                    jmethod->args[index].spill_required = true;
                }

                jit_item_attrs_t attrs = {};

                // taking a reference to a ref-struct parameter
                // keeps the nonlocality of the struct itself even
                // tho the ref itself is local
                if (arg_type->IsByRefStruct) {
                    attrs.nonlocal_ref_struct = true;
                }

                arg_type = tdn_get_verification_type(arg_type);
                CHECK_AND_RETHROW(tdn_get_byref_type(arg_type, &arg_type));
                EVAL_STACK_PUSH(arg_type, attrs);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Locals
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STLOC: {
                int index = inst.operand.variable;
                CHECK(index < arrlen(locals));
                RuntimeLocalVariableInfo local = body->LocalVariables->Elements[index];

                // verify the type
                jit_stack_value_t value = EVAL_STACK_POP();
                CHECK(verifier_assignable_to(value.type, local->LocalType));

                // set the new local attributes
                locals[index] = value.attrs;
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

                // need to keep the locality of the ref-struct
                jit_item_attrs_t attrs = {
                    .nonlocal_ref_struct = locals[index].nonlocal_ref_struct
                };

                EVAL_STACK_PUSH(type, attrs);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Fields
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STFLD: {
                RuntimeFieldInfo field = inst.operand.field;
                jit_stack_value_t value = EVAL_STACK_POP();
                jit_stack_value_t obj = EVAL_STACK_POP();

                // TODO: check accessibility

                // check that the object has the field
                RuntimeTypeInfo owner = obj.type;
                if (tdn_type_is_valuetype(field->DeclaringType)) {
                    CHECK(owner->IsByRef);
                    owner = owner->ElementType;

                    CHECK(field->DeclaringType == owner);
                } else {
                    while (owner != field->DeclaringType) {
                        owner = owner->BaseType;
                        CHECK(owner != tObject);
                    }
                }

                // check this is a valid assignment
                CHECK(verifier_assignable_to(value.type, field->FieldType));

                // if we assign a ref into a ref-struct field then
                // we need to make sure we won't be able to leak a local
                if (owner->IsByRefStruct && value.type->IsByRef) {
                    if (obj.attrs.nonlocal_ref_struct) {
                        CHECK(value.attrs.nonlocal_ref);
                    }
                }

                // make sure we have the cctor
                if (field->Attributes.Static) {
                    CHECK_AND_RETHROW(jit_queue_cctor(field->DeclaringType));
                }

                // clear the possible prefixes
                // for the instruction
                pending_prefix &= ~IL_PREFIX_VOLATILE;
            } break;

            case CEE_LDFLD: {
                RuntimeFieldInfo field = inst.operand.field;
                jit_stack_value_t obj = EVAL_STACK_POP();

                // TODO: check accessibility

                // get the owner type
                RuntimeTypeInfo owner = obj.type;
                if (tdn_type_is_valuetype(field->DeclaringType) && owner->IsByRef) {
                    owner = owner->ElementType;
                }

                // check object has field
                while (owner != field->DeclaringType) {
                    owner = owner->BaseType;
                    CHECK(owner != NULL);
                }

                // make sure we have the cctor
                if (field->Attributes.Static) {
                    CHECK_AND_RETHROW(jit_queue_cctor(field->DeclaringType));
                }

                // clear the possible prefixes
                // for the instruction
                pending_prefix &= ~IL_PREFIX_VOLATILE;


                // if we are loading a by-ref field it gets the
                // same scope as the struct it was loaded from
                jit_item_attrs_t attrs = {};
                if (field->FieldType->IsByRef) {
                    attrs.readonly = field->ReferenceIsReadOnly;
                    attrs.nonlocal_ref = obj.attrs.nonlocal_ref_struct;
                }

                RuntimeTypeInfo type = tdn_get_intermediate_type(field->FieldType);
                EVAL_STACK_PUSH(type, attrs);
            } break;

            case CEE_LDFLDA: {
                RuntimeFieldInfo field = inst.operand.field;
                jit_stack_value_t obj = EVAL_STACK_POP();

                // TODO: check accessibility

                // get the owner type
                RuntimeTypeInfo owner = obj.type;
                if (tdn_type_is_valuetype(field->DeclaringType) && owner->IsByRef) {
                    owner = owner->ElementType;
                }

                // check object has field
                while (owner != field->DeclaringType) {
                    owner = owner->BaseType;
                    CHECK(owner != NULL);
                }

                // make sure we have the cctor
                if (field->Attributes.Static) {
                    CHECK_AND_RETHROW(jit_queue_cctor(field->DeclaringType));
                }

                RuntimeTypeInfo type = tdn_get_verification_type(field->FieldType);
                CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));

                // we have a readonly field if the
                jit_item_attrs_t attrs = {
                    .readonly = field->Attributes.InitOnly
                };

                // check if the reference will be non-local
                if (tdn_type_is_referencetype(owner)) {
                    // on the heap, will always be non-local
                    attrs.nonlocal_ref = true;
                }

                EVAL_STACK_PUSH(type, attrs);
            } break;

            case CEE_STSFLD: {
                jit_stack_value_t value = EVAL_STACK_POP();

                // TODO: check accessibility

                // make sure is static
                CHECK(inst.operand.field->Attributes.Static);

                // make sure we have the cctor
                CHECK_AND_RETHROW(jit_queue_cctor(inst.operand.field->DeclaringType));

                // clear the possible prefixes
                // for the instruction
                pending_prefix &= ~IL_PREFIX_VOLATILE;

                // check assignable
                CHECK(verifier_assignable_to(value.type, inst.operand.field->FieldType));
            } break;

            case CEE_LDSFLD: {
                // TODO: check accessibility

                // make sure is static
                CHECK(inst.operand.field->Attributes.Static);

                // make sure we have the cctor
                CHECK_AND_RETHROW(jit_queue_cctor(inst.operand.field->DeclaringType));

                // clear the possible prefixes
                // for the instruction
                pending_prefix &= ~IL_PREFIX_VOLATILE;

                RuntimeTypeInfo type = tdn_get_intermediate_type(inst.operand.field->FieldType);
                EVAL_STACK_PUSH(type);
            } break;

            case CEE_LDSFLDA: {
                // TODO: check accessibility

                // make sure is static
                CHECK(inst.operand.field->Attributes.Static);

                // make sure we have the cctor
                CHECK_AND_RETHROW(jit_queue_cctor(inst.operand.field->DeclaringType));

                RuntimeTypeInfo type = tdn_get_verification_type(inst.operand.field->FieldType);
                CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));
                jit_item_attrs_t attrs = {
                    .readonly = inst.operand.field->Attributes.InitOnly,
                    .nonlocal_ref = true
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

            case CEE_DUP: {
                jit_stack_value_t value = EVAL_STACK_POP();
                EVAL_STACK_PUSH(value.type, value.attrs);
                EVAL_STACK_PUSH(value.type, value.attrs);
            } break;

            case CEE_LDVIRTFTN: {
                CHECK(last_opcode == CEE_DUP, "must have dup before CEE_LDVIRTFTN");

                // push the method
                EVAL_STACK_PUSH(.attrs = { .method = inst.operand.method, .is_method = true });

                // next must come newobj
                must_be_newobj = true;
            } break;

            case CEE_LDFTN: {
                // push the method
                EVAL_STACK_PUSH(.attrs = { .method = inst.operand.method, .is_method = true });

                // queue for verify
                jit_method_t* target_method = NULL;
                CHECK_AND_RETHROW(jit_get_or_create_method(inst.operand.method, &target_method));
                jit_queue_verify(target_method);

                // next must come newobj
                must_be_newobj = true;
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Method calling
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_NEWOBJ:
            case CEE_CALL:
            case CEE_CALLVIRT: {
                RuntimeMethodBase target = inst.operand.method;

                // queue for verify
                jit_method_t* target_method = NULL;
                CHECK_AND_RETHROW(jit_get_or_create_method(target, &target_method));
                jit_queue_verify(target_method);

                // TODO: verify the caller is visible

                // get the target this type
                RuntimeTypeInfo target_this_type = NULL;
                if (!target->Attributes.Static) {
                    target_this_type = target->DeclaringType;
                    if (tdn_type_is_valuetype(target_this_type)) {
                        CHECK_AND_RETHROW(tdn_get_byref_type(target_this_type, &target_this_type));
                    }
                } else {
                    // make sure we have the cctor
                    CHECK_AND_RETHROW(jit_queue_cctor(target->DeclaringType));
                }

                // constrained is allowed on both call and callvirt
                if (inst.opcode == CEE_CALL || inst.opcode == CEE_CALLVIRT) {
                    pending_prefix &= ~IL_PREFIX_CONSTRAINED;
                }

                // some extra verifications
                if (inst.opcode == CEE_NEWOBJ) {
                    // newobj must be done on a non-static ctor
                    CHECK(!target->Attributes.Static);
                    CHECK(target->Attributes.RTSpecialName);

                    // queue the type itself
                    CHECK_AND_RETHROW(jit_queue_type(target->DeclaringType));

                    // make sure we have the cctor
                    CHECK_AND_RETHROW(jit_queue_cctor(target->DeclaringType));

                } else if (inst.opcode == CEE_CALLVIRT) {
                    // callvirt must be done on a non-static method
                    // it can be done on a non-virtual one, then it
                    // just requires a nullcheck
                    CHECK(!target->Attributes.Static);

                    // must be a reference type, otherwise there is
                    // no vtable to check
                    CHECK(tdn_type_is_referencetype(target->DeclaringType));
                }

                // verify all of the arguments
                bool might_return_local_ref = false;
                RuntimeMethodBase delegate_method = NULL;
                for (int i = target->Parameters->Length - 1; i >= 0; i--) {
                    ParameterInfo info = target->Parameters->Elements[i];
                    jit_stack_value_t arg = EVAL_STACK_POP();

                    // when creating a delegate we have a bit of specific rules
                    if (inst.opcode == CEE_NEWOBJ && jit_is_delegate(target_this_type)) {
                        if (i == 0) {
                            if (delegate_method->Attributes.Static) {
                                CHECK(arg.type == NULL);
                            } else {
                                CHECK(delegate_method->DeclaringType == arg.type);
                            }
                        } else if (i == 1) {
                            CHECK(arg.attrs.is_method);
                            delegate_method = arg.attrs.method;

                            // TODO: check method signature against the delegate
                        }
                    } else {
                        CHECK(verifier_assignable_to(arg.type, info->ParameterType),
                                "%T verifier-assignable-to %T", arg.type, info->ParameterType);

                        // if the argument is not readonly (byref) then ensure
                        // that the variable we pass to it is also not a readonly
                        // one
                        if (!info->IsReadOnly) {
                            CHECK(!arg.attrs.readonly);
                        }

                        // check if we might pass a local-reference into this function, either by passing
                        // a reference directly or passing a reference via a ref-struct indirectly
                        // TODO: support for scoped and unscoped references
                        if (
                            (arg.type->IsByRef && !arg.attrs.nonlocal_ref) ||
                            (arg.type->IsByRefStruct && !arg.attrs.nonlocal_ref_struct)
                        ) {
                            might_return_local_ref = true;
                        }
                    }
                }

                if (inst.opcode == CEE_NEWOBJ) {
                    // push the target this type
                    // NOTE: for value types we don't actually push a byref so
                    //       take the declaring type directly
                    jit_item_attrs_t attrs = {
                        .known_type = target->DeclaringType,
                    };

                    // the rules for newobj is very similar, if we are creating
                    // a by-ref struct with a local reference we won't mark
                    // the ref-struct as non-local
                    if (target_this_type->IsByRef && target_this_type->ElementType->IsByRefStruct) {
                        attrs.nonlocal_ref_struct = !might_return_local_ref;
                    }

                    EVAL_STACK_PUSH(target->DeclaringType, attrs);

                } else {
                    // verify the this parameter
                    if (target_this_type != NULL) {
                        jit_stack_value_t obj = EVAL_STACK_POP();
                        CHECK(verifier_assignable_to(obj.type, target_this_type),
                            "%T verifier-assignable-to %T", obj.type, target_this_type);

                        // by default `this` is considered scoped in the callee
                        // meaning that we know the returned ref won't point to
                        // the this pointer
                        // TODO: unscoped reference support
                    }

                    // if we return a ref initialize if its nonlocal and if
                    // its readonly based on the return parameter
                    ParameterInfo ret_info = target->ReturnParameter;
                    if (ret_info->ParameterType != tVoid) {
                        jit_item_attrs_t attrs = {};

                        // if we return either a by-ref struct or a ref value
                        // then we need to mark it as non-local only if we don't
                        // pass any locals into it
                        if (ret_info->ParameterType->IsByRefStruct) {
                            attrs.nonlocal_ref_struct = !might_return_local_ref;

                        } else if (ret_info->ParameterType->IsByRef) {
                            // in the case of ref we also need to check if
                            // sthe returned reference is readonly
                            attrs.readonly = ret_info->IsReadOnly;
                            attrs.nonlocal_ref = !might_return_local_ref;
                        }

                        RuntimeTypeInfo ret_type = tdn_get_intermediate_type(ret_info->ParameterType);
                        EVAL_STACK_PUSH(ret_type, attrs);
                    }
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Array handlig
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_NEWARR: {
                jit_stack_value_t num_elems = EVAL_STACK_POP();

                CHECK(
                    num_elems.type == tInt32 ||
                    num_elems.type == tIntPtr
                );

                RuntimeTypeInfo array_type;
                CHECK_AND_RETHROW(tdn_get_array_type(inst.operand.type, &array_type));

                EVAL_STACK_PUSH(array_type);
            } break;

            case CEE_LDLEN: {
                jit_stack_value_t array = EVAL_STACK_POP();
                CHECK(array.type->IsArray);
                EVAL_STACK_PUSH(tIntPtr);
            } break;

            case CEE_STELEM:
            case CEE_STELEM_REF: {
                jit_stack_value_t value = EVAL_STACK_POP();
                jit_stack_value_t index = EVAL_STACK_POP();
                jit_stack_value_t array = EVAL_STACK_POP();

                CHECK(
                    index.type == tInt32 ||
                    index.type == tIntPtr
                );

                CHECK(array.type->IsArray);

                RuntimeTypeInfo T = array.type->ElementType;
                if (inst.operand.type == NULL) {
                    // stelem.ref
                    CHECK(tdn_type_is_referencetype(value.type));
                    CHECK(tdn_type_array_element_compatible_with(value.type, T));

                } else {
                    // normal stelem
                    // TODO: the spec is broken or something because I don't get it
                    //       I added the get verification type and get intermediate type manually to make it work
                    CHECK(tdn_type_array_element_compatible_with(value.type, tdn_get_intermediate_type(inst.operand.type)));
                    CHECK(tdn_type_array_element_compatible_with(inst.operand.type, tdn_get_verification_type(T)));
                }
            } break;

            case CEE_LDELEM:
            case CEE_LDELEM_REF: {
                jit_stack_value_t index = EVAL_STACK_POP();
                jit_stack_value_t array = EVAL_STACK_POP();

                CHECK(
                    index.type == tInt32 ||
                    index.type == tIntPtr
                );

                CHECK(array.type->IsArray);

                RuntimeTypeInfo T = array.type->ElementType;
                if (inst.operand.type == NULL) {
                    // ldelem.ref
                    CHECK(tdn_type_is_referencetype(T));
                } else {
                    // ldelem
                    // TODO: same as the stelem, I need to add the verification type reduction to make it pass...
                    CHECK(tdn_type_array_element_compatible_with(tdn_get_verification_type(T), inst.operand.type));
                }

                // TODO: should this actually track the inst.operand.type and not the T itself?
                //       this could be very much important when dealing with interfaces...
                EVAL_STACK_PUSH(tdn_get_intermediate_type(T));
            } break;

            case CEE_LDELEMA: {
                jit_stack_value_t index = EVAL_STACK_POP();
                jit_stack_value_t array = EVAL_STACK_POP();

                CHECK(
                    index.type == tInt32 ||
                    index.type == tIntPtr
                );

                CHECK(array.type->IsArray);

                RuntimeTypeInfo T = array.type->ElementType;

                // verify the types properly
                RuntimeTypeInfo type_tok, T_ref;
                CHECK_AND_RETHROW(tdn_get_byref_type(T, &T_ref));
                CHECK_AND_RETHROW(tdn_get_byref_type(inst.operand.type, &type_tok));
                CHECK_AND_RETHROW(tdn_type_pointer_element_compatible_with(T_ref, type_tok));

                // and push the tracked address
                RuntimeTypeInfo ref_type = tdn_get_verification_type(T);
                CHECK_AND_RETHROW(tdn_get_byref_type(ref_type, &ref_type));
                EVAL_STACK_PUSH(ref_type);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Indirect access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDIND_I1:
            case CEE_LDIND_I2:
            case CEE_LDIND_I4:
            case CEE_LDIND_I8:
            case CEE_LDIND_U1:
            case CEE_LDIND_U2:
            case CEE_LDIND_U4:
            case CEE_LDIND_I:
            case CEE_LDIND_REF:
            case CEE_LDOBJ: {
                jit_stack_value_t addr = EVAL_STACK_POP();

                CHECK(addr.type->IsByRef);

                pending_prefix &= ~IL_PREFIX_VOLATILE;

                RuntimeTypeInfo type = inst.operand.type;
                if (type == NULL) {
                    type = addr.type->ElementType;
                    CHECK(tdn_type_is_referencetype(type));
                    type = tdn_get_verification_type(type);
                } else {
                    CHECK(verifier_assignable_to(addr.type->ElementType, type));
                    type = tdn_get_intermediate_type(type);
                }
                EVAL_STACK_PUSH(type);
            } break;

            case CEE_STIND_I1:
            case CEE_STIND_I2:
            case CEE_STIND_I4:
            case CEE_STIND_I8:
            case CEE_STIND_I:
            case CEE_STIND_REF:
            case CEE_STOBJ: {
                jit_stack_value_t val = EVAL_STACK_POP();
                jit_stack_value_t addr = EVAL_STACK_POP();

                CHECK(addr.type->IsByRef);

                if (inst.operand.type != NULL) {
                    CHECK(verifier_assignable_to(val.type, inst.operand.type));
                } else {
                    CHECK(tdn_type_is_referencetype(val.type));
                }

                // check consistent
                CHECK(verifier_assignable_to(val.type, addr.type->ElementType));
            } break;

            case CEE_INITOBJ: {
                jit_stack_value_t dest = EVAL_STACK_POP();

                CHECK(dest.type->IsByRef);

                if (tdn_type_is_referencetype(dest.type->ElementType)) {
                    CHECK(tdn_type_assignable_to(inst.operand.type, dest.type->ElementType));
                } else {
                    CHECK(inst.operand.type == dest.type->ElementType);
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Math
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_ADD:
            case CEE_SUB:
            case CEE_MUL:
            case CEE_DIV:
            case CEE_REM:
            case CEE_DIV_UN:
            case CEE_REM_UN:
            case CEE_AND:
            case CEE_XOR:
            case CEE_OR:
            case CEE_ADD_OVF:
            case CEE_ADD_OVF_UN:
            case CEE_SUB_OVF:
            case CEE_SUB_OVF_UN:
            case CEE_MUL_OVF:
            case CEE_MUL_OVF_UN: {
                jit_stack_value_t value2 = EVAL_STACK_POP();
                jit_stack_value_t value1 = EVAL_STACK_POP();

                RuntimeTypeInfo result;
                if (value1.type == tInt32) {
                    if (value2.type == tInt32) {
                        result = tInt32;
                    } else {
                        CHECK(value2.type == tIntPtr);
                        result = tIntPtr;
                    }

                } else if (value1.type == tIntPtr) {
                    CHECK(value2.type == tInt32 || value2.type == tIntPtr);
                    result = tIntPtr;

                } else if (value1.type == tInt64) {
                    CHECK(value2.type == tInt64);
                    result = tInt64;

                } else {
                    CHECK_FAIL();
                }

                EVAL_STACK_PUSH(result);
            } break;

            case CEE_SHL:
            case CEE_SHR:
            case CEE_SHR_UN: {
                jit_stack_value_t shift_amount = EVAL_STACK_POP();
                jit_stack_value_t value = EVAL_STACK_POP();

                CHECK(
                    shift_amount.type == tInt32 ||
                    shift_amount.type == tIntPtr
                );

                CHECK(
                    value.type == tInt32 ||
                    value.type == tInt64 ||
                    value.type == tIntPtr
                );

                EVAL_STACK_PUSH(value.type);
            } break;

            case CEE_NOT:
            case CEE_NEG: {
                jit_stack_value_t value = EVAL_STACK_POP();
                CHECK(
                    value.type == tInt32 ||
                    value.type == tInt64 ||
                    value.type == tIntPtr
                );
                EVAL_STACK_PUSH(value.type);
            } break;

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

            case CEE_CONV_U:
            case CEE_CONV_I: {
                jit_stack_value_t value = EVAL_STACK_POP();
                CHECK(
                    value.type == tInt32 ||
                    value.type == tInt64 ||
                    value.type == tIntPtr
                );
                EVAL_STACK_PUSH(tIntPtr);
            } break;

            case CEE_CONV_U8:
            case CEE_CONV_I8: {
                jit_stack_value_t value = EVAL_STACK_POP();
                CHECK(
                    value.type == tInt32 ||
                    value.type == tInt64 ||
                    value.type == tIntPtr
                );
                EVAL_STACK_PUSH(tInt64);
            } break;

            case CEE_CONV_U4:
            case CEE_CONV_I4:
            case CEE_CONV_U2:
            case CEE_CONV_I2:
            case CEE_CONV_I1:
            case CEE_CONV_U1: {
                jit_stack_value_t value = EVAL_STACK_POP();
                CHECK(
                    value.type == tInt32 ||
                    value.type == tInt64 ||
                    value.type == tIntPtr
                );
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
                    if (!method->ReturnParameter->IsReadOnly) {
                        CHECK(!ret_val.attrs.readonly);
                    }

                    // check reference scoping
                    if (ret_val.type->IsByRef) {
                        // don't leak non-local reference
                        CHECK(ret_val.attrs.nonlocal_ref);

                        // I don't think its possible to get a nonlocal-ref
                        // to a local ref-struct, but just in case I will
                        // check it
                        if (ret_val.type->ElementType->IsByRefStruct) {
                            CHECK(ret_val.attrs.nonlocal_ref_struct);
                        }

                    } else if (ret_val.type->IsByRefStruct) {
                        // don't leak a ref-struct with non-local members
                        CHECK(ret_val.attrs.nonlocal_ref_struct);
                    }

                    // check the type is the same
                    CHECK(verifier_assignable_to(ret_val.type, type));
                }

                CHECK(arrlen(stack) == 0);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Class casting
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_BOX: {
                jit_stack_value_t value = EVAL_STACK_POP();

                // must not be something that has a ref
                CHECK(!value.type->IsByRef && !value.type->IsByRefStruct);

                // validate it can be boxed as we want
                CHECK(verifier_assignable_to(value.type, inst.operand.type));

                // TODO: how does box work with nullable

                CHECK_AND_RETHROW(jit_queue_type(inst.operand.type));

                // track it as an object
                EVAL_STACK_PUSH(tObject, { .known_type = inst.operand.type });
            } break;

            case CEE_UNBOX_ANY: {
                jit_stack_value_t obj = EVAL_STACK_POP();

                // can't be something that is a by-ref or a by-ref-struct
                CHECK(!inst.operand.type->IsByRef);
                CHECK(!inst.operand.type->IsByRefStruct);

                // must be another ref on the stack
                CHECK(tdn_type_is_referencetype(obj.type));

                EVAL_STACK_PUSH(tdn_get_intermediate_type(inst.operand.type));
            } break;

            // verification wise both do the same
            case CEE_CASTCLASS:
            case CEE_ISINST: {
                EVAL_STACK_POP();
                if (tdn_type_is_valuetype(inst.operand.type)) {
                    EVAL_STACK_PUSH(tObject, { .known_type = inst.operand.type });
                } else {
                    EVAL_STACK_PUSH(inst.operand.type);
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Exception handling
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_THROW: {
                jit_stack_value_t obj = EVAL_STACK_POP();

                // TODO: check instanceof System.Exception
                CHECK(tdn_type_is_referencetype(obj.type));

                // and clear the stack
                arrsetlen(stack, 0);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Misc
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_SIZEOF: {
                EVAL_STACK_PUSH(tInt32);
            } break;

            // nothing to do
            case CEE_NOP: break;

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
    CHECK_AND_RETHROW(jit_queue_type(type));

    // verify it and all the called methods
    CHECK_AND_RETHROW(verify_all_methods());

cleanup:
    return err;
}
