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
    if (wanted->readable && !actual->readable) {
        wanted->readable = false;
        modified = true;
    }

    // same for writable
    if (wanted->writable && !actual->writable) {
        wanted->writable = false;
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

typedef enum il_prefix {
    IL_PREFIX_CONSTRAINED = BIT0,
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
            // Argument access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDARG:
            case CEE_LDARGA: {
                int index = inst.operand.variable;
                bool is_ldarga = inst.opcode == CEE_LDARGA;

                jit_item_attrs_t attrs = {};
                RuntimeTypeInfo arg_type = NULL;

                if (this_type != NULL && index == 0) {
                    // we don't allow to load this as a refernece
                    CHECK(!is_ldarga);

                    // this is the this type
                    attrs.this_ptr = true;

                    if (this_type->IsByRef) {
                        attrs.readable = true;

                        // if this is a readonly method then we have
                        // a readonly this
                        if (!method->IsReadOnly) {
                            attrs.writable = true;
                        }
                    }

                    arg_type = this_type;
                } else {
                    if (this_type != NULL) {
                        index--;
                    }
                    CHECK(index < method->Parameters->Length);
                    ParameterInfo arg_info = method->Parameters->Elements[index];
                    if (is_ldarga) {
                        // get the by-ref of the field
                        CHECK_AND_RETHROW(tdn_get_byref_type(arg_info->ParameterType, &arg_type));
                        arg_type = tdn_get_intermediate_type(arg_type);

                        // always readable and writable since its a reference
                        // to a local variable
                        attrs.readable = true;
                        attrs.writable = true;

                        // if this is a primitive value then we need
                        // to spill it for ldarga to work
                        if (!jit_is_struct_like(arg_type)) {
                            jmethod->args[inst.operand.variable].spill_required = true;
                        }
                    } else {
                        arg_type = tdn_get_intermediate_type(arg_info->ParameterType);

                        if (arg_info->Attributes.In) {
                            // if this is a in make sure we have a byref
                            // and set it as readable only
                            CHECK(arg_type->IsByRef);
                            attrs.readable = true;

                        } else if (arg_info->Attributes.Out) {
                            // if this is a out make sure we have a byref
                            // and set it writable
                            CHECK(arg_type->IsByRef);
                            attrs.writable = true;

                        } else if (arg_type->IsByRef) {
                            // otherwise a normal ref
                            attrs.readable = true;
                            attrs.writable = true;
                        }
                    }
                }

                // this is always a non-local since it comes from the outside
                if (arg_type->IsByRef || arg_type->IsByRefStruct) {
                    attrs.nonlocal_ref = true;
                }

                EVAL_STACK_PUSH(arg_type, attrs);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Local variable access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STLOC: {
                CHECK(inst.operand.variable < body->LocalVariables->Length);
                RuntimeLocalVariableInfo info = body->LocalVariables->Elements[inst.operand.variable];
                jit_stack_value_t value = EVAL_STACK_POP();

                // make sure we can assign them
                RuntimeTypeInfo local_type = tdn_get_intermediate_type(info->LocalType);
                CHECK(verifier_is_assignable(value.type, local_type),
                    "%T -> %T", value.type, local_type);

                // remember the known type
                locals[inst.operand.variable].known_type = value.attrs.known_type;

                // store the new attributes if this is a byref or byrefstruct
                if (local_type->IsByRef || local_type->IsByRefStruct) {
                    locals[inst.operand.variable].nonlocal_ref = value.attrs.nonlocal_ref;
                    locals[inst.operand.variable].readable = value.attrs.readable;
                    locals[inst.operand.variable].writable = value.attrs.writable;
                }
            } break;

            case CEE_LDLOC: {
                CHECK(inst.operand.variable < body->LocalVariables->Length);
                RuntimeLocalVariableInfo info = body->LocalVariables->Elements[inst.operand.variable];

                // load the local with its attributes to the stack
                RuntimeTypeInfo local_type = tdn_get_intermediate_type(info->LocalType);
                EVAL_STACK_PUSH(.type = local_type, .attrs = locals[inst.operand.variable]);
            } break;

            case CEE_LDLOCA: {
                CHECK(inst.operand.variable < body->LocalVariables->Length);
                RuntimeLocalVariableInfo info = body->LocalVariables->Elements[inst.operand.variable];

                // load the local with its attributes to the stack, this will fail
                // if this is already a byref
                RuntimeTypeInfo local_type = tdn_get_intermediate_type(info->LocalType);
                CHECK_AND_RETHROW(tdn_get_byref_type(local_type, &local_type));

                // if we know the exact type then also set it
                RuntimeTypeInfo known_type = NULL;
                if (locals[inst.operand.variable].known_type != NULL) {
                    CHECK_AND_RETHROW(tdn_get_byref_type(locals[inst.operand.variable].known_type, &known_type));
                }

                // the reference is readable and writable sinec it points to a local
                // we will load the
                EVAL_STACK_PUSH(.type = local_type, .attrs = {
                    .known_type = known_type,
                    .readable = true,
                    .writable = true,
                });
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Field access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STFLD:
            case CEE_STSFLD: {
                RuntimeFieldInfo field = inst.operand.field;
                bool is_static = inst.opcode == CEE_STSFLD;

                jit_stack_value_t value = EVAL_STACK_POP();

                // make sure that the field is in the same inheritance tree
                RuntimeTypeInfo field_type = tdn_get_intermediate_type(field->FieldType);
                CHECK(verifier_is_assignable(value.type, field_type));

                if (is_static) {
                    CHECK(field->Attributes.Static);
                } else {
                    jit_stack_value_t obj = EVAL_STACK_POP();

                    // must be either an instance or a byref
                    CHECK(tdn_type_is_referencetype(obj.type) || obj.type->IsByRef);

                    // make sure we can write to it if its a byref
                    if (obj.type->IsByRef) {
                        CHECK(obj.attrs.writable);
                    }

                    // if a byref make sure its readable
                    RuntimeTypeInfo type = obj.type->ElementType;

                    // if we are loading a byref, then it must mean the declaring type is also
                    // a byref struct
                    if (field->FieldType->IsByRef || field->FieldType->IsByRefStruct) {
                        CHECK(type->IsByRefStruct);

                        // if storing it in a nonlocal object, make sure that the
                        // value is also nonlocal
                        if (obj.attrs.nonlocal_ref) {
                            CHECK(value.attrs.nonlocal_ref);
                        }

                        // must be readable to store it
                        CHECK(value.attrs.readable);

                        // TODO: support for IsReadOnlyAttribute
                        CHECK(value.attrs.writable);
                    }

                    // make sure that the field is in the same inheritance tree
                    CHECK(verifier_is_assignable(type, field->DeclaringType));
                }
            } break;

            case CEE_LDFLD:
            case CEE_LDSFLD: {
                RuntimeFieldInfo field = inst.operand.field;
                bool is_static = inst.opcode == CEE_LDSFLD;

                jit_item_attrs_t attrs = {};

                if (is_static) {
                    CHECK(field->Attributes.Static);
                } else {
                    jit_stack_value_t obj = EVAL_STACK_POP();

                    // need one of these
                    CHECK(
                        tdn_type_is_referencetype(obj.type) ||
                        jit_is_struct(obj.type) ||
                        obj.type->IsByRef
                    );

                    // if a byref make sure its readable
                    RuntimeTypeInfo type = obj.type;
                    if (obj.type->IsByRef) {
                        CHECK(obj.attrs.readable);
                        type = obj.type->ElementType;
                    }

                    // if we are loading a byref, then it must mean the declaring type is also
                    // a byref struct
                    if (field->FieldType->IsByRef || field->FieldType->IsByRefStruct) {
                        CHECK(type->IsByRefStruct);

                        // the nonlocal is the same as the one we are loading from
                        attrs.nonlocal_ref = obj.attrs.nonlocal_ref;

                        // always readable
                        attrs.readable = true;

                        // TODO: check the IsReadOnlyAttribute
                        attrs.writable = true;
                    }

                    // make sure that the field is in the same inheritance tree
                    CHECK(verifier_is_assignable(type, field->DeclaringType));
                }

                // TODO: make sure we can access the field

                EVAL_STACK_PUSH(tdn_get_intermediate_type(field->FieldType), attrs);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Method calling
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_NEWOBJ:
            case CEE_CALL:
            case CEE_CALLVIRT: {
                RuntimeMethodBase target = inst.operand.method;
                RuntimeTypeInfo method_type = NULL;

                if (!target->Attributes.Static) {
                    method_type = target->DeclaringType;
                }

                // queue for more verification
                jit_method_t* target_method;
                CHECK_AND_RETHROW(jit_get_or_create_method(target, &target_method));
                jit_queue_verify(target_method);

                // TODO: check accessibility

                // TODO: check constraints

                if (inst.opcode == CEE_CALLVIRT) {
                    CHECK(method_type != NULL);
                    CHECK(!tdn_type_is_valuetype(method_type));

                } else if (inst.opcode == CEE_CALL) {
                    CHECK(!target->Attributes.Abstract);

                } else if (inst.opcode == CEE_NEWOBJ) {
                    CHECK(target->Attributes.RTSpecialName);
                    CHECK(!target->Attributes.Static);
                    CHECK(!method_type->IsArray);
                    CHECK(!method_type->Attributes.Abstract);

                    // we are creating an instance type, queue it
                    // for verification
                    CHECK_AND_RETHROW(jit_queue_type(method_type));
                }

                // TODO: delegate handling

                // check that the arguments are assignable, also checks if
                // the call might return a byref from its argument
                bool might_leak_byref = false;
                for (int i = target->Parameters->Length - 1; i >= 0; i--) {
                    ParameterInfo info = target->Parameters->Elements[i];
                    jit_stack_value_t value = EVAL_STACK_POP();

                    // check that we can pass the value
                    RuntimeTypeInfo declared = tdn_get_intermediate_type(info->ParameterType);
                    CHECK(verifier_is_assignable(value.type, declared));

                    // reference readability/writability checks
                    if (declared->IsByRef) {
                        if (info->Attributes.In) {
                            CHECK(value.attrs.readable);

                        } else if (info->Attributes.Out) {
                            CHECK(value.attrs.writable);

                        } else {
                            CHECK(value.attrs.readable);
                            CHECK(value.attrs.writable);
                        }
                    }

                    // check if we might leak a local reference from either a byref or a byref struct
                    if (declared->IsByRef || declared->IsByRefStruct) {
                        if (!value.attrs.nonlocal_ref) {
                            might_leak_byref = true;
                        }
                    }
                }

                // and finally check the this type
                if (inst.opcode != CEE_NEWOBJ && method_type != NULL) {
                    // the stack value
                    jit_stack_value_t value = EVAL_STACK_POP();

                    RuntimeTypeInfo target_this_type = method_type;
                    if (tdn_type_is_valuetype(method_type)) {
                        CHECK_AND_RETHROW(tdn_get_byref_type(method_type, &target_this_type));
                    }

                    // check the assignability
                    CHECK(verifier_is_assignable(value.type, target_this_type));

                    // check the readable/writable attributes
                    if (target_this_type->IsByRef) {
                        CHECK(value.attrs.readable);

                        // if calling non-readonly method make sure that we pass
                        // to it a writable reference
                        if (!target->IsReadOnly) {
                            CHECK(value.attrs.writable);
                        }
                    }

                    // if we use a call on a virtual function then it must be done on `this` instance
                    // and not on anything else
                    if (inst.opcode == CEE_CALL && target->Attributes.Virtual) {
                        CHECK(value.attrs.this_ptr);
                    }
                }

                //
                // push the return value
                //
                if (inst.opcode == CEE_NEWOBJ) {
                    EVAL_STACK_PUSH(method_type, .attrs = { .known_type = method_type });

                } else if (target->ReturnParameter->ParameterType != tVoid) {
                    RuntimeTypeInfo ret_type = tdn_get_intermediate_type(target->ReturnParameter->ParameterType);

                    jit_item_attrs_t attrs = {};

                    // if we can't leak the byref then it is def non-local
                    // this applies to both byref and byref struct
                    if (ret_type->IsByRef || ret_type->IsByRefStruct) {
                        attrs.nonlocal_ref = !might_leak_byref;
                    }

                    // only writable if not readonly
                    if (ret_type->IsByRef) {
                        attrs.readable = true;
                        attrs.writable = !target->ReturnParameter->IsReadonly;
                    }

                    // and finally push it
                    EVAL_STACK_PUSH(ret_type, attrs);
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Indirect access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDIND_I1:
            case CEE_LDIND_U1:
            case CEE_LDIND_I2:
            case CEE_LDIND_U2:
            case CEE_LDIND_I4:
            case CEE_LDIND_U4:
            case CEE_LDIND_I8:
            case CEE_LDIND_I:
            case CEE_LDIND_REF: {
                jit_stack_value_t address = EVAL_STACK_POP();
                CHECK(address.type->IsByRef);
                CHECK(address.attrs.readable);

                // check we can perform the load
                RuntimeTypeInfo type = address.type->ElementType;
                if (inst.operand.type == NULL) {
                    CHECK(tdn_type_is_referencetype(type));
                } else {
                    CHECK(type == inst.operand.type);
                    type = tdn_get_intermediate_type(type);
                }

                // push it
                EVAL_STACK_PUSH(type);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Math
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_ADD:
            case CEE_SUB:
            case CEE_MUL:
            case CEE_DIV:
            case CEE_REM:
            case CEE_AND:
            case CEE_OR:
            case CEE_XOR:
            case CEE_DIV_UN:
            case CEE_REM_UN: {
                jit_stack_value_t op1 = EVAL_STACK_POP();
                jit_stack_value_t op2 = EVAL_STACK_POP();

                // check the inputs and get the result type
                RuntimeTypeInfo result = NULL;
                if (op1.type == tInt32) {
                    if (op2.type == tInt32) {
                        result = tInt32;
                    } else if (op2.type == tIntPtr) {
                        result = tIntPtr;
                    } else {
                        CHECK_FAIL();
                    }

                } else if (op1.type == tInt64) {
                    CHECK(op2.type == tInt64);

                } else if (op1.type == tIntPtr) {
                    CHECK(op2.type == tInt32 || op2.type == tIntPtr);
                    result = tIntPtr;

                } else {
                    CHECK_FAIL();
                }

                EVAL_STACK_PUSH(result);
            } break;

            case CEE_CEQ:
            case CEE_CGT:
            case CEE_CLT:
            case CEE_CGT_UN:
            case CEE_CLT_UN: {
                jit_stack_value_t value1 = EVAL_STACK_POP();
                jit_stack_value_t value2 = EVAL_STACK_POP();
                CHECK(verifier_is_binary_comparable(value1.type, value2.type, inst.opcode));
                EVAL_STACK_PUSH(tInt32);
            } break;

            case CEE_CONV_I1:
            case CEE_CONV_I2:
            case CEE_CONV_I4:
            case CEE_CONV_I8:
            case CEE_CONV_U1:
            case CEE_CONV_U2:
            case CEE_CONV_U4:
            case CEE_CONV_U8:
            case CEE_CONV_I:
            case CEE_CONV_U: {
                jit_stack_value_t value = EVAL_STACK_POP();

                CHECK(
                    value.type == tInt32 ||
                    value.type == tInt64 ||
                    value.type == tIntPtr
                );

                if (inst.opcode == CEE_CONV_I || inst.opcode == CEE_CONV_U) {
                    EVAL_STACK_PUSH(tIntPtr);

                } else if (inst.opcode == CEE_CONV_U8 || inst.opcode == CEE_CONV_I8) {
                    EVAL_STACK_PUSH(tInt64);

                } else {
                    EVAL_STACK_PUSH(tInt32);
                }
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

            case CEE_LDSTR: {
                EVAL_STACK_PUSH(tString);
            } break;

            case CEE_LDNULL: {
                EVAL_STACK_PUSH(NULL);
            } break;

            case CEE_LDTOKEN: {
                EVAL_STACK_PUSH(tRuntimeTypeHandle);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Branching
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_BR: {
                // just a branch, merge with the block target
                CHECK_AND_RETHROW(verify_merge_basic_block(jmethod, inst.operand.branch_target, stack, locals));
            } break;

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
                jit_stack_value_t value1 = EVAL_STACK_POP();
                jit_stack_value_t value2 = EVAL_STACK_POP();
                CHECK(verifier_is_binary_comparable(value1.type, value2.type, inst.opcode));

                // merge with the target
                verify_merge_basic_block(jmethod, inst.operand.branch_target, stack, locals);

                // merge with the next block
                verify_merge_basic_block(jmethod, pc, stack, locals);
            } break;

            case CEE_BRFALSE:
            case CEE_BRTRUE: {
                jit_stack_value_t value = EVAL_STACK_POP();

                // ECMA-335 doesn't say brtrue takes in anything but
                // O and native int, but I think its just an oversight
                CHECK(
                    tdn_type_is_referencetype(value.type) ||
                    value.type == tIntPtr ||
                    value.type == tInt64 ||
                    value.type == tInt32
                );

                // merge with the target
                verify_merge_basic_block(jmethod, inst.operand.branch_target, stack, locals);

                // merge with the next block
                verify_merge_basic_block(jmethod, pc, stack, locals);
            } break;

            case CEE_RET: {
                RuntimeTypeInfo return_type = tdn_get_intermediate_type(method->ReturnParameter->ParameterType);

                if (return_type == tVoid) {
                    // must have a clear stack at this point
                    CHECK(arrlen(stack) == 0);
                } else {
                    jit_stack_value_t value = EVAL_STACK_POP();
                    CHECK(verifier_is_assignable(value.type, return_type));

                    // if this is a byref or byref struct then make sure we don't return
                    // a local reference
                    if (value.type->IsByRef || value.type->IsByRefStruct) {
                        CHECK(value.attrs.nonlocal_ref);
                    }

                    // for byref check the readable/writable requirements
                    if (value.type->IsByRef) {
                        CHECK(value.attrs.readable);

                        // make sure its writable
                        if (!method->ReturnParameter->IsReadonly) {
                            CHECK(value.attrs.writable);
                        }
                    }
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Exception handling
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_THROW: {
                EVAL_STACK_POP();

                // TODO: verify inherits from exception

                // empty the stack
                arrsetlen(stack, 0);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Misc
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // nothing to do
            case CEE_NOP: break;

            default: CHECK_FAIL("Unknown opcode");
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
