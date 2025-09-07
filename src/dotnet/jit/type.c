#include "type.h"

#include <dotnet/types.h>
#include <tomatodotnet/types/type.h>
#include <util/string.h>
#include <util/except.h>
#include "tomatodotnet/util/stb_ds.h"

static void type_queue_block(jit_function_t* function, jit_block_t* block) {
    if (!block->in_queue) {
        arrpush(function->queue, block);
        block->visited = true;
        block->in_queue = true;
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Stack slot merging
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum merge_status {
    MERGE_SUCCESS,
    MERGE_FAILED,
    MERGE_MODIFIED,
} merge_status_t;

// we are going to use the verifier object merging because it is
// pretty generic and will fit well with
RuntimeTypeInfo verifier_merge_object_references(RuntimeTypeInfo class_a, RuntimeTypeInfo class_b);

static merge_status_t type_merge_stack_values(jit_stack_value_t* previous, jit_stack_value_t* new) {
    bool modified = false;

    // methods don't match, zero the method
    // NOTE: this can only be from a delegate devirt
    if (previous->method != new->method) {
        previous->method = NULL;
        modified = true;
    }

    // if we don't match in the type we need to merge it
    if (previous->kind != new->kind || previous->type != new->type) {
        if (jit_is_null_reference(previous)) {
            if (new->kind == JIT_KIND_OBJ_REF) {
                // we had a null-reference, but now we have a real type
                previous->type = new->type;
                modified = true;
            }
        } else if (previous->kind == JIT_KIND_OBJ_REF) {
            // must also be an object reference
            if (new->kind != JIT_KIND_OBJ_REF) {
                return MERGE_FAILED;
            }

            if (!jit_is_null_reference(new)) {
                RuntimeTypeInfo merged = verifier_merge_object_references(previous->type, new->type);
                if (merged == NULL) {
                    return MERGE_FAILED;
                }

                if (merged != previous->type) {
                    // we got a different type
                    previous->type = merged;
                    modified = true;
                }
            }
        } else {
            // otherwise the kind and type must match
            return MERGE_FAILED;
        }
    }

    return modified ? MERGE_MODIFIED : MERGE_SUCCESS;
}

static bool type_merge_block_local(jit_block_local_t* previous, jit_block_local_t* new) {
    bool modified = false;

    // an uninitialized entry has entered an initialized one, we are going
    // to mark as not initialized
    if (previous->initialized && !new->initialized) {
        previous->initialized = false;
        return true;
    }

    // ignore the local if its not initialized
    if (!previous->initialized) {
        return false;
    }

    // methods don't match, zero the method
    // NOTE: this can only be from a delegate devirt
    if (previous->method != new->method) {
        previous->method = NULL;
        modified = true;
    }

    return modified;
}

static tdn_err_t type_merge_blocks(jit_function_t* function, jit_block_t* from, jit_block_t* target) {
    tdn_err_t err = TDN_NO_ERROR;

    // we check against multiple_predecessors as well for the case of the second pass
    // so we will know we need to merge nicely
    if (target->visited || target->multiple_predecessors) {
        // already visited once, need to merge with the
        // type instead of setting everything as is
        target->multiple_predecessors = true;

        bool modified = false;

        for (int i = 0; i < arrlen(from->args); i++) {
            if (type_merge_block_local(&target->args[i], &from->args[i])) {
                modified = true;
            }
        }

        for (int i = 0; i < arrlen(from->locals); i++) {
            if (type_merge_block_local(&target->locals[i], &from->locals[i])) {
                modified = true;
            }
        }

        // must have the same length at this point
        CHECK(arrlen(from->stack) == arrlen(target->stack));

        for (int i = 0; i < arrlen(from->stack); i++) {
            // must have the same kind at least
            CHECK(target->stack[i].kind == from->stack[i].kind);

            // now attempt to merge the two values
            merge_status_t status = type_merge_stack_values(&target->stack[i], &from->stack[i]);
            CHECK(status != MERGE_FAILED);
            if (status == MERGE_MODIFIED) {
                modified = true;
            }
        }

        // the metadata of the block was modified, we must
        if (modified) {
            // blocks should not be modified while emitting
            CHECK(!function->emitting);
            type_queue_block(function, target);

        } else if (!target->visited) {
            type_queue_block(function, target);
        }

    } else {
        // first time being visited, copy over all the type information as is
        arrsetlen(target->stack, arrlen(from->stack));
        memcpy(target->stack, from->stack, arrlen(from->stack) * sizeof(*from->stack));

        arrsetlen(target->args, arrlen(from->args));
        memcpy(target->args, from->args, arrlen(from->args) * sizeof(*from->args));

        arrsetlen(target->locals, arrlen(from->locals));
        memcpy(target->locals, from->locals, arrlen(from->locals) * sizeof(*from->locals));

        type_queue_block(function, target);
    }

cleanup:
    return err;
}

static tdn_err_t type_propagate_control_flow(jit_function_t* function, jit_block_t* from, jit_block_t* target, bool is_fallthrough) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure we can perform the branch
    CHECK_AND_RETHROW(type_merge_blocks(function, from, target));

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Verifiers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define STACK_PUSH() \
    ({ \
        jit_stack_value_t* __value = arraddnptr(block->stack, 1); \
        memset(__value, 0, sizeof(*__value)); \
        __value; \
    })

//----------------------------------------------------------------------------------------------------------------------
// Misc instructions
//----------------------------------------------------------------------------------------------------------------------

// Use as a template for adding new instructions
static tdn_err_t type_nop(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return TDN_NO_ERROR;
}

static tdn_err_t type_sizeof(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), tInt32);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Local access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t type_load_local(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_block_local_t* block_locals = is_arg ? block->args : block->locals;
    jit_local_t* function_locals = is_arg ? function->args : function->locals;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block_locals));
    jit_block_local_t* block_local = &block_locals[inst->operand.variable];
    jit_local_t* func_local = &function_locals[inst->operand.variable];

    // check if we need an initializer
    if (!block_local->initialized) {
        func_local->zero_initialize = true;
        block_local->initialized = true;
    }

    // push the new type to the stack, copy the flags and method pointers
    jit_stack_value_t* value = jit_stack_value_init(STACK_PUSH(), func_local->type);
    value->method = block_local->method;

cleanup:
    return err;
}

static tdn_err_t type_store_local(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_block_local_t* block_locals = is_arg ? block->args : block->locals;
    jit_local_t* function_locals = is_arg ? function->args : function->locals;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block_locals));
    jit_block_local_t* block_local = &block_locals[inst->operand.variable];
    jit_local_t* func_local = &function_locals[inst->operand.variable];

    // initialized, no need to zero initialize later on
    block_local->initialized = true;

cleanup:
    return err;
}

static tdn_err_t type_load_local_address(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_block_local_t* block_locals = is_arg ? block->args : block->locals;
    jit_local_t* function_locals = is_arg ? function->args : function->locals;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block_locals));
    jit_block_local_t* block_local = &block_locals[inst->operand.variable];
    jit_local_t* func_local = &function_locals[inst->operand.variable];

    // check if we need an initializer
    // TODO: when we do location tracking have some lazy
    //       check for the zero initialzie (since ldloca -> initobj
    //       is a pretty common pattern)
    if (!block_local->initialized) {
        func_local->zero_initialize = true;
        block_local->initialized = true;
    }

    // local taken by reference must be spilled
    func_local->spilled = true;

    // setup the new stack value
    jit_stack_value_t* value = STACK_PUSH();
    value->kind = JIT_KIND_BY_REF;
    value->type = func_local->type;

cleanup:
    return err;
}

static tdn_err_t type_ldarg(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return type_load_local(function, block, inst, true);
}

static tdn_err_t type_starg(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return type_store_local(function, block, inst, stack, true);
}

static tdn_err_t type_ldarga(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return type_load_local_address(function, block, inst, true);
}

static tdn_err_t type_ldloc(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return type_load_local(function, block, inst, false);
}

static tdn_err_t type_stloc(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return type_store_local(function, block, inst, stack, false);
}

static tdn_err_t type_ldloca(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return type_load_local_address(function, block, inst, false);
}

//----------------------------------------------------------------------------------------------------------------------
// Misc instructions
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t type_ldfld(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // push the value to the stack
    RuntimeFieldInfo field = inst->operand.field;
    jit_stack_value_init(STACK_PUSH(), field->FieldType);

cleanup:
    return err;
}

static tdn_err_t type_ldflda(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // push the value to the stack
    RuntimeFieldInfo field = inst->operand.field;
    jit_stack_value_t* value = STACK_PUSH();
    value->kind = JIT_KIND_BY_REF;
    value->type = field->FieldType;

cleanup:
    return err;
}

static tdn_err_t type_stfld(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    // there is nothing to do for set field regarding type info
    // TODO: maybe for ref-structs we can track some additional type information?
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// Reference access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t type_initobj(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return TDN_NO_ERROR;
}

static tdn_err_t type_localloc(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // both int32 and native int are allowed, we are going to let this pass verification
    // because the pointer is not something we care too much about
    CHECK(
        stack->kind == JIT_KIND_INT32 ||
        stack->kind == JIT_KIND_NATIVE_INT
    );

    RuntimeTypeInfo ptr = NULL;
    CHECK_AND_RETHROW(tdn_get_pointer_type(tVoid, &ptr));

    // its just a pointer
    jit_stack_value_init(STACK_PUSH(), ptr);

cleanup:
    return err;
}

static tdn_err_t type_ldind(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), inst->operand.type);

cleanup:
    return err;
}

static tdn_err_t type_stind(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// Stack manipulation
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t type_ldnull(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_t* value = STACK_PUSH();
    value->kind = JIT_KIND_OBJ_REF;
    value->type = NULL;

cleanup:
    return err;
}

static tdn_err_t type_ldstr(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), tString);

cleanup:
    return err;
}

static tdn_err_t type_ldtoken(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeTypeInfo type = NULL;
    switch (inst->operand_type) {
        case TDN_IL_TYPE: type = tRuntimeTypeHandle; break;
        case TDN_IL_FIELD: type = tRuntimeFieldHandle; break;
        case TDN_IL_METHOD: type = tRuntimeMethodHandle; break;
        default: CHECK_FAIL();
    }
    jit_stack_value_init(STACK_PUSH(), type);

cleanup:
    return err;
}

static tdn_err_t type_ldc_i4(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), tInt32);

cleanup:
    return err;
}

static tdn_err_t type_ldc_i8(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), tInt64);

cleanup:
    return err;
}

static tdn_err_t type_ldc_r4(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), tSingle);

cleanup:
    return err;
}

static tdn_err_t type_ldc_r8(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), tDouble);

cleanup:
    return err;
}

static tdn_err_t type_dup(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    *STACK_PUSH() = *stack;
    *STACK_PUSH() = *stack;

cleanup:
    return err;
}

static tdn_err_t type_pop(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// Arith and compare operations
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t type_binary_op(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // Stack value kind is ordered to make this work
    jit_stack_value_t result = (stack[0].kind > stack[1].kind) ? stack[0] : stack[1];

    // ensure that whatever the native int was its degraded into
    // an integer and not stay as a pointer
    if (result.kind == JIT_KIND_NATIVE_INT) {
        result.type = tIntPtr;
    }

    CHECK((stack[0].kind == stack[1].kind) || (result.kind == JIT_KIND_NATIVE_INT));

    *STACK_PUSH() = result;

cleanup:
    return err;
}

static tdn_err_t type_unary_op(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // same as input
    *STACK_PUSH() = *stack;

cleanup:
    return err;
}

static tdn_err_t type_shift(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // same as the to be shifted value
    *STACK_PUSH() = stack[0];

cleanup:
    return err;
}

static tdn_err_t type_conv(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // always pushes as an int32
    RuntimeTypeInfo type = NULL;
    switch (inst->opcode) {
        case CEE_CONV_OVF_I1:
        case CEE_CONV_OVF_I2:
        case CEE_CONV_OVF_I4:
        case CEE_CONV_OVF_U1:
        case CEE_CONV_OVF_U2:
        case CEE_CONV_OVF_U4:
        case CEE_CONV_OVF_I1_UN:
        case CEE_CONV_OVF_I2_UN:
        case CEE_CONV_OVF_I4_UN:
        case CEE_CONV_OVF_U1_UN:
        case CEE_CONV_OVF_U2_UN:
        case CEE_CONV_OVF_U4_UN:
        case CEE_CONV_I1:
        case CEE_CONV_I2:
        case CEE_CONV_I4:
        case CEE_CONV_U1:
        case CEE_CONV_U2:
        case CEE_CONV_U4: type = tInt32; break;
        case CEE_CONV_OVF_I8:
        case CEE_CONV_OVF_U8:
        case CEE_CONV_OVF_I8_UN:
        case CEE_CONV_OVF_U8_UN:
        case CEE_CONV_I8:
        case CEE_CONV_U8: type = tInt64; break;
        case CEE_CONV_OVF_I:
        case CEE_CONV_OVF_U:
        case CEE_CONV_OVF_I_UN:
        case CEE_CONV_OVF_U_UN:
        case CEE_CONV_I:
        case CEE_CONV_U: type = tIntPtr; break;
        case CEE_CONV_R4: type = tSingle; break;
        case CEE_CONV_R_UN:
        case CEE_CONV_R8: type = tDouble; break;
        default: CHECK_FAIL();
    }

    jit_stack_value_init(STACK_PUSH(), type);

cleanup:
    return err;
}

static tdn_err_t type_compare(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // always pushed as int32
    jit_stack_value_init(STACK_PUSH(), tInt32);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Array related
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t type_newarr(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeTypeInfo array_type;
    CHECK_AND_RETHROW(tdn_get_array_type(inst->operand.type, &array_type));

    jit_stack_value_init(STACK_PUSH(), array_type);

cleanup:
    return err;
}

static tdn_err_t type_ldlen(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // return value is an intptr
    jit_stack_value_init(STACK_PUSH(), tIntPtr);

cleanup:
    return err;
}

static tdn_err_t type_ldelem(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_t* index = &stack[1];
    jit_stack_value_t* array = &stack[0];

    jit_stack_value_init(STACK_PUSH(), array->type->ElementType);

cleanup:
    return err;
}

static tdn_err_t type_stelem(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return TDN_NO_ERROR;
}

static tdn_err_t type_ldelema(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_t* index = &stack[1];
    jit_stack_value_t* array = &stack[0];

    jit_stack_value_t* push = STACK_PUSH();
    push->kind = JIT_KIND_BY_REF;
    push->type = array->type->ElementType;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Method related
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t type_ldftn(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = inst->operand.method;


    // must not be a constructor
    CHECK_ERROR(!tdn_compare_string_to_cstr(inst->operand.method->Name, ".ctor"),
        TDN_ERROR_VERIFIER_LDFTN_CTOR);

    RuntimeTypeInfo instance = NULL;
    if (inst->opcode == CEE_LDVIRTFTN) {
        // TODO: devirt at this point already?
        CHECK(!method->Attributes.Static);

        // We want the boxed value for the comparison
        jit_stack_value_t declared_type = {
            .kind = JIT_KIND_OBJ_REF,
            .type = method->DeclaringType
        };
    }

    // special case, since its only used to pass it to the newobj, we verify
    // externally that the opcodes are valid
    jit_stack_value_t* value = STACK_PUSH();
    value->kind = JIT_KIND_NATIVE_INT;
    value->method = method;

cleanup:
    return err;
}

static tdn_err_t type_call(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeMethodBase method = inst->operand.method;
    RuntimeTypeInfo method_type = method->Attributes.Static ? NULL : method->DeclaringType;

    if (inst->opcode == CEE_NEWOBJ) {
        jit_stack_value_t* value = STACK_PUSH();
        jit_stack_value_init(value, method_type);

    } else if (method->ReturnParameter->ParameterType != tVoid) {
        jit_stack_value_t* return_value = STACK_PUSH();
        jit_stack_value_init(return_value, method->ReturnParameter->ParameterType);
    }

cleanup:
    return err;
}

static tdn_err_t type_ret(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// type checking
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t type_castclass(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: if we are doing an upcast then change nothing, if we are doing a downcast
    //       indeed set the new type, this way we only ever gain information and not
    //       lose it

    // and push it
    jit_stack_value_t* pushed = STACK_PUSH();
    pushed->kind = JIT_KIND_OBJ_REF;
    pushed->type = inst->operand.type;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Boxing
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t type_box(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeTypeInfo type = inst->operand.type;

    // For Nullable<T> we push T
    jit_stack_value_t* pushed = STACK_PUSH();
    pushed->kind = JIT_KIND_OBJ_REF;
    pushed->type = tdn_type_is_nullable(type) ? type->GenericArguments->Elements[0] : type;

cleanup:
    return err;
}

static tdn_err_t type_unbox(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    if (inst->opcode == CEE_UNBOX_ANY) {
        jit_stack_value_init(STACK_PUSH(), inst->operand.type);
    } else {
        jit_stack_value_t* value = STACK_PUSH();
        value->kind = JIT_KIND_BY_REF;
        value->type = inst->operand.type;
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Branching
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t type_br(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);

    CHECK_AND_RETHROW(type_propagate_control_flow(function, block, target, false));

cleanup:
    return err;
}

static tdn_err_t type_br_unary_cond(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);
    jit_block_t* next = jit_function_get_block(function, block->block.end, block->leave_target_stack);
    CHECK(next != NULL);

    CHECK_AND_RETHROW(type_propagate_control_flow(function, block, target, false));
    CHECK_AND_RETHROW(type_propagate_control_flow(function, block, next, true));

cleanup:
    return err;
}

static tdn_err_t type_br_binary_cond(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);
    jit_block_t* next = jit_function_get_block(function, block->block.end, block->leave_target_stack);
    CHECK(next != NULL);

    CHECK_AND_RETHROW(type_propagate_control_flow(function, block, target, false));
    CHECK_AND_RETHROW(type_propagate_control_flow(function, block, next, true));

cleanup:
    return err;
}

static tdn_err_t type_switch(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure we have an int32 on the stack
    // TODO: can we have anything else?
    CHECK(stack->kind == JIT_KIND_INT32);

    // verify the default case
    jit_block_t* next = jit_function_get_block(function, block->block.end, block->leave_target_stack);
    CHECK(next != NULL);
    CHECK_AND_RETHROW(type_propagate_control_flow(function, block, next, true));

    // verify the rest of the blocks
    for (int i = 0; i < arrlen(inst->operand.switch_targets); i++) {
        jit_block_t* target = jit_function_get_block(function, inst->operand.switch_targets[i], block->leave_target_stack);
        CHECK(target != NULL);
        CHECK_AND_RETHROW(type_propagate_control_flow(function, block, target, false));
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Exceptions
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t type_leave(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // empty the stack
    arrsetlen(block->stack, 0);

    // TODO: is valid leave target

    // find the block that is around the target
    RuntimeExceptionHandlingClause target_clause = jit_get_enclosing_try_clause(function, inst->operand.branch_target, COR_ILEXCEPTION_CLAUSE_FINALLY, NULL);

    // find all the handlers that we need to go through and remember them
    int inst_off = arrlen(block->leave_target_stack);
    RuntimeExceptionHandlingClause clause = NULL;
    for (;;) {
        // we reached the same clause as the one around the leave target, so we can stop now, we found
        // the real target to jump to
        clause = jit_get_enclosing_try_clause(function, inst->pc, COR_ILEXCEPTION_CLAUSE_FINALLY, clause);
        if (target_clause == clause) {
            break;
        }
        CHECK(clause != NULL);

        // remember that we need to call this
        arrins(block->leave_target_stack, inst_off, clause->HandlerOffset);
    }

    // add the actual target we want to have, since this is where we want to eventually go
    arrins(block->leave_target_stack, inst_off, inst->operand.branch_target);

    // now find the actual entry we want to go to
    uint32_t target_pc = arrpop(block->leave_target_stack);
    jit_block_t* target = jit_function_get_block(function, target_pc, block->leave_target_stack);
    CHECK(target != NULL);

    // merge with the target block
    CHECK_AND_RETHROW(type_merge_blocks(function, block, target));

    // override the target pc with the actual target we want to go to
    inst->operand.branch_target = target_pc;

cleanup:
    return err;
}

static tdn_err_t type_endfinally(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // empty the stack
    arrsetlen(block->stack, 0);

    // ensure we have a handler clause for the block
    CHECK_ERROR(block->block.handler_clause != NULL,
        TDN_ERROR_VERIFIER_ENDFINALLY);

    // ensure the handler is a finally/fault region
    CHECK_ERROR(
        block->block.handler_clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY ||
        block->block.handler_clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT,
        TDN_ERROR_VERIFIER_ENDFINALLY);

    // ensure we have a valid leave target stack
    CHECK(block->leave_target_stack != NULL);
    uint32_t target_pc = arrpop(block->leave_target_stack);
    jit_block_t* target = jit_function_get_block(function, target_pc, block->leave_target_stack);
    CHECK(target != NULL);

    // and now merge with the target
    CHECK_AND_RETHROW(type_merge_blocks(function, block, target));

    // override the target pc in the instruction
    // for the emitter to know where to jump into
    inst->operand.branch_target = target_pc;

cleanup:
    return err;
}

static tdn_err_t type_throw(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // empty the stack
    arrsetlen(block->stack, 0);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Dispatch tables
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type_instruction_t g_type_dispatch_table[] = {
    [CEE_NOP] = type_nop,

    [CEE_LDARG] = type_ldarg,
    [CEE_STARG] = type_starg,
    [CEE_LDARGA] = type_ldarga,

    [CEE_LDLOC] = type_ldloc,
    [CEE_STLOC] = type_stloc,
    [CEE_LDLOCA] = type_ldloca,

    [CEE_LDFLDA] = type_ldflda,
    [CEE_LDFLD] = type_ldfld,
    [CEE_STFLD] = type_stfld,
    [CEE_LDSFLDA] = type_ldflda,
    [CEE_LDSFLD] = type_ldfld,
    [CEE_STSFLD] = type_stfld,

    [CEE_INITOBJ] = type_initobj,
    [CEE_LOCALLOC] = type_localloc,

    [CEE_LDIND_I1] = type_ldind,
    [CEE_LDIND_U1] = type_ldind,
    [CEE_LDIND_I2] = type_ldind,
    [CEE_LDIND_U2] = type_ldind,
    [CEE_LDIND_I4] = type_ldind,
    [CEE_LDIND_U4] = type_ldind,
    [CEE_LDIND_I8] = type_ldind,
    [CEE_LDIND_I] = type_ldind,
    [CEE_LDIND_REF] = type_ldind,
    [CEE_LDIND_R8] = type_ldind,
    [CEE_LDOBJ] = type_ldind,

    [CEE_STIND_I] = type_stind,
    [CEE_STIND_I1] = type_stind,
    [CEE_STIND_I2] = type_stind,
    [CEE_STIND_I4] = type_stind,
    [CEE_STIND_I8] = type_stind,
    [CEE_STIND_REF] = type_stind,
    [CEE_STIND_R8] = type_stind,
    [CEE_STOBJ] = type_stind,

    [CEE_LDNULL] = type_ldnull,
    [CEE_LDSTR] = type_ldstr,
    [CEE_LDTOKEN] = type_ldtoken,
    [CEE_LDC_I4] = type_ldc_i4,
    [CEE_LDC_I8] = type_ldc_i8,
    [CEE_LDC_R4] = type_ldc_r4,
    [CEE_LDC_R8] = type_ldc_r8,
    [CEE_DUP] = type_dup,
    [CEE_POP] = type_pop,

    [CEE_ADD] = type_binary_op,
    [CEE_SUB] = type_binary_op,
    [CEE_MUL] = type_binary_op,
    [CEE_DIV] = type_binary_op,
    [CEE_DIV_UN] = type_binary_op,
    [CEE_REM] = type_binary_op,
    [CEE_REM_UN] = type_binary_op,
    [CEE_AND] = type_binary_op,
    [CEE_OR] = type_binary_op,
    [CEE_XOR] = type_binary_op,
    [CEE_ADD_OVF] = type_binary_op,
    [CEE_ADD_OVF_UN] = type_binary_op,
    [CEE_SUB_OVF] = type_binary_op,
    [CEE_SUB_OVF_UN] = type_binary_op,
    [CEE_MUL_OVF] = type_binary_op,
    [CEE_MUL_OVF_UN] = type_binary_op,

    [CEE_NEG] = type_unary_op,
    [CEE_NOT] = type_unary_op,

    [CEE_SHR] = type_shift,
    [CEE_SHL] = type_shift,
    [CEE_SHR_UN] = type_shift,

    [CEE_CONV_OVF_I1] = type_conv,
    [CEE_CONV_OVF_I2] = type_conv,
    [CEE_CONV_OVF_I4] = type_conv,
    [CEE_CONV_OVF_U1] = type_conv,
    [CEE_CONV_OVF_U2] = type_conv,
    [CEE_CONV_OVF_U4] = type_conv,
    [CEE_CONV_OVF_I1_UN] = type_conv,
    [CEE_CONV_OVF_I2_UN] = type_conv,
    [CEE_CONV_OVF_I4_UN] = type_conv,
    [CEE_CONV_OVF_U1_UN] = type_conv,
    [CEE_CONV_OVF_U2_UN] = type_conv,
    [CEE_CONV_OVF_U4_UN] = type_conv,
    [CEE_CONV_OVF_I8] = type_conv,
    [CEE_CONV_OVF_U8] = type_conv,
    [CEE_CONV_OVF_I8_UN] = type_conv,
    [CEE_CONV_OVF_U8_UN] = type_conv,
    [CEE_CONV_OVF_I] = type_conv,
    [CEE_CONV_OVF_U] = type_conv,
    [CEE_CONV_OVF_I_UN] = type_conv,
    [CEE_CONV_OVF_U_UN] = type_conv,
    [CEE_CONV_I1] = type_conv,
    [CEE_CONV_I2] = type_conv,
    [CEE_CONV_I4] = type_conv,
    [CEE_CONV_I8] = type_conv,
    [CEE_CONV_U1] = type_conv,
    [CEE_CONV_U2] = type_conv,
    [CEE_CONV_U4] = type_conv,
    [CEE_CONV_U8] = type_conv,
    [CEE_CONV_I] = type_conv,
    [CEE_CONV_U] = type_conv,
    [CEE_CONV_R4] = type_conv,
    [CEE_CONV_R8] = type_conv,
    [CEE_CONV_R_UN] = type_conv,

    [CEE_CEQ] = type_compare,
    [CEE_CGT] = type_compare,
    [CEE_CGT_UN] = type_compare,
    [CEE_CLT] = type_compare,
    [CEE_CLT_UN] = type_compare,

    [CEE_NEWARR] = type_newarr,
    [CEE_LDLEN] = type_ldlen,
    [CEE_LDELEMA] = type_ldelema,
    [CEE_LDELEM] = type_ldelem,
    [CEE_LDELEM_REF] = type_ldelem,
    [CEE_STELEM] = type_stelem,
    [CEE_STELEM_REF] = type_stelem,

    [CEE_LDFTN] = type_ldftn,
    [CEE_LDVIRTFTN] = type_ldftn,

    [CEE_NEWOBJ] = type_call,
    [CEE_CALL] = type_call,
    [CEE_CALLVIRT] = type_call,

    [CEE_RET] = type_ret,

    [CEE_CASTCLASS] = type_castclass,
    [CEE_ISINST] = type_castclass,

    [CEE_BOX] = type_box,
    [CEE_UNBOX] = type_unbox,
    [CEE_UNBOX_ANY] = type_unbox,

    [CEE_BR] = type_br,
    [CEE_BRFALSE] = type_br_unary_cond,
    [CEE_BRTRUE] = type_br_unary_cond,
    [CEE_BEQ] = type_br_binary_cond,
    [CEE_BGE] = type_br_binary_cond,
    [CEE_BGT] = type_br_binary_cond,
    [CEE_BLE] = type_br_binary_cond,
    [CEE_BLT] = type_br_binary_cond,
    [CEE_BNE_UN] = type_br_binary_cond,
    [CEE_BGE_UN] = type_br_binary_cond,
    [CEE_BGT_UN] = type_br_binary_cond,
    [CEE_BLE_UN] = type_br_binary_cond,
    [CEE_BLT_UN] = type_br_binary_cond,
    [CEE_SWITCH] = type_switch,

    [CEE_LEAVE] = type_leave,
    [CEE_ENDFINALLY] = type_endfinally,
    [CEE_THROW] = type_throw,

    [CEE_SIZEOF] = type_sizeof,
};
size_t g_type_dispatch_table_size = ARRAY_LENGTH(g_type_dispatch_table);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Entry points
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t type_on_block_fallthrough(jit_function_t* function, jit_block_t* from, jit_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_AND_RETHROW(type_propagate_control_flow(function, from, block, true));

cleanup:
    return err;
}
