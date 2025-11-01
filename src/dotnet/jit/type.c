#include "type.h"

#include <stdalign.h>
#include <dotnet/types.h>
#include <tomatodotnet/types/type.h>
#include <util/string.h>
#include <util/except.h>

#include "dotnet/verifier/casting.h"
#include "dotnet/verifier/internal.h"
#include "tomatodotnet/tdn.h"
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

static RuntimeTypeInfo type_merge_actual_type(RuntimeTypeInfo actual_type_a, RuntimeTypeInfo actual_type_b) {
    // merge the types
    if (actual_type_a == NULL || actual_type_b == NULL) {
        // one of the side has no known type, ignore it
        actual_type_a = NULL;

    } else {
        // both have a known type, find something common about it
        actual_type_a = verifier_merge_object_references(actual_type_a, actual_type_b);
        if (actual_type_a == tObject) {
            // if we get to object just assume there is no known type
            actual_type_a = NULL;
        }
    }

    return actual_type_a;
}

static merge_status_t type_merge_stack_values(jit_stack_value_t* previous, jit_stack_value_t* new) {
    bool modified = false;

    // methods don't match, zero the method
    // NOTE: this can only be from a delegate devirt
    if (previous->method != new->method) {
        if (previous->method != NULL) {
            previous->method = NULL;
            modified = true;
        }
    }

    if (previous->actual_type != new->actual_type) {
        previous->is_actual_type_exact = false;
        RuntimeTypeInfo type = type_merge_actual_type(previous->actual_type, new->actual_type);
        if (type != previous->actual_type || previous->is_actual_type_exact) {
            previous->is_actual_type_exact = false;
            previous->actual_type = type;
            modified = true;
        }
    }

    if (previous->is_actual_type_exact != new->is_actual_type_exact) {
        if (previous->is_actual_type_exact) {
            previous->is_actual_type_exact = false;
            modified = true;
        }
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
        if (previous->method != NULL) {
            previous->method = NULL;
            modified = true;
        }
    }

    if (previous->actual_type != new->actual_type) {
        // it can't be the actual type since we have two different types
        previous->is_actual_type_exact = false;
        RuntimeTypeInfo type = type_merge_actual_type(previous->actual_type, new->actual_type);
        if (type != previous->actual_type || previous->is_actual_type_exact) {
            previous->is_actual_type_exact = false;
            previous->actual_type = type;
            modified = true;
        }
    }

    if (previous->is_actual_type_exact != new->is_actual_type_exact) {
        if (previous->is_actual_type_exact) {
            previous->is_actual_type_exact = false;
            modified = true;
        }
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
    value->actual_type = block_local->actual_type;
    value->is_actual_type_exact = block_local->is_actual_type_exact;
    value->method = block_local->method;

cleanup:
    return err;
}

static tdn_err_t type_store_local(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_block_local_t* block_locals = is_arg ? block->args : block->locals;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block_locals));
    jit_block_local_t* block_local = &block_locals[inst->operand.variable];

    // initialized, no need to zero initialize later on
    block_local->initialized = true;
    block_local->actual_type = jit_get_actual_type(stack);
    block_local->is_actual_type_exact = stack->is_actual_type_exact;
    block_local->method = stack->method;

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

    // TODO: do we want to remember the actual
    //       information in here? probably we do?

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

    RuntimeTypeInfo type = inst->operand.type;
    if (type == NULL) {
        type = tObject;
    }
    jit_stack_value_init(STACK_PUSH(), type);

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

    if (
        // optimization for the case of ptr +/- value => ptr
        (inst->opcode == CEE_ADD || inst->opcode == CEE_SUB) &&
        jit_is_pointer(&stack[0]) && !jit_is_pointer(&stack[1])
    ) {
        result = stack[0];

    } else if (
        // optimization for the case of value + ptr => ptr
        inst->opcode == CEE_ADD &&
        !jit_is_pointer(&stack[0]) && jit_is_pointer(&stack[1])
    ) {
        result = stack[1];

    } else if (result.kind == JIT_KIND_BY_REF) {
        // if we had operations with by-ref at this point
        // the turned into a native int
        result.kind = JIT_KIND_NATIVE_INT;
        result.type = tIntPtr;
    }

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

    // jit_stack_value_t* index = &stack[1];
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

    // jit_stack_value_t* index = &stack[1];
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

    if (inst->opcode == CEE_LDVIRTFTN) {
        // TODO: devirt the method if we can, in this case its just
        //       to find the real value and use it directly instead
        //       of going through the page table
        CHECK(!method->Attributes.Static);
    }

    // special case, since its only used to pass it to the newobj, we verify
    // externally that the opcodes are valid
    jit_stack_value_t* value = STACK_PUSH();
    value->kind = JIT_KIND_NATIVE_INT;
    value->method = method;

cleanup:
    return err;
}

// these constants are inspired by openjdk
#define INLINE_SMALL_CODE       1000    /* native function size */
#define MAX_INLINE_SIZE         35      /* il bytecode size */
#define MAX_TRIVIAL_SIZE        6       /* il bytecode size */
#define MAX_INLINE_LEVEL        15      /* the max nesting of inline we allow */

static bool jit_should_inline(jit_function_t* caller, RuntimeMethodBase callee) {
    // can't inline something without a body
    if (callee->MethodBody == NULL) {
        return false;
    }

    // inline is requested
    if (callee->MethodImplFlags.AggressiveInlining) {
        return true;
    }

    // TODO: for now assume a method is cold, we might want some logic
    //       to check for hot methods

    // check if already compiled into a medium method
    if (callee->MethodPtr != NULL && callee->MethodSize > (INLINE_SMALL_CODE / 4)) {
        return false;
    }

    // method too big for inline
    if (callee->MethodBody->ILSize > MAX_INLINE_SIZE) {
        return false;
    }

    return true;
}

static bool jit_should_not_inline(jit_function_t* caller, RuntimeMethodBase callee) {
    // user doesn't want to enable inline globally
    if (!tdn_get_config()->jit_inline) {
        return true;
    }

    // don't inline if marked as no inline
    if (callee->MethodImplFlags.NoInlining) {
        return true;
    }

    // don't allow to inline too much
    if (caller->inline_depth + 1 > MAX_INLINE_LEVEL) {
        return true;
    }

    // don't allow recursion
    if (caller->method == callee) {
        return true;
    }

    // requested inline, ignore the rest of the logic
    if (callee->MethodImplFlags.AggressiveInlining) {
        return false;
    }

    // don't inline big methods if they are already compiled
    if (callee->MethodPtr != NULL && callee->MethodSize > INLINE_SMALL_CODE) {
        return true;
    }

    // small methods should always get inlined
    if (callee->MethodBody->ILSize <= MAX_TRIVIAL_SIZE) {
        return false;
    }

    // TODO: something else?

    return false;
}

static tdn_err_t type_call(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeMethodBase method = inst->operand.method;
    RuntimeTypeInfo method_type = method->Attributes.Static ? NULL : method->DeclaringType;

    RuntimeTypeInfo return_actual_type = NULL;
    bool return_is_actual_type_exact = false;

    // if this is a callvirt attempt to de-virt the call
    RuntimeMethodBase devirt_method = NULL;
    if (inst->opcode == CEE_CALLVIRT) {
        devirt_method = jit_devirt_method(&stack[0], inst->operand.method);
    }

    // check if we wanted to inline before
    int inline_idx = hmgeti(block->inlines, inst->pc);

    // if this has a direct call, or the callvirt devirted the method, then check
    // if we should attempt and inline it
    if (inst->opcode == CEE_CALL || inst->opcode == CEE_NEWOBJ || devirt_method != NULL) {
        // fix it up to not have a null
        if (devirt_method == NULL) {
            devirt_method = method;
        }

        // if the previous method we wanted to inline is not the same as
        // the current one then we need to recreate the inline information
        if (inline_idx >= 0 && devirt_method != block->inlines[inline_idx].value->method) {
            jit_function_destroy(block->inlines[inline_idx].value);
            hmdel(block->inlines, inst->pc);
        }

        // check if we should inline this
        if (jit_should_inline(function, devirt_method) && !jit_should_not_inline(function, devirt_method)) {
            // we want to inline it
            jit_function_t* inlinee_function = NULL;
            if (inline_idx < 0) {
                // this is the first time we go over and want to inline it, create the method properly
                inlinee_function = tdn_host_mallocz(sizeof(jit_function_t), alignof(jit_function_t));
                CHECK(inlinee_function != NULL);
                hmput(block->inlines, inst->pc, inlinee_function);

                // inline the function right away
                CHECK_AND_RETHROW(jit_function_init(inlinee_function, devirt_method));

                // prevent too deep of inline recursion
                inlinee_function->inline_depth = function->inline_depth + 1;
            } else {
                inlinee_function = block->inlines[inline_idx].value;
                CHECK(inlinee_function->inline_depth == function->inline_depth + 1);
            }

            // update the type information, this will allow for a better devirt
            // note that for newobj we need to ignore the `this` since its added
            // implicitly
            int off = inst->opcode == CEE_NEWOBJ ? 1 : 0;
            for (int i = 0; i < arrlen(stack); i++) {
                inlinee_function->entry_block.args[off + i].actual_type = jit_get_actual_type(&stack[i]);
                inlinee_function->entry_block.args[off + i].is_actual_type_exact = stack[i].is_actual_type_exact;
                inlinee_function->entry_block.args[off + i].method = stack[i].method;
            }

            if (inst->opcode == CEE_NEWOBJ) {
                // for newobj the this is an actual exact type, so we can handle it as such
                inlinee_function->entry_block.args[0].actual_type = inst->operand.type;
                inlinee_function->entry_block.args[0].is_actual_type_exact = true;
            }

            // type the function's blocks
            CHECK_AND_RETHROW(jit_function(inlinee_function, NULL));

            // get the actual return type
            if (method->ReturnParameter->ParameterType != tVoid) {
                return_actual_type = inlinee_function->return_actual_type;
                return_is_actual_type_exact = inlinee_function->return_is_actual_type_exact;
            }
        } else {
            // we don't want to inline it, if we wanted before then ensure
            // we no longer do
            if (inline_idx >= 0) {
                jit_function_destroy(block->inlines[inline_idx].value);
                hmdel(block->inlines, inst->pc);
            }
        }
    } else {
        // we don't have anything to inline, ensure that if we did we no
        // longer do
        if (inline_idx >= 0) {
            jit_function_destroy(block->inlines[inline_idx].value);
            hmdel(block->inlines, inst->pc);
        }
    }

    if (inst->opcode == CEE_NEWOBJ) {
        jit_stack_value_t* value = STACK_PUSH();
        jit_stack_value_init(value, method_type);
        value->actual_type = method_type;
        value->is_actual_type_exact = true;

    } else if (method->ReturnParameter->ParameterType != tVoid) {
        jit_stack_value_t* return_value = STACK_PUSH();
        jit_stack_value_init(return_value, method->ReturnParameter->ParameterType);
        return_value->actual_type = return_actual_type;
        return_value->is_actual_type_exact = return_is_actual_type_exact;
    }

cleanup:
    return err;
}

static tdn_err_t type_ret(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    // if we are inside of an inline set the return type so the caller can get better information
    // about the inline that just happened
    if (function->inline_depth != 0) {
        // if we are returning an obj-ref, then get the actual on-stack type of the object
        // so we can return a more accurate object
        if (function->method->ReturnParameter->ParameterType != tVoid) {
            RuntimeTypeInfo return_type = jit_get_actual_type(stack);

            // merge it with the other known return types
            if (function->return_actual_type == NULL) {
                function->return_actual_type = return_type;
                function->return_is_actual_type_exact = stack->is_actual_type_exact;
            }

            if (function->return_actual_type != return_type) {
                function->return_is_actual_type_exact = false;
                function->return_actual_type = type_merge_actual_type(function->return_actual_type, return_type);
            }

            if (function->return_is_actual_type_exact != stack->is_actual_type_exact) {
                function->return_is_actual_type_exact = false;
            }
        }
    }
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

    // if we take the actual type, and we are performing an upcast to another type, we want
    // to keep the actual type exactly as it is, otherwise it means we have made a downcast
    // and we don't actually want to keep it
    RuntimeTypeInfo actual_type = jit_get_actual_type(stack);
    if (actual_type != NULL && verifier_can_cast_to(actual_type, inst->operand.type)) {
        pushed->actual_type = actual_type;
        pushed->is_actual_type_exact = stack->is_actual_type_exact;
    }

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
        if (target_clause == clause || clause == NULL) {
            break;
        }

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
    [CEE_LDIND_R4] = type_ldind,
    [CEE_LDIND_R8] = type_ldind,
    [CEE_LDOBJ] = type_ldind,

    [CEE_STIND_I] = type_stind,
    [CEE_STIND_I1] = type_stind,
    [CEE_STIND_I2] = type_stind,
    [CEE_STIND_I4] = type_stind,
    [CEE_STIND_I8] = type_stind,
    [CEE_STIND_REF] = type_stind,
    [CEE_STIND_R4] = type_stind,
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
