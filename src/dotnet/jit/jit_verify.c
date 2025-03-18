#include "jit_verify.h"

#include <dotnet/types.h>
#include <tomatodotnet/types/type.h>
#include <util/string.h>
#include <util/except.h>
#include <util/stb_ds.h>

#include "jit_type.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define MERGE_BOOL(name) \
    do { \
        if (previous->name && !new->name) { \
            previous->name = false; \
            modified = true; \
        } \
    } while (0)

static void verifier_queue_block(jit_function_t* function, jit_block_t* block) {
    if (!block->in_queue) {
        arrpush(function->queue, block);
        block->visited = true;
        block->in_queue = true;
    }
}

static RuntimeTypeInfo verifier_merge_type(RuntimeTypeInfo S, RuntimeTypeInfo T) {
    // TODO: find a common base
    if (verifier_assignable_to(S, T)) {
        return S;
    } else if (verifier_assignable_to(T, S)) {
        return T;
    } else {
        return NULL;
    }
}

static bool verifier_merge_stack(jit_stack_item_t* previous, jit_stack_item_t* new) {
    bool modified = false;

    RuntimeTypeInfo U = verifier_merge_type(previous->type, new->type);
    if (U != previous->type) {
        previous->type = U;
        modified = true;
    }

    U = verifier_merge_type(previous->boxed_type, new->boxed_type);
    if (U != previous->boxed_type) {
        previous->boxed_type = U;
        modified = true;
    }

    // if something was not readonly the last round, but is readonly now,
    // we need to keep the readonly and verify again
    if (!previous->readonly_ref && new->readonly_ref) {
        previous->readonly_ref = true;
        modified = true;
    }

    // these get set to false once something that does not
    // match them is merged in
    MERGE_BOOL(is_exact_type);
    MERGE_BOOL(is_this);
    MERGE_BOOL(non_local_ref);
    MERGE_BOOL(non_local_ref_struct);

    return modified;
}

static bool verifier_merge_block_local(jit_block_local_t* previous, jit_block_local_t* new) {
    bool modified = false;

    if (verifier_merge_stack(&previous->stack, &new->stack)) {
        modified = true;
    }

    MERGE_BOOL(initialized);

    return modified;
}

static tdn_err_t verifier_merge_block(jit_function_t* function, jit_block_t* from, jit_block_t* target) {
    tdn_err_t err = TDN_NO_ERROR;

    // we check against multiple_predecessors as well for the case of the second pass
    // so we will know we need to merge nicely
    if (target->visited || target->multiple_predecessors) {
        // already visited once, need to merge with the
        // type instead of setting everything as is
        target->multiple_predecessors = true;

        bool modified = false;

        for (int i = 0; i < arrlen(from->args); i++) {
            if (verifier_merge_block_local(&target->args[i], &from->args[i])) {
                modified = true;
            }
            CHECK(target->args[i].stack.type != NULL);
        }

        for (int i = 0; i < arrlen(from->locals); i++) {
            if (verifier_merge_block_local(&target->locals[i], &from->locals[i])) {
                modified = true;
            }
            CHECK(target->locals[i].stack.type != NULL);
        }

        for (int i = 0; i < arrlen(from->stack); i++) {
            if (verifier_merge_stack(&target->stack[i], &from->stack[i])) {
                modified = true;
            }
            CHECK(target->stack[i].type != NULL);
        }

        // the metadata of the block was modified, we must
        if (modified) {
            CHECK(!function->emitting); // TODO: fix when we get here
            verifier_queue_block(function, target);
        } else if (!target->visited) {
            verifier_queue_block(function, target);
        }

    } else {
        // first time being visited, copy over all the type information as is

        arrsetlen(target->stack, arrlen(from->stack));
        memcpy(target->stack, from->stack, arrlen(from->stack) * sizeof(*from->stack));

        arrsetlen(target->args, arrlen(from->args));
        memcpy(target->args, from->args, arrlen(from->args) * sizeof(*from->args));

        arrsetlen(target->locals, arrlen(from->locals));
        memcpy(target->locals, from->locals, arrlen(from->locals) * sizeof(*from->locals));

        verifier_queue_block(function, target);
    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Verifiers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define STACK_PUSH() \
    ({ \
        jit_stack_item_t* __item = arraddnptr(block->stack, 1); \
        memset(__item, 0, sizeof(*__item)); \
        __item; \
    })

//----------------------------------------------------------------------------------------------------------------------
// Misc instructions
//----------------------------------------------------------------------------------------------------------------------

// Use as a template for adding new instructions
static tdn_err_t verify_nop(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Local access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_ldarg(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->args));
    jit_block_local_t* arg = &block->args[inst->operand.variable];

    // fixup the type to be the intermediate type,
    // and push it to the stack
    jit_stack_item_t item = arg->stack;
    item.type = verifier_get_intermediate_type(item.type);
    *STACK_PUSH() = item;

cleanup:
    return err;
}

static tdn_err_t verify_starg(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->args));
    jit_block_local_t* arg = &block->args[inst->operand.variable];

    // check that the type matches
    CHECK(verifier_assignable_to(stack[0].type, function->args[inst->operand.variable].type),
        "%T verifier-assignable-to %T", stack[0].type, function->args[inst->operand.variable].type);

    // copy as is
    arg->stack.flags = stack[0].flags;
    arg->stack.type = stack[0].type;
    arg->stack.boxed_type = stack[0].boxed_type;
    arg->initialized = true;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Local access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_ldloc(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->locals));
    jit_block_local_t* local = &block->locals[inst->operand.variable];

    // check if we need to zero initialize
    if (!local->initialized) {
        // TODO: should we fail if the method is not marked as InitLocals?
        function->locals[inst->operand.variable].zero_initialize = true;
        local->initialized = true;
    }

    // fixup the type to be the intermediate type,
    // and push it to the stack
    jit_stack_item_t item = local->stack;
    item.type = verifier_get_intermediate_type(item.type);
    *STACK_PUSH() = item;

cleanup:
    return err;
}

static tdn_err_t verify_stloc(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->locals));
    jit_block_local_t* local = &block->locals[inst->operand.variable];

    // check that the type matches
    CHECK(verifier_assignable_to(stack[0].type, function->locals[inst->operand.variable].type),
        "%T verifier-assignable-to %T", stack[0].type, function->locals[inst->operand.variable].type);

    // copy as is
    local->stack.flags = stack[0].flags;
    local->stack.type = stack[0].type;
    local->stack.boxed_type = stack[0].boxed_type;
    local->initialized = true;

cleanup:
    return err;
}

static tdn_err_t verify_ldloca(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->locals));
    jit_block_local_t* local = &block->locals[inst->operand.variable];

    // check if we need to zero initialize
    if (!local->initialized) {
        // TODO: should we fail if the method is not marked as InitLocals?
        function->locals[inst->operand.variable].zero_initialize = true;
        local->initialized = true;
    }

    // mark as spilled
    function->locals[inst->operand.variable].spilled = true;

    // fixup the type to be the intermediate type,
    // and push it to the stack
    jit_stack_item_t item = {};

    // this had a non-local ref-struct, mark the item as such
    if (local->stack.non_local_ref_struct) {
        item.non_local_ref_struct = true;
    }

    item.type = verifier_get_verification_type(local->stack.type);
    CHECK_AND_RETHROW(tdn_get_byref_type(item.type, &item.type));
    *STACK_PUSH() = item;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Misc instructions
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_field_access(jit_function_t* function, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // check the type is valid
    CHECK(
        tdn_type_is_referencetype(stack[0].type) ||
        stack[0].type->IsByRef ||
        (function->method->Module->Assembly->AllowUnsafe && stack[0].type->IsPointer)
    );

    // check that the type has the field
    RuntimeTypeInfo owner = stack[0].type;
    if (owner != NULL) {
        bool found = false;
        if (owner->IsByRef || owner->IsPointer) owner = owner->ElementType;
        for (; owner != NULL; owner = owner->BaseType) {
            if (owner == inst->operand.field->DeclaringType) {
                found = true;
                break;
            }
        }
        CHECK(found);
    }

    // TODO: check the field is accessible

cleanup:
    return err;
}

// Use as a template for adding new instructions
static tdn_err_t verify_ldfld(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // verify we can access the field
    CHECK_AND_RETHROW(verify_field_access(function, inst, stack));

    // push the type into the stack
    jit_stack_item_t* item = STACK_PUSH();
    item->type = verifier_get_intermediate_type(inst->operand.field->FieldType);

    // loaded a by-ref, check if its non-local
    // TODO: how does readonly plays in here?
    if (item->type->IsByRef) {
        if (stack[0].non_local_ref_struct) {
            item->non_local_ref = true;
        }
    } else if (item->type->IsByRefStruct) {
        if (stack[0].non_local_ref_struct) {
            item->non_local_ref_struct = true;
        }
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Reference access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_ldind(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_item_t* addr = &stack[0];

    // must be a byref
    CHECK(addr->type != NULL && addr->type->IsByRef);

    // ensure the type is consistent
    RuntimeTypeInfo type;
    if (inst->operand.type != NULL) {
        CHECK(verifier_assignable_to(addr->type->ElementType, inst->operand.type),
            "%T verifier-assignable-to %T", addr->type->ElementType, inst->operand.type);

        type = verifier_get_intermediate_type(inst->operand.type);

    } else {
        CHECK(tdn_type_is_referencetype(inst->operand.type));
        type = verifier_get_verification_type(addr->type->ElementType);
    }

    STACK_PUSH()->type = type;

cleanup:
    return err;
}

static tdn_err_t verify_stind(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_item_t* addr = &stack[0];
    jit_stack_item_t* val = &stack[1];

    // must be a byref
    CHECK(addr->type != NULL && addr->type->IsByRef);

    // ensure we can assign the value to the indirect reference
    CHECK(verifier_assignable_to(val->type, addr->type->ElementType),
        "%T verifier-assignable-to %T", val->type, addr->type->ElementType);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Stack manipulation
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_ldnull(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    STACK_PUSH()->type = NULL;
    return TDN_NO_ERROR;
}

static tdn_err_t verify_ldc_i4(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    STACK_PUSH()->type = tInt32;
    return TDN_NO_ERROR;
}

static tdn_err_t verify_ldc_i8(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    STACK_PUSH()->type = tInt64;
    return TDN_NO_ERROR;
}

static tdn_err_t verify_dup(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    *STACK_PUSH() = stack[0];
    *STACK_PUSH() = stack[0];
    return TDN_NO_ERROR;
}

static tdn_err_t verify_pop(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// Arith and compare operations
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_binary_compare(tdn_il_opcode_t opcode, RuntimeTypeInfo type1, RuntimeTypeInfo type2) {
    tdn_err_t err = TDN_NO_ERROR;

    if (type1 == tInt32) {
        CHECK(type2 == tInt32 || type2 == tIntPtr);

    } else if (type1 == tInt64) {
        CHECK(type2 == tInt64);

    } else if (type1 == tIntPtr) {
        CHECK(type2 == tIntPtr || type2 == tInt32);

    } else if (tdn_type_is_referencetype(type1)) {
        CHECK(tdn_type_is_referencetype(type2));
        CHECK(opcode == CEE_CEQ || opcode == CEE_CGT_UN || opcode == CEE_BEQ || opcode == CEE_BNE_UN);

    } else {
        CHECK_FAIL("%T binary-compare %T", type1, type2);
    }

cleanup:
    return err;
}

static tdn_err_t verify_binary_op(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // validate that both are good
    RuntimeTypeInfo type1 = stack[0].type;
    RuntimeTypeInfo type2 = stack[1].type;

    RuntimeTypeInfo result_type = NULL;
    if (type1 == tInt32) {
        CHECK(type2 == tInt32);
        result_type = tInt32;

    } else if (type1 == tInt64) {
        CHECK(type2 == tInt64);
        result_type = tInt64;

    } else if (type1 == tIntPtr) {
        CHECK(type2 == tIntPtr);
        result_type = tIntPtr;

    } else {
        CHECK_FAIL();
    }

    // always pushes as an int32
    STACK_PUSH()->type = result_type;

cleanup:
    return err;
}

static tdn_err_t verify_conv(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // validate that both are good
    CHECK(
        stack[0].type == tInt32 ||
        stack[0].type == tInt64 ||
        stack[0].type == tIntPtr
    );

    // always pushes as an int32
    RuntimeTypeInfo type = NULL;
    switch (inst->opcode) {
        case CEE_CONV_I1:
        case CEE_CONV_I2:
        case CEE_CONV_I4:
        case CEE_CONV_U1:
        case CEE_CONV_U2:
        case CEE_CONV_U4: type = tInt32; break;
        case CEE_CONV_I8:
        case CEE_CONV_U8: type = tInt64; break;
        case CEE_CONV_I:
        case CEE_CONV_U: type = tIntPtr; break;
        default: CHECK_FAIL();
    }
    STACK_PUSH()->type = type;

cleanup:
    return err;
}

static tdn_err_t verify_compare(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // validate that both are good
    RuntimeTypeInfo value1 = stack[0].type;
    RuntimeTypeInfo value2 = stack[1].type;
    CHECK_AND_RETHROW(verify_binary_compare(inst->opcode, value1, value2));

    // always pushes as an int32
    STACK_PUSH()->type = tInt32;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Array related
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_newarr(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // validate the size
    CHECK(stack[0].type == tInt32 || stack[0].type == tIntPtr);

    // validate the item can be turned into an array
    CHECK(!inst->operand.type->IsByRef);
    CHECK(!inst->operand.type->IsByRefStruct);

    // push the array type
    jit_stack_item_t* item = STACK_PUSH();
    CHECK_AND_RETHROW(tdn_get_array_type(inst->operand.type, &item->type));

cleanup:
    return err;
}

static tdn_err_t verify_ldlen(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure this is an array
    CHECK(stack[0].type == NULL || stack[0].type->IsArray);

    // return value is an intptr
    STACK_PUSH()->type = tIntPtr;

cleanup:
    return err;
}

static tdn_err_t verify_ldelem(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_item_t* array = &stack[0];
    jit_stack_item_t* index = &stack[1];

    CHECK(index->type == tInt32 || index->type == tIntPtr);

    // if the array is null everything is assumed to be correct,
    // and it will fail at runtime
    if (array->type == NULL) {
        STACK_PUSH()->type = inst->operand.type;
        goto cleanup;
    }

    // must be an array type
    CHECK(array->type->IsArray);

    // ensure the types match nicely
    CHECK(verifier_array_element_compatible_with(inst->operand.type, array->type->ElementType),
        "%T array-element-compatible-with %T", inst->operand.type, array->type->ElementType);

    // track the result
    STACK_PUSH()->type = verifier_get_intermediate_type(array->type->ElementType);

cleanup:
    return err;
}

static tdn_err_t verify_stelem(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_item_t* array = &stack[0];
    jit_stack_item_t* index = &stack[1];
    jit_stack_item_t* value = &stack[2];

    CHECK(index->type == tInt32 || index->type == tIntPtr);

    // if the array is null everything is assumed to be correct,
    // and it will fail at runtime
    if (array->type == NULL) {
        goto cleanup;
    }

    // must be an array type
    CHECK(array->type->IsArray);

    // ensure the types match nicely
    CHECK(verifier_array_element_compatible_with(value->type, inst->operand.type),
        "%T array-element-compatible-with %T", value->type, inst->operand.type);
    CHECK(verifier_array_element_compatible_with(inst->operand.type, array->type->ElementType),
        "%T array-element-compatible-with %T", inst->operand.type, array->type->ElementType);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Method related
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_method_accessible(RuntimeMethodBase caller, RuntimeMethodBase callee) {
    tdn_err_t err = TDN_NO_ERROR;



cleanup:
    return err;
}

static tdn_err_t verify_ldftn(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // check we can access the method at all
    CHECK_AND_RETHROW(verify_method_accessible(function->method, inst->operand.method));

    // and pushhh it
    jit_stack_item_t* item = STACK_PUSH();
    item->is_method = true;
    item->method = inst->operand.method;

cleanup:
    return err;
}

static tdn_err_t verify_call_params(RuntimeMethodBase callee, jit_stack_item_t* stack, bool* might_leak_local) {
    tdn_err_t err = TDN_NO_ERROR;

    // if this is an instance method that is not ctor
    int arg_offset = 0;
    if (
        !callee->Attributes.Static &&
        !callee->Attributes.RTSpecialName
    ) {
        CHECK(arrlen(stack) >= 1);
        CHECK(verifier_assignable_to(stack[0].type, jit_get_method_this_type(callee)),
            "%T verifier-assignable-to %T", stack[0].type, jit_get_method_this_type(callee));

        // TODO: unscoped support

        // check if we must not pass a readonly reference
        if (!callee->IsReadOnly) {
            CHECK(!stack[0].readonly_ref);
        }

        // we had a this, skip it when verifying the rest of the parameters
        arg_offset++;
    }

    // verify the arguments
    CHECK(arrlen(stack) - arg_offset == callee->Parameters->Length);
    for (int i = arg_offset; i < arrlen(stack); i++) {
        ParameterInfo param = callee->Parameters->Elements[i - arg_offset];
        CHECK(verifier_assignable_to(stack[i].type, param->ParameterType),
            "%T verifier-assignable-to %T", stack[i].type, param->ParameterType);

        // TODO: scoped support

        // check if we must not pass a readonly reference
        if (!param->IsReadOnly) {
            CHECK(!stack[i].readonly_ref);
        }

        // check if we might leak a local reference (to prevent marking the returned reference as
        // a non-local one)
        if (might_leak_local != NULL && stack[i].type != NULL) {
            if (stack[i].type->IsByRef) {
                if (!stack[i].non_local_ref) {
                    *might_leak_local = true;
                }
                if (stack[i].type->ElementType->IsByRefStruct) {
                    if (!stack[0].non_local_ref_struct) {
                        *might_leak_local = true;
                    }
                }
            } else if (stack[i].type->IsByRefStruct && stack[i].non_local_ref_struct) {
                *might_leak_local = true;
            }
        }
    }

cleanup:
    return err;
}

static void verify_call_return(jit_block_t* block, RuntimeMethodBase callee, bool might_leak_local) {
    RuntimeTypeInfo ret_type = callee->ReturnParameter->ParameterType;
    if (ret_type == tVoid) {
        return;
    }

    // we have something to return
    jit_stack_item_t* item = STACK_PUSH();
    item->type = verifier_get_intermediate_type(ret_type);

    // mark as non-local if we are returning a reference
    if (!might_leak_local) {
        if (ret_type->IsByRef) {
            item->non_local_ref = true;
            if (ret_type->ElementType->IsByRefStruct) {
                item->non_local_ref_struct = true;
            }
        } else if (ret_type->IsByRefStruct) {
            item->non_local_ref_struct = true;
        }
    }

    // mark as readonly if returns a readonly
    if (callee->ReturnParameter->IsReadOnly) {
        item->readonly_ref = true;
    }
}

static tdn_err_t verify_call(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure we can access the callee
    CHECK_AND_RETHROW(verify_method_accessible(function->method, inst->operand.method));

    // ensure we have a method body (otherwise must use callvirt)
    CHECK(inst->operand.method->MethodBody != NULL);

    // TODO: other checks for call

    // verify the call arguments
    bool might_leak_local = false;
    CHECK_AND_RETHROW(verify_call_params(inst->operand.method, stack, &might_leak_local));
    verify_call_return(block, inst->operand.method, might_leak_local);

cleanup:
    return err;
}

static tdn_err_t verify_callvirt(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure we can access the callee
    CHECK_AND_RETHROW(verify_method_accessible(function->method, inst->operand.method));

    // ensure the method is not static
    CHECK(!inst->operand.method->Attributes.Static);

    // TODO: other checks for callvirt?

    // verify the call arguments
    bool might_leak_local = false;
    CHECK_AND_RETHROW(verify_call_params(inst->operand.method, stack, &might_leak_local));
    verify_call_return(block, inst->operand.method, might_leak_local);

cleanup:
    return err;
}

static tdn_err_t verify_newobj(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase callee = inst->operand.method;

    // ensure we can access the callee
    CHECK_AND_RETHROW(verify_method_accessible(function->method, callee));

    // ensure this is a ctor
    CHECK(callee->Attributes.RTSpecialName);
    CHECK(tdn_compare_string_to_cstr(callee->Name, ".ctor"));

    // if we are constructing a delegate, this is a special case
    if (jit_is_delegate(callee->DeclaringType)) {
        CHECK(stack[1].is_method);
        RuntimeMethodBase target = stack[1].method;

        // ensure the instance matches the delegate, for static
        // target the instance must be a null
        if (target->Attributes.Static) {
            CHECK(stack[0].type == NULL);
        } else {
            // TODO: how do value types work in here?
            CHECK(verifier_assignable_to(stack[0].type, target->DeclaringType),
                "%T verifier-assignable-to %T", stack[0].type, target->DeclaringType);
        }

        // ensure the delegate matches the wanted function signature
        CHECK(callee->DeclaringType->DeclaredMethods->Length == 1);
        RuntimeMethodBase signature = (RuntimeMethodBase)callee->DeclaringType->DeclaredMethods->Elements[0];
        CHECK(signature->Parameters->Length == target->Parameters->Length);
        for (int i = 0; i < signature->Parameters->Length; i++) {
            ParameterInfo signature_param = signature->Parameters->Elements[i];
            ParameterInfo target_param = target->Parameters->Elements[i];
            CHECK(signature_param->ParameterType == target_param->ParameterType);
            CHECK(signature_param->Attributes.Attributes == target_param->Attributes.Attributes);
            CHECK(signature_param->IsReadOnly == target_param->IsReadOnly);
        }

        // ensure the return type is the same
        CHECK(signature->ReturnParameter->ParameterType == target->ReturnParameter->ParameterType);
        CHECK(signature->ReturnParameter->Attributes.Attributes == target->ReturnParameter->Attributes.Attributes);
        CHECK(signature->ReturnParameter->IsReadOnly == target->ReturnParameter->IsReadOnly);
    } else {
        // verify the call parameters
        CHECK_AND_RETHROW(verify_call_params(inst->operand.method, stack, NULL));
    }

    // push the new instance
    jit_stack_item_t* item = STACK_PUSH();
    item->type = jit_get_method_this_type(callee);
    item->is_exact_type = true;

    // we built a delegate, mark its function
    if (jit_is_delegate(callee->DeclaringType)) {
        item->method = stack[1].method;
    }

    // TODO: mark ref-struct as non-local if only had non-local arguments

cleanup:
    return err;
}

// Use as a template for adding new instructions
static tdn_err_t verify_ret(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    ParameterInfo ret = function->method->ReturnParameter;

    // the stack must be empty at this point
    CHECK(arrlen(block->stack) == 0);

    RuntimeTypeInfo ret_type = ret->ParameterType;
    if (ret_type != tVoid) {
        // ensure the return type is valid
        CHECK(verifier_assignable_to(stack[0].type, ret_type),
            "%T verifier-assignable-to %T", stack[0].type, ret_type);

        // readonly consistency
        if (!ret->IsReadOnly) {
            CHECK(!stack[0].readonly_ref);
        }

        // consistency of refs
        if (ret_type->IsByRef) {
            CHECK(stack[0].non_local_ref);
            if (ret_type->ElementType->IsByRefStruct) {
                CHECK(stack[0].non_local_ref_struct);
            }
        } else if (ret_type->IsByRefStruct) {
            CHECK(stack[0].non_local_ref_struct);
        }

        // merge the return item type, used mainly for inline
        if (function->return_item_initialized) {
            verifier_merge_stack(&function->return_item, &stack[0]);
        } else {
            function->return_item = stack[0];
            function->return_item_initialized = true;
        }
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Branching
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_br(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);

    // check that we can merge with it
    CHECK_AND_RETHROW(verifier_merge_block(function, block, target));

cleanup:
    return err;
}

static tdn_err_t verify_br_unary_cond(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // check its a valid type
    CHECK(
        stack[0].type == tInt32 ||
        stack[0].type == tInt64 ||
        stack[0].type == NULL ||
        stack[0].type->IsByRef ||
        stack[0].type->IsPointer ||
        tdn_type_is_referencetype(stack[0].type)
    );

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);
    jit_block_t* next = jit_function_get_block(function, block->end, block->leave_target_stack);
    CHECK(next != NULL);

    // check that we can merge with the next block as well
    CHECK_AND_RETHROW(verifier_merge_block(function, block, target));

    // check that we can merge with it
    CHECK_AND_RETHROW(verifier_merge_block(function, block, next));

cleanup:
    return err;
}

static tdn_err_t verify_br_binary_cond(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // validate that both are good
    RuntimeTypeInfo value1 = stack[0].type;
    RuntimeTypeInfo value2 = stack[1].type;
    CHECK_AND_RETHROW(verify_binary_compare(inst->opcode, value1, value2));

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);
    jit_block_t* next = jit_function_get_block(function, block->end, block->leave_target_stack);
    CHECK(next != NULL);

    // check that we can merge with the next block as well
    CHECK_AND_RETHROW(verifier_merge_block(function, block, target));

    // check that we can merge with it
    CHECK_AND_RETHROW(verifier_merge_block(function, block, next));

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Exceptions
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_leave(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // empty the stack
    arrsetlen(block->stack, 0);

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

    // and now merge with the target
    CHECK_AND_RETHROW(verifier_merge_block(function, block, target));

    // override the target pc with the actual target we want to go to
    inst->operand.branch_target = target_pc;

cleanup:
    return err;
}

static tdn_err_t verify_endfinally(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // empty the stack
    arrsetlen(block->stack, 0);

    // ensure we have a valid leave target stack
    CHECK(block->leave_target_stack != NULL);

    // get the target we need to jump to
    uint32_t target_pc = arrpop(block->leave_target_stack);
    jit_block_t* target = jit_function_get_block(function, target_pc, block->leave_target_stack);
    CHECK(target != NULL);

    // and now merge with the target
    CHECK_AND_RETHROW(verifier_merge_block(function, block, target));

    // override the target pc in the instruction
    // for the emitter to know where to jump into
    inst->operand.branch_target = target_pc;

cleanup:
    return err;
}

static tdn_err_t verify_throw(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: check throwing an exception and nothing else
    CHECK(tdn_type_is_referencetype(stack[0].type));

    // empty the stack
    arrsetlen(block->stack, 0);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Dispatch tables
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

verify_instruction_t g_verify_dispatch_table[] = {
    [CEE_NOP] = verify_nop,

    [CEE_LDARG] = verify_ldarg,
    [CEE_STARG] = verify_starg,

    [CEE_LDLOC] = verify_ldloc,
    [CEE_STLOC] = verify_stloc,
    [CEE_LDLOCA] = verify_ldloca,

    [CEE_LDFLD] = verify_ldfld,

    [CEE_LDIND_I1] = verify_ldind,
    [CEE_LDIND_U1] = verify_ldind,
    [CEE_LDIND_I2] = verify_ldind,
    [CEE_LDIND_U2] = verify_ldind,
    [CEE_LDIND_I4] = verify_ldind,
    [CEE_LDIND_U4] = verify_ldind,
    [CEE_LDIND_I8] = verify_ldind,
    [CEE_LDIND_I] = verify_ldind,
    [CEE_LDIND_REF] = verify_ldind,
    [CEE_LDOBJ] = verify_ldind,

    [CEE_STIND_I] = verify_stind,
    [CEE_STIND_I1] = verify_stind,
    [CEE_STIND_I2] = verify_stind,
    [CEE_STIND_I4] = verify_stind,
    [CEE_STIND_REF] = verify_stind,
    [CEE_STOBJ] = verify_stind,

    [CEE_LDNULL] = verify_ldnull,
    [CEE_LDC_I4] = verify_ldc_i4,
    [CEE_LDC_I8] = verify_ldc_i8,
    [CEE_DUP] = verify_dup,
    [CEE_POP] = verify_pop,

    [CEE_ADD] = verify_binary_op,
    [CEE_SUB] = verify_binary_op,
    [CEE_MUL] = verify_binary_op,
    [CEE_DIV] = verify_binary_op,
    [CEE_DIV_UN] = verify_binary_op,
    [CEE_REM] = verify_binary_op,
    [CEE_REM_UN] = verify_binary_op,
    [CEE_AND] = verify_binary_op,
    [CEE_OR] = verify_binary_op,
    [CEE_XOR] = verify_binary_op,

    [CEE_CONV_I1] = verify_conv,
    [CEE_CONV_I2] = verify_conv,
    [CEE_CONV_I4] = verify_conv,
    [CEE_CONV_I8] = verify_conv,
    [CEE_CONV_U1] = verify_conv,
    [CEE_CONV_U2] = verify_conv,
    [CEE_CONV_U4] = verify_conv,
    [CEE_CONV_U8] = verify_conv,
    [CEE_CONV_I] = verify_conv,
    [CEE_CONV_U] = verify_conv,

    [CEE_CEQ] = verify_compare,
    [CEE_CGT] = verify_compare,
    [CEE_CGT_UN] = verify_compare,
    [CEE_CLT] = verify_compare,
    [CEE_CLT_UN] = verify_compare,

    [CEE_NEWARR] = verify_newarr,
    [CEE_LDLEN] = verify_ldlen,
    [CEE_LDELEM] = verify_ldelem,
    [CEE_STELEM] = verify_stelem,

    [CEE_LDFTN] = verify_ldftn,

    [CEE_NEWOBJ] = verify_newobj,
    [CEE_CALL] = verify_call,
    [CEE_CALLVIRT] = verify_callvirt,

    [CEE_RET] = verify_ret,

    [CEE_BR] = verify_br,
    [CEE_BRFALSE] = verify_br_unary_cond,
    [CEE_BRTRUE] = verify_br_unary_cond,
    [CEE_BEQ] = verify_br_binary_cond,
    [CEE_BGE] = verify_br_binary_cond,
    [CEE_BGT] = verify_br_binary_cond,
    [CEE_BLE] = verify_br_binary_cond,
    [CEE_BLT] = verify_br_binary_cond,
    [CEE_BNE_UN] = verify_br_binary_cond,
    [CEE_BGE_UN] = verify_br_binary_cond,
    [CEE_BGT_UN] = verify_br_binary_cond,
    [CEE_BLE_UN] = verify_br_binary_cond,
    [CEE_BLT_UN] = verify_br_binary_cond,

    [CEE_LEAVE] = verify_leave,
    [CEE_ENDFINALLY] = verify_endfinally,
    [CEE_THROW] = verify_throw,
};
size_t g_verify_dispatch_table_size = ARRAY_LENGTH(g_verify_dispatch_table);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Entry points
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t verifier_on_block_fallthrough(jit_function_t* function, jit_block_t* from, jit_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_AND_RETHROW(verifier_merge_block(function, from, block));

cleanup:
    return err;
}
