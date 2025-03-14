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

    MERGE_BOOL(is_exact_type);
    MERGE_BOOL(readonly_ref);
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

    if (target->visited) {
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
    jit_block_local_t* local = &block->args[inst->operand.variable];

    // fixup the type to be the intermediate type,
    // and push it to the stack
    jit_stack_item_t item = local->stack;
    item.type = verifier_get_intermediate_type(item.type);
    *STACK_PUSH() = item;

cleanup:
    return err;
}

static tdn_err_t verify_starg(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->args));
    jit_block_local_t* local = &block->args[inst->operand.variable];

    // check that the type matches
    CHECK(verifier_assignable_to(stack[0].type, function->args[inst->operand.variable].type),
        "%T verifier-assignable-to %T", stack[0].type, function->args[inst->operand.variable].type);

    // copy as is
    local->stack = stack[0];
    local->initialized = true;

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
    local->stack = stack[0];
    local->initialized = true;

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

//----------------------------------------------------------------------------------------------------------------------
// Arith and compare operations
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_binary_compare(tdn_il_opcode_t opcode, RuntimeTypeInfo type1, RuntimeTypeInfo type2) {
    tdn_err_t err = TDN_NO_ERROR;

    if (type1 == tInt32) {
        CHECK(type2 == tInt32);

    } else if (type1 == tInt64) {
        CHECK(type2 == tInt64);

    } else if (type1 == tIntPtr) {
        CHECK(type2 == tInt32);

    } else if (tdn_type_is_referencetype(type1)) {
        CHECK(tdn_type_is_referencetype(type2));
        CHECK(opcode == CEE_CEQ || opcode == CEE_CGT_UN);

    } else {
        CHECK_FAIL("%T binary-compare %T", type1, type2);
    }

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
// Method related
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_method_accessible(RuntimeMethodBase caller, RuntimeMethodBase callee) {
    tdn_err_t err = TDN_NO_ERROR;



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
                if (stack[0].type->ElementType->IsByRefStruct) {
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
    item->type = ret_type;

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

    // ensure we can access the callee
    CHECK_AND_RETHROW(verify_method_accessible(function->method, inst->operand.method));

    // ensure this is a ctor
    CHECK(inst->operand.method->Attributes.RTSpecialName);
    CHECK(tdn_compare_string_to_cstr(inst->operand.method->Name, ".ctor"));

    // verify the call parameters
    CHECK_AND_RETHROW(verify_call_params(inst->operand.method, stack, NULL));

    // TODO: push the new instance
    CHECK_FAIL();

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
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Branching
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_br(jit_function_t* verifier, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the target block
    jit_basic_block_entry_t* target = hmgetp_null(verifier->labels, inst->operand.branch_target);
    CHECK(target != NULL);

    // check that we can merge with it
    CHECK_AND_RETHROW(verifier_merge_block(verifier, block, &verifier->blocks[target->value.index]));

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Exceptions
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_throw(jit_function_t* verifier, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
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

    [CEE_LDFLD] = verify_ldfld,

    [CEE_LDNULL] = verify_ldnull,
    [CEE_LDC_I4] = verify_ldc_i4,
    [CEE_LDC_I8] = verify_ldc_i8,

    [CEE_CEQ] = verify_compare,
    [CEE_CGT] = verify_compare,
    [CEE_CGT_UN] = verify_compare,
    [CEE_CLT] = verify_compare,
    [CEE_CLT_UN] = verify_compare,

    [CEE_NEWOBJ] = verify_newobj,
    [CEE_CALL] = verify_call,
    [CEE_CALLVIRT] = verify_callvirt,

    [CEE_RET] = verify_ret,

    [CEE_BR] = verify_br,

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
