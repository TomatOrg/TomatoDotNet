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
static tdn_err_t verify_nop(jit_function_t* verifier, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Local access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_ldloc(jit_function_t* verifier, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->locals));
    jit_block_local_t* local = &block->locals[inst->operand.variable];

    // check if we need to zero initialize
    if (!local->initialized) {
        // TODO: should we fail if the method is not marked as InitLocals?
        verifier->locals[inst->operand.variable].zero_initialize = true;
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

static tdn_err_t verify_stloc(jit_function_t* verifier, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->locals));
    jit_block_local_t* local = &block->locals[inst->operand.variable];

    // check that the type matches
    CHECK(verifier_assignable_to(stack[0].type, verifier->locals[inst->operand.variable].type),
        "%T verifier-assignable-to %T", stack[0].type, verifier->locals[inst->operand.variable].type);

    // copy as is
    local->stack = stack[0];
    local->initialized = true;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Stack manipulation
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_ldc_i4(jit_function_t* verifier, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    STACK_PUSH()->type = tInt32;
    return TDN_NO_ERROR;
}

static tdn_err_t verify_ldc_i8(jit_function_t* verifier, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    STACK_PUSH()->type = tInt64;
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// Method related
//----------------------------------------------------------------------------------------------------------------------

// Use as a template for adding new instructions
static tdn_err_t verify_ret(jit_function_t* verifier, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    ParameterInfo ret = verifier->method->ReturnParameter;

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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Dispatch tables
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

verify_instruction_t g_verify_dispatch_table[] = {
    [CEE_NOP] = verify_nop,
    [CEE_LDLOC] = verify_ldloc,
    [CEE_STLOC] = verify_stloc,

    [CEE_LDC_I4] = verify_ldc_i4,
    [CEE_LDC_I8] = verify_ldc_i8,

    [CEE_RET] = verify_ret,

    [CEE_BR] = verify_br,
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
