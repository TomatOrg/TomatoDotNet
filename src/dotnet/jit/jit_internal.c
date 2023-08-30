#include "jit_internal.h"
#include "util/except.h"
#include "tinydotnet/types/type.h"

#include "spidir/spidir_debug.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Evaluation stack
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t eval_stack_push(eval_stack_t* stack, RuntimeTypeInfo type, spidir_value_t value) {
    return eval_stack_push_with_meta(stack, type, value, (stack_meta_t){});
}

tdn_err_t eval_stack_push_with_meta(eval_stack_t* stack, RuntimeTypeInfo type, spidir_value_t value, stack_meta_t meta) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the intermediate type, if the type is not an integer one
    // make sure it is not a value type
    type = tdn_get_intermediate_type(type);
    if (type != tInt32 && type != tInt64 && type != tIntPtr) {
        CHECK(!tdn_type_is_valuetype(type));
    }

    // make sure we can push more
    CHECK(arrlen(stack->stack) + 1 <= stack->max_depth);

    // push it
    eval_stack_item_t item = {
        .type = type,
        .meta = meta,
        .value = value
    };
    arrpush(stack->stack, item);

cleanup:
    return err;
}

tdn_err_t eval_stack_alloc(eval_stack_t* stack, spidir_builder_handle_t builder, RuntimeTypeInfo type, spidir_value_t* out_value) {
    tdn_err_t err = TDN_NO_ERROR;

    // make sure we can push more
    CHECK(arrlen(stack->stack) + 1 <= stack->max_depth);

    // get the type allocation
    eval_stack_value_instance_t* alloc = hmgetp_null(stack->instance_stacks, type);
    if (alloc == NULL) {
        eval_stack_value_instance_t temp = { .key = type };
        hmputs(stack->instance_stacks, temp);
        alloc = hmgetp_null(stack->instance_stacks, type);
        CHECK(alloc != NULL);
    }

    // get the next location, if non-available allocate a new one
    if (alloc->depth == arrlen(alloc->stack)) {
        spidir_value_t new = spidir_builder_build_stackslot(builder, type->StackSize, type->StackAlignment);
        arrpush(alloc->stack, new);
    }
    CHECK(alloc->depth < arrlen(alloc->stack));
    *out_value = alloc->stack[alloc->depth++];

    // push it, we mark the stack_slot as true to say
    // its already in a stack slot and we don't need
    // to move it
    eval_stack_item_t item = {
        .type = type,
        .value = *out_value
    };
    arrpush(stack->stack, item);

cleanup:
    return err;
}

tdn_err_t eval_stack_pop(
    eval_stack_t* stack,
    spidir_builder_handle_t builder,
    RuntimeTypeInfo* out_type,
    spidir_value_t* out_value,
    stack_meta_t* meta
) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(arrlen(stack->stack) >= 1);
    eval_stack_item_t item = arrpop(stack->stack);

    // output important data if needed
    if (out_type != NULL) *out_type = item.type;
    if (out_value != NULL) *out_value = item.value;
    if (meta != NULL) *meta = item.meta;

    // check if we need to remove it from the used stack-slots list
    if (jit_is_struct_type(item.type)) {
        eval_stack_value_instance_t* alloc = hmgetp_null(stack->instance_stacks, item.type);
        CHECK(alloc != NULL);
        alloc->depth--;
    }

cleanup:
    return err;
}

tdn_err_t eval_stack_snapshot(
    spidir_builder_handle_t builder,
    eval_stack_t* stack,
    jit_label_t* target,
    jit_label_t* current
) {
    tdn_err_t err = TDN_NO_ERROR;
    eval_stack_snapshot_t* snapshot = &target->snapshot;
    bool target_is_current = target == current;

    CHECK(!snapshot->initialized);

    // we are going to create phis on the target block, switch to it
    if (!target_is_current && target->needs_phi) {
        spidir_builder_set_block(builder, target->block);
    }

    // copy all of the types on the stack, and figure the correct value to use
    arrsetlen(snapshot->stack, arrlen(stack->stack));
    for (int i = 0; i < arrlen(stack->stack); i++) {
        eval_stack_snapshot_item_t* item = &snapshot->stack[i];
        spidir_value_t input_value = stack->stack[i].value;

        // copy the type over
        item->type = stack->stack[i].type;

        // create a phi if need be, otherwise just use the value as-is
        if (target->needs_phi) {
            // figure the node type, default is ptr
            spidir_value_type_t type = SPIDIR_TYPE_PTR;
            if (item->type == tInt32) {
                type = SPIDIR_TYPE_I32;
            } else if (item->type == tInt64 || item->type == tIntPtr) {
                type = SPIDIR_TYPE_I64;
            }

            // create the phi
            item->value = spidir_builder_build_phi(builder, type, 1, &input_value, &item->phi);
        } else {
            // just copy the value
            item->phi = (spidir_phi_t){ .id = -1 };
            item->value = input_value;
        }
    }

    // return to the current block
    if (!target_is_current && target->needs_phi) {
        spidir_builder_set_block(builder, current->block);
    }

    // mark this as initialized
    snapshot->initialized = true;

cleanup:
    return err;
}

tdn_err_t eval_stack_update_phis(
    eval_stack_t* stack,
    jit_label_t* label
) {
    tdn_err_t err = TDN_NO_ERROR;
    eval_stack_snapshot_t* snapshot = &label->snapshot;

    // we are updating phis so we need phis lol
    CHECK(label->needs_phi);

    // make sure we have the same amount of items
    CHECK(arrlen(stack->stack) == arrlen(snapshot->stack));

    // just copy over the phi values
    for (int i = 0; i < arrlen(stack->stack); i++) {
        stack->stack[i].value = snapshot->stack[i].value;
    }

cleanup:
    return err;
}

static RuntimeTypeInfo eval_stack_compare(RuntimeTypeInfo S, RuntimeTypeInfo T) {
    // 1.
    if (tdn_type_verifier_assignable_to(T, S)) {
        return S;
    }

    // 2.
    if (tdn_type_verifier_assignable_to(S, T)) {
        return T;
    }

    // TODO: 3.

    return NULL;
}

tdn_err_t eval_stack_merge(
    spidir_builder_handle_t builder,
    eval_stack_t* stack,
    jit_label_t* target,
    bool modify
) {
    tdn_err_t err = TDN_NO_ERROR;
    eval_stack_snapshot_t* snapshot = &target->snapshot;

    // if we are merging it must mean that we need a phi
    // because we had a snapshot already in this location
    CHECK(target->needs_phi);

    // make sure we have the same amount of items
    CHECK(arrlen(stack->stack) == arrlen(snapshot->stack));

    for (int i = 0; i < arrlen(stack->stack); i++) {
        // make sure the types are good
        // TODO: support modifying properly
        RuntimeTypeInfo S = eval_stack_compare(stack->stack[i].type, snapshot->stack[i].type);
        if (stack->stack[i].type != S) {
            CHECK(modify);
            stack->stack[i].type = S;
            snapshot->stack[i].type = S;
        }

        // add the input to the phi
        spidir_builder_add_phi_input(builder, snapshot->stack[i].phi, stack->stack[i].value);
    }

cleanup:
    return err;

}

void eval_stack_clear(eval_stack_t* stack) {
    // free the current stack
    arrfree(stack->stack);

    // reset the depths of all the different locations
    for (int i = 0; i < hmlen(stack->instance_stacks); i++) {
        stack->instance_stacks[i].depth = 0;
    }
}

void eval_stack_free(eval_stack_t* stack) {
    arrfree(stack->stack);
    for (int i = 0; i < hmlen(stack->instance_stacks); i++) {
        arrfree(stack->instance_stacks[i].stack);
    }
    hmfree(stack->instance_stacks);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit labels/blocks
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// TODO: non-linear search

jit_label_t* jit_get_label(jit_label_t* labels, uint32_t address) {
    for (int i = 0; i < arrlen(labels); i++) {
        if (labels[i].address == address) {
            // already has a label in here
            return &labels[i];
        }
    }
    return NULL;
}

jit_label_t* jit_add_label(jit_label_t** labels, uint32_t address) {
    int i;
    for (i = 0; i < arrlen(*labels); i++) {
        if ((*labels)[i].address > address) {
            break;
        } else if ((*labels)[i].address == address) {
            // already has a label in here, we will signal
            // that we need to move to a stack slot
            TRACE("NODE AT %04x NEEDS PHI", address);
            (*labels)[i].needs_phi = true;
            return NULL;
        }
    }

    TRACE("CREATED LABEL TO %04x", address);
    jit_label_t label = {
        .address = address,
    };
    arrins(*labels, i, label);
    return &((*labels)[i]);
}
