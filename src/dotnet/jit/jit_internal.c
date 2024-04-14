#include "jit_internal.h"
#include "util/except.h"
#include "tomatodotnet/types/type.h"

#include "dotnet/jit/spidir/spidir_debug.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Evaluation stack
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t eval_stack_push(eval_stack_t* stack, RuntimeTypeInfo type, jit_value_t value) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(type != NULL);

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
        .value = value
    };
    arrpush(stack->stack, item);

cleanup:
    return err;
}

tdn_err_t eval_stack_alloc(eval_stack_t* stack, jit_builder_t builder, RuntimeTypeInfo type, jit_value_t* out_value) {
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
        jit_value_t new = jit_builder_build_stackslot(builder, type->StackSize, type->StackAlignment);
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
    RuntimeTypeInfo* out_type,
    jit_value_t* out_value
) {
    tdn_err_t err = TDN_NO_ERROR;

    eval_stack_item_t item;
    CHECK_AND_RETHROW(eval_stack_pop_item(stack, &item));

    if (out_type != NULL) *out_type = item.type;
    if (out_value != NULL) *out_value = item.value;

cleanup:
    return err;
}

tdn_err_t eval_stack_pop_item(
    eval_stack_t* stack,
    eval_stack_item_t* out_item
) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(arrlen(stack->stack) >= 1);
    eval_stack_item_t item = arrpop(stack->stack);

    // output important data if needed
    *out_item = item;

    // check if we need to remove it from the used stack-slots list
    if (jit_is_struct_type(item.type)) {
        eval_stack_value_instance_t* alloc = hmgetp_null(stack->instance_stacks, item.type);
        CHECK(alloc != NULL);
        alloc->depth--;
    }

cleanup:
    return err;
}

eval_stack_item_t* eval_stack_get_top(
    eval_stack_t* stack
) {
    if (arrlen(stack->stack) == 0) {
        return NULL;
    }
    return &arrlast(stack->stack);
}

tdn_err_t eval_stack_snapshot(
    jit_builder_t builder,
    eval_stack_t* stack,
    jit_label_t* target
) {
    tdn_err_t err = TDN_NO_ERROR;
    eval_stack_snapshot_t* snapshot = &target->snapshot;

    // get the current block if we need to modify the target one 
    jit_block_t current_block;
    CHECK(jit_builder_cur_block(builder, &current_block));
    bool target_is_current = JIT_IS_SAME_BLOCK(target->block, current_block);

    CHECK(!snapshot->initialized);

    // we are going to create phis on the target block, switch to it
    if (!target_is_current && target->needs_phi) {
        jit_builder_set_block(builder, target->block);
    }

    // copy all of the types on the stack, and figure the correct value to use
    arrsetlen(snapshot->stack, arrlen(stack->stack));
    for (int i = 0; i < arrlen(stack->stack); i++) {
        eval_stack_snapshot_item_t* item = &snapshot->stack[i];
        jit_value_t input_value = stack->stack[i].value;

        // copy the type over
        item->type = stack->stack[i].type;

        // create a phi if need be, otherwise just use the value as-is
        if (target->needs_phi) {
            // figure the node type, default is ptr
            jit_value_type_t type = JIT_TYPE_PTR;
            if (item->type == tInt32) {
                type = JIT_TYPE_I32;
            } else if (item->type == tInt64 || item->type == tIntPtr) {
                type = JIT_TYPE_I64;
            }

            // create the phi
            item->value = jit_builder_build_phi(builder, type, 1, &input_value, &item->phi);
        } else {
            // just copy the value
            item->phi = JIT_INIT_PHI;
            item->value = input_value;
        }
    }

    // copy the instance stack depth
    for (int i = 0; i < hmlen(stack->instance_stacks); i++) {
        hmput(snapshot->instance_stacks, stack->instance_stacks[i].key, stack->instance_stacks[i].depth);
    }

    // return to the current block
    if (!target_is_current && target->needs_phi) {
        jit_builder_set_block(builder, current_block);
    }

    // mark this as initialized
    snapshot->initialized = true;

cleanup:
    return err;
}

tdn_err_t eval_stack_snapshot_restore(
    eval_stack_t* stack,
    eval_stack_snapshot_t* snapshot
) {
    tdn_err_t err = TDN_NO_ERROR;

    // restore the depth
    for (int i = 0; i < hmlen(stack->instance_stacks); i++) {
        stack->instance_stacks[i].depth = hmget(snapshot->instance_stacks, stack->instance_stacks[i].key);
    }

    arrsetlen(stack->stack, arrlen(snapshot->stack));
    for (int i = 0; i < arrlen(snapshot->stack); i++) {
        stack->stack[i].type = snapshot->stack[i].type;
        stack->stack[i].value = snapshot->stack[i].value;
    }

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
    jit_builder_t builder,
    eval_stack_t* stack,
    jit_label_t* target,
    bool modify
) {
    tdn_err_t err = TDN_NO_ERROR;
    eval_stack_snapshot_t* snapshot = &target->snapshot;

    //
    // we only ever merge when we go to a location which is already initialized
    // which can happen on two reasons:
    // - jump to a location we already went to
    // - back jump to a previous location with an empty stack
    //
    // in the second case we don't need a phi, but we will still
    // go to the merge path, so make sure that case is happening if we
    // didn't mark that we need a phi
    //
    if (!target->needs_phi) {
        CHECK(arrlen(stack->stack) == 0);
    }

    // make sure we have the same amount of items
    CHECK(arrlen(stack->stack) == arrlen(snapshot->stack),
          "expected %d items, got %d items", arrlen(snapshot->stack), arrlen(stack->stack));

    for (int i = 0; i < arrlen(stack->stack); i++) {
        // make sure the types are good
        // TODO: support modifying properly
        RuntimeTypeInfo S = eval_stack_compare(stack->stack[i].type, snapshot->stack[i].type);
        if (stack->stack[i].type != S) {
            CHECK(modify);
            stack->stack[i].type = S;
            snapshot->stack[i].type = S;
        }

        // if both are not the same then force this into a non-local reference
        if (stack->stack[i].is_nonlocal_ref != snapshot->stack[i].is_nonlocal_ref) {
            CHECK(modify);
            stack->stack[i].is_nonlocal_ref = false;
        }

        // if both are not the same then force this into a readonly reference
        if (stack->stack[i].is_readable != snapshot->stack[i].is_readable) {
            CHECK(modify);
            stack->stack[i].is_readable = false;
        }

        // if both are not the same then force this into a readonly reference
        if (stack->stack[i].is_writable != snapshot->stack[i].is_writable) {
            CHECK(modify);
            stack->stack[i].is_writable = false;
        }

        // add the input to the phi
        jit_builder_add_phi_input(builder, snapshot->stack[i].phi, stack->stack[i].value);
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

static void jit_region_free_labels(jit_region_t* region) {
    for (int i = 0; i < arrlen(region->labels); i++) {
        arrfree(region->labels[i].snapshot.stack);
    }
    arrfree(region->labels);
}

void jit_region_free(jit_region_t* region) {
    // only free this on the handler path, and not on the protected path, since that
    // pointer is shared in both
    if (region->is_handler && region->finally_handlers != NULL) {
        for (int i = 0; i < hmlen(region->finally_handlers->finally_paths); i++) {
            jit_region_t* reg = region->finally_handlers->finally_paths[i].region;
            jit_region_free_labels(reg);
            tdn_host_free(reg);
        }
        hmfree(region->finally_handlers->finally_paths);

        tdn_host_free(region->finally_handlers);
    }

    jit_region_free_labels(region);
}

static int jit_get_label_index(jit_region_t* ctx, uint32_t address) {
    int low = 0;
    int high = arrlen(ctx->labels);

    while (low < high) {
        int mid = (low + high) / 2;
        if (ctx->labels[mid].address < address) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }

    return low;
}

jit_label_t* jit_get_label(jit_region_t* ctx, uint32_t address) {
    int idx = jit_get_label_index(ctx, address);
    if (idx >= arrlen(ctx->labels)) return NULL;
    jit_label_t* label = &ctx->labels[idx];
    if (label->address != address) return NULL;
    return label;
}

int jit_get_label_location_index(jit_context_t* ctx, uint32_t address, bool exact) {
    int low = 0;
    int high = arrlen(ctx->labels);

    while (low < high) {
        int mid = (low + high) / 2;
        if (ctx->labels[mid].pc < address) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }

    if (exact) {
        return (low < arrlen(ctx->labels) && ctx->labels[low].pc == address) ? low : -1;
    } else {
        return low;
    }
}

void jit_add_label_location(jit_context_t* ctx, uint32_t address) {
    int index = jit_get_label_location_index(ctx, address, false);
    if (index < arrlen(ctx->labels)) {
        // adding inside the array, check if we already have this entry or not
        if (ctx->labels[index].pc == address) {
            // yes, set we need the phi and return NULL to show it was already set
            ctx->labels[index].needs_phi = true;
        } else {
            // no, insert it
            jit_label_location_t location = {
                .pc = address
            };
            arrins(ctx->labels, index, location);
        }
    } else {
        // adding outside the array, make sure its the last
        // entry and insert it
        ASSERT(index == arrlen(ctx->labels));
        jit_label_location_t location = {
            .pc = address
        };
        arrpush(ctx->labels, location);
    }
}
