#include "jit_internal.h"
#include "util/except.h"
#include "tomatodotnet/types/type.h"

#include "dotnet/jit/generic/spidir/spidir_debug.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Evaluation stack
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t eval_stack_push(eval_stack_t* stack, RuntimeTypeInfo type, ir_node* value) {
    return eval_stack_push_with_meta(stack, type, value, (stack_meta_t){});
}

tdn_err_t eval_stack_push_with_meta(eval_stack_t* stack, RuntimeTypeInfo type, ir_node* value, stack_meta_t meta) {
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
        .meta = meta,
    };
    set_value(arrlen(stack->stack), value);
    arrpush(stack->stack, item);

cleanup:
    return err;
}

tdn_err_t eval_stack_alloc(eval_stack_t* stack, RuntimeTypeInfo type, ir_node** out_value) {
    tdn_err_t err = TDN_NO_ERROR;

    // make sure we can push more
    CHECK(arrlen(stack->stack) + 1 <= stack->max_depth);

    CHECK_FAIL();

    // push it, we mark the stack_slot as true to say
    // its already in a stack slot and we don't need
    // to move it
    eval_stack_item_t item = {
        .type = type,
    };
    arrpush(stack->stack, item);

cleanup:
    return err;
}

tdn_err_t eval_stack_pop(
    eval_stack_t* stack,
    RuntimeTypeInfo* out_type,
    ir_node** out_value,
    stack_meta_t* meta
) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(arrlen(stack->stack) >= 1);
    eval_stack_item_t item = arrpop(stack->stack);

    // output important data if needed
    if (out_type != NULL) *out_type = item.type;
    if (out_value != NULL) *out_value = get_value(arrlen(stack->stack), get_type_mode(get_ir_type(item.type)));
    if (meta != NULL) *meta = item.meta;

cleanup:
    return err;
}

tdn_err_t eval_stack_snapshot(
    eval_stack_t* stack,
    jit_label_t* target
) {
    tdn_err_t err = TDN_NO_ERROR;
    eval_stack_snapshot_t* snapshot = &target->snapshot;

    CHECK(!snapshot->initialized);

    // copy all of the types on the stack, and figure the correct value to use
    arrsetlen(snapshot->stack, arrlen(stack->stack));
    for (int i = 0; i < arrlen(stack->stack); i++) {
        eval_stack_snapshot_item_t* item = &snapshot->stack[i];
        item->type = stack->stack[i].type;
    }

    // mark this as initialized
    snapshot->initialized = true;

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
    }

cleanup:
    return err;

}

void eval_stack_clear(eval_stack_t* stack) {
    // free the current stack
    arrfree(stack->stack);

//    // reset the depths of all the different locations
//    for (int i = 0; i < hmlen(stack->instance_stacks); i++) {
//        stack->instance_stacks[i].depth = 0;
//    }
}

void eval_stack_free(eval_stack_t* stack) {
    arrfree(stack->stack);
//    for (int i = 0; i < hmlen(stack->instance_stacks); i++) {
//        arrfree(stack->instance_stacks[i].stack);
//    }
//    hmfree(stack->instance_stacks);
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
        if (ctx->labels[index].pc != address) {
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
