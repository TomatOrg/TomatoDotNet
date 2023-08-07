#include "jit_internal.h"
#include "util/except.h"
#include "tinydotnet/types/type.h"

tdn_err_t eval_stack_push(eval_stack_t* stack, RuntimeTypeInfo type, spidir_value_t value) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the intermediate type
    type = tdn_get_intermediate_type(type);

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

tdn_err_t eval_stack_pop(eval_stack_t* stack, spidir_builder_handle_t builder, RuntimeTypeInfo* out_type, spidir_value_t* out_value) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(arrlen(stack->stack) >= 1);
    eval_stack_item_t item = arrpop(stack->stack);
    *out_type = item.type;

    if (item.stack_slot) {
        // if this item was moved from a stack slot
        // then we need to free that stack slot for
        // later use
        spidir_value_type_t type;
        if (item.type == tInt32) {
            stack->i32depth--;
            type = SPIDIR_TYPE_I32;
        } else if (item.type == tInt64 || item.type == tIntPtr) {
            stack->i64depth--;
            type = SPIDIR_TYPE_I64;
        } else if (tdn_type_is_referencetype(item.type) || item.type->IsByRef) {
            stack->ptrdepth--;
            type = SPIDIR_TYPE_PTR;
        } else {
            // TODO: handle valuetypes
            CHECK_FAIL();
        }

        // and return it as a load from the stack slot
        *out_value = spidir_builder_build_load(builder, type, item.value);
    } else {
        // this is just a value
        *out_value = item.value;
    }

cleanup:
    return err;
}

static tdn_err_t move_to_stack_slot(eval_stack_t* stack, spidir_builder_handle_t builder, eval_stack_item_t* item) {
    tdn_err_t err = TDN_NO_ERROR;

    // make sure not already in a stack slot
    if (item->stack_slot) {
        goto cleanup;
    }

    // keep the old value s owe can store it
    spidir_value_t old_value = item->value;

    // allocate the new stack slot for it, we are going to
    // have the item->value be turned into a load from the
    // stack slot, which should be fine
    spidir_value_type_t stack_slot_type;
    if (item->type == tInt32) {
        if (stack->i32depth == arrlen(stack->i32stack)) {
                    arrpush(stack->i32stack,
                            spidir_builder_build_stackslot(
                                    builder,
                                    sizeof(uint32_t), _Alignof(uint32_t)));
        }
        CHECK(stack->i32depth < arrlen(stack->i32stack));
        item->value = stack->i32stack[stack->i32depth++];

    } else if (item->type == tInt64 || item->type == tIntPtr) {
        if (stack->i64depth == arrlen(stack->i64stack)) {
            arrpush(stack->i64stack,
                    spidir_builder_build_stackslot(
                            builder,
                            sizeof(uint32_t), _Alignof(uint32_t)));
        }
        CHECK(stack->i64depth < arrlen(stack->i64stack));
        item->value = stack->i64stack[stack->i64depth++];

    } else if (tdn_type_is_referencetype(item->type) || item->type->IsByRef) {
        if (stack->ptrdepth == arrlen(stack->ptrstack)) {
            arrpush(stack->ptrstack,
                    spidir_builder_build_stackslot(
                            builder,
                            sizeof(uint32_t), _Alignof(uint32_t)));
        }
        CHECK(stack->ptrdepth < arrlen(stack->ptrstack));
        item->value = stack->ptrstack[stack->ptrdepth++];

    } else {
        // TODO: handle valuetypes
        CHECK_FAIL();
    }

    // this is now in a stack slot, we might need to free this later
    item->stack_slot = true;

    // move the value to the stack slot
    spidir_builder_build_store(builder, old_value, item->value);

cleanup:
    return err;
}

tdn_err_t eval_stack_move_to_slots(eval_stack_t* stack, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;

    // move all of them to stack slots
    for (int i = 0; i < arrlen(stack->stack); i++) {
        CHECK_AND_RETHROW(move_to_stack_slot(stack, builder, &stack->stack[i]));
    }

cleanup:
    return err;
}

void eval_stack_clear(eval_stack_t* stack) {
    arrfree(stack->stack);
    stack->i32depth = 0;
    stack->i64depth = 0;
    stack->ptrdepth = 0;
}
