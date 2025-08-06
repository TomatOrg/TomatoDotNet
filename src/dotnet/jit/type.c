#include "type.h"

const type_instruction_t g_type_dispatch_table[] = {

};
const size_t g_type_dispatch_table_size = ARRAY_LENGTH(g_type_dispatch_table);

static void jit_queue_block(function_t* function, block_t* block) {
    if (!block->in_queue) {
        arrpush(function->queue, block);
        block->visited = true;
        block->in_queue = true;
    }
}

// we are going to use the verifier object merging because it is
// pretty generic and will fit well with
RuntimeTypeInfo verifier_merge_object_references(RuntimeTypeInfo class_a, RuntimeTypeInfo class_b);

static bool type_merge_stack_values(stack_value_t* previous, stack_value_t* new) {
    bool modified = false;

    // we have different types, we are going to find the most common type
    // and remove the exact mark since it will not be exact at this point
    if (new->type != previous->type) {
        previous->type = verifier_merge_object_references(new->type, previous->type);
        previous->exact_type = false;
        modified = false;
    }

    // one of them is not exact, remove the exact flag
    if (new->exact_type != previous->exact_type) {
        previous->exact_type = false;
        modified = true;
    }

    // methods don't match, zero the method
    // NOTE: this can only be from a delegate devirt
    if (previous->method != new->method) {
        previous->method = NULL;
        modified = true;
    }

    return modified;
}

static bool type_merge_block_local(block_local_t* previous, block_local_t* new) {
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

    // merge the flags
    if (type_merge_stack_values(&previous->value, &new->value)) {
        modified = true;
    }

    return modified;
}

static tdn_err_t type_merge_blocks(function_t* function, block_t* from, block_t* target) {
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
            if (type_merge_stack_values(&target->stack[i], &from->stack[i])) {
                modified = true;
            }
        }

        // the metadata of the block was modified, we must
        if (modified) {
            // blocks should not be modified while emitting
            CHECK(!function->emitting);
            jit_queue_block(function, target);

        } else if (!target->visited) {
            jit_queue_block(function, target);
        }

    } else {
        // first time being visited, copy over all the type information as is
        arrsetlen(target->stack, arrlen(from->stack));
        memcpy(target->stack, from->stack, arrlen(from->stack) * sizeof(*from->stack));

        arrsetlen(target->args, arrlen(from->args));
        memcpy(target->args, from->args, arrlen(from->args) * sizeof(*from->args));

        arrsetlen(target->locals, arrlen(from->locals));
        memcpy(target->locals, from->locals, arrlen(from->locals) * sizeof(*from->locals));

        jit_queue_block(function, target);
    }

cleanup:
    return err;
}

tdn_err_t type_on_block_fallthrough(function_t* function, block_t* from, block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;

    // just use the fallthrough code
    CHECK_AND_RETHROW(type_merge_blocks(function, from, block));

cleanup:
    return err;
}

