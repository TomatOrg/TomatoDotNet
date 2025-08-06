#include "control_flow.h"

#include "internal.h"
#include "dotnet/types.h"
#include "tomatodotnet/types/type.h"
#include "tomatodotnet/util/stb_ds.h"
#include "util/except.h"
#include "util/string.h"


typedef enum merge_status {
    MERGE_SUCCESS,
    MERGE_FAILED,
    MERGE_MODIFIED,
} merge_status_t;

static RuntimeTypeInfo verifier_merge_interface_with_interface(RuntimeTypeInfo iface_a, RuntimeTypeInfo iface_b) {
    if (hmgeti(iface_a->InterfaceImpls, iface_b) >= 0) {
        // interface A extends interface B
        return iface_b;
    }

    if (hmgeti(iface_b->InterfaceImpls, iface_a) >= 0) {
        // interface B extends interface A
        return iface_a;
    }

    // Get common supertype
    for (int i = 0; i < hmlen(iface_b->InterfaceImpls); i++) {
        RuntimeTypeInfo subiface_b = iface_b->InterfaceImpls[i].key;
        if (hmgeti(iface_a->InterfaceImpls, subiface_b) >= 0) {
            return subiface_b;
        }
    }

    // No compatible interfaces found, return object instead
    return tObject;
}

static RuntimeTypeInfo verifier_merge_class_with_class(RuntimeTypeInfo class_a, RuntimeTypeInfo class_b) {
    // Find class hierarchy depth for both classes

    int a_depth = 0;
    for (RuntimeTypeInfo cur_type = class_a; cur_type != NULL; cur_type = cur_type->BaseType) {
        a_depth++;
    }

    int b_depth = 0;
    for (RuntimeTypeInfo cur_type = class_b; cur_type != NULL; cur_type = cur_type->BaseType) {
        b_depth++;
    }

    // Walk up the subperclass chain until both classes at same level
    while (a_depth > b_depth) {
        class_a = class_a->BaseType;
        a_depth--;
    }

    while (b_depth > a_depth) {
        class_b = class_b->BaseType;
        b_depth--;
    }

    while (class_a != class_b) {
        class_a = class_a->BaseType;
        class_b = class_b->BaseType;
    }

    return class_a;
}

static RuntimeTypeInfo verifier_merge_class_with_interface(RuntimeTypeInfo class, RuntimeTypeInfo iface) {
    // Check if class implements interface
    if (hmgeti(class->InterfaceImpls, iface) >= 0) {
        return iface;
    }

    // Check if class and interface implement common interface
    for (int i = 0; i < hmlen(iface->InterfaceImpls); i++) {
        RuntimeTypeInfo i_iface = iface->InterfaceImpls[i].key;
        if (hmgeti(class->InterfaceImpls, i_iface) >= 0) {
            return i_iface;
        }
    }

    // No compatible merge, return object
    return tObject;
}

RuntimeTypeInfo verifier_merge_object_references(RuntimeTypeInfo class_a, RuntimeTypeInfo class_b) {
    if (class_a == class_b) {
        return class_a;
    }

    if (tdn_is_interface(class_b)) {
        if (tdn_is_interface(class_a)) {
            return verifier_merge_interface_with_interface(class_a, class_b);
        } else {
            return verifier_merge_class_with_interface(class_a, class_b);
        }
    } else if (tdn_is_interface(class_a)) {
        return verifier_merge_class_with_interface(class_b, class_a);
    } else {
        return verifier_merge_class_with_class(class_a, class_b);
    }
}

static bool verifier_merge_flags(value_flags_t* previous, value_flags_t new) {
    bool modified = false;

    if (new.ref_read_only && !previous->ref_read_only) {
        previous->ref_read_only = true;
        modified = true;
    }

    // turned into a local ref, need to verify again
    if (!new.ref_non_local && previous->ref_non_local) {
        previous->ref_non_local = false;
        modified = true;
    }

    // turned into a local ref, need to verify again
    if (!new.ref_struct_non_local && previous->ref_struct_non_local) {
        previous->ref_struct_non_local = false;
        modified = true;
    }

    return modified;
}

static merge_status_t verifier_merge_stack_values(stack_value_t* previous, stack_value_t* new) {
    bool modified = false;

    // merge the flags
    if (verifier_merge_flags(&previous->flags, new->flags)) {
        modified = true;
    }

    // methods don't match, zero the method
    // NOTE: this can only be from a delegate devirt
    if (previous->method != new->method) {
        previous->method = NULL;
        modified = true;
    }

    // if we don't match in the type we need to merge it
    if (previous->kind != new->kind || previous->type != new->type) {
        if (verifier_is_null_reference(previous)) {
            if (new->kind == KIND_OBJ_REF) {
                // we had a null-reference, but now we have a real type
                previous->type = new->type;
                modified = true;
            }
        } else if (previous->kind == KIND_OBJ_REF) {
            // must also be an object reference
            if (new->kind != KIND_OBJ_REF) {
                return MERGE_FAILED;
            }

            if (!verifier_is_null_reference(new)) {
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

static bool verifier_merge_block_local(block_local_t* previous, block_local_t* new) {
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
    if (verifier_merge_flags(&previous->flags, new->flags)) {
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

static tdn_err_t verifier_merge_blocks(function_t* function, block_t* from, block_t* target) {
    tdn_err_t err = TDN_NO_ERROR;

    if (target->visited) {
        for (int i = 0; i < arrlen(from->args); i++) {
            if (verifier_merge_block_local(&target->args[i], &from->args[i])) {
                verifier_queue_block(function, target);
            }
        }

        for (int i = 0; i < arrlen(from->locals); i++) {
            if (verifier_merge_block_local(&target->locals[i], &from->locals[i])) {
                verifier_queue_block(function, target);
            }
        }

        // must have the same length at this point
        CHECK_ERROR(arrlen(from->stack) == arrlen(target->stack),
            TDN_ERROR_VERIFIER_PATH_STACK_DEPTH);

        for (int i = 0; i < arrlen(from->stack); i++) {
            // must have the same kind at least
            CHECK_ERROR(target->stack[i].kind == from->stack[i].kind,
                TDN_ERROR_VERIFIER_PATH_STACK_UNEXPECTED);

            // now attempt to merge the two values
            merge_status_t status = verifier_merge_stack_values(&target->stack[i], &from->stack[i]);
            CHECK_ERROR(status != MERGE_FAILED, TDN_ERROR_VERIFIER_PATH_STACK_UNEXPECTED);
            if (status == MERGE_MODIFIED) {
                verifier_queue_block(function, target);
            }
        }

        // if we are jumping to a block with this initialized, but its
        // not initialized in our block, then mark it as not initialized
        if (target->this_initialized && !from->this_initialized) {
            target->this_initialized = false;
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

        // pass over the initialized mark
        target->this_initialized = from->this_initialized;

        verifier_queue_block(function, target);
    }

cleanup:
    return err;
}


static bool verifier_is_direct_child_region(function_t* function, basic_block_t* enclosing_block, basic_block_t* enclosed_block) {
    RuntimeExceptionHandlingClause enclosed_region = enclosed_block->try_clause;
    for (int i = enclosed_region->TryOffset - 1; i > (long)enclosing_block->start; i--) {
        int idx = hmgeti(function->labels, i);
        if (idx < 0) {
            continue;
        }
        basic_block_t* block = &function->labels[idx].value;

        if (block->try_start && block->try_clause != enclosing_block->try_clause) {
            RuntimeExceptionHandlingClause block_region = block->try_clause;

            // block region is actually enclosing enclosed_region
            if (block_region->TryOffset + block_region->TryLength > enclosed_region->TryOffset) {
                return false;
            }
        }
    }

    return true;
}

static tdn_err_t verifier_is_valid_branch_target(
    function_t* function,
    basic_block_t* src, basic_block_t* target,
    bool is_fallthrough
) {
    tdn_err_t err = TDN_NO_ERROR;

    if (src->try_clause != target->try_clause) {
        if (src->try_clause == NULL) {
            // Branching to first instruction of try-block is valid
            CHECK_ERROR(
                target->start == target->try_clause->TryOffset &&
                verifier_is_direct_child_region(function, src, target),
                TDN_ERROR_VERIFIER_BRANCH_INTO_TRY
            );

        } else if (target->try_clause == NULL) {
            if (is_fallthrough) {
                CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_FALLTHROUGH_EXCEPTION);
            } else {
                CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_BRANCH_OUT_OF_TRY);
            }

        } else {
            // If target is inside source region
            RuntimeExceptionHandlingClause src_region = src->try_clause;
            RuntimeExceptionHandlingClause target_region = target->try_clause;
            if (
                src_region->TryOffset <= target_region->TryOffset &&
                target->start < src_region->TryOffset + src_region->TryLength
            ) {
                // Only branching to first instruction of try-block is valid
                CHECK_ERROR(
                    target->start == target_region->TryOffset &&
                    verifier_is_direct_child_region(function, src, target),
                    TDN_ERROR_VERIFIER_BRANCH_INTO_TRY
                );
            } else {
                if (is_fallthrough) {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_FALLTHROUGH_EXCEPTION);
                } else {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_BRANCH_OUT_OF_TRY);
                }
            }
        }
    }

    if (src->filter_clause != target->filter_clause) {
        if (src->filter_clause == NULL) {
            if (is_fallthrough) {
                CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_FALLTHROUGH_INTO_FILTER);
            } else {
                CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_BRANCH_INTO_FILTER);
            }
        } else if (target->handler_clause == NULL) {
            if (is_fallthrough) {
                CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_FALLTHROUGH_EXCEPTION);
            } else {
                CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_BRANCH_OUT_OF_FILTER);
            }
        } else {
            RuntimeExceptionHandlingClause src_region = src->filter_clause;
            RuntimeExceptionHandlingClause target_region = target->filter_clause;
            if (src_region->FilterOffset <= target_region->FilterOffset) {
                if (is_fallthrough) {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_FALLTHROUGH_INTO_FILTER);
                } else {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_BRANCH_INTO_FILTER);
                }
            } else {
                if (is_fallthrough) {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_FALLTHROUGH_EXCEPTION);
                } else {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_BRANCH_OUT_OF_FILTER);
                }
            }
        }
    }

    if (src->handler_clause != target->handler_clause) {
        if (src->handler_clause == NULL) {
            if (is_fallthrough) {
                CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_FALLTHROUGH_INTO_HANDLER);
            } else {
                CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_BRANCH_INTO_HANDLER);
            }
        } else if (target->handler_clause == NULL) {
            if (is_fallthrough) {
                CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_FALLTHROUGH_EXCEPTION);
            } else {
                if (src->handler_clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_BRANCH_OUT_OF_FINALLY);
                } else {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_BRANCH_OUT_OF_HANDLER);
                }
            }
        } else {
            RuntimeExceptionHandlingClause src_region = src->handler_clause;
            RuntimeExceptionHandlingClause target_region = target->handler_clause;
            if (src_region->HandlerOffset <= target_region->HandlerOffset) {
                if (is_fallthrough) {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_FALLTHROUGH_INTO_HANDLER);
                } else {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_BRANCH_INTO_HANDLER);
                }
            } else {
                if (is_fallthrough) {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_FALLTHROUGH_EXCEPTION);
                } else {
                    if (src_region->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
                        CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_BRANCH_OUT_OF_FINALLY);
                    } else {
                        CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_BRANCH_OUT_OF_HANDLER);
                    }
                }
            }
        }
    }

cleanup:
    return err;
}

tdn_err_t verifier_propagate_control_flow(function_t* function, block_t* from, block_t* target, bool is_fallthrough) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure we can perform the branch
    CHECK_AND_RETHROW(verifier_is_valid_branch_target(function, &from->block, &target->block, is_fallthrough));
    CHECK_AND_RETHROW(verifier_merge_blocks(function, from, target));

cleanup:
    return err;
}

static bool verifier_is_disjoint_try_block(function_t *function, RuntimeExceptionHandlingClause disjoint,
                                           RuntimeExceptionHandlingClause source) {
    if (source->TryOffset <= disjoint->TryOffset && source->TryOffset + source->TryLength >= disjoint->TryOffset +
        disjoint->TryLength) {
        // Source is enclosing disjoint
        return false;
    }

    for (int i = disjoint->TryOffset - 1; i >= 0; i--) {
        int idx = hmgeti(function->labels, i);
        if (idx < 0) {
            continue;
        }
        basic_block_t *block = &function->labels[idx].value;

        if (block->try_start) {
            RuntimeExceptionHandlingClause block_region = block->try_clause;
            if (
                (block_region->TryOffset + block_region->TryLength > disjoint->TryOffset) &&
                (block_region->TryOffset > source->TryOffset || block_region->TryOffset + block_region->TryLength <=
                 source->TryOffset)
            ) {
                return false;
            }
        }

        if (block->handler_start) {
            RuntimeExceptionHandlingClause block_region = block->handler_clause;
            if (
                (block_region->HandlerOffset + block_region->HandlerLength > disjoint->TryOffset) &&
                (block_region->HandlerOffset > source->TryOffset || block_region->HandlerOffset + block_region->
                 HandlerLength <= source->TryOffset)
            ) {
                return false;
            }
        }

        if (block->filter_start) {
            RuntimeExceptionHandlingClause block_region = block->filter_clause;
            uint32_t filter_length = block_region->HandlerOffset - block_region->FilterOffset;
            if (
                (block_region->FilterOffset + filter_length > disjoint->TryOffset) &&
                (block_region->FilterOffset > source->TryOffset || block_region->FilterOffset + filter_length <= source
                 ->TryOffset)
            ) {
                return false;
            }
        }
    }

    return true;
}

tdn_err_t verifier_is_valid_leave_target(function_t *function, block_t *src_blk, block_t *target_blk) {
    tdn_err_t err = TDN_NO_ERROR;
    basic_block_t *src = &src_blk->block;
    basic_block_t *target = &target_blk->block;

    // If source is within filter, target shall be within the same
    CHECK_ERROR(
        src->filter_clause == NULL ||
        src->filter_clause == target->filter_clause,
        TDN_ERROR_VERIFIER_LEAVE_OUT_OF_FILTER
    );

    // If the source is within fault handler or finally handler, target shall be within the same
    if (src->handler_clause != NULL && src->handler_clause != target->handler_clause) {
        int kind = src->handler_clause->Flags;
        if (kind == COR_ILEXCEPTION_CLAUSE_FAULT) {
            CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_LEAVE_OUT_OF_FAULT);
        } else if (kind == COR_ILEXCEPTION_CLAUSE_FINALLY) {
            CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_LEAVE_OUT_OF_FINALLY);
        }
    }

    // If the source is within a try block, target shall be within the same or an eclosing try block
    // or the first instruction of a disjoint try block
    // or not within any try block
    if (src->try_clause != NULL && src->try_clause != target->try_clause) {
        if (target->try_clause != NULL) {
            RuntimeExceptionHandlingClause src_region = src->try_clause;
            RuntimeExceptionHandlingClause target_region = target->try_clause;

            // Target is not enclosing source
            if (
                target_region->TryOffset > src_region->TryOffset ||
                src->start >= target_region->TryOffset + target_region->TryLength
            ) {
                // Target is not first instruction
                if (target->start != target_region->TryOffset) {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_LEAVE_INTO_TRY);

                } else if (
                    src_region->TryOffset <= target_region->TryOffset &&
                    src_region->TryOffset + src_region->TryLength > target_region->TryOffset
                ) {
                    CHECK_ERROR(verifier_is_direct_child_region(function, src, target),
                        TDN_ERROR_VERIFIER_LEAVE_INTO_TRY);

                } else if (!verifier_is_disjoint_try_block(function, target_region, src_region)) {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_LEAVE_INTO_TRY);
                }
            }
        }
    }

    // If the source is within a catch or filtered handler, target shall be within same catch or filtered handler
    // or within the associated try block
    // or within a try block enclosing the catch / filtered handler
    // or the first instruction of a disjoint try block
    // or not within any try block
    if (src->handler_clause != NULL && src->handler_clause != target->handler_clause) {
        if (target->try_clause != NULL) {
            RuntimeExceptionHandlingClause src_region = src->try_clause;
            RuntimeExceptionHandlingClause target_region = target->try_clause;

            // If target is not associated try block, and not enclosing srcRegion
            if (
                target->try_clause != src->handler_clause &&
                (
                    target_region->TryOffset > src_region->HandlerOffset ||
                    target_region->TryOffset + target_region->TryLength < src_region->HandlerOffset
                )
            ) {
                // If target is not first instruction of try, or not a direct sibling
                if (
                    target->start != target_region->TryOffset ||
                    !verifier_is_disjoint_try_block(function, target_region, src_region)
                ) {
                    CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_LEAVE_INTO_TRY);
                }
            }
        }
    }

    // If the target is within a filter or handler, source shall be within same
    if (target->handler_clause != NULL && target->handler_clause != src->handler_clause) {
        RuntimeExceptionHandlingClause target_region = target->handler_clause;
        // If target region is not enclosing source
        if (
            target_region->HandlerOffset > src->start ||
            target_region->HandlerOffset + target_region->HandlerLength < src->start
        ) {
            CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_LEAVE_INTO_HANDLER);
        }
    }
    if (target->filter_clause != NULL && target->filter_clause != src->filter_clause) {
        RuntimeExceptionHandlingClause target_region = target->filter_clause;
        int filter_length = target_region->HandlerOffset - target_region->FilterOffset;

        // If target region is not enclosing source
        if (
            target_region->FilterOffset > src->start ||
            target_region->FilterOffset + filter_length < src->start
        ) {
            CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_LEAVE_INTO_FILTER);
        }
    }

    // If the target is within a try block (except first instruction), source shall be within same
    // or within associated handler
    if (target->try_clause != NULL && src->try_clause != target->try_clause) {
        RuntimeExceptionHandlingClause target_region = target->try_clause;

        if (
            target->start != target_region->TryOffset &&
            (
                src->handler_clause == NULL ||
                src->handler_clause != target->handler_clause
            ) &&
            (
                target_region->TryOffset > src->start ||
                target_region->TryOffset + target_region->TryLength < src->start
            )
        ) {
            CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_LEAVE_INTO_TRY);
        }
    }

cleanup:
    return err;
}

tdn_err_t verifier_propagate_this_state(function_t* function, block_t* from, block_t* target) {
    tdn_err_t err = TDN_NO_ERROR;

    // propagate the `this` state
    if (target->visited) {
        // must have the same length at this point
        CHECK_ERROR(arrlen(from->stack) == arrlen(target->stack),
            TDN_ERROR_VERIFIER_PATH_STACK_DEPTH);

        // if we are jumping to a block with this initialized, but its
        // not initialized in our block, then mark it as not initialized
        if (target->this_initialized && !from->this_initialized) {
            target->this_initialized = false;
            verifier_queue_block(function, target);
        }
    } else {
        // first time being visited, copy over all the type information as is
        arrsetlen(target->stack, 0);
        arrsetlen(target->args, 0);
        arrsetlen(target->locals, 0);

        // pass over the initialized mark
        target->this_initialized = from->this_initialized;

        verifier_queue_block(function, target);
    }

cleanup:
    return err;
}
