#include "control_flow.h"

#include "instruction.h"
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

static bool verifier_try_merge_values(const stack_value_t* value_a, const stack_value_t* value_b, stack_value_t* merged) {
    *merged = *value_a;

    // merge the flags
    if (value_b->flags.ref_read_only) {
        merged->flags.ref_read_only = true;
    }

    if (!value_b->flags.this_ptr) {
        merged->flags.this_ptr = false;
    }

    if (!value_b->flags.ref_non_local) {
        merged->flags.ref_read_only = false;
    }

    if (!value_b->flags.ref_struct_non_local) {
        merged->flags.ref_struct_non_local = false;
    }

    // Same type
    if (value_a->kind == value_b->kind && value_a->type == value_b->type) {
        return true;
    }

    if (verifier_is_null_reference(value_a)) {
        // Null can be any reference type
        if (value_b->kind == KIND_OBJ_REF) {
            *merged = *value_b;
            return true;
        }

    } else if (value_a->kind == KIND_OBJ_REF) {
        if (value_b->kind != KIND_OBJ_REF) {
            return false;
        }

        // Null can be any reference type
        if (verifier_is_null_reference(value_b)) {
            return true;
        }

        // Merging classes always succeeds since System.Object always works
        stack_value_init(merged, verifier_merge_object_references(value_a->type, value_b->type));
        return true;
    }

    return false;
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

    // propogate the this-state properly
    verifier_propagate_state(from, target);

    if (target->visited) {
        // Propagate stack across block bounds
        stack_value_t* entry_stack = target->stack;
        CHECK_ERROR(arrlen(entry_stack) == arrlen(from->stack),
            TDN_ERROR_VERIFIER_PATH_STACK_DEPTH);

        for (int i = 0; i < arrlen(entry_stack); i++) {
            CHECK_ERROR(entry_stack[i].kind == from->stack[i].kind,
                TDN_ERROR_VERIFIER_PATH_STACK_UNEXPECTED);

            if (entry_stack[i].type != from->stack[i].type) {
                if (!verifier_is_assignable(&from->stack[i], &entry_stack[i])) {
                    stack_value_t merged_value = {};
                    CHECK_ERROR(verifier_try_merge_values(&entry_stack[i], &from->stack[i], &merged_value),
                        TDN_ERROR_VERIFIER_PATH_STACK_UNEXPECTED);

                    // If merged actually changed entry stack
                    if (!stack_values_same(&merged_value, &entry_stack[i])) {
                        entry_stack[i] = merged_value;

                        // Make sure the block is re-verified
                        if (target->state != BLOCK_STATE_IS_PENDING) {
                            target->state = BLOCK_STATE_UNMARKED;
                        }
                    }
                }
            }
        }

    } else {
        // copy the stack
        arrsetlen(target->stack, arrlen(from->stack));
        memcpy(target->stack, from->stack, sizeof(*from->stack) * arrlen(from->stack));
    }

    target->visited = true;
    verifier_mark_block(function, target);

cleanup:
    return err;
}

static void verifier_propagate_locals(block_t* next, block_local_t* from, block_local_t* target) {
    ASSERT(arrlen(from) == arrlen(target));

    for (int i = 0; i < arrlen(from); i++) {
        if (
            target[i].ref_read_only != from[i].ref_read_only ||
            target[i].ref_non_local != from[i].ref_non_local ||
            target[i].ref_struct_non_local != from[i].ref_struct_non_local
        ) {
            // Next block has 'this' initialized, but current state has not
            // therefore next block must be reverified with 'this' uninitialized
            if (next->state == BLOCK_STATE_WAS_VERIFIED) {
                next->state = BLOCK_STATE_UNMARKED;
            }
        }

        // merge all the values
        target[i].ref_read_only = target[i].ref_read_only && from[i].ref_read_only;
        target[i].ref_non_local = target[i].ref_non_local && from[i].ref_non_local;
        target[i].ref_struct_non_local = target[i].ref_struct_non_local && from[i].ref_struct_non_local;
    }
}

void verifier_propagate_state(block_t* from, block_t* target) {
    if (target->state == BLOCK_STATE_UNMARKED) {
        target->this_initialized = from->this_initialized;

        arrsetlen(target->locals, arrlen(from->locals));
        memcpy(target->locals, from->locals, sizeof(*target->locals) * arrlen(from->locals));

        arrsetlen(target->args, arrlen(from->args));
        memcpy(target->args, from->args, sizeof(*target->args) * arrlen(from->args));

    } else {
        if (target->this_initialized != from->this_initialized) {
            // Next block has 'this' initialized, but current state has not
            // therefore next block must be reverified with 'this' uninitialized
            if (target->state == BLOCK_STATE_WAS_VERIFIED) {
                target->state = BLOCK_STATE_UNMARKED;
            }
        }
        target->this_initialized = target->this_initialized && from->this_initialized;

        // propagate the state of the args and locals of the block
        verifier_propagate_locals(target, from->locals, target->locals);
        verifier_propagate_locals(target, from->args, target->args);
    }
}

static bool verifier_is_disjoint_try_block(function_t* function, RuntimeExceptionHandlingClause disjoint, RuntimeExceptionHandlingClause source) {
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

tdn_err_t verifier_is_valid_leave_target(function_t* function, block_t* src_blk, block_t* target_blk) {
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
            RuntimeExceptionHandlingClause src_region = src->handler_clause;
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
            // Not first instruction
            target->start != target_region->TryOffset &&

            // Not associated handler
            (src->handler_clause == NULL || src->handler_clause != target->try_clause) &&

            // Target region does not enclose source
            (target_region->TryOffset > src->start || target_region->TryOffset + target_region->TryLength < src->start)
        ) {
            CHECK_FAIL_ERROR(TDN_ERROR_VERIFIER_LEAVE_INTO_TRY);
        }
    }

cleanup:
    return err;
}
