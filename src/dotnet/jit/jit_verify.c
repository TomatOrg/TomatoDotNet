#include "jit_verify.h"

#include <dotnet/types.h>
#include <tomatodotnet/types/type.h>
#include <util/string.h>
#include <util/except.h>
#include "tomatodotnet/util/stb_ds.h"

#include "jit_type.h"

static void verifier_queue_block(jit_function_t* function, jit_block_t* block) {
    if (!block->in_queue) {
        arrpush(function->queue, block);
        block->visited = true;
        block->in_queue = true;
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Access checking helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static bool verifier_can_access_member(RuntimeTypeInfo current_type, RuntimeTypeInfo target_type, uint32_t member_visibility, RuntimeTypeInfo instance);

static bool verifier_grants_friend_access_to(RuntimeModule module_a, RuntimeModule module_b) {
    // TODO: handle System.Runtime.CompilerServices.InternalVisibleToAttribute
    return false;
}

static uint32_t verifier_nested_to_method_access_attribute(uint32_t visiblity) {
    switch (visiblity) {
        case TDN_TYPE_VISIBILITY_NESTED_ASSEMBLY: return TDN_ACCESS_ASSEMBLY;
        case TDN_TYPE_VISIBILITY_NESTED_FAMILY_AND_ASSEMBLY: return TDN_ACCESS_FAMILY_AND_ASSEMBLY;
        case TDN_TYPE_VISIBILITY_NESTED_FAMILY: return TDN_ACCESS_FAMILY;
        case TDN_TYPE_VISIBILITY_NESTED_FAMILY_OR_ASSEMBLY: return TDN_ACCESS_FAMILY_OR_ASSEMBLY;
        case TDN_TYPE_VISIBILITY_NESTED_PRIVATE: return TDN_ACCESS_PRIVATE;
        case TDN_TYPE_VISIBILITY_NESTED_PUBLIC: return TDN_ACCESS_PUBLIC;
        default: ASSERT(false, "Invalid visibility: %d", visiblity);
    }
}

static bool verifier_can_access_type(RuntimeTypeInfo current_type, RuntimeTypeInfo target_type);

static bool verifier_can_access_instantiation(RuntimeTypeInfo current_type, RuntimeTypeInfo_Array instantiation) {
    for (int i = 0; i < instantiation->Length; i++) {
        RuntimeTypeInfo inst = instantiation->Elements[i];
        if (!verifier_can_access_type(current_type, inst)) {
            return false;
        }
    }
    return true;
}

static bool verifier_can_access_type(RuntimeTypeInfo current_type, RuntimeTypeInfo target_type) {
    if (target_type->IsGenericParameter) {
        return true;
    }

    // ensure we can access the generic parameters of the type
    if (target_type->GenericArguments != NULL && !verifier_can_access_instantiation(current_type, target_type->GenericArguments)) {
        return false;
    }

    if (target_type->DeclaringType == NULL) {
        // a non-nested class can be either all public or accessible only
        // from its own assembly (and friends)
        if ((target_type->Attributes.Visibility & TDN_TYPE_VISIBILITY_PUBLIC) != 0) {
            return true;

        } else {
            return current_type->Module == target_type->Module ||
                verifier_grants_friend_access_to(target_type->Module, current_type->Module);
        }
    }

    // Target class is nested
    uint32_t visiblity = verifier_nested_to_method_access_attribute(target_type->Attributes.Visibility);
    return verifier_can_access_member(current_type, target_type->DeclaringType, visiblity, NULL);
}

bool verifier_can_cast_to(RuntimeTypeInfo this_type, RuntimeTypeInfo other_type);

static bool verifier_can_access_family(RuntimeTypeInfo current_type, RuntimeTypeInfo target_type, RuntimeTypeInfo instance) {
    if (instance->IsGenericParameter) {
        return verifier_can_cast_to(instance, target_type);
    }

    // Iterate through all containing types of instance
    while (instance != NULL) {
        RuntimeTypeInfo cur_inst_type_def = instance;
        RuntimeTypeInfo current_type_def = current_type;

        // Iterate through all super types of current instance type
        while (cur_inst_type_def != NULL) {
            if (current_type_def == cur_inst_type_def) {
                // At this point we know that the instance type is able
                // to access the same family fields as current type
                // Now iterate through all super types of current type to see
                // if current type can access family target type
                while (current_type_def != NULL) {
                    if (current_type_def == target_type) {
                        return true;
                    }
                    current_type_def = current_type_def->BaseType;
                }
            }

            cur_inst_type_def = cur_inst_type_def->BaseType;
        }

        instance = instance->DeclaringType;
    }

    return false;
}

static bool verifier_can_access_member(RuntimeTypeInfo current_type, RuntimeTypeInfo target_type, uint32_t member_visibility, RuntimeTypeInfo instance) {
    if (instance == NULL) {
        instance = current_type;
    }

    // Check accesss to class defining member
    if (!verifier_can_access_type(current_type, target_type)) {
        return false;
    }

    // Anyone can access a public member
    if (member_visibility == TDN_ACCESS_PUBLIC) {
        return true;
    }

    // Required to be in the same module
    if (member_visibility == TDN_ACCESS_PRIVATE_SCOPE) {
        return current_type->Module == target_type->Module;
    }

    // required to be in the same assembly
    if (member_visibility == TDN_ACCESS_ASSEMBLY) {
        return current_type->Module == target_type->Module ||
            verifier_grants_friend_access_to(target_type->Module, current_type->Module);
    }

    if (member_visibility == TDN_ACCESS_FAMILY_AND_ASSEMBLY) {
        if (
            current_type->Module != target_type->Module &&
            !verifier_grants_friend_access_to(target_type->Module, current_type->Module)
        ) {
            return false;
        }
    }

    do {
        // Classes have access to all of their own members
        if (current_type == target_type) {
            return true;
        }

        switch (member_visibility) {
            case TDN_ACCESS_FAMILY_OR_ASSEMBLY: {
                if (
                    current_type->Module == target_type->Module ||
                    verifier_grants_friend_access_to(target_type->Module, current_type->Module)
                ) {
                    return true;
                }

                // Check if current class is subclass of target
                if (verifier_can_access_family(current_type, target_type, instance)) {
                    return true;
                }
            } break;

            case TDN_ACCESS_FAMILY:
            case TDN_ACCESS_FAMILY_AND_ASSEMBLY: {
                if (verifier_can_access_family(current_type, target_type, instance)) {
                    return true;
                }
            } break;

            // already handled by loop
            case TDN_ACCESS_PRIVATE:
                break;

            // should not reach here
            default:
                ASSERT(!"Invalid member access type");
        }

        // Nested classes can access all members of their parent class
        current_type = current_type->DeclaringType;
    } while (current_type != NULL);

    return false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Stack slot merging
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

static RuntimeTypeInfo verifier_merge_object_references(RuntimeTypeInfo class_a, RuntimeTypeInfo class_b) {
    if (class_a == class_b) {
        return class_a;
    }

    if (jit_is_interface(class_b)) {
        if (jit_is_interface(class_a)) {
            return verifier_merge_interface_with_interface(class_a, class_b);
        } else {
            return verifier_merge_class_with_interface(class_a, class_b);
        }
    } else if (jit_is_interface(class_a)) {
        return verifier_merge_class_with_interface(class_b, class_a);
    } else {
        return verifier_merge_class_with_class(class_a, class_b);
    }
}

static bool verifier_merge_flags(jit_value_flags_t* previous, jit_value_flags_t new) {
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

static merge_status_t verifier_merge_stack_values(jit_stack_value_t* previous, jit_stack_value_t* new) {
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
        if (jit_is_null_reference(previous)) {
            if (new->kind == JIT_KIND_OBJ_REF) {
                // we had a null-reference, but now we have a real type
                previous->type = new->type;
                modified = true;
            }
        } else if (previous->kind == JIT_KIND_OBJ_REF) {
            // must also be an object reference
            if (new->kind != JIT_KIND_OBJ_REF) {
                return MERGE_FAILED;
            }

            if (!jit_is_null_reference(new)) {
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

static bool verifier_merge_block_local(jit_block_local_t* previous, jit_block_local_t* new) {
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

static bool verifier_is_direct_child_region(jit_function_t* function, jit_basic_block_t* enclosing_block, jit_basic_block_t* enclosed_block) {
    RuntimeExceptionHandlingClause enclosed_region = enclosed_block->try_clause;
    for (int i = enclosed_region->TryOffset - 1; i > (long)enclosing_block->start; i--) {
        int idx = hmgeti(function->labels, i);
        if (idx < 0) {
            continue;
        }
        jit_basic_block_t* block = &function->labels[idx].value;

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

// TODO: use this when we verify the leave targets
__attribute__((unused))
static bool verifier_is_disjoint_try_block(jit_function_t* function, RuntimeExceptionHandlingClause disjoint, RuntimeExceptionHandlingClause source) {
    if (source->TryOffset <= disjoint->TryOffset && source->TryOffset + source->TryLength >= disjoint->TryOffset + disjoint->TryLength) {
        // Source is enclosing disjoint
        return false;
    }

    for (int i = disjoint->TryOffset - 1; i >= 0; i--) {
        int idx = hmgeti(function->labels, i);
        if (idx < 0) {
            continue;
        }
        jit_basic_block_t* block = &function->labels[idx].value;

        if (block->try_start) {
            RuntimeExceptionHandlingClause block_region = block->try_clause;
            if (
                (block_region->TryOffset + block_region->TryLength > disjoint->TryOffset) &&
                (block_region->TryOffset > source->TryOffset || block_region->TryOffset + block_region->TryLength <= source->TryOffset)
            ) {
                return false;
            }
        }

        if (block->handler_start) {
            RuntimeExceptionHandlingClause block_region = block->handler_clause;
            if (
                (block_region->HandlerOffset + block_region->HandlerLength > disjoint->TryOffset) &&
                (block_region->HandlerOffset > source->TryOffset || block_region->HandlerOffset + block_region->HandlerLength <= source->TryOffset)
            ) {
                return false;
            }
        }

        if (block->filter_start) {
            RuntimeExceptionHandlingClause block_region = block->filter_clause;
            uint32_t filter_length = block_region->HandlerOffset - block_region->FilterOffset;
            if (
                (block_region->FilterOffset + filter_length > disjoint->TryOffset) &&
                (block_region->FilterOffset > source->TryOffset || block_region->FilterOffset + filter_length <= source->TryOffset)
            ) {
                return false;
            }
        }
    }

    return true;
}

static tdn_err_t verifier_is_valid_branch_target(
    jit_function_t* function,
    jit_basic_block_t* src, jit_basic_block_t* target,
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

static tdn_err_t verifier_merge_blocks(jit_function_t* function, jit_block_t* from, jit_block_t* target) {
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
        }

        for (int i = 0; i < arrlen(from->locals); i++) {
            if (verifier_merge_block_local(&target->locals[i], &from->locals[i])) {
                modified = true;
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
                modified = true;
            }
        }

        // if we are jumping to a block with this initialized, but its
        // not initialized in our block, then mark it as not initialized
        if (target->this_initialized && !from->this_initialized) {
            target->this_initialized = false;
            modified = true;
        }

        // the metadata of the block was modified, we must
        if (modified) {
            // blocks should not be modified while emitting
            CHECK(!function->emitting);
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

        // pass over the initialized mark
        target->this_initialized = from->this_initialized;

        verifier_queue_block(function, target);
    }

cleanup:
    return err;
}

static tdn_err_t verifier_propagate_control_flow(jit_function_t* function, jit_block_t* from, jit_block_t* target, bool is_fallthrough) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure we can perform the branch
    CHECK_AND_RETHROW(verifier_is_valid_branch_target(function, &from->block, &target->block, is_fallthrough));
    CHECK_AND_RETHROW(verifier_merge_blocks(function, from, target));

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Verifiers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define STACK_PUSH() \
    ({ \
        CHECK_ERROR(arrlen(block->stack) < function->method->MethodBody->MaxStackSize, TDN_ERROR_VERIFIER_STACK_OVERFLOW); \
        jit_stack_value_t* __value = arraddnptr(block->stack, 1); \
        memset(__value, 0, sizeof(*__value)); \
        __value; \
    })

#define CHECK_INIT_THIS(value) \
    do { \
        jit_stack_value_t* __value = value; \
        CHECK_ERROR(!function->track_ctor_state || !__value->flags.this_ptr || block->this_initialized, \
            TDN_ERROR_VERIFIER_UNINIT_STACK); \
    } while (0)

static bool verifier_can_cast_to_class_or_interface(RuntimeTypeInfo this_type, RuntimeTypeInfo other_type) {
    // TODO: variance support

    if (jit_is_interface(other_type)) {
        if (hmgeti(this_type->InterfaceImpls, other_type) >= 0) {
            return true;
        }
    } else {
        if (jit_is_interface(this_type) && other_type == tObject) {
            return true;
        }

        if (tdn_type_is_nullable(other_type) && !tdn_type_is_nullable(this_type)) {
            return verifier_can_cast_to(this_type, other_type->GenericArguments->Elements[0]);
        }

        do {
            if (this_type == other_type) {
                return true;
            }
            this_type = this_type->BaseType;
        } while (this_type != NULL);
    }

    return false;
}

static RuntimeTypeInfo verifier_get_underlying_type(RuntimeTypeInfo type) {
    if (type->BaseType != tEnum) {
        return type;
    }
    return type->EnumUnderlyingType;
}

static RuntimeTypeInfo verifier_get_integral_element_type(RuntimeTypeInfo type) {
    if (type == tByte) return tSByte;
    if (type == tUInt16) return tInt16;
    if (type == tUInt32) return tInt32;
    if (type == tUInt64) return tInt64;
    if (type == tUIntPtr) return tIntPtr;
    return type;
}

static bool verifier_can_cast_param_to(RuntimeTypeInfo this_type, RuntimeTypeInfo param_type) {
    if (this_type == param_type) {
        return true;
    }

    // check that the type can be casted if its a reference type
    if (tdn_type_is_referencetype(this_type)) {
        return verifier_can_cast_to(this_type, param_type);
    }

    // handle the signed/unsigned variance in here
    RuntimeTypeInfo this_underlying = verifier_get_underlying_type(this_type);
    RuntimeTypeInfo param_underlying = verifier_get_underlying_type(param_type);
    return verifier_get_integral_element_type(this_underlying) == verifier_get_integral_element_type(param_underlying);
}

bool verifier_can_cast_to(RuntimeTypeInfo this_type, RuntimeTypeInfo other_type) {
    if (this_type == other_type) {
        return true;
    }

    // special case for byref and pointer
    if (
        (this_type->IsByRef && other_type->IsByRef) ||
        (this_type->IsPointer && other_type->IsPointer)
    ) {
        return verifier_can_cast_param_to(this_type->ElementType, other_type->ElementType);
    }

    return verifier_can_cast_to_class_or_interface(this_type, other_type);
}

static bool verifier_is_assignable(jit_stack_value_t* src, jit_stack_value_t* dst) {
    // can't store readonly to a non-readonly location
    if (src->flags.ref_read_only && !dst->flags.ref_read_only) {
        return false;
    }

    // can't store a local to a non-local location
    if (!src->flags.ref_non_local && dst->flags.ref_non_local) {
        return false;
    }

    // same type, we good
    if (src->kind == dst->kind && src->type == dst->type) {
        return true;
    }

    // can't store into a null value
    if (dst->type == NULL) {
        return false;
    }

    switch (src->kind) {
        // check if can be casted
        case JIT_KIND_OBJ_REF: {
            if (dst->kind != JIT_KIND_OBJ_REF) {
                return false;
            }

            // Mull is always assignable
            if (src->type == NULL) {
                return true;
            }

            // and now check if we can perform the cast
            return verifier_can_cast_to(src->type, dst->type);
        } break;

        // must match exactly
        case JIT_KIND_INT64:
        case JIT_KIND_FLOAT:
        case JIT_KIND_VALUE_TYPE:
            return false;

        case JIT_KIND_BY_REF: {
            if (dst->kind == JIT_KIND_BY_REF && dst->flags.ref_read_only) {
                return src->type == dst->type;
            }

            return false;
        } break;

        // can either be int64 or native int
        case JIT_KIND_INT32:
            return dst->kind == JIT_KIND_INT64 || dst->kind == JIT_KIND_NATIVE_INT;

        // can also be int64
        case JIT_KIND_NATIVE_INT:
            return dst->kind == JIT_KIND_INT64;

        default:
            ASSERT(!"Invalid kind", "%d of %T", src->kind, src->type);
    }
}

static RuntimeTypeInfo verifier_get_reduced_type(RuntimeTypeInfo type) {
    if (type == NULL) {
        return NULL;
    }

    type = verifier_get_underlying_type(type);
    if (type == tByte) {
        return tSByte;
    } else if (type == tUInt16) {
        return tInt16;
    } else if (type == tUInt32) {
        return tInt32;
    } else if (type == tUInt64) {
        return tInt64;
    } else if (type == tUIntPtr) {
        return tIntPtr;
    } else {
        return type;
    }
}

static RuntimeTypeInfo verifier_get_verification_type(RuntimeTypeInfo type) {
    if (type == NULL) {
        return NULL;
    }

    if (type->IsByRef) {
        type = verifier_get_verification_type(type->ElementType);
        ASSERT(tdn_get_byref_type(type, &type));
        return type;
    } else {
        RuntimeTypeInfo reduced_type = verifier_get_reduced_type(type);
        if (reduced_type == tBoolean) {
            return tSByte;
        } else if (reduced_type == tChar) {
            return tInt16;
        } else {
            return reduced_type;
        }
    }
}

static const char* jit_kind_str(jit_stack_value_kind_t kind) {
    switch (kind) {
        case JIT_KIND_INT32: return "int32";
        case JIT_KIND_INT64: return "int64";
        case JIT_KIND_NATIVE_INT: return "native int";
        case JIT_KIND_FLOAT: return "float";
        case JIT_KIND_BY_REF: return "by-ref";
        case JIT_KIND_OBJ_REF: return "obj-ref";
        case JIT_KIND_VALUE_TYPE: return "value";
        default: return "?";
    }
}

#define CHECK_IS_ASSIGNABLE(a, b) \
    do { \
        jit_stack_value_t* __a = a; \
        jit_stack_value_t* __b = b; \
        if (!verifier_is_unsafe_assignable(function, __a, __b)) { \
            CHECK_ERROR(verifier_is_assignable(__a, __b), \
                TDN_ERROR_VERIFIER_STACK_UNEXPECTED, \
                "%T [%s] is-assignable-to %T [%s]", __a->type, jit_kind_str(__a->kind), __b->type, jit_kind_str(__b->kind)); \
        } \
    } while (0)

static bool verifier_is_unsafe_assignable(jit_function_t* function, jit_stack_value_t* a, jit_stack_value_t* b) {
    if (!function->method->Module->Assembly->AllowUnsafe) {
        return false;
    }

    // convert by-ref and native int as much as they want
    if (a->kind == JIT_KIND_BY_REF && b->kind == JIT_KIND_NATIVE_INT) {
        return true;
    } else if (a->kind == JIT_KIND_NATIVE_INT && b->kind == JIT_KIND_BY_REF) {
        return true;
    }

    return false;
}

static bool verifier_is_same_reduced_type(RuntimeTypeInfo src, RuntimeTypeInfo dst) {
    return verifier_get_reduced_type(src) == verifier_get_reduced_type(dst);
}

static bool verifier_is_assignable_type(RuntimeTypeInfo src, RuntimeTypeInfo dst, bool allow_size_equivalence) {
    if (src == dst) {
        return true;
    }

    if (tdn_type_is_valuetype(src) || tdn_type_is_valuetype(dst)) {
        if (allow_size_equivalence && verifier_is_same_reduced_type(src, dst)) {
            return true;
        }

        return false;
    }

    return verifier_can_cast_to(src, dst);
}

#define CHECK_IS_ASSIGNABLE_TYPE(a, b) \
    do { \
        RuntimeTypeInfo __a = a; \
        RuntimeTypeInfo __b = b; \
        CHECK_ERROR(verifier_is_assignable_type(__a, __b, false), \
            TDN_ERROR_VERIFIER_STACK_UNEXPECTED, \
            "%T is-assignable-to %T", __a, __b); \
    } while (0)

#define CHECK_IS_ARRAY_ELEMENT_COMPATIBLE_WITH(a, b) \
    do { \
        RuntimeTypeInfo __a = a; \
        RuntimeTypeInfo __b = b; \
        CHECK_ERROR(verifier_is_assignable_type(__a, __b, true), \
            TDN_ERROR_VERIFIER_STACK_UNEXPECTED, \
            "%T is-assignable-to %T", __a, __b); \
    } while (0)

//----------------------------------------------------------------------------------------------------------------------
// Misc instructions
//----------------------------------------------------------------------------------------------------------------------

// Use as a template for adding new instructions
static tdn_err_t verify_nop(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

cleanup:
    return err;
}

static tdn_err_t verify_sizeof(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), tInt32);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Local access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_load_local(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_block_local_t* block_locals = is_arg ? block->args : block->locals;
    jit_local_t* function_locals = is_arg ? function->args : function->locals;

    // get the local slot
    CHECK_ERROR(inst->operand.variable < arrlen(block_locals),
        is_arg ? TDN_ERROR_VERIFIER_UNRECOGNIZED_ARGUMENT_NUMBER : TDN_ERROR_VERIFIER_UNRECOGNIZED_LOCAL_NUMBER);
    jit_block_local_t* block_local = &block_locals[inst->operand.variable];
    jit_local_t* func_local = &function_locals[inst->operand.variable];

    // check if we need an initializer
    if (!block_local->initialized) {
        func_local->zero_initialize = true;
        block_local->initialized = true;

        // a zero initialized ref-struct only contains null-refs
        // which are considered to be non-local
        if (func_local->type->IsByRefStruct) {
            block_local->flags.ref_struct_non_local = true;
        }
    }

    // we don't have a valid this, enforce an invalid this
    if (!function->valid_this) {
        block_local->flags.this_ptr = false;
    }

    // push the new type to the stack, copy the flags and method pointers
    jit_stack_value_t* value = jit_stack_value_init(STACK_PUSH(), func_local->type);
    value->flags = block_local->flags;
    value->method = block_local->method;

cleanup:
    return err;
}

static tdn_err_t verify_store_local(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_INIT_THIS(stack);

    jit_block_local_t* block_locals = is_arg ? block->args : block->locals;
    jit_local_t* function_locals = is_arg ? function->args : function->locals;

    // get the local slot
    CHECK_ERROR(inst->operand.variable < arrlen(block_locals),
        is_arg ? TDN_ERROR_VERIFIER_UNRECOGNIZED_ARGUMENT_NUMBER : TDN_ERROR_VERIFIER_UNRECOGNIZED_LOCAL_NUMBER);
    jit_block_local_t* block_local = &block_locals[inst->operand.variable];
    jit_local_t* func_local = &function_locals[inst->operand.variable];

    // initialized, no need to zero initialize later on
    block_local->initialized = true;

    // taking the address of variable, invalid this
    // because the emit has a verification pass as well, it
    // will catch anything that we missed until now
    if (is_arg && inst->operand.variable == 0) {
        function->valid_this = false;
    }

    // don't allow to load the this if we are not ready yet
    if (is_arg && function->track_ctor_state && !block->this_initialized) {
        CHECK_ERROR(inst->operand.variable != 0, TDN_ERROR_VERIFIER_THIS_UNINIT_STORE);
    }

    // init the type location, inherit the flags
    // from the incoming value
    jit_stack_value_t local_value = {
        .flags = stack->flags
    };
    jit_stack_value_init(&local_value, func_local->type);
    CHECK_IS_ASSIGNABLE(stack, &local_value);

    // remember the flags of the local
    block_local->flags = stack->flags;

cleanup:
    return err;
}

static tdn_err_t verify_load_local_address(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_block_local_t* block_locals = is_arg ? block->args : block->locals;
    jit_local_t* function_locals = is_arg ? function->args : function->locals;

    // get the local slot
    CHECK_ERROR(inst->operand.variable < arrlen(block_locals),
        is_arg ? TDN_ERROR_VERIFIER_UNRECOGNIZED_ARGUMENT_NUMBER : TDN_ERROR_VERIFIER_UNRECOGNIZED_LOCAL_NUMBER);
    jit_block_local_t* block_local = &block_locals[inst->operand.variable];
    jit_local_t* func_local = &function_locals[inst->operand.variable];

    // don't allow to have nested byrefs
    CHECK_ERROR(!func_local->type->IsByRef, TDN_ERROR_VERIFIER_BYYREF_OF_BYREF);

    // check if we need an initializer
    // TODO: when we do location tracking have some lazy
    //       check for the zero initialzie (since ldloca -> initobj
    //       is a pretty common pattern)
    if (!block_local->initialized) {
        func_local->zero_initialize = true;
        block_local->initialized = true;

        // a zero initialized ref-struct only contains null-refs
        // which are considered to be non-local
        if (func_local->type->IsByRefStruct) {
            block_local->flags.ref_struct_non_local = true;
        }
    }

    // local taken by reference must be spilled
    func_local->spilled = true;

    // taking the address of variable, invalid this
    if (is_arg && inst->operand.variable == 0) {
        function->valid_this = false;
    }

    // don't allow to load the this if we are not ready yet
    if (is_arg && function->track_ctor_state && !block->this_initialized) {
        CHECK_ERROR(inst->operand.variable != 0, TDN_ERROR_VERIFIER_THIS_UNINIT_STORE);
    }

    // setup the new stack value
    jit_stack_value_t* value = STACK_PUSH();
    value->kind = JIT_KIND_BY_REF;
    value->type = func_local->type;
    value->flags = block_local->flags;

cleanup:
    return err;
}

static tdn_err_t verify_ldarg(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return verify_load_local(function, block, inst, true);
}

static tdn_err_t verify_starg(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return verify_store_local(function, block, inst, stack, true);
}

static tdn_err_t verify_ldarga(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return verify_load_local_address(function, block, inst, true);
}

static tdn_err_t verify_ldloc(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return verify_load_local(function, block, inst, false);
}

static tdn_err_t verify_stloc(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return verify_store_local(function, block, inst, stack, false);
}

static tdn_err_t verify_ldloca(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return verify_load_local_address(function, block, inst, false);
}

//----------------------------------------------------------------------------------------------------------------------
// Misc instructions
//----------------------------------------------------------------------------------------------------------------------

static bool verifier_can_access_field(RuntimeTypeInfo current_type, RuntimeFieldInfo field, RuntimeTypeInfo instance) {
    // check the access to the member field
    if (!verifier_can_access_member(current_type, field->DeclaringType, field->Attributes.FieldAccess, instance)) {
        return false;
    }

    // check access to the field type itself
    return verifier_can_access_type(current_type, field->FieldType);
}

static tdn_err_t verify_ldfld(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeFieldInfo field = inst->operand.field;

    jit_value_flags_t flags = {};
    RuntimeTypeInfo instance = NULL;
    if (inst->opcode == CEE_LDSFLD) {
        CHECK_ERROR(field->Attributes.Static, TDN_ERROR_VERIFIER_EXPECTED_STATIC_FIELD);
    } else {
        RuntimeTypeInfo owning_type = field->DeclaringType;

        // treat as a by-ref if its a value type
        if (stack->kind == JIT_KIND_VALUE_TYPE) {
            stack->kind = JIT_KIND_BY_REF;
        }

        // setup the load type, for value types its a readonly
        // reference (since we are not modifying it in the load)
        jit_stack_value_t declared_this = {};
        if (tdn_type_is_valuetype(owning_type)) {
            declared_this.kind = JIT_KIND_BY_REF;
            declared_this.type = owning_type;
            declared_this.flags.ref_read_only = true;
        } else {
            declared_this.kind = JIT_KIND_OBJ_REF;
            declared_this.type = owning_type;
        }

        // ensure that the this can be loaded properly
        if (stack->kind == JIT_KIND_NATIVE_INT) {
            CHECK(function->method->Module->Assembly->AllowUnsafe);
        } else {
            CHECK_IS_ASSIGNABLE(stack, &declared_this);
        }

        // if we are loading a ref field, and the ref-struct only contains non-local
        // references, then we are going to mark the reference as non-local
        if (field->FieldType->IsByRef && stack->flags.ref_struct_non_local) {
            flags.ref_non_local = true;
        }

        // the instance we are loading from
        instance = stack->type;
    }

    // if we are loading a readonly reference field
    // then mark it as such
    if (field->FieldType->IsByRef && field->ReferenceIsReadOnly) {
        flags.ref_read_only = true;
    }

    // check that we can access it
    CHECK_ERROR(verifier_can_access_field(function->method->DeclaringType, field, instance),
        TDN_ERROR_VERIFIER_FIELD_ACCESS);

    // push the value to the stack
    jit_stack_value_init(STACK_PUSH(), field->FieldType)->flags = flags;

cleanup:
    return err;
}

static tdn_err_t verify_ldflda(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeFieldInfo field = inst->operand.field;

    bool is_cctor = tdn_compare_string_to_cstr(function->method->Name, ".cctor") &&
            field->DeclaringType == function->method->DeclaringType;

    bool is_ctor = tdn_compare_string_to_cstr(function->method->Name, ".ctor") &&
            field->DeclaringType == function->method->DeclaringType;

    // TODO: treat IsExternalInit like is_ctor

    RuntimeTypeInfo instance = NULL;
    jit_value_flags_t flags = {};
    if (inst->opcode == CEE_LDSFLDA) {
        CHECK_ERROR(field->Attributes.Static, TDN_ERROR_VERIFIER_EXPECTED_STATIC_FIELD);
        flags.ref_non_local = true;

    } else {
        RuntimeTypeInfo owning_type = field->DeclaringType;

        // treat as a by-ref if its a value type
        if (stack->kind == JIT_KIND_VALUE_TYPE) {
            stack->kind = JIT_KIND_BY_REF;
        }

        // setup the load type, for value types its a readonly
        // reference (since we are not modifying it in the load)
        jit_stack_value_t declared_this = {};
        if (tdn_type_is_valuetype(owning_type)) {
            declared_this.kind = JIT_KIND_BY_REF;
            declared_this.type = owning_type;
            declared_this.flags.ref_read_only = true;
        } else {
            declared_this.kind = JIT_KIND_OBJ_REF;
            declared_this.type = owning_type;
        }

        // ensure that the this can be loaded properly
        CHECK_IS_ASSIGNABLE(stack, &declared_this);

        // if the reference is non-local or its an object reference then the new
        // reference is also going to be
        if (stack->flags.ref_non_local || stack->kind == JIT_KIND_OBJ_REF) {
            flags.ref_non_local = true;
        }

        // the instance we are loading from
        instance = stack->type;
    }

    // init only fields are treated as readonly references
    // unless its in the cctor/ctor itself
    if (field->Attributes.InitOnly) {
        if (field->Attributes.Static) {
            if (!is_cctor) {
                flags.ref_read_only = true;
            }
        } else if (!is_ctor) {
            flags.ref_read_only = true;
        }
    }

    // check that we can access it
    CHECK_ERROR(verifier_can_access_field(function->method->DeclaringType, field, instance),
        TDN_ERROR_VERIFIER_FIELD_ACCESS);

    // push the value to the stack
    jit_stack_value_t* value = STACK_PUSH();
    value->kind = JIT_KIND_BY_REF;
    value->type = field->FieldType;
    value->flags = flags;

cleanup:
    return err;
}

static tdn_err_t verify_stfld(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeFieldInfo field = inst->operand.field;

    bool is_cctor = function->method->Attributes.RTSpecialName &&
        tdn_compare_string_to_cstr(function->method->Name, ".cctor") &&
            field->DeclaringType == function->method->DeclaringType;

    bool is_ctor = function->method->Attributes.RTSpecialName &&
        tdn_compare_string_to_cstr(function->method->Name, ".ctor") &&
            field->DeclaringType == function->method->DeclaringType;

    // TODO: treat IsExternalInit like is_ctor

    RuntimeTypeInfo instance = NULL;
    jit_stack_value_t* value = NULL;
    if (inst->opcode == CEE_STSFLD) {
        value = &stack[0];
        CHECK_INIT_THIS(value);

        CHECK_ERROR(field->Attributes.Static, TDN_ERROR_VERIFIER_EXPECTED_STATIC_FIELD);

    } else {
        value = &stack[1];
        jit_stack_value_t* actual_this = &stack[0];
        CHECK_INIT_THIS(value);

        RuntimeTypeInfo owning_type = field->DeclaringType;

        // treat as a by-ref if its a value type
        if (actual_this->kind == JIT_KIND_VALUE_TYPE) {
            actual_this->kind = JIT_KIND_BY_REF;
        }

        // setup the load type
        jit_stack_value_t declared_this = {};
        if (tdn_type_is_valuetype(owning_type)) {
            declared_this.kind = JIT_KIND_BY_REF;
            declared_this.type = owning_type;
        } else {
            declared_this.kind = JIT_KIND_OBJ_REF;
            declared_this.type = owning_type;
        }

        // ensure that the this can be loaded properly
        CHECK_IS_ASSIGNABLE(actual_this, &declared_this);

        // the instance we are loading from
        instance = actual_this->type;
    }

    // only the ctor/cctor can write to the init only fields
    if (field->Attributes.InitOnly) {
        if (field->Attributes.Static) {
            CHECK_ERROR(is_cctor, TDN_ERROR_VERIFIER_INIT_ONLY);
        } else {
            CHECK_ERROR(is_ctor, TDN_ERROR_VERIFIER_INIT_ONLY);
        }
    }

    // check that we can access it
    CHECK_ERROR(verifier_can_access_field(function->method->DeclaringType, field, instance),
        TDN_ERROR_VERIFIER_FIELD_ACCESS);

    // setup the value for storing
    // TODO: non-local reference in a non-local ref-struct
    jit_stack_value_t field_value = {
        .flags = {
            .ref_read_only = field->ReferenceIsReadOnly
        }
    };
    jit_stack_value_init(&field_value, field->FieldType);
    CHECK_IS_ASSIGNABLE(value, &field_value);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Reference access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_initobj(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // must be a byref
    CHECK_ERROR(stack->kind == JIT_KIND_BY_REF,
        TDN_ERROR_VERIFIER_STACK_BY_REF);

    // and check it matches properly
    jit_stack_value_t value = {};
    value.type = inst->operand.type;
    value.kind = JIT_KIND_BY_REF;
    CHECK_IS_ASSIGNABLE(stack, &value);

cleanup:
    return err;
}

static tdn_err_t verify_ldind(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_INIT_THIS(stack);

    if (function->method->Module->Assembly->AllowUnsafe) {
        CHECK(stack->kind == JIT_KIND_BY_REF || stack->kind == JIT_KIND_NATIVE_INT);
    } else {
        CHECK(stack->kind == JIT_KIND_BY_REF);

        if (inst->operand.type == NULL) {
            CHECK(tdn_type_is_referencetype(stack->type));
            inst->operand.type = stack->type;
        } else {
            CHECK_IS_ASSIGNABLE_TYPE(
                verifier_get_verification_type(stack->type),
                verifier_get_verification_type(inst->operand.type));
        }
    }

    jit_stack_value_init(STACK_PUSH(), inst->operand.type);

cleanup:
    return err;
}

static tdn_err_t verify_stind(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_t* value = &stack[1];
    jit_stack_value_t* address = &stack[0];

    CHECK_INIT_THIS(value);

    // must be writable to store
    CHECK_ERROR(!address->flags.ref_read_only,
        TDN_ERROR_VERIFIER_READONLY_ILLEGAL_WRITE);

    // must be a byref
    if (function->method->Module->Assembly->AllowUnsafe) {
        CHECK(stack->kind == JIT_KIND_BY_REF || stack->kind == JIT_KIND_NATIVE_INT);
    } else {
        CHECK(stack->kind == JIT_KIND_BY_REF);
    }

    // resolve the type if stind.ref
    if (inst->operand.type == NULL) {
        inst->operand.type = address->type;
    }

    jit_stack_value_t type_val = jit_stack_value_create(inst->operand.type);
    jit_stack_value_t address_val = jit_stack_value_create(address->type);

    CHECK_IS_ASSIGNABLE(&type_val, &address_val);
    CHECK_IS_ASSIGNABLE(value, &type_val);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Stack manipulation
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_ldnull(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_t* value = STACK_PUSH();
    value->kind = JIT_KIND_OBJ_REF;
    value->type = NULL;

cleanup:
    return err;
}

static tdn_err_t verify_ldstr(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), tString);

cleanup:
    return err;
}

static tdn_err_t verify_ldtoken(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeTypeInfo type = NULL;
    switch (inst->operand_type) {
        case TDN_IL_TYPE: type = tRuntimeTypeHandle; break;
        case TDN_IL_FIELD: type = tRuntimeFieldHandle; break;
        case TDN_IL_METHOD: type = tRuntimeMethodHandle; break;
        default: CHECK_FAIL();
    }
    jit_stack_value_init(STACK_PUSH(), type);

cleanup:
    return err;
}

static tdn_err_t verify_ldc_i4(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), tInt32);

cleanup:
    return err;
}

static tdn_err_t verify_ldc_i8(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), tInt64);

cleanup:
    return err;
}

static tdn_err_t verify_ldc_r4(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), tSingle);

cleanup:
    return err;
}

static tdn_err_t verify_ldc_r8(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_init(STACK_PUSH(), tDouble);

cleanup:
    return err;
}

static tdn_err_t verify_dup(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    *STACK_PUSH() = *stack;
    *STACK_PUSH() = *stack;

cleanup:
    return err;
}

static tdn_err_t verify_pop(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    // TODO: allow uninit this
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// Arith and compare operations
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_binary_op(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // check that we got the expected item
    switch (inst->opcode) {
        case CEE_ADD:
        case CEE_SUB:
        case CEE_MUL:
        case CEE_DIV:
        case CEE_REM: {
            CHECK_ERROR(JIT_KIND_INT32 <= stack[0].kind && stack[0].kind <= JIT_KIND_FLOAT, TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE);
            CHECK_ERROR(JIT_KIND_INT32 <= stack[1].kind && stack[1].kind <= JIT_KIND_FLOAT, TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE);
        } break;

        default: {
            CHECK_ERROR(JIT_KIND_INT32 <= stack[0].kind && stack[0].kind <= JIT_KIND_NATIVE_INT, TDN_ERROR_VERIFIER_EXPECTED_INTEGER_TYPE);
            CHECK_ERROR(JIT_KIND_INT32 <= stack[1].kind && stack[1].kind <= JIT_KIND_NATIVE_INT, TDN_ERROR_VERIFIER_EXPECTED_INTEGER_TYPE);
        } break;
    }

    // Stack value kind is ordered to make this work
    jit_stack_value_t result = (stack[0].kind > stack[1].kind) ? stack[0] : stack[1];

    CHECK_ERROR((stack[0].kind == stack[1].kind) || (result.kind == JIT_KIND_NATIVE_INT),
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    *STACK_PUSH() = result;

cleanup:
    return err;
}

static tdn_err_t verify_unary_op(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure the input is correct
    if (inst->opcode == CEE_NEG) {
        CHECK_ERROR(JIT_KIND_INT32 <= stack->kind && stack->kind <= JIT_KIND_FLOAT,
            TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE);

    } else if (inst->opcode == CEE_NOT) {
        CHECK_ERROR(JIT_KIND_INT32 <= stack->kind && stack->kind <= JIT_KIND_NATIVE_INT,
            TDN_ERROR_VERIFIER_EXPECTED_INTEGER_TYPE);

    } else {
        CHECK_FAIL();
    }

    // same as input
    *STACK_PUSH() = *stack;

cleanup:
    return err;
}

static tdn_err_t verify_shift(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // the shift by should be int32 or native int
    CHECK_ERROR(stack[1].kind == JIT_KIND_INT32 || stack[1].kind == JIT_KIND_NATIVE_INT,
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    // to be shifted should be an integer
    CHECK_ERROR(JIT_KIND_INT32 <= stack[0].kind && stack[0].kind <= JIT_KIND_NATIVE_INT,
        TDN_ERROR_VERIFIER_EXPECTED_INTEGER_TYPE);

    // same as the to be shifted value
    *STACK_PUSH() = stack[0];

cleanup:
    return err;
}

static tdn_err_t verify_conv(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // validate that both are good, specifically for unsafe code allow to perform
    // the operation also on by-refs
    if (function->method->Module->Assembly->AllowUnsafe) {
        CHECK_ERROR(JIT_KIND_INT32 <= stack->kind && stack->kind <= JIT_KIND_BY_REF,
            TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE);
    } else {
        CHECK_ERROR(JIT_KIND_INT32 <= stack->kind && stack->kind <= JIT_KIND_FLOAT,
            TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE);
    }

    // always pushes as an int32
    RuntimeTypeInfo type = NULL;
    switch (inst->opcode) {
        case CEE_CONV_OVF_I1:
        case CEE_CONV_OVF_I2:
        case CEE_CONV_OVF_I4:
        case CEE_CONV_OVF_U1:
        case CEE_CONV_OVF_U2:
        case CEE_CONV_OVF_U4:
        case CEE_CONV_I1:
        case CEE_CONV_I2:
        case CEE_CONV_I4:
        case CEE_CONV_U1:
        case CEE_CONV_U2:
        case CEE_CONV_U4: type = tInt32; break;
        case CEE_CONV_OVF_I8:
        case CEE_CONV_OVF_U8:
        case CEE_CONV_I8:
        case CEE_CONV_U8: type = tInt64; break;
        case CEE_CONV_OVF_I:
        case CEE_CONV_OVF_U:
        case CEE_CONV_I:
        case CEE_CONV_U: type = tIntPtr; break;
        case CEE_CONV_R4: type = tSingle; break;
        case CEE_CONV_R_UN:
        case CEE_CONV_R8: type = tDouble; break;
        default: CHECK_FAIL();
    }

    jit_stack_value_init(STACK_PUSH(), type);

cleanup:
    return err;
}

static bool verifier_is_binary_comparable(jit_stack_value_t* a, jit_stack_value_t* b, tdn_il_opcode_t opcode) {
    if (a->kind == b->kind && a->type == b->type) {
        return true;
    }

    switch (a->kind) {
        case JIT_KIND_OBJ_REF: {
            if (b->kind != JIT_KIND_OBJ_REF) {
                return false;
            }
            return opcode == CEE_BEQ || opcode == CEE_BNE_UN || opcode == CEE_CEQ || opcode == CEE_CGT_UN;
        } break;

        case JIT_KIND_VALUE_TYPE:
            return false;

        case JIT_KIND_BY_REF: {
            if (b->kind == JIT_KIND_BY_REF) {
                return true;
            }

            if (b->kind == JIT_KIND_NATIVE_INT) {
                return opcode == CEE_BEQ || opcode == CEE_BNE_UN || opcode == CEE_CEQ;
            }

            return false;
        } break;

        case JIT_KIND_INT32:
            return b->kind == JIT_KIND_INT64 || b->kind == JIT_KIND_NATIVE_INT;

        case JIT_KIND_INT64:
            return b->kind == JIT_KIND_INT64;

        case JIT_KIND_NATIVE_INT: {
            if (b->kind == JIT_KIND_INT32 || b->kind == JIT_KIND_NATIVE_INT) {
                return true;
            }

            if (b->kind == JIT_KIND_BY_REF) {
                return opcode == CEE_BEQ || opcode == CEE_BNE_UN || opcode == CEE_CEQ;
            }

            return false;
        } break;

        case JIT_KIND_FLOAT:
            return b->kind == JIT_KIND_FLOAT;

        default:
            ASSERT(!"Invalid kind");
    }
}

static tdn_err_t verify_compare(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_ERROR(verifier_is_binary_comparable(&stack[0], &stack[1], inst->opcode),
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    // always pushed as int32
    jit_stack_value_init(STACK_PUSH(), tInt32);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Array related
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_newarr(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_t* length = &stack[0];
    CHECK_ERROR(
        length->kind == JIT_KIND_INT32 || length->kind == JIT_KIND_NATIVE_INT,
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    // can't create an array of by-refs
    CHECK_ERROR(!inst->operand.type->IsByRef,
        TDN_ERROR_VERIFIER_ARRAY_BY_REF);

    RuntimeTypeInfo array_type;
    CHECK_AND_RETHROW(tdn_get_array_type(inst->operand.type, &array_type));

    jit_stack_value_init(STACK_PUSH(), array_type);

cleanup:
    return err;
}

static tdn_err_t verify_ldlen(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(stack->type->IsArray);

    // return value is an intptr
    jit_stack_value_init(STACK_PUSH(), tIntPtr);

cleanup:
    return err;
}

static tdn_err_t verify_ldelem(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_t* index = &stack[1];
    jit_stack_value_t* array = &stack[0];

    CHECK_ERROR(index->kind == JIT_KIND_INT32 || index->kind == JIT_KIND_NATIVE_INT,
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    CHECK_ERROR(array->type->IsArray,
        TDN_ERROR_VERIFIER_EXPECTED_ARRAY);

    RuntimeTypeInfo element_type = inst->operand.type;
    RuntimeTypeInfo actual_element_type = array->type->ElementType;
    if (element_type != NULL) {
        CHECK_IS_ARRAY_ELEMENT_COMPATIBLE_WITH(
            verifier_get_verification_type(actual_element_type),
            element_type
        );
    } else {
        element_type = actual_element_type;
        CHECK(tdn_type_is_referencetype(element_type));
    }

    jit_stack_value_init(STACK_PUSH(), element_type);

cleanup:
    return err;
}

static tdn_err_t verify_stelem(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_t* value = &stack[2];
    jit_stack_value_t* index = &stack[1];
    jit_stack_value_t* array = &stack[0];

    CHECK_INIT_THIS(value);

    CHECK_ERROR(index->kind == JIT_KIND_INT32 || index->kind == JIT_KIND_NATIVE_INT,
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    CHECK_ERROR(array->type->IsArray,
        TDN_ERROR_VERIFIER_EXPECTED_ARRAY);

    RuntimeTypeInfo element_type = inst->operand.type;
    RuntimeTypeInfo actual_element_type = array->type->ElementType;
    if (element_type != NULL) {
        CHECK_IS_ARRAY_ELEMENT_COMPATIBLE_WITH(
            verifier_get_verification_type(actual_element_type),
            element_type
        );
    } else {
        element_type = actual_element_type;
        CHECK_ERROR(tdn_type_is_referencetype(element_type), TDN_ERROR_VERIFIER_STACK_OBJ_REF);
    }

    // ensure the value matches
    jit_stack_value_t element_val = jit_stack_value_create(element_type);
    CHECK_IS_ASSIGNABLE(value, &element_val);

cleanup:
    return err;
}

static tdn_err_t verify_ldelema(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_t* index = &stack[1];
    jit_stack_value_t* array = &stack[0];

    CHECK_ERROR(index->kind == JIT_KIND_INT32 || index->kind == JIT_KIND_NATIVE_INT,
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    CHECK_ERROR(array->type->IsArray,
        TDN_ERROR_VERIFIER_EXPECTED_ARRAY);

    RuntimeTypeInfo element_type = inst->operand.type;
    RuntimeTypeInfo actual_element_type = array->type->ElementType;
    CHECK_ERROR(
        actual_element_type == element_type ||
        verifier_is_same_reduced_type(actual_element_type, element_type),
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED_ARRAY_TYPE
    );

    jit_stack_value_t* push = STACK_PUSH();
    push->flags.ref_non_local = true;
    push->kind = JIT_KIND_BY_REF;
    push->type = element_type;
    if (inst->prefixes & TDN_IL_PREFIX_READONLY) {
        push->flags.ref_read_only = true;
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Method related
//----------------------------------------------------------------------------------------------------------------------

static bool verifier_can_access_method(RuntimeTypeInfo current_type, RuntimeMethodBase method, RuntimeTypeInfo instance) {
    if (method->GenericArguments != NULL && !verifier_can_access_instantiation(current_type, method->GenericArguments)) {
        return false;
    }

    if (!verifier_can_access_member(current_type, method->DeclaringType, method->Attributes.MemberAccess, instance)) {
        return false;
    }

    if (!verifier_can_access_type(current_type, method->ReturnParameter->ParameterType)) {
        return false;
    }

    for (int i = 0; i < method->Parameters->Length; i++) {
        if (!verifier_can_access_type(current_type, method->Parameters->Elements[i]->ParameterType)) {
            return false;
        }
    }

    return true;
}

static tdn_err_t verify_ldftn(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = inst->operand.method;


    // must not be a constructor
    CHECK_ERROR(!tdn_compare_string_to_cstr(inst->operand.method->Name, ".ctor"),
        TDN_ERROR_VERIFIER_LDFTN_CTOR);

    RuntimeTypeInfo instance = NULL;
    if (inst->opcode == CEE_LDVIRTFTN) {
        // TODO: devirt at this point already?
        CHECK(!method->Attributes.Static);

        // We want the boxed value for the comparison
        jit_stack_value_t declared_type = {
            .kind = JIT_KIND_OBJ_REF,
            .type = method->DeclaringType
        };

        jit_stack_value_t* this_ptr = &stack[0];
        CHECK_INIT_THIS(this_ptr);

        CHECK(this_ptr->kind == JIT_KIND_OBJ_REF);
        CHECK_IS_ASSIGNABLE(this_ptr, &declared_type);
    }

    // ensure we can access this method
    CHECK_ERROR(verifier_can_access_method(function->method->DeclaringType, method, instance),
        TDN_ERROR_VERIFIER_METHOD_ACCESS);

    // special case, since its only used to pass it to the newobj, we verify
    // externally that the opcodes are valid
    jit_stack_value_t* value = STACK_PUSH();
    value->kind = JIT_KIND_NATIVE_INT;
    value->method = method;
    value->flags.ldftn = inst->opcode == CEE_LDFTN;

cleanup:
    return err;
}

static tdn_err_t verify_call(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: tail call support

    RuntimeMethodBase method = inst->operand.method;
    RuntimeTypeInfo method_type = method->Attributes.Static ? NULL : method->DeclaringType;

    // check for methods that are considered unsafe to call unless the assembly
    // is allowed unsafe
    if (
        method->DeclaringType == tUnsafe
    ) {
        CHECK(function->method->Module->Assembly->AllowUnsafe,
            "Assembly is not allowed to call %T::%U", method->DeclaringType, method->Name);
    }

    if (inst->opcode == CEE_CALLVIRT) {
        CHECK_ERROR(method_type != NULL, TDN_ERROR_VERIFIER_CALLVIRT_ON_STATIC);
        CHECK_ERROR(!tdn_type_is_valuetype(method_type), TDN_ERROR_VERIFIER_CALLVIRT_ON_VALUE_TYPE);

    } else if (inst->opcode == CEE_CALL) {
        // this is most likely a static interface function, resolve it
        if (inst->constrained != NULL) {
            CHECK(method->Attributes.Static);
        } else {
            CHECK_ERROR(!method->Attributes.Abstract, TDN_ERROR_VERIFIER_CALL_ABSTRACT);
        }
    }

    bool might_leak_local_ref = false;

    if (inst->opcode == CEE_NEWOBJ && jit_is_delegate(method_type)) {
        // creating a delegate, ensure we call the ctor properly
        CHECK_ERROR(method->Parameters->Length == 2, TDN_ERROR_VERIFIER_DELEGATE_CTOR);
        jit_stack_value_t declared_obj = jit_stack_value_create(method->Parameters->Elements[0]->ParameterType);
        jit_stack_value_t declared_ftn = jit_stack_value_create(method->Parameters->Elements[1]->ParameterType);

        CHECK_ERROR(declared_ftn.kind == JIT_KIND_NATIVE_INT,
            TDN_ERROR_VERIFIER_DELEGATE_CTOR_SIG_I);

        jit_stack_value_t* actual_ftn = &stack[1];
        jit_stack_value_t* actual_obj = &stack[0];
        CHECK_INIT_THIS(actual_obj);

        // ensure we have a method in here, and not a normal native int
        CHECK_ERROR(actual_ftn->kind == JIT_KIND_NATIVE_INT && actual_ftn->method != NULL,
            TDN_ERROR_VERIFIER_STACK_METHOD);

        CHECK_IS_ASSIGNABLE(actual_obj, &declared_obj);
        CHECK_ERROR(actual_obj->kind == JIT_KIND_OBJ_REF,
            TDN_ERROR_VERIFIER_DELEGATE_CTOR_SIG_O);

        // we can only load non-final virtual functions that are from the base and from
        // the this pointer from ldftn, any other virtual function is not allowed
        if (actual_ftn->flags.ldftn) {
            if (actual_ftn->method->Attributes.Virtual && !actual_ftn->method->Attributes.Final && !jit_is_boxed_value_type(actual_obj)) {
                if (!actual_ftn->method->DeclaringType->Attributes.Sealed) {
                    CHECK_ERROR(actual_obj->flags.this_ptr,
                        TDN_ERROR_VERIFIER_LDFTN_NON_FINAL_VIRTUAL);
                }
            }
        }

        // TODO: check delegate assignable
    } else {
        // check that the args match, ignore the instance for now
        int off = (method_type != NULL && inst->opcode != CEE_NEWOBJ) ? 1 : 0;
        CHECK(arrlen(stack) == (off + method->Parameters->Length));
        for (int i = 0; i < method->Parameters->Length; i++) {
            jit_stack_value_t* actual = &stack[off + i];
            jit_stack_value_t declared = jit_stack_value_create(method->Parameters->Elements[i]->ParameterType);
            CHECK_IS_ASSIGNABLE(actual, &declared);

            // if this is a ref-struct, and its has local refs, then it might
            // leak a local ref from that
            if (actual->type != NULL && actual->type->IsByRefStruct) {
                might_leak_local_ref = !actual->flags.ref_struct_non_local;
            }

            // TODO: unscoped this support

            // if this is a ref-struct, and it contains local fields, ensure
            // we don't leak don't leak them accidently
            if (actual->type != NULL && actual->type->IsByRefStruct && !actual->flags.ref_struct_non_local) {
                might_leak_local_ref = true;
            }
        }
    }

    RuntimeTypeInfo instance = NULL;
    if (inst->opcode == CEE_NEWOBJ) {
        // ensure that this is a valid ctor
        CHECK_ERROR(tdn_compare_string_to_cstr(method->Name, ".ctor"), TDN_ERROR_VERIFIER_CTOR_EXPECTED);
        CHECK_ERROR(!method->Attributes.Static, TDN_ERROR_VERIFIER_CTOR_SIG);
        CHECK_ERROR(method_type != NULL, TDN_ERROR_VERIFIER_CTOR_SIG);
        CHECK_ERROR(!method->Attributes.Abstract, TDN_ERROR_VERIFIER_CTOR_SIG);

        // TODO: array ctor???

        // must not be an abstract class
        CHECK_ERROR(!method_type->Attributes.Abstract, TDN_ERROR_VERIFIER_NEWOBJ_ABSTRACT_CLASS);

    } else if (method_type != NULL) {
        jit_stack_value_t* actual_this = &stack[0];
        instance = actual_this->type;
        jit_stack_value_t declared_this = {
            .kind = tdn_type_is_valuetype(method_type) ? JIT_KIND_BY_REF : JIT_KIND_OBJ_REF,
            .type = method_type
        };

        // direct calls to ctor are mostly not allowed
        if (tdn_compare_string_to_cstr(method->Name, ".ctor")) {
            if (function->track_ctor_state && actual_this->flags.this_ptr && (method_type == method->DeclaringType || method_type == method->DeclaringType->BaseType)) {
                // we have called the base ctor, so its not initialized properly in this block
                block->this_initialized = true;
            } else {
                // allow direct calls to valuetype ctors
                CHECK_ERROR(actual_this->kind == JIT_KIND_BY_REF && tdn_type_is_valuetype(actual_this->type),
                    TDN_ERROR_VERIFIER_CALL_CTOR);
            }
        }

        if (inst->constrained != NULL) {
            // must be a by ref
            CHECK_ERROR(actual_this->kind == JIT_KIND_BY_REF,
                TDN_ERROR_VERIFIER_CONSTRAINED_CALL_WITH_NON_BYREF_THIS);

            // ensure the constrained matches the actual this type
            CHECK_ERROR(actual_this->type == inst->constrained,
                TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

            // turn into am objref
            actual_this->kind = JIT_KIND_OBJ_REF;
        }

        // if the method or type is marked as readonly then the
        // reference is also considered readonly
        if ((method_type->IsReadOnly || method->IsReadOnly) && declared_this.kind == JIT_KIND_BY_REF) {
            declared_this.flags.ref_read_only = true;
        }

        CHECK_IS_ASSIGNABLE(actual_this, &declared_this);

        // TODO: unscoped reference

        if (inst->opcode == CEE_CALL) {
            // just like the rules for creating delegates with ldftn and virtual functions, but now for
            // normal calls
            if (method->Attributes.Virtual && !method->Attributes.Final && !jit_is_boxed_value_type(actual_this)) {
                if (!method_type->Attributes.Sealed) {
                    CHECK_ERROR(actual_this->flags.this_ptr,
                        TDN_ERROR_VERIFIER_THIS_MISMATCH);
                }
            }
        }
    }

    // Check we can access the method
    CHECK_ERROR(verifier_can_access_method(method->DeclaringType, method, instance),
        TDN_ERROR_VERIFIER_METHOD_ACCESS);

    if (inst->opcode == CEE_NEWOBJ) {
        jit_stack_value_t* value = STACK_PUSH();

        // push the type
        jit_stack_value_init(value, method_type);

        // if we constructed the ref-struct only from non-local
        // references, then we can mark the struct itself also
        // as containing non-local references
        if (method_type->IsByRefStruct) {
            value->flags.ref_struct_non_local = !might_leak_local_ref;
        }

    } else if (method->ReturnParameter->ParameterType != tVoid) {
        jit_stack_value_t* return_value = STACK_PUSH();
        jit_stack_value_init(return_value, method->ReturnParameter->ParameterType);

        if (return_value->kind == JIT_KIND_BY_REF) {
            // if we won't leak a local reference, this result is a non-local reference
            return_value->flags.ref_non_local = !might_leak_local_ref;

            if (method->ReturnParameter->ReferenceIsReadOnly) {
                // the reference is readonly
                return_value->flags.ref_read_only = true;
            }

        }

        if (return_value->type->IsByRefStruct) {
            // returned a ref-struct, can't contain local references as long as the input
            // can't contain them
            return_value->flags.ref_struct_non_local = !might_leak_local_ref;
        }
    }

cleanup:
    return err;
}

static tdn_err_t verify_ret(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure that we don't return before the base ctor was called
    if (function->track_ctor_state) {
        CHECK_ERROR(block->this_initialized || function->method->DeclaringType == tObject,
            TDN_ERROR_VERIFIER_THIS_UNINIT_RETURN);
    }

    // Check we don't return from a protected block
    CHECK_ERROR(block->block.filter_clause == NULL, TDN_ERROR_VERIFIER_RETURN_FROM_FILTER);
    CHECK_ERROR(block->block.try_clause == NULL, TDN_ERROR_VERIFIER_RETURN_FROM_TRY);
    CHECK_ERROR(block->block.handler_clause == NULL, TDN_ERROR_VERIFIER_RETURN_FROM_HANDLER);

    RuntimeTypeInfo declared_return_type = function->method->ReturnParameter->ParameterType;
    if (declared_return_type == tVoid) {
        CHECK_ERROR(arrlen(block->stack) == 0, TDN_ERROR_VERIFIER_RETURN_VOID);
    } else {
        CHECK_ERROR(arrlen(block->stack) == 0, TDN_ERROR_VERIFIER_RETURN_EMPTY);
        CHECK_INIT_THIS(stack);

        jit_stack_value_t ret_val = jit_stack_value_create(declared_return_type);
        CHECK_IS_ASSIGNABLE(stack, &ret_val);

        if (stack->kind == JIT_KIND_BY_REF) {
            // ensure we don't return a pointer to the stack
            CHECK_ERROR(stack->flags.ref_non_local,
                TDN_ERROR_VERIFIER_RETURN_PTR_TO_STACK);

            // ensure we don't return a readonly ref
            // when the function return is non-readonly
            if (stack->flags.ref_read_only) {
                CHECK(function->method->ReturnParameter->ReferenceIsReadOnly);
            }
        }

        // if we are returning a ref-struct, then it must
        // have non-local references, otherwise we might
        // leak it
        if (stack->type != NULL && stack->type->IsByRefStruct) {
            CHECK(stack->flags.ref_struct_non_local);
        }
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// type checking
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_castclass(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_stack_value_t* value = &stack[0];
    CHECK_INIT_THIS(value);

    CHECK(value->kind == JIT_KIND_OBJ_REF);

    // ensure we can access that type
    CHECK_ERROR(verifier_can_access_type(function->method->DeclaringType, inst->operand.type),
        TDN_ERROR_VERIFIER_TYPE_ACCESS);

    // and push it
    jit_stack_value_t* pushed = STACK_PUSH();
    pushed->kind = JIT_KIND_OBJ_REF;
    pushed->type = inst->operand.type;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Boxing
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_box(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeTypeInfo type = inst->operand.type;

    jit_stack_value_t* value = &stack[0];
    CHECK_INIT_THIS(value);

    jit_stack_value_t target_type = {};
    jit_stack_value_init(&target_type, type);

    // can't box a byref
    CHECK_ERROR(target_type.kind != JIT_KIND_BY_REF,
        TDN_ERROR_VERIFIER_BOX_BYREF);

    // ensure we can access the boxed type
    CHECK_ERROR(verifier_can_access_type(function->method->DeclaringType, type),
        TDN_ERROR_VERIFIER_TYPE_ACCESS);

    CHECK_IS_ASSIGNABLE(value, &target_type);

    // For Nullable<T> we push T
    jit_stack_value_t* pushed = STACK_PUSH();
    pushed->kind = JIT_KIND_OBJ_REF;
    pushed->type = tdn_type_is_nullable(type) ? type->GenericArguments->Elements[0] : type;

cleanup:
    return err;
}

static tdn_err_t verify_unbox(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_INIT_THIS(stack);

    if (inst->opcode == CEE_UNBOX_ANY) {
        jit_stack_value_init(STACK_PUSH(), inst->operand.type);
    } else {
        CHECK_ERROR(tdn_type_is_valuetype(inst->operand.type),
            TDN_ERROR_VERIFIER_VALUE_TYPE_EXPEXCTED);

        jit_stack_value_t* value = STACK_PUSH();
        value->flags.ref_read_only = true;
        value->flags.ref_non_local = true;
        value->kind = JIT_KIND_BY_REF;
        value->type = inst->operand.type;
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Branching
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_br(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);

    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, target, false));

cleanup:
    return err;
}

static tdn_err_t verify_br_unary_cond(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_INIT_THIS(stack);
    CHECK_ERROR(
        stack->kind >= JIT_KIND_INT32 && stack->kind <= JIT_KIND_NATIVE_INT ||
        stack->kind == JIT_KIND_OBJ_REF ||
        stack->kind == JIT_KIND_BY_REF,
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED
    );

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);
    jit_block_t* next = jit_function_get_block(function, block->block.end, block->leave_target_stack);
    CHECK(next != NULL);

    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, target, false));
    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, next, true));

cleanup:
    return err;
}

static tdn_err_t verify_br_binary_cond(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_INIT_THIS(&stack[0]);
    CHECK_INIT_THIS(&stack[1]);

    CHECK_ERROR(verifier_is_binary_comparable(&stack[0], &stack[1], inst->opcode),
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);
    jit_block_t* next = jit_function_get_block(function, block->block.end, block->leave_target_stack);
    CHECK(next != NULL);

    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, target, false));
    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, next, true));

cleanup:
    return err;
}

static tdn_err_t verify_switch(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure we have an int32 on the stack
    // TODO: can we have anything else?
    CHECK(stack->kind == JIT_KIND_INT32);

    // verify the default case
    jit_block_t* next = jit_function_get_block(function, block->block.end, block->leave_target_stack);
    CHECK(next != NULL);
    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, next, true));

    // verify the rest of the blocks
    for (int i = 0; i < arrlen(inst->operand.switch_targets); i++) {
        jit_block_t* target = jit_function_get_block(function, inst->operand.switch_targets[i], block->leave_target_stack);
        CHECK(target != NULL);
        CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, target, false));
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Exceptions
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_leave(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // empty the stack
    arrsetlen(block->stack, 0);

    // TODO: is valid leave target

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

    // merge with the target block
    CHECK_AND_RETHROW(verifier_merge_blocks(function, block, target));

    // override the target pc with the actual target we want to go to
    inst->operand.branch_target = target_pc;

cleanup:
    return err;
}

static tdn_err_t verify_endfinally(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // empty the stack
    arrsetlen(block->stack, 0);

    // ensure we have a handler clause for the block
    CHECK_ERROR(block->block.handler_clause != NULL,
        TDN_ERROR_VERIFIER_ENDFINALLY);

    // ensure the handler is a finally/fault region
    CHECK_ERROR(
        block->block.handler_clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY ||
        block->block.handler_clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT,
        TDN_ERROR_VERIFIER_ENDFINALLY);

    // ensure we have a valid leave target stack
    CHECK(block->leave_target_stack != NULL);
    uint32_t target_pc = arrpop(block->leave_target_stack);
    jit_block_t* target = jit_function_get_block(function, target_pc, block->leave_target_stack);
    CHECK(target != NULL);

    // and now merge with the target
    CHECK_AND_RETHROW(verifier_merge_blocks(function, block, target));

    // override the target pc in the instruction
    // for the emitter to know where to jump into
    inst->operand.branch_target = target_pc;

cleanup:
    return err;
}

static tdn_err_t verify_throw(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_INIT_THIS(stack);
    CHECK(stack[0].kind == JIT_KIND_OBJ_REF);

    // TODO: check this is System.Exception

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
    [CEE_LDARGA] = verify_ldarga,

    [CEE_LDLOC] = verify_ldloc,
    [CEE_STLOC] = verify_stloc,
    [CEE_LDLOCA] = verify_ldloca,

    [CEE_LDFLDA] = verify_ldflda,
    [CEE_LDFLD] = verify_ldfld,
    [CEE_STFLD] = verify_stfld,
    [CEE_LDSFLDA] = verify_ldflda,
    [CEE_LDSFLD] = verify_ldfld,
    [CEE_STSFLD] = verify_stfld,

    [CEE_INITOBJ] = verify_initobj,

    [CEE_LDIND_I1] = verify_ldind,
    [CEE_LDIND_U1] = verify_ldind,
    [CEE_LDIND_I2] = verify_ldind,
    [CEE_LDIND_U2] = verify_ldind,
    [CEE_LDIND_I4] = verify_ldind,
    [CEE_LDIND_U4] = verify_ldind,
    [CEE_LDIND_I8] = verify_ldind,
    [CEE_LDIND_I] = verify_ldind,
    [CEE_LDIND_REF] = verify_ldind,
    [CEE_LDIND_R8] = verify_ldind,
    [CEE_LDOBJ] = verify_ldind,

    [CEE_STIND_I] = verify_stind,
    [CEE_STIND_I1] = verify_stind,
    [CEE_STIND_I2] = verify_stind,
    [CEE_STIND_I4] = verify_stind,
    [CEE_STIND_I8] = verify_stind,
    [CEE_STIND_REF] = verify_stind,
    [CEE_STIND_R8] = verify_stind,
    [CEE_STOBJ] = verify_stind,

    [CEE_LDNULL] = verify_ldnull,
    [CEE_LDSTR] = verify_ldstr,
    [CEE_LDTOKEN] = verify_ldtoken,
    [CEE_LDC_I4] = verify_ldc_i4,
    [CEE_LDC_I8] = verify_ldc_i8,
    [CEE_LDC_R4] = verify_ldc_r4,
    [CEE_LDC_R8] = verify_ldc_r8,
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

    [CEE_NEG] = verify_unary_op,
    [CEE_NOT] = verify_unary_op,

    [CEE_SHR] = verify_shift,
    [CEE_SHL] = verify_shift,
    [CEE_SHR_UN] = verify_shift,

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
    [CEE_CONV_R4] = verify_conv,
    [CEE_CONV_R8] = verify_conv,
    [CEE_CONV_R_UN] = verify_conv,

    [CEE_CEQ] = verify_compare,
    [CEE_CGT] = verify_compare,
    [CEE_CGT_UN] = verify_compare,
    [CEE_CLT] = verify_compare,
    [CEE_CLT_UN] = verify_compare,

    [CEE_NEWARR] = verify_newarr,
    [CEE_LDLEN] = verify_ldlen,
    [CEE_LDELEMA] = verify_ldelema,
    [CEE_LDELEM] = verify_ldelem,
    [CEE_LDELEM_REF] = verify_ldelem,
    [CEE_STELEM] = verify_stelem,
    [CEE_STELEM_REF] = verify_stelem,

    [CEE_LDFTN] = verify_ldftn,
    [CEE_LDVIRTFTN] = verify_ldftn,

    [CEE_NEWOBJ] = verify_call,
    [CEE_CALL] = verify_call,
    [CEE_CALLVIRT] = verify_call,

    [CEE_RET] = verify_ret,

    [CEE_CASTCLASS] = verify_castclass,
    [CEE_ISINST] = verify_castclass,

    [CEE_BOX] = verify_box,
    [CEE_UNBOX] = verify_unbox,
    [CEE_UNBOX_ANY] = verify_unbox,

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
    [CEE_SWITCH] = verify_switch,

    [CEE_LEAVE] = verify_leave,
    [CEE_ENDFINALLY] = verify_endfinally,
    [CEE_THROW] = verify_throw,

    [CEE_SIZEOF] = verify_sizeof,
};
size_t g_verify_dispatch_table_size = ARRAY_LENGTH(g_verify_dispatch_table);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Entry points
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t verifier_on_block_fallthrough(jit_function_t* function, jit_block_t* from, jit_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, from, block, true));

cleanup:
    return err;
}
