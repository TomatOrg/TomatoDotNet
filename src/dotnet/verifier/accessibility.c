#include "accessibility.h"

#include "casting.h"
#include "internal.h"
#include "tomatodotnet/types/type.h"
#include "util/except.h"

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

bool verifier_can_access_type(RuntimeTypeInfo current_class, RuntimeTypeInfo target_class) {
    if (target_class->IsGenericParameter) {
        return true; // Generic parameters are always accessible
    }

    if (target_class->IsByRef || target_class->IsPointer || target_class->IsArray) {
        return verifier_can_access_type(current_class, target_class->ElementType);
    }

    // Check access to class instantiations if generic class
    if (target_class->GenericArguments != NULL && !verifier_can_access_instantiation(current_class, target_class->GenericArguments)) {
        return false;
    }

    RuntimeTypeInfo current_type_def = verifier_get_type_definition(current_class);
    RuntimeTypeInfo target_type_def = verifier_get_type_definition(target_class);

    RuntimeTypeInfo target_containing_type = target_type_def->DeclaringType;
    if (target_containing_type == NULL) {
        // a non-nested class can be either all public or accessible only from its own assembly (and friends)
        if ((target_type_def->Attributes.Value & TDN_TYPE_VISIBILITY_PUBLIC) != 0) {
            return true;
        } else {
            return current_type_def->Module == target_type_def->Module ||
                    verifier_grants_friend_access_to(target_type_def->Module, current_type_def->Module);
        }
    }

    // Target class is nested
    uint32_t visibility = verifier_nested_to_method_access_attribute(target_type_def->Attributes.Visibility);

    // Translate access check into member access check, i.e. check whether the current class can access
    // a member of the enclosing class with the visibility of target class
    return verifier_can_access_member(current_type_def, target_containing_type, visibility, NULL);
}

static bool verifier_can_access_family(RuntimeTypeInfo current_type, RuntimeTypeInfo target_type_def, RuntimeTypeInfo instance_type) {
    // if instance type is generics and inherit from target type def members of target type def are accessible
    if (instance_type->IsGenericParameter) {
        return verifier_can_cast_to(instance_type, target_type_def);
    }

    // Iterate through all containing types of instance
    while (instance_type != NULL) {
        RuntimeTypeInfo cur_inst_type_def = instance_type;
        RuntimeTypeInfo current_type_def = verifier_get_type_definition(current_type);

        // Iterate through all super types of current instance type
        while (cur_inst_type_def != NULL) {
            if (current_type_def == verifier_get_type_definition(cur_inst_type_def)) {
                // At this point we know that instance type is able to access the same family fields as curren type
                // Now iterate through all super types of current type to see if current type can access family target type
                while (current_type_def != NULL) {
                    if (current_type_def == target_type_def) {
                        return true;
                    }

                    current_type_def = current_type_def->BaseType;
                    if (current_type_def != NULL) {
                        current_type_def = verifier_get_type_definition(current_type_def);
                    }
                }

                return false;
            }

            cur_inst_type_def = cur_inst_type_def->BaseType;
        }

        instance_type = verifier_get_type_definition(instance_type)->DeclaringType;
    }

    return false;
}

static bool verifier_can_access_member(RuntimeTypeInfo current_type, RuntimeTypeInfo target_type, uint32_t member_visibility, RuntimeTypeInfo instance) {
    if (instance == NULL) {
        instance = current_type;
    }

    // Check access to class defining member
    if (!verifier_can_access_type(current_type, target_type)) {
        return false;
    }

    RuntimeTypeInfo target_type_def = verifier_get_type_definition(target_type);

    if (member_visibility == TDN_ACCESS_PUBLIC) {
        return true;
    }

    // This is module-scope checking, to support C++ file & function statics.
    if (member_visibility == TDN_ACCESS_PRIVATE_SCOPE) {
        return current_type->Module == target_type_def->Module;
    }

    if (member_visibility == TDN_ACCESS_ASSEMBLY) {
        return current_type->Module == target_type_def->Module ||
            verifier_grants_friend_access_to(target_type_def->Module, current_type->Module);
    }

    if (member_visibility == TDN_ACCESS_FAMILY_AND_ASSEMBLY) {
        if (
            current_type->Module != target_type_def->Module &&
            !verifier_grants_friend_access_to(target_type_def->Module, current_type->Module)
        ) {
            return false;
        }
    }

    // Nested classes can access all members of their parent class
    do {
        // Classes have access to all of their own members
        if (current_type == target_type_def) {
            return true;
        }

        switch (member_visibility) {
            case TDN_ACCESS_FAMILY_OR_ASSEMBLY: {
                if (
                    current_type->Module == target_type_def->Module ||
                    verifier_grants_friend_access_to(target_type_def->Module, current_type->Module)
                ) {
                    return true;
                }

                // Check if current class is subclass of target
                if (verifier_can_access_family(current_type, target_type_def, instance)) {
                    return true;
                }
            } break;

            case TDN_ACCESS_FAMILY:
            case TDN_ACCESS_FAMILY_AND_ASSEMBLY: {
                // Assembly access we already checked earlier, so only need to check family access
                if (verifier_can_access_family(current_type, target_type_def, instance)) {
                    return true;
                }
            } break;

            case TDN_ACCESS_PRIVATE:
                break; // Already handled by loop

            default:
                ASSERT(!"Invalid");
        }

        RuntimeTypeInfo containing_type = current_type->DeclaringType;
        if (containing_type != NULL) {
            current_type = verifier_get_type_definition(containing_type);
        } else {
            current_type = NULL;
        }
    } while (current_type != NULL);

    return false;
}

bool verifier_can_access_field(RuntimeTypeInfo current_type, RuntimeFieldInfo field, RuntimeTypeInfo instance) {
    RuntimeTypeInfo field_owning_type = field->DeclaringType;

    field_owning_type = field_owning_type->GenericTypeDefinition ?: field_owning_type;
    current_type = current_type->GenericTypeDefinition ?: current_type;

    // check the access to the member field
    if (!verifier_can_access_member(current_type, field_owning_type, field->Attributes.FieldAccess, instance)) {
        return false;
    }

    // check access to the field type itself
    return verifier_can_access_type(current_type, field->FieldType);
}

static bool verifier_can_access_signature(RuntimeTypeInfo current_type, RuntimeMethodBase signature) {
    if (!verifier_can_access_type(current_type, signature->ReturnParameter->ParameterType)) {
        return false;
    }

    for (int i = 0; i < signature->Parameters->Length; i++) {
        if (!verifier_can_access_type(current_type, signature->Parameters->Elements[i]->ParameterType)) {
            return false;
        }
    }

    return true;
}

bool verifier_can_access_method(RuntimeTypeInfo current_type, RuntimeMethodBase target_method, RuntimeTypeInfo instance) {
    if (target_method->GenericArguments != NULL && !verifier_can_access_instantiation(current_type, target_method->GenericArguments)) {
        return false;
    }

    RuntimeMethodBase target_method_def = verifier_get_typical_method_definition(target_method);
    RuntimeTypeInfo current_type_def = current_type->GenericTypeDefinition ?: current_type;

    if (!verifier_can_access_member(current_type_def, target_method_def->DeclaringType, target_method_def->Attributes.MemberAccess, instance)) {
        return false;
    }

    return verifier_can_access_signature(current_type_def, target_method_def);
}
