#include "internal.h"

#include "casting.h"
#include "dotnet/types.h"
#include "tomatodotnet/util/stb_ds.h"
#include "util/except.h"

RuntimeTypeInfo verifier_get_type_definition(RuntimeTypeInfo typ) {
    return typ->GenericTypeDefinition ?: typ;
}

static RuntimeTypeInfo resolve_generic(RuntimeTypeInfo type, RuntimeMethodInfo method) {
    if (type->IsGenericMethodParameter && type->DeclaringMethod == (RuntimeMethodBase)method) {
        ASSERT(type->GenericParameterPosition < method->GenericArguments->Length);
        return method->GenericArguments->Elements[type->GenericParameterPosition];
    } else if (type->IsGenericTypeParameter && type->DeclaringType == method->DeclaringType) {
        ASSERT(type->GenericParameterPosition < method->DeclaringType->GenericArguments->Length);
        return method->DeclaringType->GenericArguments->Elements[type->GenericParameterPosition];
    }
    return type;
}

static bool match_generic_type(RuntimeTypeInfo a, RuntimeTypeInfo b, RuntimeMethodInfo ma, RuntimeMethodInfo mb) {
    a = resolve_generic(a, mb);
    b = resolve_generic(b, ma);
    if (a == b) {
        return true;
    }

    // must be one of these
    if (a->IsByRef != b->IsByRef) return false;
    if (a->IsArray != b->IsArray) return false;
    if (a->IsPointer != b->IsPointer) return false;
    if (a->ElementType != NULL) {
        if (b->ElementType == NULL) {
            return false;
        }
        if (!match_generic_type(a->ElementType, b->ElementType, ma, mb)) return false;
    } else {
        if (b->ElementType != NULL) {
            return false;
        }
    }

    return true;
}

static bool match_generic_param(ParameterInfo a, ParameterInfo b, RuntimeMethodInfo ma, RuntimeMethodInfo mb) {
    if (!match_generic_type(a->ParameterType, b->ParameterType, ma, mb)) return false;
    if (a->Attributes.In != b->Attributes.In) return false;
    return true;
}

static bool match_generic_signature(RuntimeMethodInfo a, RuntimeMethodInfo b) {
    if (a->GenericArguments != NULL) {
        if (b->GenericArguments == NULL) {
            return false;
        }

        if (a->GenericArguments->Length != b->GenericArguments->Length) {
            return false;
        }
    } else if (b->GenericArguments != NULL) {
        return false;
    }

    // check the return type
    if (!match_generic_param(a->ReturnParameter, b->ReturnParameter, a, b)) {
        return false;
    }

    // Check parameter count matches
    if (a->Parameters->Length != b->Parameters->Length) {
        return false;
    }

    // check the parameters
    for (int j = 0; j < a->Parameters->Length; j++) {
        ParameterInfo paramA = a->Parameters->Elements[j];
        ParameterInfo paramB = b->Parameters->Elements[j];
        if (!match_generic_param(paramA, paramB, a, b)) {
            return false;
        }
    }

    return true;
}

RuntimeMethodBase verifier_get_typical_method_definition(RuntimeMethodBase method) {
    if (method->GenericMethodDefinition != NULL) {
        method = (RuntimeMethodBase)method->GenericMethodDefinition;
    }

    RuntimeTypeInfo owning_type_definition = verifier_get_type_definition(method->DeclaringType);

    // If method is on a type that is its own type definition, this it is the type method
    if (owning_type_definition == method->DeclaringType) {
        return method;
    }

    // Otherwise, find its equivalent on the type definition of the owning type
    // TODO: is there a faster way to do it? maybe search in the method list and take the same
    //       index assuming that we have the same amount of methods at the same thing?
    for (int i = 0; i < owning_type_definition->DeclaredMethods->Length; i++) {
        RuntimeMethodInfo m = owning_type_definition->DeclaredMethods->Elements[i];

        // check the name
        if (!tdn_compare_string(m->Name, method->Name)) {
            continue;
        }

        if (match_generic_signature(m, (RuntimeMethodInfo)method)) {
            return (RuntimeMethodBase)m;
        }
    }

    for (int i = 0; i < owning_type_definition->DeclaredConstructors->Length; i++) {
        RuntimeMethodInfo m = (RuntimeMethodInfo)owning_type_definition->DeclaredConstructors->Elements[i];

        // check the name
        if (!tdn_compare_string(m->Name, method->Name)) {
            continue;
        }

        if (match_generic_signature(m, (RuntimeMethodInfo)method)) {
            return (RuntimeMethodBase)m;
        }
    }

    ASSERT(false, "Failed to find the method on the parent -> %T::%U ON %T", method->DeclaringType, method->Name, owning_type_definition);
}

stack_value_t* stack_value_init(stack_value_t* value, RuntimeTypeInfo type) {
    if (
        type == tBoolean ||
        type == tChar ||
        type == tSByte ||
        type == tByte ||
        type == tInt16 ||
        type == tUInt16 ||
        type == tInt32 ||
        type == tUInt32
    ) {
        value->kind = KIND_INT32;
        value->type = tInt32;

    } else if (type == tInt64 || type == tUInt64) {
        value->kind = KIND_INT64;
        value->type = tInt64;

    } else if (type == tSingle) {
        value->kind = KIND_FLOAT;
        value->type = tSingle;

    } else if (type == tDouble) {
        value->kind = KIND_FLOAT;
        value->type = tDouble;

    } else if (type == tIntPtr || type == tUIntPtr) {
        value->kind = KIND_NATIVE_INT;
        value->type = tIntPtr;

    } else if (type->IsPointer) {
        value->kind = KIND_NATIVE_INT;
        value->type = type->ElementType;

    } else if (type->BaseType == tEnum) {
        stack_value_init(value, type->EnumUnderlyingType);

    } else if (type->IsByRef) {
        value->kind = KIND_BY_REF;
        value->type = type->ElementType;

    } else if (tdn_type_is_valuetype(type) || type->IsGenericParameter) {
        value->kind = KIND_VALUE_TYPE;
        value->type = type;

    } else {
        ASSERT(tdn_type_is_gc_pointer(type));
        value->kind = KIND_OBJ_REF;
        value->type = type;

    }

    return value;
}

bool verifier_is_assignable(stack_value_t* src, stack_value_t* dst) {
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
        case KIND_OBJ_REF: {
            if (dst->kind != KIND_OBJ_REF) {
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
        case KIND_INT64:
        case KIND_FLOAT:
        case KIND_VALUE_TYPE:
            return false;

        case KIND_BY_REF: {
            if (dst->kind == KIND_BY_REF && dst->flags.ref_read_only) {
                return src->type == dst->type;
            }

            return false;
        } break;

            // can either be int64 or native int
        case KIND_INT32:
            return dst->kind == KIND_INT64 || dst->kind == KIND_NATIVE_INT;

            // can also be int64
        case KIND_NATIVE_INT:
            return dst->kind == KIND_INT64 || dst->kind == KIND_NATIVE_INT;

        default:
            ASSERT(!"Invalid kind", "%d of %T", src->kind, src->type);
    }
}
