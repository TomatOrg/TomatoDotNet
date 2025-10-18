#include "casting.h"

#include "dotnet/types.h"
#include "tomatodotnet/util/stb_ds.h"
#include "util/except.h"

#if 0
    static int m_verifier_trace_indent = 0;

    #define VERIFY_TRACE(fmt, ...) TRACE("%*s" fmt, m_verifier_trace_indent, "", ## __VA_ARGS__)

    #define VERIFY_ENTER() \
        do { \
            VERIFY_TRACE("%s || %T => %T", __FUNCTION__, this_type, other_type); \
            m_verifier_trace_indent += 2; \
        } while(0)

    #define VERIFY_LEAVE(success) \
        ({ \
            bool __success = success; \
            m_verifier_trace_indent -= 2; \
            VERIFY_TRACE("-> %s (%d)", __success ? "true" : "false", __LINE__); \
            __success; \
        })

#else
    #define VERIFY_TRACE(...)
    #define VERIFY_ENTER(...)
    #define VERIFY_LEAVE(success) success
#endif

RuntimeTypeInfo verifier_get_underlying_type(RuntimeTypeInfo type) {
    if (type->BaseType != tEnum) {
        return type;
    }
    return type->EnumUnderlyingType;
}

static RuntimeTypeInfo verifier_get_normalized_integral_array_element_type(RuntimeTypeInfo type) {
    ASSERT(type->BaseType != tEnum);

    if (type == tByte) return tSByte;
    if (type == tUInt16) return tInt16;
    if (type == tUInt32) return tInt32;
    if (type == tUInt64) return tInt64;
    if (type == tUIntPtr) return tIntPtr;
    return type;
}

static bool verifier_is_constrained_as_gc_pointer(RuntimeTypeInfo type) {
    for (int i = 0; i < type->GenericParameterConstraints->Length; i++) {
        RuntimeTypeInfo type_constraint = type->GenericParameterConstraints->Elements[i];

        if (type_constraint->IsGenericParameter) {
            if (verifier_is_constrained_as_gc_pointer(type_constraint)) {
                return true;
            }
        }

        if (!tdn_is_interface(type_constraint) && tdn_type_is_gc_pointer(type_constraint)) {
            if (type_constraint != tObject) {
                return true;
            }
        }
    }

    return false;
}

static bool verifier_can_cast_param_to(RuntimeTypeInfo cur_types_parm, RuntimeTypeInfo param_type) {
    // While boxed value classes inherit from object their
    // unboxed versions do not.  Parameterized types have the
    // unboxed version, thus, if the from type parameter is value
    // class then only an exact match/equivalence works.
    if (cur_types_parm == param_type) {
        return true;
    }

    RuntimeTypeInfo from_param_underlying_type = verifier_get_underlying_type(cur_types_parm);
    if (tdn_type_is_gc_pointer(from_param_underlying_type)) {
        return verifier_can_cast_to(cur_types_parm, param_type);

    } else if (cur_types_parm->IsGenericParameter) {
        RuntimeTypeInfo generic_variable_from_param = cur_types_parm;
        if (
            tdn_has_reference_type_constraint(generic_variable_from_param) ||
            verifier_is_constrained_as_gc_pointer(generic_variable_from_param)
        ) {
            return verifier_can_cast_to(generic_variable_from_param, param_type);
        }

    } else if (tdn_is_primitive(from_param_underlying_type)) {
        RuntimeTypeInfo to_param_underlying_type = verifier_get_underlying_type(param_type);
        if (verifier_get_normalized_integral_array_element_type(from_param_underlying_type) == verifier_get_normalized_integral_array_element_type(to_param_underlying_type)) {
            return true;
        }
    }

    return false;
}

static bool verifier_can_cast_to_non_variant_interface(RuntimeTypeInfo this_type, RuntimeTypeInfo other_type) {
    VERIFY_ENTER();

    if (other_type == this_type) {
        return VERIFY_LEAVE(true);
    }

    if (hmgeti(this_type->InterfaceImpls, other_type) >= 0) {
        return VERIFY_LEAVE(true);
    }

    return VERIFY_LEAVE(false);
}

static bool verifier_can_cast_to_interface(RuntimeTypeInfo this_type, RuntimeTypeInfo other_type) {
    // TODO: handle variance
    VERIFY_ENTER();
    return VERIFY_LEAVE(verifier_can_cast_to_non_variant_interface(this_type, other_type));
}

static bool verifier_can_cast_to_class(RuntimeTypeInfo this_type, RuntimeTypeInfo other_type) {
    RuntimeTypeInfo cur_type = this_type;

    if (tdn_is_interface(cur_type) && other_type == tObject) {
        return true;
    }

    // TODO: handle variance

    //
    // If there are no variant type parameters, just chase the hierarchy
    //
    // Allow cur_type to be nullable, which means this method
    // will additionally return true if cur_type is Nullable<T> &&
    //          curr_type == other_type
    //      OR
    //          other_type is System.ValueType or System.Object
    //

    // Always strip Nullable from other_type, if present
    if (tdn_type_is_nullable(other_type) && !tdn_type_is_nullable(this_type)) {
        return verifier_can_cast_to(this_type, other_type->GenericArguments->Elements[0]);
    }

    do {
        if (cur_type == other_type) {
            return true;
        }
        cur_type = cur_type->BaseType;
    } while (cur_type != NULL);

    return false;
}

static bool verifier_can_cast_to_class_or_interface(RuntimeTypeInfo this_type, RuntimeTypeInfo other_type) {
    VERIFY_ENTER();

    if (tdn_is_interface(other_type)) {
        return VERIFY_LEAVE(verifier_can_cast_to_interface(this_type, other_type));
    } else {
        return VERIFY_LEAVE(verifier_can_cast_to_class(this_type, other_type));
    }
}

static bool verifier_can_cast_generic_parameter_to(RuntimeTypeInfo this_type, RuntimeTypeInfo other_type) {
    VERIFY_ENTER();

    // A boxed variable type can be cast to any of its constraints, or object, if none are specified
    if (other_type == tObject) {
        return VERIFY_LEAVE(true);
    }

    if (
        tdn_has_non_nullable_value_type_constraint(this_type) && other_type == tValueType
    ) {
        return VERIFY_LEAVE(true);
    }

    // TODO: handle generic constraint properly, idk what we would actually end up with in here tbh
    // RuntimeTypeInfo_Array type_instantiation = NULL;
    // RuntimeTypeInfo_Array method_instantiation = NULL;
    // if (this_type->IsGenericMethodParameter) {
    //     type_instantiation = this_type->DeclaringMethod->DeclaringType->GenericArguments;
    //     method_instantiation = this_type->DeclaringMethod->GenericArguments;
    // } else {
    //     type_instantiation = this_type->DeclaringType->GenericArguments;
    // }

    if (this_type->GenericParameterConstraints != NULL) {
        for (int i = 0; i < this_type->GenericParameterConstraints->Length; i++) {
            RuntimeTypeInfo type_constraint = this_type->GenericParameterConstraints->Elements[i];
            ASSERT(!type_constraint->IsGenericParameter, "TODO: this");

            if (verifier_can_cast_to(type_constraint, other_type)) {
                return VERIFY_LEAVE(true);
            }
        }
    }

    return VERIFY_LEAVE(false);
}

bool verifier_can_cast_to(RuntimeTypeInfo this_type, RuntimeTypeInfo other_type) {
    VERIFY_ENTER();

    if (this_type == other_type) {
        return VERIFY_LEAVE(true);
    }

    if (this_type->IsGenericParameter) {
        return VERIFY_LEAVE(verifier_can_cast_generic_parameter_to(this_type, other_type));
    }

    if (this_type->IsByRef || this_type->IsPointer) {
        if (
            this_type->IsByRef != other_type->IsByRef ||
            this_type->IsPointer != other_type->IsPointer
        ) {
            return VERIFY_LEAVE(false);
        }

        return VERIFY_LEAVE(verifier_can_cast_param_to(this_type->ElementType, other_type->ElementType));
    }

    return VERIFY_LEAVE(verifier_can_cast_to_class_or_interface(this_type, other_type));
}
