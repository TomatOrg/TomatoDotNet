#include "types.h"

#include <util/stb_ds.h>

#include "util/except.h"

RuntimeTypeInfo tObject = NULL;
RuntimeTypeInfo tValueType = NULL;
RuntimeTypeInfo tEnum = NULL;

RuntimeTypeInfo tVoid = NULL;
RuntimeTypeInfo tBoolean = NULL;
RuntimeTypeInfo tChar = NULL;

RuntimeTypeInfo tSByte = NULL;
RuntimeTypeInfo tInt16 = NULL;
RuntimeTypeInfo tInt32 = NULL;
RuntimeTypeInfo tInt64 = NULL;
RuntimeTypeInfo tIntPtr = NULL;

RuntimeTypeInfo tByte = NULL;
RuntimeTypeInfo tUInt16 = NULL;
RuntimeTypeInfo tUInt32 = NULL;
RuntimeTypeInfo tUInt64 = NULL;
RuntimeTypeInfo tUIntPtr = NULL;

RuntimeTypeInfo tArray = NULL;
RuntimeTypeInfo tString = NULL;

RuntimeTypeInfo tMethodBase = NULL;
RuntimeTypeInfo tRuntimeAssembly = NULL;
RuntimeTypeInfo tRuntimeModule = NULL;
RuntimeTypeInfo tRuntimeFieldInfo = NULL;
RuntimeTypeInfo tRuntimeMethodBody = NULL;
RuntimeTypeInfo tRuntimeMethodInfo = NULL;
RuntimeTypeInfo tRuntimeConstructorInfo = NULL;
RuntimeTypeInfo tRuntimeLocalVariableInfo = NULL;
RuntimeTypeInfo tRuntimeTypeInfo = NULL;
RuntimeTypeInfo tParameterInfo = NULL;
RuntimeTypeInfo tRuntimeExceptionHandlingClause = NULL;
RuntimeTypeInfo tRuntimeTypeHandle = NULL;

RuntimeTypeInfo tNullable = NULL;

RuntimeTypeInfo tUnsafe = NULL;
RuntimeTypeInfo tMemoryMarshal = NULL;
RuntimeTypeInfo tBuffer = NULL;
RuntimeTypeInfo tBitOperations = NULL;
RuntimeTypeInfo tDebug = NULL;

RuntimeTypeInfo tInAttribute = NULL;
RuntimeTypeInfo tIsVolatile = NULL;
RuntimeTypeInfo tIsReadOnlyAttribute = NULL;
RuntimeTypeInfo tIsByRefLikeAttribute = NULL;
RuntimeTypeInfo tUnmanagedType = NULL;

static bool has_common_subtype(RuntimeTypeInfo T, RuntimeTypeInfo U) {
    if (!tdn_type_is_referencetype(T) || !tdn_type_is_referencetype(U)) {
        return false;
    }

    RuntimeTypeInfo V = T;
    while (V != NULL) {
        if (V == U) {
            return true;
        }
        V = V->BaseType;
    }

    return false;
}

RuntimeTypeInfo tdn_get_underlying_type(RuntimeTypeInfo type) {
    if (type->BaseType == tEnum) {
        return type->EnumUnderlyingType;
    } else {
        return type;
    }
}

RuntimeTypeInfo tdn_get_reduced_type(RuntimeTypeInfo type) {
    type = tdn_get_underlying_type(type);
    if (type == tByte) return tSByte;
    if (type == tUInt16) return tInt16;
    if (type == tUInt32) return tInt32;
    if (type == tUInt64) return tInt64;
    if (type == tUIntPtr) return tIntPtr;
    return type;
}

RuntimeTypeInfo tdn_get_verification_type(RuntimeTypeInfo type) {
    type = tdn_get_reduced_type(type);

    if (type == tBoolean) return tSByte;
    if (type == tChar) return tInt16;

    if (type->IsByRef) {
        type = tdn_get_reduced_type(type->ElementType);

        if (type == tBoolean) {
            tdn_get_byref_type(tSByte, &type);
            return type;
        }

        if (type == tChar) {
            tdn_get_byref_type(tInt16, &type);
            return type;
        }

        tdn_get_byref_type(type, &type);
        return type;
    }

    return type;
}

RuntimeTypeInfo tdn_get_intermediate_type(RuntimeTypeInfo type) {
    type = tdn_get_verification_type(type);

    if (type == tSByte || type == tInt16) return tInt32;
    return type;
}

RuntimeTypeInfo tdn_get_direct_base_class(RuntimeTypeInfo T) {
    // 1.
    if (T->IsArray) {
        return tArray;
    }

    // 2.
    if (T->Attributes.Interface) {
        return tObject;
    }

    // TODO: 3.

    // 4.
    return NULL;
}

bool tdn_type_array_element_compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U) {
    RuntimeTypeInfo V = tdn_get_underlying_type(T);
    RuntimeTypeInfo W = tdn_get_underlying_type(U);

    if (tdn_type_compatible_with(V, W)) {
        return true;
    }

    if (tdn_get_reduced_type(V) == tdn_get_reduced_type(W)) {
        return true;
    }

    return false;
}

static void tdn_type_check_generic_parameter() {

}

bool tdn_type_compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U) {
    // 1.
    if (T == U) {
        return true;
    }

    // 2.
    // TODO: is this even correct?
    if (tdn_is_instance(T, U)) {
        return true;
    }

    // 3.
    if (tdn_type_is_referencetype(T) && U == tdn_get_direct_base_class(T)) {
        return true;
    }

    // TODO: 4.

    // 5.
    if (T->IsArray && U->IsArray) {
        RuntimeTypeInfo V = T->ElementType;
        RuntimeTypeInfo W = U->ElementType;
        if (tdn_type_array_element_compatible_with(V, W)) {
            return true;
        }
    }

    // TODO: 6.

    // TODO: 7.

    // 8.
    if (U->Attributes.Interface && tdn_type_is_generic(U)) {
        for (int i = 0; i < hmlen(T->InterfaceImpls); i++) {
            if (T->InterfaceImpls[i].key->GenericTypeDefinition != U->GenericTypeDefinition) continue;

            // check that the variance
            bool matched = true;
            RuntimeTypeInfo base = T->GenericTypeDefinition;
            if (base != NULL && T->GenericArguments != NULL) {
                for (int j = 0; j < T->GenericArguments->Length; j++) {
                    RuntimeTypeInfo Ti = T->GenericArguments->Elements[j];
                    RuntimeTypeInfo Ui = U->GenericArguments->Elements[j];
                    RuntimeTypeInfo base_typ = base->GenericArguments->Elements[j];
                    uint32_t var_i = base_typ->GenericParameterAttributes.Variance;

                    // a. var_i = none (no variance) and Ti is identical to Ui
                    if (var_i == 0) {
                        if (Ti != Ui) {
                            matched = false;
                            break;
                        }
                    }

                    // b. var_i = + (covariance), and T i is compatible-with Ui
                    if (var_i == TDN_GENERIC_PARAM_VARIANCE_COVARIANT) {
                        if (!tdn_type_compatible_with(Ti, Ui)) {
                            matched = false;
                            break;
                        }
                    }

                    // c. var_i = - (contravariance), and Ui is compatible-with Ti
                    if (var_i == TDN_GENERIC_PARAM_VARIANCE_CONTRAVARIANT) {
                        if (!tdn_type_compatible_with(Ui, Ti)) {
                            matched = false;
                            break;
                        }
                    }
                }
            }

            // matches everything
            if (matched) {
                return true;
            }
        }
    }

    // TODO: 9.

    return false;
}

bool tdn_type_pointer_element_compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U) {
    RuntimeTypeInfo V = tdn_get_verification_type(T);
    RuntimeTypeInfo W = tdn_get_verification_type(U);
    return V == W;
}

bool tdn_type_compatible_with_location(RuntimeTypeInfo T, RuntimeTypeInfo U) {
    if ((!T->IsByRef || !U->IsByRef) && tdn_type_compatible_with(T, U)) {
        return true;
    }

    if (T->IsByRef && U->IsByRef && tdn_type_pointer_element_compatible_with(T, U)) {
        return true;
    }

    return false;
}

bool tdn_type_assignable_to(RuntimeTypeInfo T, RuntimeTypeInfo U) {
    // 1.
    if (T == U) {
        return true;
    }

    // 2.
    // TODO: is this even correct?
    if (T->BaseType != NULL && tdn_type_assignable_to(T->BaseType, U)) {
        return true;
    }

    // 3.
    RuntimeTypeInfo V = tdn_get_intermediate_type(T);
    RuntimeTypeInfo W = tdn_get_intermediate_type(U);
    if (V == W) {
        return true;
    }

    // 4.
    if (
        (V == tIntPtr && W == tInt32) ||
        (W == tIntPtr && V == tInt32)
    ) {
        return true;
    }

    // 5.
    if (tdn_type_compatible_with(T, U)) {
        return true;
    }

    return false;
}

tdn_err_t tdn_check_generic_argument_constraints(RuntimeTypeInfo arg_type, GenericParameterAttributes attributes, RuntimeTypeInfo_Array constraints) {
    tdn_err_t err = TDN_NO_ERROR;

    // special constraints
    if (attributes.SpecialConstraint & TDN_GENERIC_PARAM_CONSTRAINT_REFERENCE_TYPE) {
        CHECK(!tdn_type_is_valuetype(arg_type));
    }

    if (attributes.SpecialConstraint & TDN_GENERIC_PARAM_CONSTRAINT_NON_NULLABLE_VALUE_TYPE) {
        CHECK(tdn_type_is_valuetype(arg_type));
        CHECK(arg_type->GenericTypeDefinition != tNullable);
    }

    if (attributes.SpecialConstraint & TDN_GENERIC_PARAM_CONSTRAINT_DEFAULT_CONSTRUCTOR) {
        bool found = false;
        for (int j = 0; j < arg_type->DeclaredConstructors->Length; j++) {
            RuntimeConstructorInfo ctor = arg_type->DeclaredConstructors->Elements[j];
            if (ctor->Attributes.Static) continue;
            if (ctor->Parameters->Length != 0) continue;
            found = true;
            break;
        }
        CHECK(found);
    }

    if (constraints != NULL) {
        for (int i = 0; i < constraints->Length; i++) {
            RuntimeTypeInfo constraint = constraints->Elements[i];
            if (constraint == tUnmanagedType) {
                CHECK(arg_type->IsUnmanaged);
            } else {
                CHECK(tdn_is_instance(arg_type, constraint));
            }
        }
    }

cleanup:
    return err;
}

bool tdn_is_instance(RuntimeTypeInfo type, RuntimeTypeInfo base) {
    if (base->Attributes.Interface) {
        // go over the interface impls
        for (int i = 0; i < hmlen(type->InterfaceImpls); i++) {
            if (type->InterfaceImpls[i].key == base) {
                return true;
            }
        }
    } else {
        // go over the inheritance
        do {
            if (type == base) {
                return true;
            }
            type = type->BaseType;
        } while (type != NULL);
    }
    return false;
}
