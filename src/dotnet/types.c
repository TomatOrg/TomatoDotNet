#include "types.h"

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

RuntimeTypeInfo tIndexOutOfRangeException = NULL;
RuntimeTypeInfo tNullReferenceException = NULL;
RuntimeTypeInfo tOverflowException = NULL;

RuntimeTypeInfo tNull = NULL;

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

bool tdn_type_compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U) {
    // 1.
    if (T == U) {
        return true;
    }

    // 2.
    // TODO: is this even correct?
    if (T->BaseType != NULL && tdn_type_compatible_with(T->BaseType, U)) {
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

    // TODO: 8.

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

bool tdn_type_verifier_assignable_to(RuntimeTypeInfo Q, RuntimeTypeInfo R) {
    RuntimeTypeInfo T = tdn_get_verification_type(Q);
    RuntimeTypeInfo U = tdn_get_verification_type(R);

    // 1.
    if (T == U) {
        return true;
    }

    // 2.
    // TODO: is this even correct?
    if (T->BaseType != NULL && tdn_type_verifier_assignable_to(T->BaseType, U)) {
        return true;
    }

    // 3.
    if (tdn_type_assignable_to(T, U)) {
        return true;
    }

    // TODO: 4 need controlled-mutability

    // TODO: 5 need controlled-mutability

    // TODO: 6 need boxed

    // TODO: 7 need boxed

    // TODO: 8 need boxed

    // 9.
    if (T == tNull && tdn_type_is_referencetype(U)) {
        return true;
    }

    return false;
}
