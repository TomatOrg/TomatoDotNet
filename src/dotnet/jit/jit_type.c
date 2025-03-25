#include "jit_type.h"

#include <dotnet/types.h>
#include <util/except.h>
#include <util/stb_ds.h>

RuntimeTypeInfo verifier_direct_base_class(RuntimeTypeInfo T) {
    if (T == NULL) {
        return NULL;
    }

    if (T->IsArray) {
        return tArray;
    }

    if (T->Attributes.Interface) {
        return tObject;
    }

    if (!tdn_type_is_valuetype(T->BaseType) && T->BaseType != NULL) {
        return T->BaseType;
    }

    return NULL;
}

RuntimeTypeInfo verifier_get_underlying_type(RuntimeTypeInfo type) {
    if (type->BaseType == tEnum) {
        return type->EnumUnderlyingType;
    } else {
        return type;
    }
}

RuntimeTypeInfo verifier_get_reduced_type(RuntimeTypeInfo T) {
    if (T == NULL) return NULL;

    T = verifier_get_underlying_type(T);
    if (T == tByte) {
        T = tSByte;
    } else if (T == tUInt16) {
        T = tInt16;
    } else if (T == tUInt32) {
        T = tInt32;
    } else if (T == tUInt64) {
        T = tInt64;
    } else if (T == tUIntPtr) {
        T = tIntPtr;
    }

    return T;
}

RuntimeTypeInfo verifier_get_verification_type(RuntimeTypeInfo T) {
    if (T == NULL) return NULL;
    RuntimeTypeInfo T_reduced = verifier_get_reduced_type(T);

    if (T_reduced == tSByte || T_reduced == tBoolean) return tSByte;
    if (T_reduced == tInt16 || T_reduced == tChar) return tInt16;
    if (T_reduced == tInt32) return tInt32;
    if (T_reduced == tInt64) return tInt64;

    if (T->IsByRef) {
        RuntimeTypeInfo S = T->ElementType;
        RuntimeTypeInfo S_reduced = verifier_get_reduced_type(S);

        if (S_reduced == tSByte || S_reduced == tBoolean) {
            RuntimeTypeInfo result = NULL;
            ASSERT(!IS_ERROR(tdn_get_byref_type(tSByte, &result)));
            return result;
        }

        if (S_reduced == tInt16 || S_reduced == tChar) {
            RuntimeTypeInfo result = NULL;
            ASSERT(!IS_ERROR(tdn_get_byref_type(tInt16, &result)));
            return result;
        }

        if (S_reduced == tInt32) {
            RuntimeTypeInfo result = NULL;
            ASSERT(!IS_ERROR(tdn_get_byref_type(tInt32, &result)));
            return result;
        }

        if (S_reduced == tInt64) {
            RuntimeTypeInfo result = NULL;
            ASSERT(!IS_ERROR(tdn_get_byref_type(tInt64, &result)));
            return result;
        }

        if (S_reduced == tIntPtr) {
            RuntimeTypeInfo result = NULL;
            ASSERT(!IS_ERROR(tdn_get_byref_type(tIntPtr, &result)));
            return result;
        }
    }

    return T;
}

RuntimeTypeInfo verifier_get_intermediate_type(RuntimeTypeInfo T) {
    RuntimeTypeInfo T_verification = verifier_get_verification_type(T);
    if (
        T_verification == tSByte ||
        T_verification == tInt16 ||
        T_verification == tInt32
    ) {
        return tInt32;
    }

    // TODO: floating point

    return T_verification;
}

bool verifier_compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U) {
    // 1. T is identical to U.
    if (T == U) {
        return true;
    }

    // 2. There exists some V such that T is compatible-to V and V is compatible-to U
    RuntimeTypeInfo V = verifier_direct_base_class(T);
    if (V != NULL) {
        if (verifier_compatible_with(V, U)) {
            return true;
        }
    }

    // 3. T is a reference type, and U is the direct base class of T.
    if (tdn_type_is_referencetype(T) && U == verifier_direct_base_class(T)) {
        return true;
    }

    // 4. T is a reference type, and U is an interface directly implemented by T.
    if (T != NULL && U != NULL && tdn_type_is_referencetype(T) && U->Attributes.Interface) {
        for (int i = 0; i < hmlen(T->InterfaceImpls); i++) {
            if (T->InterfaceImpls[i].key == U) {
                return true;
            }
        }
    }

    // TODO: 5.

    // TODO: 6.

    // TODO: 7.

    // TODO: 8.

    // TODO: 9.

    return false;
}

static bool assignable_to(RuntimeTypeInfo T, RuntimeTypeInfo U) {
    // 1. T is identical to U.
    if (T == U) {
        return true;
    }

    // TODO: 2. There exists some V such that T is assignable-to V and V is assignable-to U

    // 3. T has intermediate type V, U has intermediate type W, and V is identical to W.
    RuntimeTypeInfo V = verifier_get_intermediate_type(T);
    RuntimeTypeInfo W = verifier_get_intermediate_type(U);
    if (V == W) {
        return true;
    }

    // 4. T has intermediate type native int and U has intermediate type int32, or vice-versa.
    if (
        (V == tIntPtr && W == tInt32) ||
        (V == tInt32 && W == tIntPtr)
    ) {
        return true;
    }

    // 5. T is compatible-with U.
    if (verifier_compatible_with(T, U)) {
        return true;
    }

    return false;
}

bool verifier_assignable_to(RuntimeTypeInfo Q, RuntimeTypeInfo R) {
    RuntimeTypeInfo T = verifier_get_verification_type(Q);
    RuntimeTypeInfo U = verifier_get_verification_type(R);

    // 1. T is identical to U
    if (T == U) {
        return true;
    }

    // TODO: 2. There exists some V such that T is verifier-assignable-to V and V is verifier-assignable-to U

    // 3. T is assignable-to U
    if (assignable_to(T, U)) {
        return true;
    }

    // TODO: controlled-mutability pointer

    // TODO: boxed variations

    // 9. T is the null type, and U is a reference type.
    if (T == NULL && tdn_type_is_referencetype(U)) {
        return true;
    }

    return false;
}

bool verifier_array_element_compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U) {
    // TODO: the original definition makes no sense?!
    // RuntimeTypeInfo V = verifier_get_underlying_type(T);
    // RuntimeTypeInfo W = verifier_get_underlying_type(U);
    RuntimeTypeInfo V = verifier_get_intermediate_type(T);
    RuntimeTypeInfo W = verifier_get_intermediate_type(U);

    if (verifier_compatible_with(V, W)) {
        return true;
    }

    // if (verifier_get_reduced_type(V) == verifier_get_reduced_type(W)) {
    //     return true;
    // }

    return false;
}
