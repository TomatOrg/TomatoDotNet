#include "casting.h"

#include "dotnet/types.h"
#include "tomatodotnet/util/stb_ds.h"

RuntimeTypeInfo verifier_get_underlying_type(RuntimeTypeInfo type) {
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

static bool verifier_can_cast_to_class_or_interface(RuntimeTypeInfo this_type, RuntimeTypeInfo other_type) {
    // TODO: variance support

    if (tdn_is_interface(other_type)) {
        if (hmgeti(this_type->InterfaceImpls, other_type) >= 0) {
            return true;
        }
    } else {
        if (tdn_is_interface(this_type) && other_type == tObject) {
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
