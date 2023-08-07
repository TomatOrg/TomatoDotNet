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
    }

    return type;
}

RuntimeTypeInfo tdn_get_intermediate_type(RuntimeTypeInfo type) {
    type = tdn_get_verification_type(type);

    if (type == tSByte || type == tInt16) return tInt32;
    return type;
}
