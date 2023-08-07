#pragma once

#include "tinydotnet/types/type.h"

// base types that can be used
extern RuntimeTypeInfo tObject;
extern RuntimeTypeInfo tValueType;
extern RuntimeTypeInfo tEnum;

extern RuntimeTypeInfo tVoid;
extern RuntimeTypeInfo tBoolean;
extern RuntimeTypeInfo tChar;

extern RuntimeTypeInfo tSByte;
extern RuntimeTypeInfo tInt16;
extern RuntimeTypeInfo tInt32;
extern RuntimeTypeInfo tInt64;
extern RuntimeTypeInfo tIntPtr;

extern RuntimeTypeInfo tByte;
extern RuntimeTypeInfo tUInt16;
extern RuntimeTypeInfo tUInt32;
extern RuntimeTypeInfo tUInt64;
extern RuntimeTypeInfo tUIntPtr;

extern RuntimeTypeInfo tArray;
extern RuntimeTypeInfo tString;

extern RuntimeTypeInfo tMethodBase;
extern RuntimeTypeInfo tRuntimeAssembly;
extern RuntimeTypeInfo tRuntimeModule;
extern RuntimeTypeInfo tRuntimeFieldInfo;
extern RuntimeTypeInfo tRuntimeMethodBody;
extern RuntimeTypeInfo tRuntimeMethodInfo;
extern RuntimeTypeInfo tRuntimeConstructorInfo;
extern RuntimeTypeInfo tRuntimeLocalVariableInfo;
extern RuntimeTypeInfo tRuntimeTypeInfo;
extern RuntimeTypeInfo tParameterInfo;

RuntimeTypeInfo tdn_get_underlying_type(RuntimeTypeInfo type);
RuntimeTypeInfo tdn_get_reduced_type(RuntimeTypeInfo type);
RuntimeTypeInfo tdn_get_verification_type(RuntimeTypeInfo type);
RuntimeTypeInfo tdn_get_intermediate_type(RuntimeTypeInfo type);
