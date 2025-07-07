#pragma once

#include "tomatodotnet/types/type.h"

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

extern RuntimeTypeInfo tSingle;
extern RuntimeTypeInfo tDouble;

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
extern RuntimeTypeInfo tRuntimeExceptionHandlingClause;
extern RuntimeTypeInfo tRuntimeFieldHandle;
extern RuntimeTypeInfo tRuntimeMethodHandle;
extern RuntimeTypeInfo tRuntimeTypeHandle;

extern RuntimeTypeInfo tNullable;

extern RuntimeTypeInfo tUnsafe;
extern RuntimeTypeInfo tMemoryMarshal;

extern RuntimeTypeInfo tInAttribute;
extern RuntimeTypeInfo tIsReadOnlyAttribute;
extern RuntimeTypeInfo tIsByRefLikeAttribute;
extern RuntimeTypeInfo tIsVolatile;
extern RuntimeTypeInfo tUnmanagedType;

extern RuntimeTypeInfo tDelegate;
extern RuntimeTypeInfo tMulticastDelegate;

/**
 * Check the argument constraint against the actual generic type
 */
tdn_err_t tdn_check_generic_argument_constraints(
    RuntimeTypeInfo arg_type,
    GenericParameterAttributes attributes,
    RuntimeTypeInfo_Array constraints,
    RuntimeTypeInfo_Array typeArgs,
    RuntimeTypeInfo_Array methodArgs
);

/**
 * Check if a type is an instance of another type dynamically, works for both interfaces and class bases
 *
 * This does not use the fast checkings but instead goes over the type reflection, this ensures that the check
 * works even before full type initialization is complete
 *
 * This can also handle un-expanded variants by using typeArgs and methodArgs
 */
bool tdn_is_instance(
    RuntimeTypeInfo type,
    RuntimeTypeInfo base,
    RuntimeTypeInfo_Array typeArgs,
    RuntimeTypeInfo_Array methodArgs
);
