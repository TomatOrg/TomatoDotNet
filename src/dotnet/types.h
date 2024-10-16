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
extern RuntimeTypeInfo tRuntimeTypeHandle;

extern RuntimeTypeInfo tNullable;

extern RuntimeTypeInfo tUnsafe;
extern RuntimeTypeInfo tMemoryMarshal;

extern RuntimeTypeInfo tInAttribute;
extern RuntimeTypeInfo tIsReadOnlyAttribute;
extern RuntimeTypeInfo tIsVolatile;

RuntimeTypeInfo tdn_get_underlying_type(RuntimeTypeInfo type);
RuntimeTypeInfo tdn_get_reduced_type(RuntimeTypeInfo type);
RuntimeTypeInfo tdn_get_verification_type(RuntimeTypeInfo type);
RuntimeTypeInfo tdn_get_intermediate_type(RuntimeTypeInfo type);

RuntimeTypeInfo tdn_get_direct_base_class(RuntimeTypeInfo T);

bool tdn_type_array_element_compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U);
bool tdn_type_pointer_element_compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U);

bool tdn_type_compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U);
bool tdn_type_compatible_with_location(RuntimeTypeInfo T, RuntimeTypeInfo U);
bool tdn_type_assignable_to(RuntimeTypeInfo T, RuntimeTypeInfo U);

tdn_err_t tdn_check_generic_argument_constraints(RuntimeTypeInfo arg_type, GenericParameterAttributes attributes, RuntimeTypeInfo_Array constraints);