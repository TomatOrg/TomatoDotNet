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
extern RuntimeTypeInfo tBuffer;
extern RuntimeTypeInfo tBitOperations;
extern RuntimeTypeInfo tDebug;

extern RuntimeTypeInfo tInAttribute;
extern RuntimeTypeInfo tIsReadOnlyAttribute;
extern RuntimeTypeInfo tIsByRefLikeAttribute;
extern RuntimeTypeInfo tIsVolatile;
extern RuntimeTypeInfo tUnmanagedType;

extern RuntimeTypeInfo tDelegate;
extern RuntimeTypeInfo tMulticastDelegate;

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

/**
 * Check the argument constraint against the actual generic type
 */
tdn_err_t tdn_check_generic_argument_constraints(RuntimeTypeInfo arg_type, GenericParameterAttributes attributes, RuntimeTypeInfo_Array constraints);

/**
 * Check if a type is an instance of another type dynamically, works for both interfaces and class bases
 *
 * This does not use the fast checkings but instead goes over the type reflection, this ensures that the check
 * works even before full type initialization is complete
 */
bool tdn_is_instance(RuntimeTypeInfo type, RuntimeTypeInfo base);
