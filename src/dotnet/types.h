#pragma once

#include "tomatodotnet/types/type.h"
#include "util/defs.h"

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
extern RuntimeTypeInfo tSpan;

extern RuntimeTypeInfo tUnsafe;
extern RuntimeTypeInfo tMemoryMarshal;
extern RuntimeTypeInfo tRuntimeHelpers;

extern RuntimeTypeInfo tInAttribute;
extern RuntimeTypeInfo tIsReadOnlyAttribute;
extern RuntimeTypeInfo tIsByRefLikeAttribute;
extern RuntimeTypeInfo tIsVolatile;
extern RuntimeTypeInfo tUnmanagedType;

extern RuntimeTypeInfo tDelegate;
extern RuntimeTypeInfo tMulticastDelegate;

static inline bool tdn_is_interface(RuntimeTypeInfo type) {
    return type != NULL && type->Attributes.Interface;
}

static inline bool tdn_is_delegate(RuntimeTypeInfo type) {
    return type != NULL && type->BaseType == tMulticastDelegate;
}

static inline bool tdn_is_struct(RuntimeTypeInfo type) {
    // Anything which is not a value type but not a
    // native type is a struct
    return tdn_type_is_valuetype(type) && !type->IsByRef &&
            type != tByte && type != tSByte &&
            type != tInt16 && type != tUInt16 &&
            type != tInt32 && type != tUInt32 &&
            type != tInt64 && type != tUInt64 &&
            type != tIntPtr && type != tUIntPtr &&
            type != tBoolean && type != tChar &&
            type != tSingle && type != tDouble &&
            type->BaseType != tEnum;
}

/**
 * ABI wise, does this type behave like a struct, includes value types
 * and fat pointers like interfaces and delegates
 */
static inline bool tdn_is_struct_like(RuntimeTypeInfo type) {
    return tdn_is_interface(type) || tdn_is_struct(type) || tdn_is_delegate(type);
}

/**
 * Get the offset into the boxed value for the given type
 * assumes its a value type
 */
static inline size_t tdn_get_boxed_value_offset(RuntimeTypeInfo type) {
    return ALIGN_UP(sizeof(struct Object), type->StackAlignment);
}

/**
 * Get the offset into the data of an array of the given element type
 */
static inline size_t tdn_get_array_elements_offset(RuntimeTypeInfo element_type) {
    return ALIGN_UP(sizeof(struct Array), element_type->StackAlignment);
}

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
