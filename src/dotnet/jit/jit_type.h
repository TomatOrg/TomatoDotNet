#pragma once

#include <dotnet/types.h>
#include <tomatodotnet/tdn.h>
#include <util/defs.h>
#include <util/except.h>

//----------------------------------------------------------------------------------------------------------------------
// Type kind helpers
//----------------------------------------------------------------------------------------------------------------------

static inline bool jit_is_interface(RuntimeTypeInfo type) {
    return type != NULL && type->Attributes.Interface;
}

static inline bool jit_is_delegate(RuntimeTypeInfo type) {
    return type != NULL && type->BaseType == tMulticastDelegate;
}

static inline bool jit_is_struct(RuntimeTypeInfo type) {
    // Anything which is not a value type but not a
    // native type is a struct
    // TODO: Floating point
    return tdn_type_is_valuetype(type) && !type->IsByRef &&
            type != tByte && type != tSByte &&
            type != tInt16 && type != tUInt16 &&
            type != tInt32 && type != tUInt32 &&
            type != tInt64 && type != tUInt64 &&
            type != tIntPtr && type != tUIntPtr &&
            type != tBoolean && type != tChar &&
            type != tSingle && type != tDouble;
}

static inline bool jit_is_struct_like(RuntimeTypeInfo type) {
    return jit_is_interface(type) || jit_is_struct(type) || jit_is_delegate(type);
}

//----------------------------------------------------------------------------------------------------------------------
// Offset helpers
//----------------------------------------------------------------------------------------------------------------------

static inline size_t jit_get_boxed_value_offset(RuntimeTypeInfo type) {
    return ALIGN_UP(sizeof(struct Object), type->StackAlignment);
}

static inline size_t jit_get_array_elements_offset(RuntimeTypeInfo element_type) {
    return ALIGN_UP(sizeof(struct Array), element_type->StackAlignment);
}
