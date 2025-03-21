#pragma once

#include <dotnet/types.h>
#include <tomatodotnet/tdn.h>
#include <util/defs.h>
#include <util/except.h>

//----------------------------------------------------------------------------------------------------------------------
// Type verification helpers
//----------------------------------------------------------------------------------------------------------------------

RuntimeTypeInfo verifier_get_underlying_type(RuntimeTypeInfo type);
RuntimeTypeInfo verifier_direct_base_class(RuntimeTypeInfo T);
RuntimeTypeInfo verifier_get_reduced_type(RuntimeTypeInfo T);
RuntimeTypeInfo verifier_get_verification_type(RuntimeTypeInfo T);
RuntimeTypeInfo verifier_get_intermediate_type(RuntimeTypeInfo T);

bool verifier_compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U);
bool verifier_assignable_to(RuntimeTypeInfo Q, RuntimeTypeInfo R);
bool verifier_array_element_compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U);
bool verifier_pointer_element_compatible_with(RuntimeTypeInfo T, RuntimeTypeInfo U);

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
            type != tBoolean && type != tChar;
}

static inline bool jit_is_struct_like(RuntimeTypeInfo type) {
    return jit_is_interface(type) || jit_is_struct(type) || jit_is_delegate(type);
}

static inline RuntimeTypeInfo jit_get_method_this_type(RuntimeMethodBase method) {
    RuntimeTypeInfo type = method->DeclaringType;
    if (tdn_type_is_valuetype(type)) {
        ASSERT(!IS_ERROR(tdn_get_byref_type(type, &type)));
    }
    return type;
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
