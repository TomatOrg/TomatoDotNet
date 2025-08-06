#include "spidir.h"

#include "dotnet/types.h"
#include "tomatodotnet/util/stb_ds.h"
#include "util/except.h"

spidir_value_type_t* jit_get_spidir_arg_types(RuntimeMethodBase method) {
    spidir_value_type_t* types = NULL;
    for (int i = 0; i < method->Parameters->Length; i++) {
        arrpush(types, jit_get_spidir_type(method->Parameters->Elements[i]->ParameterType));
    }
    return types;
}

spidir_value_type_t jit_get_spidir_ret_type(RuntimeMethodBase method) {
    RuntimeTypeInfo type = method->ReturnParameter->ParameterType;

    if (type == tVoid || jit_is_struct_like(type)) {
        // either a void or a struct, for structs we pass the location
        // from the caller as a last parameter
        return SPIDIR_TYPE_NONE;

    } else {
        return jit_get_spidir_type(type);

    }
}

spidir_value_type_t jit_get_spidir_type(RuntimeTypeInfo type) {
    if (
        type == tBoolean ||
        type == tChar ||
        type == tSByte ||
        type == tByte ||
        type == tInt16 ||
        type == tUInt16 ||
        type == tInt32 ||
        type == tUInt32
    ) {
        return SPIDIR_TYPE_I32;

    } else if (
        type == tInt64 || type == tUInt64 ||
        type == tIntPtr || type == tUIntPtr
    ) {
        return SPIDIR_TYPE_I64;

    } else if (type == tDouble) {
        return SPIDIR_TYPE_F64;

    } else if (type == tSingle) {
        ASSERT(!"TODO: float support");

    } else if (type->BaseType == tEnum) {
        return jit_get_spidir_type(type->EnumUnderlyingType);

    } else {
        // structs, references, by-refs are all passed by
        // a pointer
        return SPIDIR_TYPE_PTR;

    }
}
