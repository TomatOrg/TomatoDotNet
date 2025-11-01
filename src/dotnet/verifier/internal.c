#include "internal.h"

#include "casting.h"
#include "dotnet/types.h"
#include "tomatodotnet/util/stb_ds.h"
#include "util/except.h"

RuntimeTypeInfo verifier_get_type_definition(RuntimeTypeInfo typ) {
    return typ->GenericTypeDefinition ?: typ;
}

RuntimeMethodBase verifier_get_typical_method_definition(RuntimeMethodBase method) {
    // decompose the generic type
    if (method->GenericMethodDefinition != NULL) {
        method = (RuntimeMethodBase)method->GenericMethodDefinition;
    }

    // lookup the original method by its metadata token
    ASSERT(!IS_ERROR(tdn_assembly_lookup_method(
        method->Module->Assembly, method->MetadataToken,
        NULL, NULL,
        &method
    )));

    return method;
}

stack_value_t* stack_value_init(stack_value_t* value, RuntimeTypeInfo type) {
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
        value->kind = KIND_INT32;
        value->type = tInt32;

    } else if (type == tInt64 || type == tUInt64) {
        value->kind = KIND_INT64;
        value->type = tInt64;

    } else if (type == tSingle || type == tDouble) {
        // for the verifier f32/f64 does not matter
        // so we will just tell it everything is double
        value->kind = KIND_FLOAT;
        value->type = tDouble;

    } else if (type == tIntPtr || type == tUIntPtr) {
        value->kind = KIND_NATIVE_INT;
        value->type = tIntPtr;

    } else if (type->IsPointer) {
        value->kind = KIND_NATIVE_INT;
        value->type = type->ElementType;

    } else if (type->BaseType == tEnum) {
        stack_value_init(value, type->EnumUnderlyingType);

    } else if (type->IsByRef) {
        value->kind = KIND_BY_REF;
        value->type = type->ElementType;

    } else if (tdn_type_is_valuetype(type) || type->IsGenericParameter) {
        value->kind = KIND_VALUE_TYPE;
        value->type = type;

    } else {
        ASSERT(tdn_type_is_gc_pointer(type));
        value->kind = KIND_OBJ_REF;
        value->type = type;

    }

    return value;
}

bool verifier_is_assignable(stack_value_t* src, stack_value_t* dst) {
    // can't store readonly to a non-readonly location
    if (src->flags.ref_read_only && !dst->flags.ref_read_only) {
        return false;
    }

    // can't store a local to a non-local location
    if (!src->flags.ref_non_local && dst->flags.ref_non_local) {
        return false;
    }

    // same type, we good
    if (src->kind == dst->kind && src->type == dst->type) {
        return true;
    }

    // can't store into a null value
    if (dst->type == NULL) {
        return false;
    }

    switch (src->kind) {
        // check if can be casted
        case KIND_OBJ_REF: {
            if (dst->kind != KIND_OBJ_REF) {
                return false;
            }

            // Mull is always assignable
            if (src->type == NULL) {
                return true;
            }

            // and now check if we can perform the cast
            return verifier_can_cast_to(src->type, dst->type);
        } break;

            // must match exactly
        case KIND_INT64:
        case KIND_FLOAT:
        case KIND_VALUE_TYPE:
            return false;

        case KIND_BY_REF: {
            if (dst->kind == KIND_BY_REF && dst->flags.ref_read_only) {
                return src->type == dst->type;
            }

            return false;
        } break;

            // can either be int64 or native int
        case KIND_INT32:
            return dst->kind == KIND_INT64 || dst->kind == KIND_NATIVE_INT;

            // can also be int64
        case KIND_NATIVE_INT:
            return dst->kind == KIND_INT64 || dst->kind == KIND_NATIVE_INT;

        default:
            ASSERT(!"Invalid kind", "%d of %T", src->kind, src->type);
    }
}
