#include "builtin.h"

#include <dotnet/types.h>
#include <tomatodotnet/types/type.h>
#include <util/except.h>
#include "tomatodotnet/util/stb_ds.h"

#include "emit.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Delegate methods
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static spidir_value_t emit_delegate_ctor(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    // store the instance
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, args[1],
        spidir_builder_build_ptroff(builder, args[0],
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Instance))));

    // store the function pointer
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, args[2],
        spidir_builder_build_ptroff(builder, args[0],
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Function))));

    return SPIDIR_VALUE_INVALID;
}

static spidir_value_t emit_delegate_invoke(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    // prepare the arguments for the signature
    spidir_value_type_t* arg_types = jit_get_spidir_arg_types(method);

    // get the function from the delegate
    spidir_value_t func = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
        spidir_builder_build_ptroff(builder, args[0],
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Function))));

    // now load the instance and replace it as the first argument to pass to the function
    args[0] = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
        spidir_builder_build_ptroff(builder, args[0],
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Instance))));

    // and perform the indirect call
    spidir_value_t ret_value = spidir_builder_build_callind(
        builder,
        jit_get_spidir_ret_type(method),
        arrlen(arg_types), arg_types,
        func,
        args
    );
    arrfree(arg_types);

    // return whatever it returned
    return ret_value;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Unsafe methods
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Takes in a reference and outputs a reference, the types might be completely different and we don't
 * perform any kind of type checking
 */
static spidir_value_t emit_unsafe_as(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    return args[0];
}

/**
 * Takes in a reference and outputs a reference, the types might be completely different and we don't
 * perform any kind of type checking
 */
static spidir_value_t emit_unsafe_add_byte_offset(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    return spidir_builder_build_ptroff(builder, args[0], args[1]);
}

static spidir_value_t emit_unsafe_sub_byte_offset(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    spidir_value_t zero = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 0);
    return spidir_builder_build_ptroff(builder, args[0],
        spidir_builder_build_isub(builder, zero, args[1]));
}

static spidir_value_t emit_unsafe_byte_offset(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    return spidir_builder_build_isub(builder,
        spidir_builder_build_ptrtoint(builder, args[0]),
        spidir_builder_build_ptrtoint(builder, args[1]));
}

static spidir_value_t emit_unsafe_is_address_lt(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    return spidir_builder_build_icmp(builder, SPIDIR_ICMP_ULT, SPIDIR_TYPE_I32, args[0], args[1]);
}

static spidir_value_t emit_unsafe_are_same(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    return spidir_builder_build_icmp(builder, SPIDIR_ICMP_EQ, SPIDIR_TYPE_I32, args[0], args[1]);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Memory marshal methods
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static spidir_value_t emit_memory_marshal_get_array_data_reference(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    size_t data_offset = tdn_get_array_elements_offset(method->GenericArguments->Elements[0]);
    return spidir_builder_build_ptroff(builder, args[0],
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, data_offset));
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Runtime Helpers methods
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static spidir_value_t emit_runtime_helpers_is_reference_or_contains_references(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    return spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, !method->GenericArguments->Elements[0]->IsUnmanaged);
}

static spidir_value_t emit_runtime_helpers_is_bitwise_equatable(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    RuntimeTypeInfo element_type = method->GenericArguments->Elements[0];
    // This logic matches the coreclr logic, just so we will be consistent with them
    if (
        element_type == tBoolean ||
        element_type == tByte ||
        element_type == tSByte ||
        element_type == tChar ||
        element_type == tInt16 ||
        element_type == tUInt16 ||
        element_type == tInt32 ||
        element_type == tUInt32 ||
        element_type == tInt64 ||
        element_type == tUInt64 ||
        element_type == tIntPtr ||
        element_type == tUIntPtr ||
        element_type->BaseType == tEnum
        // TODO: also allow for value types that do not implement the IEquatable or override Equals
        // TODO: can't we also do this on objects that don't override it? since it will just need to
        // TODO: check for the pointers?
    ) {
        return spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 1);
    } else {
        return spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 0);
    }
}

static spidir_value_t emit_runtime_helpers_get_offset_to_string_data(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    return spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, offsetof(struct String, Chars));
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Object methods
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static spidir_value_t emit_object_get_hash_code(spidir_builder_handle_t builder, RuntimeMethodBase method, spidir_value_t* args) {
    // convert the pointer to an integer
    spidir_value_t ptr = spidir_builder_build_ptrtoint(builder, args[0]);

    // ((int)value) ^ (int)(value >> 32)
    return spidir_builder_build_xor(builder,
        spidir_builder_build_itrunc(builder, ptr),
        spidir_builder_build_itrunc(builder,
            spidir_builder_build_lshr(builder, ptr,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 32))));
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Generic emit code
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

jit_builtin_emitter_t jit_get_builtin_emitter(RuntimeMethodBase method) {
    RuntimeTypeInfo type = method->DeclaringType;

    if (type->BaseType == tMulticastDelegate) {
        if (tdn_compare_string_to_cstr(method->Name, ".ctor")) {
            return emit_delegate_ctor;
        }

        if (tdn_compare_string_to_cstr(method->Name, "Invoke")) {
            return emit_delegate_invoke;
        }

    } else if (type == tUnsafe) {
        if (
            tdn_compare_string_to_cstr(method->Name, "As") ||
            tdn_compare_string_to_cstr(method->Name, "AsRef") ||
            tdn_compare_string_to_cstr(method->Name, "AsPointer")
        ) {
            return emit_unsafe_as;
        } else if (tdn_compare_string_to_cstr(method->Name, "AddByteOffset")) {
            return emit_unsafe_add_byte_offset;
        } else if (tdn_compare_string_to_cstr(method->Name, "SubtractByteOffset")) {
            return emit_unsafe_sub_byte_offset;
        } else if (tdn_compare_string_to_cstr(method->Name, "ByteOffset")) {
            return emit_unsafe_byte_offset;
        } else if (tdn_compare_string_to_cstr(method->Name, "IsAddressLessThan")) {
            return emit_unsafe_is_address_lt;
        } else if (tdn_compare_string_to_cstr(method->Name, "AreSame")) {
            return emit_unsafe_are_same;
        }

    } else if (type == tMemoryMarshal) {
        if (tdn_compare_string_to_cstr(method->Name, "GetArrayDataReference")) {
            return emit_memory_marshal_get_array_data_reference;
        }

    } else if (type == tRuntimeHelpers) {
        if (tdn_compare_string_to_cstr(method->Name, "IsReferenceOrContainsReferences")) {
            return emit_runtime_helpers_is_reference_or_contains_references;
        } else if (tdn_compare_string_to_cstr(method->Name, "IsBitwiseEquatable")) {
            return emit_runtime_helpers_is_bitwise_equatable;
        } else if (tdn_compare_string_to_cstr(method->Name, "get_OffsetToStringData")) {
            return emit_runtime_helpers_get_offset_to_string_data;
        }

    } else if (type == tObject) {
        if (tdn_compare_string_to_cstr(method->Name, "GetHashCode")) {
            return emit_object_get_hash_code;
        }
    }

    // not matched to anything
    return NULL;
}

void jit_emit_builtin(spidir_builder_handle_t builder, void* _ctx) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_builtin_context_t* ctx = _ctx;
    RuntimeMethodBase method = ctx->method;
    spidir_value_t* args = NULL;

    // prepare the main block
    spidir_block_t block = spidir_builder_create_block(builder);
    spidir_builder_set_block(builder, block);
    spidir_builder_set_entry_block(builder, block);

    // get the emitter
    jit_builtin_emitter_t emitter = jit_get_builtin_emitter(method);
    CHECK(emitter != NULL, "Unknown builtin method %T::%U", method->DeclaringType, method->Name);

    // gather the arguments
    int i = 0;
    if (!method->Attributes.Static) {
        arrpush(args, spidir_builder_build_param_ref(builder, i++));
    }
    for (int j = 0; j < method->Parameters->Length; j++) {
        arrpush(args, spidir_builder_build_param_ref(builder, i++));
    }

    // now emit it and return the value as it was
    spidir_value_t value = emitter(builder, method, args);
    spidir_builder_build_return(builder, value);

cleanup:
    arrfree(args);
    ctx->err = err;
}
