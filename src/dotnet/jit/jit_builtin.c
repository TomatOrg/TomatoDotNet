#include "jit_builtin.h"

#include <dotnet/types.h>
#include <tomatodotnet/types/type.h>
#include <util/except.h>
#include <util/stb_ds.h>

#include "jit_emit.h"
#include "jit_helpers.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// System.Object
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void emit_object_get_type(spidir_builder_handle_t builder) {
    // get the object
    spidir_value_t arg0 = spidir_builder_build_param_ref(builder, 0);

    // deref the vtable pointer and convert to pointer
    spidir_value_t vtable = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64, arg0);
    vtable = spidir_builder_build_inttoptr(builder, vtable);

    // load the type from the vtable
    spidir_value_t type = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, vtable);
    STATIC_ASSERT(offsetof(ObjectVTable, Type) == 0);

    // return it
    spidir_builder_build_return(builder, type);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// System.Runtime.CompilerServices.Unsafe
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void emit_unsafe_as(spidir_builder_handle_t builder) {
    spidir_value_t arg0 = spidir_builder_build_param_ref(builder, 0);
    spidir_builder_build_return(builder, arg0);
}

static void emit_unsafe_are_same(spidir_builder_handle_t builder) {
    spidir_value_t arg0 = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t arg1 = spidir_builder_build_param_ref(builder, 1);
    spidir_builder_build_return(builder,
        spidir_builder_build_icmp(builder,
            SPIDIR_ICMP_EQ, SPIDIR_TYPE_I32, arg0, arg1));
}

static void emit_unsafe_add_byte_offset(spidir_builder_handle_t builder) {
    spidir_value_t arg0 = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t arg1 = spidir_builder_build_param_ref(builder, 1);
    spidir_builder_build_return(builder,
        spidir_builder_build_ptroff(builder, arg0, arg1));
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// System.Runtime.InteropServices.MemoryMarshal
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void emit_memory_marshal_get_array_data_reference(spidir_builder_handle_t builder, RuntimeMethodBase method) {
    spidir_value_t arr = spidir_builder_build_param_ref(builder, 0);
    RuntimeTypeInfo arr_type = method->Parameters->Elements[0]->ParameterType;

    // calculate the offset to the data
    arr = spidir_builder_build_ptroff(
        builder,
        arr,
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64,
            ALIGN_UP(sizeof(struct Array), arr_type->ElementType->StackAlignment))
    );

    spidir_builder_build_return(builder, arr);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// System.Buffer
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void emit_buffer_memmove(spidir_builder_handle_t builder, RuntimeMethodBase method) {
    RuntimeTypeInfo copy_type = method->Parameters->Elements[0]->ParameterType->ElementType;
    RuntimeTypeInfo len_type = method->Parameters->Elements[2]->ParameterType;

    spidir_value_t dst = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t src = spidir_builder_build_param_ref(builder, 1);
    spidir_value_t len = spidir_builder_build_param_ref(builder, 2);

    // zero extend to 64bit if need be
    if (len_type == tUInt32) {
        len = spidir_builder_build_iext(builder, len);
        len = spidir_builder_build_and(builder, len,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, UINT32_MAX));
    }

    // turn into a byte value
    len = spidir_builder_build_imul(builder,
        len,
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64,
            copy_type->StackSize)
    );

    // TODO: use gc_memcpy when applicable
    // TODO: right now we don't do that but we can use versions
    //       that assume certain alignment (or lack there of)

    spidir_builder_build_call(builder,
        g_jit_memcpy,
        3,
        (spidir_value_t[]){
            dst,
            src,
            len
        }
    );

    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// System.Numerics.BitOperations
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// TODO: use spidir opcodes for these

static void emit_bit_operations_leading_zero_count(spidir_builder_handle_t builder, RuntimeMethodBase method) {
    spidir_function_t func;
    if (method->Parameters->Elements[0]->ParameterType == tUInt32) {
        func = g_jit_leading_zero_count_32;
    } else if (method->Parameters->Elements[0]->ParameterType == tUInt64) {
        func = g_jit_leading_zero_count_64;
    } else {
        ASSERT(!"Invalid LeadingZeroCount parameter type");
    }

    spidir_value_t res = spidir_builder_build_call(builder,
        func,
        1,
        (spidir_value_t[]){
            spidir_builder_build_param_ref(builder, 0)
        }
    );
    spidir_builder_build_return(builder, res);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// System.Diagnostics.Debug
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void emit_debug_print(spidir_builder_handle_t builder, RuntimeMethodBase method) {
    RuntimeTypeInfo type = method->Parameters->Elements[0]->ParameterType;

    spidir_function_t func;
    if (type == tString) {
        func = g_jit_print_str;
    } else if (type == tInt32) {
        func = g_jit_print_int;
    } else if (type->IsByRef) {
        func = g_jit_print_ptr;
    } else {
        ASSERT(!"Invalid debug print type");
    }

    spidir_builder_build_call(builder,
        func,
        1,
        (spidir_value_t[]){
            spidir_builder_build_param_ref(builder, 0)
        }
    );
    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Delegate handling
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void emit_delegate_ctor(spidir_builder_handle_t builder) {
    spidir_value_t delegate = spidir_builder_build_param_ref(builder, 0);
    spidir_value_t target = spidir_builder_build_param_ref(builder, 1);
    spidir_value_t method = spidir_builder_build_param_ref(builder, 2);

    // store the instance
    STATIC_ASSERT(offsetof(Delegate, Instance) == 0);
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, target, delegate);

    // store the method
    spidir_value_t method_ptr = spidir_builder_build_ptroff(builder, delegate,
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Function)));
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, method, method_ptr);

    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
}


static void emit_delegate_invoke(spidir_builder_handle_t builder, RuntimeMethodBase method) {
    spidir_value_t delegate = spidir_builder_build_param_ref(builder, 0);

    // load the target and method ptr
    spidir_value_t method_ptr = spidir_builder_build_ptroff(builder, delegate,
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Function)));
    method_ptr = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, method_ptr);
    spidir_value_t target = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, delegate);

    // load all the arguments
    spidir_value_t* args = NULL;
    arrpush(args, target);
    for (int i = 0; i < method->Parameters->Length; i++) {
        arrpush(args, spidir_builder_build_param_ref(builder, i + 1));
    }

    // perform the indirect call
    spidir_value_type_t* arg_types = jit_get_spidir_arg_types(method);
    spidir_value_t result = spidir_builder_build_callind(
        builder,
        jit_get_spidir_ret_type(method),
        arrlen(arg_types), arg_types,
        method_ptr,
        args
    );

    // and return it
    spidir_builder_build_return(builder, result);

    arrfree(arg_types);
    arrfree(args);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Generic emit code
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void jit_emit_builtin(spidir_builder_handle_t handle, void* _ctx) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_builtin_context_t* ctx = _ctx;
    RuntimeMethodBase method = ctx->method;
    RuntimeTypeInfo type = method->DeclaringType;

    // prepare the main block
    spidir_block_t block = spidir_builder_create_block(handle);
    spidir_builder_set_block(handle, block);
    spidir_builder_set_entry_block(handle, block);

    // and now check it
    if (type == tObject) {
        CHECK(tdn_compare_string_to_cstr(method->Name, "GetType"));
        emit_object_get_type(handle);

    } else if (type == tUnsafe) {
        if (tdn_compare_string_to_cstr(method->Name, "As")) {
            emit_unsafe_as(handle);
        } else if (tdn_compare_string_to_cstr(method->Name, "AsRef")) {
            emit_unsafe_as(handle);
        } else if (tdn_compare_string_to_cstr(method->Name, "AreSame")) {
            emit_unsafe_are_same(handle);
        } else if (tdn_compare_string_to_cstr(method->Name, "CopyBlockUnaligned")) {
            emit_buffer_memmove(handle, method);
        } else if (tdn_compare_string_to_cstr(method->Name, "AddByteOffset")) {
            emit_unsafe_add_byte_offset(handle);
        } else {
            CHECK_FAIL("Invalid function %T::%U", method->DeclaringType, method->Name);
        }

    } else if (type == tMemoryMarshal) {
        if (tdn_compare_string_to_cstr(method->Name, "GetArrayDataReference")) {
            emit_memory_marshal_get_array_data_reference(handle, method);
        } else {
            CHECK_FAIL("Invalid function %T::%U", method->DeclaringType, method->Name);
        }

    } else if (type == tBuffer) {
        if (tdn_compare_string_to_cstr(method->Name, "Memmove")) {
            emit_buffer_memmove(handle, method);
        } else {
            CHECK_FAIL("Invalid function %T::%U", method->DeclaringType, method->Name);
        }

    } else if (type == tBitOperations) {
        if (tdn_compare_string_to_cstr(method->Name, "LeadingZeroCount")) {
            emit_bit_operations_leading_zero_count(handle, method);
        } else {
            CHECK_FAIL("Invalid function %T::%U", method->DeclaringType, method->Name);
        }

    } else if (type == tDebug) {
        if (tdn_compare_string_to_cstr(method->Name, "Print")) {
            emit_debug_print(handle, method);
        } else {
            CHECK_FAIL("Invalid function %T::%U", method->DeclaringType, method->Name);
        }

    } else if (type->BaseType == tMulticastDelegate) {
        // this is a delegate instance, the ctor takes
        if (tdn_compare_string_to_cstr(method->Name, ".ctor")) {
            emit_delegate_ctor(handle);

        } else if (tdn_compare_string_to_cstr(method->Name, "Invoke")) {
            emit_delegate_invoke(handle, method);

        } else {
            CHECK_FAIL("Invalid function %T::%U", method->DeclaringType, method->Name);
        }

    } else {
        CHECK_FAIL("Invalid function %T::%U", method->DeclaringType, method->Name);
    }

cleanup:
    ctx->err = err;
}
