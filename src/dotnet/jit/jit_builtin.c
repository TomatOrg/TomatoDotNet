#include "jit_builtin.h"

#include <dotnet/types.h>
#include <tomatodotnet/types/type.h>
#include <util/except.h>
#include <util/stb_ds.h>

#include "jit_emit.h"
#include "jit_helpers.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Delegate handling
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static spidir_value_t emit_delegate_ctor(
    spidir_builder_handle_t builder,
    RuntimeMethodBase method,
    spidir_value_t* args
) {
    spidir_value_t delegate = args[0];
    spidir_value_t target = args[1];
    spidir_value_t method_native_ptr = args[2];

    // store the instance
    STATIC_ASSERT(offsetof(Delegate, Instance) == 0);
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, target, delegate);

    // store the method
    spidir_value_t method_ptr = spidir_builder_build_ptroff(builder, delegate,
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Function)));
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, method_native_ptr, method_ptr);

    return SPIDIR_VALUE_INVALID;
}


static spidir_value_t emit_delegate_invoke(
    spidir_builder_handle_t builder,
    RuntimeMethodBase method,
    spidir_value_t* args
) {
    spidir_value_t delegate = args[0];

    // load the target and method ptr
    spidir_value_t method_ptr = spidir_builder_build_ptroff(builder, delegate,
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Function)));
    method_ptr = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, method_ptr);
    spidir_value_t target = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, delegate);

    // load all the arguments
    spidir_value_t* temp_args = NULL;
    arrpush(temp_args, target);
    for (int i = 0; i < method->Parameters->Length; i++) {
        arrpush(temp_args, args[i + 1]);
    }

    // perform the indirect call
    spidir_value_type_t* arg_types = jit_get_spidir_arg_types(method);
    spidir_value_t result = spidir_builder_build_callind(
        builder,
        jit_get_spidir_ret_type(method),
        arrlen(arg_types), arg_types,
        method_ptr,
        temp_args
    );

    arrfree(arg_types);
    arrfree(temp_args);

    return result;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Generic emit code
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

jit_builtin_emitter_t jit_get_builtin_emitter(RuntimeMethodBase method) {
    RuntimeTypeInfo type = method->DeclaringType;

    if (type->BaseType == tMulticastDelegate) {
        // this is a delegate instance, the ctor takes
        if (tdn_compare_string_to_cstr(method->Name, ".ctor")) {
            return emit_delegate_ctor;
        }

        if (tdn_compare_string_to_cstr(method->Name, "Invoke")) {
            return emit_delegate_invoke;
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
