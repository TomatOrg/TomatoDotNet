#include "jit_builtin.h"

#include <dotnet/types.h>
#include <tomatodotnet/types/type.h>
#include <util/except.h>
#include <util/stb_ds.h>

#include "jit_emit.h"
#include "jit_helpers.h"


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
