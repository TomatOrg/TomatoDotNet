#include "jit_builtin.h"

#include <dotnet/types.h>
#include <tomatodotnet/types/type.h>
#include <util/except.h>
#include <util/stb_ds.h>

#include "jit_emit.h"
#include "jit_helpers.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Generic emit code
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

jit_builtin_emitter_t jit_get_builtin_emitter(RuntimeMethodBase method) {
    RuntimeTypeInfo type = method->DeclaringType;

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
