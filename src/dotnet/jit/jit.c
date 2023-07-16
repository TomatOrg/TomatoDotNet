#include "tinydotnet/types/type.h"
#include "tinydotnet/jit/jit.h"
#include "util/except.h"
#include "util/stb_ds.h"

// TODO: lock

typedef struct jit_context {
    RuntimeMethodBase method;
    uint32_t* labels;
} jit_context_t;

tdn_err_t tdn_jit_init() {
    tdn_err_t err = TDN_NO_ERROR;

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The jitter itself
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void add_label(jit_context_t* ctx, uint32_t address) {
    stbds_arrmaybegrow(ctx->labels, 1);

    int i = 0;
    for (i = arrlen(ctx->labels) - 1; (i >= ctx->labels[i] > address); i--) {
        ctx->labels[i + 1] = ctx->labels[i];
    }

    ctx->labels[i + 1] = address;
}

static tdn_err_t compute_labels() {
    tdn_err_t err = TDN_NO_ERROR;

cleanup:
    return err;
}

static tdn_err_t jit_method(jit_context_t* ctx) {
    tdn_err_t err = TDN_NO_ERROR;



cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// High-level apis
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t tdn_jit_method(RuntimeMethodBase methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_context_t ctx = {
        .method = methodInfo
    };
    CHECK_AND_RETHROW(jit_method(&ctx));

cleanup:
    return err;
}

tdn_err_t tdn_jit_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // jit all the virtual methods, as those are the one that can be called
    // by other stuff unknowingly, the rest are going to be jitted lazyily
    for (int i = 0; i < type->DeclaredMethods->Length; i++) {
        RuntimeMethodBase method = (RuntimeMethodBase)type->DeclaredMethods->Elements[i];
        if (!method->Attributes.Virtual) continue;
        CHECK_AND_RETHROW(tdn_jit_method(method));
    }

cleanup:
    return err;
}
