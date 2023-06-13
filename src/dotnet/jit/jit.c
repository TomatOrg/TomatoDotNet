#include "tinydotnet/types/type.h"
#include "tinydotnet/jit/jit.h"
#include "util/except.h"

static bool m_jit_inited = false;

tdn_err_t tdn_jit_init() {
    tdn_err_t err = TDN_NO_ERROR;

    if (m_jit_inited) {
        goto cleanup;
    }

    TRACE("TODO: init jit");

    m_jit_inited = true;

cleanup:
    return err;
}

tdn_err_t tdn_jit_method(System_Reflection_MethodInfo methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_AND_RETHROW(tdn_jit_init());

    TRACE("Jitting method %U", methodInfo->Name);

cleanup:
    return err;
}

tdn_err_t tdn_jit_type(System_Type type) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_AND_RETHROW(tdn_jit_init());

    TRACE("Jitting type %U", type->Name);

cleanup:
    return err;
}
