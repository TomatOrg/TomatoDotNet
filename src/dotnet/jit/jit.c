#include "jit.h"

#include <util/except.h>
#include <util/stb_ds.h>

#include "jit_basic_block.h"
#include "jit_emit.h"
#include "jit_verify.h"


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Top level dispatching
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t tdn_jit_init() {
    tdn_err_t err = TDN_NO_ERROR;

    // initialize the emit backend
    CHECK_AND_RETHROW(jit_init_emit());

cleanup:
    return err;
}

// TODO: protect with a lock, only one assembly/method/type can be jitted at any given time
//       this ensures easier ordering between different jitting sessions

tdn_err_t tdn_jit_method(RuntimeMethodBase methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: take mutex

    // queue the method
    CHECK_AND_RETHROW(jit_verify_method(methodInfo));

    // emit everything
    // CHECK_AND_RETHROW(jit_emit());

cleanup:
    // we can now clean the session
    jit_clean();

    // TODO: release mutex

    return err;
}

tdn_err_t tdn_jit_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: take mutex

    // queue the type
    CHECK_AND_RETHROW(jit_verify_type(type));

    // emit everything
    // CHECK_AND_RETHROW(jit_emit());

cleanup:
    // we can now clean the session
    jit_clean();

    // TODO: release mutex

    return err;
}