#include "jit.h"

#include <util/except.h>
#include <util/stb_ds.h>

#include <spidir/log.h>

#include "jit_basic_block.h"
#include "jit_emit.h"
#include "jit_verify.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Top level dispatching
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Top level dispatching
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void jit_spidir_log_callback(spidir_log_level_t level, const char* module, size_t module_len, const char* message, size_t message_len) {
    switch (level) {
        case SPIDIR_LOG_LEVEL_ERROR: ERROR("spidir/%.*s: %.*s", module_len, module, message_len, message); break;
        case SPIDIR_LOG_LEVEL_WARN: WARN("spidir/%.*s: %.*s", module_len, module, message_len, message); break;
        case SPIDIR_LOG_LEVEL_INFO:
        case SPIDIR_LOG_LEVEL_DEBUG:
        case SPIDIR_LOG_LEVEL_TRACE:
        default: TRACE("spidir/%.*s: %.*s", module_len, module, message_len, message); break;
    }
}

tdn_err_t tdn_jit_init() {
    tdn_err_t err = TDN_NO_ERROR;

    spidir_log_init(jit_spidir_log_callback);
#ifdef JIT_VERBOSE_SPIDIR
    spidir_log_set_max_level(SPIDIR_LOG_LEVEL_TRACE);
#else
    spidir_log_set_max_level(SPIDIR_LOG_LEVEL_INFO);
#endif

cleanup:
    return err;
}

// TODO: protect with a lock, only one assembly/method/type can be jitted at any given time
//       this ensures easier ordering between different jitting sessions

tdn_err_t tdn_jit_method(RuntimeMethodBase methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_verifier_t verifier = {};

    CHECK_AND_RETHROW(jit_verifier_init(&verifier, methodInfo));

    CHECK_AND_RETHROW(jit_verify(&verifier));

cleanup:
    jit_verifier_destroy(&verifier);

    return err;
}

tdn_err_t tdn_jit_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: take mutex

    spidir_module_handle_t module = spidir_module_create();

    CHECK_FAIL();

cleanup:
    spidir_module_destroy(module);

    // TODO: release mutex

    return err;
}