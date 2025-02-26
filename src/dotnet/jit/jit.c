#include "jit.h"

#include <util/except.h>
#include <util/stb_ds.h>

#include <spidir/log.h>

#include "jit_basic_block.h"
#include "jit_emit.h"
#include "jit_verify.h"

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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Top level dispatching
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static jit_cctor_t* m_jit_cctors_to_run = NULL;

void jit_queue_cctor(jit_cctor_t type) {
    arrpush(m_jit_cctors_to_run, type);
}

static void jit_call_cctors() {
    for (int i = 0; i < arrlen(m_jit_cctors_to_run); i++) {
        m_jit_cctors_to_run[i]();
    }

    arrfree(m_jit_cctors_to_run);
}

tdn_err_t tdn_jit_init() {
    tdn_err_t err = TDN_NO_ERROR;

    // initialize the emit backend
    CHECK_AND_RETHROW(jit_init_emit());

    spidir_log_init(jit_spidir_log_callback);
#ifdef JIT_VERBOSE_SPIDIR
    spidir_log_set_max_level(SPIDIR_LOG_LEVEL_TRACE);
#else
    spidir_log_set_max_level(SPIDIR_LOG_LEVEL_INFO);
#endif

cleanup:
    return err;
}

static tdn_err_t jit_common(spidir_module_handle_t module) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_AND_RETHROW(jit_emit(module));

    // call all the cctors
    jit_call_cctors();

cleanup:
    return err;
}

// TODO: protect with a lock, only one assembly/method/type can be jitted at any given time
//       this ensures easier ordering between different jitting sessions

tdn_err_t tdn_jit_method(RuntimeMethodBase methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: take mutex

    spidir_module_handle_t module = spidir_module_create();

    // start from the caller
    CHECK_AND_RETHROW(jit_queue_emit_method(module, methodInfo));

    // and init everything
    CHECK_AND_RETHROW(jit_common(module));

cleanup:
    // we can now clean the session
    jit_clean();

    spidir_module_destroy(module);

    // TODO: release mutex

    return err;
}

tdn_err_t tdn_jit_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: take mutex

    spidir_module_handle_t module = spidir_module_create();

    // emit everything in the vtable
    for (int i = 0; i < type->VTable->Length; i++) {
        RuntimeMethodInfo info = type->VTable->Elements[i];
        CHECK_AND_RETHROW(jit_queue_emit_method(module, (RuntimeMethodBase)info));
    }

    // and init everything
    CHECK_AND_RETHROW(jit_common(module));

cleanup:
    // we can now clean the session
    jit_clean();

    spidir_module_destroy(module);

    // TODO: release mutex

    return err;
}