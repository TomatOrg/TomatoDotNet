#include "jit_internal.h"

#include <util/except.h>
#include <util/stb_ds.h>

#include "jit_basic_block.h"
#include "jit_emit.h"

static struct {
    RuntimeMethodBase key;
    jit_method_t* value;
}* m_jit_methods;

static struct {
    spidir_function_t key;
    jit_method_t* value;
}* m_jit_functions;

tdn_err_t jit_get_or_create_method(RuntimeMethodBase method, jit_method_t** result) {
    tdn_err_t err = TDN_NO_ERROR;

    int idx = hmgeti(m_jit_methods, method);
    if (idx >= 0) {
        *result = m_jit_methods[idx].value;
        goto cleanup;
    }

    jit_method_t* jmethod = tdn_host_mallocz(sizeof(*method));
    CHECK_ERROR(jmethod != NULL, TDN_ERROR_OUT_OF_MEMORY);
    hmput(m_jit_methods, method, jmethod);

    jmethod->method = method;

    // TODO: maybe we should just have a way to save the optimized spidir
    //       instead of re-emitting every time...
    if (method->MethodPtr != NULL && method->MethodBody == NULL) {
        // the method is implemented by the runtime/host, so
        // emit it as an extern
        jit_queue_emit_extern(jmethod);

        // extern doesn't need verification
        jmethod->verifying = true;

    } else if (method->MethodBody == NULL) {
        // the method is a runtime method, we will
        // need to emit it manually, this will be done
        // later on
        jit_queue_emit(jmethod);

        // runtime methods don't need verification
        jmethod->verifying = true;

    } else {
        // just a normal method
        jit_queue_emit(jmethod);
    }

    // we can now put the function -> method lookup for later use
    hmput(m_jit_functions, jmethod->function, jmethod);

    *result = jmethod;

cleanup:
    return err;
}

jit_method_t* jit_get_method_from_function(spidir_function_t function) {
    int idx = hmgeti(m_jit_functions, function);
    if (idx >= 0) {
        return m_jit_functions[idx].value;
    }
    return NULL;
}

void jit_clean() {
    for (int i = 0; i < hmlen(m_jit_methods); i++) {
        jit_method_t* method = m_jit_methods[i].value;
        if (method == NULL) {
            continue;
        }

        for (int j = 0; j < arrlen(method->basic_blocks); j++) {
            jit_basic_block_t* block = &method->basic_blocks[j];
            arrfree(block->locals);
            arrfree(block->stack);
        }

        arrfree(method->basic_blocks);
        hmfree(method->labels);
    }

    hmfree(m_jit_methods);
}

