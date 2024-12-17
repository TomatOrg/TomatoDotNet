#include "jit_internal.h"

#include <util/alloc.h>
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

    jit_method_t* jmethod = tdn_mallocz(sizeof(*method));
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
        if (method->MethodImplFlags.CodeType == TDN_METHOD_IMPL_CODE_TYPE_RUNTIME) {
            // the method is a runtime method, we will
            // need to emit it manually, this will be done
            // later on
            jit_queue_emit(jmethod);
        } else {
            CHECK(method->MethodImplFlags.CodeType == TDN_METHOD_IMPL_CODE_TYPE_IL);
            CHECK(method->Attributes.Abstract);
        }

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

void jit_method_register_thunk(jit_method_t* method) {
    hmput(m_jit_functions, method->thunk, method);
}

jit_method_t* jit_get_method_from_function(spidir_function_t function) {
    int idx = hmgeti(m_jit_functions, function);
    if (idx >= 0) {
        return m_jit_functions[idx].value;
    }
    return NULL;
}

RuntimeExceptionHandlingClause jit_get_enclosing_try_clause(jit_method_t* method, uint32_t pc, int type, RuntimeExceptionHandlingClause previous) {
    RuntimeExceptionHandlingClause_Array arr = method->method->MethodBody->ExceptionHandlingClauses;
    RuntimeExceptionHandlingClause matched = NULL;

    for (int i = 0; i < arr->Length; i++) {
        RuntimeExceptionHandlingClause clause = arr->Elements[i];
        if (previous == clause) {
            break;
        }

        if (clause->Flags != type) {
            continue;
        }

        if (clause->TryOffset <= pc && pc < clause->TryOffset + clause->TryLength) {
            if (
                matched == NULL ||
                (
                    matched->TryOffset < clause->TryOffset &&
                    matched->TryOffset + matched->TryLength > clause->TryOffset + clause->TryLength
                )
            ) {
                matched = clause;
            }
        }
    }

    return matched;
}

static void free_basic_block(jit_basic_block_t* block) {
    arrfree(block->locals);
    arrfree(block->stack);
    arrfree(block->leave_target_stack);
    tdn_host_free(block);
}

void jit_clean() {
    for (int i = 0; i < hmlen(m_jit_methods); i++) {
        jit_method_t* method = m_jit_methods[i].value;
        if (method == NULL) {
            continue;
        }

        // make sure all the things made while
        arrfree(method->args);
        arrfree(method->locals);
        arrfree(method->block_queue);

        // free the labels
        hmfree(method->labels);

        // free all the basic block structs
        for (int j = 0; j < arrlen(method->basic_blocks); j++) {
            free_basic_block(method->basic_blocks[j]);
        }
        arrfree(method->basic_blocks);

        // free all the leave paths
        for (int j = 0; j < hmlen(method->leave_blocks); j++) {
            free_basic_block(method->leave_blocks[j].value);
        }
        hmfree(method->leave_blocks);
    }

    hmfree(m_jit_methods);
}

