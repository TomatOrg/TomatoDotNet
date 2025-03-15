#include "jit.h"

#include <util/except.h>
#include <util/stb_ds.h>

#include <spidir/log.h>
#include <spidir/opt.h>
#include <spidir/module.h>
#include <tomatodotnet/types/type.h>

#include <util/string_builder.h>

#include "jit_basic_block.h"
#include "jit_builtin.h"
#include "jit_codegen.h"
#include "jit_emit.h"
#include "jit_helpers.h"
#include "jit_verify.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Top level dispatching
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * queue of methods that need to be dispatched
 */
static spidir_function_t* m_jit_queue = NULL;

/**
 * Types that need to be initialized
 */
static RuntimeTypeInfo* m_jit_type_queue = NULL;

static struct {
    spidir_function_t key;
    RuntimeMethodBase value;
}* m_method_lookup;

static struct {
    RuntimeMethodBase key;
    spidir_function_t value;
}* m_function_lookup = NULL;

spidir_function_t jit_get_function(spidir_module_handle_t module, RuntimeMethodBase method) {
    int id = hmgeti(m_function_lookup, method);
    if (id >= 0) {
        return m_function_lookup[id].value;
    }

    // prepare the name
    string_builder_t builder = {};
    string_builder_push_method_signature(&builder, method, true);
    const char* name = string_builder_build(&builder);

    // prepare the arguments
    spidir_value_type_t ret_type = jit_get_spidir_ret_type(method);
    spidir_value_type_t* args = jit_get_spidir_arg_types(method);

    // we need to create the function, if it was already jitted before
    // then create as an extern, otherwise

    spidir_function_t function;
    if (method->MethodPtr != NULL) {
        function = spidir_module_create_extern_function(module, name, ret_type, arrlen(args), args);
    } else {
        function = spidir_module_create_function(module, name, ret_type, arrlen(args), args);

        // we need to jit this method
        arrpush(m_jit_queue, function);
    }

    // free the args
    string_builder_free(&builder);
    arrfree(args);

    // insert the function
    hmput(m_method_lookup, function, method);
    hmput(m_function_lookup, method, function);

    return function;
}

RuntimeMethodBase jit_get_method_from_function(spidir_function_t function) {
    int idx = hmgeti(m_method_lookup, function);
    if (idx < 0) {
        return NULL;
    }
    return m_method_lookup[idx].value;
}

void jit_queue_type(RuntimeTypeInfo type) {
    if (type->JitQueued == 0) {
        arrpush(m_jit_type_queue, type);
        type->JitQueued = 1;
    }
}

static void jit_emit_function(spidir_builder_handle_t builder, void* _ctx) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_builtin_context_t* ctx = _ctx;
    jit_function_t function = {};

    // prepare the function
    CHECK_AND_RETHROW(jit_function_init(&function, ctx->method));

    // and we can jit the function normally
    CHECK_AND_RETHROW(jit_function(&function, builder));

cleanup:
    jit_function_destroy(&function);
    ctx->err = err;
}

static tdn_err_t jit_module(spidir_module_handle_t module) {
    tdn_err_t err = TDN_NO_ERROR;

    while (arrlen(m_jit_queue) != 0) {
        // get the spidir and dotnet method to jit
        spidir_function_t spidir_function = arrpop(m_jit_queue);
        int idx = hmgeti(m_method_lookup, spidir_function);
        CHECK(idx >= 0);
        RuntimeMethodBase method = m_method_lookup[idx].value;

        // emit the function, either using the builtin emitter or using the
        // normal function emitter
        jit_builtin_context_t emitter = {
            .err = TDN_NO_ERROR,
            .method = method
        };
        if (jit_get_builtin_emitter(method) != NULL) {
            CHECK(method->MethodBody == NULL);
            spidir_module_build_function(module, spidir_function, jit_emit_builtin, &emitter);
        } else {
            CHECK(method->MethodBody != NULL,
                "Missing method body %T::%U", method->DeclaringType, method->Name);

            spidir_module_build_function(module, spidir_function, jit_emit_function, &emitter);
        }
        CHECK_AND_RETHROW(emitter.err);

    }

    // run the optimizer
    spidir_opt_run(module);

    // dump it for debugging
    void* ctx = tdn_host_jit_start_dump();
    spidir_module_dump(module, tdn_host_jit_dump_callback, ctx);
    tdn_host_jit_end_dump(ctx);

    // now trigger the codegen
    CHECK_AND_RETHROW(jit_codegen(module));

    // now fill all the vtables
    for (int i = 0; i < arrlen(m_jit_type_queue); i++) {
        RuntimeTypeInfo type = m_jit_type_queue[i];
        for (int j = 0; j < type->VTable->Length; j++) {
            RuntimeMethodInfo method = type->VTable->Elements[j];
            CHECK(method->MethodPtr != NULL);

            // put the method pointer into the vtable (when thunk is available
            // we use that)
            void* ptr = method->MethodPtr;
            if (method->ThunkPtr != NULL) {
                ptr = method->MethodPtr;
            }
            type->JitVTable->Functions[j] = ptr;
        }
    }

    // TODO:

cleanup:
    // free the codegen resources
    jit_codgen_cleanup();
    jit_clean_helpers();

    // free all the jit resources
    arrfree(m_jit_type_queue);
    arrfree(m_jit_queue);
    hmfree(m_method_lookup);
    hmfree(m_function_lookup);

    return err;
}

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

    // setup the spidir logger
    spidir_log_init(jit_spidir_log_callback);
    spidir_log_set_max_level(SPIDIR_LOG_LEVEL_TRACE);

    // initialize the codegen
    jit_codegen_init();

cleanup:
    return err;
}

// TODO: protect with a lock, only one assembly/method/type can be jitted at any given time
//       this ensures easier ordering between different jitting sessions

tdn_err_t tdn_jit_method(RuntimeMethodBase methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: take mutex

    spidir_module_handle_t module = spidir_module_create();

    // start from the first function
    spidir_function_t function = jit_get_function(module, methodInfo);
    jit_codegen_queue(methodInfo, function, false);

    // and start jitting
    CHECK_AND_RETHROW(jit_module(module));

cleanup:
    spidir_module_destroy(module);

    // TODO: release mutex

    return err;
}

tdn_err_t tdn_jit_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: take mutex

    spidir_module_handle_t module = spidir_module_create();

    // queue the type
    jit_queue_type(type);

    // queue all the virtual methods
    for (int i = 0; i < type->VTable->Length; i++) {
        RuntimeMethodBase methodInfo = (RuntimeMethodBase)type->VTable->Elements[i];
        spidir_function_t function = jit_get_function(module, methodInfo);
        jit_codegen_queue(methodInfo, function, false);
    }

    // and start jitting
    CHECK_AND_RETHROW(jit_module(module));

cleanup:
    spidir_module_destroy(module);

    // TODO: release mutex

    return err;
}