#include "jit.h"

#include <util/except.h>
#include "tomatodotnet/util/stb_ds.h"

#include <spidir/log.h>
#include <spidir/opt.h>
#include <spidir/module.h>
#include <tomatodotnet/tdn.h>
#include <tomatodotnet/types/type.h>

#include <util/string_builder.h>

#include "builtin.h"
#include "codegen.h"
#include "emit.h"
#include "helpers.h"
#include "type.h"
#include "native/native.h"

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

/**
 * Cctors that need to run
 */
static RuntimeMethodBase* m_jit_cctor_queue = NULL;

static struct {
    spidir_funcref_t key;
    RuntimeMethodBase value;
}* m_method_lookup;

static struct {
    RuntimeMethodBase key;
    spidir_funcref_t value;
}* m_function_lookup = NULL;

spidir_funcref_t jit_get_function(spidir_module_handle_t module, RuntimeMethodBase method) {
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

    // attempt to search for a native method that implements
    // this method first, if it doesn't find one it stays as
    // null, otherwise it can export this properly
    if (method->MethodPtr == NULL) {
        method->MethodPtr = jit_get_native_method((RuntimeMethodInfo)method);
    }

    spidir_funcref_t function;
    if (method->MethodPtr != NULL) {
        function = spidir_funcref_make_external(spidir_module_create_extern_function(module, name, ret_type, arrlen(args), args));
    } else {
        spidir_function_t func = spidir_module_create_function(module, name, ret_type, arrlen(args), args);
        function = spidir_funcref_make_internal(func);

        // we need to jit this method
        arrpush(m_jit_queue, func);
    }

    // free the args
    string_builder_free(&builder);
    arrfree(args);

    // insert the function
    hmput(m_method_lookup, function, method);
    hmput(m_function_lookup, method, function);

    return function;
}

RuntimeMethodBase jit_get_method_from_function(spidir_funcref_t function) {
    int idx = hmgeti(m_method_lookup, function);
    if (idx < 0) {
        return NULL;
    }
    return m_method_lookup[idx].value;
}

void jit_queue_type(spidir_module_handle_t module, RuntimeTypeInfo type) {
    if (type->JitQueued == 0) {
        arrpush(m_jit_type_queue, type);
        type->JitQueued = 1;

        // queue all the virtual methods
        for (int i = 0; i < type->VTable->Length; i++) {
            RuntimeMethodBase methodInfo = (RuntimeMethodBase)type->VTable->Elements[i];
            if (methodInfo->MethodPtr == NULL) {
                spidir_funcref_t function = jit_get_function(module, methodInfo);
                if (spidir_funcref_is_internal(function)) {
                    // internal function, need to jit it
                    jit_codegen_queue(methodInfo, spidir_funcref_get_internal(function), false);

                } else if (spidir_funcref_is_external(function)) {
                    // external function, its a helper function,
                    // we can initialize it right away
                    methodInfo->MethodPtr = jit_get_helper_ptr(function);

                    // TODO: we need to generate a thunk to move the this pointer
                    //       into the data area, but for now we don't have a way
                    //       to do it with helper functions
                    ASSERT(!tdn_type_is_valuetype(type));

                } else {
                    ASSERT(!"Unreachable");
                }
            }
        }
    }
}

void jit_queue_cctor(spidir_module_handle_t module, RuntimeTypeInfo type) {
    if (type->TypeInitializer == NULL) {
        return;
    }

    RuntimeMethodBase method = (RuntimeMethodBase)type->TypeInitializer;

    // ensure the finalizer can run async since we don't support
    // the other kind just yet
    ASSERT(type->Attributes.BeforeFieldInit);

    // ignore if already added
    if (hmgeti(m_function_lookup, method) >= 0 || method->MethodPtr != NULL) {
        return;
    }

    // queue it for codegen and for running as a cctor
    spidir_funcref_t function = jit_get_function(module, method);
    if (spidir_funcref_is_internal(function)) {
        jit_codegen_queue(method, spidir_funcref_get_internal(function), false);
    }
    arrpush(m_jit_cctor_queue, method);
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
        int idx = hmgeti(m_method_lookup, spidir_funcref_make_internal(spidir_function));
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
    if (tdn_get_config()->jit_optimize) {
        spidir_opt_run(module);
    }

    // dump it for debugging
    if (tdn_get_config()->jit_spidir_dump) {
        void* ctx = tdn_host_jit_start_dump(TDN_JIT_DUMP_SPIDIR);
        spidir_module_dump(module, tdn_host_jit_dump_callback, ctx);
        tdn_host_jit_end_dump(ctx);
    }

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
                ptr = method->ThunkPtr;
            }
            type->JitVTable->Functions[j] = ptr;
        }
    }

    // run the cctors that can run right now
    for (int i = 0; i < arrlen(m_jit_cctor_queue); i++) {
        RuntimeMethodBase method = m_jit_cctor_queue[i];
        TRACE("Running cctor of %T", method->DeclaringType);
        CHECK(method->MethodPtr != NULL);
        ((void(*)())(method->MethodPtr))();
    }

cleanup:
    // free the codegen resources
    jit_codgen_cleanup();
    jit_clean_helpers();

    // free all the jit resources
    arrfree(m_jit_type_queue);
    arrfree(m_jit_cctor_queue);
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
    spidir_log_set_max_level(tdn_get_config()->jit_spidir_log_level);

    // initialize the codegen
    jit_codegen_init();

cleanup:
    return err;
}

// TODO: protect with a lock, only one assembly/method/type can be jitted at any given time
//       this ensures easier ordering between different jitting sessions

tdn_err_t tdn_jit_method(RuntimeMethodBase methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(methodInfo != NULL);

    // TODO: take mutex

    spidir_module_handle_t module = spidir_module_create();

    // start from the first function
    spidir_funcref_t function = jit_get_function(module, methodInfo);
    if (spidir_funcref_is_internal(function)) {
        jit_codegen_queue(methodInfo, spidir_funcref_get_internal(function), false);
    }

    // and start jitting
    CHECK_AND_RETHROW(jit_module(module));

cleanup:
    if (methodInfo != NULL) {
        spidir_module_destroy(module);
    }

    // TODO: release mutex

    return err;
}

tdn_err_t tdn_jit_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: take mutex

    spidir_module_handle_t module = spidir_module_create();

    // queue the type
    jit_queue_type(module, type);

    // and start jitting
    CHECK_AND_RETHROW(jit_module(module));

cleanup:
    spidir_module_destroy(module);

    // TODO: release mutex

    return err;
}