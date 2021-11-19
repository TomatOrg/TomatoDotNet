#include "thread.h"

#include "assembly_internal.h"
#include "gc.h"
#include "jit.h"

#include <mir-gen.h>


app_domain_t* new_app_domain() {
    app_domain_t* domain = calloc(1, sizeof(app_domain_t));

    // init mir stuff
    domain->ctx = MIR_init();

    // setup the gen
    MIR_gen_init(domain->ctx, 0);
    MIR_gen_set_optimize_level(domain->ctx, 0, 4);

    // setup runtime functions
    MIR_load_external(domain->ctx, "gc_alloc_from_token", gc_alloc_from_token);

    return domain;
}

err_t app_domain_load_assembly(app_domain_t* domain, assembly_t* assembly) {
    err_t err = NO_ERROR;
    FILE* f = NULL;

    CHECK(!domain->linked);

    // TODO: check it does not exists already

    // set the global module pointer
    char current_assembly_name[256];
    snprintf(current_assembly_name, sizeof(current_assembly_name), "assembly$[%s]", assembly->name);
    MIR_load_external(domain->ctx, current_assembly_name, assembly);

    // prepare for jitting
    jit_instance_t instance = { 0 };
    CHECK_AND_RETHROW(jit_prepare_assembly(&instance, assembly));

    // read the module
    f = fmemopen(assembly->module_data, assembly->module_data_size, "r");
    MIR_read(domain->ctx, f);

//    MIR_output(domain->ctx, stdout);

cleanup:
    if (f != NULL) {
        fclose(f);
    }
    return err;
}

err_t new_thread(app_domain_t* app_domain, method_t* method, thread_t** out_thread) {
    err_t err = NO_ERROR;
    thread_t* thread = NULL;

    CHECK(method->return_type == g_int32);
//    CHECK(method->parameter_count == 1);
//    CHECK(type_is_reference_type(method->parameters[0].type));

    // make sure everything is linked properly
    if (!app_domain->linked) {
        MIR_link(app_domain->ctx, MIR_set_gen_interface, NULL);
    }

    thread = calloc(1, sizeof(thread_t));
    CHECK_ERROR(thread != NULL, ERROR_OUT_OF_RESOURCES);
    thread->start_method = method;
    thread->stack = malloc(1024 * 1024 * 2);

    // get the function start
    char func_name[256];
    CHECK_AND_RETHROW(jit_mangle_name(method, func_name, sizeof(func_name)));
    MIR_item_t func = MIR_get_global_item(app_domain->ctx, func_name);
    CHECK(func != NULL);
    CHECK(func->item_type == MIR_func_item);

    // generate the start address
    MIR_output(app_domain->ctx, stdout);
    thread->start_address = MIR_gen(app_domain->ctx, 0, func);

    *out_thread = thread;

cleanup:
    if (IS_ERROR(err)) {
        SAFE_FREE(thread);
    }
    return err;
}

err_t thread_exec(thread_t* thread, int* return_value, void* argument) {
    err_t err = NO_ERROR;

    // TODO: top level exception handling

    *return_value = ((int(*)(void*))(thread->start_address))(argument);

cleanup:
    return err;
}
