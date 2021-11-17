#pragma once

#include "method.h"

#include <stdbool.h>
#include <stdint.h>

#include <mir.h>

struct thread;

typedef struct thread {
    // the stack of this thread
    uint8_t* stack;

    // the start address of the thread
    method_t* start_method;
    void* start_address;
} thread_t;

typedef struct app_domain {
    // list of all the threads in this domain
    thread_t* threads;

    // The context of the runtime for this process
    MIR_context_t ctx;

    // did we link
    bool linked;
} app_domain_t;

app_domain_t* new_app_domain();

err_t app_domain_load_assembly(app_domain_t* domain, assembly_t* assembly);

err_t new_thread(app_domain_t* app_domain, method_t* method, thread_t** thread);

err_t thread_exec(thread_t* thread, int* return_value, void* argument);
