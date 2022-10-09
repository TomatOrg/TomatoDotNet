#include "jit.h"

static mutex_t m_type_init_lock = INIT_MUTEX();

static waitable_t* m_type_init_start = NULL;

typedef struct type_init_request {
    System_Type* types;
    waitable_t* waitable;
} type_init_request_t;

static type_init_request_t* m_type_init_reqs = NULL;

void jit_type_init_start() {
    mutex_lock(&m_type_init_lock);
    type_init_request_t req = { 0 };
    arrins(m_type_init_reqs, 0, req);
}

void jit_type_init_queue(System_Type type) {
    arrpush(m_type_init_reqs[0].types, type);
}

waitable_t* jit_type_init_commit() {
    waitable_t* w = create_waitable(1);

    if (arrlen(m_type_init_reqs[0].types) == 0) {
        // send it right now, because we have nothing to submit
        waitable_send(w, false);

        // remove this request
        arrdel(m_type_init_reqs, 0);

        // unlock this mutex, so it can properly start doing the request
        mutex_unlock(&m_type_init_lock);
    } else {
        // put a new reference to the request structure
        m_type_init_reqs[0].waitable = put_waitable(w);

        // unlock this mutex, so it can properly start doing the request
        mutex_unlock(&m_type_init_lock);

        // signal the type initialization thread that it should run
        // our new stuff, we are doing a blocking signal because we
        // want to be sure it actually arrived to it
        ASSERT(waitable_send(m_type_init_start, true));
    }

    // return our reference to the caller
    return w;
}

void jit_run_type_initializers() {
    err_t err = NO_ERROR;

    TRACE("jit: started type initializing thread");

    m_type_init_start = create_waitable(0);

    while (true) {
        waitable_result_t res = waitable_wait(m_type_init_start, true);
        CHECK(res == WAITABLE_SUCCESS);

        TRACE("jit: got type initialization request");

        // pop from the list
        mutex_lock(&m_type_init_lock);
        CHECK(arrlen(m_type_init_reqs) != 0);
        type_init_request_t req = arrpop(m_type_init_reqs);
        mutex_unlock(&m_type_init_lock);

        for (int i = 0; i < arrlen(req.types); i++) {
            System_Type type = req.types[i];

            // don't run twice
            if (type->RanTypeInitializer) {
                continue;
            }
            type->RanTypeInitializer = true;

            strbuilder_t builder = strbuilder_new();
            type_print_full_name(type, &builder);
            TRACE("jit: \tinitializing `%s`", strbuilder_get(&builder));

            // run it
            System_Exception(*cctor)() = type->TypeInitializer->MirFunc->addr;
            System_Exception exception = cctor();
            CHECK(exception == NULL, "Type initializer for %s: `%U`",
                  strbuilder_get(&builder), exception->Message);

            strbuilder_free(&builder);
        }

        // free it
        arrfree(req.types);

        // do a non-blocking wait, this should always work because
        // we have a queue of 1
        waitable_send(req.waitable, false);

        // release our reference
        release_waitable(req.waitable);
    }

cleanup:
    PANIC_ON(err);
}
