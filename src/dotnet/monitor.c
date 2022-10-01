#include "monitor.h"
#include "util/fastrand.h"
#include "sync/conditional.h"

#include <thread/scheduler.h>
#include <sync/mutex.h>

#include <stdalign.h>
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Object -> pointer management
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct monitor {
    // the links
    struct monitor* parent;
    struct monitor* next;
    struct monitor* prev;
    uint32_t ticket;

    // the object we are waiting on
    void* object;

    // the thread that locked the mutex
    thread_t* locker;

    // the mutex for Enter+Exit
    mutex_t mutex;

    // the conditional for Pulse+PulseAll+Wait
    conditional_t cond;
} monitor_t;

typedef struct monitor_root {
    spinlock_t lock;

    // root of balanced tree of unique locks
    monitor_t* treap;
} monitor_root_t;

// Prime to not correlate with any user patterns.
#define MONITOR_TABLE_SIZE 251

typedef struct monitor_table {
    alignas(64) monitor_root_t root;
} monitor_table_t;

static monitor_table_t m_monitor_table[MONITOR_TABLE_SIZE];

static monitor_root_t* get_monitor_root(void* addr) {
    return &m_monitor_table[((uintptr_t)addr >> 3) % MONITOR_TABLE_SIZE].root;
}

static void rotate_left(monitor_root_t* root, monitor_t* x) {
    monitor_t* p = x->parent;
    monitor_t* y = x->next;
    monitor_t* b = y->prev;

    y->prev = x;
    x->parent = y;
    x->next = b;
    if (b != NULL) {
        b->parent = x;
    }

    y->parent = p;
    if (p == NULL) {
        root->treap = y;
    } else if (p->prev == x) {
        p->prev = y;
    } else {
        ASSERT(p->next == x);
        p->next = y;
    }
}

static void rotate_right(monitor_root_t* root, monitor_t* y) {
    monitor_t* p = y->parent;
    monitor_t* x = y->prev;
    monitor_t* b = x->next;

    x->next = y;
    y->parent = x;
    y->prev = b;
    if (b != NULL) {
        b->parent = y;
    }

    x->parent = p;
    if (p == NULL) {
        root->treap = x;
    } else if (p->prev == y) {
        p->prev = x;
    } else {
        ASSERT(p->next == y);
        p->next = x;
    }
}

static monitor_t* get_monitor(monitor_root_t* root, void* addr) {
    spinlock_lock(&root->lock);

    monitor_t* last = NULL;
    monitor_t** pm = &root->treap;
    for (monitor_t* m = *pm; m != NULL; m = *pm) {
        if (m->object == addr) {
            // Already have addr in the list
            spinlock_unlock(&root->lock);
            return m;
        }

        last = m;
        if (addr < m->object) {
            pm = &m->prev;
        } else {
            pm = &m->next;
        }
    }

    // Add monitor as new leaf in tree of unique addrs.
    // The balanced tree is a treap using ticket as the random heap priority.
    // That is, it is a binary tree ordered according to the elem addresses,
    // but then among the space of possible binary trees respecting those
    // addresses, it is kept balanced on average by maintaining a heap ordering
    // on the ticket: s.ticket <= both s.prev.ticket and s.next.ticket.
    // https://en.wikipedia.org/wiki/Treap
    // https://faculty.washington.edu/aragon/pubs/rst89.pdf
    //
    // monitor.ticket compared with zero in couple of places, therefore set lowest bit.
    // It will not affect treap's quality noticeably.
    monitor_t* monitor = malloc(sizeof(monitor_t));
    if (monitor == NULL) {
        spinlock_unlock(&root->lock);
        return NULL;
    }
    monitor->cond = INIT_CONDITIONAL();
    monitor->locker = NULL;
    monitor->mutex = INIT_MUTEX();
    monitor->next = NULL;
    monitor->prev = NULL;
    
    monitor->ticket = fastrand() | 1;
    monitor->object = addr;
    monitor->parent = last;
    *pm = monitor;

    while (monitor->parent != NULL && monitor->parent->ticket > monitor->ticket) {
        if (monitor->parent->prev == monitor) {
            rotate_right(root, monitor->parent);
        } else {
            ASSERT(monitor->parent->next == monitor);
            rotate_left(root, monitor->parent);
        }
    }

    spinlock_unlock(&root->lock);
    return monitor;
}


void free_monitor(void* object) {
    monitor_root_t* root = get_monitor_root(object);

    spinlock_lock(&root->lock);

    monitor_t** pm = &root->treap;
    monitor_t* m = *pm;
    for (; m != NULL; m = *pm) {
        if (m->object == object) {
            goto found;
        }
        if (object < m->object) {
            pm = &m->prev;
        } else {
            pm = &m->next;
        }
    }

    // no monitor for this object
    spinlock_unlock(&root->lock);
    return;

found:
    // Rotate down to be leaf of tree for removal, respecting priorities
    while (m->next != NULL || m->prev != NULL) {
        if (m->next == NULL || (m->prev != NULL && m->prev->ticket < m->next->ticket)) {
            rotate_right(root, m);
        } else {
            rotate_left(root, m);
        }
    }

    // Remove m, now a leaf
    if (m->parent != NULL) {
        if (m->parent->prev == m) {
            m->parent->prev = NULL;
        } else {
            m->parent->next = NULL;
        }
    } else {
        root->treap = NULL;
    }

    spinlock_unlock(&root->lock);

    // we can now properly free it
    free(m);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// monitor Implementation
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * take the ownership of the lock
 */
static void take_lock(monitor_t* monitor) {
    ASSERT(monitor->locker == NULL);
    monitor->locker = put_thread(get_current_thread());
}

/**
 * Release the ownership of the lock
 */
static void release_lock(monitor_t* monitor) {
    ASSERT(monitor->locker == get_current_thread());
    release_thread(monitor->locker);
    monitor->locker = NULL;
}

err_t monitor_enter(void* object) {
    err_t err = NO_ERROR;

    // get the monitor object
    monitor_t* monitor = get_monitor(get_monitor_root(object), object);
    CHECK_ERROR(monitor != NULL, ERROR_OUT_OF_MEMORY);

//    mutex_lock(&monitor->mutex);
    ASSERT(mutex_try_lock(&monitor->mutex));
    take_lock(monitor);

cleanup:
    return err;
}

err_t monitor_exit(void* object) {
    err_t err = NO_ERROR;

    // get the monitor object
    monitor_t* monitor = get_monitor(get_monitor_root(object), object);
    CHECK_ERROR(monitor != NULL, ERROR_OUT_OF_MEMORY);

    // make sure the locked thread also frees this
    CHECK_ERROR(monitor->locker == get_current_thread(), ERROR_SYNCHRONIZATION_LOCK);

    // unlock the mutex
    mutex_unlock(&monitor->mutex);
    release_lock(monitor);

cleanup:
    return err;
}

err_t monitor_pulse(void* object) {
    err_t err = NO_ERROR;

    // get the monitor object
    monitor_t* monitor = get_monitor(get_monitor_root(object), object);
    CHECK_ERROR(monitor != NULL, ERROR_OUT_OF_MEMORY);

    // make sure the locked thread also frees this
    CHECK_ERROR(monitor->locker == get_current_thread(), ERROR_SYNCHRONIZATION_LOCK);

    // pulse it
    conditional_signal(&monitor->cond);

cleanup:
    return err;
}

err_t monitor_pulse_all(void* object) {
    err_t err = NO_ERROR;

    // get the monitor object
    monitor_t* monitor = get_monitor(get_monitor_root(object), object);
    CHECK_ERROR(monitor != NULL, ERROR_OUT_OF_MEMORY);

    // make sure the locked thread also frees this
    CHECK_ERROR(monitor->locker == get_current_thread(), ERROR_SYNCHRONIZATION_LOCK);

    // pulse all
    conditional_broadcast(&monitor->cond);

cleanup:
    return err;
}

err_t monitor_wait(void* object) {
    err_t err = NO_ERROR;

    // get the monitor object
    monitor_t* monitor = get_monitor(get_monitor_root(object), object);
    CHECK_ERROR(monitor != NULL, ERROR_OUT_OF_MEMORY);

    // make sure the locked thread also frees this
    CHECK_ERROR(monitor->locker == get_current_thread(), ERROR_SYNCHRONIZATION_LOCK);

    // we are going to unlock, so remove our ownership
    release_lock(monitor);

    // wait for it
    conditional_wait(&monitor->cond, &monitor->mutex);

    // we are again the owners of the lock
    take_lock(monitor);

cleanup:
    return err;
}
