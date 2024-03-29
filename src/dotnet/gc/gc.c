#include "gc.h"

#include "gc_thread_data.h"
#include "heap.h"

#include "../types.h"
#include "dotnet/loader.h"
#include "time/tick.h"

#include <thread/scheduler.h>
#include <thread/thread.h>

#include <util/stb_ds.h>
#include <time/tsc.h>


#include <stdnoreturn.h>
#include <stdatomic.h>

/**
 * Get the gc local data as fs relative pointer, this should allow the compiler
 * to nicely optimize this access, while still allowing us to access it from the
 * tcb which is stored in the thread struct
 */
#define GTD ((gc_thread_data_t __seg_fs*)offsetof(thread_control_block_t, gc_data))

gc_thread_data_t g_default_gc_thread_data;

/**
 * The color used for allocation, switched with clear
 * color on collection
 */
static int m_allocation_color = COLOR_WHITE;

/**
 * The color used for clearing objects, switched with
 * allocation color on collection
 */
static int m_clear_color = COLOR_YELLOW;

/**
 * Is the garbage collector tracing right now
 */
static _Atomic(bool) m_gc_tracing = false;

/**
 * The gc/collector thread
 */
static thread_t* m_collector_thread = NULL;

/**
 * Read object field
 */
static System_Object read_field(void* o, size_t offset) {
    return *(System_Object*)((uintptr_t)o + offset);
}

/**
 * Write to a pointer field
 */
static void write_field(void* o, size_t offset, void* new) {
    *(void**)((uintptr_t)o + offset) = new;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Global root management
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Protects the global root list
 */
static mutex_t m_global_roots_lock;

/**
 * The global root list
 */
static System_Object** m_global_roots = NULL;

void gc_add_root(void* object) {
    mutex_lock(&m_global_roots_lock);
    arrpush(m_global_roots, object);
    mutex_unlock(&m_global_roots_lock);
}

void gc_remove_root(void* object) {
    mutex_lock(&m_global_roots_lock);

    int index = -1;
    for (int i = 0; i < arrlen(m_global_roots); i++) {
        if (m_global_roots[i] == object) {
            index = -1;
            break;
        }
    }
    ASSERT(index != -1);

    stbds_arrdelswap(m_global_roots, index);

    mutex_unlock(&m_global_roots_lock);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

bool g_allow_null_type = true;

void* gc_new(System_Type type, size_t size) {
    scheduler_preempt_disable();

    // allocate the object
    System_Object o = heap_alloc(size, m_allocation_color);

    // set the object type
    if (type != NULL) {
        // set the indirect, 32bit type pointer
        ASSERT(type->SmallPointer != 0);
        o->type = type->SmallPointer;
        o->vtable = (uintptr_t)type->VTable;
    } else if (!g_allow_null_type) {
        // no longer allowed
        ASSERT(type != NULL);
    }

    // if there is no finalize then always suppress the finalizer
    if (type != NULL) {
        o->suppress_finalizer = type->Finalize == NULL;
    }

    scheduler_preempt_enable();

    return o;
}

/**
 * Used to tell the collector that there are more gray objects
 */
static _Atomic(bool) m_gc_has_gray_objects = false;

static void gc_mark_gray(System_Object object) {
    if (
        object != NULL &&
        (
            object->color == m_clear_color ||
            (object->color == m_allocation_color && GTD->status != THREAD_STATUS_ASYNC)
        )
    ) {
        object->color = COLOR_GRAY;
        m_gc_has_gray_objects = true;
    }
}

#define MARK_BEGIN(o, new) \
    do { \
        scheduler_preempt_disable(); \
        if (GTD->status != THREAD_STATUS_ASYNC) { \
            gc_mark_gray(o); \
            gc_mark_gray(new); \
        } else if (m_gc_tracing) { \
            gc_mark_gray(o); \
            /* Mark card, done implicitly because we */ \
            /* are going to change the object */ \
        } else { \
            /* Mark card, done implicitly because we */ \
            /* are going to change the object */ \
        } \
    } while (0)

#define MARK_END \
    do { \
        scheduler_preempt_enable(); \
    } while (0)

void gc_update(void* o, size_t offset, void* new) {
    MARK_BEGIN(o, new);
    write_field(o, offset, new);
    MARK_END;
}

System_Object gc_compare_exchange_ref(_Atomic(System_Object)* ptr, System_Object new, System_Object comparand) {
    System_Object object = heap_find_fast(ptr);
    if (object != NULL) {
        MARK_BEGIN(object, new);
        atomic_compare_exchange_strong(ptr, &comparand, new);
        MARK_END;
        return comparand;
    } else {
        atomic_compare_exchange_strong(ptr, &comparand, new);
        return comparand;
    }
}

System_Object gc_exchange_ref(_Atomic(System_Object)* ptr, System_Object new) {
    System_Object object = heap_find_fast(ptr);
    if (object != NULL) {
        MARK_BEGIN(object, new);
        System_Object res = atomic_exchange(ptr, new);
        MARK_END;
        return res;
    } else {
        return atomic_exchange(ptr, new);
    }
}

void gc_update_ref(void* ptr, void* new) {
    System_Object object = heap_find_fast(ptr);
    if (object != NULL) {
        gc_update(object, (uintptr_t)ptr - (uintptr_t)object, new);
    } else {
        write_field(ptr, 0, new);
    }
}

//----------------------------------------------------------------------------------------------------------------------
// Handshaking with all the threads, async to the main collector
//----------------------------------------------------------------------------------------------------------------------

static const char* m_status_str[] = {
    [THREAD_STATUS_ASYNC] = "ASYNC",
    [THREAD_STATUS_SYNC1] = "SYNC1",
    [THREAD_STATUS_SYNC2] = "SYNC2",
};

static void gc_mark_ptr(uintptr_t ptr) {

    // TODO: this also needs to check for DmaBuffer pointers to make sure
    //       to not delete dma buffers until even the stack no longer has it

    System_Object object = heap_find(ptr);
    if (object != NULL) {
        gc_mark_gray(object);
    }
}

/**
 * The mutex that protects the condition
 */
static mutex_t m_gc_handshake_mutex = INIT_MUTEX();

/**
 * The condition that is set when the condition is done
 */
static condition_t m_gc_handshake_condition = INIT_CONDITION();

/**
 * This thread performs the work of the handshake
 *
 * TODO: maybe turn this into being a signal sent to all threads so
 *       the scheduler can do it nicely, the original allocator did have
 *       it so the mutators are the one running the code anyways
 */
static void gc_handshake_thread(void* arg) {
    gc_thread_status_t status = (gc_thread_status_t)(uintptr_t)arg;

    // set our own status so gc_mark_gray will know
    // our status
    GTD->status = status;

    // iterate over all mutators, suspend them, and
    // set the status as needed, if more work is needed
    // do it now
    lock_all_threads();

    // set the default status for the next threads that will be created
    g_default_gc_thread_data.status = status;

    for (int i = 0; i < arrlen(g_all_threads); i++) {
        // get the thread and suspend it if it is not us
        // TODO: optimize by only doing the threads that don't run right now or something
        // TODO: skip any gc thread
        thread_t* thread = g_all_threads[i];

        if (!thread) continue;

        // don't suspend either our thread or the collector thread
        if (thread == get_current_thread() || thread == m_collector_thread) continue;

        // suspend and get the thread state
        suspend_state_t state = scheduler_suspend_thread(thread);

        gc_thread_data_t* gcl = &thread->tcb->tcb->gc_data;
        // sync2 == finding roots
        if (status == THREAD_STATUS_SYNC2 && !state.dead) {
            thread_save_state_t* regs = &thread->save_state;

            // mark the stack
            for (uintptr_t ptr = ALIGN_UP(regs->rsp - 128, 8); ptr <= (uintptr_t)thread->stack_top - 8; ptr += 8) {
                gc_mark_ptr(*((uintptr_t*)(ptr)));
            }

            // mark the registers
            gc_mark_ptr(regs->r15);
            gc_mark_ptr(regs->r14);
            gc_mark_ptr(regs->r13);
            gc_mark_ptr(regs->r12);
            gc_mark_ptr(regs->r11);
            gc_mark_ptr(regs->r10);
            gc_mark_ptr(regs->r9);
            gc_mark_ptr(regs->r8);
            gc_mark_ptr(regs->rbp);
            gc_mark_ptr(regs->rdi);
            gc_mark_ptr(regs->rsi);
            gc_mark_ptr(regs->rdx);
            gc_mark_ptr(regs->rcx);
            gc_mark_ptr(regs->rbx);
            gc_mark_ptr(regs->rax);

            // the managed thread instance for this thread
            gc_mark_ptr((uintptr_t) thread->tcb->managed_thread);
        }

        // set the status
        gcl->status = status;

        // resume the threads operation
        scheduler_resume_thread(state);
    }

    unlock_all_threads();

    mutex_lock(&m_gc_handshake_mutex);
    condition_notify_all(&m_gc_handshake_condition);
    mutex_unlock(&m_gc_handshake_mutex);
}

static void gc_post_handshake(gc_thread_status_t status) {
    // set the status of our thread so everything will sync nicely
    GTD->status = status;

    // create the handshake thread and ready it
    thread_t* thread = create_thread(gc_handshake_thread, (void *) status, "gc/handshake[%s]", m_status_str[status]);
    if (thread == NULL) {
        // TODO: panic
        ASSERT(!"failed to create gc_post_handshake thread");
    }
    scheduler_ready_thread(thread);
}

static void gc_wait_handshake() {
    mutex_lock(&m_gc_handshake_mutex);
    condition_wait(&m_gc_handshake_condition, &m_gc_handshake_mutex, -1);
    mutex_unlock(&m_gc_handshake_mutex);
}

static void gc_handshake(gc_thread_status_t status) {
    gc_post_handshake(status);
    gc_wait_handshake();
}

//----------------------------------------------------------------------------------------------------------------------
// Actual collector logic
//----------------------------------------------------------------------------------------------------------------------

static void gc_set_allocation_color(System_Object object) {
    // set all
    if (object->color == COLOR_BLACK || object->color == COLOR_GRAY) {
        object->color = m_allocation_color;
    }
}

static void gc_init_full_collection() {
    // set allocation color for all objects
    heap_iterate_objects(gc_set_allocation_color);

    // clear all the dirty bits
    heap_iterate_dirty_objects(NULL);
}

static void gc_clear_cards_callback(System_Object object) {
    if (object->color == COLOR_BLACK) {
        object->color = COLOR_GRAY;
        m_gc_has_gray_objects = true;
    }
}

static void gc_clear_cards() {
    heap_iterate_dirty_objects(gc_clear_cards_callback);
}

static void gc_clear(bool full_collection) {
    if (full_collection) {
        gc_init_full_collection();
    }
    gc_handshake(THREAD_STATUS_SYNC1);
}

static void gc_switch_allocation_clear_colors() {
    int temp = m_clear_color;
    m_clear_color = m_allocation_color;
    m_allocation_color = temp;
}

static void gc_mark_global_roots() {
    // add all the global roots
    mutex_lock(&m_global_roots_lock);
    for (int i = 0; i < arrlen(m_global_roots); i++) {
        System_Object object = *m_global_roots[i];
        if (!object) continue;
        if (object->color == m_clear_color || object->color == m_allocation_color) {
            gc_mark_gray(object);
        }
    }
    mutex_unlock(&m_global_roots_lock);

    // get all the loaded assemblies
    spinlock_lock(&g_loaded_assemblies_lock);
    for (int i = 0; i < shlen(g_loaded_assemblies); i++) {
        if (g_loaded_assemblies[i].value != NULL) {
            gc_mark_gray((System_Object)g_loaded_assemblies[i].value);
        }
    }
    spinlock_unlock(&g_loaded_assemblies_lock);
}

static void gc_mark_fields_gray(void* base, System_Type type) {
    for (int i = 0; i < arrlen(type->ManagedPointersOffsets); i++) {
        gc_mark_gray(read_field(base, type->ManagedPointersOffsets[i]));
    }
}

static void gc_mark_black(System_Object object) {
    System_Type type = OBJECT_TYPE(object);

    // mark all the children as gray
    if (type->IsArray) {
        // array object, mark all the items
        System_Array array = (System_Array) object;
        System_Type elementType = type->ElementType;
        if (elementType->IsValueType && arrlen(elementType->ManagedPointersOffsets) != 0) {
            // this is an array of structs that have managed values, so we need to iterate each
            // of the items and read all the pointers
            for (int i = 0; i < array->Length; i++) {
                size_t offset = type->ManagedSize + i * elementType->StackSize;
                gc_mark_fields_gray(read_field(object, offset), elementType);
            }
        } else if (!elementType->IsValueType) {
            // this is an array of pointers
            for (int i = 0; i < array->Length; i++) {
                size_t offset = type->ManagedSize + i * sizeof(void *);
                gc_mark_gray(read_field(object, offset));
            }
        }
    } else {
        // special case for the assembly which has a bunch of
        // un-managed structs holding managed info
        // TODO: this is not ideal
        if (type == tSystem_Reflection_Assembly) {
            System_Reflection_Assembly assembly = (System_Reflection_Assembly)object;

            // string table
            for (int i = 0; i < hmlen(assembly->UserStringsTable); i++) {
                gc_mark_gray((System_Object)assembly->UserStringsTable[i].value);
            }

            // custom attributes
            for (int i = 0; i < hmlen(assembly->CustomAttributeMap); i++) {
                gc_mark_gray(assembly->CustomAttributeMap[i].key);
                System_Object* objects = assembly->CustomAttributeMap[i].value;
                for (int j = 0; j < arrlen(objects); j++) {
                    gc_mark_gray(objects[j]);
                }
            }
        }

        // for normal objects iterate the managed pointer offsets, which
        // essentially contains all the offsets for all the pointers in
        // the object
        gc_mark_fields_gray(object, type);
    }

    // don't forget about the Type object
    gc_mark_gray((System_Object)type);

    // mark this as black
    object->color = COLOR_BLACK;
}

static void gc_trace_gray(System_Object object) {
    // skip non-gray objects
    if (object->color != COLOR_GRAY) return;

    // mark as black
    gc_mark_black(object);
}

static void gc_complete_trace() {
    while (m_gc_has_gray_objects) {
        m_gc_has_gray_objects = false;
        heap_iterate_objects(gc_trace_gray);
    }
}

static void gc_mark() {
    gc_post_handshake(THREAD_STATUS_SYNC2);
    gc_clear_cards();
    gc_switch_allocation_clear_colors();
    gc_wait_handshake();

    gc_post_handshake(THREAD_STATUS_ASYNC);
    gc_mark_global_roots();
    gc_complete_trace();
    gc_wait_handshake();
}

static void gc_trace() {
    gc_complete_trace();
}

static void gc_finalize_clear_objects(System_Object object) {
    if (object->color == m_clear_color && !object->suppress_finalizer) {
        // mark that no need for finalizer anymore
        object->suppress_finalizer = 1;

        // get the finalizer function and run it
        System_Exception(*finalize)(System_Object this) = OBJECT_TYPE(object)->Finalize->MirFunc->addr;
        System_Exception exception = finalize(object);
        if (exception != NULL) {
            WARN("Got exception in finalizer: `%U`", exception->Message);
        }
    }
}

static void gc_free_clear_objects(System_Object object) {
    if (object->color == m_clear_color) {
        // if this is still marked as clear color it means
        // that it should not be alive for finalization
        heap_free(object);
    }
}

static void gc_sweep(bool full_collection) {
    // first we will run the finalizer of all the objects, this makes sure that the
    // objects are fully finalized and ready to be cleared by the time that we need
    // to free them, and that we don't have any invalid refs, this saves on the need
    // to revive at the cost of running finalizers on the main gc thread, which I don't
    // think should be a real problem
    heap_iterate_objects(gc_finalize_clear_objects);

    // now actually free all the objects
    heap_iterate_objects(gc_free_clear_objects);

    if (full_collection) {
        // in a full collection we are also going to reclaim memory back
        // from the collector to the pmm, this is a bit slower so only
        // done on full collection
        heap_reclaim();
    }
}

static void gc_collection_cycle(bool full_collection) {
    gc_clear(full_collection);
    gc_mark();

    m_gc_tracing = true;
    gc_trace();
    gc_sweep(full_collection);
    m_gc_tracing = false;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// GC Main thread
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//----------------------------------------------------------------------------------------------------------------------
// Conductor, allows mutators to trigger the gc
//----------------------------------------------------------------------------------------------------------------------

/**
 * Is the gc currently running
 */
static atomic_bool m_gc_running = true;

/**
 * Mutex for controlling the running state
 */
static mutex_t m_gc_mutex = INIT_MUTEX();

/**
 * Conditional variable for waking the garbage collector
 */
static condition_t m_gc_wake = INIT_CONDITION();

/**
 * Conditional variable for waiting for the gc to be finished on the cycle
 */
static condition_t m_gc_done = INIT_CONDITION();

/**
 * Allows the gc to wait until the next request for a collection
 */
static void gc_conductor_next() {
    m_gc_running = false;
    condition_notify_all(&m_gc_done);
    do {
        condition_wait(&m_gc_wake, &m_gc_mutex, -1);
    } while (!m_gc_running);
}

/**
 * Wakeup the garbage collector
 */
static void gc_conductor_wake() {
    if (m_gc_running) {
        // gc is already running or someone
        // already requested it to run
        return;
    }

    m_gc_running = true;
    condition_notify_one(&m_gc_wake);
}

/**
 * Wait for the garbage collector
 */
static void gc_conductor_wait() {
    do {
        condition_wait(&m_gc_done, &m_gc_mutex, -1);
    } while (m_gc_running);
}

/**
 * To signal the collector to do a full cycle
 */
static atomic_bool m_full_collection = false;

void gc_wake(bool full) {
    m_full_collection = full;
    gc_conductor_wake();
}

void gc_wait(bool full) {
    mutex_lock(&m_gc_mutex);
    m_full_collection = full;
    gc_conductor_wake();
    gc_conductor_wait();
    mutex_unlock(&m_gc_mutex);
}

static atomic_bool m_had_full_collection = false;

static atomic_int m_gc_count = 0;

noreturn static void gc_thread(void* ctx) {
    TRACE("gc: GC thread started");
    while (true) {
        mutex_lock(&m_gc_mutex);
        gc_conductor_next();
        mutex_unlock(&m_gc_mutex);

        m_gc_count++;
        TRACE("gc: Starting collection #%d", m_gc_count);

        // setup for the collection
        bool was_full = m_full_collection;
        if (was_full) m_had_full_collection = true;

        // do a full cycle
        uint64_t start = microtime();
        gc_collection_cycle(was_full);
        TRACE("gc: Collection finished after %dms", (microtime() - start) / 1000);

        // clean up
        if (was_full) {
            m_full_collection = false;
        }
    }
}

err_t init_gc() {
    err_t err = NO_ERROR;

    m_collector_thread = create_thread(gc_thread, NULL, "gc/collector");
    CHECK(m_collector_thread != NULL);
    scheduler_ready_thread(m_collector_thread);

cleanup:
    return err;
}
