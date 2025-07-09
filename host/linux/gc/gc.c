#include <stdalign.h>
#include <stdio.h>
#include <stdlib.h>
#include <util/string.h>

#include "tomatodotnet/host.h"
#include "util/except.h"
#include "dotnet/metadata/metadata.h"
#include "../../../include/tomatodotnet/util/stb_ds.h"

#include <sanitizer/asan_interface.h>

#include "mem_tree.h"
#include "dotnet/jit/jit_type.h"
#include "tomatodotnet/tdn.h"

/**
 * The total size needs to be reserved for the heap
 */
#define GC_HEAP_SIZE        ((512ull * 1024ull * 1024ull * 1024ull) * 85ull)

/**
 * Object that is not allocated
 */
#define GC_COLOR_UNALLOCATED       0

/**
 * Color of object that is allocated and was scanned
 */
static int m_gc_color_reached = 1;

/**
 * Color of object that is allocated and was not scanned yet
 */
static int m_gc_color_unreached = 2;

static Object gc_get_object(void* ptr) {
    return mem_tree_find(ptr);
}

Object tdn_host_gc_alloc(ObjectVTable* vtable, size_t size, size_t alignment) {
    Object object = aligned_alloc(alignment, size);
    if (UNLIKELY(object == NULL)) {
        return NULL;
    }
    mem_tree_insert(object, size);

    // set the vtable
    memset(object, 0, size);
    object->VTable = vtable;
    object->GcColor = m_gc_color_reached;
    return object;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Actual collection
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Array that contains all the GC roots
 */
static void** m_gc_roots = NULL;

void tdn_host_gc_register_root(void* field) {
    arrpush(m_gc_roots, field);
}

void tdn_host_gc_unregister_root(void* field) {
    for (int i = 0; i < arrlen(m_gc_roots); i++) {
        if (m_gc_roots[i] == field) {
            arrdelswap(m_gc_roots, i);
            return;
        }
    }
    ASSERT(!"Failed to find root");
}

void tdn_host_gc_trace_object(Object parent, Object obj) {
    // mark the object as black
    if (obj->GcColor == m_gc_color_reached) return;
    obj->GcColor = m_gc_color_reached;

    // trace children of the object
    tdn_gc_trace_children(obj);
}

static void gc_scan_stack(void* start, size_t size) {
    ASSERT(((uintptr_t)start % 8) == 0);
    ASSERT((size % 8) == 0);
    for (size_t i = 0; i < size; i+= sizeof(void*)) {
        Object obj = gc_get_object(start + i);
        if (obj != NULL) {
            tdn_host_gc_trace_object(NULL, obj);
        }
    }
}

static void gc_scan_roots(void) {
    for (int i = 0; i < arrlen(m_gc_roots); i++) {
        Object obj = *(Object*)m_gc_roots[i];
        if (obj != NULL) {
            tdn_host_gc_trace_object(NULL, obj);
        }
    }
}

static void gc_free_object(Object obj) {
    ASSERT(obj->GcColor == m_gc_color_unreached);
    tdn_gc_free(obj);
    obj->GcColor = GC_COLOR_UNALLOCATED;
    free(obj);
    mem_tree_remove(obj);
}

bool tdn_gc_sweep(void) {
    int unreachable_color = m_gc_color_unreached;
    RuntimeTypeInfo* delayed_free = NULL;

    mem_tree_iter_t iter = {};
    mem_tree_iter_begin(&iter);
    for (Object obj; (obj = mem_tree_iter_current(&iter)); mem_tree_iter_next(&iter)) {
        if (obj->GcColor == unreachable_color) {
            if (obj->VTable->Type == tRuntimeTypeInfo) {
                // we need to delay the freeing of type info because
                // the vtable might still be needed at this point
                arrpush(delayed_free, (RuntimeTypeInfo)obj);
            } else {
                gc_free_object(obj);
            }
        }
    }

    for (int i = 0; i < arrlen(delayed_free); i++) {
        gc_free_object((Object)delayed_free[i]);
    }

    arrfree(delayed_free);

    return false;
}

void tdn_gc_run_finalizers(void) {
    // TODO: go over the objects we need to finalize
}

void* g_gc_bottom_of_stack = NULL;

void tdn_host_gc_start(void) {
    // swap the colors
    int col = m_gc_color_unreached;
    m_gc_color_unreached = m_gc_color_reached;
    m_gc_color_reached = col;

    void* current_stack = __builtin_frame_address(0);
    size_t stack_size = g_gc_bottom_of_stack - current_stack;
    gc_scan_stack(current_stack, stack_size);
    gc_scan_roots();
    if (tdn_gc_sweep()) {
        tdn_gc_run_finalizers();
    }
}
