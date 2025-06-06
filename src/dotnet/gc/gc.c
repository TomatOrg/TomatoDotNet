#include "gc.h"

#include <stdalign.h>
#include <util/string.h>

#include "tomatodotnet/host.h"
#include "util/except.h"
#include "dotnet/metadata/metadata.h"
#include "util/stb_ds.h"

#include <sanitizer/asan_interface.h>

#include "size_class.h"
#include "dotnet/jit/jit_type.h"
#include "tomatodotnet/tdn.h"

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

/**
 * The base of the heap
 */
static void* m_heap_base;

/**
 * Watermarks for allocations
 * TODO: something more advanced
 */
static size_t m_water_marks[GC_NUM_CLASSES] = {0};

void tdn_init_gc(void* base_address) {
    m_heap_base = base_address;
    gc_size_class_init();
}

static Object gc_get_object(void* ptr) {
    // ensure within the range of the heap
    if (m_heap_base > ptr || ptr >= m_heap_base + TDN_GC_HEAP_SIZE) {
        return NULL;
    }

    // remove the base
    uintptr_t uptr = ptr - m_heap_base;

    // get the class size
    size_t class_size = uptr / SIZE_512GB;
    size_t size = gc_class_to_size(class_size);

    // get the bottom of the object from this object
    return ptr - (uptr % size);
}

__attribute__((noinline))
static void* gc_slow_alloc_small(size_t size, uint32_t size_class) {
    size_t class_size = gc_class_to_size(size_class);
    size_t watermark = m_water_marks[size_class];
    m_water_marks[size_class] += class_size;
    void* ptr = m_heap_base + (size_class * SIZE_512GB) + watermark;
    return ptr;
}

static inline void* gc_fast_alloc(size_t size, size_t align) {
    // If the size is too large its not a fast-path anymore.
    int size_class = gc_get_size_class(size, align);
    if (UNLIKELY(size_class < 0)) {
        ASSERT(!"gc_slow_alloc_large");
    }

    // Fast path for allocating small size memory.
    // TODO: cpu cache allocation
    void* ret = NULL;
    if (UNLIKELY(ret == NULL)) {
        ret = gc_slow_alloc_small(size, size_class);
    }

    return ret;
}

void* tdn_gc_new(RuntimeTypeInfo type, size_t size) {
    if (type == NULL) {
        ASSERT(!"Tried to allocate object with null type?");
    } else {

        // if this is a generic type or a generic type parameter then we
        // can't create an instance of this object
        if (type->IsGenericParameter) {
            ASSERT(!"Tried to create a generic type parameter");
            return NULL;
        }

        if (type->GenericTypeDefinition == type) {
            ASSERT(!"Tried to create a generic type definition");
            return NULL;
        }

        if (type->Attributes.Abstract) {
            ASSERT(!"Tried to create an abstract class");
            return NULL;
        }

        if (type->JitVTable == NULL) {
            ASSERT(!"Tried to create a type without a vtable");
            return NULL;
        }
    }

    // allocate the object itself
    Object object = gc_fast_alloc(size, type->HeapAlignment);
    if (UNLIKELY(object == NULL)) {
        return NULL;
    }

    // set the vtable
    object->VTable = type->JitVTable;
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

void tdn_gc_start(void) {
    // swap the colors
    int col = m_gc_color_unreached;
    m_gc_color_unreached = m_gc_color_reached;
    m_gc_color_reached = col;

    // start the actual scan
    tdn_host_gc_start();
}

void gc_register_root(void* field) {
    arrpush(m_gc_roots, field);
}

void gc_unregister_root(void* field) {
    for (int i = 0; i < arrlen(m_gc_roots); i++) {
        if (m_gc_roots[i] == field) {
            arrdelswap(m_gc_roots, i);
            return;
        }
    }
    ASSERT(!"Failed to find root");
}

static void gc_trace_object(void* _obj) {
    Object obj = _obj;

    // mark the object as black
    if (obj->GcColor == m_gc_color_reached) return;
    obj->GcColor = m_gc_color_reached;

    int unreached_color = m_gc_color_unreached;

    RuntimeTypeInfo type = obj->VTable->Type;
    // TRACE("%p - %T", obj, type);

    // scan the type instance itself, to ensure we don't delete it before
    // we handle all of the children
    gc_trace_object(type);

    // some runtime objects have native structures that we need to properly scan
    if (type == tRuntimeAssembly) {
        RuntimeAssembly assembly = (RuntimeAssembly)obj;
        for (int i = 0; i < arrlen(assembly->StringTable); i++) {
            gc_trace_object(assembly->StringTable[i].value);
        }
    }

    //
    // and now actually scan it
    //
    if (type->IsArray) {
        type = type->ElementType;
        if (tdn_type_is_valuetype(type) && arrlen(type->ManagedPointers) != 0) {
            ASSERT(!"TODO: array with managed structs");
        } else {
            // array with objects
            Object_Array arr = _obj;
            for (int i = 0; i < arr->Length; i++) {
                Object child = arr->Elements[i];
                if (child != NULL) {
                    gc_trace_object(child);
                }
            }
        }
    } else {
        // get the start of the object, if its a boxed type then we
        // need to ignore the fields
        void* obj_ptr = obj;
        if (tdn_type_is_valuetype(type)) {
            obj_ptr += jit_get_boxed_value_offset(type);
        }

        // go over all the managed pointers
        int len = arrlen(type->ManagedPointers);
        for (int i = 0; i < len; i++) {
            // get the child object
            Object child = *(Object*)(obj_ptr + type->ManagedPointers[i]);
            if (child != NULL) {
                gc_trace_object(child);
            }
        }
    }
}

void tdn_gc_scan_stack(void* start, size_t size) {
    ASSERT(((uintptr_t)start % 8) == 0);
    ASSERT((size % 8) == 0);
    for (size_t i = 0; i < size; i+= sizeof(void*)) {
        Object obj = gc_get_object(start + i);
        if (obj != NULL) {
            gc_trace_object(obj);
        }
    }
}

void tdn_gc_scan_roots(void) {
    for (int i = 0; i < arrlen(m_gc_roots); i++) {
        Object obj = *(Object*)m_gc_roots[i];
        if (obj != NULL) {
            gc_trace_object(obj);
        }
    }
}

static void gc_free_type(RuntimeTypeInfo value) {
    hmfree(value->InterfaceImpls);
    hmfree(value->GenericTypeInstances);
    arrfree(value->ManagedPointers);
    tdn_host_free(value->JitVTable);
}

static void gc_free_object(Object obj) {
    // free special builtin types
    RuntimeTypeInfo type = obj->VTable->Type;
    ASSERT(obj->GcColor == m_gc_color_unreached);

    if (type == tRuntimeTypeInfo && (RuntimeTypeInfo)obj != type) {
        RuntimeTypeInfo value = (RuntimeTypeInfo)obj;
        gc_free_type(value);

    } else if (type == tRuntimeAssembly) {
        RuntimeAssembly value = (RuntimeAssembly)obj;
        hmfree(value->StringTable);
        dotnet_free_file(value->Metadata);
        tdn_host_free(value->Metadata);

    } else if (type == tRuntimeFieldInfo) {
        RuntimeFieldInfo value = (RuntimeFieldInfo)obj;

        if (value->JitFieldPtr != NULL) {
            // Unregister gc roots
            if (tdn_type_is_valuetype(value->FieldType)) {
                // register the children of the struct
                for (int i = 0; i < arrlen(value->FieldType->ManagedPointers); i++) {
                    gc_unregister_root(value->JitFieldPtr + value->FieldType->ManagedPointers[i]);
                }
            } else {
                // a pointer to an object, register it directly
                gc_unregister_root(value->JitFieldPtr);
            }

            // free the memory
            tdn_host_free(value->JitFieldPtr);
        }
    }


    // mark as unallocated
    obj->GcColor = GC_COLOR_UNALLOCATED;
}

bool tdn_gc_sweep(void) {
    int reachable_color = m_gc_color_unreached;
    RuntimeTypeInfo* delayed_free = NULL;

    // and now we can actually free objects
    for (int i = 0; i < GC_NUM_CLASSES; i++) {
        size_t size = gc_class_to_size(i);
        size_t watermark = m_water_marks[i];
        for (size_t offset = 0; offset < watermark; offset += size) {
            Object obj = (m_heap_base + (SIZE_512GB * i) + offset);
            if (obj->GcColor == reachable_color) {
                if (obj->VTable->Type == tRuntimeTypeInfo) {
                    // we need to delay the freeing of type info because
                    // the vtable might still be needed at this point
                    arrpush(delayed_free, (RuntimeTypeInfo)obj);
                } else {
                    gc_free_object(obj);
                }
            }
        }
    }

    for (int i = 0; i < arrlen(delayed_free); i++) {
        gc_free_type(delayed_free[i]);
    }

    arrfree(delayed_free);

    return false;
}

void tdn_gc_run_finalizers(void) {
    // TODO: go over the objects we need to finalize
}
