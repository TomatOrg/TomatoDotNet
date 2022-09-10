#pragma once

#include "../types.h"

#include <util/except.h>

#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>

/**
 * Initialize the garbage collector
 */
err_t init_gc();

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Allocate objects and update pointers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Add a new root to the garbage collector
 */
void gc_add_root(void* object);

/**
 * Remove a root from the garbage collector
 */
void gc_remove_root(void* object);

/**
 * Allocate a new object from the garbage collector of the given type and of
 * the given size
 *
 * @param type      [IN] The type of the object
 * @param count     [IN] The size to allocate
 */
void* gc_new(System_Type type, size_t size);

/**
 * Get the memory info of the GC
 */
void gc_get_memory_info(System_GCMemoryInfo* memoryInfo);

/**
 * Helper to allocate a new gc object
 */
#define UNSAFE_GC_NEW(type) \
    ({ \
        System_Type __type = type; \
        void* object = gc_new(__type, __type->ManagedSize); \
        ASSERT(object != NULL); \
        object; \
    })

#define GC_NEW(type) \
    ({ \
        System_Type __type = type; \
        void* object = gc_new(__type, __type->ManagedSize); \
        CHECK_ERROR(object != NULL, ERROR_OUT_OF_MEMORY); \
        object; \
    })

#define GC_NEW_STRING(count) \
    ({ \
        size_t __count = count; \
        System_String __str = gc_new(tSystem_String, tSystem_String->ManagedSize + 2 * __count); \
        ASSERT(__str != NULL); \
        __str->Length = __count; \
        __str; \
    })

/**
 * Helper to allocate a new array
 *
 * TODO: empty array optimization
 */
#define GC_NEW_ARRAY(elementType, count) \
    ({ \
        size_t __count = count; \
        System_Type __elementType = elementType; \
        System_Type __arrayType = get_array_type(__elementType); \
        System_Array __newArray = gc_new(__arrayType, __arrayType->ManagedSize + __elementType->StackSize * __count); \
        ASSERT(__newArray != NULL); \
        __newArray->Length = __count; \
        (void*)__newArray; \
    })

/**
 * Update a pointer on the heap
 *
 * @remark
 * This must take an object that is allocated on the heap, it should not be used for local
 * pointers on the stack or for global variables.
 *
 * TODO: maybe just give the field info
 *
 * @param o         [IN] The object we are updating
 * @param offset    [IN] The offset of the field to update
 * @param new       [IN] The new object we are updating
 */
void gc_update(void* o, size_t offset, void* new);

System_Object gc_compare_exchange_ref(_Atomic(System_Object)* ptr, System_Object new, System_Object comparand);
System_Object gc_exchange_ref(_Atomic(System_Object)* ptr, System_Object new);

/**
 * Update a pointer that is possibly on the heap
 *
 * @param ptr       [IN] The pointer to the base of the struct
 * @param offset    [IN] The offset to the field to update
 * @param new       [IN] The new object we are updating
 */
void gc_update_ref(void* ptr, void* new);

/**
 * Run finalizers on the current thread until no finalizers are available
 */
void gc_run_finalizers();

/**
 * Should finalizers run right now
 */
bool gc_need_to_run_finalizers();

/**
 * Update a pointer on the heap
 *
 * This is a wrapper around gc_update that takes a field name instead of raw offset
 */
#define GC_UPDATE(o, field, new) \
    do { \
        __typeof(o) _o = o; \
        gc_update(_o, offsetof(__typeof(*(_o)), field), new); \
    } while (0)

#define GC_UPDATE_ARRAY(o, idx, new) \
    do { \
        __typeof(o) _o = o; \
        gc_update(_o, offsetof(__typeof(*(_o)), Data) + (idx) * sizeof(__typeof(_o->Data[0])), new); \
    } while (0)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Triggering the garbage collector
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Trigger the collection in an async manner
 */
void gc_wake(bool full);

/**
 * Trigger the gc and wait for it to finish
 */
void gc_wait(bool full);
