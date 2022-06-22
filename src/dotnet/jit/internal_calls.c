#include "internal_calls.h"

#include "jit.h"

#include "../gc/gc.h"
#include "../types.h"

#include <thread/scheduler.h>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Other more generic utilities
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void managed_memcpy(System_Object this, System_Type struct_type, size_t offset, void* from) {
    uint8_t* this_base = (uint8_t*)this;

    int last_offset = 0;
    for (int i = 0; i < arrlen(struct_type->ManagedPointersOffsets); i++) {
        int current_offset = struct_type->ManagedPointersOffsets[i];

        // check if we have some unmanaged bytes to copy, if so copy them
        if (last_offset != current_offset) {
            memcpy(this_base + offset + last_offset, from + last_offset, current_offset - last_offset);
        }

        // copy the managed reference
        gc_update(this, offset + current_offset, *((System_Object*)from + current_offset));

        // update the last offset
        last_offset = current_offset;
    }

    // if we have more bytes at the end, copy them, in the case of an unmanaged
    // struct this is going to just copy all of the bytes
    if (last_offset != struct_type->StackSize) {
        memcpy(this_base + offset + last_offset, from + last_offset, struct_type->StackSize - last_offset);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Implementation of internal methods
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//----------------------------------------------------------------------------------------------------------------------
// System.Object
//----------------------------------------------------------------------------------------------------------------------

// TODO: generate this instead
static method_result_t object_GetType(System_Object this) {
    return (method_result_t){ .exception = NULL, .value = (uintptr_t) this->vtable->type};
}

//----------------------------------------------------------------------------------------------------------------------
// System.Threading.Interlocked
//----------------------------------------------------------------------------------------------------------------------

static method_result_t interlocked_add_i32(_Atomic(int32_t)* location1, int32_t value)      { return (method_result_t) { .value = atomic_fetch_add(location1, value) + value, .exception = NULL }; }
static method_result_t interlocked_add_u32(_Atomic(uint32_t)* location1, uint32_t value)    { return (method_result_t) { .value = atomic_fetch_add(location1, value) + value, .exception = NULL }; }
static method_result_t interlocked_add_i64(_Atomic(int64_t)* location1, int64_t value)      { return (method_result_t) { .value = atomic_fetch_add(location1, value) + value, .exception = NULL }; }
static method_result_t interlocked_add_u64(_Atomic(uint64_t)* location1, uint64_t value)    { return (method_result_t) { .value = atomic_fetch_add(location1, value) + value, .exception = NULL }; }

static method_result_t interlocked_and_i32(_Atomic(int32_t)* location1, int32_t value)      { return (method_result_t) { .value = atomic_fetch_and(location1, value) & value, .exception = NULL }; }
static method_result_t interlocked_and_u32(_Atomic(uint32_t)* location1, uint32_t value)    { return (method_result_t) { .value = atomic_fetch_and(location1, value) & value, .exception = NULL }; }
static method_result_t interlocked_and_i64(_Atomic(int64_t)* location1, int64_t value)      { return (method_result_t) { .value = atomic_fetch_and(location1, value) & value, .exception = NULL }; }
static method_result_t interlocked_and_u64(_Atomic(uint64_t)* location1, uint64_t value)    { return (method_result_t) { .value = atomic_fetch_and(location1, value) & value, .exception = NULL }; }

static method_result_t interlocked_compare_exchange_i32(_Atomic(int32_t)* location1, int32_t value, int32_t comparand) {
    atomic_compare_exchange_strong(location1, &value, comparand);
    return (method_result_t) { .value = value, .exception = NULL };
}

static method_result_t interlocked_compare_exchange_u32(_Atomic(uint32_t)* location1, uint32_t value, uint32_t comparand) {
    atomic_compare_exchange_strong(location1, &value, comparand);
    return (method_result_t) { .value = value, .exception = NULL };
}

static method_result_t interlocked_compare_exchange_i64(_Atomic(int64_t)* location1, int64_t value, int64_t comparand) {
    atomic_compare_exchange_strong(location1, &value, comparand);
    return (method_result_t) { .value = value, .exception = NULL };
}

static method_result_t interlocked_compare_exchange_u64(_Atomic(uint64_t)* location1, uint64_t value, uint64_t comparand) {
    atomic_compare_exchange_strong(location1, &value, comparand);
    return (method_result_t) { .value = value, .exception = NULL };
}

static method_result_t interlocked_dec_i32(_Atomic(int32_t)* location1)     { return (method_result_t) { .value = atomic_fetch_sub(location1, 1) - 1, .exception = NULL }; }
static method_result_t interlocked_dec_u32(_Atomic(uint32_t)* location1)    { return (method_result_t) { .value = atomic_fetch_sub(location1, 1) - 1, .exception = NULL }; }
static method_result_t interlocked_dec_i64(_Atomic(int64_t)* location1)     { return (method_result_t) { .value = atomic_fetch_sub(location1, 1) - 1, .exception = NULL }; }
static method_result_t interlocked_dec_u64(_Atomic(uint64_t)* location1)    { return (method_result_t) { .value = atomic_fetch_sub(location1, 1) - 1, .exception = NULL }; }

static method_result_t interlocked_exchange_i32(_Atomic(int32_t)* location1, int32_t value) { return (method_result_t)      { .value = atomic_exchange(location1, value), .exception = NULL }; }
static method_result_t interlocked_exchange_u32(_Atomic(uint32_t)* location1, uint32_t value) { return (method_result_t)    { .value = atomic_exchange(location1, value), .exception = NULL }; }
static method_result_t interlocked_exchange_i64(_Atomic(int64_t)* location1, int64_t value) { return (method_result_t)      { .value = atomic_exchange(location1, value), .exception = NULL }; }
static method_result_t interlocked_exchange_u64(_Atomic(uint64_t)* location1, uint64_t value) { return (method_result_t)    { .value = atomic_exchange(location1, value), .exception = NULL }; }

static method_result_t interlocked_inc_i32(_Atomic(int32_t)* location1)     { return (method_result_t) { .value = atomic_fetch_add(location1, 1) + 1, .exception = NULL }; }
static method_result_t interlocked_inc_u32(_Atomic(uint32_t)* location1)    { return (method_result_t) { .value = atomic_fetch_add(location1, 1) + 1, .exception = NULL }; }
static method_result_t interlocked_inc_i64(_Atomic(int64_t)* location1)     { return (method_result_t) { .value = atomic_fetch_add(location1, 1) + 1, .exception = NULL }; }
static method_result_t interlocked_inc_u64(_Atomic(uint64_t)* location1)    { return (method_result_t) { .value = atomic_fetch_add(location1, 1) + 1, .exception = NULL }; }

static System_Exception interlocked_memory_barrier() { return NULL; }

static method_result_t interlocked_or_i32(_Atomic(int32_t)* location1, int32_t value)      { return (method_result_t) { .value = atomic_fetch_or(location1, value) | value, .exception = NULL }; }
static method_result_t interlocked_or_u32(_Atomic(uint32_t)* location1, uint32_t value)    { return (method_result_t) { .value = atomic_fetch_or(location1, value) | value, .exception = NULL }; }
static method_result_t interlocked_or_i64(_Atomic(int64_t)* location1, int64_t value)      { return (method_result_t) { .value = atomic_fetch_or(location1, value) | value, .exception = NULL }; }
static method_result_t interlocked_or_u64(_Atomic(uint64_t)* location1, uint64_t value)    { return (method_result_t) { .value = atomic_fetch_or(location1, value) | value, .exception = NULL }; }

static method_result_t interlocked_read_i64(_Atomic(int64_t)* location1)    { return (method_result_t) { .value = atomic_load(location1), .exception = NULL }; }
static method_result_t interlocked_read_u64(_Atomic(uint64_t)* location1)   { return (method_result_t) { .value = atomic_load(location1), .exception = NULL }; }

//----------------------------------------------------------------------------------------------------------------------
// System.Array
//----------------------------------------------------------------------------------------------------------------------

static System_Exception System_Array_ClearInternal(System_Array array, int index, int length) {
    System_Type elementType = array->vtable->type->ElementType;
    int elementSize = elementType->StackSize;

    if (type_get_stack_type(elementType) == STACK_TYPE_O) {
        // we need a memset with a barrier so we won't
        // get preempted in the middle, the nice thing
        // is that the GC does not actually care for not
        // using a write barrier with a null value
        scheduler_preempt_disable();
        memset((void*)((uintptr_t) (array + 1) + index * elementSize), 0, length * elementSize);
        scheduler_preempt_enable();
    } else {
        // we don't need a barrier
        memset((void*)((uintptr_t) (array + 1) + index * elementSize), 0, length * elementSize);
    }

    return NULL;
}

static System_Exception System_Array_CopyInternal(System_Array sourceArray, int64_t sourceIndex, System_Array destinationArray, int64_t destinationIndex, int64_t length) {
    System_Type elementType = sourceArray->vtable->type->ElementType;
    int elementSize = elementType->StackSize;

    // TODO: have this check be done in the IL code
    ASSERT(elementType == destinationArray->vtable->type->ElementType);

    void* src_data = (void*)(sourceArray + 1) + sourceIndex * elementSize;
    void* dst_data = (void*)(destinationArray + 1) + destinationIndex * elementSize;
    size_t dst_offset = sizeof(struct System_Array) + (destinationIndex) * elementSize;
    size_t copy_size = length * elementSize;

    if (type_get_stack_type(elementType) == STACK_TYPE_VALUE_TYPE || type_is_interface(elementType)) {
        if (arrlen(elementType->ManagedPointersOffsets) == 0) {
            // fast path for copying non-managed struct arrays
            memmove(dst_data, src_data, copy_size);
        } else {
            // copying an array, copy each item as a managed memcpy
            if ((src_data < dst_data) && dst_data < src_data + copy_size) {
                for (dst_offset += copy_size, src_data += copy_size; length--; ) {
                    src_data -= elementSize;
                    dst_offset -= elementSize;
                    managed_memcpy((System_Object)destinationArray, elementType, dst_offset, src_data);
                }
            } else {
                while (length--) {
                    managed_memcpy((System_Object)destinationArray, elementType, dst_offset, src_data);
                    src_data += elementSize;
                    dst_offset += elementSize;
                }
            }
        }
    } else if (type_get_stack_type(elementType) == STACK_TYPE_O) {
        // we are copying objects, need to use a membarrier for each
        // TODO: maybe do group copies/barriers instead of one by one
        if ((src_data < dst_data) && dst_data < src_data + copy_size) {
            for (dst_offset += copy_size, src_data += copy_size; length--; ) {
                src_data -= elementSize;
                dst_offset -= elementSize;
                gc_update((System_Object)destinationArray, dst_offset, src_data);
            }
        } else {
            while (length--) {
                gc_update((System_Object)destinationArray, dst_offset, src_data);
                src_data += elementSize;
                dst_offset += elementSize;
            }
        }
    } else {
        // normal memcpy, no need to do anything special
        memmove(dst_data, src_data, copy_size);
    }

    return NULL;
}

//----------------------------------------------------------------------------------------------------------------------
// System.GC
//----------------------------------------------------------------------------------------------------------------------

static System_Exception System_GC_Collect(int generations, int collectionMode, bool blocking) {
    // turn Default to Forced
    if (collectionMode == 0) {
        collectionMode = 1;
    }

    // TODO: how do we wanna to treat the optimized one?

    // a full one is determined by this
    bool full = generations == 0 ? false : true;

    if (blocking) {
        gc_wait(full);
    } else {
        gc_wake(full);
    }

    return NULL;
}

static System_Exception System_GC_KeepAlive(void* obj) {
    return NULL;
}

//----------------------------------------------------------------------------------------------------------------------
// everything
//----------------------------------------------------------------------------------------------------------------------

internal_call_t g_internal_calls[] = {
    {
        "object::GetType()",
            object_GetType,
    },
    {
        "[Corelib-v1]System.Array::ClearInternal([Corelib-v1]System.Array,int32,int32)",
        System_Array_ClearInternal,
    },
    {
        "[Corelib-v1]System.Array::CopyInternal([Corelib-v1]System.Array,int64,[Corelib-v1]System.Array,int64,int64)",
        System_Array_CopyInternal,
    },
    {
        "[Corelib-v1]System.GC::Collect(int32,[Corelib-v1]System.GCCollectionMode,bool)",
        System_GC_Collect,
    },
    {
        "[Corelib-v1]System.GC::KeepAlive(object)",
        System_GC_KeepAlive,
    },

    { "[Corelib-v1]System.Threading.Interlocked::Add([Corelib-v1]System.Int32&,int32)", interlocked_add_i32 },
    { "[Corelib-v1]System.Threading.Interlocked::Add([Corelib-v1]System.UInt32&,uint32)", interlocked_add_u32 },
    { "[Corelib-v1]System.Threading.Interlocked::Add([Corelib-v1]System.Int64&,int64)", interlocked_add_i64 },
    { "[Corelib-v1]System.Threading.Interlocked::Add([Corelib-v1]System.UInt64&,uint64)", interlocked_add_u64 },

    { "[Corelib-v1]System.Threading.Interlocked::And([Corelib-v1]System.Int32&,int32)", interlocked_and_i32 },
    { "[Corelib-v1]System.Threading.Interlocked::And([Corelib-v1]System.UInt32&,uint32)", interlocked_and_u32 },
    { "[Corelib-v1]System.Threading.Interlocked::And([Corelib-v1]System.Int64&,int64)", interlocked_and_i64 },
    { "[Corelib-v1]System.Threading.Interlocked::And([Corelib-v1]System.UInt64&,uint64)", interlocked_and_u64 },

    { "[Corelib-v1]System.Threading.Interlocked::Decrement([Corelib-v1]System.Int32&)", interlocked_dec_i32 },
    { "[Corelib-v1]System.Threading.Interlocked::Decrement([Corelib-v1]System.UInt32&)", interlocked_dec_u32 },
    { "[Corelib-v1]System.Threading.Interlocked::Decrement([Corelib-v1]System.Int64&)", interlocked_dec_i64 },
    { "[Corelib-v1]System.Threading.Interlocked::Decrement([Corelib-v1]System.UInt64&)", interlocked_dec_u64 },

    { "[Corelib-v1]System.Threading.Interlocked::Exchange([Corelib-v1]System.Int32&,int32)", interlocked_exchange_i32 },
    { "[Corelib-v1]System.Threading.Interlocked::Exchange([Corelib-v1]System.UInt32&,uint32)", interlocked_exchange_u32 },
    { "[Corelib-v1]System.Threading.Interlocked::Exchange([Corelib-v1]System.Int64&,int64)", interlocked_exchange_i64 },
    { "[Corelib-v1]System.Threading.Interlocked::Exchange([Corelib-v1]System.UInt64&,uint64)", interlocked_exchange_u64 },

    { "[Corelib-v1]System.Threading.Interlocked::Increment([Corelib-v1]System.Int32&)", interlocked_inc_i32 },
    { "[Corelib-v1]System.Threading.Interlocked::Increment([Corelib-v1]System.UInt32&)", interlocked_inc_u32 },
    { "[Corelib-v1]System.Threading.Interlocked::Increment([Corelib-v1]System.Int64&)", interlocked_inc_i64 },
    { "[Corelib-v1]System.Threading.Interlocked::Increment([Corelib-v1]System.UInt64&)", interlocked_inc_u64 },

    { "[Corelib-v1]System.Threading.Interlocked::CompareExchange([Corelib-v1]System.Int32&,int32,int32)", interlocked_compare_exchange_i32 },
    { "[Corelib-v1]System.Threading.Interlocked::CompareExchange([Corelib-v1]System.UInt32&,uint32,uint32)", interlocked_compare_exchange_u32 },
    { "[Corelib-v1]System.Threading.Interlocked::CompareExchange([Corelib-v1]System.Int64&,int64,int64)", interlocked_compare_exchange_i64 },
    { "[Corelib-v1]System.Threading.Interlocked::CompareExchange([Corelib-v1]System.UInt64&,uint64,uint64)", interlocked_compare_exchange_u64 },

    { "[Corelib-v1]System.Threading.Interlocked::MemoryBarrier()", interlocked_memory_barrier },

    { "[Corelib-v1]System.Threading.Interlocked::Or([Corelib-v1]System.Int32&,int32)", interlocked_or_i32 },
    { "[Corelib-v1]System.Threading.Interlocked::Or([Corelib-v1]System.UInt32&,uint32)", interlocked_or_u32 },
    { "[Corelib-v1]System.Threading.Interlocked::Or([Corelib-v1]System.Int64&,int64)", interlocked_or_i64 },
    { "[Corelib-v1]System.Threading.Interlocked::Or([Corelib-v1]System.UInt64&,uint64)", interlocked_or_u64 },

    { "[Corelib-v1]System.Threading.Interlocked::Read([Corelib-v1]System.Int64&)", interlocked_read_i64 },
    { "[Corelib-v1]System.Threading.Interlocked::Read([Corelib-v1]System.UInt64&)", interlocked_read_u64 },

};

size_t g_internal_calls_count = ARRAY_LEN(g_internal_calls);
