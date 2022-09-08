#include "internal_calls.h"

#include "jit.h"

#include "../gc/gc.h"
#include "../types.h"
#include "time/tsc.h"
#include "thread/waitable.h"
#include "converter.h"
#include "dotnet/monitor.h"
#include "dotnet/loader.h"
#include "dotnet/activator.h"

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
        gc_update(this, offset + current_offset, *(System_Object*)(from + current_offset));

        // increment past the pointer
        current_offset += sizeof(void*);

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
// System.Diagnostic.Stopwatch
//----------------------------------------------------------------------------------------------------------------------

static method_result_t System_Diagnostic_Stopwatch_GetTscFrequency() {
    return (method_result_t) { .exception = NULL, .value = get_tsc_freq() * 1000000 };
}

static method_result_t System_Diagnostic_Stopwatch_GetTimestamp() {
    return (method_result_t) { .exception = NULL, .value = get_tsc() };
}

//----------------------------------------------------------------------------------------------------------------------
// System.Threading.Monitor
//----------------------------------------------------------------------------------------------------------------------

// TODO: figure how to properly throw errors from native code...

static method_result_t System_Threading_Monitor_EnterInternal(System_Object obj, bool* lockTaken) {
    err_t err = monitor_enter(obj);
    if (err == NO_ERROR) {
        *lockTaken = true;
    }
    return (method_result_t) { .exception = NULL, .value = err };
}

static method_result_t System_Threading_Monitor_ExitInternal(System_Object obj) {
    err_t err = monitor_exit(obj);
    return (method_result_t) { .exception = NULL, .value = err };
}

static method_result_t System_Threading_Monitor_PulseInternal(System_Object obj) {
    err_t err = monitor_pulse(obj);
    return (method_result_t) { .exception = NULL, .value = err };
}

static method_result_t System_Threading_Monitor_PulseAllInternal(System_Object obj) {
    err_t err = monitor_pulse_all(obj);
    return (method_result_t) { .exception = NULL, .value = err };
}

static method_result_t System_Threading_Monitor_WaitInternal(System_Object obj) {
    err_t err = monitor_wait(obj);
    return (method_result_t) { .exception = NULL, .value = err };
}

//----------------------------------------------------------------------------------------------------------------------
// System.Threading.WaitHandle
//----------------------------------------------------------------------------------------------------------------------

static method_result_t System_Threading_WaitHandle_WaitableSend(waitable_t* waitable, bool block) {
    return (method_result_t) { .exception = NULL, .value = waitable_send(waitable, block) };
}

static method_result_t System_Threading_WaitHandle_WaitableWait(waitable_t* waitable, bool block) {
    return (method_result_t) { .exception = NULL, .value = waitable_wait(waitable, block) };
}

static method_result_t System_Threading_WaitHandle_WaitableSelect2(waitable_t* w1, waitable_t* w2, bool block) {
    waitable_t* waitables[] = { w1, w2 };
    return (method_result_t) { .exception = NULL, .value = waitable_select(waitables, 0, 2, block).index };
}

static method_result_t System_Threading_WaitHandle_CreateWaitable(int count) {
    return (method_result_t) { .exception = NULL, .value = (uintptr_t) create_waitable(count)};
}

static method_result_t System_Threading_WaitHandle_WaitableAfter(int64_t timeout) {
    return (method_result_t) { .exception = NULL, .value = (uintptr_t) after(timeout)};
}

static method_result_t System_Threading_WaitHandle_ReleaseWaitable(waitable_t* w) {
    release_waitable(w);
    return (method_result_t) { .exception = NULL, .value = 0 };
}

static System_Exception System_Threading_WaitHandle_WaitableClose(waitable_t* w) {
    waitable_close(w);
    return NULL;
}

static method_result_t System_Threading_WaitHandle_PutWaitable(waitable_t* w) {
    return (method_result_t) { .exception = NULL, .value = (uintptr_t)put_waitable(w) };
}

//----------------------------------------------------------------------------------------------------------------------
// System.Runtime.Intrinsics.X86
//----------------------------------------------------------------------------------------------------------------------

static System_Exception System_Runtime_Intrinsics_X86_X86Base_Pause() {
    __builtin_ia32_pause();
    return NULL;
}

//----------------------------------------------------------------------------------------------------------------------
// System.Threading.Thread
//----------------------------------------------------------------------------------------------------------------------

static method_result_t System_Threading_Thread_get_CurrentThread(System_Object this) {
    return (method_result_t) { .exception = NULL, .value = (uintptr_t) get_current_thread()->tcb->managed_thread };
}

static method_result_t System_Threading_Thread_Yield() {
    // TODO: properly figure out if we got a reschedule or not, probably via
    //       implementing it in the scheduler_yield function
    scheduler_yield();
    return (method_result_t){ .exception = NULL, .value = true };
}

static method_result_t System_Threading_Thread_GetNativeThreadState() {
    return (method_result_t){ .exception = NULL, .value = get_thread_status(get_current_thread()) };
}

static method_result_t System_Threading_CreateNativeThread(System_Delegate delegate, System_Object thread) {
    // first we need to get the invoke method, since it is going to be the actual
    // entry point of the function
    System_Type type = OBJECT_TYPE(delegate);
    System_Reflection_MethodInfo invoke = type->DelegateSignature;

    // create the thread, the parameter is the delegate instance, and set
    // the managed thread that is related to this thread
    thread_t* new_thread = create_thread(invoke->MirFunc->addr, delegate, "dotnet/thread");
    new_thread->tcb->managed_thread = new_thread;

    // we need to keep an instance of the thread since
    // the managed code stores it
    put_thread(new_thread);

    // return it
    return (method_result_t){ .exception = NULL, .value = (uintptr_t) new_thread};
}

static System_Exception System_Threading_StartNativeThread(thread_t* thread, System_Object parameter) {
    // the first argument is set to be the delegate, the second
    // is going to be the actual parameter we want to pass
    thread->save_state.rsi = (uint64_t) parameter;

    // queue the thread for scheduling
    scheduler_ready_thread(thread);

    // no exception
    return NULL;
}

static System_Exception System_Threading_ReleaseNativeThread(thread_t* thread) {
    release_thread(thread);
    return NULL;
}

static System_Exception System_Threading_SetNativeThreadName(thread_t* thread, System_String name) {
    utf16_to_utf8(name->Chars, name->Length, (utf8_t*)thread->name, sizeof(thread->name));
    return NULL;
}

//----------------------------------------------------------------------------------------------------------------------
// System.Object
//----------------------------------------------------------------------------------------------------------------------

// TODO: generate this instead
static method_result_t object_GetType(System_Object this) {
    return (method_result_t){ .exception = NULL, .value = (uintptr_t) OBJECT_TYPE(this) };
}

//----------------------------------------------------------------------------------------------------------------------
// System.Threading.Interlocked
//----------------------------------------------------------------------------------------------------------------------

static method_result_t interlocked_add_i32(_Atomic(int32_t)* location1, int32_t value)      { return (method_result_t) { .value = atomic_fetch_add(location1, value) + value, .exception = NULL }; }
static method_result_t interlocked_add_u32(_Atomic(uint32_t)* location1, uint32_t value)    { return (method_result_t) { .value = atomic_fetch_add(location1, value) + value, .exception = NULL }; }
static method_result_t interlocked_add_i64(_Atomic(int64_t)* location1, int64_t value)      { return (method_result_t) { .value = atomic_fetch_add(location1, value) + value, .exception = NULL }; }
static method_result_t interlocked_add_u64(_Atomic(uint64_t)* location1, uint64_t value)    { return (method_result_t) { .value = atomic_fetch_add(location1, value) + value, .exception = NULL }; }

static method_result_t interlocked_and_i32(_Atomic(int32_t)* location1, int32_t value)      { return (method_result_t) { .value = atomic_fetch_and(location1, value), .exception = NULL }; }
static method_result_t interlocked_and_u32(_Atomic(uint32_t)* location1, uint32_t value)    { return (method_result_t) { .value = atomic_fetch_and(location1, value), .exception = NULL }; }
static method_result_t interlocked_and_i64(_Atomic(int64_t)* location1, int64_t value)      { return (method_result_t) { .value = atomic_fetch_and(location1, value), .exception = NULL }; }
static method_result_t interlocked_and_u64(_Atomic(uint64_t)* location1, uint64_t value)    { return (method_result_t) { .value = atomic_fetch_and(location1, value), .exception = NULL }; }

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

static method_result_t interlocked_compare_exchange_object(_Atomic(System_Object)* location1, System_Object value, System_Object comparand) {
    return (method_result_t) { .value = (uintptr_t)gc_compare_exchange_ref(location1, value, comparand), .exception = NULL };
}

static method_result_t interlocked_dec_i32(_Atomic(int32_t)* location1)     { return (method_result_t) { .value = atomic_fetch_sub(location1, 1), .exception = NULL }; }
static method_result_t interlocked_dec_u32(_Atomic(uint32_t)* location1)    { return (method_result_t) { .value = atomic_fetch_sub(location1, 1), .exception = NULL }; }
static method_result_t interlocked_dec_i64(_Atomic(int64_t)* location1)     { return (method_result_t) { .value = atomic_fetch_sub(location1, 1), .exception = NULL }; }
static method_result_t interlocked_dec_u64(_Atomic(uint64_t)* location1)    { return (method_result_t) { .value = atomic_fetch_sub(location1, 1), .exception = NULL }; }

static method_result_t interlocked_exchange_i32(_Atomic(int32_t)* location1, int32_t value) { return (method_result_t)      { .value = atomic_exchange(location1, value), .exception = NULL }; }
static method_result_t interlocked_exchange_u32(_Atomic(uint32_t)* location1, uint32_t value) { return (method_result_t)    { .value = atomic_exchange(location1, value), .exception = NULL }; }
static method_result_t interlocked_exchange_i64(_Atomic(int64_t)* location1, int64_t value) { return (method_result_t)      { .value = atomic_exchange(location1, value), .exception = NULL }; }
static method_result_t interlocked_exchange_u64(_Atomic(uint64_t)* location1, uint64_t value) { return (method_result_t)    { .value = atomic_exchange(location1, value), .exception = NULL }; }
static method_result_t interlocked_exchange_object(_Atomic(System_Object)* location1, System_Object value) {
    return (method_result_t) { .value = (uintptr_t)gc_exchange_ref(location1, value), .exception = NULL };
}

static method_result_t interlocked_inc_i32(_Atomic(int32_t)* location1)     { return (method_result_t) { .value = atomic_fetch_add(location1, 1), .exception = NULL }; }
static method_result_t interlocked_inc_u32(_Atomic(uint32_t)* location1)    { return (method_result_t) { .value = atomic_fetch_add(location1, 1), .exception = NULL }; }
static method_result_t interlocked_inc_i64(_Atomic(int64_t)* location1)     { return (method_result_t) { .value = atomic_fetch_add(location1, 1), .exception = NULL }; }
static method_result_t interlocked_inc_u64(_Atomic(uint64_t)* location1)    { return (method_result_t) { .value = atomic_fetch_add(location1, 1), .exception = NULL }; }

static System_Exception interlocked_memory_barrier() { return NULL; }

static method_result_t interlocked_or_i32(_Atomic(int32_t)* location1, int32_t value)      { return (method_result_t) { .value = atomic_fetch_or(location1, value), .exception = NULL }; }
static method_result_t interlocked_or_u32(_Atomic(uint32_t)* location1, uint32_t value)    { return (method_result_t) { .value = atomic_fetch_or(location1, value), .exception = NULL }; }
static method_result_t interlocked_or_i64(_Atomic(int64_t)* location1, int64_t value)      { return (method_result_t) { .value = atomic_fetch_or(location1, value), .exception = NULL }; }
static method_result_t interlocked_or_u64(_Atomic(uint64_t)* location1, uint64_t value)    { return (method_result_t) { .value = atomic_fetch_or(location1, value), .exception = NULL }; }

static method_result_t interlocked_read_i64(_Atomic(int64_t)* location1)    { return (method_result_t) { .value = atomic_load(location1), .exception = NULL }; }
static method_result_t interlocked_read_u64(_Atomic(uint64_t)* location1)   { return (method_result_t) { .value = atomic_load(location1), .exception = NULL }; }

//----------------------------------------------------------------------------------------------------------------------
// System.Reflection.Assembly
//----------------------------------------------------------------------------------------------------------------------

static method_result_t System_Reflection_Assembly_LoadInternal_raw(System_Byte_Array rawAssembly, System_Boolean reflection) {
    err_t err = NO_ERROR;
    System_Exception exception = NULL;

    System_Reflection_Assembly assembly = NULL;
    CHECK_AND_RETHROW(loader_load_assembly(rawAssembly->Data, rawAssembly->Length, &assembly));

    // TODO: don't run custom attributes since this is essentially loading as non-reflection

    if (!reflection) {
        // TODO: instead we probably want to jit specifically the entry point
        //       and not the whole type, just to have less to see
        // TODO: time this
        CHECK_AND_RETHROW(jit_type(assembly->EntryPoint->DeclaringType));
        CHECK(assembly->EntryPoint->Parameters->Length == 0);
        CHECK(assembly->EntryPoint->ReturnType == NULL);
        exception = ((System_Exception(*)())assembly->EntryPoint->MirFunc->addr)();
        CHECK(exception == NULL);
    }

cleanup:
    if (IS_ERROR(err)) {
        assembly = NULL;
    }

    return (method_result_t) { .exception = exception, .value = (uintptr_t)assembly };
}

static method_result_t System_Reflection_Assembly_LoadInternal_string(System_Byte_Array rawAssembly, System_Boolean reflection) {
    err_t err = NO_ERROR;
    System_Exception exception = NULL;

    System_Reflection_Assembly assembly = NULL;
    // TODO: load by-name, will either
    CHECK_FAIL();

    if (!reflection) {
        // TODO: instead we probably want to jit specifically the entry point
        //       and not the whole type, just to have less to see
        // TODO: time this
        CHECK_AND_RETHROW(jit_type(assembly->EntryPoint->DeclaringType));
        CHECK(assembly->EntryPoint->Parameters->Length == 0);
        CHECK(assembly->EntryPoint->ReturnType == NULL);
        exception = ((System_Exception(*)())assembly->EntryPoint->MirFunc->addr)();
        CHECK(exception == NULL);
    }

cleanup:
    if (IS_ERROR(err)) {
        assembly = NULL;
    }

    return (method_result_t) { .exception = exception, .value = (uintptr_t)assembly };
}

//----------------------------------------------------------------------------------------------------------------------
// System.Activator
//----------------------------------------------------------------------------------------------------------------------

static method_result_t System_Activator_CreateInstance(System_Type type, System_Object_Array args) {
    System_Object obj = NULL;
    switch (activator_create_instance(type, args->Data, args->Length, &obj)) {
        case NO_ERROR: return (method_result_t){ .exception = NULL, .value = (uintptr_t)obj };
        case ERROR_OUT_OF_MEMORY: return (method_result_t){ .exception = activator_create_exception(tSystem_OutOfMemoryException), .value = 0 };
        // TODO: handle nicely
        default: return (method_result_t){ .exception = activator_create_exception(tSystem_Exception), .value = 0 };
    }
}

//----------------------------------------------------------------------------------------------------------------------
// System.Array
//----------------------------------------------------------------------------------------------------------------------

static System_Exception System_Array_ClearInternal(System_Array array, int index, int length) {
    System_Type elementType = OBJECT_TYPE(array)->ElementType;
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
    System_Type elementType = OBJECT_TYPE(sourceArray)->ElementType;
    int elementSize = elementType->StackSize;

    // TODO: have this check be done in the IL code
    ASSERT(elementType == OBJECT_TYPE(destinationArray)->ElementType);

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
                gc_update((System_Object)destinationArray, dst_offset, *(void**)src_data);
            }
        } else {
            while (length--) {
                gc_update((System_Object)destinationArray, dst_offset, *(void**)src_data);
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
    { "[Corelib-v1]System.Type object::GetType()", object_GetType },

    { "[Corelib-v1]System.Reflection.Assembly [Corelib-v1]System.Reflection.Assembly::LoadInternal([Corelib-v1]System.Byte[],bool)", System_Reflection_Assembly_LoadInternal_raw },
    { "[Corelib-v1]System.Reflection.Assembly [Corelib-v1]System.Reflection.Assembly::LoadInternal(string,bool)", System_Reflection_Assembly_LoadInternal_string },

    { "object [Corelib-v1]System.Activator::CreateInstance([Corelib-v1]System.Type,[Corelib-v1]System.Object[])", System_Activator_CreateInstance },

    { "[Corelib-v1]System.Array::ClearInternal([Corelib-v1]System.Array,int32,int32)", System_Array_ClearInternal },
    { "[Corelib-v1]System.Array::CopyInternal([Corelib-v1]System.Array,int64,[Corelib-v1]System.Array,int64,int64)", System_Array_CopyInternal },

    { "[Corelib-v1]System.GC::Collect(int32,[Corelib-v1]System.GCCollectionMode,bool)", System_GC_Collect },
    { "[Corelib-v1]System.GC::KeepAlive(object)", System_GC_KeepAlive },

    { "int32 [Corelib-v1]System.Threading.Monitor::EnterInternal(object,[Corelib-v1]System.Boolean&)", System_Threading_Monitor_EnterInternal },
    { "int32 [Corelib-v1]System.Threading.Monitor::ExitInternal(object)", System_Threading_Monitor_ExitInternal },
    { "int32 [Corelib-v1]System.Threading.Monitor::PulseInternal(object)", System_Threading_Monitor_PulseInternal },
    { "int32 [Corelib-v1]System.Threading.Monitor::PulseAllInternal(object)", System_Threading_Monitor_PulseAllInternal },
    { "int32 [Corelib-v1]System.Threading.Monitor::WaitInternal(object)", System_Threading_Monitor_WaitInternal },

    { "[Corelib-v1]System.Runtime.Intrinsics.X86.X86Base::Pause()", System_Runtime_Intrinsics_X86_X86Base_Pause },

    { "int64 [Corelib-v1]System.Diagnostics.Stopwatch::GetTscFrequency()", System_Diagnostic_Stopwatch_GetTscFrequency },
    { "int64 [Corelib-v1]System.Diagnostics.Stopwatch::GetTimestamp()", System_Diagnostic_Stopwatch_GetTimestamp },

    { "[Corelib-v1]System.Threading.Thread [Corelib-v1]System.Threading.Thread::get_CurrentThread()", System_Threading_Thread_get_CurrentThread },
    { "bool [Corelib-v1]System.Threading.Thread::Yield()", System_Threading_Thread_Yield },
    { "int32 [Corelib-v1]System.Threading.Thread::GetNativeThreadState(uint64)", System_Threading_Thread_GetNativeThreadState },
    { "uint64 [Corelib-v1]System.Threading.Thread::CreateNativeThread([Corelib-v1]System.Delegate,[Corelib-v1]System.Threading.Thread)", System_Threading_CreateNativeThread },
    { "[Corelib-v1]System.Threading.Thread::StartNativeThread(uint64,object)", System_Threading_StartNativeThread },
    { "[Corelib-v1]System.Threading.Thread::ReleaseNativeThread(uint64)", System_Threading_ReleaseNativeThread },
    { "[Corelib-v1]System.Threading.Thread::SetNativeThreadName(uint64,string)", System_Threading_SetNativeThreadName },

    { "bool [Corelib-v1]System.Threading.WaitHandle::WaitableSend(uint64,bool)",             System_Threading_WaitHandle_WaitableSend },
    { "int32 [Corelib-v1]System.Threading.WaitHandle::WaitableWait(uint64,bool)",             System_Threading_WaitHandle_WaitableWait },
    { "int32 [Corelib-v1]System.Threading.WaitHandle::WaitableSelect2(uint64,uint64,bool)",   System_Threading_WaitHandle_WaitableSelect2 },
    { "uint64 [Corelib-v1]System.Threading.WaitHandle::CreateWaitable(int32)",                 System_Threading_WaitHandle_CreateWaitable },
    { "uint64 [Corelib-v1]System.Threading.WaitHandle::WaitableAfter(int64)",                  System_Threading_WaitHandle_WaitableAfter },
    { "[Corelib-v1]System.Threading.WaitHandle::ReleaseWaitable(uint64)",               System_Threading_WaitHandle_ReleaseWaitable },
    { "[Corelib-v1]System.Threading.WaitHandle::WaitableClose(uint64)",                 System_Threading_WaitHandle_WaitableClose },
    { "uint64 [Corelib-v1]System.Threading.WaitHandle::PutWaitable(uint64)",                   System_Threading_WaitHandle_PutWaitable },

    { "int32 [Corelib-v1]System.Threading.Interlocked::Add([Corelib-v1]System.Int32&,int32)", interlocked_add_i32 },
    { "uint32 [Corelib-v1]System.Threading.Interlocked::Add([Corelib-v1]System.UInt32&,uint32)", interlocked_add_u32 },
    { "int64 [Corelib-v1]System.Threading.Interlocked::Add([Corelib-v1]System.Int64&,int64)", interlocked_add_i64 },
    { "uint64 [Corelib-v1]System.Threading.Interlocked::Add([Corelib-v1]System.UInt64&,uint64)", interlocked_add_u64 },

    { "int32 [Corelib-v1]System.Threading.Interlocked::And([Corelib-v1]System.Int32&,int32)", interlocked_and_i32 },
    { "uint32 [Corelib-v1]System.Threading.Interlocked::And([Corelib-v1]System.UInt32&,uint32)", interlocked_and_u32 },
    { "int64 [Corelib-v1]System.Threading.Interlocked::And([Corelib-v1]System.Int64&,int64)", interlocked_and_i64 },
    { "uint64 [Corelib-v1]System.Threading.Interlocked::And([Corelib-v1]System.UInt64&,uint64)", interlocked_and_u64 },

    { "int32 [Corelib-v1]System.Threading.Interlocked::Decrement([Corelib-v1]System.Int32&)", interlocked_dec_i32 },
    { "uint32 [Corelib-v1]System.Threading.Interlocked::Decrement([Corelib-v1]System.UInt32&)", interlocked_dec_u32 },
    { "int64 [Corelib-v1]System.Threading.Interlocked::Decrement([Corelib-v1]System.Int64&)", interlocked_dec_i64 },
    { "uint64 [Corelib-v1]System.Threading.Interlocked::Decrement([Corelib-v1]System.UInt64&)", interlocked_dec_u64 },

    { "int32 [Corelib-v1]System.Threading.Interlocked::Exchange([Corelib-v1]System.Int32&,int32)", interlocked_exchange_i32 },
    { "uint32 [Corelib-v1]System.Threading.Interlocked::Exchange([Corelib-v1]System.UInt32&,uint32)", interlocked_exchange_u32 },
    { "int64 [Corelib-v1]System.Threading.Interlocked::Exchange([Corelib-v1]System.Int64&,int64)", interlocked_exchange_i64 },
    { "uint64 [Corelib-v1]System.Threading.Interlocked::Exchange([Corelib-v1]System.UInt64&,uint64)", interlocked_exchange_u64 },
    { "object [Corelib-v1]System.Threading.Interlocked::Exchange([Corelib-v1]System.Object&,object)", interlocked_exchange_object },

    { "int32 [Corelib-v1]System.Threading.Interlocked::Increment([Corelib-v1]System.Int32&)", interlocked_inc_i32 },
    { "uint32 [Corelib-v1]System.Threading.Interlocked::Increment([Corelib-v1]System.UInt32&)", interlocked_inc_u32 },
    { "int64 [Corelib-v1]System.Threading.Interlocked::Increment([Corelib-v1]System.Int64&)", interlocked_inc_i64 },
    { "uint64 [Corelib-v1]System.Threading.Interlocked::Increment([Corelib-v1]System.UInt64&)", interlocked_inc_u64 },

    { "int32 [Corelib-v1]System.Threading.Interlocked::CompareExchange([Corelib-v1]System.Int32&,int32,int32)", interlocked_compare_exchange_i32 },
    { "uint32 [Corelib-v1]System.Threading.Interlocked::CompareExchange([Corelib-v1]System.UInt32&,uint32,uint32)", interlocked_compare_exchange_u32 },
    { "int64 [Corelib-v1]System.Threading.Interlocked::CompareExchange([Corelib-v1]System.Int64&,int64,int64)", interlocked_compare_exchange_i64 },
    { "uint64 [Corelib-v1]System.Threading.Interlocked::CompareExchange([Corelib-v1]System.UInt64&,uint64,uint64)", interlocked_compare_exchange_u64 },
    { "object [Corelib-v1]System.Threading.Interlocked::CompareExchange([Corelib-v1]System.Object&,object,object)", interlocked_compare_exchange_object },

    { "[Corelib-v1]System.Threading.Interlocked::MemoryBarrier()", interlocked_memory_barrier },

    { "int32 [Corelib-v1]System.Threading.Interlocked::Or([Corelib-v1]System.Int32&,int32)", interlocked_or_i32 },
    { "uint32 [Corelib-v1]System.Threading.Interlocked::Or([Corelib-v1]System.UInt32&,uint32)", interlocked_or_u32 },
    { "int64 [Corelib-v1]System.Threading.Interlocked::Or([Corelib-v1]System.Int64&,int64)", interlocked_or_i64 },
    { "uint64 [Corelib-v1]System.Threading.Interlocked::Or([Corelib-v1]System.UInt64&,uint64)", interlocked_or_u64 },

    { "int64 [Corelib-v1]System.Threading.Interlocked::Read([Corelib-v1]System.Int64&)", interlocked_read_i64 },
    { "uint64 [Corelib-v1]System.Threading.Interlocked::Read([Corelib-v1]System.UInt64&)", interlocked_read_u64 },

};

size_t g_internal_calls_count = ARRAY_LEN(g_internal_calls);
