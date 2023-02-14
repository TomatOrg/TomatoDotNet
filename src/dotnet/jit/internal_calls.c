#include "internal_calls.h"

#include "jit_internal.h"
#include "jit.h"

#include "../gc/gc.h"
#include "../types.h"
#include "time/tsc.h"
#include "converter.h"
#include "dotnet/loader.h"
#include "dotnet/activator.h"
#include "kernel.h"
#include "time/tick.h"
#include "dotnet/exception.h"

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

static int64_t System_Diagnostic_Stopwatch_GetTscFrequency() {
    return get_tsc_freq();
}

static int64_t System_Diagnostic_Stopwatch_GetTimestamp() {
    return get_tsc();
}

//----------------------------------------------------------------------------------------------------------------------
// System.Threading.Monitor
//----------------------------------------------------------------------------------------------------------------------

// TODO: this

//----------------------------------------------------------------------------------------------------------------------
// System.Runtime.Intrinsics.X86
//----------------------------------------------------------------------------------------------------------------------

static void System_Runtime_Intrinsics_X86_X86Base_Pause() {
    __builtin_ia32_pause();
}

//----------------------------------------------------------------------------------------------------------------------
// System.Threading.Thread
//----------------------------------------------------------------------------------------------------------------------

static void* System_Threading_Thread_get_CurrentThread(System_Object this) {
    return get_current_thread()->tcb->managed_thread;
}

static bool System_Threading_Thread_Yield() {
    // TODO: properly figure out if we got a reschedule or not, probably via
    //       implementing it in the scheduler_yield function
    scheduler_yield();
    return true;
}

static int32_t System_Threading_Thread_GetNativeThreadState() {
    return get_thread_status(get_current_thread());
}

static uint64_t System_Threading_CreateNativeThread(System_Delegate delegate, System_Object thread) {
    // first we need to get the invoke method, since it is going to be the actual
    // entry point of the function
    System_Type type = OBJECT_TYPE(delegate);
    System_Reflection_MethodInfo invoke = type->DelegateSignature;

    // create the thread, the parameter is the delegate instance, and set
    // the managed thread that is related to this thread
    thread_t* new_thread = create_thread(invoke->MirFunc->addr, delegate, "dotnet/thread");
    new_thread->tcb->managed_thread = thread;

    // we need to keep an instance of the thread since
    // the managed code stores it
    put_thread(new_thread);

    // return it
    return (uintptr_t)new_thread;
}

static void System_Threading_StartNativeThread(thread_t* thread, System_Object parameter) {
    // the first argument is set to be the delegate, the second
    // is going to be the actual parameter we want to pass
    thread->save_state.rsi = (uint64_t) parameter;

    // queue the thread for scheduling
    scheduler_ready_thread(thread);
}

static void System_Threading_ReleaseNativeThread(thread_t* thread) {
    release_thread(thread);
}

static void System_Threading_SetNativeThreadName(thread_t* thread, System_String name) {
    utf16_to_utf8(name->Chars, name->Length, (utf8_t*)thread->name, sizeof(thread->name));
}

static int32_t System_Threading_Thread_GetCurrentProcessorId() {
    return get_apic_id();
}

//----------------------------------------------------------------------------------------------------------------------
// System.Threading.Interlocked
//----------------------------------------------------------------------------------------------------------------------

static int32_t interlocked_add_i32(_Atomic(int32_t)* location1, int32_t value) { return atomic_fetch_add(location1, value) + value; }
static uint32_t interlocked_add_u32(_Atomic(uint32_t)* location1, uint32_t value) { return atomic_fetch_add(location1, value) + value; }
static int64_t interlocked_add_i64(_Atomic(int64_t)* location1, int64_t value) { return atomic_fetch_add(location1, value) + value; }
static uint64_t interlocked_add_u64(_Atomic(uint64_t)* location1, uint64_t value) { return atomic_fetch_add(location1, value) + value; }

static int32_t interlocked_and_i32(_Atomic(int32_t)* location1, int32_t value) { return atomic_fetch_and(location1, value); }
static uint32_t interlocked_and_u32(_Atomic(uint32_t)* location1, uint32_t value) { return atomic_fetch_and(location1, value); }
static int64_t interlocked_and_i64(_Atomic(int64_t)* location1, int64_t value) { return atomic_fetch_and(location1, value); }
static uint64_t interlocked_and_u64(_Atomic(uint64_t)* location1, uint64_t value) { return atomic_fetch_and(location1, value); }

static int32_t interlocked_compare_exchange_i32(_Atomic(int32_t)* location1, int32_t value, int32_t comparand) {
    atomic_compare_exchange_strong(location1, &comparand, value);
    return comparand;
}

static uint32_t interlocked_compare_exchange_u32(_Atomic(uint32_t)* location1, uint32_t value, uint32_t comparand) {
    atomic_compare_exchange_strong(location1, &comparand, value);
    return comparand;
}

static int64_t interlocked_compare_exchange_i64(_Atomic(int64_t)* location1, int64_t value, int64_t comparand) {
    atomic_compare_exchange_strong(location1, &comparand, value);
    return comparand;
}

static uint64_t interlocked_compare_exchange_u64(_Atomic(uint64_t)* location1, uint64_t value, uint64_t comparand) {
    atomic_compare_exchange_strong(location1, &comparand, value);
    return comparand;
}

static System_Object interlocked_compare_exchange_object(_Atomic(System_Object)* location1, System_Object value, System_Object comparand) {
    return gc_compare_exchange_ref(location1, value, comparand);
}

static int32_t interlocked_dec_i32(_Atomic(int32_t)* location1) { return atomic_fetch_sub(location1, 1) - 1; }
static uint32_t interlocked_dec_u32(_Atomic(uint32_t)* location1) { return atomic_fetch_sub(location1, 1) - 1; }
static int64_t interlocked_dec_i64(_Atomic(int64_t)* location1) { return atomic_fetch_sub(location1, 1) - 1; }
static uint64_t interlocked_dec_u64(_Atomic(uint64_t)* location1) { return atomic_fetch_sub(location1, 1) - 1; }

static int32_t interlocked_exchange_i32(_Atomic(int32_t)* location1, int32_t value) { return atomic_exchange(location1, value); }
static uint32_t interlocked_exchange_u32(_Atomic(uint32_t)* location1, uint32_t value) { return atomic_exchange(location1, value); }
static int64_t interlocked_exchange_i64(_Atomic(int64_t)* location1, int64_t value) { return atomic_exchange(location1, value); }
static uint64_t interlocked_exchange_u64(_Atomic(uint64_t)* location1, uint64_t value) { return atomic_exchange(location1, value); }
static System_Object interlocked_exchange_object(_Atomic(System_Object)* location1, System_Object value) {
    return gc_exchange_ref(location1, value);
}

static int32_t interlocked_inc_i32(_Atomic(int32_t)* location1) { return atomic_fetch_add(location1, 1) + 1; }
static uint32_t interlocked_inc_u32(_Atomic(uint32_t)* location1) { return atomic_fetch_add(location1, 1) + 1; }
static int64_t interlocked_inc_i64(_Atomic(int64_t)* location1) { return atomic_fetch_add(location1, 1) + 1; }
static uint64_t interlocked_inc_u64(_Atomic(uint64_t)* location1) { return atomic_fetch_add(location1, 1) + 1; }

static void interlocked_memory_barrier() { }

static int32_t interlocked_or_i32(_Atomic(int32_t)* location1, int32_t value) { return atomic_fetch_or(location1, value); }
static uint32_t interlocked_or_u32(_Atomic(uint32_t)* location1, uint32_t value) { return atomic_fetch_or(location1, value); }
static int64_t interlocked_or_i64(_Atomic(int64_t)* location1, int64_t value) { return atomic_fetch_or(location1, value); }
static uint64_t interlocked_or_u64(_Atomic(uint64_t)* location1, uint64_t value) { return atomic_fetch_or(location1, value); }

static int64_t interlocked_read_i64(_Atomic(int64_t)* location1) { return atomic_load(location1); }
static uint64_t interlocked_read_u64(_Atomic(uint64_t)* location1) { return atomic_load(location1); }

//----------------------------------------------------------------------------------------------------------------------
// System.Reflection.Assembly
//----------------------------------------------------------------------------------------------------------------------

static System_Reflection_Assembly System_Reflection_Assembly_LoadInternal_raw(System_Byte_Array rawAssembly, System_Boolean reflection) {
    err_t err = NO_ERROR;

    System_Reflection_Assembly assembly = NULL;
    CHECK_AND_RETHROW(loader_load_assembly(rawAssembly->Data, rawAssembly->Length, &assembly));

    // TODO: don't run custom attributes since this is essentially loading as non-reflection

    if (!reflection) {
        // check entry point is valid
        CHECK(assembly->EntryPoint->Parameters->Length == 0);
        CHECK(assembly->EntryPoint->ReturnType == NULL);

        // jit it
        CHECK_AND_RETHROW(jit_method(assembly->EntryPoint));

        // run it
        ((void(*)())assembly->EntryPoint->MirFunc->addr)();
    }

cleanup:
    if (IS_ERROR(err)) {
        exception_throw(activator_create_exception(tSystem_Exception));
    }

    return assembly;
}

static System_Reflection_Assembly System_Reflection_Assembly_LoadInternal_string(System_Byte_Array rawAssembly, System_Boolean reflection) {
    err_t err = NO_ERROR;

    System_Reflection_Assembly assembly = NULL;
    // TODO: load by-name, will either
    CHECK_FAIL();

    if (!reflection) {
        // check entry point is valid
        CHECK(assembly->EntryPoint->Parameters->Length == 0);
        CHECK(assembly->EntryPoint->ReturnType == NULL);

        // jit it
        CHECK_AND_RETHROW(jit_method(assembly->EntryPoint));

        // run it
        ((void(*)())assembly->EntryPoint->MirFunc->addr)();
    }

cleanup:
    if (IS_ERROR(err)) {
        exception_throw(activator_create_exception(tSystem_Exception));
    }

    return assembly;
}

//----------------------------------------------------------------------------------------------------------------------
// System.Activator
//----------------------------------------------------------------------------------------------------------------------

static System_Object System_Activator_InternalCreateInstance(System_Type type, System_Object_Array args) {
    System_Object obj = NULL;

    System_Object* argsPtr = NULL;
    int argsCount = 0;
    if (args != NULL) {
        argsPtr = args->Data;
        argsCount = args->Length;
    }

    switch (activator_create_instance(type, argsPtr, argsCount, &obj)) {
        case NO_ERROR: return obj;
        case ERROR_OUT_OF_MEMORY: exception_throw(activator_create_exception(tSystem_OutOfMemoryException));
        // TODO: handle nicely
        default: exception_throw(activator_create_exception(tSystem_Exception));
    }
}

//----------------------------------------------------------------------------------------------------------------------
// System.Array
//----------------------------------------------------------------------------------------------------------------------

static void do_array_copy(System_Array sourceArray, System_Array destinationArray, int64_t length) {
    System_Type elementType = OBJECT_TYPE(sourceArray)->ElementType;
    int elementSize = elementType->StackSize;

    ASSERT(elementType == OBJECT_TYPE(destinationArray)->ElementType);

    void* src_data = (void*)(sourceArray + 1);
    void* dst_data = (void*)(destinationArray + 1);
    size_t dst_offset = OBJECT_TYPE(destinationArray)->ManagedSize;
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
}

//----------------------------------------------------------------------------------------------------------------------
// System.GC
//----------------------------------------------------------------------------------------------------------------------

static void System_GC_Collect(int generations, int collectionMode, bool blocking) {
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
}

static void System_GC_KeepAlive(void* obj) {
}

//----------------------------------------------------------------------------------------------------------------------
// Attribute
//----------------------------------------------------------------------------------------------------------------------

static System_Object System_Attribute_GetCustomAttributeNative(System_Object element, System_Type attributeType, int* index) {
    // TODO: pass from the caller?
    System_Reflection_Assembly assembly = NULL;
    if (isinstance(element, tSystem_Reflection_MemberInfo)) {
        assembly = ((System_Reflection_MemberInfo) element)->Module->Assembly;
    } else if (isinstance(element, tSystem_Reflection_Assembly)) {
        assembly = (System_Reflection_Assembly)element;
    } else if (isinstance(element, tSystem_Reflection_Module)) {
        assembly = ((System_Reflection_Module) element)->Assembly;
    }

    if (assembly == NULL) {
        ASSERT(!"assembly should not be null at this point!");
    }

    int idx = hmgeti(assembly->CustomAttributeMap, element);
    if (idx >= 0) {
        System_Object* attributes = assembly->CustomAttributeMap[idx].value;
        for (; *index < arrlen(attributes); (*index)++) {
            if (OBJECT_TYPE(attributes[*index]) == attributeType) {
                return attributes[*index];
            }
        }
    }

    return NULL;
}

//----------------------------------------------------------------------------------------------------------------------
// System.Object
//----------------------------------------------------------------------------------------------------------------------

// TODO: generate this instead
static System_Type object_GetType(System_Object this) {
    if (this == NULL) {
        exception_throw(activator_create_exception(tSystem_NullReferenceException));
    } else {
        return OBJECT_TYPE(this);
    }
}

static System_Object object_MemberwiseClone(System_Object this) {
    System_Object newObject = NULL;

    System_Type type = OBJECT_TYPE(this);
    if (type->IsArray) {
        System_Array inArray = (System_Array)this;
        System_Array outArray = GC_NEW_ARRAY(type->ElementType, inArray->Length);
        do_array_copy(inArray, outArray, inArray->Length);
        newObject = (System_Object)outArray;
    } else if (type == tSystem_String) {
        System_String inStr = (System_String)this;
        System_String outStr = GC_NEW_STRING(inStr->Length);
        memcpy(outStr->Chars, inStr->Chars, sizeof(System_Char) * inStr->Length);
        newObject = (System_Object)outStr;
    } else {
        newObject = UNSAFE_GC_NEW(type);
        if (newObject == NULL) {
            exception_throw(activator_create_exception(tSystem_OutOfMemoryException));
        }

        managed_memcpy(newObject, type, 0, this);
    }

    return newObject;
}

//----------------------------------------------------------------------------------------------------------------------
// System.Type
//----------------------------------------------------------------------------------------------------------------------

static System_Type System_Type_InternalMakeGenericType(System_Type type, System_Type_Array arguments) {
    System_Type new_type = NULL;

    // create the generic type
    err_t err = type_make_generic(type, arguments, &new_type);
    if (err != NO_ERROR) {
        switch (err) {
            case ERROR_OUT_OF_MEMORY: exception_throw(activator_create_exception(tSystem_OutOfMemoryException));
            default: exception_throw(activator_create_exception(tSystem_Exception));
        }
    }

    // actually expand it now that it was created
    err = type_expand_generic(new_type);
    if (err != NO_ERROR) {
        switch (err) {
            case ERROR_OUT_OF_MEMORY: exception_throw(activator_create_exception(tSystem_OutOfMemoryException));
            default: exception_throw(activator_create_exception(tSystem_Exception));
        }
    }

    return new_type;
}

static int32_t System_Environment_GetProcessorCount() {
    return get_cpu_count();
}

static void System_Diagnostic_DebugProvider_WriteInternal(System_String str) {
    printf("%U", str);
}

//----------------------------------------------------------------------------------------------------------------------
// Synchronization stuff
//----------------------------------------------------------------------------------------------------------------------

static void monitor_enter(System_Object object) {
    // if this thread does not own the lock then simply try to lock
    // it directly with the mutex
    thread_t* thread = get_current_thread();
    if (object->lock_thread_id != thread->id) {
        mutex_lock(&object->mutex);
        object->lock_thread_id = thread->id;
    }

    // increment the depth and make sure it does not overflow
    object->lock_depth++;
    ASSERT(object->lock_depth != 0); // TODO: overflow
}

static void System_Threading_Monitor_Enter(System_Object object) {
    if (object == NULL) {
        exception_throw(activator_create_exception(tSystem_NullReferenceException));
    }

    monitor_enter(object);
}

static void System_Threading_Monitor_Exit(System_Object object) {
    if (object == NULL) {
        exception_throw(activator_create_exception(tSystem_NullReferenceException));
    }

    // verify this thread owns the lock
    if (object->lock_thread_id != get_current_thread()->id) {
        exception_throw(activator_create_exception(tSystem_Threading_SynchronizationLockException));
    }

    // decrement the recursion, if reaches zero then this is
    // no longer ours
    if (--object->lock_depth == 0) {
        object->lock_thread_id = 0;
        mutex_unlock(&object->mutex);
    }
}

static bool System_Threading_Monitor_IsEnteredNative(System_Object object) {
    return object->lock_thread_id == get_current_thread()->id;
}

static void System_Threading_Monitor_ReliableEnter(System_Object object, bool* lockTaken) {
    if (object == NULL) {
        exception_throw(activator_create_exception(tSystem_NullReferenceException));
    }

    monitor_enter(object);
    *lockTaken = true;
}

static bool System_Threading_Monitor_ObjWait(int millisecondTimeout, System_Object object) {
    // save the depth, and release the lock id so new threads can take it afterwards and see it
    // as not entered
    int lock_depth = object->lock_depth;
    object->lock_thread_id = 0;
    object->lock_depth = 0;

    bool result = condition_wait(&object->condition, &object->mutex, millisecondTimeout * TICKS_PER_MILLISECOND);

    // we have the lock again
    object->lock_thread_id = get_current_thread()->id;
    object->lock_depth = lock_depth;

    return result;
}

static void TinyDotNet_Sync_Mutex_Lock(mutex_t* mutex) {
    mutex_lock(mutex);
}

static void TinyDotNet_Sync_Mutex_Unlock(mutex_t* mutex) {
    mutex_unlock(mutex);
}

static bool TinyDotNet_Sync_Condition_Wait(condition_t* condition, mutex_t* mutex, int64_t timeoutMilliseconds) {
    return condition_wait(condition, mutex, timeoutMilliseconds * TICKS_PER_MILLISECOND);
}

static bool TinyDotNet_Sync_Condition_NotifyOne(condition_t* condition) {
    return condition_notify_one(condition);
}

static void TinyDotNet_Sync_Condition_NotifyAll(condition_t* condition) {
    condition_notify_all(condition);
}

//----------------------------------------------------------------------------------------------------------------------
// everything
//----------------------------------------------------------------------------------------------------------------------

internal_call_t g_internal_calls[] = {
    { "[Corelib-v1]System.Type object::GetType()", object_GetType },
    { "object object::MemberwiseClone()", object_MemberwiseClone, },

    { "[Corelib-v1]System.Reflection.Assembly [Corelib-v1]System.Reflection.Assembly::LoadInternal([Corelib-v1]System.Byte[],bool)", System_Reflection_Assembly_LoadInternal_raw },
    { "[Corelib-v1]System.Reflection.Assembly [Corelib-v1]System.Reflection.Assembly::LoadInternal(string,bool)", System_Reflection_Assembly_LoadInternal_string },

    { "object [Corelib-v1]System.Activator::InternalCreateInstance([Corelib-v1]System.Type,[Corelib-v1]System.Object[])", System_Activator_InternalCreateInstance },

    { "[Corelib-v1]System.GC::Collect(int32,[Corelib-v1]System.GCCollectionMode,bool)", System_GC_Collect },
    { "[Corelib-v1]System.GC::KeepAlive(object)", System_GC_KeepAlive },

    { "[Corelib-v1]System.Threading.Monitor::Enter(object)", System_Threading_Monitor_Enter },
    { "[Corelib-v1]System.Threading.Monitor::Exit(object)", System_Threading_Monitor_Exit },
    { "bool [Corelib-v1]System.Threading.Monitor::IsEnteredNative(object)", System_Threading_Monitor_IsEnteredNative },
    { "[Corelib-v1]System.Threading.Monitor::ReliableEnter(object,[Corelib-v1]System.Boolean&)", System_Threading_Monitor_ReliableEnter },
    { "bool [Corelib-v1]System.Threading.Monitor::ObjWait(int32,object)", System_Threading_Monitor_ObjWait },

    { "[Corelib-v1]TinyDotNet.Sync.Mutex::Lock()", TinyDotNet_Sync_Mutex_Lock },
    { "[Corelib-v1]TinyDotNet.Sync.Mutex::Unlock()", TinyDotNet_Sync_Mutex_Unlock },

    { "bool [Corelib-v1]TinyDotNet.Sync.Condition::Wait([Corelib-v1]TinyDotNet.Sync.Mutex&,int64)", TinyDotNet_Sync_Condition_Wait },
    { "bool [Corelib-v1]TinyDotNet.Sync.Condition::NotifyOne()", TinyDotNet_Sync_Condition_NotifyOne },
    { "[Corelib-v1]TinyDotNet.Sync.Condition::NotifyAll()", TinyDotNet_Sync_Condition_NotifyAll },

    { "[Corelib-v1]System.Runtime.Intrinsics.X86.X86Base::Pause()", System_Runtime_Intrinsics_X86_X86Base_Pause },

    { "int64 [Corelib-v1]System.Diagnostics.Stopwatch::GetTscFrequency()", System_Diagnostic_Stopwatch_GetTscFrequency },
    { "int64 [Corelib-v1]System.Diagnostics.Stopwatch::GetTimestamp()", System_Diagnostic_Stopwatch_GetTimestamp },

    { "[Corelib-v1]System.Threading.Thread [Corelib-v1]System.Threading.Thread::get_CurrentThread()",                                    System_Threading_Thread_get_CurrentThread },
    { "bool [Corelib-v1]System.Threading.Thread::Yield()",                                                                               System_Threading_Thread_Yield },
    { "int32 [Corelib-v1]System.Threading.Thread::GetNativeThreadState(uint64)",                                                         System_Threading_Thread_GetNativeThreadState },
    { "uint64 [Corelib-v1]System.Threading.Thread::CreateNativeThread([Corelib-v1]System.Delegate,[Corelib-v1]System.Threading.Thread)", System_Threading_CreateNativeThread },
    { "[Corelib-v1]System.Threading.Thread::StartNativeThread(uint64,object)",                   System_Threading_StartNativeThread },
    { "[Corelib-v1]System.Threading.Thread::ReleaseNativeThread(uint64)",                        System_Threading_ReleaseNativeThread },
    { "[Corelib-v1]System.Threading.Thread::SetNativeThreadName(uint64,string)",                 System_Threading_SetNativeThreadName },
    { "int32 [Corelib-v1]System.Threading.Thread::GetCurrentProcessorId()",                      System_Threading_Thread_GetCurrentProcessorId },

    { "int32 [Corelib-v1]System.Threading.Interlocked::Add([Corelib-v1]System.Int32&,int32)",    interlocked_add_i32 },
    { "uint32 [Corelib-v1]System.Threading.Interlocked::Add([Corelib-v1]System.UInt32&,uint32)", interlocked_add_u32 },
    { "int64 [Corelib-v1]System.Threading.Interlocked::Add([Corelib-v1]System.Int64&,int64)",    interlocked_add_i64 },
    { "uint64 [Corelib-v1]System.Threading.Interlocked::Add([Corelib-v1]System.UInt64&,uint64)", interlocked_add_u64 },

    { "int32 [Corelib-v1]System.Threading.Interlocked::And([Corelib-v1]System.Int32&,int32)",    interlocked_and_i32 },
    { "uint32 [Corelib-v1]System.Threading.Interlocked::And([Corelib-v1]System.UInt32&,uint32)", interlocked_and_u32 },
    { "int64 [Corelib-v1]System.Threading.Interlocked::And([Corelib-v1]System.Int64&,int64)",    interlocked_and_i64 },
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

    { "[Corelib-v1]System.Reflection.Assembly::Finalize()", assembly_finalizer },

    { "[Corelib-v1]System.Attribute [Corelib-v1]System.Attribute::GetCustomAttributeNative(object,[Corelib-v1]System.Type,[Corelib-v1]System.Int32&)", System_Attribute_GetCustomAttributeNative },

    { "[Corelib-v1]System.Type [Corelib-v1]System.Type::InternalMakeGenericType([Corelib-v1]System.Type[])", System_Type_InternalMakeGenericType },

    { "int32 [Corelib-v1]System.Environment::GetProcessorCount()", System_Environment_GetProcessorCount },

    { "[Corelib-v1]System.Diagnostics.DebugProvider::WriteInternal(string)", System_Diagnostic_DebugProvider_WriteInternal },

};

size_t g_internal_calls_count = ARRAY_LEN(g_internal_calls);
