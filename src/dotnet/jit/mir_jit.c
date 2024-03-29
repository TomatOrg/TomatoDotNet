#include "jit.h"
#include "thread/scheduler.h"
#include "internal_calls.h"
#include "debug/debug.h"
#include "dotnet/filler.h"

#include <dotnet/opcodes.h>
#include <dotnet/types.h>
#include <dotnet/gc/gc.h>
#include <dotnet/loader.h>
#include <dotnet/gc/heap.h>

#include <mem/malloc.h>

#include <util/except.h>
#include <util/stb_ds.h>
#include <time/tsc.h>

#include <mir/mir-gen.h>
#include <mir/mir.h>

#include <stdint.h>
#include <stddef.h>

#include "opcodes/opcodes.h"
#include "sync/rwmutex.h"

// TODO: we need a mir try-catch so we can recover from mir errors


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// functions we need for the runtime
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

MIR_item_t m_dynamic_cast_obj_to_interface_proto = NULL;
MIR_item_t m_dynamic_cast_obj_to_interface_func = NULL;
MIR_item_t m_is_instance_proto = NULL;
MIR_item_t m_is_instance_func = NULL;
MIR_item_t m_gc_new_proto = NULL;
MIR_item_t m_gc_new_func = NULL;
MIR_item_t m_gc_update_proto = NULL;
MIR_item_t m_gc_update_func = NULL;
MIR_item_t m_gc_update_ref_proto = NULL;
MIR_item_t m_gc_update_ref_func = NULL;
MIR_item_t m_managed_memcpy_proto = NULL;
MIR_item_t m_managed_memcpy_func = NULL;
MIR_item_t m_managed_ref_memcpy_proto = NULL;
MIR_item_t m_managed_ref_memcpy_func = NULL;
MIR_item_t m_memcpy_proto = NULL;
MIR_item_t m_memcpy_func = NULL;
MIR_item_t m_memmove_proto = NULL;
MIR_item_t m_memmove_func = NULL;
MIR_item_t m_memset_proto = NULL;
MIR_item_t m_memset_func = NULL;
MIR_item_t m_on_throw_proto = NULL;
MIR_item_t m_on_throw_func = NULL;
MIR_item_t m_on_rethrow_proto = NULL;
MIR_item_t m_on_rethrow_func = NULL;
MIR_item_t m_get_thread_local_ptr_proto = NULL;
MIR_item_t m_get_thread_local_ptr_func = NULL;
MIR_item_t m_delegate_ctor_func = NULL;
MIR_item_t m_unsafe_as_func = NULL;

MIR_item_t m_debug_trace_proto = NULL;
MIR_item_t m_debug_trace_func = NULL;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// methods that are used as intrinsics
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define INVALID_IMPORT ((void*)-1)

System_Reflection_MethodInfo m_Unsafe_SizeOf;
System_Reflection_MethodInfo m_MemoryMarshal_GetArrayDataReference;
System_Reflection_MethodInfo m_RuntimeHelpers_IsReferenceOrContainsReferences;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// thread statics
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Arrays of locals for this thread, no need for a mutex
 * to protect because this can only be accessed from a single
 */
static THREAD_LOCAL void** m_thread_locals = NULL;

/**
 * Used to generate thread local ids, need a protection
 * since this can be read from any thread and can also
 * happen at the same time as a thread local addition
 */
static System_Type* m_thread_local_types = 0;

/**
 * We will use a rwmutex to allow multiple
 * threads to read form the local types at
 * the same time but only the jit can write
 * to it
 */
static rwmutex_t m_thread_local_mutex = { 0 };

/**
 * Adds a new thread local
 */
static int add_thread_local(System_Type type) {
    rwmutex_write_lock(&m_thread_local_mutex);
    int index = arrlen(m_thread_local_types);
    arrpush(m_thread_local_types, type);
    rwmutex_write_unlock(&m_thread_local_mutex);
    return index;
}

/**
 * Get a pointer to a thread local by its ID, initializes
 * it if not initialized yet
 */
static void* get_thread_local_ptr(int local_index) {
    // this local was not accessed ever, allocate it
    int cur_len = arrlen(m_thread_locals);
    if (cur_len <= local_index) {
        arrsetlen(m_thread_locals, local_index + 1);
        memset(m_thread_locals + cur_len, 0, sizeof(void*) * (local_index - cur_len + 1));
    }

    void* ptr = m_thread_locals[local_index];

    if (ptr == NULL) {
        int first = 0;

        // get the stack size
        rwmutex_read_lock(&m_thread_local_mutex);
        System_Type type = m_thread_local_types[local_index];
        size_t size = type->StackSize;
        int* managedOffsets = NULL;
        int managedOffsetsCount = 0;
        if (type->IsValueType) {
            managedOffsets = type->ManagedPointersOffsets;
            managedOffsetsCount = arrlen(managedOffsets);
        } else {
            managedOffsets = &first;
            managedOffsetsCount = 1;
        }
        rwmutex_read_unlock(&m_thread_local_mutex);

        // allocate the entry in memory to hold this local,
        // and add it as a gc root
        ptr = malloc(size);
        memset(ptr, 0, size);

        // add managed offsets to the gc
        for (int i = 0; i < managedOffsetsCount; i++) {
            gc_add_root(ptr + managedOffsets[i]);
        }

        m_thread_locals[local_index] = ptr;
    }

    // return the pointer to it
    return ptr;
}

void jit_free_thread_locals() {
    // free the entries
    for (int local_index = 0; local_index < arrlen(m_thread_locals); local_index++) {
        void* ptr = m_thread_locals[local_index];
        int first = 0;

        if (ptr == NULL)
            continue;

        // get the managed offsets to remove them
        rwmutex_read_lock(&m_thread_local_mutex);
        System_Type type = m_thread_local_types[local_index];
        int* managedOffsets = NULL;
        int managedOffsetsCount = 0;
        if (type->IsValueType) {
            managedOffsets = type->ManagedPointersOffsets;
            managedOffsetsCount = arrlen(managedOffsets);
        } else {
            managedOffsets = &first;
            managedOffsetsCount = 1;
        }
        rwmutex_read_unlock(&m_thread_local_mutex);

        // remove it from the gc
        for (int i = 0; i < managedOffsetsCount; i++) {
            gc_remove_root(ptr + managedOffsets[i]);
        }

        // free it
        free(ptr);
    }
    arrfree(m_thread_locals);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// runtime functions
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// The flatten attribute causes mem{cpy,set} to be inlined, if possible
// The wrappers are needed here because the two functions are just macros over __builtin_X

__attribute__((flatten))
static void memset_wrapper(void* dest, int c, size_t count) {
    memset(dest, c, count);
}

__attribute__((flatten))
static void memcpy_wrapper(void* dest, void* src, size_t count) {
    memcpy(dest, src, count);
}

__attribute__((flatten))
static void memmove_wrapper(void* dest, void* src, size_t count) {
    memmove(dest, src, count);
}


// TODO: generate this instead?
static bool dynamic_cast_obj_to_interface(Interface* dest, System_Object source, System_Type targetInterface) {
    // should only be called after the type checking
    TinyDotNet_Reflection_InterfaceImpl interface = type_get_interface_impl(OBJECT_TYPE(source), targetInterface);
    if (interface == NULL) {
        dest->VTable = 0;
        dest->This = 0;
        return false;
    }

    // set the interface fields
    dest->VTable = &((void**)(uintptr_t)source->vtable)[interface->VTableOffset];
    dest->This = source;

    return true;
}

static void managed_ref_memcpy(void* base, System_Type struct_type, void* from) {
    //uint8_t* this_base = (uint8_t*)this;
    System_Object this = heap_find_fast(base);
    if (this != NULL) {
        managed_memcpy(this, struct_type, (uintptr_t)base - (uintptr_t)this, from);
    } else {
        // not on the heap, do simple memcpy
        memcpy(base, from, struct_type->StackSize);
    }
}

static void on_throw(System_Exception exception, System_Reflection_MethodInfo methodInfo, int il_offset) {
#ifdef THROW_TRACE
    strbuilder_t builder = strbuilder_new();
    type_print_full_name(methodInfo->DeclaringType, &builder);
    ERROR("Exception `%U` (of type %U.%U) thrown at %s::%U -- IL_%04x",
          exception->Message, OBJECT_TYPE(exception)->Namespace, OBJECT_TYPE(exception)->Name,
          strbuilder_get(&builder), methodInfo->Name, il_offset);
    strbuilder_free(&builder);
#endif
}

static void on_rethrow(System_Reflection_MethodInfo methodInfo, int il_offset) {
#ifdef THROW_TRACE
    strbuilder_t builder = strbuilder_new();
    type_print_full_name(methodInfo->DeclaringType, &builder);
    ERROR("\trethrown at %s::%U -- IL_%04x", strbuilder_get(&builder), methodInfo->Name, il_offset);
    strbuilder_free(&builder);
#endif
}

static void debug_trace(uintptr_t ptr) {
    TRACE("%08lx", ptr);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// init the jit
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * The global context of the MIR jit
 */
static MIR_context_t m_mir_context;

static void jit_generate_System_Array_GetDataPtr() {
    const char* fname = "[Corelib-v1]System.Void* [Corelib-v1]System.Array::GetDataPtr()";
    MIR_type_t res[] = {
        MIR_T_P,
        MIR_T_P
    };
    MIR_item_t func = MIR_new_func(m_mir_context, fname, 2, res, 1, MIR_T_P, "this");
    MIR_reg_t this = MIR_reg(m_mir_context, "this", func->u.func);
    MIR_append_insn(m_mir_context, func,
                    MIR_new_insn(m_mir_context, MIR_ADD,
                                 MIR_new_reg_op(m_mir_context, this),
                                 MIR_new_reg_op(m_mir_context, this),
                                 MIR_new_int_op(m_mir_context, sizeof(struct System_Array))));
    MIR_append_insn(m_mir_context, func,
                    MIR_new_ret_insn(m_mir_context, 2,
                                     MIR_new_int_op(m_mir_context, 0),
                                     MIR_new_reg_op(m_mir_context, this)));
    MIR_finish_func(m_mir_context);
    MIR_new_export(m_mir_context, fname);
}

static void jit_generate_System_Type_GetTypeFromHandle() {
    const char* fname = "[Corelib-v1]System.Type [Corelib-v1]System.Type::GetTypeFromHandle([Corelib-v1]System.RuntimeTypeHandle)";
    MIR_var_t args[] = {
        {
            .name = "handle",
            .type = MIR_T_BLK,
            .size = sizeof(System_RuntimeTypeHandle)
        }
    };
    MIR_type_t res[] = {
        MIR_T_P,
        MIR_T_P
    };
    MIR_item_t func = MIR_new_func_arr(m_mir_context, fname, 2, res, 1, args);
    MIR_reg_t handle = MIR_reg(m_mir_context, "handle", func->u.func);
    MIR_append_insn(m_mir_context, func,
                    MIR_new_ret_insn(m_mir_context, 2,
                                     MIR_new_int_op(m_mir_context, 0),
                                     MIR_new_mem_op(m_mir_context, MIR_T_P, 0, handle, 0, 1)));
    MIR_finish_func(m_mir_context);
    MIR_new_export(m_mir_context, fname);
}

/**
 * All delegates have the exact same ctor, so we are going to just use
 * generate the ctor once and use it every time that we need to init
 * a ctor of a delegate
 *
 * this works for both multicast and non-multicast delegates
 */
static void jit_generate_delegate_ctor() {
    const char* fname = "delegate_ctor";
    MIR_type_t res = MIR_T_P;
    MIR_item_t func = MIR_new_func(m_mir_context, fname, 1, &res, 3, MIR_T_P, "this", MIR_T_P, "target", MIR_T_P, "method");
    MIR_reg_t this_reg = MIR_reg(m_mir_context, "this", func->u.func);
    MIR_reg_t target_reg = MIR_reg(m_mir_context, "target", func->u.func);
    MIR_reg_t method_reg = MIR_reg(m_mir_context, "method", func->u.func);

    MIR_op_t target_op = MIR_new_mem_op(m_mir_context, MIR_T_P, offsetof(struct System_Delegate, Target), this_reg, 0, 1);
    MIR_op_t fnptr_op = MIR_new_mem_op(m_mir_context, MIR_T_P, offsetof(struct System_Delegate, Fnptr), this_reg, 0, 1);

    MIR_append_insn(m_mir_context, func,
                    MIR_new_insn(m_mir_context, MIR_MOV,
                                 target_op,
                                 MIR_new_reg_op(m_mir_context, target_reg)));

    MIR_append_insn(m_mir_context, func,
                    MIR_new_insn(m_mir_context, MIR_MOV,
                                 fnptr_op,
                                 MIR_new_reg_op(m_mir_context, method_reg)));

    MIR_append_insn(m_mir_context, func,
                    MIR_new_ret_insn(m_mir_context, 1,
                                     MIR_new_int_op(m_mir_context, 0)));

    MIR_finish_func(m_mir_context);
    MIR_new_export(m_mir_context, fname);
    m_delegate_ctor_func = func;
}

static void jit_generate_unsafe_as() {
    const char* fname = "unsafe_as";
    MIR_type_t res[] = {
        MIR_T_P,
        MIR_T_P,
    };
    MIR_item_t func = MIR_new_func(m_mir_context, fname, 2, res, 1, MIR_T_P, "arg");
    MIR_reg_t arg = MIR_reg(m_mir_context, "arg", func->u.func);
    MIR_append_insn(m_mir_context, func,
                    MIR_new_ret_insn(m_mir_context, 2,
                                     MIR_new_int_op(m_mir_context, 0),
                                     MIR_new_reg_op(m_mir_context, arg)));
    MIR_finish_func(m_mir_context);
    MIR_new_export(m_mir_context, fname);
    m_unsafe_as_func = func;
}

static void jit_generate_memmove() {
    const char* fname = "[Corelib-v1]System.Buffer::Memmove([Corelib-v1]System.Byte&,[Corelib-v1]System.Byte&,nuint)";
    MIR_type_t res = MIR_T_P;
    MIR_item_t func = MIR_new_func(m_mir_context, fname, 1, &res, 3, MIR_T_P, "dst", MIR_T_P, "src", MIR_T_U64, "len");
    MIR_reg_t dst_reg = MIR_reg(m_mir_context, "dst", func->u.func);
    MIR_reg_t src_reg = MIR_reg(m_mir_context, "src", func->u.func);
    MIR_reg_t len_reg = MIR_reg(m_mir_context, "len", func->u.func);
    MIR_append_insn(m_mir_context, func,
                    MIR_new_call_insn(m_mir_context, 5,
                                      MIR_new_ref_op(m_mir_context, m_memmove_proto),
                                      MIR_new_ref_op(m_mir_context, m_memmove_func),
                                      MIR_new_reg_op(m_mir_context, dst_reg),
                                      MIR_new_reg_op(m_mir_context, src_reg),
                                      MIR_new_reg_op(m_mir_context, len_reg)));
    MIR_append_insn(m_mir_context, func,
                    MIR_new_ret_insn(m_mir_context, 1,
                                      MIR_new_int_op(m_mir_context, 0)));
    MIR_finish_func(m_mir_context);
    MIR_new_export(m_mir_context, fname);
}

static void jit_generate_zeromem() {
    const char* fname = "[Corelib-v1]System.Buffer::_ZeroMemory([Corelib-v1]System.Byte&,nuint)";
    MIR_type_t res = MIR_T_P;
    MIR_item_t func = MIR_new_func(m_mir_context, fname, 1, &res, 2, MIR_T_P, "b", MIR_T_U64, "len");
    MIR_reg_t b_reg = MIR_reg(m_mir_context, "b", func->u.func);
    MIR_reg_t len_reg = MIR_reg(m_mir_context, "len", func->u.func);
    MIR_append_insn(m_mir_context, func,
                    MIR_new_call_insn(m_mir_context, 5,
                                      MIR_new_ref_op(m_mir_context, m_memset_proto),
                                      MIR_new_ref_op(m_mir_context, m_memset_func),
                                      MIR_new_reg_op(m_mir_context, b_reg),
                                      MIR_new_int_op(m_mir_context, 0),
                                      MIR_new_reg_op(m_mir_context, len_reg)));
    MIR_append_insn(m_mir_context, func,
                    MIR_new_ret_insn(m_mir_context, 1,
                                     MIR_new_int_op(m_mir_context, 0)));
    MIR_finish_func(m_mir_context);
    MIR_new_export(m_mir_context, fname);
}

err_t init_jit() {
    err_t err = NO_ERROR;

    // we want corelib to have access to extern
    jit_add_extern_whitelist("Corelib.dll");

    m_mir_context = MIR_init();

    //
    // Initialize all internal functions
    //
   MIR_module_t internal_module = MIR_new_module(m_mir_context, "tinydotnet");

    MIR_type_t res_type = MIR_T_P;
    m_gc_new_proto = MIR_new_proto(m_mir_context, "gc_new$proto", 1, &res_type, 2, MIR_T_P, "type", MIR_T_U64, "size");
    m_gc_new_func = MIR_new_import(m_mir_context, "gc_new");

    m_gc_update_proto = MIR_new_proto(m_mir_context, "gc_update$proto", 0, NULL, 3, MIR_T_P, "o", MIR_T_U64, "idx", MIR_T_P, "new");
    m_gc_update_func = MIR_new_import(m_mir_context, "gc_update");

    m_gc_update_ref_proto = MIR_new_proto(m_mir_context, "gc_update_ref$proto", 0, NULL, 2, MIR_T_P, "o", MIR_T_P, "new");
    m_gc_update_ref_func = MIR_new_import(m_mir_context, "gc_update_ref");

    m_managed_memcpy_proto = MIR_new_proto(m_mir_context, "managed_memcpy$proto", 0, NULL, 4, MIR_T_P, "this", MIR_T_P, "struct_type", MIR_T_I64, "offset", MIR_T_P, "from");
    m_managed_memcpy_func = MIR_new_import(m_mir_context, "managed_memcpy");

    m_managed_ref_memcpy_proto = MIR_new_proto(m_mir_context, "managed_ref_memcpy$proto", 0, NULL, 3, MIR_T_P, "this", MIR_T_P, "struct_type", MIR_T_P, "from");
    m_managed_ref_memcpy_func = MIR_new_import(m_mir_context, "managed_ref_memcpy");

    m_memcpy_proto = MIR_new_proto(m_mir_context, "memcpy$proto", 0, NULL, 3, MIR_T_P, "dest", MIR_T_P, "src", MIR_T_U64, "count");
    m_memcpy_func = MIR_new_import(m_mir_context, "memcpy");

    m_memmove_proto = MIR_new_proto(m_mir_context, "memmove$proto", 0, NULL, 3, MIR_T_P, "dest", MIR_T_P, "src", MIR_T_U64, "count");
    m_memmove_func = MIR_new_import(m_mir_context, "memmove");

    m_memset_proto = MIR_new_proto(m_mir_context, "memset$proto", 0, NULL, 3, MIR_T_P, "dest", MIR_T_I32, "c", MIR_T_U64, "count");
    m_memset_func = MIR_new_import(m_mir_context, "memset");

    m_on_throw_proto = MIR_new_proto(m_mir_context, "on_throw$proto", 0, NULL, 3, MIR_T_P, "exception", MIR_T_P, "method_info", MIR_T_I32, "il_offset");
    m_on_throw_func = MIR_new_import(m_mir_context, "on_throw");

    m_on_rethrow_proto = MIR_new_proto(m_mir_context, "on_rethrow$proto", 0, NULL, 2, MIR_T_P, "method_info", MIR_T_I32, "il_offset");
    m_on_rethrow_func = MIR_new_import(m_mir_context, "on_rethrow");

    m_debug_trace_proto = MIR_new_proto(m_mir_context, "debug_trace$proto", 0, NULL, 1, MIR_T_I64, "a");
    m_debug_trace_func = MIR_new_import(m_mir_context, "debug_trace");

    m_get_thread_local_ptr_proto = MIR_new_proto(m_mir_context, "get_thread_local_ptr$proto", 1, &res_type, 1, MIR_T_I32, "local_index");
    m_get_thread_local_ptr_func = MIR_new_import(m_mir_context, "get_thread_local_ptr");

    res_type = MIR_T_I8;

    m_dynamic_cast_obj_to_interface_proto = MIR_new_proto(m_mir_context, "dynamic_cast_obj_to_interface$proto", 1, &res_type, 3, MIR_T_P, "dest", MIR_T_P, "source", MIR_T_P, "targetInterface");
    m_dynamic_cast_obj_to_interface_func = MIR_new_import(m_mir_context, "dynamic_cast_obj_to_interface");

    m_is_instance_proto = MIR_new_proto(m_mir_context, "isinstance$proto", 1, &res_type, 2, MIR_T_P, "object", MIR_T_P, "type");
    m_is_instance_func = MIR_new_import(m_mir_context, "isinstance");

    // generate some builtin methods that we can't properly create in CIL because we don't allow
    // any unsafe code, and it is not worth having them as native functions
    jit_generate_System_Array_GetDataPtr();
    jit_generate_System_Type_GetTypeFromHandle();
    jit_generate_delegate_ctor();
    jit_generate_unsafe_as();
    jit_generate_memmove();
    jit_generate_zeromem();

    MIR_finish_module(m_mir_context);

    //
    // load all internal functions
    //

    // load the module
    MIR_load_module(m_mir_context, internal_module);

    // load JIT required functions
    MIR_load_external(m_mir_context, "dynamic_cast_obj_to_interface", dynamic_cast_obj_to_interface);
    MIR_load_external(m_mir_context, "isinstance", isinstance);
    MIR_load_external(m_mir_context, "gc_new", gc_new);
    MIR_load_external(m_mir_context, "gc_update", gc_update);
    MIR_load_external(m_mir_context, "gc_update_ref", gc_update_ref);
    MIR_load_external(m_mir_context, "get_array_type", get_array_type);
    MIR_load_external(m_mir_context, "memcpy", memcpy_wrapper);
    MIR_load_external(m_mir_context, "memmove", memmove_wrapper);
    MIR_load_external(m_mir_context, "memset", memset_wrapper);
    MIR_load_external(m_mir_context, "managed_memcpy", managed_memcpy);
    MIR_load_external(m_mir_context, "managed_ref_memcpy", managed_ref_memcpy);
    MIR_load_external(m_mir_context, "on_throw", on_throw);
    MIR_load_external(m_mir_context, "on_rethrow", on_rethrow);
    MIR_load_external(m_mir_context, "get_thread_local_ptr", get_thread_local_ptr);
    MIR_load_external(m_mir_context, "debug_trace", debug_trace);

    // load internal functions
    for (int i = 0; i < g_internal_calls_count; i++) {
        MIR_load_external(m_mir_context, g_internal_calls[i].target, g_internal_calls[i].impl);
    }

    // init the code gen
    // FIXME: BUG IN MIR?
    // lazy jit on multiple threads hits some race condition when two threads are jitting at once
    // even on hosted TDN
    // TODO: run valgrind helgrind+drd and fix everythign

    int count = 1;
    MIR_gen_init(m_mir_context, count);
    for (int i = 0; i < count; i++) {
        MIR_gen_set_optimize_level(m_mir_context, i, 4);
    }

#if 0
    MIR_gen_set_debug_file(m_mir_context, 0, stdout);
    MIR_gen_set_debug_level(m_mir_context, 0, 0);
#endif

cleanup:
    return err;
}

static jit_generic_extern_hook_t** m_generic_extern_hooks = NULL;

static const char** m_extern_whitelist = NULL;

void jit_add_generic_extern_hook(jit_generic_extern_hook_t* hook) {
    arrpush(m_generic_extern_hooks, hook);
}

void jit_add_extern_whitelist(const char* assembly) {
    arrpush(m_extern_whitelist, assembly);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// MIR helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Get the MIR type for the given dotnet type, meant to be used for function signatures
 * to create a proper ABI that could be used for other stuff as well
 */
MIR_type_t jit_get_mir_type(System_Type type) {
    type = type_get_underlying_type(type);
    if (type == tSystem_Byte) {
        return MIR_T_U8;
    } else if (type == tSystem_SByte) {
        return MIR_T_I8;
    } else if (type == tSystem_UInt16) {
        return MIR_T_U16;
    } else if (type == tSystem_Int16) {
        return MIR_T_I16;
    } else if (type == tSystem_UInt32) {
        return MIR_T_U32;
    } else if (type == tSystem_Int32) {
        return MIR_T_I32;
    } else if (type == tSystem_UInt64) {
        return MIR_T_U64;
    } else if (type == tSystem_Int64) {
        return MIR_T_I64;
    } else if (type == tSystem_UIntPtr) {
        return MIR_T_U64;
    } else if (type == tSystem_IntPtr) {
        return MIR_T_I64;
    } else if (type == tSystem_Char) {
        return MIR_T_U16;
    } else if (type == tSystem_Boolean) {
        return MIR_T_I8;
    } else if (type == tSystem_Single) {
        return MIR_T_F;
    } else if (type == tSystem_Double) {
        return MIR_T_D;
    } else if (type->IsByRef || type->IsPointer) {
        return MIR_T_P;
    } else if (type->IsValueType || type_is_interface(type)) {
        return MIR_T_BLK;
    } else {
        ASSERT(type == NULL || type_is_object_ref(type));
        return MIR_T_P;
    }
}

/**
 * Create a new register for the given type, can be used for temporaries as
 * well as for stack slots
 */
static MIR_type_t get_mir_stack_type(System_Type type) {
    MIR_type_t mir_type = MIR_T_UNDEF;
    if (type == NULL) {
        mir_type = MIR_T_I64;
    } else {
        switch (type_get_stack_type(type)) {
            case STACK_TYPE_INT64:
            case STACK_TYPE_INT32:
            case STACK_TYPE_INTPTR:
            case STACK_TYPE_O:
            case STACK_TYPE_REF: {
                if (type_is_interface(type)) {
                    mir_type = MIR_T_BLK;
                } else {
                    mir_type = MIR_T_I64;
                }
            } break;

            case STACK_TYPE_FLOAT: {
                if (type == tSystem_Single) {
                    mir_type = MIR_T_F;
                } else {
                    ASSERT(type == tSystem_Double);
                    mir_type = MIR_T_D;
                }
            } break;

            case STACK_TYPE_VALUE_TYPE: {
                mir_type = MIR_T_BLK;
            } break;
        }
    }

    return mir_type;
}

#ifdef JIT_TRACE_MIR
    #define MIR_append_insn(...) MIR_append_insn_output(__VA_ARGS__)
#endif

MIR_reg_t jit_push_new_reg(jit_method_context_t* ctx, System_Type type, bool temp) {
    char prefix = temp ? 't' : 's';

    // get the mir type and create the proper reg name
    MIR_type_t mir_type = get_mir_stack_type(type);
    MIR_type_t reg_type = mir_type;
    stack_keeping_t* stack = NULL;
    char type_prefix = '\0';
    switch (mir_type) {
        case MIR_T_BLK: reg_type = MIR_T_I64;
        case MIR_T_I64: stack = temp ? &ctx->itmp : &ctx->ireg; type_prefix = 'i'; break;
        case MIR_T_F: stack = temp ? &ctx->ftmp : &ctx->freg; type_prefix = 'f'; break;
        case MIR_T_D: stack = temp ? &ctx->dtmp : &ctx->dreg; type_prefix = 'd'; break;
        default: ASSERT(FALSE);
    }

    MIR_reg_t reg = 0;
    if (stack->depth < arrlen(stack->regs)) {
        // there is an entry inside the regs array
        reg = stack->regs[stack->depth];
    } else {
        // we need a new register

        // setup the name
        char name[64] = { 0 };
        snprintf(name, sizeof(name), "%c%c%d", prefix, type_prefix, stack->depth);

        // create it
        reg = MIR_new_func_reg(mir_ctx, mir_func->u.func, reg_type, name);
        arrpush(stack->regs, reg);
    }

    // if its a value type we need to allocate place on the stack for it, we are gonna save it in another
    // reg that is never going to change and then move it to place
    if (mir_type == MIR_T_BLK) {
        char name[64] = { 0 };
        snprintf(name, sizeof(name), "%cv%d", prefix, ctx->value_type_name_gen++);
        MIR_reg_t stack_value = MIR_new_func_reg(mir_ctx, mir_func->u.func, MIR_T_I64, name);

        // at the start allocate it and store it in the stack value reg
        MIR_prepend_insn(mir_ctx, mir_func,
                         MIR_new_insn(mir_ctx, MIR_ALLOCA,
                                      MIR_new_reg_op(mir_ctx, stack_value),
                                      MIR_new_int_op(mir_ctx, type->StackSize)));

        // then in the current place in the code move it to the stack position
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_MOV,
                                     MIR_new_reg_op(mir_ctx, reg),
                                     MIR_new_reg_op(mir_ctx, stack_value)));
    }

    // increment the depth
    stack->depth++;

    return reg;
}

#undef MIR_prepend_insn

/**
 * Create a new temp register
 */
MIR_reg_t jit_new_temp_reg(jit_method_context_t* ctx, System_Type type) {
    return jit_push_new_reg(ctx, type, true);
}

/**
 * Push a new item on the stack, returning the stack slot for it as a register
 *
 * This verifies that the stack depth is not too big
 */
err_t jit_stack_push(jit_method_context_t* ctx, System_Type type, MIR_reg_t* out_reg) {
    err_t err = NO_ERROR;

    ASSERT(type == NULL || type->IsSetupFinished);

    if (type != NULL) {
        // some types can get here unfilled because they were
        // taken from other generation stuff, so make sure
        // that they are filled right now
        CHECK_AND_RETHROW(filler_fill_type(type));
    }

    // Make sure we don't exceed the stack depth
    CHECK(arrlen(ctx->stack.entries) < ctx->method->MethodBody->MaxStackSize);

    // create the register for the output
    *out_reg = jit_push_new_reg(ctx, type, false);

    // append to the stack
    arrpush(ctx->stack.entries, (stack_entry_t){ .type = type });

cleanup:
    return err;
}

/**
 * Pop an item from the stack, returning its type and register location, will fail
 * if there are not enough items on the stack
 */
err_t jit_stack_pop(jit_method_context_t* ctx, System_Type* out_type, MIR_reg_t* out_reg, stack_entry_t* ste) {
    err_t err = NO_ERROR;

    // pop the entry
    CHECK(arrlen(ctx->stack.entries) > 0);
    stack_entry_t entry = arrpop(ctx->stack.entries);
    System_Type type = entry.type;
    if (out_type != NULL) *out_type = type;
    if (ste != NULL) *ste = entry;

    // get the reg stack
    MIR_insn_code_t mov = MIR_MOV;
    stack_keeping_t* stack = NULL;
    MIR_type_t stack_type = get_mir_stack_type(type);
    switch (stack_type) {
        case MIR_T_BLK: ;
        case MIR_T_I64: stack = &ctx->ireg; break;
        case MIR_T_F: stack = &ctx->freg; mov = MIR_FMOV; break;
        case MIR_T_D: stack = &ctx->dreg; mov = MIR_DMOV; break;
        default: ASSERT(FALSE);
    }

    // pop the reg essentially
    MIR_reg_t reg = stack->regs[--stack->depth];

    if (out_reg != NULL) {
        // the caller wants to use this, so we are going to allocate
        // a new reg and put the content in it, this allows the caller
        // to assume the push after this pop is not the same register as
        // it gets right now

        if (stack_type == MIR_T_BLK) {
            // don't allocate the whole memory range, just mvoe the pointer
            type = tSystem_IntPtr;
        }
        MIR_reg_t real_reg = jit_new_temp_reg(ctx, type);
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, mov,
                                     MIR_new_reg_op(mir_ctx, real_reg),
                                     MIR_new_reg_op(mir_ctx, reg)));

        *out_reg = real_reg;
    }

cleanup:
    return err;
}

/**
 * Take a snapshot of the stack, used for verification
 */
static stack_t stack_snapshot(jit_method_context_t* ctx) {
    stack_t snapshot = { 0 };
    arrsetlen(snapshot.entries, arrlen(ctx->stack.entries));
    memcpy(snapshot.entries, ctx->stack.entries, arrlen(ctx->stack.entries) * sizeof(ctx->stack.entries[0]));
    return snapshot;
}

/**
 * Create a copy of the current stack, put it in the output stack
 */
static void stack_copy(jit_method_context_t* ctx, stack_t* stack) {
    arrsetlen(ctx->stack.entries, arrlen(stack->entries));
    memcpy(ctx->stack.entries, stack->entries, arrlen(stack->entries) * sizeof(stack->entries[0]));
}

/**
 * Merge the two stacks, either checking if the merge is possible or merging
 * it in place depending on allow_merge
 */
static err_t stack_merge(jit_method_context_t* ctx, stack_t* stack, bool allow_change) {
    err_t err = NO_ERROR;

    // we must have the same number of slots
    CHECK(arrlen(stack->entries) == arrlen(ctx->stack.entries));

    // now merge it
    for (int i = 0; i < arrlen(stack->entries); i++) {
        System_Type T = ctx->stack.entries[i].type;
        System_Type S = stack->entries[i].type;

        // figure the new value that should be in here
        System_Type U = NULL;
        if (type_is_verifier_assignable_to(T, S)) {
            U = S;
        } else if (type_is_verifier_assignable_to(S, T)) {
            U = T;
        } else if (type_get_stack_type(T) == STACK_TYPE_O && type_get_stack_type(S) == STACK_TYPE_O) {
            // both are ref types, get the closest common subtype, do that by finding two types
            // in the inheritance tree that are the same, worst case we will reach to object
            // in the end which is the smallest common subtype

            while (T != NULL) {
                System_Type SBase = S;
                while (SBase != NULL) {
                    if (T == SBase) {
                        U = T;
                        break;
                    }
                    SBase = SBase->BaseType;
                }

                if (U != NULL) {
                    break;
                }

                T = T->BaseType;
            }

            CHECK(U != NULL);
        } else {
            CHECK_FAIL();
        }

        if (allow_change) {
            // for forward jumps we allow to merge properly
            stack->entries[i].type = U;
        } else {
            // for backwards jumps we are going to check the stack
            // does not change after merging
            CHECK(stack->entries[i].type == U);
        }
    }

cleanup:
    return err;
}

#undef MIR_append_insn

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Codegen helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

MIR_insn_code_t jit_number_cast_inscode(System_Type srctype, System_Type desttype) {
    MIR_insn_code_t code = MIR_MOV;
    if (srctype == tSystem_Single) {
        if (desttype == tSystem_Double) {
            code = MIR_F2D;
        } else if (desttype == tSystem_Single) {
            code = MIR_FMOV;
        }
    } else if (srctype == tSystem_Double) {
        if (desttype == tSystem_Single) {
            code = MIR_D2F;
        } else if (desttype == tSystem_Double) {
            code = MIR_DMOV;
        }
    }
    return code;
}

MIR_insn_code_t jit_mov_insn_code(System_Type type) {
    MIR_insn_code_t code = MIR_MOV;
    if (type == tSystem_Single) {
        code = MIR_FMOV;
    } else if (type == tSystem_Double) {
        code = MIR_DMOV;
    }
    return code;
}

#ifdef JIT_TRACE_MIR
    #define MIR_append_insn(...) MIR_append_insn_output(__VA_ARGS__)
#endif

void jit_emit_memcpy(jit_method_context_t* ctx, MIR_reg_t dest, MIR_reg_t src, size_t count) {
    if (count <= 64) {
        int off = 0;
        while (count >= 8) {
            //
            // TODO: fine tune it to emit something like
            //       r0 = load64()
            //       r1 = load64()
            //       r2 = load64()
            //       ...
            //       store64(r2)
            //       store64(r1)
            //       store64(r0)
            //
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_mem_op(mir_ctx, MIR_T_I64, off, dest, 0, 1),
                                         MIR_new_mem_op(mir_ctx, MIR_T_I64, off, src, 0, 1)));
            off += 8;
            count -= 8;
        }
        if (count >= 4) {
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_mem_op(mir_ctx, MIR_T_I32, off, dest, 0, 1),
                                         MIR_new_mem_op(mir_ctx, MIR_T_I32, off, src, 0, 1)));
            off += 4;
            count -= 4;
        }
        if (count >= 2) {
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_mem_op(mir_ctx, MIR_T_I16, off, dest, 0, 1),
                                         MIR_new_mem_op(mir_ctx, MIR_T_I16, off, src, 0, 1)));
            off += 2;
            count -= 2;
        }
        if (count >= 1) {
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_mem_op(mir_ctx, MIR_T_I8, off, dest, 0, 1),
                                         MIR_new_mem_op(mir_ctx, MIR_T_I8, off, src, 0, 1)));
            off += 1;
            count -= 1;
        }
    } else {
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_call_insn(mir_ctx, 5,
                                          MIR_new_ref_op(mir_ctx, m_memcpy_proto),
                                          MIR_new_ref_op(mir_ctx, m_memcpy_func),
                                          MIR_new_reg_op(mir_ctx, dest),
                                          MIR_new_reg_op(mir_ctx, src),
                                          MIR_new_int_op(mir_ctx, count)));
    }
}

void jit_emit_zerofill(jit_method_context_t* ctx, MIR_reg_t dest, size_t count) {
    if (count <= 64) {
        int off = 0;
        while (count >= 8) {
            //
            // TODO: fine tune it to emit something like
            //       r0 = load64()
            //       r1 = load64()
            //       r2 = load64()
            //       ...
            //       store64(r2)
            //       store64(r1)
            //       store64(r0)
            //
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_mem_op(mir_ctx, MIR_T_I64, off, dest, 0, 1),
                                         MIR_new_int_op(mir_ctx, 0)));
            off += 8;
            count -= 8;
        }
        if (count >= 4) {
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_mem_op(mir_ctx, MIR_T_I32, off, dest, 0, 1),
                                         MIR_new_int_op(mir_ctx, 0)));
            off += 4;
            count -= 4;
        }
        if (count >= 2) {
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_mem_op(mir_ctx, MIR_T_I16, off, dest, 0, 1),
                                         MIR_new_int_op(mir_ctx, 0)));
            off += 2;
            count -= 2;
        }
        if (count >= 1) {
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_mem_op(mir_ctx, MIR_T_I8, off, dest, 0, 1),
                                         MIR_new_int_op(mir_ctx, 0)));
            off += 1;
            count -= 1;
        }
    } else {
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_call_insn(mir_ctx, 5,
                                          MIR_new_ref_op(mir_ctx, m_memset_proto),
                                          MIR_new_ref_op(mir_ctx, m_memset_func),
                                          MIR_new_reg_op(mir_ctx, dest),
                                          MIR_new_int_op(mir_ctx, 0),
                                          MIR_new_uint_op(mir_ctx, count)));
    }
}

#undef MIR_append_insn

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit the delegate wrappers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Generated code is essentially
 *
 *   var return_value;
 *   do {
 *        if (this->Target != NULL) {
 *            return_value = this->Fnptr(this->Target, ...);
 *        } else {
 *            return_value = this->Fnptr(...);
 *        }
 *        this = this->Next;
 *   } (this != NULL);
 *   return return_value;
 *
 */
static err_t jit_multicast_delegate_invoke(jit_context_t* ctx, System_Reflection_MethodInfo method, MIR_item_t func, MIR_item_t proto_static) {
    err_t err = NO_ERROR;

    // to access the delegate's parameters
    MIR_reg_t this_reg = MIR_reg(ctx->ctx, "this", func->u.func);
    MIR_op_t fnptr_op = MIR_new_mem_op(ctx->ctx, MIR_T_P, offsetof(struct System_MulticastDelegate, Fnptr), this_reg, 0, 1);
    MIR_op_t target_op = MIR_new_mem_op(ctx->ctx, MIR_T_P, offsetof(struct System_MulticastDelegate, Target), this_reg, 0, 1);
    MIR_op_t next_op = MIR_new_mem_op(ctx->ctx, MIR_T_P, offsetof(struct System_MulticastDelegate, Next), this_reg, 0, 1);

    System_Type ret_type = method->ReturnType;

    // count the amount of arguments, +1 if we have a this
    int arg_count = method->Parameters->Length;

    // prepare array of all the operands
    // 1st is the prototype
    // 2nd is the reference
    // 3rd is exception_reg return
    // 4rd is return type (optionally)
    // 5th is this type (optionally)
    // Rest are the arguments
    size_t other_args = 3;
    if (ret_type != NULL) other_args++;
    other_args++;
    MIR_op_t arg_ops[other_args + arg_count];

    // setup all the parameters
    int i;
    for (i = arg_count + other_args - 1; i >= other_args; i--) {
        System_Type signature_type = method->Parameters->Data[i - other_args]->ParameterType;

        // get the argument value
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "arg%d", i - other_args);
        MIR_reg_t arg_reg = MIR_reg(ctx->ctx, buffer, func->u.func);

        // check if we need to pass by value (a bit in-efficient but whatever)
        bool mem_op = false;
        if (type_is_interface(signature_type) || type_get_stack_type(signature_type) == STACK_TYPE_VALUE_TYPE) {
            mem_op = true;
        }

        // set the op, for anything passed by value we need to use MIR_T_BLK with the disp
        // being the size instead of the displacement
        if (mem_op) {
            arg_ops[i] = MIR_new_mem_op(ctx->ctx, MIR_T_BLK, signature_type->StackSize, arg_reg, 0, 1);
        } else {
            arg_ops[i] = MIR_new_reg_op(ctx->ctx, arg_reg);
        }
    }

    // the this comes from the target field of this object
    arg_ops[i] = target_op;

    // get the MIR signature
    arg_ops[0] = MIR_new_ref_op(ctx->ctx, method->MirProto);

    // indirect call
    arg_ops[1] = fnptr_op;

    // get it to the exception_reg register
    MIR_reg_t exception_reg = MIR_new_func_reg(ctx->ctx, func->u.func, MIR_T_I64, "exception_reg");
    arg_ops[2] = MIR_new_reg_op(ctx->ctx, exception_reg);

    // handle the return type
    size_t nres = 1;
    MIR_reg_t return_reg = 0;

    // emit the IR
    if (ret_type != NULL) {
        if (type_is_interface(ret_type) || type_get_stack_type(ret_type) == STACK_TYPE_VALUE_TYPE) {
            // returned as an implicit pointer
            return_reg = MIR_reg(ctx->ctx, "return_block", func->u.func);
        } else {
            // returned as a value
            nres++;
            if (ret_type == tSystem_Single) {
                return_reg = MIR_new_func_reg(ctx->ctx, func->u.func, MIR_T_F, "return");
            } else if (ret_type == tSystem_Double) {
                return_reg = MIR_new_func_reg(ctx->ctx, func->u.func, MIR_T_D, "return");
            } else {
                return_reg = MIR_new_func_reg(ctx->ctx, func->u.func, MIR_T_I64, "return");
            }
        }

        // this should just work, because if the value is a struct it is going to be allocated properly
        // in the stack push, and it is going to be passed by a pointer that we give, and everything will
        // just work out because of how we have the order of everything :)
        arg_ops[3] = MIR_new_reg_op(ctx->ctx, return_reg);
    }

    //
    // Start the call loop
    //

    MIR_insn_t label_do_next_call = MIR_new_label(ctx->ctx);
    MIR_append_insn(ctx->ctx, func, label_do_next_call);

    MIR_insn_t label_static_call = MIR_new_label(ctx->ctx);
    MIR_insn_t label_check_exception = MIR_new_label(ctx->ctx);

    // check if we have a this parameter
    MIR_append_insn(ctx->ctx, func,
                    MIR_new_insn(ctx->ctx, MIR_BF,
                                 MIR_new_label_op(ctx->ctx, label_static_call),
                                 target_op));

    /************************************/
    /*** Call with a `this` parameter ***/
    /************************************/

    MIR_append_insn(ctx->ctx, func,
                    MIR_new_insn_arr(ctx->ctx, MIR_CALL,
                                     other_args + arg_count,
                                     arg_ops));

    MIR_append_insn(ctx->ctx, func,
                    MIR_new_insn(ctx->ctx, MIR_JMP,
                                 MIR_new_label_op(ctx->ctx, label_check_exception)));

    /***************************************/
    /*** Call without a `this` parameter ***/
    /***************************************/

    MIR_append_insn(ctx->ctx, func, label_static_call);

    // need to move the arguments one over so we won't pass the this register
    other_args--;
    memmove(&arg_ops[i], &arg_ops[i + 1], arg_count * sizeof(MIR_op_t));

    // setup the static prototype signature
    arg_ops[0] = MIR_new_ref_op(ctx->ctx, proto_static);

    MIR_append_insn(ctx->ctx, func,
                    MIR_new_insn_arr(ctx->ctx, MIR_CALL,
                                     other_args + arg_count,
                                     arg_ops));

    //
    // Check for exception from the delegate's call
    //

    MIR_append_insn(ctx->ctx, func, label_check_exception);

    MIR_insn_t label_no_exception = MIR_new_label(ctx->ctx);

    MIR_append_insn(ctx->ctx, func,
                    MIR_new_insn(ctx->ctx, MIR_BF,
                                 MIR_new_label_op(ctx->ctx, label_no_exception),
                                 MIR_new_reg_op(ctx->ctx, exception_reg)));

    MIR_append_insn(ctx->ctx, func,
                    MIR_new_ret_insn(ctx->ctx, nres,
                                     MIR_new_reg_op(ctx->ctx, exception_reg),
                                     MIR_new_int_op(ctx->ctx, 0)));

    MIR_append_insn(ctx->ctx, func, label_no_exception);

    //
    // call the next delegate, do that by overriding the this register with the next
    // multicast delegate and then calling it
    //

    // read the next to the this register
    MIR_append_insn(ctx->ctx, func,
                    MIR_new_insn(ctx->ctx, MIR_MOV,
                                 MIR_new_reg_op(ctx->ctx, this_reg),
                                 next_op));

    // and loop if not null
    MIR_append_insn(ctx->ctx, func,
                    MIR_new_insn(ctx->ctx, MIR_BT,
                                 MIR_new_label_op(ctx->ctx, label_do_next_call),
                                 MIR_new_reg_op(ctx->ctx, this_reg)));

    //
    // return the last value
    //

    MIR_append_insn(ctx->ctx, func,
                    MIR_new_ret_insn(ctx->ctx, nres,
                                     MIR_new_int_op(ctx->ctx, 0),
                                     MIR_new_reg_op(ctx->ctx, return_reg)));

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Preparing code gen of a function
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

err_t jit_prepare_method(jit_context_t* ctx, System_Reflection_MethodInfo method) {
    err_t err = NO_ERROR;
    MIR_var_t* vars = NULL;
    strbuilder_t proto_static_name = { 0 };
    strbuilder_t proto_name = { 0 };
    strbuilder_t func_name = { 0 };

    if (method->MirProto != NULL) {
        goto cleanup;
    }

    // generate the method name
    proto_name = strbuilder_new();
    method_print_full_name(method, &proto_name);
    strbuilder_cstr(&proto_name, "$proto");

    // generate the function name
    func_name = strbuilder_new();
    method_print_full_name(method, &func_name);

    size_t nres = 1;
    MIR_type_t res_type[2] = {
        MIR_T_P, // exception
        MIR_T_UNDEF, // return value if any
    };

    // handle the return value
    if (method->ReturnType != NULL) {
        CHECK_AND_RETHROW(jit_prepare_static_type(ctx, method->ReturnType));

        res_type[1] = jit_get_mir_type(method->ReturnType);
        if (res_type[1] == MIR_T_BLK) {
            // value type return
            MIR_var_t var = {
                .name = "return_block",
                .type = MIR_T_P, // TODO: do we want to use rblk along size a normal return value
                .size = method->ReturnType->StackSize
            };
            arrpush(vars, var);
        } else {
            // we can use normal return
            nres = 2;
        }
    }

    int this_index = -1;
    if (!method_is_static(method)) {
        System_Type declaringType = method->DeclaringType;
        CHECK_AND_RETHROW(jit_prepare_static_type(ctx, method->DeclaringType));

        if (declaringType->IsValueType)
            declaringType = get_by_ref_type(declaringType);

        MIR_var_t var = {
            .name = "this",
            .type = jit_get_mir_type(declaringType),
        };
        if (var.type == MIR_T_BLK) {
            var.type = MIR_T_P;
        }
        this_index = arrlen(vars);
        arrpush(vars, var);
    }

    for (int i = 0; i < method->Parameters->Length; i++) {
        CHECK_AND_RETHROW(jit_prepare_static_type(ctx, method->Parameters->Data[i]->ParameterType));

        char name[64];
        snprintf(name, sizeof(name), "arg%d", i);
        MIR_var_t var = {
            .name = _MIR_uniq_string(ctx->ctx, name),
            .type = jit_get_mir_type(method->Parameters->Data[i]->ParameterType),
        };
        if (var.type == MIR_T_BLK) {
            var.size = method->Parameters->Data[i]->ParameterType->StackSize;
        }
        arrpush(vars, var);
    }

    // create the proto def
    method->MirProto = MIR_new_proto_arr(ctx->ctx, strbuilder_get(&proto_name), nres, res_type, arrlen(vars), vars);

    // no need to do anything else if the method is abstract
    if (method_is_abstract(method)) {
        goto cleanup;
    }

    // we don't have such code
    CHECK(!method_is_unmanaged(method));

    //
    // If runtime:
    //      If InternalCall:
    //          means this is a dynamically generated method that the host
    //          generates once upon initialization of the jit, so it has to
    //          be imported, used mostly by non-generic internal methods
    //      else:
    //          means this is a dynamically generated method, that we are
    //          going to generate right in here, used mostly for generic
    //          internal methods
    // If native:
    //      create an import, used to expose native functions to the runtime
    // If Il:
    //      must have a body
    //

    // check how to generate the method itself
    if (method_get_code_type(method) == METHOD_RUNTIME) {
        if (method_is_internal_call(method)) {
            // only the corelib is allowed to have native methods
            bool found = false;
            for (int i = 0; i < arrlen(m_extern_whitelist); i++) {
                if (string_equals_cstr(method->Module->Name, m_extern_whitelist[i])) {
                    found = true;
                    break;
                }
            }
            CHECK(found, "Assembly `%U` is not allowed to have internal calls", method->Module->Name);

            // import it
            method->MirFunc = MIR_new_import(ctx->ctx, strbuilder_get(&func_name));

        } else {
            //
            // Delegates have special stuff
            //
            if (method->DeclaringType->BaseType == tSystem_MulticastDelegate) {
                // for the ctor
                if (string_equals_cstr(method->Name, ".ctor")) {
                    CHECK(method->ReturnType == NULL);
                    CHECK(method->Parameters->Length == 2);
                    CHECK(method->Parameters->Data[0]->ParameterType == tSystem_Object);
                    CHECK(method->Parameters->Data[1]->ParameterType == tSystem_IntPtr);
                    CHECK(method_is_special_name(method));
                    CHECK(method_is_rt_special_name(method));
                    CHECK(!method_is_static(method));
                    method->MirFunc = m_delegate_ctor_func;

                } else if (string_equals_cstr(method->Name, "Invoke")) {
                    CHECK(!method_is_static(method));
                    CHECK(method->DeclaringType->DelegateSignature == NULL);
                    method->DeclaringType->DelegateSignature = method;

                    // create the function
                    method->MirFunc = MIR_new_func_arr(ctx->ctx, strbuilder_get(&func_name), nres, res_type, arrlen(vars),
                                                       vars);

                    // remove the this so we can have a static prototype
                            arrdel(vars, this_index);

                    // prepare the signature for the non-
                    proto_static_name = strbuilder_new();
                    method_print_full_name(method, &proto_static_name);
                    strbuilder_cstr(&proto_static_name, "$proto_static");
                    MIR_item_t static_invoke_proto = MIR_new_proto_arr(ctx->ctx, strbuilder_get(&proto_static_name), nres,
                                                                       res_type, arrlen(vars), vars);

                    // generate the dispatcher
                    CHECK_AND_RETHROW(jit_multicast_delegate_invoke(ctx, method, method->MirFunc, static_invoke_proto));
                    MIR_finish_func(ctx->ctx);
                    MIR_new_export(ctx->ctx, strbuilder_get(&func_name));

                } else {
                    CHECK_FAIL();
                }

            //
            // Unsafe has special generic functions we want to generate
            //
            } else if (method->DeclaringType == tSystem_Runtime_CompilerServices_Unsafe) {
                // The Unsafe.As functions are super simple, so we are going
                // to have them point to the same method, and hope that it will get inlined
                if (
                    string_equals_cstr(method->GenericMethodDefinition->Name, "As") ||
                    string_equals_cstr(method->GenericMethodDefinition->Name, "AsPointer") ||
                    string_equals_cstr(method->GenericMethodDefinition->Name, "AsRef")
                ) {
                    method->MirFunc = m_unsafe_as_func;
                } else if (string_equals_cstr(method->GenericMethodDefinition->Name, "SizeOf")) {
                    method->MirFunc = INVALID_IMPORT;
                    m_Unsafe_SizeOf = method->GenericMethodDefinition;
                } else {
                    CHECK_FAIL();
                }

            //
            // RuntimeHelpers has special generic functions we want to generate
            //
            } else if (method->DeclaringType == tSystem_Runtime_CompilerServices_RuntimeHelpers) {
                if (method->GenericMethodDefinition != NULL) {
                    if (string_equals_cstr(method->GenericMethodDefinition->Name, "IsReferenceOrContainsReferences")) {
                        method->MirFunc = INVALID_IMPORT;
                        m_RuntimeHelpers_IsReferenceOrContainsReferences = method->GenericMethodDefinition;
                    } else {
                        CHECK_FAIL();
                    }
                } else if (string_equals_cstr(method->Name, "GetObjectPointer")) {
                    method->MirFunc = m_unsafe_as_func;
                } else {
                    CHECK_FAIL();
                }

            //
            // Handle extensions from the host
            //
            } else {
                bool found = false;
                for (int i = 0; i < arrlen(m_generic_extern_hooks); i++) {
                    if (m_generic_extern_hooks[i]->can_gen(method)) {

                        // create the function
                        method->MirFunc = MIR_new_func_arr(ctx->ctx, strbuilder_get(&func_name), nres, res_type, arrlen(vars), vars);
                        CHECK_AND_RETHROW(m_generic_extern_hooks[i]->gen(ctx->ctx, method));
                        MIR_finish_func(ctx->ctx);
                        MIR_new_export(ctx->ctx, strbuilder_get(&func_name));

                        found = true;
                        break;
                    }
                }

                // make sure someone handled this
                CHECK(found);
            }
        }

    //
    // Handle native methods, these are just an import
    //
    } else if (method_get_code_type(method) == METHOD_NATIVE) {

        // only the corelib is allowed to have native methods
        bool found = false;
        for (int i = 0; i < arrlen(m_extern_whitelist); i++) {
            if (string_equals_cstr(method->Module->Name, m_extern_whitelist[i])) {
                found = true;
                break;
            }
        }
        CHECK(found, "Assembly `%U` is not allowed to have internal calls", method->Module->Name);

        method->MirFunc = MIR_new_import(ctx->ctx, strbuilder_get(&func_name));

    //
    // Handle normal methods
    //
    } else if (method_get_code_type(method) == METHOD_IL) {
        // don't allow, as we may use this in a later stage
        CHECK(!method_is_internal_call(method),
              "Method marked as IL must not be internal (%s)", strbuilder_get(&func_name));

        // TODO: support for synchronized methods
        CHECK(!method_is_synchronized(method));

        // create a function, we will finish it right away and append to it in the future
        method->MirFunc = MIR_new_func_arr(ctx->ctx, strbuilder_get(&func_name), nres, res_type, arrlen(vars), vars);
        MIR_finish_func(ctx->ctx);

        // queue for jit
        arrpush(ctx->methods_to_jit, method);
    } else {
        CHECK_FAIL();
    }

cleanup:
    arrfree(vars);
    strbuilder_free(&proto_name);
    strbuilder_free(&func_name);

    return err;
}

err_t jit_prepare_static_type(jit_context_t* ctx, System_Type type) {
    err_t err = NO_ERROR;

    // set type dependencies
    // TODO: use hash array instead
    if (ctx->current_method != NULL) {
        int idx = hmgeti(ctx->type_init_dependencies, ctx->current_method->DeclaringType);
        if (idx >= 0) {
            System_Type* arr = ctx->type_init_dependencies[idx].value;
            bool found = false;
            for (int i = 0; i < arrlen(arr); i++) {
                if (arr[i] == type) {
                    found = true;
                    break;
                }
            }

            if (!found) {
                arrpush(arr, type);
            }

            ctx->type_init_dependencies[idx].value = arr;
        } else {
            System_Type* arr = NULL;
            arrpush(arr, type);
            hmput(ctx->type_init_dependencies, ctx->current_method->DeclaringType, arr);
        }
    }

    if (type->MirType != NULL) {
        goto cleanup;
    }

    // make sure the type is fully filled, so we can actually do the static fields stuff
    // TODO: in theory we can further delay the methods initialization, but I think it is
    //       good practice to do it in here
    CHECK_AND_RETHROW(filler_fill_type(type));

    // prepare the type reference
    strbuilder_t type_name = strbuilder_new();
    type_print_full_name(type, &type_name);
    type->MirType = MIR_new_import(ctx->ctx, strbuilder_get(&type_name));
    MIR_load_external(m_mir_context, strbuilder_get(&type_name), type);
    strbuilder_free(&type_name);

    // generic definitions are not initialized here
    if (type_is_generic_definition(type)) {
        goto cleanup;
    }

    // references: prepare the base type
    if (type->BaseType != NULL) {
        CHECK_AND_RETHROW(jit_prepare_static_type(ctx, type->BaseType));
    }

    // arrays and enums: prepare the element type
    if (type->ElementType != NULL) {
        CHECK_AND_RETHROW(jit_prepare_static_type(ctx, type->ElementType));
    }

    // prepare the static fields
    if (type->Fields != NULL) {
        for (int i = 0; i < type->Fields->Length; i++) {
            System_Reflection_FieldInfo fieldInfo = type->Fields->Data[i];

            // prepare the field type
            CHECK_AND_RETHROW(jit_prepare_static_type(ctx, fieldInfo->FieldType));

            // for static fields declare the bss
            if (field_is_static(fieldInfo)) {
                if (field_is_thread_static(fieldInfo)) {
                    // thread local
                    fieldInfo->ThreadStaticIndex = add_thread_local(fieldInfo->FieldType);
                } else if (!fieldInfo->HasRva) {
                    // normal field
                    strbuilder_t field_name = strbuilder_new();
                    type_print_full_name(fieldInfo->DeclaringType, &field_name);
                    strbuilder_cstr(&field_name, "::");
                    strbuilder_utf16(&field_name, fieldInfo->Name->Chars, fieldInfo->Name->Length);
                    fieldInfo->MirField = MIR_new_bss(ctx->ctx, strbuilder_get(&field_name), fieldInfo->FieldType->StackSize);
                    strbuilder_free(&field_name);
                }
            }
        }
    }

    // prepare the cctor
    if (type->TypeInitializer != NULL) {
        CHECK_AND_RETHROW(jit_prepare_method(ctx, type->TypeInitializer));
    }

    // queue this class to the static types
    arrpush(ctx->static_types, type);

cleanup:
    return err;
}

err_t jit_prepare_instance_type(jit_context_t* ctx, System_Type type) {
    err_t err = NO_ERROR;

    // mark that we don't need this again
    if (type->JittedVirtualMethods) {
        goto cleanup;
    }
    type->JittedVirtualMethods = true;

    // first init the type as a static one
    CHECK_AND_RETHROW(jit_prepare_static_type(ctx, type));

    // we can't have instance generic definitions
    ASSERT(!type_is_generic_definition(type));

    // now we want to prepare all the virtual methods that we have methods
    for (int i = 0; i < type->VirtualMethods->Length; i++) {
        System_Reflection_MethodInfo method = type->VirtualMethods->Data[i];

        // if this is a generic method then we don't prepare it in here
        // since it needs to be prepared per-instance
        ASSERT(method->GenericArguments == NULL);

        // now prepare the method itself
        CHECK_AND_RETHROW(jit_prepare_method(ctx, method));
        ASSERT(method->MirFunc != NULL);
    }

    // queue this class to the static types
    arrpush(ctx->instance_types, type);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Branching helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#ifdef JIT_TRACE_MIR
#define MIR_append_insn(...) MIR_append_insn_output(__VA_ARGS__)
#endif

err_t jit_resolve_branch(jit_method_context_t* ctx, int il_target, MIR_label_t* label) {
    err_t err = NO_ERROR;

    // resolve the label
    if (il_target >= ctx->il_offset) {
        // forward jump, check if someone already jumps to there
        int i = hmgeti(ctx->pc_to_stack_snapshot, il_target);
        if (i == -1) {
            // nope, we are the first
            *label = MIR_new_label(mir_ctx);
            stack_snapshot_t snapshot = {
                .key = il_target,
                .label = *label,
                .stack = stack_snapshot(ctx),
                .ireg_depth = ctx->ireg.depth,
                .freg_depth = ctx->freg.depth,
                .dreg_depth = ctx->dreg.depth,
            };
            hmputs(ctx->pc_to_stack_snapshot, snapshot);
        } else {
            // yes, we need to merge with it, we can allow changes because we did not
            // arrive to that part of scanning yet
            stack_t snapshot = ctx->pc_to_stack_snapshot[i].stack;
            CHECK_AND_RETHROW(stack_merge(ctx, &snapshot, true));
            *label = ctx->pc_to_stack_snapshot[i].label;
        }
    } else {
        // backwards jump, get the stack there and validate it, we can not
        // actually merge the stack because we already scanned through that
        // part of the code
        int i = hmgeti(ctx->pc_to_stack_snapshot, il_target);
        CHECK(i != -1);
        stack_t snapshot = ctx->pc_to_stack_snapshot[i].stack;
        CHECK_AND_RETHROW(stack_merge(ctx, &snapshot, false));
        *label = ctx->pc_to_stack_snapshot[i].label;
    }

cleanup:
    return err;
}

/**
 * In addition to calling jit_resolve_branch, this will also take care of rules regarding
 * jumping in and out of exception clauses
 */
err_t jit_branch_point(jit_method_context_t* ctx, int il_target, MIR_label_t* label) {
    err_t err = NO_ERROR;

    // validate we are not actually exiting a protected block with this branch
    System_Reflection_ExceptionHandlingClause_Array exceptions = ctx->method->MethodBody->ExceptionHandlingClauses;
    for (int i = 0; i < exceptions->Length; i++) {
        System_Reflection_ExceptionHandlingClause clause = exceptions->Data[i];

        bool is_offset_in_try = clause->TryOffset <= ctx->il_offset && ctx->il_offset < clause->TryOffset + clause->TryLength;
        bool is_target_in_try = clause->TryOffset <= il_target && il_target < clause->TryOffset + clause->TryLength;

        if (is_offset_in_try) {
            // we are in the handler, make sure we only jump within it
            CHECK(is_target_in_try);

            // we know source and target, we are clear
            break;
        } else {
            // we are outside the handler, make sure we don't jump into it
            CHECK(!is_target_in_try);
        }

        bool is_offset_in_handler = clause->HandlerOffset <= ctx->il_offset && ctx->il_offset < clause->HandlerOffset + clause->HandlerLength;
        bool is_target_in_handler = clause->HandlerOffset <= il_target && il_target < clause->HandlerOffset + clause->HandlerLength;

        if (is_offset_in_handler) {
            // we are in the handler, make sure we only jump within it
            CHECK(is_target_in_handler);

            // we know source and target, we are clear
            break;
        } else {
            // we are outside the handler, make sure we don't jump into it
            CHECK(!is_target_in_handler);
        }
    }

    // now we can do the actual branch resolving
    CHECK_AND_RETHROW(jit_resolve_branch(ctx, il_target, label));

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Allocation related
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Allocate a new object, testing for out of memory if needed
 *
 * TODO: maybe move the throwing into the allocating function and call it normally?
 */
err_t jit_new(jit_method_context_t* ctx, MIR_reg_t result, System_Type type, MIR_op_t size) {
    err_t err = NO_ERROR;

    // make sure the type is known, we need this specifically in here
    // so all the exceptions that we throw from the runtime will be
    // added properly
    CHECK_AND_RETHROW(jit_prepare_instance_type(ctx->ctx, type));

    // allocate the new object
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_call_insn(mir_ctx, 5,
                                      MIR_new_ref_op(mir_ctx, m_gc_new_proto),
                                      MIR_new_ref_op(mir_ctx, m_gc_new_func),
                                      MIR_new_reg_op(mir_ctx, result),
                                      MIR_new_ref_op(mir_ctx, type->MirType),
                                      size));

#ifdef READABLE_JIT
    goto cleanup;
#endif

    // this is an edge case, if we get to this point then just let it crash...
//    if (type != tSystem_OutOfMemoryException) {
//        // if we got NULL from the gc_new function it means we got an OOM
//
//        // handle any exception which might have been thrown
//        MIR_insn_t label = MIR_new_label(mir_ctx);
//
//        // if we have a non-zero value then skip the throw
//        MIR_append_insn(mir_ctx, mir_func,
//                        MIR_new_insn(mir_ctx, MIR_BT,
//                                     MIR_new_label_op(mir_ctx, label),
//                                     MIR_new_reg_op(mir_ctx, result)));
//
//        // throw the error, it has an unknown type
//        CHECK_AND_RETHROW(jit_throw_new(ctx, tSystem_OutOfMemoryException));
//
//        // insert the skip label
//        MIR_append_insn(mir_ctx, mir_func, label);
//    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Checking for stuff
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Emit a null check, throwing System.NullReferenceException if the value at reg is null
 */
err_t jit_null_check(jit_method_context_t* ctx, MIR_reg_t reg, System_Type type) {
    err_t err = NO_ERROR;

#ifdef READABLE_JIT
    goto cleanup;
#endif

    if (type == NULL) {
        // this is a null type, just throw it
        CHECK_AND_RETHROW(jit_throw_new(ctx, tSystem_NullReferenceException));
    } else {
        CHECK(type_is_object_ref(type));

        MIR_label_t not_null = MIR_new_label(mir_ctx);

        if (type_is_interface(type)) {
            // this is an interface, we need to get the object reference in it and check if
            // that is zero
            MIR_reg_t temp_reg = jit_new_temp_reg(ctx, tSystem_Object);
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_reg_op(mir_ctx, temp_reg),
                                         MIR_new_mem_op(mir_ctx, MIR_T_P,
                                                        sizeof(void*), reg, 0, 1)));
            reg = temp_reg;
        }

        // check if reg is null
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_BT,
                                     MIR_new_label_op(mir_ctx, not_null),
                                     MIR_new_reg_op(mir_ctx, reg)));

        CHECK_AND_RETHROW(jit_throw_new(ctx, tSystem_NullReferenceException));

        MIR_append_insn(mir_ctx, mir_func, not_null);
    }

cleanup:
    return err;
}

/**
 * Emit a range check on an array, throwing System.IndexOutOfRangeException if the index was
 * outside the array bounds
 */
static err_t jit_oob_check(jit_method_context_t* ctx, MIR_reg_t array_reg, MIR_reg_t index_reg) {
    err_t err = NO_ERROR;

#ifdef READABLE_JIT
    goto cleanup;
#endif

    MIR_label_t not_oob = MIR_new_label(mir_ctx);
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_UBLT,
                                 MIR_new_label_op(mir_ctx, not_oob),
                                 MIR_new_reg_op(mir_ctx, index_reg),
                                 MIR_new_mem_op(mir_ctx, MIR_T_I32,
                                                offsetof(struct System_Array, Length),
                                                array_reg, 0, 1)));
    CHECK_AND_RETHROW(jit_throw_new(ctx, tSystem_IndexOutOfRangeException));
    MIR_append_insn(mir_ctx, mir_func, not_oob);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Casting helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Emit a cast between an object to an interface, this is not always the most trivial
 * thing but it is needed
 */
err_t jit_cast_obj_to_interface(jit_method_context_t* ctx,
                                   MIR_reg_t result_reg, MIR_reg_t from_reg,
                                   System_Type from_type, System_Type to_type
) {
    err_t err = NO_ERROR;

    // temp register to use for stuff
    MIR_reg_t vtable_reg = jit_new_temp_reg(ctx, tSystem_IntPtr);

    MIR_op_t vtable_op;
    if (from_type != NULL) {
        TinyDotNet_Reflection_InterfaceImpl interface = type_get_interface_impl(from_type, to_type);
        CHECK(interface != NULL);

        MIR_insn_t skip_vtable = MIR_new_label(mir_ctx);

        // vtable_reg = 0
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_MOV,
                                     MIR_new_reg_op(mir_ctx, vtable_reg),
                                     MIR_new_int_op(mir_ctx, 0)));

        // if (from_reg == 0) goto skip_vtable;
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_BF,
                                     MIR_new_label_op(mir_ctx, skip_vtable),
                                     MIR_new_reg_op(mir_ctx, from_reg)));

        // vtable_reg = &object->vtable[vtable_offset];
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_ADD,
                                     MIR_new_reg_op(mir_ctx, vtable_reg),
                                     MIR_new_mem_op(mir_ctx, MIR_T_U32,
                                                    offsetof(struct System_Object, vtable),
                                                    from_reg, 0, 1),
                                     MIR_new_int_op(mir_ctx, interface->VTableOffset * sizeof(void *))));

        // skip_vtable:
        MIR_append_insn(mir_ctx, mir_func, skip_vtable);

        vtable_op = MIR_new_reg_op(mir_ctx, vtable_reg);
    } else {
        // cast a known null, just emit it nicely
        vtable_op = MIR_new_int_op(mir_ctx, 0);
    }

    // result_reg[0] = vtable_reg;
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_MOV,
                                 MIR_new_mem_op(mir_ctx, MIR_T_P,
                                                offsetof(Interface, VTable),
                                                result_reg, 0, 1),
                                 vtable_op));

    // TODO: figure a way to optimize this better, update the object reference
    //       using a gc_update_ref call, this would be better if we can know from
    //       the caller if this is needed or not
    // we are going to store the result in the vtable_reg so we won't override the result
    // register, as it may still be used by the caller to do stuff
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_ADD,
                                 MIR_new_reg_op(mir_ctx, vtable_reg),
                                 MIR_new_reg_op(mir_ctx, result_reg),
                                 MIR_new_int_op(mir_ctx, sizeof(void*))));
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_call_insn(mir_ctx, 4,
                                      MIR_new_ref_op(mir_ctx, m_gc_update_ref_proto),
                                      MIR_new_ref_op(mir_ctx, m_gc_update_ref_func),
                                      MIR_new_reg_op(mir_ctx, vtable_reg),
                                      MIR_new_reg_op(mir_ctx, from_reg)));

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Generic opcode jitting
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Jit both compare and compare-branch operations, will do so based on the passed
 * MIR instruction code.
 *
 * Handles implicit floating point casting as well as int32<->intptr handling
 */
static err_t jit_compare_branch(jit_method_context_t* ctx, int il_target, MIR_insn_code_t code) {
    err_t err = NO_ERROR;

    MIR_reg_t value2_reg;
    MIR_reg_t value1_reg;
    System_Type value2_type;
    System_Type value1_type;
    CHECK_AND_RETHROW(jit_stack_pop(ctx, &value2_type, &value2_reg, NULL));
    CHECK_AND_RETHROW(jit_stack_pop(ctx, &value1_type, &value1_reg, NULL));

    MIR_reg_t result_reg = 0;
    MIR_label_t label = NULL;
    if (MIR_branch_code_p(code)) {
        CHECK(il_target >= 0);
        CHECK_AND_RETHROW(jit_branch_point(ctx, il_target, &label));
    } else {
        CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_Int32, &result_reg));
    }

    // get the float offset, different for EQ and NE
    int float_offset = (code == MIR_EQ || code == MIR_NE || code == MIR_BEQ || code == MIR_BNE) ? 2 : 4;
    int double_offset = float_offset + 1;

    switch (type_get_stack_type(value1_type)) {
        case STACK_TYPE_INT32: {
            if (type_get_stack_type(value2_type) == STACK_TYPE_INT32) {
                code += 1;
            } else {
                CHECK(type_get_stack_type(value2_type) == STACK_TYPE_INTPTR);

                // sign extend to intptr
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_UEXT32,
                                             MIR_new_reg_op(mir_ctx, value1_reg),
                                             MIR_new_reg_op(mir_ctx, value1_reg)));
            }
        } break;

        case STACK_TYPE_INT64: {
            CHECK(type_get_stack_type(value2_type) == STACK_TYPE_INT64);
        } break;

        case STACK_TYPE_INTPTR: {
            if (type_get_stack_type(value2_type) == STACK_TYPE_INT32) {
                // sign extend to intptr
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_UEXT32,
                                             MIR_new_reg_op(mir_ctx, value2_reg),
                                             MIR_new_reg_op(mir_ctx, value2_reg)));
            } else {
                CHECK(type_get_stack_type(value2_type) == STACK_TYPE_INTPTR);
            }
        } break;

        case STACK_TYPE_FLOAT: {
            CHECK(value2_type == tSystem_Double || value2_type == tSystem_Single);

            // convert unsigned operations to normal ones
            // TODO: figure what the fuck unordered means and how do I make MIR do that
            switch (code) {
                case MIR_ULT: code = MIR_LT; break;
                case MIR_ULE: code = MIR_LE; break;
                case MIR_UGT: code = MIR_GT; break;
                case MIR_UGE: code = MIR_GE; break;
                case MIR_UBLT: code = MIR_BLT; break;
                case MIR_UBLE: code = MIR_BLE; break;
                case MIR_UBGT: code = MIR_BGT; break;
                case MIR_UBGE: code = MIR_BGE; break;
                default:
                    break;
            }

            if (value1_type == tSystem_Single) {
                if (value2_type == tSystem_Single) {
                    // need to do float compare
                    code += float_offset;
                } else if (value2_type == tSystem_Double) {
                    // need to do double compare
                    code += double_offset;

                    // implicit conversion float->double
                    MIR_reg_t value1_double_reg = jit_new_temp_reg(ctx, tSystem_Double);
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_F2D,
                                                 MIR_new_reg_op(mir_ctx, value1_double_reg),
                                                 MIR_new_reg_op(mir_ctx, value1_reg)));
                    value1_reg = value1_double_reg;
                }
            } else if (value1_type == tSystem_Double) {
                // always double math
                code += double_offset;

                if (value2_type == tSystem_Single) {
                    // implicit conversion float->double
                    MIR_reg_t value2_double_reg = jit_new_temp_reg(ctx, tSystem_Double);
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_F2D,
                                                 MIR_new_reg_op(mir_ctx, value2_double_reg),
                                                 MIR_new_reg_op(mir_ctx, value2_reg)));
                    value2_reg = value2_double_reg;
                }
            }
        } break;

        case STACK_TYPE_O: {
            CHECK(type_get_stack_type(value2_type) == STACK_TYPE_O);

            CHECK(
                code == MIR_EQ ||
                code == MIR_BEQ ||
                code == MIR_BNE ||
                code == MIR_UGT
            );

            // for interfaces convert them to the object pointer
            if (type_is_interface(value1_type)) {
                MIR_reg_t value1_ptr_reg = jit_new_temp_reg(ctx, tSystem_Object);
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, value1_ptr_reg),
                                             MIR_new_mem_op(mir_ctx, MIR_T_P, 8, value1_reg, 0, 1)));
                value1_reg = value1_ptr_reg;
            }

            if (type_is_interface(value2_type)) {
                MIR_reg_t value2_ptr_reg = jit_new_temp_reg(ctx, tSystem_Object);
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, value2_ptr_reg),
                                             MIR_new_mem_op(mir_ctx, MIR_T_P, 8, value2_reg, 0, 1)));
                value2_reg = value2_ptr_reg;
            }
        } break;

        case STACK_TYPE_REF: {
            CHECK(type_get_stack_type(value2_type) == STACK_TYPE_REF);
        } break;

        // invalid comparisons
        case STACK_TYPE_VALUE_TYPE:
            CHECK_FAIL();
    }

    // emit common code
    if (MIR_branch_code_p(code)) {
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, code,
                                     MIR_new_label_op(mir_ctx, label),
                                     MIR_new_reg_op(mir_ctx, value1_reg),
                                     MIR_new_reg_op(mir_ctx, value2_reg)));
    } else {
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, code,
                                     MIR_new_reg_op(mir_ctx, result_reg),
                                     MIR_new_reg_op(mir_ctx, value1_reg),
                                     MIR_new_reg_op(mir_ctx, value2_reg)));
    }

cleanup:
    return err;
}

/**
 * Emit a binary operation, taking care of all the implicit casting that is needed to be done, and
 * checking for specific edge cases on others.
 *
 * This also emits a divide by zero check if needed.
 *
 * If the operation is only allowed for integers then make sure to mark it as such
 */
static err_t jit_binary_numeric_operation(jit_method_context_t* ctx, MIR_insn_code_t code, bool integer_only) {
    err_t err = NO_ERROR;

    MIR_reg_t value2_reg;
    MIR_reg_t value1_reg;
    MIR_reg_t result_reg = 0;
    System_Type value2_type;
    System_Type value1_type;
    CHECK_AND_RETHROW(jit_stack_pop(ctx, &value2_type, &value2_reg, NULL));
    CHECK_AND_RETHROW(jit_stack_pop(ctx, &value1_type, &value1_reg, NULL));

    stack_type_t value1_stacktype = type_get_stack_type(value1_type);
    stack_type_t value2_stacktype = type_get_stack_type(value2_type);

    if (code == MIR_LSH || code == MIR_RSH || code == MIR_URSH) {
        // check the types are valid
        CHECK(value1_stacktype == STACK_TYPE_INT32 || value1_stacktype == STACK_TYPE_INT64 || value1_stacktype == STACK_TYPE_INTPTR);
        CHECK(value2_stacktype == STACK_TYPE_INT32 || value2_stacktype == STACK_TYPE_INTPTR);

        // we are doing a 32bit operation
        if (value1_stacktype == STACK_TYPE_INT32) {
            code += 1;
        }

        // push the result
        CHECK_AND_RETHROW(jit_stack_push(ctx, value1_type, &result_reg));
    } else {
        if (code == MIR_DIV || code == MIR_UDIV || code == MIR_MOD || code == MIR_UMOD) {
            MIR_insn_t label = MIR_new_label(mir_ctx);

            // do floats have div by zero
            if (value1_type->StackType != STACK_TYPE_FLOAT) {
                // these need to check that value2 is not zero
                // if we have a non-zero value then skip the throw
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_BT,
                                             MIR_new_label_op(mir_ctx, label),
                                             MIR_new_reg_op(mir_ctx, value2_reg)));

                // throw the error, it has an unknown type
                CHECK_AND_RETHROW(jit_throw_new(ctx, tSystem_DivideByZeroException));

                // insert the skip label
                MIR_append_insn(mir_ctx, mir_func, label);
            }
        }

        switch (type_get_stack_type(value1_type)) {
            case STACK_TYPE_INT32: {
                if (type_get_stack_type(value2_type) == STACK_TYPE_INT32) {
                    // int32 x int32
                    CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_Int32, &result_reg));
                    code += 1;
                } else {
                    // int32 x intptr
                    CHECK(type_get_stack_type(value2_type) == STACK_TYPE_INTPTR);
                    CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_IntPtr, &result_reg));

                    // sign extend the int32 to intptr
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_EXT32,
                                                 MIR_new_reg_op(mir_ctx, value1_reg),
                                                 MIR_new_reg_op(mir_ctx, value1_reg)));
                }
            } break;

            case STACK_TYPE_INT64: {
                // int64 x int64
                CHECK(type_get_stack_type(value2_type) == STACK_TYPE_INT64);
                CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_Int64, &result_reg));
            } break;

            case STACK_TYPE_INTPTR: {
                CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_IntPtr, &result_reg));

                if (type_get_stack_type(value2_type) == STACK_TYPE_INT32) {
                    // intptr x int32
                    // sign extend
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_EXT32,
                                                 MIR_new_reg_op(mir_ctx, value2_reg),
                                                 MIR_new_reg_op(mir_ctx, value2_reg)));
                } else {
                    // intptr x intptr
                    CHECK(type_get_stack_type(value2_type) == STACK_TYPE_INTPTR);
                }
            } break;

            case STACK_TYPE_FLOAT: {
                // make sure this is not an integer only operation
                CHECK(!integer_only);

                if (value1_type == tSystem_Single) {
                    if (value2_type == tSystem_Single) {
                        // float x float -> float
                        CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_Single, &result_reg));
                        if (code == MIR_DIV) {
                            // this is not in offset of 2
                            code = MIR_FDIV;
                        } else {
                            code += 2;
                        }
                    } else {
                        CHECK(value2_type == tSystem_Double);

                        // float x double
                        // convert the float to a double
                        CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_Double, &result_reg));
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_F2D,
                                                     MIR_new_reg_op(mir_ctx, result_reg),
                                                     MIR_new_reg_op(mir_ctx, value1_reg)));
                        value1_reg = result_reg;
                        if (code == MIR_DIV) {
                            // this is not in offset of 3
                            code = MIR_DDIV;
                        } else {
                            code += 3;
                        }
                    }
                } else {
                    CHECK(value1_type == tSystem_Double);

                    // this always results in a double math
                    CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_Double, &result_reg));
                    if (code == MIR_DIV) {
                        // this is not in offset of 3
                        code = MIR_DDIV;
                    } else {
                        code += 3;
                    }

                    if (value2_type == tSystem_Single) {
                        // double x float
                        // convert the float to a double
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_F2D,
                                                     MIR_new_reg_op(mir_ctx, result_reg),
                                                     MIR_new_reg_op(mir_ctx, value2_reg)));
                        value2_reg = result_reg;
                    } else {
                        CHECK(value2_type == tSystem_Double);
                    }
                }
            } break;

            // only allowed if pointers are allowed
            case STACK_TYPE_REF: {
                CHECK(ctx->allow_pointers, "math on byref is not allowed");
                CHECK(code == MIR_ADD); // only addition is supported

                CHECK_AND_RETHROW(jit_stack_push(ctx, value1_type, &result_reg));

                if (type_get_stack_type(value2_type) == STACK_TYPE_INT32) {
                    // ref x int32
                    // sign extend
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_EXT32,
                                                 MIR_new_reg_op(mir_ctx, value2_reg),
                                                 MIR_new_reg_op(mir_ctx, value2_reg)));
                } else {
                    // ref x intptr
                    CHECK(type_get_stack_type(value2_type) == STACK_TYPE_INTPTR);
                }
            } break;

            // not allowed to do math on these
            case STACK_TYPE_VALUE_TYPE:
            case STACK_TYPE_O:
                CHECK_FAIL();
        }
    }

    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, code,
                                 MIR_new_reg_op(mir_ctx, result_reg),
                                 MIR_new_reg_op(mir_ctx, value1_reg),
                                 MIR_new_reg_op(mir_ctx, value2_reg)));

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Main jitter code
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static err_t jit_replace_reg_with_op(MIR_func_t func, MIR_reg_t reg, MIR_op_t op) {
    err_t err = NO_ERROR;

    for (
        MIR_insn_t insn = DLIST_HEAD (MIR_insn_t, func->insns);
        insn != NULL;
        insn = DLIST_NEXT (MIR_insn_t, insn)
    ) {
        for (int i = 0; i < insn->nops; i++) {
            switch (insn->ops[i].mode) {
                // reg, check if the one we wanna replace and
                // if so replace it
                case MIR_OP_REG: {
                    if (insn->ops[i].u.reg == reg) {
                        insn->ops[i] = op;
                    }
                } break;

                // for memory check that the base and index are not the one we want
                // TODO: lets hope we won't need to ever spill something like that...
                case MIR_OP_MEM: {
                    CHECK(insn->ops[i].u.mem.base != reg);
                    CHECK(insn->ops[i].u.mem.index != reg);
                } break;

                // The rest don't hold any registers
                default:
                    break;
            }
        }
    }

cleanup:
    return err;
}

typedef struct local {
    // the operation to get this entry
    MIR_op_t op;

    // this is a readonly ref
    uint32_t readonly_ref : 1;

    // this is a non-local ref
    uint32_t non_local_ref : 1;
} local_t;

/**
 * This is the main jitting function, it takes a method and jits it completely
 */
static err_t jit_method_body(jit_method_context_t* ctx) {
    err_t err = NO_ERROR;

    System_Reflection_MethodInfo method = ctx->method;

    // we are going to allow to corelib specifically
    // to do deref on pointers
    ctx->allow_pointers = string_equals_cstr(method->Module->Name, "Corelib.dll");

    System_Reflection_Assembly assembly = method->Module->Assembly;

    strbuilder_t method_name = strbuilder_new();
    method_print_full_name(method, &method_name);

    JIT_TRACE(
        strbuilder_t ret_type_name = strbuilder_new();
        type_print_full_name(method->ReturnType, &ret_type_name);

        TRACE(".method %s %s %s %s",
              method_access_str(method_get_access(method)),
              method_is_static(method) ? "static" : "instance",
              strbuilder_get(&ret_type_name),
              strbuilder_get(&method_name));
        strbuilder_free(&ret_type_name);

        TRACE("{");
        TRACE("\t.maxstack %d", body->MaxStackSize);
    );

    // variables
    local_t* locals = NULL;
    MIR_op_t* arguments = NULL;

    // jump table dynamic array
    MIR_op_t* switch_ops = NULL;

    // Create the exception handling reg and zero it
    ctx->exception_reg = MIR_new_func_reg(mir_ctx, mir_func->u.func, MIR_T_I64, "exception");

    // get the return block register, if any
    MIR_reg_t return_block_reg = 0;
    if (method->ReturnType != NULL && jit_get_mir_type(method->ReturnType) == MIR_T_BLK) {
        return_block_reg = MIR_reg(mir_ctx, "return_block", mir_func->u.func);
    }

    JIT_TRACE(
        if (body->LocalVariables->Length > 0) {
            TRACE("\t.locals %s(", body->InitLocals ? "init " : "");
        }
    );

    // init the arguments
    // check if has this
    if (!method_is_static(method)) {
        arrpush(arguments, MIR_new_reg_op(mir_ctx, MIR_reg(mir_ctx, "this", mir_func->u.func)));
    }

    // now handle arguments
    for (int i = 0; i < method->Parameters->Length; i++) {
        char arg_name_buf[64];
        snprintf(arg_name_buf, sizeof(arg_name_buf), "arg%d", i);
        arrpush(arguments, MIR_new_reg_op(mir_ctx, MIR_reg(mir_ctx, arg_name_buf, mir_func->u.func)));
    }

    // actually create locals
    // TODO: now that we have spilling code we could maybe optimize this
    for (int i = 0; i < body->LocalVariables->Length; i++) {
        System_Reflection_LocalVariableInfo variable = body->LocalVariables->Data[i];
        CHECK(variable->LocalIndex == i);

        JIT_TRACE(
            strbuilder_t local_type_name = strbuilder_new();
            type_print_full_name(variable->LocalType, &local_type_name);
            TRACE("\t\t[%d] %s%s", i, strbuilder_get(&local_type_name), i == body->LocalVariables->Length - 1 ? "" : ",");
            strbuilder_free(&local_type_name);
        );

        // prepare the variable type
        CHECK_AND_RETHROW(jit_prepare_static_type(ctx->ctx, variable->LocalType));

        // we are going to initialize all of the variables
        char name[64] = { 0 };
        snprintf(name, sizeof(name), "var%d", i);
        MIR_reg_t reg = MIR_new_func_reg(mir_ctx, mir_func->u.func, MIR_T_I64, name);
        arrpush(locals, (local_t){ .op = MIR_new_reg_op(mir_ctx, reg) });

        // allocate the stack memory
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_ALLOCA,
                                     MIR_new_reg_op(mir_ctx, reg),
                                     MIR_new_int_op(mir_ctx, variable->LocalType->StackSize)));

        switch (variable->LocalType->StackType) {
            case STACK_TYPE_O:
            case STACK_TYPE_INT32:
            case STACK_TYPE_INT64:
            case STACK_TYPE_REF:
            case STACK_TYPE_INTPTR: {
                // interface is a fat pointer, so treat it like a value type
                if (type_is_interface(variable->LocalType)) {
                    goto init_local_value_type;
                }
                locals[i].op = MIR_new_mem_op(mir_ctx, jit_get_mir_type(variable->LocalType), 0, reg, 0, 1);
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             locals[i].op,
                                             MIR_new_int_op(mir_ctx, 0)));
            } break;

            case STACK_TYPE_FLOAT: {
                locals[i].op = MIR_new_mem_op(mir_ctx, jit_get_mir_type(variable->LocalType), 0, reg, 0, 1);
                if (variable->LocalType == tSystem_Single) {
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_FMOV,
                                                 locals[i].op,
                                                 MIR_new_float_op(mir_ctx, 0.0f)));
                } else {
                    ASSERT(variable->LocalType == tSystem_Double);
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_DMOV,
                                                 locals[i].op,
                                                 MIR_new_double_op(mir_ctx, 0.0)));
                }
            } break;

            init_local_value_type:
            case STACK_TYPE_VALUE_TYPE: {
                jit_emit_zerofill(ctx, reg, variable->LocalType->StackSize);
            } break;
        }
    }

    JIT_TRACE(
        if (body->LocalVariables->Length > 0) {
            TRACE("\t)");
        }
    );

    // TODO: we need to validate that all branch targets and that all the
    //       try and handler offsets are actually in valid instructions and
    //       not in the middle of instructions

    // prepare the stacks at certain points for exception handling
    bool created_first_entry = false;
    for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
        System_Reflection_ExceptionHandlingClause clause = body->ExceptionHandlingClauses->Data[i];

        int handler_offset = clause->HandlerOffset;
        if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
            handler_offset = clause->FilterOffset;
        }

        // create the stack location
        MIR_label_t label = MIR_new_label(mir_ctx);
        stack_snapshot_t snapshot = {
            .key = handler_offset,
            .label = label,
            .stack = { .entries = NULL },
            .ireg_depth = 0,
            .freg_depth = 0,
            .dreg_depth = 0,
        };

        if (clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
            CHECK_AND_RETHROW(jit_prepare_static_type(ctx->ctx, clause->CatchType));

            // this is an exception caluse, we need to have the exception pushed on the stack
            // prepare it
            arrpush(snapshot.stack.entries, (stack_entry_t) { .type = clause->CatchType });
            snapshot.ireg_depth++;
            if (!created_first_entry) {
                // create the first stack entry just in case, we are going to create a reg
                // and pop it right away since we don't actually want to have it pushed
                // right now, we just wanna make sure that the stack location exists
                jit_push_new_reg(ctx, tSystem_Object, false);
                ctx->ireg.depth--;
                created_first_entry = true;
            }
        } else if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
            // this is  filter caluse, we need to have the exception pushed on the stack
            // prepare it
            arrpush(snapshot.stack.entries, (stack_entry_t) { .type = tSystem_Object });
            snapshot.ireg_depth++;
            if (!created_first_entry) {
                // create the first stack entry just in case, we are going to create a reg
                // and pop it right away since we don't actually want to have it pushed
                // right now, we just wanna make sure that the stack location exists
                jit_push_new_reg(ctx, tSystem_Object, false);
                ctx->ireg.depth--;
                created_first_entry = true;
            }

            // also add the stack position at the position of the handler itself, which will get jumped to
            // implicitly by the endfilter
            MIR_label_t handler_label = MIR_new_label(mir_ctx);
            stack_snapshot_t handler_snapshot = {
                .key = clause->HandlerOffset,
                .label = handler_label,
                .stack = { .entries = NULL },
                .ireg_depth = 0,
                .freg_depth = 0,
                .dreg_depth = 0,
            };
            arrpush(handler_snapshot.stack.entries, (stack_entry_t) { .type = tSystem_Object });
            handler_snapshot.ireg_depth++;
            hmputs(ctx->pc_to_stack_snapshot, handler_snapshot);
        }

        // now put it in
        hmputs(ctx->pc_to_stack_snapshot, snapshot);
    }

    ctx->jit_trace_indent = 4;

    //
    // The main loop for decoding and jitting opcodes
    //
    opcode_control_flow_t last_cf = OPCODE_CONTROL_FLOW_INVALID;
    opcode_t last_opcode = CEE_INVALID;
    int il_ptr = 0;
    while (il_ptr < body->Il->Length) {

        //--------------------------------------------------------------------------------------------------------------
        // Control flow and stack verification
        //--------------------------------------------------------------------------------------------------------------

        // store the start of the instruction for others to use
        ctx->il_offset = il_ptr;

        // create a snapshot of the stack, if we already have a snapshot
        // of this verify it is the same (we will get a snapshot if we have
        // a forward jump)
        MIR_insn_t cur_label;
        int stacki = hmgeti(ctx->pc_to_stack_snapshot, ctx->il_offset);

        if (
            last_cf == OPCODE_CONTROL_FLOW_BRANCH ||
            last_cf == OPCODE_CONTROL_FLOW_THROW ||
            last_cf == OPCODE_CONTROL_FLOW_RETURN
        ) {
            // control changed by a jump or an exception, this stack can not be full, but rather must
            // be empty or be whatever the stack is already set to be at this point
            if (stacki == -1) {
                // create a new empty stack
                arrfree(ctx->stack.entries);
                ctx->ireg.depth = 0;
                ctx->freg.depth = 0;
                ctx->dreg.depth = 0;
            } else {
                // copy the stack to the current stack
                stack_copy(ctx, &ctx->pc_to_stack_snapshot[stacki].stack);
                ctx->ireg.depth = ctx->pc_to_stack_snapshot[stacki].ireg_depth;
                ctx->freg.depth = ctx->pc_to_stack_snapshot[stacki].freg_depth;
                ctx->dreg.depth = ctx->pc_to_stack_snapshot[stacki].dreg_depth;
            }
        }

        if (stacki != -1) {
            // verify it is the same
            stack_t snapshot = ctx->pc_to_stack_snapshot[stacki].stack;
            cur_label = ctx->pc_to_stack_snapshot[stacki].label;
            CHECK_AND_RETHROW(stack_merge(ctx, &snapshot, true));
        } else {
            // take snapshot
            cur_label = MIR_new_label(mir_ctx);
            stack_snapshot_t snapshot = {
                .key = ctx->il_offset,
                .label = cur_label,
                .stack = stack_snapshot(ctx),
                .ireg_depth = ctx->ireg.depth,
                .freg_depth = ctx->freg.depth,
                .dreg_depth = ctx->dreg.depth
            };
            hmputs(ctx->pc_to_stack_snapshot, snapshot);
        }
        MIR_append_insn(mir_ctx, mir_func, cur_label);

        // validate the control flow from the previous instruction, we can not have anything that
        // continues to enter a handler block
        for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
            System_Reflection_ExceptionHandlingClause clause = body->ExceptionHandlingClauses->Data[i];

            JIT_TRACE(
                if (clause->TryOffset == ctx->il_offset) {
                    TRACE("%*s.try %d", ctx->jit_trace_indent, "", i);
                    TRACE("%*s{", ctx->jit_trace_indent, "");
                    ctx->jit_trace_indent += 4;
                } else if (clause->TryOffset + clause->TryLength == ctx->il_offset) {
                    ctx->jit_trace_indent -= 4;
                    TRACE("%*s} // end .try %d", ctx->jit_trace_indent, "", i);

                }

                if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER && clause->FilterOffset == ctx->il_offset) {
                    TRACE("%*sfilter", ctx->jit_trace_indent, "");
                    TRACE("%*s{", ctx->jit_trace_indent, "");
                    ctx->jit_trace_indent += 4;
                }

                if (clause->HandlerOffset == ctx->il_offset) {
                    if (clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
                        TRACE("%*scatch %U.%U", ctx->jit_trace_indent, "", clause->CatchType->Namespace, clause->CatchType->Name);
                    } else if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
                        TRACE("%*sfinally", ctx->jit_trace_indent, "");
                    } else if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT) {
                        TRACE("%*sfault", ctx->jit_trace_indent, "");
                    } else if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
                        TRACE("%*scatch", ctx->jit_trace_indent, "");
                    }
                    TRACE("%*s{", ctx->jit_trace_indent, "");
                    ctx->jit_trace_indent += 4;
                } else if (clause->HandlerOffset + clause->HandlerLength == ctx->il_offset) {
                    ctx->jit_trace_indent -= 4;
                    TRACE("%*s} // end handler", ctx->jit_trace_indent, "");
                }
            );

            // figure if we enter a filter right now
            bool entered_filter = false;
            if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
                entered_filter = clause->FilterOffset == ctx->il_offset;
            }

            // for catch/filter we need to set the first register on
            // the stack to the exception register so we will have the
            // correct value on the stack the start
            if (
                (
                    clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER ||
                    clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION
                ) &&
                (
                    clause->HandlerOffset == ctx->il_offset ||
                    entered_filter
                )
            ) {
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, ctx->ireg.regs[0]),
                                             MIR_new_reg_op(mir_ctx, ctx->exception_reg)));
            }

            // entry/exit to handler can only happen from exception, so
            // we can't have any instruction that goes next, that is
            // the same for exiting from handler or protected block
            if (
                entered_filter ||
                clause->HandlerOffset == ctx->il_offset ||
                clause->HandlerOffset + clause->HandlerLength == ctx->il_offset ||
                clause->TryOffset + clause->TryLength == ctx->il_offset
            ) {
                CHECK(
                    last_cf == OPCODE_CONTROL_FLOW_BRANCH ||
                    last_cf == OPCODE_CONTROL_FLOW_THROW ||
                    last_cf == OPCODE_CONTROL_FLOW_RETURN
                );
            }

            // remember that we are now in a filter clause, so we can handle
            // endfilter correctly
            if (entered_filter) {
                ctx->filter_clause = clause;
            }
        }

        //--------------------------------------------------------------------------------------------------------------
        // Opcode decoding
        //--------------------------------------------------------------------------------------------------------------

        // get the opcode value
        uint16_t opcode_value = (REFPRE << 8) | body->Il->Data[il_ptr++];

        // get the actual opcode
        opcode_t opcode = g_dotnet_opcode_lookup[opcode_value];
        CHECK_ERROR(opcode != CEE_INVALID, ERROR_INVALID_OPCODE);

        // handle opcodes with special prefix
        if (
            opcode == CEE_PREFIX1 ||
            opcode == CEE_PREFIX2 ||
            opcode == CEE_PREFIX3 ||
            opcode == CEE_PREFIX4 ||
            opcode == CEE_PREFIX5 ||
            opcode == CEE_PREFIX6 ||
            opcode == CEE_PREFIX7
        ) {
            CHECK(il_ptr + 1 <= body->Il->Length);

            // setup the new prefix
            opcode_value <<= 8;
            opcode_value |= body->Il->Data[il_ptr++];
            opcode = g_dotnet_opcode_lookup[opcode_value];
            CHECK_ERROR(opcode != CEE_INVALID, ERROR_INVALID_OPCODE);
        }

        // get the opcode info
        opcode_info_t* opcode_info = &g_dotnet_opcodes[opcode];

        JIT_TRACE(
            printf("[*] %*sIL_%04x: %s ", ctx->jit_trace_indent, "", ctx->il_offset, opcode_info->name);
        );

        // set the last control flow to this one
        last_cf = opcode_info->control_flow;

        //--------------------------------------------------------------------------------------------------------------
        // Handle operands of the opcode
        //--------------------------------------------------------------------------------------------------------------

        // reset the operand info
        ctx->operand = (jit_operand_t){ 0 };
        switch (opcode_info->operand) {
            case OPCODE_OPERAND_InlineBrTarget: {
                operand_i32 = *(int32_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int32_t);
                operand_i32 += il_ptr;

                JIT_TRACE(
                    printf("IL_%04x", operand_i32)
                );
            } break;

            case OPCODE_OPERAND_InlineField: {
                // fetch it
                token_t value = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);

                // resolve it
                CHECK_AND_RETHROW(assembly_get_field_by_token(assembly, value, method->DeclaringType->GenericArguments,
                                                              method->GenericArguments, &operand_field));
                CHECK(operand_field != NULL);

                JIT_TRACE(
                    strbuilder_t type_name = strbuilder_new();
                    type_print_full_name(operand_field->DeclaringType, &type_name);
                    printf("%s::%U", strbuilder_get(&type_name), operand_field->Name);
                    strbuilder_free(&type_name);
                );

                // check we can access it
                CHECK(check_field_accessibility(method, operand_field));

                // make sure we initialized its type
                CHECK_AND_RETHROW(jit_prepare_static_type(ctx->ctx, operand_field->DeclaringType));
            } break;

            case OPCODE_OPERAND_InlineI: {
                operand_i32 = *(int32_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int32_t);

                JIT_TRACE(
                    printf("%d", operand_i32);
                );
            } break;

            case OPCODE_OPERAND_InlineI8: {
                operand_i64 = *(int64_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int64_t);

                JIT_TRACE(
                    printf("%ld", operand_i64);
                );
            } break;

            case OPCODE_OPERAND_InlineMethod: {
                // fetch it
                token_t value = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);

                // resolve it
                CHECK_AND_RETHROW(assembly_get_method_by_token(assembly, value, method->DeclaringType->GenericArguments,
                                                               method->GenericArguments, &operand_method));
                CHECK(operand_method != NULL);

                JIT_TRACE(
                    strbuilder_t type_name = strbuilder_new();
                    method_print_full_name(operand_method, &type_name);
                    printf("%s", strbuilder_get(&type_name));
                    strbuilder_free(&type_name);
                );

                // check we can access it
                CHECK(check_method_accessibility(method, operand_method),
                      "from %U.%U to %U.%U",
                      method->DeclaringType->Name, method->Name,
                      operand_method->DeclaringType->Name, operand_method->Name);

                // prepare the owning type
                CHECK_AND_RETHROW(jit_prepare_static_type(ctx->ctx, operand_method->DeclaringType));

                // if the method is a generic instance then we need to handle it separately
                CHECK_AND_RETHROW(jit_prepare_method(ctx->ctx, operand_method));
            } break;

            case OPCODE_OPERAND_InlineR: {
                operand_f64 = *(double*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(double);

                JIT_TRACE(
                    printf("<InlineR>");
                );
            } break;

            case OPCODE_OPERAND_InlineSig: CHECK_FAIL("TODO: sig support"); break;

            case OPCODE_OPERAND_InlineString: {
                token_t value = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);
                operand_string = assembly_get_string_by_token(assembly, value);
                CHECK(operand_string != NULL);

                JIT_TRACE(
                    printf("\"%U\"", operand_string);
                );
            } break;

            case OPCODE_OPERAND_InlineSwitch: {
                operand_switch_n = *(uint32_t*)&body->Il->Data[il_ptr];
                il_ptr += 4;
                operand_switch_dests = (int32_t*)&body->Il->Data[il_ptr];
                il_ptr += operand_switch_n * 4;

                JIT_TRACE(
                    printf("<InlineSwitch>");
                );
            } break;

            case OPCODE_OPERAND_InlineTok: {
                // fetch it
                operand_token = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);

                JIT_TRACE(
                    printf("<InlineTok>");
                );
            } break;

            case OPCODE_OPERAND_InlineType: {
                // fetch it
                token_t value = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);

                // resolve it
                CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, value, method->DeclaringType->GenericArguments
                        , method->GenericArguments, &operand_type));
                CHECK(operand_type != NULL);

                JIT_TRACE(
                    strbuilder_t type_name = strbuilder_new();
                    type_print_full_name(operand_type, &type_name);
                    printf("%s", strbuilder_get(&type_name));
                    strbuilder_free(&type_name);
                );

                // check it is visible
                CHECK(check_type_visibility(method, operand_type));

                // init it
                CHECK_AND_RETHROW(jit_prepare_static_type(ctx->ctx, operand_type));
            } break;

            case OPCODE_OPERAND_InlineVar: {
                operand_i32 = *(uint16_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(uint16_t);

                JIT_TRACE(
                    printf("%d", operand_i32);
                );
            } break;

            case OPCODE_OPERAND_ShortInlineBrTarget: {
                operand_i32 = *(int8_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int8_t);
                operand_i32 += il_ptr;

                JIT_TRACE(
                    printf("IL_%04x", operand_i32);
                );
            } break;

            case OPCODE_OPERAND_ShortInlineI: {
                operand_i32 = *(int8_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int8_t);

                JIT_TRACE(
                    printf("%d", operand_i32);
                );
            } break;

            case OPCODE_OPERAND_ShortInlineR: {
                operand_f32 = *(float*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(float);

                JIT_TRACE(
                    printf("<ShortInlineR>");
                );
            } break;

            case OPCODE_OPERAND_ShortInlineVar: {
                operand_i32 = *(uint8_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(uint8_t);

                JIT_TRACE(
                        printf("%d", operand_i32);
                );
            } break;

            case OPCODE_OPERAND_InlineNone:
                break;

            default:
                CHECK_FAIL();
        }

        JIT_TRACE(
            printf("\r\n");
        );

        //--------------------------------------------------------------------------------------------------------------
        // Handle the opcode
        //--------------------------------------------------------------------------------------------------------------

        // constrained prefix should appear
        // before callvirt opcode only
        if (ctx->constrainedType != NULL) {
            CHECK(opcode == CEE_CALLVIRT);
        }

        // after ldftn/ldvirtftn there must be a newobj
        if (ctx->ftnMethod != NULL) {
            CHECK(opcode == CEE_NEWOBJ);
        }

        switch (opcode) {

            //----------------------------------------------------------------------------------------------------------
            // Prefixes
            //----------------------------------------------------------------------------------------------------------

            // save the constrained type for use in the next instruction
            case CEE_CONSTRAINED: {
                ctx->constrainedType = operand_type;
            } break;

            case CEE_READONLY: {
                // TODO: this should turn into a controlled mutability by-ref, otherwise
                //       we can have a bad time
            } break;

            // ignore for now, tell MIR that this can be turned into a tailcall
            case CEE_TAILCALL: break;

            // ignored for now, tell MIR everything is aligned except instructions
            // like these
            case CEE_UNALIGNED: break;

            // ignored for now, tell MIR that this access is volatile
            case CEE_VOLATILE: break;

            //----------------------------------------------------------------------------------------------------------
            // Base Instructions
            //----------------------------------------------------------------------------------------------------------

            // nothing to do with nop
            case CEE_NOP: break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arithmetic
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // binary operations
            case CEE_ADD:       CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_ADD, false)); break;
            case CEE_DIV:       CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_DIV, false)); break;
            case CEE_DIV_UN:    CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_UDIV, true)); break;
            case CEE_MUL:       CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_MUL, false)); break;
            case CEE_REM:       CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_MOD, false)); break;
            case CEE_REM_UN:    CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_UMOD, true)); break;
            case CEE_SUB:       CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_SUB, false)); break;

            // bitwise binary operations
            case CEE_AND:       CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_AND, true)); break;
            case CEE_OR:        CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_OR, true)); break;
            case CEE_XOR:       CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_XOR, true)); break;

            // bit shifts
            case CEE_SHL:       CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_LSH, true)); break;
            case CEE_SHR:       CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_RSH, true)); break;
            case CEE_SHR_UN:    CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_URSH, true)); break;

            // overflow binary operations
            // TODO: just a stub for now
            case CEE_ADD_OVF:   CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_ADD, false)); break;
            case CEE_MUL_OVF:   CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_MUL, false)); break;

            // unary operations
            case CEE_NEG: {
                MIR_reg_t value_reg;
                System_Type value_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &value_type, &value_reg, NULL));

                MIR_reg_t result_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, value_type, &result_reg));

                MIR_insn_code_t code = 0;
                switch (type_get_stack_type(value_type)) {
                    case STACK_TYPE_INT32: {
                        code = MIR_NEGS;
                    } break;

                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR: {
                        code = MIR_NEG;
                    } break;

                    case STACK_TYPE_FLOAT: {
                        if (value_type == tSystem_Single) {
                            code = MIR_FNEG;
                        } else if (value_type == tSystem_Double) {
                            code = MIR_DNEG;
                        } else {
                            CHECK_FAIL();
                        }
                    } break;

                    case STACK_TYPE_O:
                    case STACK_TYPE_REF:
                    case STACK_TYPE_VALUE_TYPE:
                        CHECK_FAIL();
                }

                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, code,
                                             MIR_new_reg_op(mir_ctx, result_reg),
                                             MIR_new_reg_op(mir_ctx, value_reg)));
            } break;

            case CEE_NOT: {
                MIR_reg_t value_reg;
                System_Type value_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &value_type, &value_reg, NULL));

                MIR_reg_t result_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, value_type, &result_reg));

                MIR_insn_code_t code = 0;
                switch (type_get_stack_type(value_type)) {
                    case STACK_TYPE_INT32: {
                        code = MIR_XORS;
                    } break;

                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR: {
                        code = MIR_XOR;
                    } break;

                    case STACK_TYPE_FLOAT:
                    case STACK_TYPE_O:
                    case STACK_TYPE_REF:
                    case STACK_TYPE_VALUE_TYPE:
                        CHECK_FAIL();
                }

                // ~value == value ^ -1
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, code,
                                             MIR_new_reg_op(mir_ctx, result_reg),
                                             MIR_new_reg_op(mir_ctx, value_reg),
                                             MIR_new_int_op(mir_ctx, -1)));
            } break;

            // TODO: checked arithmetic

            case CEE_SIZEOF: {
                MIR_reg_t result_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_UInt32, &result_reg));

                // just push the size
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, result_reg),
                                             MIR_new_int_op(mir_ctx, operand_type->StackSize)));
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Casting and boxing
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_CONV_I1:
            case CEE_CONV_I2:
            case CEE_CONV_I4:
            case CEE_CONV_I8:
            case CEE_CONV_R4:
            case CEE_CONV_R8:
            case CEE_CONV_U1:
            case CEE_CONV_U2:
            case CEE_CONV_U4:
            case CEE_CONV_U8:
            case CEE_CONV_I:
            case CEE_CONV_U:
            case CEE_CONV_R_UN:
            case CEE_CONV_OVF_I:
            case CEE_CONV_OVF_U:
            case CEE_CONV_OVF_I1_UN:
            case CEE_CONV_OVF_I2_UN:
            case CEE_CONV_OVF_I4_UN:
            case CEE_CONV_OVF_I8_UN:
            case CEE_CONV_OVF_U1_UN:
            case CEE_CONV_OVF_U2_UN:
            case CEE_CONV_OVF_U4_UN:
            case CEE_CONV_OVF_U8_UN:
            case CEE_CONV_OVF_I_UN:
            case CEE_CONV_OVF_U_UN: {
                MIR_reg_t reg;
                System_Type type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &type, &reg, NULL));

                MIR_reg_t result_reg;
                System_Type result_type;
                switch (opcode) {
                    case CEE_CONV_OVF_I1_UN: case CEE_CONV_I1:
                    case CEE_CONV_OVF_U1_UN: case CEE_CONV_U1:
                    case CEE_CONV_OVF_I2_UN: case CEE_CONV_I2:
                    case CEE_CONV_OVF_U2_UN: case CEE_CONV_U2:
                    case CEE_CONV_OVF_I4_UN: case CEE_CONV_I4:
                    case CEE_CONV_OVF_U4_UN: case CEE_CONV_U4: result_type = tSystem_Int32; break;
                    case CEE_CONV_OVF_I8_UN: case CEE_CONV_I8:
                    case CEE_CONV_OVF_U8_UN: case CEE_CONV_U8: result_type = tSystem_Int64; break;
                    case CEE_CONV_OVF_I: case CEE_CONV_OVF_U:
                    case CEE_CONV_OVF_I_UN: case CEE_CONV_I:
                    case CEE_CONV_OVF_U_UN: case CEE_CONV_U: result_type = tSystem_IntPtr; break;
                    case CEE_CONV_R4: result_type = tSystem_Single; break;
                    case CEE_CONV_R8:
                    case CEE_CONV_R_UN: result_type = tSystem_Double; break;
                    default: CHECK_FAIL();
                }
                CHECK_AND_RETHROW(jit_stack_push(ctx, result_type, &result_reg));

                MIR_insn_code_t code = MIR_INVALID_INSN;
                switch (type_get_stack_type(type)) {
                    case STACK_TYPE_INT32: {
                        switch (opcode) {
                            case CEE_CONV_I1: code = MIR_EXT8; break;
                            case CEE_CONV_U1: code = MIR_UEXT8; break;
                            case CEE_CONV_I2: code = MIR_EXT16; break;
                            case CEE_CONV_U2: code = MIR_UEXT16; break;
                            case CEE_CONV_I4: code = MIR_MOV; break;
                            case CEE_CONV_U4: code = MIR_MOV; break;
                            case CEE_CONV_I8: code = MIR_EXT32; break;
                            case CEE_CONV_U8: code = MIR_UEXT32; break;
                            case CEE_CONV_I: code = MIR_EXT32; break;
                            case CEE_CONV_U: code = MIR_UEXT32; break;
                            case CEE_CONV_R4: code = MIR_I2F; break;
                            case CEE_CONV_R8: code = MIR_I2D; break;
                            case CEE_CONV_R_UN: code = MIR_UI2D; break;
                            default: CHECK_FAIL();
                        }
                    } break;

                    conv_intptr:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_INT64: {
                        switch (opcode) {
                            case CEE_CONV_I1: code = MIR_EXT8; break;
                            case CEE_CONV_U1: code = MIR_UEXT8; break;
                            case CEE_CONV_I2: code = MIR_EXT16; break;
                            case CEE_CONV_U2: code = MIR_UEXT16; break;
                            case CEE_CONV_I4: code = MIR_EXT32; break;
                            case CEE_CONV_U4: code = MIR_UEXT32; break;
                            case CEE_CONV_I8: code = MIR_MOV; break;
                            case CEE_CONV_U8: code = MIR_MOV; break;
                            case CEE_CONV_I: code = MIR_MOV; break;
                            case CEE_CONV_U: code = MIR_MOV; break;
                            case CEE_CONV_OVF_I: code = MIR_MOV; break; // no need for overflow checking, same size
                            case CEE_CONV_OVF_U: code = MIR_MOV; break; // no need for overflow checking, same size
                            case CEE_CONV_R4: code = MIR_I2F; break;
                            case CEE_CONV_R8: code = MIR_I2D; break;
                            case CEE_CONV_R_UN: code = MIR_UI2D; break;
                            case CEE_CONV_OVF_I_UN: code = MIR_MOV; break; // TODO: overflow checking
                            default: CHECK_FAIL();
                        }
                    } break;

                    case STACK_TYPE_FLOAT: {
                        if (type_get_stack_type(result_type) == STACK_TYPE_INT32) {
                            // we are converting from float to small
                            // type, first convert to native int and
                            // only then do a truncation
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, type == tSystem_Single ? MIR_F2I : MIR_D2I,
                                                         MIR_new_reg_op(mir_ctx, result_reg),
                                                         MIR_new_reg_op(mir_ctx, reg)));

                            // now our input is the result reg as well
                            reg = result_reg;
                        }
                        switch (opcode) {
                            case CEE_CONV_I1: code = MIR_EXT8; break;
                            case CEE_CONV_U1: code = MIR_UEXT8; break;
                            case CEE_CONV_I2: code = MIR_EXT16; break;
                            case CEE_CONV_U2: code = MIR_UEXT16; break;
                            case CEE_CONV_I4: code = MIR_EXT32; break;
                            case CEE_CONV_U4: code = MIR_UEXT32; break;
                            case CEE_CONV_I8: code = MIR_F2I; break;
                            case CEE_CONV_U8: code = MIR_F2I; break;
                            case CEE_CONV_I: code = MIR_F2I; break;
                            case CEE_CONV_U: code = MIR_F2I; break;
                            case CEE_CONV_R4: code = type == tSystem_Single ? MIR_FMOV : MIR_D2F; break;
                            case CEE_CONV_R8: code = type == tSystem_Single ? MIR_F2D : MIR_DMOV; break;
                            default: CHECK_FAIL();
                        }
                    } break;

                    case STACK_TYPE_O:
                    case STACK_TYPE_REF: {
                        CHECK(ctx->allow_pointers, "con.x reference to integer is not allowed");
                        goto conv_intptr;
                    } break;

                    case STACK_TYPE_VALUE_TYPE:
                        CHECK_FAIL();
                }

                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, code,
                                             MIR_new_reg_op(mir_ctx, result_reg),
                                             MIR_new_reg_op(mir_ctx, reg)));
            } break;

            // TODO: if at jit time we know it is true/false then
            //       we can remove the check, useful especially for
            //       generic instances
            case CEE_ISINST:
            case CEE_CASTCLASS:
            case CEE_UNBOX_ANY: {
                MIR_reg_t obj_reg;
                System_Type obj_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &obj_type, &obj_reg, NULL));

                // the object type must always be a ref type for unboxing
                CHECK(obj_type->StackType == STACK_TYPE_O);

                System_Type check_type = operand_type;
                if (opcode != CEE_UNBOX_ANY) {
                    // figure the result type, not needed for unbox
                    if (operand_type->IsValueType) {
                        // get it as a boxed type
                        operand_type = get_boxed_type(operand_type);
                    }
                }

                // push it, but now as the new type
                MIR_reg_t obj2_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, operand_type, &obj2_reg));

                // temp for the cast result
                MIR_reg_t cast_result_reg = jit_new_temp_reg(ctx, tSystem_Boolean);
                MIR_insn_t cast_success = MIR_new_label(mir_ctx);

                // unbox.any does not accept null
                if (opcode == CEE_UNBOX_ANY) {
                    CHECK_AND_RETHROW(jit_null_check(ctx, obj_reg, obj_type));
                }

                // if this is an interface get the type instance itself
                if (type_is_interface(obj_type)) {
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, obj_reg),
                                                 MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*),
                                                                obj_reg, 0, 1)));
                }

                // call the isinstance method to dynamically check the cast is valid
                if (type_is_interface(check_type)) {
                    // casting to an interface, use the dynamic_cast_obj_to_interface to do the cast
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_call_insn(mir_ctx, 6,
                                                      MIR_new_ref_op(mir_ctx, m_dynamic_cast_obj_to_interface_proto),
                                                      MIR_new_ref_op(mir_ctx, m_dynamic_cast_obj_to_interface_func),
                                                      MIR_new_reg_op(mir_ctx, cast_result_reg),
                                                      MIR_new_reg_op(mir_ctx, obj2_reg),
                                                      MIR_new_reg_op(mir_ctx, obj_reg),
                                                      MIR_new_ref_op(mir_ctx, check_type->MirType)));
                } else {
                    // casting to an object, so everything is fine
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_call_insn(mir_ctx, 5,
                                                      MIR_new_ref_op(mir_ctx, m_is_instance_proto),
                                                      MIR_new_ref_op(mir_ctx, m_is_instance_func),
                                                      MIR_new_reg_op(mir_ctx, cast_result_reg),
                                                      MIR_new_reg_op(mir_ctx, obj_reg),
                                                      MIR_new_ref_op(mir_ctx, check_type->MirType)));
                }

                // check that it was a success
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_BT,
                                             MIR_new_label_op(mir_ctx, cast_success),
                                             MIR_new_reg_op(mir_ctx, cast_result_reg)));

                // cast has failed
                if (opcode == CEE_ISINST) {
                    // for ISINST just return null, the dynamic cast already handles that
                    // case for interfaces
                    if (!type_is_interface(operand_type)) {
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, obj_reg),
                                                     MIR_new_int_op(mir_ctx, 0)));
                    }
                } else {
                    // for CLASSCAST throw an exception
                    CHECK(opcode == CEE_CASTCLASS || opcode == CEE_UNBOX_ANY);
                    CHECK_AND_RETHROW(jit_throw_new(ctx, tSystem_InvalidCastException));
                }

                MIR_append_insn(mir_ctx, mir_func, cast_success);

                switch (type_get_stack_type(operand_type)) {
                    case STACK_TYPE_O: {
                        // interfaces are handled by the dynamic cast object to interface function so there
                        // is nothing to do for them, for normal objects just move them to the obj2
                        if (!type_is_interface(operand_type)) {
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, MIR_MOV,
                                                         MIR_new_reg_op(mir_ctx, obj2_reg),
                                                         MIR_new_reg_op(mir_ctx, obj_reg)));
                        }
                    } break;

                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_FLOAT: {
                        // store the item in the type
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, jit_mov_insn_code(operand_type),
                                                     MIR_new_reg_op(mir_ctx, obj2_reg),
                                                     MIR_new_mem_op(mir_ctx, jit_get_mir_type(operand_type),
                                                                    tSystem_Object->ManagedSize, obj_reg, 0, 1)));

                    } break;

                    case STACK_TYPE_VALUE_TYPE: {
                        // memcpy it

                        // first get the base for memcpy
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_ADD,
                                                     MIR_new_reg_op(mir_ctx, obj_reg),
                                                     MIR_new_reg_op(mir_ctx, obj_reg),
                                                     MIR_new_int_op(mir_ctx, tSystem_Object->ManagedSize)));

                        // now emit the memcpy
                        jit_emit_memcpy(ctx, obj2_reg, obj_reg, operand_type->ManagedSize);
                    } break;

                        // already handled in the castclass case
                    case STACK_TYPE_REF:
                        CHECK_FAIL();
                }
            } break;

            case CEE_UNBOX: {
                System_Type obj_type;
                MIR_reg_t obj_reg;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &obj_type, &obj_reg, NULL));

                // must be a value type
                // TODO: this will require a bit more work
                CHECK(operand_type->IsValueType);
                CHECK(operand_type->GenericTypeDefinition != tSystem_Nullable);

                CHECK(type_get_stack_type(obj_type) == STACK_TYPE_O);

                // make sure obj is not null
                CHECK_AND_RETHROW(jit_null_check(ctx, obj_reg, obj_type));

                // push it, but now as the new type
                MIR_reg_t value_type_ptr_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, get_by_ref_type(operand_type), &value_type_ptr_reg));

                // temp for the cast result
                MIR_reg_t cast_result_reg = jit_new_temp_reg(ctx, tSystem_Boolean);
                MIR_insn_t cast_success = MIR_new_label(mir_ctx);

                // if this is an interface get the object instance itself
                if (type_is_interface(obj_type)) {
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, obj_reg),
                                                 MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*),
                                                                obj_reg, 0, 1)));
                }

                // check the instance of properly
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_call_insn(mir_ctx, 5,
                                                  MIR_new_ref_op(mir_ctx, m_is_instance_proto),
                                                  MIR_new_ref_op(mir_ctx, m_is_instance_func),
                                                  MIR_new_reg_op(mir_ctx, cast_result_reg),
                                                  MIR_new_reg_op(mir_ctx, obj_reg),
                                                  MIR_new_ref_op(mir_ctx, operand_type->MirType)));

                // check that it was a success
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_BT,
                                             MIR_new_label_op(mir_ctx, cast_success),
                                             MIR_new_reg_op(mir_ctx, cast_result_reg)));

                // no, throw an exception
                CHECK_AND_RETHROW(jit_throw_new(ctx, tSystem_InvalidCastException));

                // yes, return the pointer
                MIR_append_insn(mir_ctx, mir_func, cast_success);
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_ADD,
                                             MIR_new_reg_op(mir_ctx, value_type_ptr_reg),
                                             MIR_new_reg_op(mir_ctx, obj_reg),
                                             MIR_new_int_op(mir_ctx, tSystem_Object->ManagedSize)));


            } break;

            case CEE_BOX: {
                System_Type val_type;
                MIR_reg_t val_reg;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &val_type, &val_reg, NULL));

                // make sure that this is fine
                CHECK(type_is_verifier_assignable_to(val_type, operand_type));

                // Only actually do a boxed type if it is boxable
                System_Type boxed_type;
                if (type_is_value_type(val_type)) {
                    boxed_type = get_boxed_type(val_type);
                } else {
                    boxed_type = val_type;
                }

                // we track this as an object now
                MIR_reg_t obj_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, boxed_type, &obj_reg));

                MIR_insn_t has_null_value = NULL;

                // check if we need to allocate memory for this
                if (operand_type->IsValueType) {

                    // if this is a nullable we need to check the HasValue field before
                    // we actually try and box the value, since otherwise we return null
                    if (operand_type->GenericTypeDefinition == tSystem_Nullable) {
                        has_null_value = MIR_new_label(mir_ctx);

                        // zero out the obj_reg, so we can just skip the allocation if its null
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, obj_reg),
                                                     MIR_new_int_op(mir_ctx, 0)));

                        // check if the HasValue is zero, if so then jump over
                        // the allocation and the value copy
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_BF,
                                                     MIR_new_label_op(mir_ctx, has_null_value),
                                                     MIR_new_mem_op(mir_ctx, MIR_T_I8,
                                                                    offsetof(System_Nullable, HasValue),
                                                                    val_reg, 0, 1)));
                    }

                    // allocate it
                    CHECK_AND_RETHROW(jit_new(ctx, obj_reg, operand_type,
                                              MIR_new_int_op(mir_ctx, tSystem_Object->ManagedSize + val_type->ManagedSize)));
                }

                // must be a value type
                switch (type_get_stack_type(operand_type)) {
                    case STACK_TYPE_O: {
                        // return unchanged
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, obj_reg),
                                                     MIR_new_reg_op(mir_ctx, val_reg)));
                    } break;

                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_FLOAT: {
                        // store the item in the type
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, jit_mov_insn_code(operand_type),
                                                     MIR_new_mem_op(mir_ctx, jit_get_mir_type(operand_type),
                                                                    tSystem_Object->ManagedSize, obj_reg, 0, 1),
                                                     MIR_new_reg_op(mir_ctx, val_reg)));

                    } break;

                    case STACK_TYPE_VALUE_TYPE: {
                        // memcpy it
                        MIR_reg_t memcpy_base_reg = jit_new_temp_reg(ctx, tSystem_Object);

                        // first get the base for memcpy
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_ADD,
                                                     MIR_new_reg_op(mir_ctx, memcpy_base_reg),
                                                     MIR_new_reg_op(mir_ctx, obj_reg),
                                                     MIR_new_int_op(mir_ctx, tSystem_Object->ManagedSize)));

                        int offset = -1;
                        if (has_null_value != NULL) {
                            // find the offset of the Value field inside the Nullable instance
                            for (int i = 0; i < val_type->Fields->Length; i++) {
                                System_Reflection_FieldInfo field = val_type->Fields->Data[i];
                                if (string_equals_cstr(field->Name, "_value")) {
                                    offset = field->MemoryOffset;
                                }
                            }
                            CHECK(offset != -1);

                            // this is a nullable, we need to skip the first
                            // element when copying over
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, MIR_ADD,
                                                         MIR_new_reg_op(mir_ctx, val_reg),
                                                         MIR_new_reg_op(mir_ctx, val_reg),
                                                         MIR_new_int_op(mir_ctx, offset)));
                        } else {
                            offset = 0;
                        }

                        // now emit the memcpy
                        jit_emit_memcpy(ctx, memcpy_base_reg, val_reg, operand_type->ManagedSize - offset);
                    } break;

                    case STACK_TYPE_REF:
                        CHECK_FAIL();
                }

                if (has_null_value != NULL) {
                    // we had a null value, went to here
                    MIR_append_insn(mir_ctx, mir_func, has_null_value);
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Indirect reference access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDIND_I1: operand_type = tSystem_SByte; goto cee_ldind;
            case CEE_LDIND_U1: operand_type = tSystem_Byte; goto cee_ldind;
            case CEE_LDIND_I2: operand_type = tSystem_Int16; goto cee_ldind;
            case CEE_LDIND_U2: operand_type = tSystem_UInt16; goto cee_ldind;
            case CEE_LDIND_I4: operand_type = tSystem_Int32; goto cee_ldind;
            case CEE_LDIND_U4: operand_type = tSystem_UInt32; goto cee_ldind;
            case CEE_LDIND_I8: operand_type = tSystem_Int64; goto cee_ldind;
            case CEE_LDIND_I: operand_type = tSystem_IntPtr; goto cee_ldind;
            case CEE_LDIND_R4: operand_type = tSystem_Single; goto cee_ldind;
            case CEE_LDIND_R8: operand_type = tSystem_Double; goto cee_ldind;
            case CEE_LDIND_REF: operand_type = NULL; goto cee_ldind;
            case CEE_LDOBJ: goto cee_ldind;
            cee_ldind: {
                // pop all the values from the stack
                MIR_reg_t addr_reg;
                System_Type addr_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &addr_type, &addr_reg, NULL));

                // this must be an reference
                MIR_reg_t value_reg;
                if (addr_type->IsByRef) {
                    // for anything which is not ldelem.ref we know the operand_type
                    // from the reference
                    if (operand_type != NULL) {
                        CHECK(type_is_verifier_assignable_to(addr_type->BaseType, operand_type));
                        CHECK_AND_RETHROW(jit_stack_push(ctx, type_get_intermediate_type(operand_type), &value_reg));
                    } else {
                        // the type is gotten from the reference
                        operand_type = addr_type->BaseType;
                        CHECK_AND_RETHROW(jit_stack_push(ctx, type_get_verification_type(operand_type), &value_reg));
                    }
                } else {
                    // make sure we are trying to access via a pointer, note that
                    // only the corelib can use pointers
                    CHECK(addr_type == tSystem_IntPtr);
                    CHECK(ctx->allow_pointers, "ldind/ldobj from pointer is not allowed");
                    CHECK_AND_RETHROW(jit_stack_push(ctx, type_get_intermediate_type(operand_type), &value_reg));
                }

                switch (type_get_stack_type(operand_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(operand_type)) {
                            goto ldind_value_type;
                        } else {
                            goto ldind_primitive_type;
                        }
                    } break;

                    ldind_primitive_type:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT: {
                        // we need to extend this properly if the field is smaller
                        // than an int32 (because we are going to load into an int32
                        // essentially)
                        MIR_insn_code_t code = MIR_MOV;
                        if (operand_type == tSystem_SByte || operand_type == tSystem_Boolean) {
                            code = MIR_EXT8;
                        } else if (operand_type == tSystem_Byte) {
                            code = MIR_UEXT8;
                        } else if (operand_type == tSystem_Int16) {
                            code = MIR_EXT16;
                        } else if (operand_type == tSystem_UInt16 || operand_type == tSystem_Char) {
                            code = MIR_UEXT16;
                        } else if (operand_type == tSystem_Single) {
                            code = MIR_FMOV;
                        } else if (operand_type == tSystem_Double) {
                            code = MIR_DMOV;
                        }

                        // we can copy this in a single mov
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_mem_op(mir_ctx, jit_get_mir_type(operand_type),
                                                                    0, addr_reg, 0, 1)));
                    } break;

                    ldind_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        jit_emit_memcpy(ctx, value_reg, addr_reg, operand_type->StackSize);
                    } break;

                    case STACK_TYPE_REF:
                        CHECK_FAIL();
                }
            } break;

            case CEE_STIND_REF: operand_type = NULL; goto cee_stind;
            case CEE_STIND_I1: operand_type = tSystem_SByte; goto cee_stind;
            case CEE_STIND_I2: operand_type = tSystem_Int16; goto cee_stind;
            case CEE_STIND_I4: operand_type = tSystem_Int32; goto cee_stind;
            case CEE_STIND_I8: operand_type = tSystem_Int64; goto cee_stind;
            case CEE_STIND_R4: operand_type = tSystem_Single; goto cee_stind;
            case CEE_STIND_R8: operand_type = tSystem_Double; goto cee_stind;
            case CEE_STIND_I: operand_type = tSystem_IntPtr; goto cee_stind;
            case CEE_STOBJ: goto cee_stind;
            cee_stind: {
                // pop all the values from the stack
                stack_entry_t addr_entry;
                MIR_reg_t value_reg;
                MIR_reg_t addr_reg;
                System_Type value_type;
                System_Type addr_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &value_type, &value_reg, NULL));
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &addr_type, &addr_reg, &addr_entry));

                if (addr_type->IsByRef) {
                    CHECK(!addr_entry.readonly_ref);

                    // for stind.ref the operand type is the same as the
                    // byref itself
                    if (operand_type == NULL) {
                        operand_type = addr_type->BaseType;
                    }

                    // validate all the type stuff
                    CHECK(type_is_verifier_assignable_to(value_type, operand_type));
                } else {
                    CHECK(addr_type == tSystem_IntPtr);
                    CHECK(ctx->allow_pointers, "stind/stobj to pointer is not allowed");
                    CHECK(operand_type != NULL);
                }

                switch (type_get_stack_type(value_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(operand_type)) {
                            if (type_is_interface(value_type)) {
                                // interface -> interface
                                goto stind_value_type;
                            } else {
                                // object -> interface

                                // from an object, cast required, need a write barrier
                                CHECK_AND_RETHROW(jit_cast_obj_to_interface(ctx,
                                                                            addr_reg, value_reg,
                                                                            value_type, operand_type));
                            }
                        } else {
                            // check if we need to cast to an object from an interface
                            if (type_is_interface(value_type)) {
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_MOV,
                                                             MIR_new_reg_op(mir_ctx, value_reg),
                                                             MIR_new_mem_op(mir_ctx, MIR_T_P,
                                                                            sizeof(void*), value_reg,
                                                                            0, 1)));
                            }

                            // storing to an object from an object
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_call_insn(mir_ctx, 4,
                                                              MIR_new_ref_op(mir_ctx, m_gc_update_ref_proto),
                                                              MIR_new_ref_op(mir_ctx, m_gc_update_ref_func),
                                                              MIR_new_reg_op(mir_ctx, addr_reg),
                                                              MIR_new_reg_op(mir_ctx, value_reg)));
                        }
                    } break;

                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT: {
                        MIR_insn_code_t code = jit_number_cast_inscode(value_type, operand_type);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_mem_op(mir_ctx, jit_get_mir_type(operand_type),
                                                                    0, addr_reg, 0, 1),
                                                     MIR_new_reg_op(mir_ctx, value_reg)));
                    } break;

                    stind_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        // copying into a managed pointer, use the managed ref memcpy
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_call_insn(mir_ctx, 5,
                                                          MIR_new_ref_op(mir_ctx, m_managed_ref_memcpy_proto),
                                                          MIR_new_ref_op(mir_ctx, m_managed_ref_memcpy_func),
                                                          MIR_new_reg_op(mir_ctx, addr_reg),
                                                          MIR_new_ref_op(mir_ctx, operand_type->MirType),
                                                          MIR_new_reg_op(mir_ctx, value_reg)));
                    } break;

                    case STACK_TYPE_REF:
                        CHECK_FAIL("wtf");
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Comparison and branching
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_CEQ: CHECK_AND_RETHROW(jit_compare_branch(ctx, -1, MIR_EQ)); break;
            case CEE_CGT: CHECK_AND_RETHROW(jit_compare_branch(ctx, -1, MIR_GT)); break;
            case CEE_CGT_UN: CHECK_AND_RETHROW(jit_compare_branch(ctx, -1, MIR_UGT)); break;
            case CEE_CLT: CHECK_AND_RETHROW(jit_compare_branch(ctx, -1, MIR_LT)); break;
            case CEE_CLT_UN: CHECK_AND_RETHROW(jit_compare_branch(ctx, -1, MIR_ULT)); break;

            // unconditional branch
            case CEE_BR:
            case CEE_BR_S: {
                // get the label
                MIR_label_t label;
                CHECK_AND_RETHROW(jit_branch_point(ctx, operand_i32, &label));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_JMP,
                                             MIR_new_label_op(mir_ctx, label)));
            } break;

            // branch false/true
            case CEE_BRFALSE:
            case CEE_BRFALSE_S:
            case CEE_BRTRUE:
            case CEE_BRTRUE_S: {
                // get the value
                MIR_reg_t value_reg;
                System_Type value_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &value_type, &value_reg, NULL));

                // get the label
                MIR_label_t label;
                CHECK_AND_RETHROW(jit_branch_point(ctx, operand_i32, &label));

                // emit it properly
                MIR_insn_code_t code;
                if (opcode == CEE_BRFALSE || opcode == CEE_BRFALSE_S) {
                    code = MIR_BF;
                } else {
                    code = MIR_BT;
                }

                switch (type_get_stack_type(value_type)) {
                    // for 32bit we want the 32bit op
                    case STACK_TYPE_INT32:
                        code += 1;

                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_REF:
                    case STACK_TYPE_O: {
                        if (type_is_interface(value_type)) {
                            // this is an interface, we need to get the
                            // object pointer
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, MIR_MOV,
                                                         MIR_new_reg_op(mir_ctx, value_reg),
                                                         MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), value_reg, 0, 1)));
                        }

                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_label_op(mir_ctx, label),
                                                     MIR_new_reg_op(mir_ctx, value_reg)));
                    } break;

                    case STACK_TYPE_VALUE_TYPE:
                    case STACK_TYPE_FLOAT:
                        CHECK_FAIL();
                }
            } break;

            // all the different compare and branch instructions
            case CEE_BEQ:
            case CEE_BEQ_S: CHECK_AND_RETHROW(jit_compare_branch(ctx, operand_i32, MIR_BEQ)); break;
            case CEE_BGE:
            case CEE_BGE_S: CHECK_AND_RETHROW(jit_compare_branch(ctx, operand_i32, MIR_BGE)); break;
            case CEE_BGT:
            case CEE_BGT_S: CHECK_AND_RETHROW(jit_compare_branch(ctx, operand_i32, MIR_BGT)); break;
            case CEE_BLE:
            case CEE_BLE_S: CHECK_AND_RETHROW(jit_compare_branch(ctx, operand_i32, MIR_BLE)); break;
            case CEE_BLT:
            case CEE_BLT_S: CHECK_AND_RETHROW(jit_compare_branch(ctx, operand_i32, MIR_BLT)); break;
            case CEE_BNE_UN:
            case CEE_BNE_UN_S: CHECK_AND_RETHROW(jit_compare_branch(ctx, operand_i32, MIR_BNE)); break;
            case CEE_BGE_UN:
            case CEE_BGE_UN_S: CHECK_AND_RETHROW(jit_compare_branch(ctx, operand_i32, MIR_UBGE)); break;
            case CEE_BGT_UN:
            case CEE_BGT_UN_S: CHECK_AND_RETHROW(jit_compare_branch(ctx, operand_i32, MIR_UBGT)); break;
            case CEE_BLE_UN:
            case CEE_BLE_UN_S: CHECK_AND_RETHROW(jit_compare_branch(ctx, operand_i32, MIR_UBLE)); break;
            case CEE_BLT_UN:
            case CEE_BLT_UN_S: CHECK_AND_RETHROW(jit_compare_branch(ctx, operand_i32, MIR_UBLT)); break;

            // switch
            case CEE_SWITCH: {
                MIR_reg_t value_reg;
                System_Type value_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &value_type, &value_reg, NULL));

                // allocate enough space for the ops
                switch_ops = realloc(switch_ops, (operand_switch_n + 1) * sizeof(MIR_op_t));

                // branch selector
                switch_ops[0] = MIR_new_reg_op(mir_ctx, value_reg);

                // all the locations
                for (int i = 0; i < operand_switch_n; i++) {
                    MIR_label_t label;
                    CHECK_AND_RETHROW(jit_branch_point(ctx, il_ptr + operand_switch_dests[i], &label));
                    switch_ops[i + 1] = MIR_new_label_op(mir_ctx, label);
                }

                // setup the not taken label
                MIR_label_t not_taken = MIR_new_label(mir_ctx);

                // if the value is invalid then don't take the route and go to the default case
                // case 1: it's past the end
                // case 2: it's before the start (which is 0)
                // as operand_switch_n is positive signed, when treated as unsigned it is less than all negative signed numbers
                // hence you can use a single branch to handle both cases
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_UBGE,
                                             MIR_new_label_op(mir_ctx, not_taken),
                                             MIR_new_reg_op(mir_ctx, value_reg),
                                             MIR_new_int_op(mir_ctx, operand_switch_n)));

                // do the switch itself
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn_arr(mir_ctx, MIR_SWITCH,
                                                 operand_switch_n + 1, switch_ops));

                MIR_append_insn(mir_ctx, mir_func, not_taken);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Exception control flow
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LEAVE:
            case CEE_LEAVE_S: CHECK_AND_RETHROW(jit_emit_leave(ctx)); break;
            case CEE_ENDFINALLY: CHECK_AND_RETHROW(jit_emit_endfinally(ctx)); break;
            case CEE_ENDFILTER: CHECK_AND_RETHROW(jit_emit_endfilter(ctx)); break;
            case CEE_THROW: CHECK_AND_RETHROW(jit_emit_throw(ctx)); break;
            case CEE_RETHROW: CHECK_AND_RETHROW(jit_emit_rethrow(ctx)); break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Variables
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STLOC_0:
            case CEE_STLOC_1:
            case CEE_STLOC_2:
            case CEE_STLOC_3: operand_i32 = opcode - CEE_STLOC_0;
            case CEE_STLOC_S:
            case CEE_STLOC: {
                // get the top value
                stack_entry_t value_entry;
                MIR_reg_t value_reg;
                System_Type value_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &value_type, &value_reg, &value_entry));

                // get the variable
                CHECK(operand_i32 < body->LocalVariables->Length);
                System_Reflection_LocalVariableInfo variable = body->LocalVariables->Data[operand_i32];
                System_Type variable_type = type_get_intermediate_type(variable->LocalType);

                // check the type is valid
                if (value_type == tSystem_IntPtr && variable_type->IsByRef) {
                    // trying to store pointer to a ref field, stop that
                    CHECK(ctx->allow_pointers, "stloc from native int to byref is not allowed");
                } else {
                    CHECK(type_is_verifier_assignable_to(value_type, variable_type));
                }

                switch (type_get_stack_type(value_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(variable_type)) {
                            if (type_is_interface(value_type)) {
                                // interface -> interface
                                goto stloc_value_type;
                            } else {
                                // object -> interface
                                CHECK(locals[operand_i32].op.mode == MIR_OP_REG);
                                CHECK_AND_RETHROW(jit_cast_obj_to_interface(ctx,
                                                                            locals[operand_i32].op.u.reg, value_reg,
                                                                            value_type, variable_type));
                            }
                        } else {
                            if (type_is_interface(value_type)) {
                                // interface -> object
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_MOV,
                                                             locals[operand_i32].op,
                                                             MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), value_reg, 0, 1)));
                            } else {
                                // object -> object
                                goto stloc_primitive_type;
                            }
                        }
                    } break;

                    stloc_primitive_type:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT: {
                        MIR_insn_code_t code = jit_number_cast_inscode(value_type, variable_type);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     locals[operand_i32].op,
                                                     MIR_new_reg_op(mir_ctx, value_reg)));
                    } break;

                    stloc_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        CHECK(locals[operand_i32].op.mode == MIR_OP_REG);
                        jit_emit_memcpy(ctx, locals[operand_i32].op.u.reg, value_reg, value_type->StackSize);
                    } break;

                    // continue tracking of references
                    case STACK_TYPE_REF: {
                        locals[operand_i32].non_local_ref = value_entry.non_local_ref;
                        locals[operand_i32].readonly_ref = value_entry.readonly_ref;
                        goto stloc_primitive_type;
                    } break;
                }
            } break;

            case CEE_LDLOC_0:
            case CEE_LDLOC_1:
            case CEE_LDLOC_2:
            case CEE_LDLOC_3: operand_i32 = opcode - CEE_LDLOC_0;
            case CEE_LDLOC_S:
            case CEE_LDLOC: {
                // get the variable
                CHECK(operand_i32 < body->LocalVariables->Length);
                System_Reflection_LocalVariableInfo variable = body->LocalVariables->Data[operand_i32];
                System_Type value_type = type_get_intermediate_type(variable->LocalType);

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, value_type, &value_reg));

                switch (type_get_stack_type(value_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(value_type)) {
                            // interface type, copy it
                            goto ldloc_value_type;
                        } else {
                            // primitive type, move it
                            goto ldloc_primitive_type;
                        }
                    } break;

                    ldloc_primitive_type:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT: {
                        MIR_insn_code_t code = jit_mov_insn_code(value_type);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     locals[operand_i32].op));
                    } break;

                    ldloc_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        CHECK(locals[operand_i32].op.mode == MIR_OP_REG);
                        jit_emit_memcpy(ctx, value_reg, locals[operand_i32].op.u.reg, value_type->StackSize);
                    } break;

                    // restore tracking of non-local ref and readonly ref
                    case STACK_TYPE_REF: {
                        // TODO: I think this is actually not the most correct thing, what if someone does:
                        //          <push a ref that can be written to>
                        //          stloc.0
                        //       label:
                        //          ldloc.0
                        //          <write to it>
                        //          <push reference that can't be written to>
                        //          stloc.0
                        //          br label
                        //      we now wrote to a readonly reference... but idk if there is a simple way to
                        //      handle this without more restrict stuff, maybe we can enforce so variables that
                        //      store a readonly ref can't have any other ref, but then what if there is valid
                        //      CIL generated that does that?
                        //      we will need to think about this some more but for now this is good enough
                        STACK_TOP.readonly_ref = locals[operand_i32].readonly_ref;
                        STACK_TOP.non_local_ref = locals[operand_i32].non_local_ref;
                        goto ldloc_primitive_type;
                    } break;

                }
            } break;

            case CEE_LDLOCA:
            case CEE_LDLOCA_S: {
                // get the variable
                CHECK(operand_i32 < body->LocalVariables->Length);
                System_Reflection_LocalVariableInfo variable = body->LocalVariables->Data[operand_i32];
                System_Type value_type = get_by_ref_type(type_get_verification_type(variable->LocalType));

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, value_type, &value_reg));

                switch (type_get_stack_type(variable->LocalType)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(variable->LocalType)) {
                            goto ldloca_value_type;
                        } else {
                            goto ldloca_primitive_type;
                        }
                    } break;

                    ldloca_primitive_type:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT: {
                        CHECK(locals[operand_i32].op.mode == MIR_OP_MEM);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_reg_op(mir_ctx, locals[operand_i32].op.u.mem.base)));
                    } break;

                    ldloca_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     locals[operand_i32].op));
                    } break;

                    // idk if this is possible tbh
                    case STACK_TYPE_REF:
                        CHECK_FAIL();
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arguments
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STARG_S:
            case CEE_STARG: {
                // get the top value
                MIR_reg_t value_reg;
                System_Type value_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &value_type, &value_reg, NULL));

                // check the length is fine
                CHECK(operand_i32 < arrlen(arguments));

                // resolve the type
                MIR_op_t arg_op = arguments[operand_i32];
                System_Type arg_type = NULL;
                if (!method_is_static(method)) {
                    if (operand_i32 == 0) {
                        arg_type = method->DeclaringType;
                        if (arg_type->IsValueType) {
                            // TODO: readonly this

                            // value types turn into a by-ref when using this
                            arg_type = get_by_ref_type(arg_type);
                        }
                    }
                    operand_i32--;
                }

                if (arg_type == NULL) {
                    CHECK(operand_i32 < method->Parameters->Length);
                    System_Reflection_ParameterInfo parameter = method->Parameters->Data[operand_i32];
                    arg_type = parameter->ParameterType;
                    CHECK(!parameter_is_in(parameter));
                }

                // check the type is valid
                CHECK(type_is_verifier_assignable_to(value_type, arg_type));

                switch (type_get_stack_type(value_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(arg_type)) {
                            if (type_is_interface(value_type)) {
                                // interface -> interface
                                goto starg_value_type;
                            } else {
                                // object -> interface
                                CHECK(arg_op.mode == MIR_OP_REG);
                                CHECK_AND_RETHROW(jit_cast_obj_to_interface(ctx,
                                                                            arg_op.u.reg, value_reg,
                                                                            value_type, arg_type));
                            }
                        } else {
                            if (type_is_interface(value_type)) {
                                // interface -> object
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_MOV,
                                                             arg_op,
                                                             MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), value_reg, 0, 1)));
                            } else {
                                // object -> object
                                goto starg_primitive_type;
                            }
                        }
                    } break;

                    starg_primitive_type:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT:
                    case STACK_TYPE_REF: {
                        MIR_insn_code_t code = jit_number_cast_inscode(value_type, arg_type);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     arg_op,
                                                     MIR_new_reg_op(mir_ctx, value_reg)));
                    } break;

                    starg_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        CHECK(arg_op.mode == MIR_OP_REG);
                        jit_emit_memcpy(ctx, arg_op.u.reg, value_reg, value_type->StackSize);
                    } break;
                }
            } break;

            case CEE_LDARG_0:
            case CEE_LDARG_1:
            case CEE_LDARG_2:
            case CEE_LDARG_3: operand_i32 = opcode - CEE_LDARG_0;
            case CEE_LDARG_S:
            case CEE_LDARG: {
                bool readonly_ref = false;
                bool is_this = false;

                // check the length is fine
                CHECK(operand_i32 < arrlen(arguments));

                // resolve the type
                MIR_op_t arg_op = arguments[operand_i32];
                System_Type arg_type = NULL;
                if (!method_is_static(method)) {
                    if (operand_i32 == 0) {
                        is_this = true;
                        arg_type = method->DeclaringType;
                        if (arg_type->IsValueType) {
                            // TODO: readonly this

                            // value types turn into a by-ref when using this
                            arg_type = get_by_ref_type(arg_type);
                        }
                    }
                    operand_i32--;
                }

                if (arg_type == NULL) {
                    System_Reflection_ParameterInfo parameter = method->Parameters->Data[operand_i32];
                    arg_type = parameter->ParameterType;
                    readonly_ref = parameter_is_in(parameter);
                }

                // Get the stack type of the arg
                System_Type arg_stack_type = type_get_intermediate_type(arg_type);

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, arg_stack_type, &value_reg));

                // mark if the value is this
                STACK_TOP.this = is_this;

                switch (type_get_stack_type(arg_stack_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(arg_stack_type)) {
                            // interface type, copy it
                            goto ldarg_value_type;
                        } else {
                            // primitive type, move it
                            goto ldarg_primitive_type;
                        }
                    } break;

                    ldarg_primitive_type:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT: {
                        MIR_insn_code_t code = jit_mov_insn_code(arg_stack_type);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     arg_op));
                    } break;

                    ldarg_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        CHECK(arg_op.mode == MIR_OP_REG);
                        jit_emit_memcpy(ctx, value_reg, arg_op.u.reg, arg_stack_type->StackSize);
                    } break;

                    case STACK_TYPE_REF:
                        // mark that this is a non-local ref type, as it comes from the outside
                        // and depending on `in` it might be a readonly ref
                        STACK_TOP.non_local_ref = true;
                        STACK_TOP.readonly_ref = readonly_ref;
                        goto ldarg_primitive_type;
                }
            } break;

            case CEE_LDARGA:
            case CEE_LDARGA_S: {
                // check the length is fine
                CHECK(operand_i32 < arrlen(arguments));

                // resolve the type
                int arg_index = operand_i32;
                MIR_op_t arg_op = arguments[operand_i32];
                System_Type arg_type = NULL;
                if (!method_is_static(method)) {
                    if (operand_i32 == 0) {
                        arg_type = method->DeclaringType;
                        if (arg_type->IsValueType) {
                            // value types turn into a by-ref when using this
                            arg_type = get_by_ref_type(arg_type);
                        }
                    }
                    operand_i32--;
                }


                if (arg_type == NULL) {
                    CHECK(operand_i32 < method->Parameters->Length);
                    arg_type = method->Parameters->Data[operand_i32]->ParameterType;
                }

                // Get the value type
                System_Type value_type = get_by_ref_type(type_get_verification_type(arg_type));

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, value_type, &value_reg));

                switch (type_get_stack_type(arg_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(arg_type)) {
                            // interface type, copy it
                            goto ldarga_value_type;
                        } else {
                            // primitive type, move it
                            goto ldarga_primitive_type;
                        }
                    } break;

                    ldarga_primitive_type:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT: {
                        // check if we need to spill the reg or if its already spilled
                        if (arg_op.mode == MIR_OP_REG) {
                            // allocate space for the argument to be stored
                            char spill_arg_name[64];
                            snprintf(spill_arg_name, sizeof(spill_arg_name), "sarg%d", arg_index);

                            // setup space for the argument
                            // NOTE: this is switched because we are prepending
                            MIR_reg_t stack_arg_reg = MIR_new_func_reg(mir_ctx, mir_func->u.func, MIR_T_I64, spill_arg_name);

                            // setup the new op
                            MIR_op_t old_arg_op = arg_op;
                            MIR_reg_t old_arg_reg = old_arg_op.u.reg;
                            arg_op = MIR_new_mem_op(mir_ctx, jit_get_mir_type(arg_type), 0, stack_arg_reg, 0, 1);

                            // replace the old register access with the new operand
                            // NOTE: this must be done before the storage allocation otherwise it will also
                            //       replace the register for the storage allocation
                            CHECK_AND_RETHROW(jit_replace_reg_with_op(mir_func->u.func, old_arg_reg, arg_op));

                            // move the value to the stack storage
                            MIR_prepend_insn(mir_ctx, mir_func,
                                             MIR_new_insn(mir_ctx, jit_mov_insn_code(arg_type),
                                                          arg_op,
                                                          old_arg_op));

                            // allocate the storage
                            MIR_prepend_insn(mir_ctx, mir_func,
                                             MIR_new_insn(mir_ctx, MIR_ALLOCA,
                                                          MIR_new_reg_op(mir_ctx, stack_arg_reg),
                                                          MIR_new_int_op(mir_ctx, arg_type->StackSize)));

                            // set the new op for future uses
                            arguments[arg_index] = arg_op;
                        }

                        // already spilled, just move the pointer in the memory reference
                        CHECK(arg_op.mode == MIR_OP_MEM);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, jit_mov_insn_code(arg_type),
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_reg_op(mir_ctx, arg_op.u.mem.base)));
                    } break;

                    ldarga_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        // just move the pointer in the register
                        CHECK(arg_op.mode == MIR_OP_REG);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_reg_op(mir_ctx, arg_op.u.reg)));
                    } break;

                    // can't do a ref by ref
                    case STACK_TYPE_REF:
                        CHECK_FAIL();
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Basic stack manipulation
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDC_I4_M1:
            case CEE_LDC_I4_0:
            case CEE_LDC_I4_1:
            case CEE_LDC_I4_2:
            case CEE_LDC_I4_3:
            case CEE_LDC_I4_4:
            case CEE_LDC_I4_5:
            case CEE_LDC_I4_6:
            case CEE_LDC_I4_7:
            case CEE_LDC_I4_8: operand_i32 = (int32_t)opcode - CEE_LDC_I4_0;
            case CEE_LDC_I4_S:
            case CEE_LDC_I4: {
                MIR_reg_t sr;
                CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_Int32, &sr));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, sr),
                                             MIR_new_int_op(mir_ctx, operand_i32)));
            } break;

            case CEE_LDC_I8: {
                MIR_reg_t reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_Int64, &reg));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, reg),
                                             MIR_new_int_op(mir_ctx, operand_i64)));
            } break;

            case CEE_LDC_R4: {
                MIR_reg_t reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_Single, &reg));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_FMOV,
                                             MIR_new_reg_op(mir_ctx, reg),
                                             MIR_new_float_op(mir_ctx, operand_f32)));
            } break;

            case CEE_LDC_R8: {
                MIR_reg_t reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_Double, &reg));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_DMOV,
                                             MIR_new_reg_op(mir_ctx, reg),
                                             MIR_new_double_op(mir_ctx, operand_f64)));
            } break;

            case CEE_LDSTR: {
                // push a string type
                MIR_reg_t string_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_String, &string_reg));

                // move it to the register
                // TODO: better way to do this?
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, string_reg),
                                             MIR_new_uint_op(mir_ctx, (uintptr_t)operand_string)));
            } break;

            case CEE_LDNULL: {
                // push a null type
                MIR_reg_t null_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, NULL, &null_reg));

                // load a null value
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, null_reg),
                                             MIR_new_int_op(mir_ctx, 0)));
            } break;

            case CEE_LDTOKEN: {
                // all of them are about the same, they have a single pointer of this
                MIR_item_t runtime_handle_item = NULL;
                MIR_reg_t runtime_handle_reg = 0;

                // first resolve what we are going to even be pushing
                switch (operand_token.table) {
                    case METADATA_METHOD_DEF:
                    case METADATA_METHOD_SPEC: {
                        CHECK_FAIL("TODO: RuntimeMethodHandle");
                    } break;

                    case METADATA_TYPE_DEF:
                    case METADATA_TYPE_REF:
                    case METADATA_TYPE_SPEC: {
                        // get the value
                        System_Type type;
                        CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, operand_token,
                                                                     method->DeclaringType->GenericArguments,
                                                                     method->GenericArguments,
                                                                     &type));

                        // make sure the type is prepared
                        CHECK_AND_RETHROW(jit_prepare_static_type(ctx->ctx, type));

                        // push it
                        CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_RuntimeTypeHandle, &runtime_handle_reg));

                        // the ptr is the type
                        runtime_handle_item = type->MirType;
                    } break;

                    case METADATA_FIELD: {
                        CHECK_FAIL("TODO: RuntimeFieldHandle");
                    } break;

                    case METADATA_MEMBER_REF: {
                        CHECK_FAIL("TODO: figure method or field");
                    } break;

                    default:
                        CHECK_FAIL();
                }

                // move it
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_mem_op(mir_ctx, MIR_T_P, 0, runtime_handle_reg, 0, 1),
                                             MIR_new_ref_op(mir_ctx, runtime_handle_item)));
            } break;

            case CEE_DUP: {
                // get the top value
                MIR_reg_t top_reg;
                System_Type top_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &top_type, &top_reg, NULL));

                // create new two values
                MIR_reg_t value_1;
                MIR_reg_t value_2;
                CHECK_AND_RETHROW(jit_stack_push(ctx, top_type, &value_1));
                CHECK_AND_RETHROW(jit_stack_push(ctx, top_type, &value_2));

                switch (type_get_stack_type(top_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(top_type)) {
                            goto dup_value_type;
                        } else {
                            goto dup_primitive_type;
                        }
                    } break;

                    dup_primitive_type:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT:
                    case STACK_TYPE_REF: {
                        MIR_insn_code_t code = jit_mov_insn_code(top_type);
                        // normal value, copy the two regs
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_reg_op(mir_ctx, value_1),
                                                     MIR_new_reg_op(mir_ctx, top_reg)));
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_reg_op(mir_ctx, value_2),
                                                     MIR_new_reg_op(mir_ctx, top_reg)));
                    } break;

                    dup_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        // only copy the second value, we can move the pointer
                        // to the second one because we are essentially SSA
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, value_1),
                                                     MIR_new_reg_op(mir_ctx, top_reg)));

                        jit_emit_memcpy(ctx, value_2, value_1, top_type->StackSize);
                    } break;
                }
            } break;

            case CEE_POP: {
                CHECK_AND_RETHROW(jit_stack_pop(ctx, NULL, NULL, NULL));
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Field access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STSFLD: {
                // get the top value
                MIR_reg_t value_reg;
                System_Type value_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &value_type, &value_reg, NULL));

                // get the field type, ignoring stuff like enums
                System_Type field_type = type_get_underlying_type(operand_field->FieldType);

                // make sure the field is static
                CHECK(field_is_static(operand_field));

                // TODO: should this be supported
                CHECK(!operand_field->HasRva);

                // if this is an init-only field then make sure that
                // only rtspecialname can access it (.ctor and .cctor)
                if (field_is_init_only(operand_field)) {
                    CHECK(
                        method->DeclaringType == operand_field->DeclaringType &&
                        method_is_rt_special_name(method) &&
                        string_equals_cstr(method->Name, ".cctor")
                    );
                }

                // validate the assignability
                CHECK(type_is_verifier_assignable_to(value_type, operand_field->FieldType));

                // have the reference in a register for easy access
                MIR_reg_t field_reg = jit_new_temp_reg(ctx, tSystem_IntPtr);
                if (field_is_thread_static(operand_field)) {
                    // thread local field
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_call_insn(mir_ctx, 4,
                                                      MIR_new_ref_op(mir_ctx, m_get_thread_local_ptr_proto),
                                                      MIR_new_ref_op(mir_ctx, m_get_thread_local_ptr_func),
                                                      MIR_new_reg_op(mir_ctx, field_reg),
                                                      MIR_new_uint_op(mir_ctx, operand_field->ThreadStaticIndex)));
                } else {
                    // normal static field
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, field_reg),
                                                 MIR_new_ref_op(mir_ctx, operand_field->MirField)));
                }
                MIR_op_t field_op = MIR_new_mem_op(mir_ctx, jit_get_mir_type(field_type), 0, field_reg, 0, 1);

                switch (type_get_stack_type(value_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(field_type)) {
                            if (type_is_interface(value_type)) {
                                // interface -> interface
                                goto stsfld_value_type;
                            } else {
                                // object -> interface
                                CHECK_AND_RETHROW(jit_cast_obj_to_interface(ctx,
                                                                            field_reg, value_reg,
                                                                            value_type, field_type));
                            }
                        } else {
                            if (type_is_interface(value_type)) {
                                // interface -> object
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_MOV,
                                                             field_op,
                                                             MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), value_reg, 0, 1)));
                            } else {
                                // object -> object
                                goto stsfld_primitive_type;
                            }
                        }
                    } break;

                    stsfld_primitive_type:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT:
                    case STACK_TYPE_REF: {
                        MIR_insn_code_t code = jit_number_cast_inscode(value_type, field_type);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     field_op,
                                                     MIR_new_reg_op(mir_ctx, value_reg)));
                    } break;

                    stsfld_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        jit_emit_memcpy(ctx, field_reg, value_reg, value_type->StackSize);
                    } break;
                }
            } break;

            case CEE_LDSFLD: {
                // only static fields
                CHECK(field_is_static(operand_field));

                // TODO: should this be supported
                CHECK(!operand_field->HasRva);

                // Get the field type
                System_Type field_stack_type = type_get_intermediate_type(operand_field->FieldType);
                System_Type field_type = type_get_underlying_type(operand_field->FieldType);

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, field_stack_type, &value_reg));

                // have the reference in a register for easy access
                MIR_reg_t field_reg = jit_new_temp_reg(ctx, tSystem_IntPtr);
                if (field_is_thread_static(operand_field)) {
                    // thread local field
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_call_insn(mir_ctx, 4,
                                                      MIR_new_ref_op(mir_ctx, m_get_thread_local_ptr_proto),
                                                      MIR_new_ref_op(mir_ctx, m_get_thread_local_ptr_func),
                                                      MIR_new_reg_op(mir_ctx, field_reg),
                                                      MIR_new_uint_op(mir_ctx, operand_field->ThreadStaticIndex)));
                } else {
                    // normal static field
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, field_reg),
                                                 MIR_new_ref_op(mir_ctx, operand_field->MirField)));
                }
                MIR_op_t field_op = MIR_new_mem_op(mir_ctx, jit_get_mir_type(field_type), 0, field_reg, 0, 1);

                switch (type_get_stack_type(field_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(field_type)) {
                            goto ldsfld_value_type;
                        } else {
                            goto ldsfld_primitive_type;
                        }
                    } break;

                    ldsfld_primitive_type:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT: {
                        // we need to extend this properly if the field is smaller
                        // than an int32 (because we are going to load into an int32
                        // essentially)
                        MIR_insn_code_t insn = MIR_MOV;
                        if (field_type == tSystem_SByte || field_type == tSystem_Boolean) {
                            insn = MIR_EXT8;
                        } else if (field_type == tSystem_Byte) {
                            insn = MIR_UEXT8;
                        } else if (field_type == tSystem_Int16) {
                            insn = MIR_EXT16;
                        } else if (field_type == tSystem_UInt16 || field_type == tSystem_Char) {
                            insn = MIR_UEXT16;
                        } else if (field_type == tSystem_Single) {
                            insn = MIR_FMOV;
                        } else if (field_type == tSystem_Double) {
                            insn = MIR_DMOV;
                        }

                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, insn,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     field_op));
                    } break;

                    ldsfld_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        // take the offset and copy it
                        jit_emit_memcpy(ctx, value_reg, field_reg, field_type->StackSize);
                    } break;

                    case STACK_TYPE_REF:
                        CHECK_FAIL("wtf");
                }
            } break;

            case CEE_LDSFLDA: {
                // only static fields
                CHECK(field_is_static(operand_field));

                if (operand_field->HasRva) {
                    // this is a special case, we are going to push it as a pointer instead of a
                    // managed pointer, for simplicity of implementation

                    // push it
                    MIR_reg_t value_reg;
                    CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_UIntPtr, &value_reg));
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, value_reg),
                                                 MIR_new_uint_op(mir_ctx, (uintptr_t)operand_field->Rva)));

                } else {
                    // Get the field type
                    System_Type field_stack_type = get_by_ref_type(type_get_verification_type(operand_field->FieldType));

                    // push it
                    MIR_reg_t value_reg;
                    CHECK_AND_RETHROW(jit_stack_push(ctx, field_stack_type, &value_reg));
                    STACK_TOP.non_local_ref = true; // static field, not local

                    // only set that this is a readonly ref if this is not the cctor and we are loading a static
                    // readonly field of the current class
                    bool is_cctor = method_is_rt_special_name(method) && string_equals_cstr(method->Name, ".cctor");
                    if (!(is_cctor && operand_field->DeclaringType == method->DeclaringType)) {
                        STACK_TOP.readonly_ref = field_is_init_only(operand_field);
                    }

                    if (field_is_thread_static(operand_field)) {
                        // thread local field, return the pointer directly
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_call_insn(mir_ctx, 4,
                                                          MIR_new_ref_op(mir_ctx, m_get_thread_local_ptr_proto),
                                                          MIR_new_ref_op(mir_ctx, m_get_thread_local_ptr_func),
                                                          MIR_new_reg_op(mir_ctx, value_reg),
                                                          MIR_new_uint_op(mir_ctx, operand_field->ThreadStaticIndex)));
                    } else {
                        // very simple, just move the reference to the value field
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_ref_op(mir_ctx, operand_field->MirField)));
                    }
                }
            } break;

            case CEE_STFLD: {
                // get the values
                stack_entry_t obj_entry;
                MIR_reg_t obj_reg;
                MIR_reg_t value_reg;
                System_Type obj_type;
                System_Type value_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &value_type, &value_reg, NULL));
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &obj_type, &obj_reg, &obj_entry));

                // validate that the object type is a valid one for stfld
                if (type_get_stack_type(obj_type) == STACK_TYPE_REF) {
                    // this is a reference, so it has to be a reference to a value type
                    // note that we can't know if the value type is part of another class
                    // or not so we have to use gc_update_ref
                    CHECK(obj_type->BaseType->IsValueType);

                    // must be a non-readonly reference
                    CHECK(!obj_entry.readonly_ref);
                } else {
                    CHECK(type_get_stack_type(obj_type) == STACK_TYPE_O);
                }

                // validate the field is part of the object
                System_Type target = type_get_intermediate_type(operand_field->DeclaringType);
                System_Type base = obj_type;
                while (base != NULL && base != target) {
                    base = base->BaseType;
                }
                CHECK(base != NULL);

                // make sure the field is compatible
                CHECK(type_is_verifier_assignable_to(value_type, operand_field->FieldType));

                // TODO: does the runtime actually use ldfld for static fields?
                //       in theory CIL allows that, but I think I won't for simplicity
                CHECK(!field_is_static(operand_field));

                // if this is an init-only field then make sure that
                // only rtspecialname can access it (.ctor and .cctor)
                if (field_is_init_only(operand_field)) {
                    CHECK(
                        obj_entry.this &&
                        method_is_rt_special_name(method) &&
                        string_equals_cstr(method->Name, ".ctor")
                    );
                }

                // check the object is not null
                if (type_get_stack_type(obj_type) == STACK_TYPE_O) {
                    CHECK_AND_RETHROW(jit_null_check(ctx, obj_reg, obj_type));
                }

                // validate the assignability
                CHECK(type_is_verifier_assignable_to(value_type, operand_field->FieldType));

                // get the field type, ignoring stuff like enums
                System_Type field_type = type_get_underlying_type(operand_field->FieldType);

                // check how we should assign the given item
                switch (type_get_stack_type(value_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(field_type)) {
                            if (type_is_interface(value_type)) {
                                // interface -> interface
                                goto stfld_value_type;
                            } else {
                                // object -> interface

                                // get the result, which is the object base plus the offset
                                MIR_reg_t field_addr_reg = jit_new_temp_reg(ctx, tSystem_UIntPtr);
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_ADD,
                                                             MIR_new_reg_op(mir_ctx, field_addr_reg),
                                                             MIR_new_reg_op(mir_ctx, obj_reg),
                                                             MIR_new_int_op(mir_ctx, (int)operand_field->MemoryOffset)));

                                // emit the cast
                                CHECK_AND_RETHROW(jit_cast_obj_to_interface(ctx,
                                                                            field_addr_reg, value_reg,
                                                                            value_type, field_type));
                            }
                        } else {
                            // check for interface -> object casting
                            if (type_is_interface(value_type)) {
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_MOV,
                                                             MIR_new_reg_op(mir_ctx, value_reg),
                                                             MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), value_reg, 0, 1)));
                            }

                            // storing to an object from an object, use a write-barrier
                            if (type_get_stack_type(obj_type) == STACK_TYPE_O) {
                                // the base is an object, call the gc_update write barrier
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_call_insn(mir_ctx, 5,
                                                                  MIR_new_ref_op(mir_ctx, m_gc_update_proto),
                                                                  MIR_new_ref_op(mir_ctx, m_gc_update_func),
                                                                  MIR_new_reg_op(mir_ctx, obj_reg),
                                                                  MIR_new_int_op(mir_ctx, (int)operand_field->MemoryOffset),
                                                                  MIR_new_reg_op(mir_ctx, value_reg)));
                            } else {
                                // the base is a struct

                                // add the offset to the object base
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_ADD,
                                                             MIR_new_reg_op(mir_ctx, obj_reg),
                                                             MIR_new_reg_op(mir_ctx, obj_reg),
                                                             MIR_new_int_op(mir_ctx, (int)operand_field->MemoryOffset)));

                                // call the gc_update_ref write-barrier
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_call_insn(mir_ctx, 4,
                                                                  MIR_new_ref_op(mir_ctx, m_gc_update_ref_proto),
                                                                  MIR_new_ref_op(mir_ctx, m_gc_update_ref_func),
                                                                  MIR_new_reg_op(mir_ctx, obj_reg),
                                                                  MIR_new_reg_op(mir_ctx, value_reg)));
                            }
                        }
                    } break;

                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT: {
                        MIR_insn_code_t code = jit_number_cast_inscode(value_type, field_type);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_mem_op(mir_ctx,
                                                                    jit_get_mir_type(operand_field->FieldType),
                                                                    (int)operand_field->MemoryOffset,
                                                                    obj_reg, 0, 1),
                                                     MIR_new_reg_op(mir_ctx, value_reg)));
                    } break;

                    stfld_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        if (arrlen(value_type->ManagedPointersOffsets) == 0) {
                            // there are no managed pointers in the value type we are storing, so
                            // we can do a normal memcpy no matter what

                            // add the offset to the object base
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, MIR_ADD,
                                                         MIR_new_reg_op(mir_ctx, obj_reg),
                                                         MIR_new_reg_op(mir_ctx, obj_reg),
                                                         MIR_new_int_op(mir_ctx, (int)operand_field->MemoryOffset)));

                            // emit a memcpy
                            jit_emit_memcpy(ctx, obj_reg, value_reg, value_type->StackSize);
                        } else {
                            if (type_get_stack_type(obj_type) == STACK_TYPE_O) {
                                // copying into a class, use the managed memcpy
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_call_insn(mir_ctx, 6,
                                                                  MIR_new_ref_op(mir_ctx, m_managed_memcpy_proto),
                                                                  MIR_new_ref_op(mir_ctx, m_managed_memcpy_func),
                                                                  MIR_new_reg_op(mir_ctx, obj_reg),
                                                                  MIR_new_ref_op(mir_ctx, field_type->MirType),
                                                                  MIR_new_int_op(mir_ctx, (int)operand_field->MemoryOffset),
                                                                  MIR_new_reg_op(mir_ctx, value_reg)));
                            } else {
                                // add the offset to the object base
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_ADD,
                                                             MIR_new_reg_op(mir_ctx, obj_reg),
                                                             MIR_new_reg_op(mir_ctx, obj_reg),
                                                             MIR_new_int_op(mir_ctx, (int)operand_field->MemoryOffset)));

                                // copying into a managed pointer, use the managed ref memcpy
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_call_insn(mir_ctx, 5,
                                                                  MIR_new_ref_op(mir_ctx, m_managed_ref_memcpy_proto),
                                                                  MIR_new_ref_op(mir_ctx, m_managed_ref_memcpy_func),
                                                                  MIR_new_reg_op(mir_ctx, obj_reg),
                                                                  MIR_new_ref_op(mir_ctx, field_type->MirType),
                                                                  MIR_new_reg_op(mir_ctx, value_reg)));
                            }
                        }
                    } break;

                    case STACK_TYPE_REF: {
                        CHECK_FAIL("There is no such thing as a ref-field");
                    } break;
                }
            } break;

            case CEE_LDFLD: {
                // get the object instance
                System_Type obj_type;
                MIR_reg_t obj_reg;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &obj_type, &obj_reg, NULL));

                // validate that the object type is a valid one for stfld
                if (type_get_stack_type(obj_type) == STACK_TYPE_REF) {
                    // this is a reference, so it has to be a reference to a value type
                    CHECK(obj_type->BaseType->IsValueType);
                } else {
                    CHECK(type_get_stack_type(obj_type) == STACK_TYPE_O || type_get_stack_type(obj_type) == STACK_TYPE_VALUE_TYPE);
                }

                // validate the field is part of the object
                System_Type target = type_get_intermediate_type(operand_field->DeclaringType);
                System_Type base = obj_type;
                while (base != NULL && base != target) {
                    base = base->BaseType;
                }
                CHECK(base != NULL);

                // TODO: does the runtime actually use ldfld for static fields?
                CHECK(!field_is_static(operand_field));

                // Get the field type
                System_Type field_stack_type = type_get_intermediate_type(operand_field->FieldType);
                System_Type field_type = type_get_underlying_type(operand_field->FieldType);

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, field_stack_type, &value_reg));

                // check the object is not null
                if (type_get_stack_type(obj_type) == STACK_TYPE_O) {
                    CHECK_AND_RETHROW(jit_null_check(ctx, obj_reg, obj_type));
                }

                switch (type_get_stack_type(field_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(field_type)) {
                            goto ldfld_value_type;
                        } else {
                            goto ldfld_primitive_type;
                        }
                    } break;

                    ldfld_primitive_type:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT: {
                        // we need to extend this properly if the field is smaller
                        // than an int32 (because we are going to load into an int32
                        // essentially)
                        MIR_insn_code_t insn = MIR_MOV;
                        if (field_type == tSystem_SByte || field_type == tSystem_Boolean) {
                            insn = MIR_EXT8;
                        } else if (field_type == tSystem_Byte) {
                            insn = MIR_UEXT8;
                        } else if (field_type == tSystem_Int16) {
                            insn = MIR_EXT16;
                        } else if (field_type == tSystem_UInt16 || field_type == tSystem_Char) {
                            insn = MIR_UEXT16;
                        } else if (field_type == tSystem_Single) {
                            insn = MIR_FMOV;
                        } else if (field_type == tSystem_Double) {
                            insn = MIR_DMOV;
                        }

                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, insn,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_mem_op(mir_ctx,
                                                                    jit_get_mir_type(operand_field->FieldType),
                                                                    (int)operand_field->MemoryOffset,
                                                                    obj_reg, 0, 1)));
                    } break;

                    ldfld_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        // take the offset and copy it
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_ADD,
                                                     MIR_new_reg_op(mir_ctx, obj_reg),
                                                     MIR_new_reg_op(mir_ctx, obj_reg),
                                                     MIR_new_int_op(mir_ctx, (int)operand_field->MemoryOffset)));
                        jit_emit_memcpy(ctx, value_reg, obj_reg, field_type->StackSize);
                    } break;

                    case STACK_TYPE_REF:
                        CHECK_FAIL("wtf");
                }
            } break;

            case CEE_LDFLDA: {
                // get the object instance
                stack_entry_t obj_entry;
                System_Type obj_type;
                MIR_reg_t obj_reg;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &obj_type, &obj_reg, &obj_entry));

                // validate that the object type is a valid one for ldfld
                CHECK(type_get_stack_type(obj_type) == STACK_TYPE_O || type_get_stack_type(obj_type) == STACK_TYPE_REF);

                // validate the field is part of the object
                System_Type target = type_get_intermediate_type(operand_field->DeclaringType);
                System_Type base = obj_type;
                while (base != NULL && base != target) {
                    base = base->BaseType;
                }
                CHECK(base != NULL);

                // TODO: does the runtime actually use ldfld for static fields?
                CHECK(!field_is_static(operand_field));

                // Get the field type
                System_Type field_stack_type = get_by_ref_type(type_get_verification_type(operand_field->FieldType));

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, field_stack_type, &value_reg));

                // check if this comes as an outside reference
                bool non_stack_local = false;
                bool readonly = field_is_init_only(operand_field);
                if (type_get_stack_type(obj_type) == STACK_TYPE_O) {
                    // heap object
                    non_stack_local = true;
                } else if (type_get_stack_type(obj_type) == STACK_TYPE_REF) {
                    // check if the reference is a non-value type
                    CHECK(type_is_value_type(obj_type->BaseType));
                    non_stack_local = obj_entry.non_local_ref;
                    readonly = readonly || obj_entry.readonly_ref;
                }
                STACK_TOP.non_local_ref = non_stack_local;

                // only set the readonly mark if we are not loading from the
                // `this` object on the ctor, on any other case we want it to
                // be read only
                bool is_ctor = method_is_rt_special_name(method) && string_equals_cstr(method->Name, ".ctor");
                if (!obj_entry.this || !is_ctor) {
                    STACK_TOP.readonly_ref = readonly;
                }

                // check the object is not null
                if (type_get_stack_type(obj_type) == STACK_TYPE_O) {
                    CHECK_AND_RETHROW(jit_null_check(ctx, obj_reg, obj_type));
                }

                // very simple, just add to the object the field offset
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_ADD,
                                             MIR_new_reg_op(mir_ctx, value_reg),
                                             MIR_new_reg_op(mir_ctx, obj_reg),
                                             MIR_new_int_op(mir_ctx, (int)operand_field->MemoryOffset)));
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Calls and Returns
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // allocate space on the stack
            // NOTE: while in theory this is not a verifiable instruction, we are going to allow it
            //       because without the ability to access pointers it is not that useful
            case CEE_LOCALLOC: {
                // first make sure we are not in an exception handler

                // validate we are not actually exiting a protected block with this branch
                System_Reflection_ExceptionHandlingClause_Array exceptions = ctx->method->MethodBody->ExceptionHandlingClauses;
                for (int i = 0; i < exceptions->Length; i++) {
                    System_Reflection_ExceptionHandlingClause clause = exceptions->Data[i];
                    CHECK(ctx->il_offset < clause->HandlerOffset || clause->HandlerOffset + clause->HandlerLength <= ctx->il_offset);
                }

                // pop the size
                System_Type size_type;
                MIR_reg_t size_reg;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &size_type, &size_reg, NULL));

                // push the address
                MIR_reg_t addr_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_UIntPtr, &addr_reg));

                // only int32 and intptr are allowed
                if (type_get_stack_type(size_type) == STACK_TYPE_INT32) {
                    // sign extend to int64
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_EXT32,
                                                 MIR_new_reg_op(mir_ctx, size_reg),
                                                 MIR_new_reg_op(mir_ctx, size_reg)));
                } else {
                    CHECK(type_get_stack_type(size_type) == STACK_TYPE_INTPTR);
                }

                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_ALLOCA,
                                             MIR_new_reg_op(mir_ctx, addr_reg),
                                             MIR_new_reg_op(mir_ctx, size_reg)));

                // zero the memory
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_call_insn(mir_ctx, 5,
                                                  MIR_new_ref_op(mir_ctx, m_memset_proto),
                                                  MIR_new_ref_op(mir_ctx, m_memset_func),
                                                  MIR_new_reg_op(mir_ctx, addr_reg),
                                                  MIR_new_int_op(mir_ctx, 0),
                                                  MIR_new_reg_op(mir_ctx, size_reg)));
            } break;

            case CEE_NEWOBJ:
            case CEE_CALLVIRT:
            case CEE_CALL: {
                CHECK_AND_RETHROW(jit_emit_call(ctx, opcode));
            } break;

            case CEE_INITOBJ: {
                System_Type dest_type;
                MIR_reg_t dest_reg;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &dest_type, &dest_reg, NULL));

                CHECK(dest_type->IsByRef);
                CHECK(type_is_verifier_assignable_to(operand_type, dest_type->BaseType));

                jit_emit_zerofill(ctx, dest_reg, operand_type->StackSize);
            } break;

            case CEE_RET: {
                CHECK(ctx->filter_clause == NULL);

                System_Type method_ret_type = type_get_underlying_type(method->ReturnType);

                if (method_ret_type == NULL) {
                    // must be an empty stack, since we have no return value
                    CHECK(arrlen(ctx->stack.entries) == 0);

                    // there is no return value, just add a ret
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_ret_insn(mir_ctx, 1,
                                                     MIR_new_int_op(mir_ctx, 0)));
                } else {
                    // pop the return from the stack
                    MIR_reg_t ret_arg;
                    System_Type ret_type;
                    CHECK_AND_RETHROW(jit_stack_pop(ctx, &ret_type, &ret_arg, NULL));

                    // verify the stack is empty
                    CHECK(arrlen(ctx->stack.entries) == 0);

                    // verify the IL
                    CHECK(type_is_verifier_assignable_to(ret_type, method->ReturnType));

                    switch (type_get_stack_type(ret_type)) {
                        case STACK_TYPE_O: {
                            if (type_is_interface(method_ret_type)) {
                                if (type_is_interface(ret_type)) {
                                    // interface -> interface
                                    goto ret_value_type;
                                } else {
                                    // object -> interface
                                    CHECK_AND_RETHROW(jit_cast_obj_to_interface(ctx,
                                                                                return_block_reg, ret_arg,
                                                                                ret_type, method_ret_type));

                                    // return no exception
                                    MIR_append_insn(mir_ctx, mir_func,
                                                    MIR_new_ret_insn(mir_ctx, 1,
                                                                     MIR_new_int_op(mir_ctx, 0)));
                                }
                            } else {
                                if (type_is_interface(ret_type)) {
                                    // interface -> object
                                    MIR_append_insn(mir_ctx, mir_func,
                                                    MIR_new_ret_insn(mir_ctx, 2,
                                                                     MIR_new_int_op(mir_ctx, 0),
                                                                     MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), ret_arg, 0, 1)));
                                } else {
                                    // object -> object
                                    goto ret_primitive_type;

                                }
                            }
                        } break;

                        ret_primitive_type:
                        case STACK_TYPE_INT32:
                        case STACK_TYPE_INT64:
                        case STACK_TYPE_INTPTR:
                        case STACK_TYPE_FLOAT: {
                            // TODO: do we need to do float conversion in this case?

                            // it is stored in a register directly, just return it
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_ret_insn(mir_ctx, 2,
                                                             MIR_new_int_op(mir_ctx, 0),
                                                             MIR_new_reg_op(mir_ctx, ret_arg)));
                        } break;

                        ret_value_type:
                        case STACK_TYPE_VALUE_TYPE: {
                            // this is a big struct, copy it to the return block
                            jit_emit_memcpy(ctx, return_block_reg, ret_arg, ret_type->StackSize);

                            // return no exception
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_ret_insn(mir_ctx, 1,
                                                             MIR_new_int_op(mir_ctx, 0)));
                        } break;

                        case STACK_TYPE_REF:
                            stack_entry_t entry = ctx->stack.entries[arrlen(ctx->stack.entries)];

                            // make sure that the return value is a non-local reference
                            CHECK(entry.non_local_ref);

                            // TODO: methods that do allow to return readonly refs
                            // make sure that the return value is a non-readonly reference
                            CHECK(!entry.readonly_ref);

                            // it is, then return it like we return any other primitive
                            goto ret_primitive_type;
                    }
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Array handling
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_NEWARR: {
                // get the number of elements
                MIR_reg_t num_elems_reg;
                System_Type num_elems_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &num_elems_type, &num_elems_reg, NULL));

                // only int32 and intptr are allowed
                if (type_get_stack_type(num_elems_type) == STACK_TYPE_INT32) {
                    // sign extend to int64
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_EXT32,
                                                 MIR_new_reg_op(mir_ctx, num_elems_reg),
                                                 MIR_new_reg_op(mir_ctx, num_elems_reg)));
                } else {
                    CHECK(type_get_stack_type(num_elems_type) == STACK_TYPE_INTPTR);
                }

                // creating a new array type, make sure that it has all the virtual methods
                // set up correctly
                System_Type array_type = get_array_type(operand_type);
                CHECK_AND_RETHROW(jit_prepare_instance_type(ctx->ctx, array_type));

                // push the array type
                MIR_reg_t array_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, array_type, &array_reg));

                // calculate the size we are going to need:
                //  num_elems * sizeof(value_type) + sizeof(System.Array)
                MIR_reg_t size_reg = jit_new_temp_reg(ctx, tSystem_Int64);
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MUL,
                                             MIR_new_reg_op(mir_ctx, size_reg),
                                             MIR_new_reg_op(mir_ctx, num_elems_reg),
                                             MIR_new_int_op(mir_ctx, operand_type->StackSize)));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_ADD,
                                             MIR_new_reg_op(mir_ctx, size_reg),
                                             MIR_new_reg_op(mir_ctx, size_reg),
                                             MIR_new_int_op(mir_ctx, array_type->ManagedSize)));

                // actually allocate it now
                // allocate the new object
                CHECK_AND_RETHROW(jit_new(ctx,
                                          array_reg, get_array_type(operand_type),
                                          MIR_new_reg_op(mir_ctx, size_reg)));

                // Set the length of the array
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_mem_op(mir_ctx,
                                                            MIR_T_I32,
                                                            offsetof(struct System_Array, Length),
                                                            array_reg,
                                                            0, 1),
                                             MIR_new_reg_op(mir_ctx, num_elems_reg)));
            } break;

            case CEE_LDLEN: {
                // get the number of elements
                MIR_reg_t array_reg;
                System_Type array_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &array_type, &array_reg, NULL));

                // this must be an array
                CHECK(array_type->IsArray);

                // check the object is not null
                CHECK_AND_RETHROW(jit_null_check(ctx, array_reg, array_type));

                // push the length
                MIR_reg_t length_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_IntPtr, &length_reg));

                // simply read the array's length
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, length_reg),
                                             MIR_new_mem_op(mir_ctx, MIR_T_I32,
                                                            offsetof(struct System_Array, Length),
                                                            array_reg, 0, 1)));
            } break;

            case CEE_STELEM_I1: operand_type = tSystem_SByte; goto cee_stelem;
            case CEE_STELEM_I2: operand_type = tSystem_Int16; goto cee_stelem;
            case CEE_STELEM_I4: operand_type = tSystem_Int32; goto cee_stelem;
            case CEE_STELEM_I8: operand_type = tSystem_Int64; goto cee_stelem;
            case CEE_STELEM_R4: operand_type = tSystem_Single; goto cee_stelem;
            case CEE_STELEM_R8: operand_type = tSystem_Double; goto cee_stelem;
            case CEE_STELEM_I: operand_type = tSystem_IntPtr; goto cee_stelem;
            case CEE_STELEM_REF:
            case CEE_STELEM:
            cee_stelem: {
                // pop all the values from the stack
                MIR_reg_t value_reg;
                MIR_reg_t index_reg;
                MIR_reg_t array_reg;
                System_Type value_type;
                System_Type index_type;
                System_Type array_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &value_type, &value_reg, NULL));
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &index_type, &index_reg, NULL));
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &array_type, &array_reg, NULL));

                // this must be an array
                CHECK(array_type->IsArray);

                // for stelem.ref the operand type is the same as the
                // array itself
                if (operand_type == NULL) {
                    operand_type = array_type->ElementType;
                }

                // validate all the type stuff
                CHECK(type_is_array_element_compatible_with(value_type, type_get_intermediate_type(operand_type)));
                CHECK(type_is_array_element_compatible_with(operand_type, array_type->ElementType));

                // only int32 and intptr are allowed
                if (type_get_stack_type(index_type) == STACK_TYPE_INT32) {
                    // sign extend to int64
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_EXT32,
                                                 MIR_new_reg_op(mir_ctx, index_reg),
                                                 MIR_new_reg_op(mir_ctx, index_reg)));
                } else {
                    CHECK(type_get_stack_type(index_type) == STACK_TYPE_INTPTR);
                }

                // check the object is not null
                CHECK_AND_RETHROW(jit_null_check(ctx, array_reg, array_type));

                // check the array indexes
                CHECK_AND_RETHROW(jit_oob_check(ctx, array_reg, index_reg));

                switch (type_get_stack_type(value_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(operand_type)) {
                            if (type_is_interface(value_type)) {
                                // interface -> interface
                                goto stelem_value_type;
                            } else {
                                // object -> interface

                                // calculate the base as `array_reg + index_reg * sizeof(operand_type) + sizeof(System.Array)`
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_MUL,
                                                             MIR_new_reg_op(mir_ctx, index_reg),
                                                             MIR_new_reg_op(mir_ctx, index_reg),
                                                             MIR_new_int_op(mir_ctx, operand_type->StackSize)));
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_ADD,
                                                             MIR_new_reg_op(mir_ctx, index_reg),
                                                             MIR_new_reg_op(mir_ctx, index_reg),
                                                             MIR_new_int_op(mir_ctx, array_type->ManagedSize)));
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_ADD,
                                                             MIR_new_reg_op(mir_ctx, index_reg),
                                                             MIR_new_reg_op(mir_ctx, index_reg),
                                                             MIR_new_reg_op(mir_ctx, array_reg)));


                                // from an object, cast required, need a write barrier
                                CHECK_AND_RETHROW(jit_cast_obj_to_interface(ctx,
                                                                            index_reg, value_reg,
                                                                            value_type, operand_type));
                            }
                        } else {
                            // check if we need to cast to an object from an interface
                            if (type_is_interface(value_type)) {
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_MOV,
                                                             MIR_new_reg_op(mir_ctx, value_reg),
                                                             MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), value_reg, 0, 1)));
                            }

                            // calculate the offset as `index_reg * sizeof(operand_type) + sizeof(System.Array)`
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, MIR_MUL,
                                                         MIR_new_reg_op(mir_ctx, index_reg),
                                                         MIR_new_reg_op(mir_ctx, index_reg),
                                                         MIR_new_int_op(mir_ctx, operand_type->StackSize)));
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, MIR_ADD,
                                                         MIR_new_reg_op(mir_ctx, index_reg),
                                                         MIR_new_reg_op(mir_ctx, index_reg),
                                                         MIR_new_int_op(mir_ctx, array_type->ManagedSize)));

                            // storing to an object from an object
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_call_insn(mir_ctx, 5,
                                                              MIR_new_ref_op(mir_ctx, m_gc_update_proto),
                                                              MIR_new_ref_op(mir_ctx, m_gc_update_func),
                                                              MIR_new_reg_op(mir_ctx, array_reg),
                                                              MIR_new_reg_op(mir_ctx, index_reg),
                                                              MIR_new_reg_op(mir_ctx, value_reg)));
                        }
                    } break;

                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT: {
                        MIR_insn_code_t code = jit_number_cast_inscode(value_type, operand_type);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_mem_op(mir_ctx, jit_get_mir_type(operand_type),
                                                                    array_type->ManagedSize,
                                                                    array_reg, index_reg, operand_type->StackSize),
                                                     MIR_new_reg_op(mir_ctx, value_reg)));
                    } break;

                    stelem_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        // calculate the offset as `array + index_reg * sizeof(operand_type) + sizeof(System.Array)`
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MUL,
                                                     MIR_new_reg_op(mir_ctx, index_reg),
                                                     MIR_new_reg_op(mir_ctx, index_reg),
                                                     MIR_new_int_op(mir_ctx, operand_type->StackSize)));
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_ADD,
                                                     MIR_new_reg_op(mir_ctx, index_reg),
                                                     MIR_new_reg_op(mir_ctx, index_reg),
                                                     MIR_new_int_op(mir_ctx, array_type->ManagedSize)));

                        if (arrlen(value_type->ManagedPointersOffsets) == 0) {
                            // add the base, so we have an abs address
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, MIR_ADD,
                                                         MIR_new_reg_op(mir_ctx, array_reg),
                                                         MIR_new_reg_op(mir_ctx, index_reg),
                                                         MIR_new_reg_op(mir_ctx, array_reg)));

                            // can use a simple memcpy
                            jit_emit_memcpy(ctx, array_reg, value_reg, operand_type->StackSize);
                        } else {
                            // has pointers, use managed memcpy
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_call_insn(mir_ctx, 6,
                                                              MIR_new_ref_op(mir_ctx, m_managed_memcpy_proto),
                                                              MIR_new_ref_op(mir_ctx, m_managed_memcpy_func),
                                                              MIR_new_reg_op(mir_ctx, array_reg),
                                                              MIR_new_ref_op(mir_ctx, operand_type->MirType),
                                                              MIR_new_reg_op(mir_ctx, index_reg),
                                                              MIR_new_reg_op(mir_ctx, value_reg)));
                        }
                    } break;

                    case STACK_TYPE_REF:
                        CHECK_FAIL("wtf");
                }
            } break;

            case CEE_LDELEM_I1: operand_type = tSystem_SByte; goto cee_ldelem;
            case CEE_LDELEM_I2: operand_type = tSystem_Int16; goto cee_ldelem;
            case CEE_LDELEM_I4: operand_type = tSystem_Int32; goto cee_ldelem;
            case CEE_LDELEM_I8: operand_type = tSystem_Int64; goto cee_ldelem;
            case CEE_LDELEM_U1: operand_type = tSystem_Byte; goto cee_ldelem;
            case CEE_LDELEM_U2: operand_type = tSystem_UInt16; goto cee_ldelem;
            case CEE_LDELEM_U4: operand_type = tSystem_UInt32; goto cee_ldelem;
            case CEE_LDELEM_R4: operand_type = tSystem_Single; goto cee_ldelem;
            case CEE_LDELEM_R8: operand_type = tSystem_Double; goto cee_ldelem;
            case CEE_LDELEM_I: operand_type = tSystem_IntPtr; goto cee_ldelem;
            case CEE_LDELEM_REF:    // implicit from array type
            case CEE_LDELEM:        // operand type is loaded
            cee_ldelem: {
                // pop all the values from the stack
                MIR_reg_t index_reg;
                MIR_reg_t array_reg;
                System_Type index_type;
                System_Type array_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &index_type, &index_reg, NULL));
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &array_type, &array_reg, NULL));

                // this must be an array
                CHECK(array_type->IsArray);

                // for anything which is not ldelem.ref we know the operand_type
                // from the array
                if (operand_type != NULL) {
                    CHECK(type_is_array_element_compatible_with(array_type->ElementType, operand_type));
                } else {
                    // the type is gotten from the array
                    operand_type = array_type->ElementType;
                }

                // only int32 and intptr are allowed
                if (type_get_stack_type(index_type) == STACK_TYPE_INT32) {
                    // sign extend to int64
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_EXT32,
                                                 MIR_new_reg_op(mir_ctx, index_reg),
                                                 MIR_new_reg_op(mir_ctx, index_reg)));
                } else {
                    CHECK(type_get_stack_type(index_type) == STACK_TYPE_INTPTR);
                }

                // check the object is not null
                CHECK_AND_RETHROW(jit_null_check(ctx, array_reg, array_type));

                // check the array indexes
                CHECK_AND_RETHROW(jit_oob_check(ctx, array_reg, index_reg));

                // push to the stack
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, type_get_intermediate_type(operand_type), &value_reg));

                switch (type_get_stack_type(operand_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(operand_type)) {
                            goto ldelem_value_type;
                        } else {
                            goto ldelem_primitive_type;
                        }
                    } break;

                    ldelem_primitive_type:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INTPTR:
                    case STACK_TYPE_FLOAT: {
                        // we need to extend this properly if the field is smaller
                        // than an int32 (because we are going to load into an int32
                        // essentially)
                        MIR_insn_code_t code = MIR_MOV;
                        if (operand_type == tSystem_SByte || operand_type == tSystem_Boolean) {
                            code = MIR_EXT8;
                        } else if (operand_type == tSystem_Byte) {
                            code = MIR_UEXT8;
                        } else if (operand_type == tSystem_Int16) {
                            code = MIR_EXT16;
                        } else if (operand_type == tSystem_UInt16 || operand_type == tSystem_Char) {
                            code = MIR_UEXT16;
                        } else if (operand_type == tSystem_Single) {
                            code = MIR_FMOV;
                        } else if (operand_type == tSystem_Double) {
                            code = MIR_DMOV;
                        }

                        // we can copy this in a single mov
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_mem_op(mir_ctx, jit_get_mir_type(operand_type),
                                                                    array_type->ManagedSize,
                                                                    array_reg, index_reg, operand_type->StackSize)));
                    } break;

                    ldelem_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        // just emit a memcpy
                        // calculate the offset as `array + index_reg * sizeof(operand_type) + sizeof(System.Array)`
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MUL,
                                                     MIR_new_reg_op(mir_ctx, index_reg),
                                                     MIR_new_reg_op(mir_ctx, index_reg),
                                                     MIR_new_int_op(mir_ctx, operand_type->StackSize)));
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_ADD,
                                                     MIR_new_reg_op(mir_ctx, index_reg),
                                                     MIR_new_reg_op(mir_ctx, index_reg),
                                                     MIR_new_int_op(mir_ctx, array_type->ManagedSize)));
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_ADD,
                                                     MIR_new_reg_op(mir_ctx, array_reg),
                                                     MIR_new_reg_op(mir_ctx, index_reg),
                                                     MIR_new_reg_op(mir_ctx, array_reg)));

                        // can use a simple memcpy
                        jit_emit_memcpy(ctx, value_reg, array_reg, operand_type->StackSize);
                    } break;

                    case STACK_TYPE_REF:
                        CHECK_FAIL();
                }
            } break;

            case CEE_LDELEMA: {
                // pop all the values from the stack
                MIR_reg_t value_reg;
                MIR_reg_t index_reg;
                MIR_reg_t array_reg;
                System_Type index_type;
                System_Type array_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &index_type, &index_reg, NULL));
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &array_type, &array_reg, NULL));

                // this must be an array
                CHECK(array_type->IsArray);

                // for anything which is not ldelem.ref we know the operand_type
                // from the array
                CHECK(type_is_array_element_compatible_with(array_type->ElementType, operand_type));

                // only int32 and intptr are allowed
                if (type_get_stack_type(index_type) == STACK_TYPE_INT32) {
                    // sign extend to int64
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_EXT32,
                                                 MIR_new_reg_op(mir_ctx, index_reg),
                                                 MIR_new_reg_op(mir_ctx, index_reg)));
                } else {
                    CHECK(type_get_stack_type(index_type) == STACK_TYPE_INTPTR);
                }

                // check the object is not null
                CHECK_AND_RETHROW(jit_null_check(ctx, array_reg, array_type));

                // check the array indexes
                CHECK_AND_RETHROW(jit_oob_check(ctx, array_reg, index_reg));

                // push to the stack
                CHECK_AND_RETHROW(jit_stack_push(ctx, get_by_ref_type(type_get_verification_type(operand_type)), &value_reg));
                STACK_TOP.non_local_ref = true;

                // calculate the element reference offset
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MUL,
                                             MIR_new_reg_op(mir_ctx, value_reg),
                                             MIR_new_reg_op(mir_ctx, index_reg),
                                             MIR_new_int_op(mir_ctx, operand_type->StackSize)));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_ADD,
                                             MIR_new_reg_op(mir_ctx, value_reg),
                                             MIR_new_reg_op(mir_ctx, value_reg),
                                             MIR_new_int_op(mir_ctx, array_type->ManagedSize)));

                // add the object base to the offset
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_ADD,
                                             MIR_new_reg_op(mir_ctx, value_reg),
                                             MIR_new_reg_op(mir_ctx, value_reg),
                                             MIR_new_reg_op(mir_ctx, array_reg)));
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Delegate
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDVIRTFTN: {
                // according to the ECMA spec it should always be
                //
                //  DUP
                //  LDVIRTFTN method
                //  NEWOBJ delegate_ctor
                //
                // the reason is that this makes sure that the method and the type we are
                // creating the delegate on are def the same type
                //
                CHECK(last_opcode == CEE_DUP);

                // pop the object
                MIR_reg_t object_reg;
                System_Type object_type;
                CHECK_AND_RETHROW(jit_stack_pop(ctx, &object_type, &object_reg, NULL));

                // check that the method matches the object
                CHECK(type_is_verifier_assignable_to(object_type, operand_method->DeclaringType));

                // resolve it
                // TODO: does this cover all cases?
                CHECK(method_is_virtual(operand_method));
                operand_method = object_type->VirtualMethods->Data[operand_method->VTableOffset];

                // save the method that we are pushing
                ctx->ftnMethod = operand_method;

                MIR_reg_t ftn_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_UIntPtr, &ftn_reg));

                // save the method that we are pushing
                ctx->ftnMethod = operand_method;

                // get the vtable pointer from the object, it is at the first
                // item for both an interface and an object
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, ftn_reg),
                                             MIR_new_mem_op(mir_ctx, MIR_T_U32,
                                                            offsetof(struct System_Object, vtable),
                                                                    object_reg, 0, 1)));

                // figure offset and the actual method
                int vtable_index;
                if (type_is_interface(object_type)) {
                    // we have an interface on the stack, the vtable is the first element
                    // and the vtable index is exactly as given in the operand
                    vtable_index = operand_method->VTableOffset;
                } else {
                    if (type_is_interface(operand_method->DeclaringType)) {
                        // we want to call an interface method on the object, so resolve it and get the
                        // object inside the object's vtable instead
                        vtable_index = type_get_interface_method_impl(object_type, operand_method)->VTableOffset;
                    } else {
                        // this is a normal virtual method, nothing to resolve
                        vtable_index = operand_method->VTableOffset;
                    }
                }

                ctx->ftnMethod = object_type->VirtualMethods->Data[vtable_index];

                if (type_is_sealed(object_type) || method_is_final(operand_method)) {
                    // this is an instance class which is a sealed class, choose the unboxer form if exists and the
                    // normal one otherwise
                    CHECK_AND_RETHROW(jit_prepare_method(ctx->ctx, ctx->ftnMethod));
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, ftn_reg),
                                                 MIR_new_ref_op(mir_ctx, ctx->ftnMethod->MirUnboxerFunc ?: ctx->ftnMethod->MirFunc)));


                } else {
                    // get the address of the function from the vtable
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, ftn_reg),
                                                 MIR_new_mem_op(mir_ctx, MIR_T_P,
                                                                vtable_index * sizeof(void*),
                                                                ftn_reg, 0, 1)));
                }
            } break;

            case CEE_LDFTN: {
                // push the reference to the function
                MIR_reg_t ftn_reg;
                CHECK_AND_RETHROW(jit_stack_push(ctx, tSystem_UIntPtr, &ftn_reg));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, ftn_reg),
                                             MIR_new_ref_op(mir_ctx, operand_method->MirFunc)));


                // save the method that we are pushing
                ctx->ftnMethod = operand_method;
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Unknown opcode
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            default: {
                CHECK_FAIL("TODO: opcode %s", opcode_info->name);
            } break;
        }

        // reset the temp registers
        ctx->itmp.depth = 0;
        ctx->ftmp.depth = 0;
        ctx->dtmp.depth = 0;

        // store the last opcode
        last_opcode = opcode;
    }

    // make sure that the last instruction is either
    // a return or a branch or a throw
    CHECK(
        last_cf == OPCODE_CONTROL_FLOW_THROW ||
        last_cf == OPCODE_CONTROL_FLOW_BRANCH ||
        last_cf == OPCODE_CONTROL_FLOW_RETURN
    );

    JIT_TRACE(
        TRACE("}");
        TRACE();
    );

#ifdef JIT_TRACE_FINAL
    if (trace_filter(ctx->method)) {
        MIR_output_item(mir_ctx, stdout, mir_func);
    }
#endif

cleanup:
    if (IS_ERROR(err)) {
        ERROR("At method %s", method->MirFunc->u.func->name);
    }

    // free all the memory used for jitting the method
    SAFE_FREE(switch_ops);
    strbuilder_free(&method_name);

    arrfree(arguments);
    arrfree(locals);

    for (int i = 0; i < hmlen(ctx->pc_to_stack_snapshot); i++) {
        arrfree(ctx->pc_to_stack_snapshot[i].stack.entries);
    }
    hmfree(ctx->pc_to_stack_snapshot);

    arrfree(ctx->stack.entries);
    arrfree(ctx->ireg.regs);
    arrfree(ctx->dreg.regs);
    arrfree(ctx->freg.regs);
    arrfree(ctx->itmp.regs);
    arrfree(ctx->dtmp.regs);
    arrfree(ctx->ftmp.regs);

    for (int i = 0; i < hmlen(ctx->finally_chain); i++) {
        arrfree(ctx->finally_chain[i].labels);
    }
    hmfree(ctx->finally_chain);

    return err;
}

#undef MIR_append_insn

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//
// TODO: this can be much cheaper if we just have a
//
//  add rdi, sizeof(struct System_Object)
//  jmp <original method>
//
// but idk if we have a good way to do it in MIR
//
static err_t jit_generate_unboxer(jit_context_t* ctx, System_Reflection_MethodInfo method) {
    err_t err = NO_ERROR;
    MIR_var_t* vars = NULL;
    MIR_op_t* ops = NULL;

    // generate the function name
    strbuilder_t func_name = strbuilder_new();
    method_print_full_name(method, &func_name);
    strbuilder_cstr(&func_name, "$unboxer");

    size_t nres = 1;
    MIR_type_t res_type[2] = {
        MIR_T_P, // exception
        MIR_T_UNDEF, // return value if any
    };

    // handle the return value
    if (method->ReturnType != NULL) {
        res_type[1] = jit_get_mir_type(method->ReturnType);
        if (res_type[1] == MIR_T_BLK) {
            // value type return
            MIR_var_t var = {
                .name = "return_block",
                .type = MIR_T_P, // TODO: do we want to use rblk along size a normal return value
                .size = method->ReturnType->StackSize
            };
            arrpush(vars, var);
        } else {
            // we can use normal return
            nres = 2;
        }
    }

    // the this is always going to be boxed
    MIR_var_t var = {
        .name = "this",
        .type = MIR_T_P,
    };
    arrpush(vars, var);

    for (int i = 0; i < method->Parameters->Length; i++) {
        char name[64];
        snprintf(name, sizeof(name), "arg%d", i);
        MIR_var_t var = {
            .name = _MIR_uniq_string(ctx->ctx, name),
            .type = jit_get_mir_type(method->Parameters->Data[i]->ParameterType),
        };
        if (var.type == MIR_T_BLK) {
            var.size = method->Parameters->Data[i]->ParameterType->StackSize;
        }
        arrpush(vars, var);
    }

    method->MirUnboxerFunc = MIR_new_func_arr(ctx->ctx, strbuilder_get(&func_name), nres, res_type, arrlen(vars), vars);

    // push the prototype and the address
    arrpush(ops, MIR_new_ref_op(ctx->ctx, method->MirProto));
    arrpush(ops, MIR_new_ref_op(ctx->ctx, method->MirFunc));

    // the exception return register
    MIR_reg_t exception_reg = MIR_new_func_reg(ctx->ctx, method->MirUnboxerFunc->u.func, MIR_T_I64, "exception");
    arrpush(ops, MIR_new_reg_op(ctx->ctx, exception_reg));

    // the return value
    MIR_reg_t return_reg = 0;
    if (method->ReturnType != NULL) {
        if (res_type[1] == MIR_T_BLK) {
            // uses an implicit pointer, get it
            MIR_reg_t return_block = MIR_reg(ctx->ctx, "return_block", method->MirUnboxerFunc->u.func);
            arrpush(ops, MIR_new_reg_op(ctx->ctx, return_block));
        } else {
            // uses another thing, get it
            return_reg = MIR_new_func_reg(ctx->ctx, method->MirUnboxerFunc->u.func, MIR_T_I64, "return");
            arrpush(ops, MIR_new_reg_op(ctx->ctx, return_reg));
        }
    }

    // the this argument, increment after the object header to unbox it
    MIR_reg_t this_reg = MIR_reg(ctx->ctx, "this", method->MirUnboxerFunc->u.func);
    MIR_append_insn(ctx->ctx, method->MirUnboxerFunc,
                    MIR_new_insn(ctx->ctx, MIR_ADD,
                                 MIR_new_reg_op(ctx->ctx, this_reg),
                                 MIR_new_reg_op(ctx->ctx, this_reg),
                                 MIR_new_int_op(ctx->ctx, sizeof(struct System_Object))));
    arrpush(ops, MIR_new_reg_op(ctx->ctx, this_reg));

    // parameters
    for (int i = 0; i < method->Parameters->Length; i++) {
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "arg%d", i);
        MIR_reg_t arg_reg = MIR_reg(ctx->ctx, buffer, method->MirUnboxerFunc->u.func);
        System_Type parameter_type = method->Parameters->Data[i]->ParameterType;
        if (type_get_stack_type(parameter_type) == STACK_TYPE_VALUE_TYPE || type_is_interface(parameter_type)) {
            // pass struct by-value
            arrpush(ops, MIR_new_mem_op(ctx->ctx, MIR_T_BLK, method->Parameters->Data[i]->ParameterType->StackSize, arg_reg, 0, 1));
        } else {
            // pass as a register
            arrpush(ops, MIR_new_reg_op(ctx->ctx, arg_reg));
        }
    }

    // actually call the real method now that we have unboxed the this parameter
    MIR_append_insn(ctx->ctx, method->MirUnboxerFunc,
                    MIR_new_insn_arr(ctx->ctx, MIR_CALL, arrlen(ops), ops));

    // return the exception and value
    MIR_append_insn(ctx->ctx, method->MirUnboxerFunc,
                    MIR_new_ret_insn(ctx->ctx, nres,
                                     MIR_new_reg_op(ctx->ctx, exception_reg),
                                     MIR_new_reg_op(ctx->ctx, return_reg)));

    MIR_finish_func(ctx->ctx);

cleanup:
    strbuilder_free(&func_name);
    arrfree(vars);
    arrfree(ops);

    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jitter entry points
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Mutex guarding so there won't be multiple jitting at the same time
 */
static mutex_t m_jit_mutex = INIT_MUTEX();

/**
 * For generating mir module names
 */
static int m_mir_module_gen = 0;

static err_t jit_run_initializer(jit_context_t* ctx, System_Type type) {
    err_t err = NO_ERROR;

    // don't recurse
    if (type->RanTypeInitializer) {
        goto cleanup;
    }
    type->RanTypeInitializer = true;

    // handle dependencies
    int idx = hmgeti(ctx->type_init_dependencies, type);
    if (idx >= 0) {
        System_Type* arr = ctx->type_init_dependencies[idx].value;
        for (int i = 0; i < arrlen(arr); i++) {
            CHECK_AND_RETHROW(jit_run_initializer(ctx, arr[i]));
        }
    }

    // now we are ready to run our initializer
    if (type->TypeInitializer != NULL) {
        System_Exception(*cctor)() = type->TypeInitializer->MirFunc->addr;
        System_Exception exception = cctor();
        CHECK(exception == NULL, "Type initializer for %U: `%U`",
              type->Name, exception->Message);
    }


cleanup:
    return err;
}

#ifdef JIT_DEBUG_SYMBOLS

static void debug_set_gen_interface(MIR_context_t ctx, MIR_item_t func_item) {
    if (func_item == NULL) return; /* finish setting interfaces */
    MIR_gen (ctx, 0, func_item);
    size_t size = debug_get_code_size(func_item->u.func->machine_code);
    debug_create_symbol(func_item->u.func->name, (uintptr_t)func_item->u.func->machine_code, size);
}

#endif

static err_t jit_process(jit_context_t* ctx, MIR_module_t module) {
    err_t err = NO_ERROR;
    jit_method_context_t mctx;

    //------------------------------------------------------------------------------------------------------------------
    // Start by going over all the methods we need to jit and convert them to mir
    //------------------------------------------------------------------------------------------------------------------

    // while we have methods to go over, go over them
    while (arrlen(ctx->methods_to_jit) != 0) {
        System_Reflection_MethodInfo method = arrpop(ctx->methods_to_jit);

        // start by clearing the context
        memset(&mctx, 0, sizeof(mctx));
        mctx.ctx = ctx;
        mctx.method = method;
        ctx->current_method = method;

        // jit the body of the method
        CHECK_AND_RETHROW(jit_method_body(&mctx));

        // generate an unboxer if needed
        if (method_is_virtual(method) && method->DeclaringType->IsValueType) {
            CHECK_AND_RETHROW(jit_generate_unboxer(ctx, method));
        }
    }

    // we are done with the module
    MIR_finish_module(ctx->ctx);

    //------------------------------------------------------------------------------------------------------------------
    // we finished emitting MIR code, now we need to link it
    //------------------------------------------------------------------------------------------------------------------

    // we are now going to link everything nicely
    // move the module to the main context
    MIR_change_module_ctx(ctx->ctx, module, m_mir_context);

    // load the module
    MIR_load_module(m_mir_context, module);

    // set a lazy generation
#ifdef JIT_DEBUG_SYMBOLS
    MIR_link(m_mir_context, debug_set_gen_interface, NULL);
#else
    MIR_link(m_mir_context, MIR_set_parallel_gen_interface, NULL);
#endif

    //------------------------------------------------------------------------------------------------------------------
    // now do the post setup
    //------------------------------------------------------------------------------------------------------------------

    // generate the vtables
    for (int i = 0; i < arrlen(ctx->instance_types); i++) {
        System_Type created_type = ctx->instance_types[i];

        // prepare the vtable for the type
        for (int vi = 0; vi < created_type->VirtualMethods->Length; vi++) {
            // if this has an unboxer use the unboxer instead of the actual method
            System_Reflection_MethodInfo method = created_type->VirtualMethods->Data[vi];
            if (method->MirUnboxerFunc != NULL) {
                created_type->VTable[vi] = method->MirUnboxerFunc->addr;
            } else {
                created_type->VTable[vi] = method->MirFunc->addr;
            }
            ASSERT(created_type->VTable[vi] != NULL);
        }
    }

    // add the gc roots
    for (int i = 0; i < arrlen(ctx->static_types); i++) {
        System_Type created_type = ctx->static_types[i];

        if (created_type->Fields == NULL)
            continue;

        for (int fi = 0; fi < created_type->Fields->Length; fi++) {
            System_Reflection_FieldInfo fieldInfo = created_type->Fields->Data[fi];
            if (!field_is_static(fieldInfo)) continue;
            if (field_is_thread_static(fieldInfo)) continue;
            if (fieldInfo->HasRva) continue;
            if (fieldInfo->MirField->item_type != MIR_bss_item) continue;

            switch (type_get_stack_type(fieldInfo->FieldType)) {
                case STACK_TYPE_O: {
                    gc_add_root(fieldInfo->MirField->addr);
                } break;

                case STACK_TYPE_VALUE_TYPE: {
                    for (int j = 0; j < arrlen(fieldInfo->FieldType->ManagedPointersOffsets); j++) {
                        gc_add_root(fieldInfo->MirField->addr + fieldInfo->FieldType->ManagedPointersOffsets[j]);
                    }
                } break;

                    // ignore
                case STACK_TYPE_INT32:
                case STACK_TYPE_INTPTR:
                case STACK_TYPE_REF:
                case STACK_TYPE_INT64:
                case STACK_TYPE_FLOAT:
                    break;

                default:
                    CHECK_FAIL();
            }
        }
    }

    // now handle initializers
    for (int i = 0; i < arrlen(ctx->static_types); i++) {
        System_Type created_type = ctx->static_types[i];
        CHECK_AND_RETHROW(jit_run_initializer(ctx, created_type));
    }

cleanup:
    return err;
}

err_t jit_type(System_Type type) {
    err_t err = NO_ERROR;

    jit_context_t ctx = { 0 };

    jit_get_mir_context();

    // setup the mir context
    ctx.ctx = MIR_init();

    // prepare the module
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "m%d", m_mir_module_gen++);
    MIR_module_t module = MIR_new_module(ctx.ctx, buffer);

    // process the type as if it is going to be initialized
    CHECK_AND_RETHROW(jit_prepare_instance_type(&ctx, type));

    // process it
    CHECK_AND_RETHROW(jit_process(&ctx, module));

cleanup:
    MIR_finish(ctx.ctx);
    arrfree(ctx.static_types);
    arrfree(ctx.instance_types);
    for (int i = 0; i < hmlen(ctx.type_init_dependencies); i++) {
        arrfree(ctx.type_init_dependencies[i].value);
    }
    hmfree(ctx.type_init_dependencies);

    jit_release_mir_context();

    return err;
}

err_t jit_method(System_Reflection_MethodInfo method) {
    err_t err = NO_ERROR;

    jit_context_t ctx = { 0 };

    jit_get_mir_context();

    // setup the mir context
    ctx.ctx = MIR_init();

    // prepare the module
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "m%d", m_mir_module_gen++);
    MIR_module_t module = MIR_new_module(ctx.ctx, buffer);

    // prepare the method
    CHECK_AND_RETHROW(jit_prepare_method(&ctx, method));

    // process it
    CHECK_AND_RETHROW(jit_process(&ctx, module));

cleanup:
    // clear the context
    MIR_finish(ctx.ctx);
    arrfree(ctx.static_types);
    arrfree(ctx.instance_types);
    for (int i = 0; i < hmlen(ctx.type_init_dependencies); i++) {
        arrfree(ctx.type_init_dependencies[i].value);
    }
    hmfree(ctx.type_init_dependencies);

    jit_release_mir_context();

    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jitter API
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static thread_t* m_jit_mutex_owner = NULL;

static int m_depth = 0;

MIR_context_t jit_get_mir_context() {
    thread_t* current_thread = get_current_thread();
    if (m_jit_mutex_owner != current_thread) {
        // we are not the owner, wait for the lock
        mutex_lock(&m_jit_mutex);
        m_jit_mutex_owner = current_thread;
    }

    // increase the depth
    m_depth++;

    return m_mir_context;
}

void jit_release_mir_context() {
    ASSERT(get_current_thread() == m_jit_mutex_owner);
    if (--m_depth == 0) {
        m_jit_mutex_owner = NULL;
        mutex_unlock(&m_jit_mutex);
    }
}

void jit_dump_method(System_Reflection_MethodInfo method) {
    MIR_output_item(m_mir_context, stdout, method->MirFunc);
}

void jit_dump_context() {
    MIR_output(m_mir_context, stdout);
}
