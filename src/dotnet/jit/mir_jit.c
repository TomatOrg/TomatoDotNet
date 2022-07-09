#include "jit.h"
#include "dotnet/monitor.h"
#include "thread/scheduler.h"
#include "internal_calls.h"

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

// TODO: we need a mir try-catch so we can recover from mir errors

/**
 * Uncomment to remove null-checks, out of memory checks, oob checks and more
 * to make the JITed code a bit more readable
 */
//#define READABLE_JIT


/**
 * Uncomment to make the jit trace the IL opcodes it is trying to figure out
 */
//#define JIT_TRACE

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// functions we need for the runtime
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static MIR_item_t m_dynamic_cast_obj_to_interface_proto = NULL;
static MIR_item_t m_dynamic_cast_obj_to_interface_func = NULL;

static MIR_item_t m_is_instance_proto = NULL;
static MIR_item_t m_is_instance_func = NULL;

static MIR_item_t m_gc_new_proto = NULL;
static MIR_item_t m_gc_new_func = NULL;

static MIR_item_t m_gc_update_proto = NULL;
static MIR_item_t m_gc_update_func = NULL;

static MIR_item_t m_gc_update_ref_proto = NULL;
static MIR_item_t m_gc_update_ref_func = NULL;

static MIR_item_t m_managed_memcpy_proto = NULL;
static MIR_item_t m_managed_memcpy_func = NULL;

static MIR_item_t m_managed_ref_memcpy_proto = NULL;
static MIR_item_t m_managed_ref_memcpy_func = NULL;

static MIR_item_t m_get_array_type_proto = NULL;
static MIR_item_t m_get_array_type_func = NULL;

static MIR_item_t m_memcpy_proto = NULL;
static MIR_item_t m_memcpy_func = NULL;

static MIR_item_t m_memset_proto = NULL;
static MIR_item_t m_memset_func = NULL;

static MIR_item_t m_delegate_ctor_proto = NULL;
static MIR_item_t m_delegate_ctor_func = NULL;

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

// TODO: generate this instead?
static bool dynamic_cast_obj_to_interface(void** dest, System_Object source, System_Type targetInterface) {
    // should only be called after the type checking
    TinyDotNet_Reflection_InterfaceImpl interface = type_get_interface_impl(OBJECT_TYPE(source), targetInterface);
    if (interface == NULL) {
        dest[0] = 0;
        dest[1] = 0;
        return false;
    }

    // set the interface fields
    dest[0] = &source->vtable[interface->VTableOffset];
    dest[1] = source;

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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// init the jit
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * The global context of the MIR jit
 */
static MIR_context_t m_mir_context;

static void jit_generate_System_String_GetCharInternal() {
    const char* fname = "string::GetCharInternal(int32)";
    MIR_type_t res[] = {
        MIR_T_P,
        MIR_T_U16
    };
    MIR_item_t func = MIR_new_func(m_mir_context, fname, 2, res, 2, MIR_T_P, "this", MIR_T_I32, "index");
    MIR_reg_t this = MIR_reg(m_mir_context, "this", func->u.func);
    MIR_reg_t index = MIR_reg(m_mir_context, "index", func->u.func);
    MIR_append_insn(m_mir_context, func,
                    MIR_new_ret_insn(m_mir_context, 2,
                                     MIR_new_int_op(m_mir_context, 0),
                                     MIR_new_mem_op(m_mir_context, MIR_T_I16,
                                                    offsetof(struct System_String, Chars),
                                                    this, index, 2)));
    MIR_finish_func(m_mir_context);
    MIR_new_export(m_mir_context, fname);
}

static void jit_generate_System_Array_GetDataPtr() {
    const char* fname = "[Corelib-v1]System.Array::GetDataPtr()";
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
    const char* fname = "[Corelib-v1]System.Type::GetTypeFromHandle([Corelib-v1]System.RuntimeTypeHandle)";
    MIR_type_t res[] = {
        MIR_T_P,
        MIR_T_P
    };
    MIR_item_t func = MIR_new_func(m_mir_context, fname, 2, res, 1, MIR_T_P, "handle");
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

err_t init_jit() {
    err_t err = NO_ERROR;

    m_mir_context = MIR_init();

    //
    // Initialize all internal functions
    //
   MIR_module_t internal_module = MIR_new_module(m_mir_context, "tinydotnet");

    MIR_type_t res_type = MIR_T_P;
    m_gc_new_proto = MIR_new_proto(m_mir_context, "gc_new$proto", 1, &res_type, 2, MIR_T_P, "type", MIR_T_U64, "size");
    m_gc_new_func = MIR_new_import(m_mir_context, "gc_new");

    m_get_array_type_proto = MIR_new_proto(m_mir_context, "get_array_type$proto", 1, &res_type, 1, MIR_T_P, "type");
    m_get_array_type_func = MIR_new_import(m_mir_context, "get_array_type");

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

    m_memset_proto = MIR_new_proto(m_mir_context, "memset$proto", 0, NULL, 3, MIR_T_P, "dest", MIR_T_I32, "c", MIR_T_U64, "count");
    m_memset_func = MIR_new_import(m_mir_context, "memset");

    res_type = MIR_T_I8;

    m_dynamic_cast_obj_to_interface_proto = MIR_new_proto(m_mir_context, "dynamic_cast_obj_to_interface$proto", 1, &res_type, 3, MIR_T_P, "dest", MIR_T_P, "source", MIR_T_P, "targetInterface");
    m_dynamic_cast_obj_to_interface_func = MIR_new_import(m_mir_context, "dynamic_cast_obj_to_interface");

    m_is_instance_proto = MIR_new_proto(m_mir_context, "isinstance$proto", 1, &res_type, 2, MIR_T_P, "object", MIR_T_P, "type");
    m_is_instance_func = MIR_new_import(m_mir_context, "isinstance");

    m_delegate_ctor_proto = MIR_new_proto(m_mir_context, "delegate_ctor$proto", 0, NULL, 3, MIR_T_P, "this", MIR_T_P, "object", MIR_T_P, "method");

    // generate some builtin methods that we can't properly create in CIL because we don't allow
    // any unsafe code, and it is not worth having them as native functions
    jit_generate_System_String_GetCharInternal();
    jit_generate_System_Array_GetDataPtr();
    jit_generate_System_Type_GetTypeFromHandle();
    jit_generate_delegate_ctor();

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
    MIR_load_external(m_mir_context, "memset", memset_wrapper);
    MIR_load_external(m_mir_context, "managed_memcpy", managed_memcpy);
    MIR_load_external(m_mir_context, "managed_ref_memcpy", managed_ref_memcpy);

    // load internal functions
    for (int i = 0; i < g_internal_calls_count; i++) {
        MIR_load_external(m_mir_context, g_internal_calls[i].target, g_internal_calls[i].impl);
    }

    // init the code gen
    // TODO: for real parallel gen we want to have more generators
    MIR_gen_init(m_mir_context, 1);
    MIR_gen_set_optimize_level(m_mir_context, 0, 4);

#if 0
    MIR_gen_set_debug_file(m_mir_context, 0, stdout);
    MIR_gen_set_debug_level(m_mir_context, 0, 0);
#endif

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// MIR helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Get the MIR type for the given dotnet type, meant to be used for function signatures
 * to create a proper ABI that could be used for other stuff as well
 */
static MIR_type_t get_mir_type(System_Type type) {
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
    } else if (type->IsValueType || type_is_interface(type)) {
        return MIR_T_BLK;
    } else {
        ASSERT(type == NULL || type_is_object_ref(type) || type->IsByRef);
        return MIR_T_P;
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// jitting context (for parallel jitting)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct stack_entry {
    System_Type type;
    bool non_local_ref;
} stack_entry_t;

typedef struct stack {
    // the stack entries
    stack_entry_t* entries;
} stack_t;

typedef struct stack_snapshot {
    int key;
    stack_t stack;
    MIR_label_t label;

    // snapshot of the stack depth on each of them,
    // should total to the stack size
    int ireg_depth;
    int freg_depth;
    int dreg_depth;
} stack_snapshot_t;

typedef struct stack_keeping {
    MIR_reg_t* regs;
    int depth;
} stack_keeping_t;

typedef struct exception_handling {
    System_Reflection_ExceptionHandlingClause key;
    MIR_label_t value;
    MIR_label_t endfinally;
    bool last_in_chain;
} exception_handling_t;

typedef struct jit_context {
    // the mir context for this jit instance
    MIR_context_t ctx;

    // types that we need to set a vtable for
    System_Type* created_types;

    // stack of messages that we need to jit
    System_Reflection_MethodInfo* methods_to_jit;
} jit_context_t;

typedef struct jit_method_context {
    jit_context_t* ctx;

    /********************/
    /* stack management */
    /********************/

    // get a stack snapshot for the given PC
    stack_snapshot_t* pc_to_stack_snapshot;

    // the current stack
    stack_t stack;

    // the actual stack registers
    stack_keeping_t ireg;
    stack_keeping_t freg;
    stack_keeping_t dreg;

    // created temp registers
    stack_keeping_t itmp;
    stack_keeping_t ftmp;
    stack_keeping_t dtmp;

    // used for generating register names that
    // hold the buffer of a value type
    // TODO: maybe use Map<size, Array<reg>> so we can reuse buffers properly
    int value_type_name_gen;

    /**********************/
    /* exception handling */
    /**********************/

    // used to store the exception between catch-clauses
    MIR_reg_t exception_reg;

    // transform a clause to a label
    exception_handling_t* clause_to_label;

    /*******************/
    /* jitting context */
    /*******************/

    // the current il offset
    int il_offset;

    // the method we are dealing with
    System_Reflection_MethodInfo method;
} jit_method_context_t;

// helper
#define mir_ctx ctx->ctx->ctx
#define mir_func ctx->method->MirFunc

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

static MIR_reg_t push_new_reg(jit_method_context_t* ctx, System_Type type, bool temp) {
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

/**
 * Create a new temp register
 */
static MIR_reg_t new_temp_reg(jit_method_context_t* ctx, System_Type type) {
    return push_new_reg(ctx, type, true);
}

/**
 * Push a new item on the stack, returning the stack slot for it as a register
 *
 * This verifies that the stack depth is not too big
 */
static err_t stack_push(jit_method_context_t* ctx, System_Type type, MIR_reg_t* out_reg) {
    err_t err = NO_ERROR;

    if (type != NULL) {
        ASSERT(type->IsFilled);
    }

    // Make sure we don't exceed the stack depth
    CHECK(arrlen(ctx->stack.entries) < ctx->method->MethodBody->MaxStackSize);

    // create the register for the output
    *out_reg = push_new_reg(ctx, type, false);

    // append to the stack
    arrpush(ctx->stack.entries, (stack_entry_t){ .type = type });

cleanup:
    return err;
}

/**
 * Pop an item from the stack, returning its type and register location, will fail
 * if there are not enough items on the stack
 */
static err_t stack_pop(jit_method_context_t* ctx, System_Type* out_type, MIR_reg_t* out_reg, bool* non_local_ref) {
    err_t err = NO_ERROR;

    // pop the entry
    CHECK(arrlen(ctx->stack.entries) > 0);
    stack_entry_t entry = arrpop(ctx->stack.entries);
    System_Type type = entry.type;
    if (out_type != NULL) *out_type = type;
    if (non_local_ref != NULL) *non_local_ref = entry.non_local_ref;

    // get the reg stack
    stack_keeping_t* stack = NULL;
    MIR_type_t stack_type = get_mir_stack_type(type);
    char prefix = '\0';
    switch (stack_type) {
        case MIR_T_BLK: ;
        case MIR_T_I64: stack = &ctx->ireg; break;
        case MIR_T_F: stack = &ctx->freg; break;
        case MIR_T_D: stack = &ctx->dreg; break;
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
        MIR_reg_t real_reg = new_temp_reg(ctx, type);
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_MOV,
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
        }
        // TODO: closest common subtype of S and T
        else {
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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Codegen helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Code used for anything that stores a primitive value from the stack into a field, and mostly handles
 * implicit casting of float->double and double->float
 */
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

/**
 * Code used for anything that loads a primitive value from a field to the stack, mostly
 * handles giving the correct loading opcode
 */
MIR_insn_code_t jit_number_inscode(System_Type type) {
    MIR_insn_code_t code = MIR_MOV;
    if (type == tSystem_Single) {
        code = MIR_FMOV;
    } else if (type == tSystem_Double) {
        code = MIR_DMOV;
    }
    return code;
}

/**
 * Helper for copying memory in MIR.
 *
 * If it is 64 bytes or smaller will emit an in-place memcpy, otherwise it
 * will call the memcpy function.
 */
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

/**
 * Same as memcpy, but for zeroing memory.
 */
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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit span functions
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void jit_generate_System_Span_GetItemInternal(jit_context_t* ctx, System_Reflection_MethodInfo method, MIR_item_t func) {
    System_Type type = method->DeclaringType->GenericArguments->Data[0];

    MIR_reg_t this_reg = MIR_reg(ctx->ctx, "this", func->u.func);
    MIR_reg_t arg0_reg = MIR_reg(ctx->ctx, "arg0", func->u.func);

    MIR_append_insn(ctx->ctx, func,
                    MIR_new_insn(ctx->ctx, MIR_MUL,
                                 MIR_new_reg_op(ctx->ctx, arg0_reg),
                                 MIR_new_reg_op(ctx->ctx, arg0_reg),
                                 MIR_new_int_op(ctx->ctx, type->StackSize)));

    MIR_append_insn(ctx->ctx, func,
                    MIR_new_insn(m_mir_context, MIR_ADD,
                                 MIR_new_reg_op(ctx->ctx, this_reg),
                                 MIR_new_mem_op(ctx->ctx, MIR_T_P,
                                                offsetof(System_Span, Ptr),
                                                this_reg, 0, 1),
                                 MIR_new_reg_op(ctx->ctx, arg0_reg)));

    MIR_append_insn(ctx->ctx, func,
                    MIR_new_ret_insn(ctx->ctx, 2,
                                     MIR_new_int_op(ctx->ctx, 0),
                                     MIR_new_reg_op(ctx->ctx, this_reg)));
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit the Unsafe builtins
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void jit_generate_Unsafe_SizeOf(jit_context_t* ctx, System_Reflection_MethodInfo method, MIR_item_t func) {
    System_Type type = method->GenericArguments->Data[0];
    MIR_append_insn(ctx->ctx, func,
                    MIR_new_ret_insn(ctx->ctx, 2,
                                     MIR_new_int_op(ctx->ctx, 0),
                                     MIR_new_int_op(ctx->ctx, type->StackSize)));
}

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

    MIR_insn_t label_no_next = MIR_new_label(ctx->ctx);

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

/**
 * Which binaries are allowed to have internal calls
 */
static const char* m_allowed_internal_call_assemblies[] = {
    "Corelib.dll",
    "Pentagon.dll"
};

// forward declare
static err_t jit_prepare_type(jit_context_t* ctx, System_Type type);

/**
 * Prepare a method signature and MIR item.
 *
 * If it is a function will also jit it in place
 *
 * This is a NOP if the function was already generated or is in the generation stack
 */
static err_t jit_prepare_method(jit_context_t* ctx, System_Reflection_MethodInfo method) {
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
        CHECK_AND_RETHROW(jit_prepare_type(ctx, method->ReturnType));

        res_type[1] = get_mir_type(method->ReturnType);
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
        ASSERT(declaringType->IsFilled);
        if (declaringType->IsValueType) declaringType = get_by_ref_type(declaringType);

        MIR_var_t var = {
            .name = "this",
            .type = get_mir_type(declaringType),
        };
        if (var.type == MIR_T_BLK) {
            var.type = MIR_T_P;
        }
        this_index = arrlen(vars);
        arrpush(vars, var);
    }

    for (int i = 0; i < method->Parameters->Length; i++) {
        CHECK_AND_RETHROW(jit_prepare_type(ctx, method->Parameters->Data[i]->ParameterType));

        char name[64];
        snprintf(name, sizeof(name), "arg%d", i);
        MIR_var_t var = {
            .name = _MIR_uniq_string(ctx->ctx, name),
            .type = get_mir_type(method->Parameters->Data[i]->ParameterType),
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

    // check how to generate the method itself
    if (method_get_code_type(method) == METHOD_RUNTIME) {
        // right now only delegate types are supported
        CHECK(method->DeclaringType->BaseType == tSystem_MulticastDelegate);

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
            method->MirFunc = MIR_new_func_arr(ctx->ctx, strbuilder_get(&func_name), nres, res_type, arrlen(vars), vars);

            // remove the this so we can have a static prototype
            arrdel(vars, this_index);

            // prepare the signature for the non-
            proto_static_name = strbuilder_new();
            method_print_full_name(method, &proto_static_name);
            strbuilder_cstr(&proto_static_name, "$proto_static");
            MIR_item_t static_invoke_proto = MIR_new_proto_arr(ctx->ctx, strbuilder_get(&proto_static_name), nres, res_type, arrlen(vars), vars);

            // generate the dispatcher
            CHECK_AND_RETHROW(jit_multicast_delegate_invoke(ctx, method, method->MirFunc, static_invoke_proto));
            MIR_finish_func(ctx->ctx);
            MIR_new_export(ctx->ctx, strbuilder_get(&func_name));

        } else {
            CHECK_FAIL();
        }
    } else if (method_get_code_type(method) == METHOD_IL) {
        if (method_is_internal_call(method)) {
            // internal methods have no body
            CHECK(method->MethodBody == NULL);

            // only the corelib is allowed to have internal methods
            bool found = false;
            for (int i = 0; i < ARRAY_LEN(m_allowed_internal_call_assemblies); i++) {
                if (string_equals_cstr(method->Module->Name, m_allowed_internal_call_assemblies[i])) {
                    found = true;
                    break;
                }
            }
            CHECK(found, "Assembly `%U` is not allowed to have internal calls", method->Module->Name);

            //
            // Span<T> has special generic functions we want to generate
            //
            if (method->DeclaringType->GenericTypeDefinition == tSystem_Span) {
                // create the function
                method->MirFunc = MIR_new_func_arr(ctx->ctx, strbuilder_get(&func_name), nres, res_type, arrlen(vars),
                                                   vars);

                // Span has special stuff
                if (string_equals_cstr(method->Name, "GetItemInternal")) {
                    jit_generate_System_Span_GetItemInternal(ctx, method, method->MirFunc);
                } else {
                    CHECK_FAIL();
                }

                MIR_finish_func(ctx->ctx);
                MIR_new_export(ctx->ctx, strbuilder_get(&func_name));

            //
            // Unsafe has special generic functions we want to generate
            //
            } else if (method->DeclaringType == tSystem_Runtime_CompilerServices_Unsafe) {
                // create the function
                method->MirFunc = MIR_new_func_arr(ctx->ctx, strbuilder_get(&func_name), nres, res_type, arrlen(vars),
                                                   vars);

                // Span has special stuff
                if (string_equals_cstr(method->GenericMethodDefinition->Name, "SizeOf")) {
                    jit_generate_Unsafe_SizeOf(ctx, method, method->MirFunc);
                } else {
                    CHECK_FAIL();
                }

                MIR_finish_func(ctx->ctx);
                MIR_new_export(ctx->ctx, strbuilder_get(&func_name));
            } else {
                // create the import for it
                method->MirFunc = MIR_new_import(ctx->ctx, strbuilder_get(&func_name));
            }
        } else {
            // create a function, we will finish it right away and append to it in the future
            method->MirFunc = MIR_new_func_arr(ctx->ctx, strbuilder_get(&func_name), nres, res_type, arrlen(vars), vars);
            MIR_finish_func(ctx->ctx);

            // queue for jit
            arrpush(ctx->methods_to_jit, method);
        }
    } else {
        CHECK_FAIL();
    }

cleanup:
    arrfree(vars);
    strbuilder_free(&proto_name);
    strbuilder_free(&func_name);

    return err;
}

/**
 * Prepares a type so it can be used proprely, mostly requires emitting all
 * the virtual functions so they can be put in a vtable
 */
static err_t jit_prepare_type(jit_context_t* ctx, System_Type type) {
    err_t err = NO_ERROR;

    if (type->MirType != NULL) {
        ASSERT(type->IsFilled);
        goto cleanup;
    }

    // make sure the type is filled, can happen with generics
    // that it is not filled yet
    CHECK_AND_RETHROW(loader_fill_type(type));

    // create the type ref
    strbuilder_t type_name = strbuilder_new();
    type_print_full_name(type, &type_name);
    type->MirType = MIR_new_import(ctx->ctx, strbuilder_get(&type_name));
    MIR_load_external(m_mir_context, strbuilder_get(&type_name), type);
    strbuilder_free(&type_name);

    // setup the base type
    if (type->BaseType != NULL) {
        CHECK_AND_RETHROW(jit_prepare_type(ctx, type->BaseType));
    }

    // setup the element type
    if (type->ElementType != NULL) {
        CHECK_AND_RETHROW(jit_prepare_type(ctx, type->ElementType));
    }

    // setup fields
    if (type->Fields != NULL) {
        for (int i = 0; i < type->Fields->Length; i++) {
            System_Reflection_FieldInfo fieldInfo = type->Fields->Data[i];

            // prepare the field type
            CHECK_AND_RETHROW(jit_prepare_type(ctx, fieldInfo->FieldType));

            // for static fields declare the bss
            if (field_is_static(fieldInfo)) {
                strbuilder_t field_name = strbuilder_new();
                type_print_full_name(fieldInfo->DeclaringType, &field_name);
                strbuilder_cstr(&field_name, "::");
                strbuilder_utf16(&field_name, fieldInfo->Name->Chars, fieldInfo->Name->Length);
                fieldInfo->MirField = MIR_new_bss(ctx->ctx, strbuilder_get(&field_name), fieldInfo->FieldType->StackSize);
                strbuilder_free(&field_name);
            }
        }
    }

    // if the type is not abstract then jit all of its methods
    if (type->Methods != NULL) {
        for (int i = 0; i < type->Methods->Length; i++) {
            System_Reflection_MethodInfo method = type->Methods->Data[i];

            // if this is a generic method then we don't prepare it in here
            // since it needs to be prepared per-instance
            if (method->GenericArguments != NULL) continue;

            // now prepare the method itself
            CHECK_AND_RETHROW(jit_prepare_method(ctx, type->Methods->Data[i]));
        }
    }

    // we just jitted this type, so make sure to setup the vtables and
    // everything else as needed
    arrpush(ctx->created_types, type);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Branching helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Resolves a label to the location we want to jump to, so it can be used for the jump.
 *
 * This also takes care of stack merging for forward jumps and stack verification for backwards
 * jumps, so this could fail if the stacks don't match in a branch point.
 *
 * This takes in the current offset in the il code, and the target offset for the code
 */
static err_t jit_resolve_branch(jit_method_context_t* ctx, int il_target, MIR_label_t* label) {
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
static err_t jit_branch_point(jit_method_context_t* ctx, int il_target, MIR_label_t* label) {
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

// forward decl
static err_t jit_throw_new(jit_method_context_t* ctx, System_Type type);

/**
 * Allocate a new object, testing for out of memory if needed
 *
 * TODO: maybe move the throwing into the allocating function and call it normally?
 */
static err_t jit_new(jit_method_context_t* ctx, MIR_reg_t result, System_Type type, MIR_op_t size) {
    err_t err = NO_ERROR;

    // make sure the type is known, we need this specifically in here
    // so all the exceptions that we throw from the runtime will be
    // added properly
    CHECK_AND_RETHROW(jit_prepare_type(ctx->ctx, type));

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
    if (type != tSystem_OutOfMemoryException) {
        // if we got NULL from the gc_new function it means we got an OOM

        // handle any exception which might have been thrown
        MIR_insn_t label = MIR_new_label(mir_ctx);

        // if we have a non-zero value then skip the throw
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_BT,
                                     MIR_new_label_op(mir_ctx, label),
                                     MIR_new_reg_op(mir_ctx, result)));

        // throw the error, it has an unknown type
        CHECK_AND_RETHROW(jit_throw_new(ctx, tSystem_OutOfMemoryException));

        // insert the skip label
        MIR_append_insn(mir_ctx, mir_func, label);
    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Excpetion jitting helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Given the dotnet clause, will generate a jump to it and setup the stack for it accordingly
 */
static err_t jit_jump_to_exception_clause(jit_method_context_t* ctx, System_Reflection_ExceptionHandlingClause clause) {
    err_t err = NO_ERROR;

    // we have found an exact handler to jump to, jump to it
    int i = hmgeti(ctx->clause_to_label, clause);
    CHECK(i != -1);
    MIR_label_t label = ctx->clause_to_label[i].value;

    if (clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
        // get the stack snapshot so we know which reg stores the stack slot
        // of the pushed exception
        i = hmgeti(ctx->pc_to_stack_snapshot, clause->HandlerOffset);
        CHECK(i != -1);
        stack_t stack = ctx->pc_to_stack_snapshot[i].stack;

        // validate it is the correct one
        CHECK(arrlen(stack.entries) == 1);
        CHECK(stack.entries[0].type == clause->CatchType);

        // move the exception to it
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_MOV,
                                     MIR_new_reg_op(mir_ctx, ctx->ireg.regs[0]),
                                     MIR_new_reg_op(mir_ctx, ctx->exception_reg)));
    }

    // jump to the correct handler
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_JMP,
                                 MIR_new_label_op(mir_ctx, label)));

cleanup:
    return err;
}

/**
 * Will throw an exception of the given type (or NULL if not known).
 *
 * Assumes that the ctx->exception_reg contains the exception isntance
 */
static err_t jit_throw(jit_method_context_t* ctx, System_Type type) {
    err_t err = NO_ERROR;

    // verify it is a valid object
    CHECK(type_is_object_ref(type));

    MIR_reg_t temp_reg = 0;

    // find the exception handler to use
    System_Reflection_ExceptionHandlingClause_Array exceptions = ctx->method->MethodBody->ExceptionHandlingClauses;
    System_Reflection_ExceptionHandlingClause my_clause = NULL;
    for (int i = 0; i < exceptions->Length; i++) {
        System_Reflection_ExceptionHandlingClause clause = exceptions->Data[i];

        // check that this instruction is in the try range
        if (clause->TryOffset > ctx->il_offset || ctx->il_offset >= clause->TryOffset + clause->TryLength)
            continue;

        // if this is a finally or fault block, then we can jump to it directly
        if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT || clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
            my_clause = clause;
            break;
        }

        if (clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
            if (type != NULL) {
                // check if the exception matches anything in here
                System_Type thrown = type;
                while (thrown != NULL) {
                    if (thrown == clause->CatchType) {
                        // found the correct one!
                        break;
                    }

                    // try next
                    thrown = thrown->BaseType;
                }

                if (thrown != NULL) {
                    // we found the correct one!
                    my_clause = clause;
                    break;
                }
            } else {
                // we don't know the exact exception type that
                // is thrown, so we need to handle it dynamically

                // if needed create a temp register to hold the
                // result of the check
                if (temp_reg == 0) {
                    temp_reg = new_temp_reg(ctx, tSystem_Boolean);
                }

                MIR_label_t skip = MIR_new_label(mir_ctx);

                // check if the current instance is dervied
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_call_insn(mir_ctx, 5,
                                                  MIR_new_ref_op(mir_ctx, m_is_instance_proto),
                                                  MIR_new_ref_op(mir_ctx, m_is_instance_func),
                                                  MIR_new_reg_op(mir_ctx, temp_reg),
                                                  MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                                  MIR_new_ref_op(mir_ctx, clause->CatchType->MirType)));

                // check the result, if it was false then skip the jump to the exception handler
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_BF,
                                             MIR_new_label_op(mir_ctx, skip),
                                             MIR_new_reg_op(mir_ctx, temp_reg)));

                // emit the jump the to exception handler
                CHECK_AND_RETHROW(jit_jump_to_exception_clause(ctx, clause));

                // insert the skip label
                MIR_append_insn(mir_ctx, mir_func, skip);
            }
        } else {
            CHECK_FAIL("TODO: filter exception handler");
        }
    }

    if (my_clause == NULL) {
        // check if we need the extra argument or not
        size_t nres = 1;
        if (ctx->method->ReturnType != NULL) {
            MIR_type_t mtype = get_mir_type(ctx->method->ReturnType);
            if (mtype != MIR_T_BLK) {
                nres = 2;
            }
        }

        // we did not have a handler in the current function, just
        // return our own instruction
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_ret_insn(mir_ctx, nres,
                                         MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                         MIR_new_int_op(mir_ctx, 0)));
    } else {
        // we found an exact clause to jump to
        CHECK_AND_RETHROW(jit_jump_to_exception_clause(ctx, my_clause));
    }

cleanup:
    return err;
}

/**
 * Throw a new exception, calling the default ctor, useful for throwing exceptions on any checks
 * that the jit emits
 */
static err_t jit_throw_new(jit_method_context_t* ctx, System_Type type) {
    err_t err = NO_ERROR;

    // call the default ctor
    System_Reflection_MethodInfo ctor = NULL;
    for (int i = 0; i < type->Methods->Length; i++) {
        System_Reflection_MethodInfo mi = type->Methods->Data[i];
        if (method_is_static(mi)) continue;
        if (!method_is_special_name(mi) || !method_is_rt_special_name(mi)) continue;
        if (!string_equals_cstr(mi->Name, ".ctor")) continue;
        if (mi->Parameters->Length != 0) continue;
        if (mi->ReturnType != NULL) continue;
        ctor = mi;
        break;
    }
    CHECK(ctor != NULL);

    // the temp reg for the new obejct
    MIR_reg_t exception_obj = new_temp_reg(ctx, type);

    // allocate the new object
    CHECK_AND_RETHROW(jit_new(ctx, exception_obj, type, MIR_new_int_op(mir_ctx, type->ManagedSize)));

    // call the ctor for it
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_call_insn(mir_ctx, 4,
                                      MIR_new_ref_op(mir_ctx, ctor->MirProto),
                                      MIR_new_ref_op(mir_ctx, ctor->MirFunc),
                                      MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                      MIR_new_reg_op(mir_ctx, exception_obj)));

    MIR_label_t no_exception = MIR_new_label(mir_ctx);

    // check if we need to throw an exception coming from creating this exception
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_BF,
                                 MIR_new_label_op(mir_ctx, no_exception),
                                 MIR_new_reg_op(mir_ctx, ctx->exception_reg)));

    // throw an unknown exception
    CHECK_AND_RETHROW(jit_throw(ctx, NULL));

    // put the label to skip the ctor exception handling
    MIR_append_insn(mir_ctx, mir_func, no_exception);

    // mov the newly created exception to the exception register
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_MOV,
                                 MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                 MIR_new_reg_op(mir_ctx, exception_obj)));

    // throw it nicely
    CHECK_AND_RETHROW(jit_throw(ctx, type));

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Checking for stuff
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Emit a null check, throwing System.NullReferenceException if the value at reg is null
 */
static err_t jit_null_check(jit_method_context_t* ctx, MIR_reg_t reg, System_Type type) {
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
            MIR_reg_t temp_reg = new_temp_reg(ctx, tSystem_Object);
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
static err_t jit_cast_obj_to_interface(jit_method_context_t* ctx,
                                       MIR_reg_t result_reg, MIR_reg_t from_reg,
                                       System_Type from_type, System_Type to_type,
                                       MIR_reg_t this_reg
) {
    err_t err = NO_ERROR;

    TinyDotNet_Reflection_InterfaceImpl interface = type_get_interface_impl(from_type, to_type);
    CHECK(interface != NULL);

    // &object->vtable[vtable_offset]
    MIR_reg_t vtable_reg = new_temp_reg(ctx, tSystem_IntPtr);
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_MOV,
                                 MIR_new_reg_op(mir_ctx, vtable_reg),
                                 MIR_new_mem_op(mir_ctx, MIR_T_P,
                                                offsetof(struct System_Object, vtable),
                                                from_reg, 0, 1)));
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_ADD,
                                 MIR_new_reg_op(mir_ctx, vtable_reg),
                                 MIR_new_reg_op(mir_ctx, vtable_reg),
                                 MIR_new_int_op(mir_ctx, interface->VTableOffset * sizeof(void*))));

    // set the vtable
    MIR_append_insn(mir_ctx, mir_func,
                    MIR_new_insn(mir_ctx, MIR_MOV,
                                 MIR_new_mem_op(mir_ctx, MIR_T_P, 0, result_reg, 0, 1),
                                 MIR_new_reg_op(mir_ctx, vtable_reg)));

    // set the type
    if (this_reg != 0) {
        // we need a write barrier
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_call_insn(mir_ctx, 5,
                                          MIR_new_ref_op(mir_ctx, m_gc_update_proto),
                                          MIR_new_ref_op(mir_ctx, m_gc_update_func),
                                          MIR_new_reg_op(mir_ctx, this_reg),
                                          MIR_new_int_op(mir_ctx, sizeof(void*)),
                                          MIR_new_reg_op(mir_ctx, from_reg)));
    } else {
        // we don't need a write barrier
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_MOV,
                                     MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), result_reg, 0, 1),
                                     MIR_new_reg_op(mir_ctx, from_reg)));
    }

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
    CHECK_AND_RETHROW(stack_pop(ctx, &value2_type, &value2_reg, NULL));
    CHECK_AND_RETHROW(stack_pop(ctx, &value1_type, &value1_reg, NULL));

    MIR_reg_t result_reg = 0;
    MIR_label_t label = NULL;
    if (MIR_branch_code_p(code)) {
        CHECK(il_target >= 0);
        CHECK_AND_RETHROW(jit_branch_point(ctx, il_target, &label));
    } else {
        CHECK_AND_RETHROW(stack_push(ctx, tSystem_Int32, &result_reg));
    }

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

            if (value1_type == tSystem_Single) {
                if (value2_type == tSystem_Single) {
                    // need to do float math
                    code += 2;
                } else if (value2_type == tSystem_Double) {
                    // need to do double math
                    code += 3;

                    // implicit conversion float->double
                    MIR_reg_t value1_double_reg = new_temp_reg(ctx, tSystem_Double);
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_F2D,
                                                 MIR_new_reg_op(mir_ctx, value1_double_reg),
                                                 MIR_new_reg_op(mir_ctx, value1_reg)));
                }
            } else if (value1_type == tSystem_Double) {
                // always double math
                code += 3;

                if (value2_type == tSystem_Single) {
                    // implicit conversion float->double
                    MIR_reg_t value2_double_reg = new_temp_reg(ctx, tSystem_Double);
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_F2D,
                                                 MIR_new_reg_op(mir_ctx, value2_double_reg),
                                                 MIR_new_reg_op(mir_ctx, value2_reg)));
                }
            }
        } break;

        case STACK_TYPE_O: {
            CHECK(type_get_stack_type(value2_type) == STACK_TYPE_O);

            CHECK(
                code == MIR_EQ ||
                code == MIR_BEQ ||
                code == MIR_BNE
            );

            // TODO: handle interface comparison
            if (type_is_interface(value1_type)) CHECK_FAIL();
            if (type_is_interface(value2_type)) CHECK_FAIL();
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
    MIR_reg_t result_reg;
    System_Type value2_type;
    System_Type value1_type;
    CHECK_AND_RETHROW(stack_pop(ctx, &value2_type, &value2_reg, NULL));
    CHECK_AND_RETHROW(stack_pop(ctx, &value1_type, &value1_reg, NULL));

    stack_type_t value1_stacktype = type_get_stack_type(value1_type);
    stack_type_t value2_stacktype = type_get_stack_type(value2_type);

    if (code == MIR_LSH || code == MIR_RSH || code == MIR_URSH) {
        CHECK(value1_stacktype == STACK_TYPE_INT32 || value1_stacktype == STACK_TYPE_INT64 || value1_stacktype == STACK_TYPE_INTPTR);
        CHECK(value2_stacktype == STACK_TYPE_INT32 || value1_stacktype == STACK_TYPE_INTPTR);
        bool is32 = value2_stacktype == STACK_TYPE_INT32;
        if (is32) {
            if (code == MIR_LSH) code = MIR_LSHS;
            else if (code == MIR_RSH) code = MIR_RSHS;
            else if (code == MIR_URSH) code = MIR_URSHS;
        }
        CHECK_AND_RETHROW(stack_push(ctx, value1_type, &result_reg));
    } else {
        if (code == MIR_DIV || code == MIR_UDIV || code == MIR_MOD || code == MIR_UMOD) {
            MIR_insn_t label = MIR_new_label(mir_ctx);

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

        switch (type_get_stack_type(value1_type)) {
            case STACK_TYPE_INT32: {
                if (type_get_stack_type(value2_type) == STACK_TYPE_INT32) {
                    // int32 x int32
                    CHECK_AND_RETHROW(stack_push(ctx, tSystem_Int32, &result_reg));
                    code += 1;
                } else {
                    // int32 x intptr
                    CHECK(type_get_stack_type(value2_type) == STACK_TYPE_INTPTR);
                    CHECK_AND_RETHROW(stack_push(ctx, tSystem_IntPtr, &result_reg));

                    // sign extend the int32 to intptr
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_UEXT32,
                                                 MIR_new_reg_op(mir_ctx, value1_reg),
                                                 MIR_new_reg_op(mir_ctx, value1_reg)));
                }
            } break;

            case STACK_TYPE_INT64: {
                // int64 x int64
                CHECK(type_get_stack_type(value2_type) == STACK_TYPE_INT64);
                CHECK_AND_RETHROW(stack_push(ctx, tSystem_Int64, &result_reg));
            } break;

            case STACK_TYPE_INTPTR: {
                CHECK_AND_RETHROW(stack_push(ctx, tSystem_IntPtr, &result_reg));

                if (type_get_stack_type(value2_type) == STACK_TYPE_INT32) {
                    // intptr x int32
                    // sign extend
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_UEXT32,
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
                        CHECK_AND_RETHROW(stack_push(ctx, tSystem_Single, &result_reg));
                        code += 2;
                    } else {
                        CHECK(value2_type == tSystem_Double);

                        // float x double
                        // convert the float to a double
                        CHECK_AND_RETHROW(stack_push(ctx, tSystem_Double, &result_reg));
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_F2D,
                                                     MIR_new_reg_op(mir_ctx, result_reg),
                                                     MIR_new_reg_op(mir_ctx, value1_reg)));
                        value1_reg = result_reg;
                        code += 3;
                    }
                } else {
                    CHECK(value1_type == tSystem_Double);

                    // this always results in a double math
                    CHECK_AND_RETHROW(stack_push(ctx, tSystem_Double, &result_reg));
                    code += 3;

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

                // not allowed to do math on these
            case STACK_TYPE_VALUE_TYPE:
            case STACK_TYPE_O:
            case STACK_TYPE_REF:
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

/**
 * This is the main jitting function, it takes a method and jits it completely
 */
err_t jit_method(jit_context_t* jctx, System_Reflection_MethodInfo method) {
    err_t err = NO_ERROR;

    // setup the context for this method
    jit_method_context_t _ctx = {
        .ctx = jctx,
        .method = method
    };
    jit_method_context_t* ctx = &_ctx;

    System_Reflection_MethodBody body = method->MethodBody;
    System_Reflection_Assembly assembly = method->Module->Assembly;

    strbuilder_t method_name = strbuilder_new();
    method_print_full_name(method, &method_name);

#ifdef JIT_TRACE
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
#endif

    // variables
    MIR_op_t* locals = NULL;

    // jump table dynamic array
    MIR_op_t *switch_ops = NULL;

    // Create the exception handling reg
    ctx->exception_reg = MIR_new_func_reg(mir_ctx, mir_func->u.func, MIR_T_I64, "exception");

    // get the return block register, if any
    MIR_reg_t return_block_reg = 0;
    if (method->ReturnType != NULL && get_mir_type(method->ReturnType) == MIR_T_BLK) {
        return_block_reg = MIR_reg(mir_ctx, "return_block", mir_func->u.func);
    }

#ifdef JIT_TRACE
    if (body->LocalVariables->Length > 0) {
        TRACE("\t.locals %s(", body->InitLocals ? "init " : "");
    }
#endif

    // actually create locals
    for (int i = 0; i < body->LocalVariables->Length; i++) {
        System_Reflection_LocalVariableInfo variable = body->LocalVariables->Data[i];
        CHECK(variable->LocalIndex == i);

#ifdef JIT_TRACE
        strbuilder_t local_type_name = strbuilder_new();
        type_print_full_name(variable->LocalType, &local_type_name);
        TRACE("\t\t[%d] %s%s", i, strbuilder_get(&local_type_name), i == body->LocalVariables->Length - 1 ? "" : ",");
        strbuilder_free(&local_type_name);
#endif

        // prepare the variable type
        CHECK_AND_RETHROW(jit_prepare_type(ctx->ctx, variable->LocalType));

        // we are going to initialize all of the variables
        char name[64] = { 0 };
        snprintf(name, sizeof(name), "var%d", i);
        MIR_reg_t reg = MIR_new_func_reg(mir_ctx, mir_func->u.func, MIR_T_I64, name);
        arrpush(locals, MIR_new_reg_op(mir_ctx, reg));

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
                locals[i] = MIR_new_mem_op(mir_ctx, get_mir_type(variable->LocalType), 0, reg, 0, 1);
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             locals[i],
                                             MIR_new_int_op(mir_ctx, 0)));
            } break;

            case STACK_TYPE_FLOAT: {
                locals[i] = MIR_new_mem_op(mir_ctx, get_mir_type(variable->LocalType), 0, reg, 0, 1);
                if (variable->LocalType == tSystem_Single) {
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_FMOV,
                                                 locals[i],
                                                 MIR_new_float_op(mir_ctx, 0.0f)));
                } else {
                    ASSERT(variable->LocalType == tSystem_Double);
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_DMOV,
                                                 locals[i],
                                                 MIR_new_double_op(mir_ctx, 0.0)));
                }
            } break;

            init_local_value_type:
            case STACK_TYPE_VALUE_TYPE: {
                jit_emit_zerofill(ctx, reg, variable->LocalType->StackSize);
            } break;
        }
    }

#ifdef JIT_TRACE
    if (body->LocalVariables->Length > 0) {
        TRACE("\t)");
    }
#endif

    // TODO: we need to validate that all branch targets and that all the
    //       try and handler offsets are actually in valid instructions and
    //       not in the middle of instructions

    // prepare the stacks at certain points for exception handling
    bool created_first_entry = false;
    for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
        System_Reflection_ExceptionHandlingClause clause = body->ExceptionHandlingClauses->Data[i];

        // create the stack location
        MIR_label_t label = MIR_new_label(mir_ctx);
        stack_snapshot_t snapshot = {
            .key = clause->HandlerOffset,
            .label = label,
            .stack = { .entries = NULL },
            .ireg_depth = 0,
            .freg_depth = 0,
            .dreg_depth = 0,
        };

        if (clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
            CHECK_AND_RETHROW(jit_prepare_type(ctx->ctx, clause->CatchType));

            // this is an exception caluse, we need to have the exception pushed on the stack
            // prepare it
            arrpush(snapshot.stack.entries, (stack_entry_t) { .type = clause->CatchType });
            snapshot.ireg_depth++;
            if (!created_first_entry) {
                // create the first stack entry just in case, we are going to create a reg
                // and pop it right away since we don't actually want to have it pushed
                // right now, we just wanna make sure that the stack location exists
                push_new_reg(ctx, tSystem_Object, false);
                ctx->ireg.depth--;
                created_first_entry = true;
            }
        }

        // now put it in
        hmputs(ctx->pc_to_stack_snapshot, snapshot);

        // add to label lookup
        hmput(ctx->clause_to_label, clause, label);
    }

#ifdef JIT_TRACE
    int jit_trace_indent = 4;
#endif

    //
    // The main loop for decoding and jitting opcodes
    //
    opcode_control_flow_t last_cf = OPCODE_CONTROL_FLOW_INVALID;
    opcode_t last_opcode = CEE_INVALID;
    System_Reflection_MethodInfo ftnMethod = NULL;
    System_Type constrainedType = NULL;
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
            last_cf == OPCODE_CONTROL_FLOW_THROW
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

#ifdef JIT_TRACE
            if (clause->TryOffset == ctx->il_offset) {
                TRACE("%*s.try", jit_trace_indent, "");
                TRACE("%*s{", jit_trace_indent, "");
                jit_trace_indent += 4;
            } else if (clause->TryOffset + clause->TryLength == ctx->il_offset) {
                jit_trace_indent -= 4;
                TRACE("%*s} // end .try", jit_trace_indent, "");
            }

            if (clause->HandlerOffset == ctx->il_offset) {
                if (clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
                    TRACE("%*scatch %U.%U", jit_trace_indent, "", clause->CatchType->Namespace, clause->CatchType->Name);
                } else if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
                    TRACE("%*sfinally", jit_trace_indent, "");
                } else if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT) {
                    TRACE("%*sfault", jit_trace_indent, "");
                } else if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT) {
                    TRACE("%*sfilter", jit_trace_indent, "");
                }
                TRACE("%*s{", jit_trace_indent, "");
                jit_trace_indent += 4;
            } else if (clause->HandlerOffset + clause->HandlerLength == ctx->il_offset) {
                jit_trace_indent -= 4;
                TRACE("%*s} // end handler", jit_trace_indent, "");
            }
#endif

            if (
                clause->HandlerOffset == ctx->il_offset ||
                clause->HandlerOffset + clause->HandlerLength == ctx->il_offset ||
                clause->TryOffset + clause->TryLength == ctx->il_offset
            ) {
                // entry to handler can only happen from exception, so
                // we can't have any instruction that goes next, that is
                // the same for exiting from handler or protected block
                CHECK(
                    last_cf == OPCODE_CONTROL_FLOW_BRANCH ||
                    last_cf == OPCODE_CONTROL_FLOW_THROW ||
                    last_cf == OPCODE_CONTROL_FLOW_RETURN
                );
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
            opcode_info_t* opcode_info = &g_dotnet_opcodes[opcode];
            CHECK(il_ptr + 1 <= body->Il->Length);

            // setup the new prefix
            opcode_value <<= 8;
            opcode_value |= body->Il->Data[il_ptr++];
            opcode = g_dotnet_opcode_lookup[opcode_value];
            CHECK_ERROR(opcode != CEE_INVALID, ERROR_INVALID_OPCODE);
        }

        // get the opcode info
        opcode_info_t* opcode_info = &g_dotnet_opcodes[opcode];

#ifdef JIT_TRACE
        printf("[*] %*sIL_%04x: %s ", jit_trace_indent, "", ctx->il_offset, opcode_info->name);
#endif

        // set the last control flow to this one
        last_cf = opcode_info->control_flow;

        //--------------------------------------------------------------------------------------------------------------
        // Handle operands of the opcode
        //--------------------------------------------------------------------------------------------------------------

        token_t operand_token = { 0 };
        int32_t operand_i32 = 0;
        int64_t operand_i64 = 0;
        System_Reflection_FieldInfo operand_field = NULL;
        System_Reflection_MethodInfo operand_method = NULL;
        float operand_f32 = 0;
        double operand_f64 = 0;
        System_Type operand_type = NULL;
        System_String operand_string = NULL;
        uint32_t operand_switch_n = 0;
        int32_t* operand_switch_dests = NULL;

        char param[128] = { 0 };
        switch (opcode_info->operand) {
            case OPCODE_OPERAND_InlineBrTarget: {
                operand_i32 = *(int32_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int32_t);
                operand_i32 += il_ptr;

#ifdef JIT_TRACE
                printf("IL_%04x", operand_i32);
#endif
            } break;

            case OPCODE_OPERAND_InlineField: {
                // fetch it
                token_t value = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);

                // resolve it
                CHECK_AND_RETHROW(assembly_get_field_by_token(assembly, value, method->DeclaringType->GenericArguments,
                                                              method->GenericArguments, &operand_field));
                CHECK(operand_field != NULL);

#ifdef JIT_TRACE
                strbuilder_t type_name = strbuilder_new();
                type_print_full_name(operand_field->DeclaringType, &type_name);
                printf("%s::%U", strbuilder_get(&type_name), operand_field->Name);
                strbuilder_free(&type_name);
#endif

                // check we can access it
                CHECK(check_field_accessibility(method, operand_field));

                // make sure we initialized its type
                CHECK_AND_RETHROW(jit_prepare_type(ctx->ctx, operand_field->DeclaringType));
            } break;

            case OPCODE_OPERAND_InlineI: {
                operand_i32 = *(int32_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int32_t);

#ifdef JIT_TRACE
                printf("%d", operand_i32);
#endif
            } break;

            case OPCODE_OPERAND_InlineI8: {
                operand_i64 = *(int64_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int64_t);

#ifdef JIT_TRACE
                printf("%ld", operand_i64);
#endif
            } break;

            case OPCODE_OPERAND_InlineMethod: {
                // fetch it
                token_t value = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);

                // resolve it
                CHECK_AND_RETHROW(assembly_get_method_by_token(assembly, value, method->DeclaringType->GenericArguments,
                                                               method->GenericArguments, &operand_method));
                CHECK(operand_method != NULL);

#ifdef JIT_TRACE
                strbuilder_t type_name = strbuilder_new();
                method_print_full_name(operand_method, &type_name);
                printf("%s", strbuilder_get(&type_name));
                strbuilder_free(&type_name);
#endif

                // check we can access it
                CHECK(check_method_accessibility(method, operand_method),
                      "from %U to %U", method->Name, operand_method->Name);

                // prepare the owning type
                CHECK_AND_RETHROW(jit_prepare_type(ctx->ctx, operand_method->DeclaringType));

                // if the method is a generic instance then we need to handle it separately
                if (operand_method->GenericArguments != NULL) {
                    CHECK_AND_RETHROW(jit_prepare_method(ctx->ctx, operand_method));
                }
            } break;

            case OPCODE_OPERAND_InlineR: {
                operand_f64 = *(double*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(double);

#ifdef JIT_TRACE
                printf("<InlineR>");
#endif
            } break;

            case OPCODE_OPERAND_InlineSig: CHECK_FAIL("TODO: sig support"); break;

            case OPCODE_OPERAND_InlineString: {
                token_t value = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);
                operand_string = assembly_get_string_by_token(assembly, value);
                CHECK(operand_string != NULL);

#ifdef JIT_TRACE
                printf("\"%U\"", operand_string);
#endif
            } break;

            case OPCODE_OPERAND_InlineSwitch: {
                operand_switch_n = *(uint32_t*)&body->Il->Data[il_ptr];
                il_ptr += 4;
                operand_switch_dests = (int32_t*)&body->Il->Data[il_ptr];
                il_ptr += operand_switch_n * 4;

#ifdef JIT_TRACE
                printf("<InlineSwitch>");
#endif
            } break;

            case OPCODE_OPERAND_InlineTok: {
                // fetch it
                operand_token = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);

#ifdef JIT_TRACE
                printf("<InlineTok>");
#endif
            } break;

            case OPCODE_OPERAND_InlineType: {
                // fetch it
                token_t value = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);

                // resolve it
                CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, value, method->DeclaringType->GenericArguments
                        , method->GenericArguments, &operand_type));
                CHECK(operand_type != NULL);

#ifdef JIT_TRACE
                strbuilder_t type_name = strbuilder_new();
                type_print_full_name(operand_type, &type_name);
                printf("%s", strbuilder_get(&type_name));
                strbuilder_free(&type_name);
#endif

                // check it is visible
                CHECK(check_type_visibility(method, operand_type));

                // init it
                CHECK_AND_RETHROW(jit_prepare_type(ctx->ctx, operand_type));
            } break;

            case OPCODE_OPERAND_InlineVar: {
                operand_i32 = *(uint16_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(uint16_t);

#ifdef JIT_TRACE
                printf("%d", operand_i32);
#endif
            } break;

            case OPCODE_OPERAND_ShortInlineBrTarget: {
                operand_i32 = *(int8_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int8_t);
                operand_i32 += il_ptr;

#ifdef JIT_TRACE
                printf("IL_%04x", operand_i32);
#endif
            } break;

            case OPCODE_OPERAND_ShortInlineI: {
                operand_i32 = *(int8_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int8_t);

#ifdef JIT_TRACE
                printf("%d", operand_i32);
#endif
            } break;

            case OPCODE_OPERAND_ShortInlineR: {
                operand_f32 = *(float*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(float);

#ifdef JIT_TRACE
                printf("<ShortInlineR>");
#endif
            } break;

            case OPCODE_OPERAND_ShortInlineVar: {
                operand_i32 = *(uint8_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(uint8_t);

#ifdef JIT_TRACE
                printf("%d", operand_i32);
#endif
            } break;

            case OPCODE_OPERAND_InlineNone:
                break;

            default:
                CHECK_FAIL();
        }

#ifdef JIT_TRACE
        printf("\r\n");
#endif

        //--------------------------------------------------------------------------------------------------------------
        // Handle the opcode
        //--------------------------------------------------------------------------------------------------------------

        // constrained prefix should appear
        // before callvirt opcode only
        if (constrainedType != NULL) {
            CHECK(opcode == CEE_CALLVIRT);
        }

        // after ldftn/ldvirtftn there must be a newobj
        if (ftnMethod != NULL) {
            CHECK(opcode == CEE_NEWOBJ);
        }

        switch (opcode) {

            //----------------------------------------------------------------------------------------------------------
            // Prefixes
            //----------------------------------------------------------------------------------------------------------

            // save the constrained type for use in the next instruction
            case CEE_CONSTRAINED: {
                constrainedType = operand_type;
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

            // unary operations
            case CEE_NEG: {
                MIR_reg_t value_reg;
                System_Type value_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg, NULL));

                MIR_reg_t result_reg;
                CHECK_AND_RETHROW(stack_push(ctx, value_type, &result_reg));

                MIR_insn_code_t code;
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
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg, NULL));

                MIR_reg_t result_reg;
                CHECK_AND_RETHROW(stack_push(ctx, value_type, &result_reg));

                MIR_insn_code_t code;
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

            // TODO: shift operations

            // TODO: checked arithmetic

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
            case CEE_CONV_R_UN: {
                MIR_reg_t reg;
                System_Type type;
                CHECK_AND_RETHROW(stack_pop(ctx, &type, &reg, NULL));

                MIR_reg_t result_reg;
                System_Type result_type;
                switch (opcode) {
                    case CEE_CONV_I1: result_type = tSystem_Int32; break;
                    case CEE_CONV_U1: result_type = tSystem_Int32; break;
                    case CEE_CONV_I2: result_type = tSystem_Int32; break;
                    case CEE_CONV_U2: result_type = tSystem_Int32; break;
                    case CEE_CONV_I4: result_type = tSystem_Int32; break;
                    case CEE_CONV_U4: result_type = tSystem_Int32; break;
                    case CEE_CONV_I8: result_type = tSystem_Int64; break;
                    case CEE_CONV_U8: result_type = tSystem_Int64; break;
                    case CEE_CONV_I: result_type = tSystem_IntPtr; break;
                    case CEE_CONV_U: result_type = tSystem_IntPtr; break;
                    case CEE_CONV_R4: result_type = tSystem_Single; break;
                    case CEE_CONV_R8: result_type = tSystem_Double; break;
                    default: CHECK_FAIL();
                }
                CHECK_AND_RETHROW(stack_push(ctx, result_type, &result_reg));

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
                            default: CHECK_FAIL();
                        }
                    } break;

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
                            case CEE_CONV_R4: code = MIR_I2F; break;
                            case CEE_CONV_R8: code = MIR_I2D; break;
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
                    case STACK_TYPE_VALUE_TYPE:
                    case STACK_TYPE_REF:
                        CHECK_FAIL();
                }

                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, code,
                                             MIR_new_reg_op(mir_ctx, result_reg),
                                             MIR_new_reg_op(mir_ctx, reg)));
            } break;

            case CEE_ISINST:
            case CEE_CASTCLASS:
            case CEE_UNBOX_ANY: {
                MIR_reg_t obj_reg;
                System_Type obj_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &obj_type, &obj_reg, NULL));

                // the object type must always be a ref type for unboxing
                CHECK(obj_type->StackType == STACK_TYPE_O);

                // push it, but now as the new type
                MIR_reg_t obj2_reg;
                CHECK_AND_RETHROW(stack_push(ctx, operand_type, &obj2_reg));

                // temp for the cast result
                MIR_reg_t cast_result_reg = new_temp_reg(ctx, tSystem_Boolean);

                MIR_insn_t cast_success = MIR_new_label(mir_ctx);

                // if this is an interface get the type instance itself
                if (type_is_interface(obj_type)) {
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, obj_reg),
                                                 MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*),
                                                                obj_reg, 0, 1)));
                }

                // call the isinstance method to dynamically check the cast is valid
                if (type_is_interface(operand_type)) {
                    // casting to an interface, use the dynamic_cast_obj_to_interface to do the cast
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_call_insn(mir_ctx, 6,
                                                      MIR_new_ref_op(mir_ctx, m_dynamic_cast_obj_to_interface_proto),
                                                      MIR_new_ref_op(mir_ctx, m_dynamic_cast_obj_to_interface_func),
                                                      MIR_new_reg_op(mir_ctx, cast_result_reg),
                                                      MIR_new_reg_op(mir_ctx, obj2_reg),
                                                      MIR_new_reg_op(mir_ctx, obj_reg),
                                                      MIR_new_ref_op(mir_ctx, operand_type->MirType)));
                } else {
                    // casting to an object, so everything is fine
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_call_insn(mir_ctx, 5,
                                                      MIR_new_ref_op(mir_ctx, m_is_instance_proto),
                                                      MIR_new_ref_op(mir_ctx, m_is_instance_func),
                                                      MIR_new_reg_op(mir_ctx, cast_result_reg),
                                                      MIR_new_reg_op(mir_ctx, obj_reg),
                                                      MIR_new_ref_op(mir_ctx, operand_type->MirType)));
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
                                        MIR_new_insn(mir_ctx, jit_number_inscode(operand_type),
                                                     MIR_new_reg_op(mir_ctx, obj2_reg),
                                                     MIR_new_mem_op(mir_ctx, get_mir_type(operand_type),
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

            case CEE_BOX: {
                System_Type val_type;
                MIR_reg_t val_reg;
                CHECK_AND_RETHROW(stack_pop(ctx, &val_type, &val_reg, NULL));

                // make sure that this is fine
                CHECK(type_is_verifier_assignable_to(val_type, operand_type));

                System_Type boxed_type = get_boxed_type(val_type);

                // we track this as an object now
                MIR_reg_t obj_reg;
                CHECK_AND_RETHROW(stack_push(ctx, boxed_type, &obj_reg));

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
                                        MIR_new_insn(mir_ctx, jit_number_inscode(operand_type),
                                                     MIR_new_mem_op(mir_ctx, get_mir_type(operand_type),
                                                                    tSystem_Object->ManagedSize, obj_reg, 0, 1),
                                                     MIR_new_reg_op(mir_ctx, val_reg)));

                    } break;

                    case STACK_TYPE_VALUE_TYPE: {
                        // memcpy it
                        MIR_reg_t memcpy_base_reg = new_temp_reg(ctx, tSystem_Object);

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
                CHECK_AND_RETHROW(stack_pop(ctx, &addr_type, &addr_reg, NULL));

                // this must be an array
                CHECK(addr_type->IsByRef);

                MIR_reg_t value_reg;

                // for anything which is not ldelem.ref we know the operand_type
                // from the array
                if (operand_type != NULL) {
                    CHECK(type_is_verifier_assignable_to(addr_type->BaseType, operand_type));
                    CHECK_AND_RETHROW(stack_push(ctx, type_get_intermediate_type(operand_type), &value_reg));
                } else {
                    // the type is gotten from the array
                    operand_type = addr_type->BaseType;
                    CHECK_AND_RETHROW(stack_push(ctx, type_get_verification_type(operand_type), &value_reg));
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
                        } else if (operand_type == tSystem_Single) {
                            code = MIR_DMOV;
                        }

                        // we can copy this in a single mov
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_mem_op(mir_ctx, get_mir_type(operand_type),
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
            case CEE_STOBJ: goto cee_stind;
            cee_stind: {
                // pop all the values from the stack
                MIR_reg_t value_reg;
                MIR_reg_t addr_reg;
                System_Type value_type;
                System_Type addr_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg, NULL));
                CHECK_AND_RETHROW(stack_pop(ctx, &addr_type, &addr_reg, NULL));

                // this must be an array
                CHECK(addr_type->IsByRef);

                // for stind.ref the operand type is the same as the
                // byref itself
                if (operand_type == NULL) {
                    operand_type = addr_type->BaseType;
                }

                // validate all the type stuff
                CHECK(type_is_verifier_assignable_to(value_type, operand_type));

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
                                                                            value_type, operand_type,
                                                                            0));
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
                                                     MIR_new_mem_op(mir_ctx, get_mir_type(operand_type),
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
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg, NULL));

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
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg, NULL));

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

                // if the value is invalid then don't take the route and
                // go to the default case
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_BGE,
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

            case CEE_THROW: {
                // get the return argument
                MIR_reg_t obj_reg;
                System_Type obj_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &obj_type, &obj_reg, NULL));

                // free this entirely
                arrfree(ctx->stack.entries);
                ctx->ireg.depth = 0;
                ctx->freg.depth = 0;
                ctx->dreg.depth = 0;

                // check the object is not null
                CHECK_AND_RETHROW(jit_null_check(ctx, obj_reg, obj_type));

                // append the instruction itself
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                             MIR_new_reg_op(mir_ctx, obj_reg)));

                // throw it
                CHECK_AND_RETHROW(jit_throw(ctx, obj_type));
            } break;

            case CEE_LEAVE:
            case CEE_LEAVE_S: {
                // resolve the label
                MIR_label_t target_label;
                CHECK_AND_RETHROW(jit_resolve_branch(ctx, operand_i32, &target_label));

                int last_clausi = -1;

                // we found a leave, we are going to find every finally clause that we are in, and build
                // up a chain of where to go next, if we already have a clause with an entry to go to, we
                // are going to make sure it goes to the same place
                bool in_a_protected_block = false;
                System_Reflection_ExceptionHandlingClause_Array exceptions = body->ExceptionHandlingClauses;
                for (int i = 0; i < exceptions->Length; i++) {
                    System_Reflection_ExceptionHandlingClause clause = exceptions->Data[i];

                    if (clause->HandlerOffset <= ctx->il_offset && ctx->il_offset < clause->HandlerOffset + clause->HandlerLength) {
                        // we are in a handler region, this means that the exception has been dealt with and
                        // we should clear it out so the finally nodes won't think that it might need to do
                        // something with it
                        in_a_protected_block = true;

                        // reset the exception value
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                                     MIR_new_int_op(mir_ctx, 0)));
                    }

                    // make sure we are in this try
                    if (clause->TryOffset > ctx->il_offset || ctx->il_offset >= clause->TryOffset + clause->TryLength)
                        continue;

                    // we are in a try block
                    in_a_protected_block = true;

                    // make sure we are getting a final block
                    if (clause->Flags != COR_ILEXCEPTION_CLAUSE_FINALLY)
                        continue;

                    // lets get the clause label and offset
                    int clausei = hmgeti(ctx->clause_to_label, clause);
                    CHECK(clausei != -1);
                    MIR_label_t finally_label = ctx->clause_to_label[clausei].value;

                    // the current finally clause is going to jump into the target label
                    // (unless it is nested in someone else)
                    ctx->clause_to_label[clausei].endfinally = target_label;
                    ctx->clause_to_label[clausei].last_in_chain = true;

                    if (last_clausi == -1) {
                        // jump to the first finally we see
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_JMP,
                                                     MIR_new_label_op(mir_ctx, finally_label)));
                    } else {
                        // the last clause is going to actually jump to us
                        ctx->clause_to_label[last_clausi].endfinally = finally_label;
                        ctx->clause_to_label[last_clausi].last_in_chain = false;
                    }

                    last_clausi = clausei;
                }

                // make sure we are in a try region
                CHECK(in_a_protected_block);

                if (last_clausi == -1) {
                    // there is no finally around us, we can
                    // safely jump to the target
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_JMP,
                                                 MIR_new_label_op(mir_ctx, target_label)));

                }
            } break;

            case CEE_ENDFINALLY: {
                // find the finally block we are in
                bool found = false;
                System_Reflection_ExceptionHandlingClause_Array exceptions = body->ExceptionHandlingClauses;
                for (int i = 0; i < exceptions->Length; i++) {
                    System_Reflection_ExceptionHandlingClause clause = exceptions->Data[i];

                    // make sure we are in this try
                    if (clause->HandlerOffset > ctx->il_offset || ctx->il_offset >= clause->HandlerOffset + clause->HandlerLength)
                        continue;

                    // make sure we are getting a final block
                    CHECK (clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY);

                    // lets get the clause label and offset
                    int clausei = hmgeti(ctx->clause_to_label, clause);
                    CHECK(clausei != -1);
                    MIR_label_t endfinally_label = ctx->clause_to_label[clausei].endfinally;
                    CHECK(endfinally_label != NULL);

                    if (ctx->clause_to_label[clausei].last_in_chain) {
                        MIR_label_t skip = MIR_new_label(mir_ctx);

                        // add a check if we need to "rethrow" the error instead
                        // check the result, if it was false then skip the jump to the exception handler
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_BF,
                                                     MIR_new_label_op(mir_ctx, skip),
                                                     MIR_new_reg_op(mir_ctx, ctx->exception_reg)));

                        // figure how many we need
                        size_t nres = 1;
                        if (ctx->method->ReturnType != NULL) {
                            MIR_type_t mtype = get_mir_type(ctx->method->ReturnType);
                            if (mtype != MIR_T_BLK) {
                                nres = 2;
                            }
                        }

                        // we did not have a handler in the current function, just
                        // return our own instruction
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_ret_insn(mir_ctx, nres,
                                                         MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                                         MIR_new_int_op(mir_ctx, 0)));

                        // insert the skip label
                        MIR_append_insn(mir_ctx, mir_func, skip);
                    }

                    // jump to the first finally we see
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_JMP,
                                                 MIR_new_label_op(mir_ctx, endfinally_label)));

                    found = true;
                    break;
                }

                CHECK(found);
            } break;

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
                MIR_reg_t value_reg;
                System_Type value_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg, NULL));

                // get the variable
                CHECK(operand_i32 < body->LocalVariables->Length);
                System_Reflection_LocalVariableInfo variable = body->LocalVariables->Data[operand_i32];
                System_Type variable_type = type_get_intermediate_type(variable->LocalType);

                // check the type is valid
                CHECK(type_is_verifier_assignable_to(value_type, variable_type));

                switch (type_get_stack_type(value_type)) {
                    case STACK_TYPE_O: {
                        if (type_is_interface(variable_type)) {
                            if (type_is_interface(value_type)) {
                                // interface -> interface
                                goto stloc_value_type;
                            } else {
                                // object -> interface
                                CHECK(locals[operand_i32].mode == MIR_OP_REG);
                                CHECK_AND_RETHROW(jit_cast_obj_to_interface(ctx,
                                                                            locals[operand_i32].u.reg, value_reg,
                                                                            value_type, variable_type, 0));
                            }
                        } else {
                            if (type_is_interface(value_type)) {
                                // interface -> object
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_MOV,
                                                             locals[operand_i32],
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
                    case STACK_TYPE_FLOAT:
                    case STACK_TYPE_REF: {
                        MIR_insn_code_t code = jit_number_cast_inscode(value_type, variable_type);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     locals[operand_i32],
                                                     MIR_new_reg_op(mir_ctx, value_reg)));
                    } break;

                    stloc_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        CHECK(locals[operand_i32].mode == MIR_OP_REG);
                        jit_emit_memcpy(ctx, locals[operand_i32].u.reg, value_reg, value_type->StackSize);
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
                CHECK_AND_RETHROW(stack_push(ctx, value_type, &value_reg));

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
                    case STACK_TYPE_FLOAT:
                    case STACK_TYPE_REF: {
                        MIR_insn_code_t code = jit_number_inscode(value_type);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     locals[operand_i32]));
                    } break;

                    ldloc_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        CHECK(locals[operand_i32].mode == MIR_OP_REG);
                        jit_emit_memcpy(ctx, value_reg, locals[operand_i32].u.reg, value_type->StackSize);
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
                CHECK_AND_RETHROW(stack_push(ctx, value_type, &value_reg));

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
                    case STACK_TYPE_FLOAT:
                    case STACK_TYPE_REF: {
                        CHECK(locals[operand_i32].mode == MIR_OP_MEM);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_reg_op(mir_ctx, locals[operand_i32].u.mem.base)));
                    } break;

                    ldloca_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     locals[operand_i32]));
                    } break;
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arguments
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // TODO: STARG

            case CEE_LDARG_0:
            case CEE_LDARG_1:
            case CEE_LDARG_2:
            case CEE_LDARG_3: operand_i32 = opcode - CEE_LDARG_0;
            case CEE_LDARG_S:
            case CEE_LDARG: {
                char arg_name_buf[64];
                const char* arg_name = NULL;

                // resolve the type
                System_Type arg_type = NULL;
                if (!method_is_static(method)) {
                    if (operand_i32 == 0) {
                        arg_name = "this";
                        arg_type = method->DeclaringType;
                        if (arg_type->IsValueType) {
                            // value types turn into a by-ref when using this
                            arg_type = get_by_ref_type(arg_type);
                        }
                    }
                    operand_i32--;
                }

                // if this is not `this` then get the name
                if (arg_name == NULL) {
                    snprintf(arg_name_buf, sizeof(arg_name_buf), "arg%d", operand_i32);
                    arg_name = arg_name_buf;
                }

                if (arg_type == NULL) {
                    CHECK(operand_i32 < method->Parameters->Length);
                    arg_type = method->Parameters->Data[operand_i32]->ParameterType;
                }

                // the register containing the value
                MIR_reg_t arg_reg = MIR_reg(mir_ctx, arg_name, mir_func->u.func);

                // Get the stack type of the arg
                System_Type arg_stack_type = type_get_intermediate_type(arg_type);

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(stack_push(ctx, arg_stack_type, &value_reg));

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
                        MIR_insn_code_t code = jit_number_inscode(arg_stack_type);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_reg_op(mir_ctx, arg_reg)));
                    } break;

                    ldarg_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        jit_emit_memcpy(ctx, value_reg, arg_reg, arg_stack_type->StackSize);
                    } break;

                    case STACK_TYPE_REF:
                        // mark that this is a non-local ref type, as it comes from the outside
                        ctx->stack.entries[arrlen(ctx->stack.entries) - 1].non_local_ref = true;
                        goto ldarg_primitive_type;
                }
            } break;

            case CEE_LDARGA:
            case CEE_LDARGA_S: {
                char arg_name_buf[64];
                const char* arg_name = NULL;

                // resolve the type
                System_Type arg_type = NULL;
                if (!method_is_static(method)) {
                    if (operand_i32 == 0) {
                        arg_name = "this";
                        arg_type = method->DeclaringType;
                        if (arg_type->IsValueType) {
                            // value types turn into a by-ref when using this
                            arg_type = get_by_ref_type(arg_type);
                        }
                    }
                    operand_i32--;
                }

                // if this is not `this` then get the name
                if (arg_name == NULL) {
                    snprintf(arg_name_buf, sizeof(arg_name_buf), "arg%d", operand_i32);
                    arg_name = arg_name_buf;
                }

                if (arg_type == NULL) {
                    CHECK(operand_i32 < method->Parameters->Length);
                    arg_type = method->Parameters->Data[operand_i32]->ParameterType;
                }

                // the register containing the value
                MIR_reg_t arg_reg = MIR_reg(mir_ctx, arg_name, mir_func->u.func);

                // Get the value type
                System_Type value_type = get_by_ref_type(type_get_verification_type(arg_type));

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(stack_push(ctx, value_type, &value_reg));

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
                    case STACK_TYPE_FLOAT:
                    case STACK_TYPE_REF: {
                        CHECK_FAIL("TODO: spill arguments from registers");
                    } break;

                    ldarga_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_reg_op(mir_ctx, arg_reg)));
                    } break;
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
                CHECK_AND_RETHROW(stack_push(ctx, tSystem_Int32, &sr));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, sr),
                                             MIR_new_int_op(mir_ctx, operand_i32)));
            } break;

            case CEE_LDC_I8: {
                MIR_reg_t reg;
                CHECK_AND_RETHROW(stack_push(ctx, tSystem_Int64, &reg));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, reg),
                                             MIR_new_int_op(mir_ctx, operand_i64)));
            } break;

            case CEE_LDC_R4: {
                MIR_reg_t reg;
                CHECK_AND_RETHROW(stack_push(ctx, tSystem_Single, &reg));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_FMOV,
                                             MIR_new_reg_op(mir_ctx, reg),
                                             MIR_new_float_op(mir_ctx, operand_f32)));
            } break;

            case CEE_LDC_R8: {
                MIR_reg_t reg;
                CHECK_AND_RETHROW(stack_push(ctx, tSystem_Double, &reg));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_DMOV,
                                             MIR_new_reg_op(mir_ctx, reg),
                                             MIR_new_double_op(mir_ctx, operand_f64)));
            } break;

            case CEE_LDSTR: {
                // push a string type
                MIR_reg_t string_reg;
                CHECK_AND_RETHROW(stack_push(ctx, tSystem_String, &string_reg));

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
                CHECK_AND_RETHROW(stack_push(ctx, NULL, &null_reg));

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
                        CHECK_AND_RETHROW(jit_prepare_type(ctx->ctx, type));

                        // push it
                        CHECK_AND_RETHROW(stack_push(ctx, tSystem_RuntimeTypeHandle, &runtime_handle_reg));

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
                CHECK_AND_RETHROW(stack_pop(ctx, &top_type, &top_reg, NULL));

                // create new two values
                MIR_reg_t value_1;
                MIR_reg_t value_2;
                CHECK_AND_RETHROW(stack_push(ctx, top_type, &value_1));
                CHECK_AND_RETHROW(stack_push(ctx, top_type, &value_2));

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
                        MIR_insn_code_t code = jit_number_inscode(top_type);
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
                CHECK_AND_RETHROW(stack_pop(ctx, NULL, NULL, NULL));
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Field access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STSFLD: {
                // get the top value
                MIR_reg_t value_reg;
                System_Type value_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg, NULL));

                // get the field type, ignoring stuff like enums
                System_Type field_type = type_get_underlying_type(operand_field->FieldType);

                // make sure the field is static
                CHECK(field_is_static(operand_field));

                // if this is an init-only field then make sure that
                // only rtspecialname can access it (.ctor and .cctor)
                if (field_is_init_only(operand_field)) {
                    CHECK(method_is_rt_special_name(method));
                }

                // validate the assignability
                CHECK(type_is_verifier_assignable_to(value_type, operand_field->FieldType));

                // have the reference in a register for easy access
                MIR_reg_t field_reg = new_temp_reg(ctx, tSystem_IntPtr);
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, field_reg),
                                             MIR_new_ref_op(mir_ctx, operand_field->MirField)));
                MIR_op_t field_op = MIR_new_mem_op(mir_ctx, get_mir_type(field_type), 0, field_reg, 0, 1);

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
                                                                            value_type, field_type, 0));
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

                // Get the field type
                System_Type field_stack_type = type_get_intermediate_type(operand_field->FieldType);
                System_Type field_type = type_get_underlying_type(operand_field->FieldType);

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(stack_push(ctx, field_stack_type, &value_reg));

                // have the reference in a register for easy access
                MIR_reg_t field_reg = new_temp_reg(ctx, tSystem_IntPtr);
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, field_reg),
                                             MIR_new_ref_op(mir_ctx, operand_field->MirField)));
                MIR_op_t field_op = MIR_new_mem_op(mir_ctx, get_mir_type(field_type), 0, field_reg, 0, 1);

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
                        } else if (field_type == tSystem_Single) {
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

                // Get the field type
                System_Type field_stack_type = get_by_ref_type(type_get_verification_type(operand_field->FieldType));
                System_Type field_type = type_get_underlying_type(operand_field->FieldType);

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(stack_push(ctx, field_stack_type, &value_reg));
                ctx->stack.entries[arrlen(ctx->stack.entries) - 1].non_local_ref = true; // static field, not local

                // very simple, just move the reference to the value field
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, value_reg),
                                             MIR_new_ref_op(mir_ctx, operand_field->MirField)));
            } break;

            case CEE_STFLD: {
                // get the values
                MIR_reg_t obj_reg;
                MIR_reg_t value_reg;
                System_Type obj_type;
                System_Type value_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg, NULL));
                CHECK_AND_RETHROW(stack_pop(ctx, &obj_type, &obj_reg, NULL));

                // validate that the object type is a valid one for stfld
                if (type_get_stack_type(obj_type) == STACK_TYPE_REF) {
                    // this is a reference, so it has to be a reference to a value type
                    // note that we can't know if the value type is part of another class
                    // or not so we have to use gc_update_ref
                    CHECK(obj_type->BaseType->IsValueType);
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
                    CHECK(method_is_rt_special_name(method));
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
                                CHECK_AND_RETHROW(jit_cast_obj_to_interface(ctx,
                                                                            obj_reg, value_reg,
                                                                            value_type, field_type,
                                                                            obj_reg));
                            }
                        } else {
                            // storing to an object from an object, use a write-barrier
                            if (type_get_stack_type(obj_type) == STACK_TYPE_O) {

                                // check for interface -> object casting
                                if (type_is_interface(value_type)) {
                                    MIR_append_insn(mir_ctx, mir_func,
                                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                                 MIR_new_reg_op(mir_ctx, value_reg),
                                                                 MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), value_reg, 0, 1)));
                                }

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
                                                                    get_mir_type(operand_field->FieldType),
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
                CHECK_AND_RETHROW(stack_pop(ctx, &obj_type, &obj_reg, NULL));

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
                CHECK_AND_RETHROW(stack_push(ctx, field_stack_type, &value_reg));

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
                        } else if (field_type == tSystem_Single) {
                            insn = MIR_DMOV;
                        }

                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, insn,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_mem_op(mir_ctx,
                                                                    get_mir_type(operand_field->FieldType),
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
                bool obj_non_local_ref;
                System_Type obj_type;
                MIR_reg_t obj_reg;
                CHECK_AND_RETHROW(stack_pop(ctx, &obj_type, &obj_reg, &obj_non_local_ref));

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
                System_Type field_type = type_get_underlying_type(operand_field->FieldType);

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(stack_push(ctx, field_stack_type, &value_reg));

                // check if this comes as an outside reference
                bool non_stack_local = false;
                if (type_get_stack_type(obj_type) == STACK_TYPE_O) {
                    // heap object
                    non_stack_local = true;
                } else if (type_get_stack_type(obj_type) == STACK_TYPE_REF) {
                    // check if the reference is a non-value type
                    CHECK(type_is_value_type(obj_type->BaseType));
                    non_stack_local = obj_non_local_ref;
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

            //
            // we are going to do NEWOBJ in here as well, because it is essentially like a call
            // but we create the object right now instead of getting it from the stack, so I
            // think this will remove alot of duplicate code if we just handle it in here
            //

            case CEE_NEWOBJ:
            case CEE_CALLVIRT:
            case CEE_CALL: {
                System_Type ret_type = type_get_underlying_type(operand_method->ReturnType);

                // count the amount of arguments, +1 if we have a this
                int arg_count = operand_method->Parameters->Length;
                bool aggressive_inlining = method_is_aggressive_inlining(operand_method);

                if (opcode == CEE_NEWOBJ) {
                    // newobj must call a ctor, we verify that ctors are good
                    // in the loader
                    CHECK(method_is_rt_special_name(operand_method));
                    CHECK(string_equals_cstr(operand_method->Name, ".ctor"));

                    if (ftnMethod != NULL) {
                        // we had an ldftn/ldvirtftb, meaning that we are going to
                        // create a delegate now, make sure of that
                        CHECK(operand_method->DeclaringType->BaseType == tSystem_MulticastDelegate);
                        CHECK(operand_method->Parameters->Length == 2);
                        CHECK(operand_method->Parameters->Data[0]->ParameterType == tSystem_Object);
                        CHECK(operand_method->Parameters->Data[1]->ParameterType == tSystem_IntPtr);

                        // verify that the method signature matches the delegate we
                        // want to create
                        System_Reflection_MethodInfo signature = operand_method->DeclaringType->DelegateSignature;
                        CHECK(signature != NULL);
                        CHECK(signature->ReturnType == ftnMethod->ReturnType);
                        CHECK(signature->Parameters->Length == ftnMethod->Parameters->Length);
                        for (int i = 0; i < signature->Parameters->Length; i++) {
                            CHECK(signature->Parameters->Data[i]->ParameterType == ftnMethod->Parameters->Data[i]->ParameterType);
                        }
                    } else {
                        // make sure that this is *NOT* a delegate
                        CHECK(operand_method->DeclaringType->BaseType != tSystem_MulticastDelegate);
                    }
                } else if (opcode == CEE_CALLVIRT) {
                    // callvirt must call an instance methods
                    CHECK(!method_is_static(operand_method));
                } else {
                    // call must call a method with a body
                    CHECK(!method_is_abstract(operand_method));
                }

                // prepare array of all the operands
                // 1st is the prototype
                // 2nd is the reference
                // 3rd is exception return
                // 4rd is return type (optionally)
                // 5th is this type (optionally)
                // Rest are the arguments
                size_t other_args = 3;
                if (ret_type != NULL) other_args++;
                if (!method_is_static(operand_method)) other_args++;
                MIR_op_t arg_ops[other_args + arg_count];

                // to track if we passed local refs
                bool ret_is_non_local_ref = true;

                // pop all the arguments from the stack
                int i;
                for (i = arg_count + other_args - 1; i >= other_args; i--) {
                    System_Type signature_type = operand_method->Parameters->Data[i - other_args]->ParameterType;

                    // get the argument value
                    bool arg_non_local_ref;
                    MIR_reg_t arg_reg;
                    System_Type arg_type;
                    CHECK_AND_RETHROW(stack_pop(ctx, &arg_type, &arg_reg, &arg_non_local_ref));

                    // do implicit conversion as needed
                    bool mem_op = false;
                    switch (type_get_stack_type(arg_type)) {
                        case STACK_TYPE_O: {
                            if (type_is_interface(signature_type)) {
                                if (!type_is_interface(arg_type)) {
                                    // object --> interface
                                    CHECK_FAIL("TODO: cast to interface");
                                }

                                // pass by value
                                mem_op = true;
                            } else {
                                if (type_is_interface(arg_type)) {
                                    // interface --> object
                                    MIR_append_insn(mir_ctx, mir_func,
                                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                                     MIR_new_reg_op(mir_ctx, arg_reg),
                                                                     MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), arg_reg, 0, 1)));
                                }
                            }
                        } break;

                        case STACK_TYPE_INT32: {
                            if (
                                signature_type == tSystem_SByte ||
                                signature_type == tSystem_Byte ||
                                signature_type == tSystem_Boolean ||
                                signature_type == tSystem_Int16 ||
                                signature_type == tSystem_UInt16 ||
                                signature_type == tSystem_Char
                            ) {
                                // truncate, going to be done implicitly by mir
                                arg_type = signature_type;
                            } else if (signature_type == tSystem_IntPtr) {
                                // sign extend
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_EXT32,
                                                             MIR_new_reg_op(mir_ctx, arg_reg),
                                                             MIR_new_reg_op(mir_ctx, arg_reg)));
                                arg_type = signature_type;
                            } else if (signature_type == tSystem_UIntPtr) {
                                // zero extend
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_UEXT32,
                                                             MIR_new_reg_op(mir_ctx, arg_reg),
                                                             MIR_new_reg_op(mir_ctx, arg_reg)));
                                arg_type = signature_type;
                            }
                        } break;

                        case STACK_TYPE_INTPTR: {
                            if (type_is_integer(signature_type)) {
                                // truncate or nop, we don't really care
                                arg_type = signature_type;
                            }
                        } break;

                        case STACK_TYPE_FLOAT: {
                            // handle implicit float casting
                            if (arg_type == tSystem_Single) {
                                if (signature_type == tSystem_Double) {
                                    // float->double conversion
                                    MIR_reg_t real_arg_reg = new_temp_reg(ctx, tSystem_Double);
                                    MIR_append_insn(mir_ctx, mir_func,
                                                    MIR_new_insn(mir_ctx, MIR_F2D,
                                                                 MIR_new_reg_op(mir_ctx, real_arg_reg),
                                                                 MIR_new_reg_op(mir_ctx, arg_reg)));
                                    arg_reg = real_arg_reg;
                                    arg_type = signature_type;
                                }
                            } else if (arg_type == tSystem_Double) {
                                if (signature_type == tSystem_Single) {
                                    // double->float conversion
                                    MIR_reg_t real_arg_reg = new_temp_reg(ctx, tSystem_Single);
                                    MIR_append_insn(mir_ctx, mir_func,
                                                    MIR_new_insn(mir_ctx, MIR_D2F,
                                                                 MIR_new_reg_op(mir_ctx, real_arg_reg),
                                                                 MIR_new_reg_op(mir_ctx, arg_reg)));
                                    arg_reg = real_arg_reg;
                                    arg_type = signature_type;
                                }
                            }
                        } break;

                        case STACK_TYPE_REF: {
                            // if this is a reference that might be returned from the method we are calling
                            // to, and the argument is a local reference on its own, then we need to mark
                            // that the  reference type can't be trusted
                            if (!arg_non_local_ref && type_is_verifier_assignable_to(arg_type, method->ReturnType)) {
                                ret_is_non_local_ref = false;
                            }
                        } break;

                        // nothing to do
                        case STACK_TYPE_INT64: break;

                        // in mir when calling
                        case STACK_TYPE_VALUE_TYPE: {
                            mem_op = true;
                        } break;

                        default:
                            CHECK_FAIL();
                    }

                    // we are creating a new delegate and this is the target parameter
                    if (i - other_args == 0 && ftnMethod != NULL) {
                        if (!method_is_static(ftnMethod)) {
                            // this is an instance method, emit a null check on the target
                            // to make sure that it is not null
                            CHECK_AND_RETHROW(jit_null_check(ctx, arg_reg, arg_type));
                        } else {
                            // this is a static method, we need a null target, if already null
                            // ignore it, otherwise just zero the reg
                            if (arg_type != NULL) {
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_MOV,
                                                             MIR_new_reg_op(mir_ctx, arg_reg),
                                                             MIR_new_int_op(mir_ctx, 0)));
                            }
                        }

                        // make sure to reset it now
                        ftnMethod = NULL;
                    }

                    // set the op, for anything passed by value we need to use MIR_T_BLK with the disp
                    // being the size instead of the displacement
                    if (mem_op) {
                        arg_ops[i] = MIR_new_mem_op(mir_ctx, MIR_T_BLK, arg_type->StackSize, arg_reg, 0, 1);
                    } else {
                        arg_ops[i] = MIR_new_reg_op(mir_ctx, arg_reg);
                    }

                    // verify a normal argument
                    CHECK(type_is_verifier_assignable_to(arg_type, signature_type));
                }

                // handle the `this` argument
                MIR_reg_t number_reg;
                MIR_reg_t this_reg;
                System_Type this_type;
                if (!method_is_static(operand_method)) {
                    if (opcode == CEE_NEWOBJ) {
                        // this is the this_type
                        this_type = operand_method->DeclaringType;

                        // make sure this is a type we can actually create
                        CHECK(!type_is_abstract(this_type));
                        CHECK(!type_is_interface(this_type));

                        CHECK_AND_RETHROW(stack_push(ctx, operand_method->DeclaringType, &this_reg));

                        if (this_type->IsValueType) {
                            if (type_get_stack_type(this_type) != STACK_TYPE_VALUE_TYPE) {
                                // this is an integer/float type, so allocate it on the stack
                                // so we can pass it as a reference and then just copy it into
                                // the eval stack as a normal variable

                                // save the position on the eval stack
                                number_reg = this_reg;

                                // set a temp new location
                                this_reg = new_temp_reg(ctx, tSystem_IntPtr);
                                MIR_prepend_insn(mir_ctx, mir_func,
                                                 MIR_new_insn(mir_ctx, MIR_ALLOCA,
                                                              MIR_new_reg_op(mir_ctx, this_reg),
                                                              MIR_new_int_op(mir_ctx, operand_method->DeclaringType->StackSize)));
                            }

                            // For a value type we just need to zero it out before calling the ctor
                            jit_emit_zerofill(ctx, this_reg, this_type->StackSize);
                        } else {
                            // allocate the new object
                            CHECK_AND_RETHROW(jit_new(ctx,
                                                      this_reg, operand_method->DeclaringType,
                                                      MIR_new_int_op(mir_ctx, operand_method->DeclaringType->ManagedSize)));
                        }
                    } else {
                        // this is a call, get it from the stack
                        CHECK_AND_RETHROW(stack_pop(ctx, &this_type, &this_reg, NULL));

                        // Value types have their this as a by-ref
                        System_Type signature_this_type = operand_method->DeclaringType;
                        if (signature_this_type->IsValueType) {
                            signature_this_type = get_by_ref_type(signature_this_type);
                        }

                        if (constrainedType != NULL) {
                            CHECK(this_type->IsByRef);
                            CHECK(this_type->BaseType == constrainedType);

                            // If this_type is a reference type (as opposed to a value type)
                            if (type_is_object_ref(constrainedType)) {
                                // ptr is dereferenced and passed as the ‘this’ pointer to the callvirt of method
                                this_type = constrainedType;
                                MIR_append_insn(mir_ctx, mir_func,
                                                MIR_new_insn(mir_ctx, MIR_MOV,
                                                                MIR_new_reg_op(mir_ctx, this_reg),
                                                                MIR_new_mem_op(mir_ctx, MIR_T_P, 0, this_reg, 0, 1)));
                            } else {
                                // this is a value type, the signature this is a reference
                                signature_this_type = get_by_ref_type(constrainedType);
                            }

                            // get the static dispatch, the call later will
                            // actually handle making sure this is correct
                            CHECK(operand_method->VTableOffset < constrainedType->VirtualMethods->Length);

                            // figure the real method for this dispatch
                            int vtable_offset = operand_method->VTableOffset;
                            if (type_is_interface(operand_method->DeclaringType)) {
                                // comes from interface, need to give a static offset
                                TinyDotNet_Reflection_InterfaceImpl impl = type_get_interface_impl(constrainedType, operand_method->DeclaringType);
                                CHECK(impl != NULL);
                                vtable_offset += impl->VTableOffset;
                            }
                            operand_method = constrainedType->VirtualMethods->Data[vtable_offset];

                            // clear the type
                            constrainedType = NULL;
                        }

                        // verify a normal argument
                        if (signature_this_type == tSystem_Object && this_type->BaseType == tSystem_ValueType) {
                            // this is an edge case, we are going to politely ignore it
                            // in short, for whatever reason the compiler generates a ctor in System.ValueType
                            // that calls the ctor of System.Object ????
                        } else {
                            CHECK(type_is_verifier_assignable_to(this_type, signature_this_type));
                        }

                        // make sure that the object is not null, only if not a byref
                        if (!this_type->IsByRef) {
                            CHECK_AND_RETHROW(jit_null_check(ctx, this_reg, this_type));
                        }
                    }

                    arg_ops[i] = MIR_new_reg_op(mir_ctx, this_reg);
                }

                // get the MIR signature and address
                arg_ops[0] = MIR_new_ref_op(mir_ctx, operand_method->MirProto);

                // byref uses static dispatch since we know the exact type always
                if (
                    opcode == CEE_CALLVIRT &&
                    method_is_virtual(operand_method)
                ) {
                    // we are using callvirt and this is a virtual method, so we have to
                    // use a dynamic dispatch

                    MIR_reg_t temp_reg = new_temp_reg(ctx, tSystem_Type);

                    // get the vtable pointer from the object, it is at the first
                    // item for both an interface and an object
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, temp_reg),
                                                 MIR_new_mem_op(mir_ctx, MIR_T_P, 0, this_reg, 0, 1)));

                    // figure offset and the actual method
                    int vtable_index;
                    if (type_is_interface(this_type)) {
                        // we have an interface on the stack, the vtable is the first element
                        // and the vtable index is exactly as given in the operand
                        vtable_index = operand_method->VTableOffset;

                        // read the actual instance pointer of the interface, so we can use it
                        // when calling the function
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, this_reg),
                                                     MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), this_reg, 0, 1)));
                    } else {
                        if (type_is_interface(operand_method->DeclaringType)) {
                            // we want to call an interface method on the object, so resolve it and get the
                            // object inside the object's vtable instead
                            vtable_index = type_get_interface_method_impl(this_type, operand_method)->VTableOffset;
                        } else {
                            // this is a normal virtual method, nothing to resolve
                            vtable_index = operand_method->VTableOffset;
                        }
                    }

                    if (this_type->IsByRef) {
                        // we have a ref on the stack, which means it must be a value type, so we can call the actual
                        // method directly since all value types are sealed by default
                        CHECK(type_is_sealed(this_type->BaseType));
                        arg_ops[1] = MIR_new_ref_op(mir_ctx, this_type->BaseType->VirtualMethods->Data[vtable_index]->MirFunc);
                    } else if (type_is_sealed(this_type)) {
                        // this is an instance class which is a sealed class, choose the unboxer form if exists and the
                        // normal one otherwise
                        System_Reflection_MethodInfo m = this_type->VirtualMethods->Data[vtable_index];
                        arg_ops[1] = MIR_new_ref_op(mir_ctx, m->MirUnboxerFunc ?: m->MirFunc);
                    } else {
                        // get the address of the function from the vtable
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, temp_reg),
                                                     MIR_new_mem_op(mir_ctx, MIR_T_P,
                                                                    vtable_index * sizeof(void*),
                                                                    temp_reg, 0, 1)));

                        // indirect call
                        arg_ops[1] = MIR_new_reg_op(mir_ctx, temp_reg);
                    }
                } else {
                    // static dispatch
                    arg_ops[1] = MIR_new_ref_op(mir_ctx, operand_method->MirFunc);
                }

                // get it to the exception register
                arg_ops[2] = MIR_new_reg_op(mir_ctx, ctx->exception_reg);

                // emit the IR
                if (operand_method->ReturnType != NULL) {
                    MIR_reg_t ret_reg;
                    CHECK_AND_RETHROW(stack_push(ctx, type_get_intermediate_type(operand_method->ReturnType), &ret_reg));

                    if (type_get_stack_type(operand_method->ReturnType) == STACK_TYPE_REF) {
                        // we did not pass any local references to this, so we know for sure it can't be a local address
                        // being returned from the method
                        ctx->stack.entries[arrlen(ctx->stack.entries) - 1].non_local_ref = ret_is_non_local_ref;
                    }

                    // this should just work, because if the value is a struct it is going to be allocated properly
                    // in the stack push, and it is going to be passed by a pointer that we give, and everything will
                    // just work out because of how we have the order of everything :)
                    arg_ops[3] = MIR_new_reg_op(mir_ctx, ret_reg);
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn_arr(mir_ctx, aggressive_inlining ? MIR_INLINE : MIR_CALL,
                                                     other_args + arg_count,
                                                     arg_ops));
                } else {
                    // Does not have a return argument, no need to handle
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn_arr(mir_ctx, aggressive_inlining ? MIR_INLINE : MIR_CALL,
                                                     other_args + arg_count,
                                                     arg_ops));
                }

                // handle any exception which might have been thrown
                MIR_insn_t label = MIR_new_label(mir_ctx);

                // if we have a zero value skip the return
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_BF,
                                             MIR_new_label_op(mir_ctx, label),
                                             MIR_new_reg_op(mir_ctx, ctx->exception_reg)));

                // throw the error, it has an unknown type
                CHECK_AND_RETHROW(jit_throw(ctx, NULL));

                // insert the skip label
                MIR_append_insn(mir_ctx, mir_func, label);

                // check if we need to copy the left out value from the stack
                // to the eval stack
                if (
                    opcode == CEE_NEWOBJ &&
                    operand_method->DeclaringType->IsValueType &&
                    type_get_stack_type(operand_method->DeclaringType) != STACK_TYPE_VALUE_TYPE
                ) {
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, jit_number_inscode(operand_method->DeclaringType),
                                                 MIR_new_reg_op(mir_ctx, number_reg),
                                                 MIR_new_mem_op(mir_ctx, get_mir_type(operand_method->DeclaringType), 0, this_reg, 0, 1)));
                }
            } break;

            case CEE_INITOBJ: {
                System_Type dest_type;
                MIR_reg_t dest_reg;
                CHECK_AND_RETHROW(stack_pop(ctx, &dest_type, &dest_reg, NULL));

                CHECK(dest_type->IsByRef);
                CHECK(type_is_verifier_assignable_to(operand_type, dest_type->BaseType));

                jit_emit_zerofill(ctx, dest_reg, operand_type->StackSize);
            } break;

            case CEE_RET: {
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
                    CHECK_AND_RETHROW(stack_pop(ctx, &ret_type, &ret_arg, NULL));

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
                                                                                ret_type, method_ret_type,
                                                                                0));

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
                            // first make sure that the return value is a non-local reference
                            CHECK(ctx->stack.entries[arrlen(ctx->stack.entries)].non_local_ref);

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
                CHECK_AND_RETHROW(stack_pop(ctx, &num_elems_type, &num_elems_reg, NULL));

                // make sure it has a valid type
                CHECK(num_elems_type == tSystem_Int32);

                // push the array type
                MIR_reg_t array_reg;
                CHECK_AND_RETHROW(stack_push(ctx, get_array_type(operand_type), &array_reg));

                // calculate the size we are going to need:
                //  num_elems * sizeof(value_type) + sizeof(System.Array)
                MIR_reg_t size_reg = new_temp_reg(ctx, tSystem_Int64);
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MUL,
                                             MIR_new_reg_op(mir_ctx, size_reg),
                                             MIR_new_reg_op(mir_ctx, num_elems_reg),
                                             MIR_new_int_op(mir_ctx, operand_type->StackSize)));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_ADD,
                                             MIR_new_reg_op(mir_ctx, size_reg),
                                             MIR_new_reg_op(mir_ctx, size_reg),
                                             MIR_new_int_op(mir_ctx, tSystem_Array->ManagedSize)));

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
                CHECK_AND_RETHROW(stack_pop(ctx, &array_type, &array_reg, NULL));

                // this must be an array
                CHECK(array_type->IsArray);

                // check the object is not null
                CHECK_AND_RETHROW(jit_null_check(ctx, array_reg, array_type));

                // push the length
                MIR_reg_t length_reg;
                CHECK_AND_RETHROW(stack_push(ctx, tSystem_IntPtr, &length_reg));

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
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg, NULL));
                CHECK_AND_RETHROW(stack_pop(ctx, &index_type, &index_reg, NULL));
                CHECK_AND_RETHROW(stack_pop(ctx, &array_type, &array_reg, NULL));

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
                                                             MIR_new_int_op(mir_ctx, tSystem_Array->ManagedSize)));


                                // from an object, cast required, need a write barrier
                                CHECK_AND_RETHROW(jit_cast_obj_to_interface(ctx,
                                                                            index_reg, value_reg,
                                                                            value_type, operand_type,
                                                                            array_reg));
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
                                                         MIR_new_int_op(mir_ctx, tSystem_Array->ManagedSize)));

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
                                                     MIR_new_mem_op(mir_ctx, get_mir_type(operand_type),
                                                                    tSystem_Array->ManagedSize,
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
                                                     MIR_new_int_op(mir_ctx, tSystem_Array->ManagedSize)));
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_ADD,
                                                     MIR_new_reg_op(mir_ctx, array_reg),
                                                     MIR_new_reg_op(mir_ctx, index_reg),
                                                     MIR_new_reg_op(mir_ctx, array_reg)));

                        if (arrlen(value_type->ManagedPointersOffsets) == 0) {
                            // can use a simple memcpy
                            jit_emit_memcpy(ctx, array_reg, value_reg, operand_type->StackSize);
                        } else {
                            // has pointers, use managed memcpy
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_call_insn(mir_ctx, 5,
                                                              MIR_new_ref_op(mir_ctx, m_managed_ref_memcpy_proto),
                                                              MIR_new_ref_op(mir_ctx, m_managed_ref_memcpy_func),
                                                              MIR_new_reg_op(mir_ctx, array_reg),
                                                              MIR_new_ref_op(mir_ctx, operand_type->MirType),
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
                MIR_reg_t value_reg;
                MIR_reg_t index_reg;
                MIR_reg_t array_reg;
                System_Type index_type;
                System_Type array_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &index_type, &index_reg, NULL));
                CHECK_AND_RETHROW(stack_pop(ctx, &array_type, &array_reg, NULL));

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
                CHECK_AND_RETHROW(stack_push(ctx, type_get_intermediate_type(operand_type), &value_reg));

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
                        } else if (operand_type == tSystem_Single) {
                            code = MIR_DMOV;
                        }

                        // we can copy this in a single mov
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, code,
                                                     MIR_new_reg_op(mir_ctx, value_reg),
                                                     MIR_new_mem_op(mir_ctx, get_mir_type(operand_type),
                                                                    tSystem_Array->ManagedSize,
                                                                    array_reg, index_reg, operand_type->StackSize)));
                    } break;

                    ldelem_value_type:
                    case STACK_TYPE_VALUE_TYPE: {
                        CHECK_FAIL("TODO: struct value load from array");
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
                CHECK_AND_RETHROW(stack_pop(ctx, &index_type, &index_reg, NULL));
                CHECK_AND_RETHROW(stack_pop(ctx, &array_type, &array_reg, NULL));

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
                CHECK_AND_RETHROW(stack_push(ctx, get_by_ref_type(type_get_intermediate_type(operand_type)), &value_reg));
                ctx->stack.entries[arrlen(ctx->stack.entries) - 1].non_local_ref = true;

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
                                             MIR_new_int_op(mir_ctx, tSystem_Array->ManagedSize)));

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

            case CEE_LDFTN:
            case CEE_LDVIRTFTN:
            {

                if (opcode == CEE_LDVIRTFTN) {
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
                    CHECK_AND_RETHROW(stack_pop(ctx, &object_type, &object_reg, NULL));

                    // check that the method matches the object
                    CHECK(type_is_verifier_assignable_to(object_type, operand_method->DeclaringType));

                    // resolve it
                    CHECK(method_is_virtual(operand_method));
                    operand_method = object_type->VirtualMethods->Data[operand_method->VTableOffset];
                    CHECK(method_is_final(operand_method));
                }

                // push the reference to the function
                MIR_reg_t ftn_reg;
                CHECK_AND_RETHROW(stack_push(ctx, tSystem_UIntPtr, &ftn_reg));
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, ftn_reg),
                                             MIR_new_ref_op(mir_ctx, operand_method->MirFunc)));


                // save the method that we are pushing
                ftnMethod = operand_method;
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

#ifdef JIT_TRACE
    TRACE("}");
    TRACE();
#endif

cleanup:
    if (IS_ERROR(err)) {
        MIR_output_item(mir_ctx, stdout, mir_func);
    }

    // free all the memory used for jitting the method
    SAFE_FREE(switch_ops);
    strbuilder_free(&method_name);
    arrfree(locals);
    for (int i = 0; i < hmlen(ctx->pc_to_stack_snapshot); i++) {
        arrfree(ctx->pc_to_stack_snapshot[i].stack.entries);
    }
    arrfree(ctx->stack.entries);
    arrfree(ctx->ireg.regs);
    arrfree(ctx->dreg.regs);
    arrfree(ctx->freg.regs);
    arrfree(ctx->itmp.regs);
    arrfree(ctx->dtmp.regs);
    arrfree(ctx->ftmp.regs);
    hmfree(ctx->pc_to_stack_snapshot);
    hmfree(ctx->clause_to_label);

    return err;
}

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
        CHECK_AND_RETHROW(jit_prepare_type(ctx, method->ReturnType));

        res_type[1] = get_mir_type(method->ReturnType);
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
        CHECK_AND_RETHROW(jit_prepare_type(ctx, method->Parameters->Data[i]->ParameterType));

        char name[64];
        snprintf(name, sizeof(name), "arg%d", i);
        MIR_var_t var = {
            .name = _MIR_uniq_string(ctx->ctx, name),
            .type = get_mir_type(method->Parameters->Data[i]->ParameterType),
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
    if (nres > 1) {
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

/**
 * Mutex guarding so there won't be multiple jitting at the same time
 */
static mutex_t m_jit_mutex = INIT_MUTEX();

/**
 * For generating mir module names
 */
static int m_mir_module_gen = 0;

MIR_context_t jit_get_mir_context() {
    mutex_lock(&m_jit_mutex);
    return m_mir_context;
}

void jit_release_mir_context() {
    mutex_unlock(&m_jit_mutex);
}

err_t jit_type(System_Type type) {
    err_t err = NO_ERROR;

    // TODO: figure if we can move this lock from here or not
    mutex_lock(&m_jit_mutex);

    jit_context_t ctx = {
        .ctx = MIR_init(),
    };

    // prepare the module
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "m%d", m_mir_module_gen++);
    MIR_module_t module = MIR_new_module(ctx.ctx, buffer);

    // start from the type we want
    CHECK_AND_RETHROW(jit_prepare_type(&ctx, type));

    // now jit all the methods as they come in
    while (arrlen(ctx.methods_to_jit) != 0) {
        System_Reflection_MethodInfo method = arrpop(ctx.methods_to_jit);
        CHECK_AND_RETHROW(jit_method(&ctx, method));

        // generate an unboxer
        if (method_is_virtual(method) && method->DeclaringType->IsValueType) {
            CHECK_AND_RETHROW(jit_generate_unboxer(&ctx, method));
        }
    }

    // we are done with the module
    MIR_finish_module(ctx.ctx);

    // move the module to the main context
    MIR_change_module_ctx(ctx.ctx, module, m_mir_context);

    // load the module
    MIR_load_module(m_mir_context, module);

    // link it
    MIR_link(m_mir_context, MIR_set_lazy_gen_interface, NULL);

    // now that everything is linked prepare all the types we have created
    for (int i = 0; i < arrlen(ctx.created_types); i++) {
        System_Type created_type = ctx.created_types[i];

        // prepare the vtable for the type
        if (created_type->VirtualMethods != NULL && !type_is_abstract(created_type) && !type_is_interface(created_type)) {
            for (int vi = 0; vi < created_type->VirtualMethods->Length; vi++) {
                // if this has an unboxer use the unboxer instead of the actual method
                System_Reflection_MethodInfo method = created_type->VirtualMethods->Data[vi];
                if (method->MirUnboxerFunc != NULL) {
                    created_type->VTable[vi] = method->MirUnboxerFunc->addr;
                } else {
                    created_type->VTable[vi] = method->MirFunc->addr;
                }
            }
        }

        // add the gc roots for the types
        if (created_type->Fields != NULL) {
            for (int fi = 0; fi < created_type->Fields->Length; fi++) {
                System_Reflection_FieldInfo fieldInfo = created_type->Fields->Data[fi];
                if (!field_is_static(fieldInfo)) continue;
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
    }

    // and finally, we can run all the ctors that should run from this
    for (int i = arrlen(ctx.created_types) - 1; i >= 0; i--) {
        System_Type created_type = ctx.created_types[i];

        // call the ctor
        if (created_type->StaticCtor != NULL) {
            System_Exception(*cctor)() = created_type->StaticCtor->MirFunc->addr;
            System_Exception exception = cctor();
            CHECK(exception == NULL, "Type initializer for %U: `%U`",
                  created_type->Name, exception->Message);
        }
    }

cleanup:
    if (IS_ERROR(err)) {
        // we need to finish the module just so we can finish the context
        MIR_finish_module(ctx.ctx);
    }

    // we no longer need the old module
    MIR_finish(ctx.ctx);

    if (IS_ERROR(err)) {
        // we failed to load it, so first make sure to remove any
        // reference to the mir types
        for (int i = 0; i < arrlen(ctx.created_types); i++) {
            System_Type created_type = ctx.created_types[i];

            // remove the type setup
            created_type->MirType = NULL;

            // TODO: if this is a generic type, then make sure to remove it if we are not using it
            // TODO: I am not sure how we will do it but whatever.

            // remove all methods setup
            for (int vi = 0; vi < created_type->Methods->Length; vi++) {
                if (created_type->Methods->Data[i] == NULL) continue;
                created_type->Methods->Data[i]->MirFunc = NULL;
                created_type->Methods->Data[i]->MirProto = NULL;
            }

            // remove the field setup
            for (int vi = 0; vi < created_type->Fields->Length; vi++) {
                if (created_type->Fields->Data[i] == NULL) continue;
                if (field_is_static(created_type->Fields->Data[i])) {
                    created_type->Fields->Data[i]->MirField = NULL;
                }
            }
        }
    }

    // unlock the mutex
    mutex_unlock(&m_jit_mutex);


        // free all the arrays we need
    arrfree(ctx.created_types);

    return err;
}

void jit_dump_method(System_Reflection_MethodInfo method) {
    MIR_output_item(m_mir_context, stdout, method->MirFunc);
}

void jit_dump_context() {
    MIR_output(m_mir_context, stdout);
}
