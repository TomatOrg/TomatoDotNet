#include "jit.h"
#include "dotnet/monitor.h"
#include "thread/scheduler.h"

#include <dotnet/opcodes.h>
#include <dotnet/types.h>
#include <dotnet/gc/gc.h>
#include <dotnet/loader.h>
#include <dotnet/gc/heap.h>

#include <util/except.h>
#include <util/stb_ds.h>
#include <time/timer.h>

#include <mir/mir-gen.h>
#include <mir/mir.h>

#include <stdint.h>
#include <stddef.h>

// TODO: we need a mir try-catch so we can recover from mir errors

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

static bool dynamic_cast_obj_to_interface(void** dest, System_Object source, System_Type targetInterface) {
    // should only be called after the type checking
    TinyDotNet_Reflection_InterfaceImpl interface = type_get_interface_impl(source->vtable->type, targetInterface);
    if (interface == NULL) {
        dest[0] = 0;
        dest[1] = 0;
        return false;
    }

    // set the interface fields
    dest[0] = &source->vtable->virtual_functions[interface->VTableOffset];
    dest[1] = source;

    return true;
}

/**
 * memcpy a struct to an heap object, taking care of any memory barrier that should happen
 * while we are copying it
 */
static void managed_memcpy(System_Object this, System_Type struct_type, int offset, void* from) {
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
// Internal Calls
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static method_result_t System_Object_GetType(System_Object this) {
    return (method_result_t){ .exception = NULL, .value = (uintptr_t) this->vtable->type};
}

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

    if (type_get_stack_type(elementType) == STACK_TYPE_VALUE_TYPE) {
        // copying an array, copy each item as a managed memcpy
        // TODO: fast path for copying non-managed struct arrays
        for (int i = 0; i < length; i++) {
            void* src = *(void**)((sourceArray + 1) + (sourceIndex + i) * elementSize);
            size_t destOffset = sizeof(struct System_Array) + (destinationIndex + i) * elementSize;
            managed_memcpy((System_Object)destinationArray, elementType, destOffset, src);
        }
    } else if (type_get_stack_type(elementType) == STACK_TYPE_O) {
        // we are copying objects, need to use a membarrier for each
        // TODO: maybe do group copies/barriers instead of one by one
        for (int i = 0; i < length; i++) {
            void* src = *(void**)((sourceArray + 1) + (sourceIndex + i) * elementSize);
            size_t destOffset = sizeof(struct System_Array) + (destinationIndex + i) * elementSize;
            gc_update(destinationArray, destOffset, src);
        }
    } else {
        // normal memcpy, no need to do anything special
        void* src = (sourceArray + 1) + (sourceIndex) * elementSize;
        void* dest = (destinationArray + 1) + (destinationIndex) * elementSize;
        memcpy(dest, src, length * elementSize);
    }

    return NULL;
}

typedef struct internal_call {
    const char* target;
    void* impl;
} internal_call_t;

static internal_call_t m_internal_calls[] = {
    {
        "[Corelib-v1]System.Object::GetType()",
        System_Object_GetType,
    },
    {
        "[Corelib-v1]System.Array::ClearInternal([Corelib-v1]System.Array,[Corelib-v1]System.Int32,[Corelib-v1]System.Int32)",
        System_Array_ClearInternal,
    },
    {
        "[Corelib-v1]System.Array::CopyInternal([Corelib-v1]System.Array,[Corelib-v1]System.Int64,[Corelib-v1]System.Array,[Corelib-v1]System.Int64,[Corelib-v1]System.Int64)",
        System_Array_CopyInternal,
    }
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// init the jit
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * The global context of the MIR jit
 */
static MIR_context_t m_mir_context;

err_t init_jit() {
    err_t err = NO_ERROR;

    m_mir_context = MIR_init();

    //
    // Initialize all internal functions
    //
   MIR_module_t internal_module = MIR_new_module(m_mir_context, "jit_internal");

    MIR_type_t res_type = MIR_T_P;
    m_gc_new_proto = MIR_new_proto(m_mir_context, "gc_new$proto", 1, &res_type, 2, MIR_T_P, "type", MIR_T_U64, "size");
    m_gc_new_func = MIR_new_import(m_mir_context, "gc_new");

    m_get_array_type_proto = MIR_new_proto(m_mir_context, "get_array_type$proto", 1, &res_type, 1, MIR_T_P, "type");
    m_get_array_type_func = MIR_new_import(m_mir_context, "get_array_type");

    m_gc_update_proto = MIR_new_proto(m_mir_context, "gc_update$proto", 0, NULL, 3, MIR_T_P, "o", MIR_T_U64, "idx", MIR_T_P, "new");
    m_gc_update_func = MIR_new_import(m_mir_context, "gc_update");

    m_gc_update_ref_proto = MIR_new_proto(m_mir_context, "gc_update_ref$proto", 0, NULL, 2, MIR_T_P, "o", MIR_T_P, "new");
    m_gc_update_ref_func = MIR_new_import(m_mir_context, "gc_update_ref");

    m_managed_memcpy_proto = MIR_new_proto(m_mir_context, "managed_memcpy$proto", 0, NULL, 4, MIR_T_P, "this", MIR_T_P, "struct_type", MIR_T_I32, "offset", MIR_T_P, "from");
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
    for (int i = 0; i < ARRAY_LEN(m_internal_calls); i++) {
        MIR_load_external(m_mir_context, m_internal_calls[i].target, m_internal_calls[i].impl);
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
    // the type of the stack entry
    System_Type type;

    // the location where this value
    // is stored on the stack
    MIR_reg_t reg;
} stack_entry_t;

typedef struct stack {
    // the stack entries
    stack_entry_t* entries;
} stack_t;

typedef struct stack_snapshot {
    int key;
    stack_t stack;
    MIR_label_t label;
} stack_snapshot_t;

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

    // used for generating register names
    int reg_name_gen;
} jit_method_context_t;

// helper
#define mir_ctx ctx->ctx->ctx
#define mir_func ctx->method->MirFunc

/**
 * Create a new register for the given type, can be used for temporaries as
 * well as for stack slots
 */
static MIR_reg_t new_reg(jit_method_context_t* ctx, System_Type type) {
    // create the name
    char name[64];
    snprintf(name, sizeof(name), "s%d", ++ctx->reg_name_gen);

    // create the reg
    MIR_reg_t reg;

    // choose the type
    MIR_type_t mir_type = MIR_T_UNDEF;
    if (type == NULL) {
        // NULL reference probably
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

    if (mir_type == MIR_T_BLK) {
        // special case, value type that needs alloca
        reg = MIR_new_func_reg(mir_ctx, mir_func->u.func, MIR_T_I64, name);
        MIR_prepend_insn(mir_ctx, mir_func,
                         MIR_new_insn(mir_ctx, MIR_ALLOCA,
                                      MIR_new_reg_op(mir_ctx, reg),
                                      MIR_new_int_op(mir_ctx, type->StackSize)));
    } else {
        // create the reg
        reg = MIR_new_func_reg(mir_ctx, mir_func->u.func, mir_type, name);
    }

    return reg;
}

/**
 * Pop an item from the stack, returning its type and register location, will fail
 * if there are not enough items on the stack
 */
static err_t stack_pop(jit_method_context_t* ctx, System_Type* type, MIR_reg_t* reg) {
    err_t err = NO_ERROR;

    // pop the entry
    CHECK(arrlen(ctx->stack.entries) > 0);
    stack_entry_t entry = arrpop(ctx->stack.entries);
    if (reg != NULL) *reg = entry.reg;
    if (type != NULL) *type = entry.type;

cleanup:
    return err;
}

/**
 * Push a new item on the stack, returning the stack slot for it as a register
 *
 * This verifies that the stack depth is not too big
 */
static err_t stack_push(jit_method_context_t* ctx, System_Type type, MIR_reg_t* out_reg) {
    err_t err = NO_ERROR;

    stack_entry_t entry = {
        .type = type
    };

    // Make sure we don't exceed the stack depth
    CHECK(arrlen(ctx->stack.entries) < ctx->method->MethodBody->MaxStackSize);

    // create the reg
    MIR_reg_t reg = new_reg(ctx, type);

    // set the actual op
    entry.reg = reg;

    // give out if needed
    if (out_reg != NULL) {
        *out_reg = entry.reg;
    }

    // append to the stack
    arrpush(ctx->stack.entries, entry);

cleanup:
    return err;
}

/**
 * Take a snapshot of the stack, used for verification
 */
static stack_t stack_snapshot(jit_method_context_t* ctx) {
    stack_t snapshot = { 0 };
    arrsetlen(snapshot.entries, arrlen(ctx->stack.entries));
    memcpy(snapshot.entries, ctx->stack.entries, arrlen(ctx->stack.entries) * sizeof(stack_entry_t));
    return snapshot;
}

/**
 * Create a copy of the current stack, put it in the output stack
 */
static void stack_copy(jit_method_context_t* ctx, stack_t* stack) {
    arrsetlen(ctx->stack.entries, arrlen(stack->entries));
    memcpy(ctx->stack.entries, stack->entries, arrlen(stack->entries) * sizeof(stack_entry_t));
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
 * If it is 32 bytes or smaller will emit an in-place memcpy, otherwise it
 * will call the memcpy function.
 */
void jit_emit_memcpy(jit_method_context_t* ctx, MIR_reg_t dest, MIR_reg_t src, size_t count) {
    if (count <= 32 && (count % 8) == 0) {
        for (size_t off = 0; off < count; off += 8) {
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_mem_op(mir_ctx, MIR_T_I64, off, dest, 0, 1),
                                         MIR_new_mem_op(mir_ctx, MIR_T_I64, off, src, 0, 1)));
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
    if (count <= 32 && (count % 8) == 0) {
        for (size_t off = 0; off < count; off += 8) {
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_mem_op(mir_ctx, MIR_T_I64, off, dest, 0, 1),
                                         MIR_new_int_op(mir_ctx, 0)));
        }
    } else {
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_call_insn(mir_ctx, 5,
                                          MIR_new_ref_op(mir_ctx, m_memset_proto),
                                          MIR_new_ref_op(mir_ctx, m_memset_func),
                                          MIR_new_reg_op(mir_ctx, dest),
                                          MIR_new_int_op(mir_ctx, 0),
                                          MIR_new_int_op(mir_ctx, count)));
    }
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

    if (!method_is_static(method)) {
        MIR_var_t var = {
            .name = "this",
            .type = get_mir_type(method->DeclaringType),
        };
        if (var.type == MIR_T_BLK) {
            var.type = MIR_T_P;
        }
        arrpush(vars, var);
    }

    for (int i = 0; i < method->Parameters->Length; i++) {
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
        CHECK_FAIL("TODO: runtime methods");
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

            // create the import for it
            method->MirFunc = MIR_new_import(ctx->ctx, strbuilder_get(&func_name));
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

            // prepare the return type
            if (method->ReturnType != NULL) {
                CHECK_AND_RETHROW(jit_prepare_type(ctx, method->ReturnType));
            }

            // prepare the types of the parameters
            for (int pi = 0; pi < method->Parameters->Length; pi++) {
                CHECK_AND_RETHROW(jit_prepare_type(ctx, method->Parameters->Data[pi]->ParameterType));
            }

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
                                     MIR_new_reg_op(mir_ctx, stack.entries[0].reg),
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
                    temp_reg = new_reg(ctx, tSystem_Boolean);
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
    MIR_reg_t exception_obj = new_reg(ctx, type);

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

    if (type == NULL) {
        // this is a null type, just throw it
        CHECK_AND_RETHROW(jit_throw_new(ctx, tSystem_NullReferenceException));
    } else {
        CHECK(type_is_object_ref(type));

        MIR_label_t not_null = MIR_new_label(mir_ctx);

        if (type_is_interface(type)) {
            // this is an interface, we need to get the object reference in it and check if
            // that is zero
            MIR_reg_t temp_reg = new_reg(ctx, tSystem_Object);
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

    // &object->vtable[offsetof(vtable, virtual_functions) + vtable_offset]
    MIR_reg_t vtable_reg = new_reg(ctx, tSystem_IntPtr);
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
                                 MIR_new_int_op(mir_ctx, offsetof(object_vtable_t, virtual_functions) + interface->VTableOffset * sizeof(void*))));

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
    CHECK_AND_RETHROW(stack_pop(ctx, &value2_type, &value2_reg));
    CHECK_AND_RETHROW(stack_pop(ctx, &value1_type, &value1_reg));

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
                    MIR_reg_t value1_double_reg = new_reg(ctx, tSystem_Double);
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
                    MIR_reg_t value2_double_reg = new_reg(ctx, tSystem_Double);
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
    System_Type value2_type;
    System_Type value1_type;
    CHECK_AND_RETHROW(stack_pop(ctx, &value2_type, &value2_reg));
    CHECK_AND_RETHROW(stack_pop(ctx, &value1_type, &value1_reg));

    if (code == MIR_DIV || code == MIR_UDIV || code == MIR_MOD || code == MIR_UMOD) {
        MIR_insn_t label = MIR_new_label(mir_ctx);

        // these need to check that value2 is not zero
        // if we have a non-zero value then skip the throw
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_BT,
                                     MIR_new_label_op(mir_ctx, label),
                                     MIR_new_reg_op(mir_ctx, ctx->exception_reg)));

        // throw the error, it has an unknown type
        CHECK_AND_RETHROW(jit_throw_new(ctx, tSystem_DivideByZeroException));

        // insert the skip label
        MIR_append_insn(mir_ctx, mir_func, label);
    }

    MIR_reg_t result_reg;
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

    // actually create locals
    for (int i = 0; i < body->LocalVariables->Length; i++) {
        System_Reflection_LocalVariableInfo variable = body->LocalVariables->Data[i];
        CHECK(variable->LocalIndex == i);

        if (body->InitLocals) {
            // we are going to initialize all of the variables
            MIR_reg_t reg = new_reg(ctx, variable->LocalType);
                    arrpush(locals, MIR_new_reg_op(mir_ctx, reg));
            if (
                type_is_object_ref(variable->LocalType) ||
                variable->LocalType == tSystem_Int32 ||
                variable->LocalType == tSystem_Int64 ||
                variable->LocalType == tSystem_IntPtr
            ) {
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_MOV,
                                             MIR_new_reg_op(mir_ctx, reg),
                                             MIR_new_int_op(mir_ctx, 0)));
            } else if (variable->LocalType == tSystem_Single) {
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_FMOV,
                                             MIR_new_reg_op(mir_ctx, reg),
                                             MIR_new_float_op(mir_ctx, 0.0f)));
            } else if (variable->LocalType == tSystem_Double) {
                MIR_append_insn(mir_ctx, mir_func,
                                MIR_new_insn(mir_ctx, MIR_DMOV,
                                             MIR_new_reg_op(mir_ctx, reg),
                                             MIR_new_double_op(mir_ctx, 0.0)));
            } else {
                jit_emit_zerofill(ctx, reg, variable->LocalType->StackSize);
            }
        } else {
            // we can not verify non-initlocals methods, so we are not
            // going to support them at all for now
            CHECK_FAIL();
        }
    }

    // TODO: we need to validate that all branch targets and that all the
    //       try and handler offsets are actually in valid instructions and
    //       not in the middle of instructions

    // prepare the stacks at certain points for exception handling
    for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
        System_Reflection_ExceptionHandlingClause clause = body->ExceptionHandlingClauses->Data[i];

        // create the stack location
        MIR_label_t label = MIR_new_label(mir_ctx);
        stack_snapshot_t snapshot = {
            .key = clause->HandlerOffset,
            .label = label,
            .stack = { .entries = NULL }
        };

        if (clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
            stack_entry_t entry = {
                .type = clause->CatchType,
                .reg = new_reg(ctx, clause->CatchType),
            };
            arrpush(snapshot.stack.entries, entry);
        }

        // now put it in
        hmputs(ctx->pc_to_stack_snapshot, snapshot);

        // add to label lookup
        hmput(ctx->clause_to_label, clause, label);
    }

    //
    // The main loop for decoding and jitting opcodes
    //
    opcode_control_flow_t last_cf = OPCODE_CONTROL_FLOW_INVALID;
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
            } else {
                // copy the stack to the current stack
                stack_copy(ctx, &ctx->pc_to_stack_snapshot[stacki].stack);
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
                    .stack = stack_snapshot(ctx)
            };
            hmputs(ctx->pc_to_stack_snapshot, snapshot);
        }
        MIR_append_insn(mir_ctx, mir_func, cur_label);

        // validate the control flow from the previous instruction, we can not have anything that
        // continues to enter a handler block
        for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
            System_Reflection_ExceptionHandlingClause clause = body->ExceptionHandlingClauses->Data[i];

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

        // set the last control flow to this one
        last_cf = opcode_info->control_flow;

        //--------------------------------------------------------------------------------------------------------------
        // Handle operands of the opcode
        //--------------------------------------------------------------------------------------------------------------

        int32_t operand_i32;
        int64_t operand_i64;
        System_Reflection_FieldInfo operand_field;
        System_Reflection_MethodInfo operand_method;
        float operand_f32;
        double operand_f64;
        System_Type operand_type;
        System_String operand_string;
        uint32_t operand_switch_n;
        int32_t *operand_switch_dests;

        char param[128] = { 0 };
        switch (opcode_info->operand) {
            case OPCODE_OPERAND_InlineBrTarget: {
                operand_i32 = *(int32_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int32_t);
                operand_i32 += il_ptr;
            } break;

            case OPCODE_OPERAND_InlineField: {
                // fetch it
                token_t value = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);

                // resolve it
                CHECK_AND_RETHROW(assembly_get_field_by_token(assembly, value, method->DeclaringType->GenericArguments,
                                                              method->GenericArguments, &operand_field));
                CHECK(operand_field != NULL);

                // check we can access it
                CHECK(check_field_accessibility(method->DeclaringType, operand_field));

                // make sure we initialized its type
                CHECK_AND_RETHROW(jit_prepare_type(ctx->ctx, operand_field->DeclaringType));
            } break;

            case OPCODE_OPERAND_InlineI: {
                operand_i32 = *(int32_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int32_t);
            } break;

            case OPCODE_OPERAND_InlineI8: {
                operand_i64 = *(int64_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int64_t);
            } break;

            case OPCODE_OPERAND_InlineMethod: {
                // fetch it
                token_t value = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);

                // resolve it
                CHECK_AND_RETHROW(assembly_get_method_by_token(assembly, value, method->DeclaringType->GenericArguments,
                                                               method->GenericArguments, &operand_method));
                CHECK(operand_method != NULL);

                // check we can access it
                CHECK(check_method_accessibility(method->DeclaringType, operand_method));

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
            } break;

            case OPCODE_OPERAND_InlineSig: CHECK_FAIL("TODO: sig support"); break;

            case OPCODE_OPERAND_InlineString: {
                token_t value = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);
                operand_string = assembly_get_string_by_token(assembly, value);
                CHECK(operand_string != NULL);
            } break;

            case OPCODE_OPERAND_InlineSwitch: {
                operand_switch_n = *(uint32_t*)&body->Il->Data[il_ptr];
                il_ptr += 4;
                operand_switch_dests = (int32_t*)&body->Il->Data[il_ptr];
                il_ptr += operand_switch_n * 4;
            } break;

            case OPCODE_OPERAND_InlineTok: CHECK_FAIL("TODO: tok support"); break;

            case OPCODE_OPERAND_InlineType: {
                // fetch it
                token_t value = *(token_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(token_t);

                // resolve it
                CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, value, method->DeclaringType->GenericArguments
                        , method->GenericArguments, &operand_type));
                CHECK(operand_type != NULL);

                // check it is visible
                CHECK(check_type_visibility(method->DeclaringType, operand_type));

                // init it
                CHECK_AND_RETHROW(jit_prepare_type(ctx->ctx, operand_type));
            } break;

            case OPCODE_OPERAND_InlineVar: {
                operand_i32 = *(uint16_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(uint16_t);
            } break;

            case OPCODE_OPERAND_ShortInlineBrTarget: {
                operand_i32 = *(int8_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int8_t);
                operand_i32 += il_ptr;
            } break;

            case OPCODE_OPERAND_ShortInlineI: {
                operand_i32 = *(int8_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(int8_t);
            } break;

            case OPCODE_OPERAND_ShortInlineR: {
                operand_f32 = *(float*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(float);
            } break;

            case OPCODE_OPERAND_ShortInlineVar: {
                operand_i32 = *(uint8_t*)&body->Il->Data[il_ptr];
                il_ptr += sizeof(uint8_t);
            } break;

            case OPCODE_OPERAND_InlineNone:
                break;

            default:
                CHECK_FAIL();
        }

        //--------------------------------------------------------------------------------------------------------------
        // Handle the opcode
        //--------------------------------------------------------------------------------------------------------------

        switch (opcode) {
            //----------------------------------------------------------------------------------------------------------
            // Base Instructions
            //----------------------------------------------------------------------------------------------------------

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arithmetic
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // binary operations
            case CEE_ADD: CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_ADD, false)); break;
            case CEE_DIV: CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_DIV, false)); break;
            case CEE_DIV_UN: CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_UDIV, true)); break;
            case CEE_MUL: CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_MUL, false)); break;
            case CEE_REM: CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_MOD, false)); break;
            case CEE_REM_UN: CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_UMOD, true)); break;
            case CEE_SUB: CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_SUB, false)); break;

            // bitwise binary operations
            case CEE_AND: CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_AND, true)); break;
            case CEE_OR: CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_OR, true)); break;
            case CEE_XOR: CHECK_AND_RETHROW(jit_binary_numeric_operation(ctx, MIR_XOR, true)); break;

            // unary operations
            case CEE_NEG: {
                MIR_reg_t value_reg;
                System_Type value_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg));

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
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg));

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
                CHECK_AND_RETHROW(stack_pop(ctx, &type, &reg));

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
                CHECK_AND_RETHROW(stack_pop(ctx, &obj_type, &obj_reg));

                if (opcode == CEE_ISINST || opcode == CEE_CASTCLASS) {
                    // for castclass/isinst we must have a result of a reference type
                    CHECK(operand_type->StackType == STACK_TYPE_O);

                    // check that casting from the wanted type to the tracked type is possible,
                    // otherwise it is not actually possible to get the opposite
                    CHECK(type_is_verifier_assignable_to(operand_type, obj_type));
                }

                // the object type must always be a ref type for unboxing
                CHECK(obj_type->StackType == STACK_TYPE_O);

                // push it, but now as the new type
                MIR_reg_t obj2_reg;
                CHECK_AND_RETHROW(stack_push(ctx, operand_type, &obj2_reg));

                // temp for the cast result
                MIR_reg_t cast_result_reg = new_reg(ctx, tSystem_Boolean);

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
                CHECK_AND_RETHROW(stack_pop(ctx, &val_type, &val_reg));

                // make sure that this is fine
                CHECK(type_is_verifier_assignable_to(val_type, operand_type));

                // we track this as an object now
                MIR_reg_t obj_reg;
                CHECK_AND_RETHROW(stack_push(ctx, tSystem_Object, &obj_reg));

                // check if we need to allocate memory for this
                if (operand_type->IsValueType) {
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

                        // first get the base for memcpy
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_ADD,
                                                     MIR_new_reg_op(mir_ctx, obj_reg),
                                                     MIR_new_reg_op(mir_ctx, obj_reg),
                                                     MIR_new_int_op(mir_ctx, tSystem_Object->ManagedSize)));

                        // now emit the memcpy
                        jit_emit_memcpy(ctx, obj_reg, val_reg, operand_type->ManagedSize);
                    } break;

                    case STACK_TYPE_REF:
                        CHECK_FAIL();
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
            cee_ldind: {
                // pop all the values from the stack
                MIR_reg_t addr_reg;
                System_Type addr_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &addr_type, &addr_reg));

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
            cee_stind: {
                // pop all the values from the stack
                MIR_reg_t value_reg;
                MIR_reg_t addr_reg;
                System_Type value_type;
                System_Type addr_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg));
                CHECK_AND_RETHROW(stack_pop(ctx, &addr_type, &addr_reg));

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
                                            MIR_new_call_insn(mir_ctx, 5,
                                                              MIR_new_ref_op(mir_ctx, m_gc_update_proto),
                                                              MIR_new_ref_op(mir_ctx, m_gc_update_func),
                                                              MIR_new_reg_op(mir_ctx, addr_reg),
                                                              MIR_new_int_op(mir_ctx, 0),
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
                        CHECK_FAIL("TODO: struct value store in array");
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
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg));

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
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg));

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
                CHECK_AND_RETHROW(stack_pop(ctx, &obj_type, &obj_reg));

                // free this entirely
                        arrfree(ctx->stack.entries);

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
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg));

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
                        if (locals[operand_i32].mode == MIR_OP_REG) {
                            CHECK_FAIL("TODO: spill the value into the stack");
                        } else {
                            // already spilled, get the base register
                            CHECK(locals[operand_i32].mode == MIR_OP_MEM);
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, MIR_MOV,
                                                         MIR_new_reg_op(mir_ctx, value_reg),
                                                         MIR_new_reg_op(mir_ctx, locals[operand_i32].u.mem.base)));
                        }
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
                    case STACK_TYPE_FLOAT:
                    case STACK_TYPE_REF: {
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
                }
            } break;

            // TODO: LDARGA

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

            case CEE_DUP: {
                // get the top value
                MIR_reg_t top_reg;
                System_Type top_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &top_type, &top_reg));

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
                CHECK_AND_RETHROW(stack_pop(ctx, NULL, NULL));
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Field access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STSFLD: {
                // get the top value
                MIR_reg_t value_reg;
                System_Type value_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg));

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
                MIR_reg_t field_reg = new_reg(ctx, tSystem_IntPtr);
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
                MIR_reg_t field_reg = new_reg(ctx, tSystem_IntPtr);
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

            case CEE_STFLD: {
                // get the values
                MIR_reg_t obj_reg;
                MIR_reg_t value_reg;
                System_Type obj_type;
                System_Type value_type;
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg));
                CHECK_AND_RETHROW(stack_pop(ctx, &obj_type, &obj_reg));

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
                System_Type base = obj_type;
                while (base != NULL && base != operand_field->DeclaringType) {
                    base = base->BaseType;
                }
                CHECK(base != NULL);

                // get the field type, ignoring stuff like enums
                System_Type field_type = type_get_underlying_type(operand_field->FieldType);

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
                                                MIR_new_call_insn(mir_ctx, 5,
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
                                                MIR_new_call_insn(mir_ctx, 5,
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
                CHECK_AND_RETHROW(stack_pop(ctx, &obj_type, &obj_reg));

                // validate that the object type is a valid one for stfld
                if (type_get_stack_type(obj_type) == STACK_TYPE_REF) {
                    // this is a reference, so it has to be a reference to a value type
                    CHECK(type_get_stack_type(obj_type->BaseType) == STACK_TYPE_VALUE_TYPE);
                } else {
                    CHECK(type_get_stack_type(obj_type) == STACK_TYPE_O || type_get_stack_type(obj_type) == STACK_TYPE_VALUE_TYPE);
                }

                // validate the field is part of the object
                System_Type base = obj_type;
                while (base != NULL && base != operand_field->DeclaringType) {
                    base = base->BaseType;
                }
                CHECK(base != NULL);

                // TODO: does the runtime actually use ldfld for static fields?
                CHECK(!field_is_static(operand_field));

                // make sure the field is compatible
                CHECK(type_is_compatible_with(obj_type, operand_field->DeclaringType));

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
                                                     MIR_new_int_op(mir_ctx, (int)operand_field->MemoryOffset)));
                        jit_emit_memcpy(ctx, value_reg, obj_reg, field_type->StackSize);
                    } break;

                    case STACK_TYPE_REF:
                        CHECK_FAIL("wtf");
                }
            } break;

            case CEE_LDFLDA: {
                // get the object instance
                System_Type obj_type;
                MIR_reg_t obj_reg;
                CHECK_AND_RETHROW(stack_pop(ctx, &obj_type, &obj_reg));

                // validate that the object type is a valid one for ldfld
                CHECK(type_get_stack_type(obj_type) == STACK_TYPE_O || type_get_stack_type(obj_type) == STACK_TYPE_REF);

                // validate the field is part of the object
                System_Type base = obj_type;
                while (base != NULL && base != operand_field->DeclaringType) {
                    base = base->BaseType;
                }
                CHECK(base != NULL);

                // TODO: does the runtime actually use ldfld for static fields?
                CHECK(!field_is_static(operand_field));

                // make sure the field is compatible
                CHECK(type_is_compatible_with(obj_type, operand_field->DeclaringType));

                // Get the field type
                System_Type field_stack_type = get_by_ref_type(type_get_verification_type(operand_field->FieldType));
                System_Type field_type = type_get_underlying_type(operand_field->FieldType);

                // push it
                MIR_reg_t value_reg;
                CHECK_AND_RETHROW(stack_push(ctx, field_stack_type, &value_reg));

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

                // pop all the arguments from the stack
                int i;
                for (i = arg_count + other_args - 1; i >= other_args; i--) {
                    System_Type signature_type = operand_method->Parameters->Data[i - other_args]->ParameterType;

                    // get the argument value
                    MIR_reg_t arg_reg;
                    System_Type arg_type;
                    CHECK_AND_RETHROW(stack_pop(ctx, &arg_type, &arg_reg));

                    // do implicit conversion as needed
                    if (arg_type == tSystem_Int32) {
                        if (
                                signature_type == tSystem_SByte ||
                                signature_type == tSystem_Byte ||
                                signature_type == tSystem_Boolean ||
                                signature_type == tSystem_Int16 ||
                                signature_type == tSystem_UInt16
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
                    } else if (arg_type == tSystem_IntPtr) {
                        if (type_is_integer(signature_type)) {
                            // truncate or nop, we don't really care
                            arg_type = signature_type;
                        }
                    } else if (arg_type == tSystem_Single) {
                        if (signature_type == tSystem_Double) {
                            // float->double conversion
                            MIR_reg_t real_arg_reg = new_reg(ctx, tSystem_Double);
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
                            MIR_reg_t real_arg_reg = new_reg(ctx, tSystem_Single);
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, MIR_D2F,
                                                         MIR_new_reg_op(mir_ctx, real_arg_reg),
                                                         MIR_new_reg_op(mir_ctx, arg_reg)));
                            arg_reg = real_arg_reg;
                            arg_type = signature_type;
                        }
                    }

                    // set the op reg
                    arg_ops[i] = MIR_new_reg_op(mir_ctx, arg_reg);

                    // verify a normal argument
                    CHECK(type_is_verifier_assignable_to(type_get_verification_type(arg_type), signature_type));
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
                                this_reg = new_reg(ctx, tSystem_IntPtr);
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
                        CHECK_AND_RETHROW(stack_pop(ctx, &this_type, &this_reg));

                        // Value types have their this as a by-ref
                        System_Type thisType = operand_method->DeclaringType;
                        if (thisType->IsValueType) {
                            thisType = get_by_ref_type(thisType);
                        }

                        // verify a normal argument
                        CHECK(type_is_verifier_assignable_to(
                                type_get_verification_type(this_type), thisType));

                        // make sure that the object is not null
                        CHECK_AND_RETHROW(jit_null_check(ctx, this_reg, this_type));
                    }

                    arg_ops[i] = MIR_new_reg_op(mir_ctx, this_reg);
                }

                // get the MIR signature and address
                arg_ops[0] = MIR_new_ref_op(mir_ctx, operand_method->MirProto);

                if (
                    opcode == CEE_CALLVIRT &&
                    method_is_virtual(operand_method)
                ) {
                    // we are using callvirt and this is a virtual method, so we have to
                    // use a dynamic dispatch

                    MIR_reg_t temp_reg = new_reg(ctx, tSystem_Type);

                    // get the vtable pointer from the object, it is at the first
                    // item for both an interface and an object
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, temp_reg),
                                                 MIR_new_mem_op(mir_ctx, MIR_T_P, 0, this_reg, 0, 1)));

                    // figure offset and the actual method
                    int offset;
                    int vtable_index;
                    if (type_is_interface(this_type)) {
                        // we have an interface on the stack, the vtable is the first element
                        // and the vtable index is exactly as given in the operand
                        offset = 0;
                        vtable_index = operand_method->VTableOffset;

                        // read the actual instance pointer of the interface, so we can use it
                        // when calling the function
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, this_reg),
                                                     MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), this_reg, 0, 1)));
                    } else {
                        // we have an object reference on the stack, the vtable is at offset
                        // of the virtual functions in the object vtable
                        offset = offsetof(object_vtable_t, virtual_functions);

                        if (type_is_interface(operand_method->DeclaringType)) {
                            // we want to call an interface method on the object, so resolve it and get the
                            // object inside the object's vtable instead
                            vtable_index = type_get_interface_method_impl(this_type, operand_method)->VTableOffset;
                        } else {
                            // this is a normal virtual method, nothing to resolve
                            vtable_index = operand_method->VTableOffset;
                        }
                    }

                    // get the address of the function from the vtable
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, temp_reg),
                                                 MIR_new_mem_op(mir_ctx, MIR_T_P,
                                                                offset + vtable_index * sizeof(void*),
                                                                temp_reg, 0, 1)));

                    // indirect call
                    arg_ops[1] = MIR_new_reg_op(mir_ctx, temp_reg);
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
                CHECK_AND_RETHROW(stack_pop(ctx, &dest_type, &dest_reg));

                CHECK(dest_type->IsByRef);
                CHECK(type_get_stack_type(dest_type->BaseType) == STACK_TYPE_VALUE_TYPE);
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
                    CHECK_AND_RETHROW(stack_pop(ctx, &ret_type, &ret_arg));

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
                            CHECK_FAIL();
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
                CHECK_AND_RETHROW(stack_pop(ctx, &num_elems_type, &num_elems_reg));

                // make sure it has a valid type
                CHECK(num_elems_type == tSystem_Int32);

                // push the array type
                MIR_reg_t array_reg;
                CHECK_AND_RETHROW(stack_push(ctx, get_array_type(operand_type), &array_reg));

                // calculate the size we are going to need:
                //  num_elems * sizeof(value_type) + sizeof(System.Array)
                MIR_reg_t size_reg = new_reg(ctx, tSystem_Int64);
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
                CHECK_AND_RETHROW(stack_pop(ctx, &array_type, &array_reg));

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
                CHECK_AND_RETHROW(stack_pop(ctx, &value_type, &value_reg));
                CHECK_AND_RETHROW(stack_pop(ctx, &index_type, &index_reg));
                CHECK_AND_RETHROW(stack_pop(ctx, &array_type, &array_reg));

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
                        CHECK_FAIL("TODO: struct value store in array");
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
                CHECK_AND_RETHROW(stack_pop(ctx, &index_type, &index_reg));
                CHECK_AND_RETHROW(stack_pop(ctx, &array_type, &array_reg));

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
                CHECK_AND_RETHROW(stack_pop(ctx, &index_type, &index_reg));
                CHECK_AND_RETHROW(stack_pop(ctx, &array_type, &array_reg));

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
            // Unknown opcode
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            default: {
                CHECK_FAIL("TODO: opcode %s", opcode_info->name);
            } break;
        }
    }

    // make sure that the last instruction is either
    // a return or a branch or a throw
    CHECK(
        last_cf == OPCODE_CONTROL_FLOW_THROW ||
        last_cf == OPCODE_CONTROL_FLOW_BRANCH ||
        last_cf == OPCODE_CONTROL_FLOW_RETURN
    );

#if 0
    MIR_output_item(mir_ctx, stdout, mir_func);
#endif

cleanup:
    // free all the memory used for jitting the method
    FREE(switch_ops);
    strbuilder_free(&method_name);
    arrfree(locals);
    for (int i = 0; i < hmlen(ctx->pc_to_stack_snapshot); i++) {
        arrfree(ctx->pc_to_stack_snapshot[i].stack.entries);
    }
    arrfree(ctx->stack.entries);
    hmfree(ctx->pc_to_stack_snapshot);
    hmfree(ctx->clause_to_label);

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

static err_t jit_setup_vtables(System_Reflection_Assembly assembly) {
    err_t err = NO_ERROR;

    //
    // go over all the types and setup the vtables for each of them
    //
    for (int i = 0; i < assembly->DefinedTypes->Length; i++) {
        System_Type type = assembly->DefinedTypes->Data[i];
        if (type_is_abstract(type) || type_is_interface(type)) continue;
        if (type->VirtualMethods == NULL) continue;

        // setup the vtable for the whole type
        for (int vi = 0; vi < type->VirtualMethods->Length; vi++) {
            type->VTable->virtual_functions[vi] = type->VirtualMethods->Data[vi]->MirFunc->addr;
        }

        // setup the vtable for each interface we implemented here
    }

cleanup:
    return err;
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
                created_type->VTable->virtual_functions[vi] = created_type->VirtualMethods->Data[vi]->MirFunc->addr;
            }
        }

        // add the gc roots for the types
        if (created_type->Fields != NULL) {
            for (int fi = 0; fi < created_type->Fields->Length; fi++) {
                System_Reflection_FieldInfo fieldInfo = created_type->Fields->Data[fi];
                if (!field_is_static(fieldInfo)) continue;
                if (fieldInfo->MirField->item_type != MIR_bss_item) continue;

                switch (type_get_stack_type(type)) {
                    case STACK_TYPE_O: {
                        gc_add_root(fieldInfo->MirField->addr);
                    } break;

                    case STACK_TYPE_VALUE_TYPE: {
                        for (int j = 0; j < arrlen(type->ManagedPointersOffsets); j++) {
                            gc_add_root(fieldInfo->MirField->addr + type->ManagedPointersOffsets[j]);
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
    for (int i = 0; i < arrlen(ctx.created_types); i++) {
        System_Type created_type = ctx.created_types[i];

        // call the ctor
        if (created_type->StaticCtor != NULL) {
            System_Exception(*cctor)() = created_type->StaticCtor->MirFunc->addr;
            System_Exception exception = cctor();
            CHECK(exception == NULL);
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
                created_type->Methods->Data[i]->MirFunc = NULL;
                created_type->Methods->Data[i]->MirProto = NULL;
            }

            // remove the field setup
            for (int vi = 0; vi < created_type->Fields->Length; vi++) {
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