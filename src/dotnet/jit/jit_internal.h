#pragma once


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// jitting context (for parallel jitting)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "dotnet/types.h"

typedef struct stack_entry {
    /**
     * The type on the stack
     */
    System_Type type;

    /**
     * This entry has a reference to a non-local object,
     * we can safely return it from the method
     */
    uint32_t non_local_ref : 1;

    /**
     * The reference is readonly, it should never be
     * written to
     */
    uint32_t readonly_ref : 1;

    /**
     * This entry points to the `this` of the function
     */
    uint32_t this : 1;
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

typedef struct finally_chain_t {
    // the finally we are in
    System_Reflection_ExceptionHandlingClause key;

    // the label it is going to
    MIR_label_t* labels;

    // used to select which one to use
    MIR_reg_t selector;

    // the label we want to add right now
    MIR_label_t new_label;

    // this was already set, if we need
    // to set anything new we are wrong
    bool done;
} finally_chain_t;

typedef struct jit_context {
    // the mir context for this jit instance
    MIR_context_t ctx;

    // types that we need to set a vtable for
    System_Type* instance_types;

    // types that we need to run a cctor on
    System_Type* static_types;

    // stack of messages that we need to jit
    System_Reflection_MethodInfo* methods_to_jit;

    // tracks dependencies of static initializations
    struct {
        System_Type key;
        System_Type* value;
    }* type_init_dependencies;

    // points to the current method, used for resolving
    // static initialization order
    System_Reflection_MethodInfo current_method;
} jit_context_t;

// for debugging it might be smarter to not use a union, just
// so using the wrong operand will result in a bad result
typedef struct jit_operand {
    token_t token;
    int32_t i32;
    int64_t i64;
    System_Reflection_FieldInfo field;
    System_Reflection_MethodInfo method;
    float f32;
    double f64;
    System_Type type;
    System_String string;
    struct {
        uint32_t switch_n;
        int32_t* switch_dests;
    };
} jit_operand_t;

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
    finally_chain_t* finally_chain;

    // The current filter clause we are in
    System_Reflection_ExceptionHandlingClause filter_clause;

    /**********************/
    /* Prefix/Operands    */
    /**********************/

    // the function method pushed to the stack
    System_Reflection_MethodInfo ftnMethod;

    // the constrained type
    System_Type constrainedType;

    // the operand we have
    jit_operand_t operand;

    /*******************/
    /* jitting context */
    /*******************/

    // the current il offset
    int il_offset;

    // the indentation
    int jit_trace_indent;

    // the method we are dealing with
    System_Reflection_MethodInfo method;

    // marks that pointers are allowed to be used
    bool allow_pointers;
} jit_method_context_t;

/**
 * Quick macro to get the top of the stack, so we can modify the entry easily
 */
#define STACK_TOP ctx->stack.entries[arrlen(ctx->stack.entries) - 1]

// helper macros to get what we want from the context quickly
#define mir_ctx ctx->ctx->ctx
#define mir_func ctx->method->MirFunc
#define body ctx->method->MethodBody

// quickly get the operand we want
#define operand_token ctx->operand.token
#define operand_i32 ctx->operand.i32
#define operand_i64 ctx->operand.i64
#define operand_field ctx->operand.field
#define operand_method ctx->operand.method
#define operand_f32 ctx->operand.f32
#define operand_f64 ctx->operand.f64
#define operand_type ctx->operand.type
#define operand_string ctx->operand.string
#define operand_switch_n ctx->operand.switch_n
#define operand_switch_dests ctx->operand.switch_dests

/**
 * Create a new object, of the given type, putting it in the result register, with the given size.
 *
 * This function makes sure to handle out-of-memory exception.
 *
 * @param ctx       [IN] The jit context
 * @param result    [IN] Where to put the result
 * @param type      [IN] The type of the object to allocate
 * @param size      [IN] The size of the object to allocate
 */
err_t jit_new(jit_method_context_t* ctx, MIR_reg_t result, System_Type type, MIR_op_t size);

//----------------------------------------------------------------------------------------------------------------------
// Register context helpers
//----------------------------------------------------------------------------------------------------------------------

/**
 * Push a new register to the stack of the given type.
 *
 * A temp register will be discarded after the current opcode generation is done, useful
 * for temporary storage for the current opcode generation.
 *
 * @param ctx   [IN] The jit context
 * @param type  [IN] The type that will be stored in the register
 * @param temp  [IN] Is this a temp register
 * @return
 */
MIR_reg_t jit_push_new_reg(jit_method_context_t* ctx, System_Type type, bool temp);

/**
 * Wrapper around jit_push_new_reg, with temp=true.
 *
 * @param ctx   [IN] The jit context
 * @param type  [IN] The type that will be stored in the register
 */
MIR_reg_t jit_new_temp_reg(jit_method_context_t* ctx, System_Type type);

/**
 * Push a new stack element, returning the register where the data should be stored.
 *
 * This makes sure that the eval stack is not too big
 *
 * @param ctx       [IN]    The jit context
 * @param type      [IN]    The type that will be stored in the register
 * @param out_reg   [OUT]   The register where to store the data
 */
err_t jit_stack_push(jit_method_context_t* ctx, System_Type type, MIR_reg_t* out_reg);

/**
 * Pop a stack element, returning the register it is in, its type and its attributes
 *
 * @param ctx       [IN]                The jit context
 * @param out_type  [OUT, OPTIONAL]     The type of the element
 * @param out_reg   [OUT, OPTIONAL]     The register of the element
 * @param ste       [OUT, OPTIONAL]     The stack entry itself (attributes)
 */
err_t jit_stack_pop(jit_method_context_t* ctx, System_Type* out_type, MIR_reg_t* out_reg, stack_entry_t* ste);

//----------------------------------------------------------------------------------------------------------------------
// Type helpers
//----------------------------------------------------------------------------------------------------------------------

/**
 * Get the mir type from the given type, handles everything from structs
 * to integers to enums.
 *
 * @param type      [IN] The type to convert
 */
MIR_type_t jit_get_mir_type(System_Type type);

/**
 * Code used for anything that stores a primitive value from the stack into a field, and mostly handles
 * implicit casting of float->double and double->float
 *
 * @param srctype   [IN] The source type (on the stack)
 * @param desttype  [IN] The destination type
 *
 * @returns The opcode that should be used to move the src to the dest
 */
MIR_insn_code_t jit_number_cast_inscode(System_Type srctype, System_Type desttype);

/**
 * Code used for anything that loads a primitive value from a field to the stack, mostly
 * handles giving the correct loading opcode
 *
 * @param type      [IN] The type to move
 *
 * @returns The opcode that should be used to move from the src to the dest
 */
MIR_insn_code_t jit_mov_insn_code(System_Type type);

//----------------------------------------------------------------------------------------------------------------------
// Code generation helpers
//----------------------------------------------------------------------------------------------------------------------

/**
 * A helper to generate a memcpy, if the size is small enough it will be inlined
 *
 * @param ctx       [IN] The jit context
 * @param dest      [IN] The dest register
 * @param src       [IN] The src register
 * @param count     [IN] The size to copy
 */
void jit_emit_memcpy(jit_method_context_t* ctx, MIR_reg_t dest, MIR_reg_t src, size_t count);

/**
 * A helper to generate a memset zero, if the size is small enough it will be inlined.
 *
 * @param ctx       [IN] The jit context
 * @param dest      [IN] The dest register
 * @param count     [IN] The size to zero
 */
void jit_emit_zerofill(jit_method_context_t* ctx, MIR_reg_t dest, size_t count);

//----------------------------------------------------------------------------------------------------------------------
// Branching helpers
//----------------------------------------------------------------------------------------------------------------------

/**
 * Resolves a label to the location we want to jump to, so it can be used for the jump.
 *
 * This also takes care of stack merging for forward jumps and stack verification for backwards
 * jumps, so this could fail if the stacks don't match in a branch point.
 *
 * This takes in the current offset in the il code, and the target offset for the code
 *
 * @param ctx           [IN]    The jit context
 * @param il_target     [IN]    Where to jump to
 * @param label         [OUT]   The label of the destination
 */
err_t jit_resolve_branch(jit_method_context_t* ctx, int il_target, MIR_label_t* label);

/**
 * In addition to doing the job that jit_resolve_branch does, this will also take care
 * of making sure that the jump does not crosses exception clause/handle borders
 *
 * @param ctx           [IN]    The jit context
 * @param il_target     [IN]    Where to jump to
 * @param label         [OUT]   The label of the destination
 */
err_t jit_branch_point(jit_method_context_t* ctx, int il_target, MIR_label_t* label);

//----------------------------------------------------------------------------------------------------------------------
// Exception throwing
//----------------------------------------------------------------------------------------------------------------------

/**
 * Throw an exception that was put to the exception register, this makes sure
 * to find the proper exception handler to jump to, and jump to it.
 *
 * If the type is not known it will simply resolve it at runtime
 *
 * The rethrow is only used when THROW_TRACE is enabled to show a nice trace
 * of the exception throwing.
 *
 * @param ctx           [IN]            The jit context
 * @param type          [IN, OPTIONAL]  The exception type
 * @param rethrow       [IN]            Is this coming from a rethrow
 */
err_t jit_throw(jit_method_context_t* ctx, System_Type type, bool rethrow);

/**
 * Throw a new exception, creating it and putting it in the exception register
 * before calling the jit_throw function.
 *
 * @param ctx           [IN]    The jit context
 * @param type          [IN]    The clause to jump to
 */
err_t jit_throw_new(jit_method_context_t* ctx, System_Type type);

/**
 * Perform a null check, on the object inside of reg, if the type is null assuming this
 * is already a null and throwing un-conditionally
 *
 * @param ctx           [IN]    The jit context
 * @param reg           [IN]    The object location
 * @param type          [IN]    The object type
 */
err_t jit_null_check(jit_method_context_t* ctx, MIR_reg_t reg, System_Type type);

//----------------------------------------------------------------------------------------------------------------------
// Type handling
//----------------------------------------------------------------------------------------------------------------------

/**
 * Prepare a static type, meaning that one of the static fields of this type
 * is accessed OR the type is going to be created. Only makes sure to fill
 * the type and queue it for .cctor
 *
 * @param ctx       [IN] The jit context
 * @param type      [IN] The type
 */
err_t jit_prepare_static_type(jit_context_t* ctx, System_Type type);

/**
 * Prepare a new instance type, meaning that an instance of this type is
 * going to be created so we need all the virtual functions to be ready
 * for it.
 *
 * @param ctx       [IN] The jit context
 * @param type      [IN] The type
 */
err_t jit_prepare_instance_type(jit_context_t* ctx, System_Type type);

/**
 * Prepare the method to be called, makes sure to init everything related to it
 *
 * @param ctx       [IN] The jit context
 * @param method    [IN] The method we are calling
 */
err_t jit_prepare_method(jit_context_t* ctx, System_Reflection_MethodInfo method);

//----------------------------------------------------------------------------------------------------------------------
// Misc jit helpers
//----------------------------------------------------------------------------------------------------------------------

/**
 * Cast an object to an interface, from the from_reg of from_type, to the result reg,
 * putting there a to_type.
 *
 * @param ctx           [IN] The jit context
 * @param result_reg    [IN] Where the result should be put (output interface)
 * @param from_reg      [IN] Where the object is (the input object)
 * @param from_type     [IN] The type of the input object
 * @param to_type       [IN] The interface type we want
 * @return
 */
err_t jit_cast_obj_to_interface(jit_method_context_t* ctx,
                                MIR_reg_t result_reg, MIR_reg_t from_reg,
                                System_Type from_type, System_Type to_type);

//----------------------------------------------------------------------------------------------------------------------
// Jit runtime functions
//----------------------------------------------------------------------------------------------------------------------

extern MIR_item_t m_dynamic_cast_obj_to_interface_proto;
extern MIR_item_t m_dynamic_cast_obj_to_interface_func;
extern MIR_item_t m_is_instance_proto;
extern MIR_item_t m_is_instance_func;
extern MIR_item_t m_gc_new_proto;
extern MIR_item_t m_gc_new_func;
extern MIR_item_t m_gc_update_proto;
extern MIR_item_t m_gc_update_func;
extern MIR_item_t m_gc_update_ref_proto;
extern MIR_item_t m_gc_update_ref_func;
extern MIR_item_t m_managed_memcpy_proto;
extern MIR_item_t m_managed_memcpy_func;
extern MIR_item_t m_managed_ref_memcpy_proto;
extern MIR_item_t m_managed_ref_memcpy_func;
extern MIR_item_t m_memcpy_proto;
extern MIR_item_t m_memcpy_func;
extern MIR_item_t m_memmove_proto;
extern MIR_item_t m_memmove_func;
extern MIR_item_t m_memset_proto;
extern MIR_item_t m_memset_func;
extern MIR_item_t m_on_throw_proto;
extern MIR_item_t m_on_throw_func;
extern MIR_item_t m_on_rethrow_proto;
extern MIR_item_t m_on_rethrow_func;
extern MIR_item_t m_get_thread_local_ptr_proto;
extern MIR_item_t m_get_thread_local_ptr_func;
extern MIR_item_t m_delegate_ctor_func;
extern MIR_item_t m_unsafe_as_func;

extern MIR_item_t m_debug_trace_proto;
extern MIR_item_t m_debug_trace_func;

// intrinsic methods
extern System_Reflection_MethodInfo m_Unsafe_SizeOf;
extern System_Reflection_MethodInfo m_MemoryMarshal_GetArrayDataReference;
extern System_Reflection_MethodInfo m_RuntimeHelpers_IsReferenceOrContainsReferences;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// JIT debug options
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Uncomment to remove null-checks, out of memory checks, oob checks and more
 * to make the JITed code a bit more readable
 */
//#define READABLE_JIT

/**
 * Will make the JIT generate functions to trace when an exception is thrown/rethrown
 */
#define THROW_TRACE

/**
 * Uncomment to make the jit trace the IL opcodes it is trying to figure out
 */
//#define JIT_TRACE

/**
 * Uncomment to make the jit trace the MIR generated from the IL
 */
//#define JIT_TRACE_MIR

/**
 * Uncomment to print the final MIR function, will not print
 * the IL code in between
 */
//#define JIT_TRACE_FINAL

/**
 * Uncomment if you want debug symbols, note that this forces
 * the parallel generator instead of the lazy one!
 */
#define JIT_DEBUG_SYMBOLS

/**
 * This function is called to decide if we should trace or not
 */
static inline bool trace_filter(System_Reflection_MethodInfo method) {
//    if (method->DeclaringType->GenericTypeDefinition == NULL)
//        return false;

    if (!string_equals_cstr(method->DeclaringType->Name, "Utf8Utility"))
        return false;

    if (!string_equals_cstr(method->Name, "TranscodeToUtf16"))
        return false;

    return true;
}

/**
 * Used to replace the append instruction
 *  So every append insn will be printed
 */
#define MIR_append_insn_output(__ctx, __func, ...) \
    do { \
        MIR_insn_t _insn = __VA_ARGS__; \
        MIR_context_t ___ctx = __ctx; \
        MIR_item_t _func = __func; \
        if (trace_filter(ctx->method)) { \
            MIR_output_insn(___ctx, stdout, _insn, _func->u.func, true); \
        } \
        MIR_append_insn(___ctx, _func, _insn); \
    } while (0)

/* paste before code generation
#ifdef JIT_TRACE_MIR
#define MIR_append_insn(...) MIR_append_insn_output(__VA_ARGS__)
#endif
 */

#ifdef JIT_TRACE
    #undef JIT_TRACE
    #define JIT_TRACE(...) \
        do { \
            if (trace_filter(ctx->method)) { \
                __VA_ARGS__; \
            } \
        } while (0)
#else
    #define JIT_TRACE(...)
#endif
