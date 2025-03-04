#pragma once

#include <dotnet/types.h>
#include <spidir/module.h>
#include <tomatodotnet/types/reflection.h>
#include <util/defs.h>

// #define JIT_DISABLE_INLINE
// #define JIT_DISABLE_OPTIMIZATIONS

// enable printing while verifying
#define JIT_VERBOSE_VERIFY
#define JIT_DEBUG_VERIFY

#ifdef JIT_VERBOSE_VERIFY
    #define JIT_DEBUG_VERIFY
#endif

// enable printing while emitting
#define JIT_VERBOSE_SPIDIR
#define JIT_VERBOSE_EMIT
#define JIT_DEBUG_EMIT
#define JIT_DUMP_EMIT

#ifdef JIT_VERBOSE_EMIT
    #define JIT_DEBUG_EMIT
#endif

typedef struct jit_value_attrs {
    union {
        // the actual known type, used whenever the type on the stack
        // doesn't represent the correct type, either because of boxing
        // locals or whatever else
        RuntimeTypeInfo known_type;

        // when its a method, the method this references
        RuntimeMethodBase method;
    };

    // The type info is exact (either boxed or the actual type)
    // encodes the exact real type
    size_t exact_type : 1;

    // does this have a method instead of a known type
    size_t is_method : 1;

    //
    // Only used for the verifier, emitter completely ignores it
    //

    // is this a readonly reference
    size_t readonly : 1;

    // if this is a reference that comes
    // from non-scoped place
    size_t nonlocal_ref : 1;

    // the ref-struct contains a nonlocal
    // reference, so it should not be
    // returned
    size_t nonlocal_ref_struct : 1;

    // the value holds the this ptr
    size_t this_ptr : 1;

    //
    // Only used by the args/locals in the jit method itself
    // not used for normal stack entries
    //

    // does the local need an initialized, only used for global locals
    size_t needs_init : 1;

    // was the local variable assigned already, only used for per-block locals
    size_t is_assigned : 1;

    // don't generate a phi for this entry, since it
    // was taken by reference (because it was spilled)
    size_t spilled : 1;
} jit_value_attrs_t;

typedef struct jit_value {
    // the type on the verification stack
    RuntimeTypeInfo type;

    // the attributes of this slot
    jit_value_attrs_t attrs;

    // the jit value of this
    spidir_value_t value;

    // the phi we created for this slot
    spidir_phi_t phi;
} jit_value_t;

static inline void jit_copy_type(jit_value_t* dst, jit_value_t* src) {
    dst->attrs = src->attrs;
    dst->type = src->type;
}

typedef struct jit_basic_block {
    // the start and end range of this basic block
    uint32_t start;
    uint32_t end;

    // leave target, larger than zero if any
    uint32_t* leave_target_stack;

    // the stack at the start of the block
    jit_value_t* stack;

    // the locals at the start of the block
    jit_value_t* locals;

    // the locals at the start of the block
    jit_value_t* args;

    // the jit block
    spidir_block_t block;

    // is this block initialized
    bool initialized;

    // the block state
    bool in_queue;

    // did we emit the block already
    bool emitted;

    // request to generate phis for the stack
    // and for locals (only required if the block
    // has multiple callers)
    bool need_phis;
} jit_basic_block_t;

typedef struct jit_leave_block_key {
    jit_basic_block_t* block;
    uint64_t leave_target;
} jit_leave_block_key_t;

typedef struct jit_method {
    // the C# method we are handling right now
    RuntimeMethodBase method;

    // the queue of blocks to process
    jit_basic_block_t** block_queue;

    // the list of basic blocks in the method
    jit_basic_block_t** basic_blocks;

    // the initial locals state
    jit_value_t* locals;

    // the initial args state
    jit_value_t* args;

    // the value used when returning a struct
    spidir_value_t return_ref;

    // list of labels, from pc -> basic block index
    // this is used to find jump targets
    struct {
        uint32_t key;
        jit_basic_block_t* value;
    }* labels;

    // blocks duplicated as part of being in a leave path
    // they are initialized to a default value but are
    // associated with a leave target
    // the hashmap owns the basic blocks in it
    struct {
        jit_leave_block_key_t key;
        jit_basic_block_t* value;
    }* leave_blocks;

    // more advanced return information used mainly for inline
    jit_value_attrs_t return_attrs;
    RuntimeTypeInfo return_type;
    bool has_return_type_info;

    spidir_phi_t inline_return_phi;
    spidir_block_t inline_return_block;

    // are we inlining right now
    int inline_level;

    // the spidir function for this method
    spidir_function_t function;

    // is the method verified yet
    bool verified;

    // the method's state
    bool emitting;
} jit_method_t;

static inline bool jit_is_interface(RuntimeTypeInfo type) {
    return type != NULL && type->Attributes.Interface;
}

static inline bool jit_is_delegate(RuntimeTypeInfo type) {
    return type != NULL && type->BaseType == tMulticastDelegate;
}

static inline bool jit_is_struct(RuntimeTypeInfo type) {
    // Anything which is not a value type but not a
    // native type is a struct
    return tdn_type_is_valuetype(type) && !type->IsByRef &&
            type != tByte && type != tSByte &&
            type != tInt16 && type != tUInt16 &&
            type != tInt32 && type != tUInt32 &&
            type != tInt64 && type != tUInt64 &&
            type != tIntPtr && type != tUIntPtr &&
            type != tBoolean && type != tChar;
}

static inline bool jit_is_struct_like(RuntimeTypeInfo type) {
    return jit_is_interface(type) || jit_is_struct(type) || jit_is_delegate(type);
}

static inline size_t jit_get_boxed_value_offset(RuntimeTypeInfo type) {
    return ALIGN_UP(sizeof(struct Object), type->StackAlignment);
}

/*
 * Create or get the jit method for the given method
 *
 * This will also make sure to queue the emitting of the method
 * after the verification stage
 */
tdn_err_t jit_get_method(RuntimeMethodBase method, jit_method_t** jit_method);

/**
 * Destroy a jit method instance
 */
void jit_destroy_method(jit_method_t* method);

/**
 * Register the thunk of the given method
 */
void jit_method_register_thunk(jit_method_t* method);

/**
 * Get the jit method from the spidir function
 */
jit_method_t* jit_get_method_from_function(spidir_function_t function);

/**
 * Find an enclosing try clause of the given type
 * which comes before the given one
 *
 * @param method    [IN] The method to check
 * @param pc        [IN] The PC we are in
 * @param type      [IN] The clause type we want
 * @param previous  [IN] The clause to stop at
 */
RuntimeExceptionHandlingClause jit_get_enclosing_try_clause(jit_method_t* method, uint32_t pc, int type, RuntimeExceptionHandlingClause previous);

/**
 * clear the created jit methods, done as part of cleaning up the codegen
 */
void jit_clean();

RuntimeTypeInfo jit_get_reduced_type(RuntimeTypeInfo type);
RuntimeTypeInfo jit_get_verification_type(RuntimeTypeInfo type);
RuntimeTypeInfo jit_get_intermediate_type(RuntimeTypeInfo type);

