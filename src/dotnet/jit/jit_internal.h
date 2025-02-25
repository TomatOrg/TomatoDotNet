#pragma once

#include <dotnet/types.h>
#include <spidir/module.h>
#include <tomatodotnet/types/reflection.h>
#include <util/defs.h>

// enable printing while verifying
// #define JIT_VERBOSE_VERIFY
// #define JIT_DEBUG_VERIFY

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

typedef struct jit_item_attrs {
    // the known type, only set if we know this is the exact
    // type given, for de-virt
    union {
        RuntimeTypeInfo known_type;
        RuntimeMethodBase method;
    };

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

    // the method is valid
    size_t is_method : 1;
} jit_item_attrs_t;

typedef struct jit_stack_value {
    // the type on the verification stack
    RuntimeTypeInfo type;

    // the attributes of this slot
    jit_item_attrs_t attrs;

    // the jit value of this
    spidir_value_t value;

    // the phi we created for this slot
    spidir_phi_t phi;
} jit_stack_value_t;

typedef enum jit_basic_block_state {
    // not processed yet
    JIT_BLOCK_NONE,

    // pending for verification
    JIT_BLOCK_PENDING_VERIFY,

    // verified
    JIT_BLOCK_VERIFIED,

    // pending for emit
    JIT_BLOCK_PENDING_EMIT,

    // the block is emitted
    JIT_BLOCK_FINISHED,
} jit_basic_block_state_t;

typedef struct jit_basic_block {
    // the start and end range of this basic block
    uint32_t start;
    uint32_t end;

    // leave target, larger than zero if any
    uint32_t* leave_target_stack;

    // the stack at the entry point
    jit_stack_value_t* stack;

    // the attributes of all the locals at the entry point
    jit_item_attrs_t* locals;

    // is this block verified
    jit_basic_block_state_t state;

    // the jit block
    spidir_block_t block;

    // is this block initialized
    bool initialized;

    // do we need a phi on the stack entries
    bool needs_phi;
} jit_basic_block_t;

typedef struct jit_arg {
    // the type of the argument
    RuntimeTypeInfo type;

    // the value, either the spill location if
    // was spilled or the argument reference itself
    spidir_value_t value;

    // is this the this type
    bool spill_required;
} jit_arg_t;

typedef struct jit_local {
    // the type of the local
    RuntimeTypeInfo type;

    // The value of the local
    spidir_value_t value;
} jit_local_t;

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

    // the locals of the method
    jit_local_t* locals;

    // the args of the method
    jit_arg_t* args;

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

    // the spidir function for this method
    spidir_function_t function;

    // the static stub of the method
    spidir_function_t thunk;

    // the method's state
    bool verifying;

    // do we have a static stub
    bool has_thunk;
} jit_method_t;

static inline bool jit_is_interface(RuntimeTypeInfo type) {
    return type->Attributes.Interface;
}

static inline bool jit_is_delegate(RuntimeTypeInfo type) {
    return type->BaseType == tMulticastDelegate || type == tMulticastDelegate || type == tDelegate;
}

static inline bool jit_is_struct(RuntimeTypeInfo type) {
    // Anything which is not a value type but not a
    // native type is a struct
    type = tdn_get_intermediate_type(type);
    return tdn_type_is_valuetype(type) &&
            type != tInt32 &&
            type != tInt64 &&
            type != tIntPtr &&
            !type->IsByRef;
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
tdn_err_t jit_get_or_create_method(RuntimeMethodBase method, jit_method_t** jit_method);

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
