#pragma once

#include <dotnet/types.h>
#include <spidir/module.h>
#include <tomatodotnet/types/reflection.h>

// enable printing while verifying
#define JIT_VERBOSE_VERIFY
#define JIT_DEBUG_VERIFY

#ifdef JIT_VERBOSE_VERIFY
    #define JIT_DEBUG_VERIFY
#endif

// enable printing while emitting
#define JIT_VERBOSE_EMIT
#define JIT_DEBUG_EMIT

#ifdef JIT_VERBOSE_EMIT
    #define JIT_DEBUG_EMIT
#endif

typedef struct jit_item_attrs {
    // the known type, only set if we know this is the exact
    // type given, for de-virt
    RuntimeTypeInfo known_type;

    // is this a readonly reference
    size_t readonly : 1;

    // this is a non-local reference
    // also applies for ref-structs
    size_t nonlocal_ref : 1;

    // the value holds the this ptr
    size_t this_ptr : 1;
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

typedef struct jit_method {
    // the C# method we are handling right now
    RuntimeMethodBase method;

    // the queue of blocks to process
    int* block_queue;

    // the list of basic blocks in the method
    jit_basic_block_t* basic_blocks;

    // the method's state
    bool verifying;

    // the locals of the method
    jit_local_t* locals;

    // the args of the method
    jit_arg_t* args;

    // list of labels, from pc -> basic block index
    // this is used to find jump targets
    struct {
        uint32_t key;
        int value;
    }* labels;

    // the spidir function for this method
    spidir_function_t function;
} jit_method_t;

static inline bool jit_is_interface(RuntimeTypeInfo type) {
    return type->Attributes.Interface;
}

static inline bool jit_is_struct(RuntimeTypeInfo type) {
    // Anything which is not a value type but not a
    // native type is a struct
    type = tdn_get_intermediate_type(type);
    return tdn_type_is_valuetype(type) &&
            type != tInt32 &&
            type != tInt64 &&
            type != tIntPtr;
}

static inline bool jit_is_struct_like(RuntimeTypeInfo type) {
    return jit_is_interface(type) || jit_is_struct(type);
}

/*
 * Create or get the jit method for the given method
 *
 * This will also make sure to queue the emitting of the method
 * after the verification stage
 */
tdn_err_t jit_get_or_create_method(RuntimeMethodBase method, jit_method_t** jit_method);

/**
 * Get the jit method from the spidir function
 */
jit_method_t* jit_get_method_from_function(spidir_function_t function);

/**
 * clear the created jit methods, done as part of cleaning up the codegen
 */
void jit_clean();
