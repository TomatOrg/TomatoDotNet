#pragma once


#include <spidir/module.h>
#include <tomatodotnet/disasm.h>
#include <tomatodotnet/except.h>

#include "jit_basic_block.h"


typedef struct jit_stack_item {
    // the actual type of the slot
    RuntimeTypeInfo type;

    union {
        // the underlying type when the value is boxed (type == tObject)
        RuntimeTypeInfo boxed_type;

        // the underlying method when the value is a delegate (type.BaseType == tMulticastDelegate)
        RuntimeMethodBase method;
    };

    // the value of the stack item
    spidir_value_t value;

    // is the type an exact match, or could it maybe
    // be something higher up the chain
    size_t is_exact_type : 1;

    // do we have a method stored in here, pushed
    // by either ldftn or ldvftn
    size_t is_method : 1;

    //
    // verification related
    //

    // does this argument refer to the `this`
    // of the method
    size_t is_this : 1;

    //
    // Reference related
    //

    // is this a read-only reference
    size_t readonly_ref : 1;

    // the reference is non-local
    size_t non_local_ref : 1;

    // the ref-struct is non-local, so the references it
    // contains are non-local as well
    size_t non_local_ref_struct : 1;
} jit_stack_item_t;

typedef struct jit_block_local {
    // the stack related attributes
    jit_stack_item_t stack;

    // the phi of the local
    spidir_phi_t phi;

    // was the value initialized, whenever stloc is called it will
    // be set to true, if ldloc/ldloca is called with this being false
    // we will zero initialize it at the start of the function
    bool initialized;
} jit_block_local_t;

typedef struct jit_block {
    // the basic block range
    jit_basic_block_t block;

    // the spidir block
    spidir_block_t spidir_block;

    // the arguments at the start of the basic block
    jit_block_local_t* args;

    // the locals at the start of the basic block
    jit_block_local_t* locals;

    // the stack at the start of the basic block
    jit_stack_item_t* stack;

    // the phi's of the stack entries
    spidir_phi_t* stack_phis;

    // is this block already in the queue?
    bool in_queue;

    // does this block have multiple predecessors?
    bool multiple_predecessors;

    // was this block visited once
    bool visited;

    // did we initialize the phis yet
    bool initialized_phis;
} jit_block_t;

typedef struct jit_local {
    // the original local's type
    RuntimeTypeInfo type;

    // is zero-initialize required
    bool zero_initialize;

    // taken by reference, so we have a stack-slot
    // for this and we must use it no matter what
    bool spilled;
} jit_local_t;

typedef struct jit_function {
    // the method that we are dealing with
    RuntimeMethodBase method;

    // map of basic blocks
    jit_basic_block_entry_t* labels;

    // the verifier blocks
    jit_block_t* blocks;

    // the locals of this function
    jit_local_t* locals;

    // the locals of this function
    jit_local_t* args;

    // the block that is used to enter the function, this is initialized with
    // all of the initial type information, and for inline functions should
    // include the inline values
    jit_block_t entry_block;

    // the queue of blocks to verify
    jit_block_t** queue;

    // allow merging the stack
    // entries or not (when emitting
    // we are not allowed to)
    bool emitting;

    // the item used when returning from the method
    jit_stack_item_t return_item;

    // was the return item initialized
    bool return_item_initialized;

    //
    // inline information
    //

    // the phi for setting the return
    spidir_phi_t return_phi;

    // the block to return to when returning from the inline
    spidir_block_t return_block;

    // the depth of the inline, 0 meaning not inlining, 1 or
    // more means we are part of an inline
    int inline_depth;
} jit_function_t;

/**
 * Initialize a new verifier, this will create the labels lookup
 * and initialize the attributes of the first block
 */
tdn_err_t jit_function_init(jit_function_t* function, RuntimeMethodBase method);

/**
 * Fully run the verifier, until it reaches a stable type information
 * position.
 */
tdn_err_t jit_function(jit_function_t* function, spidir_builder_handle_t builder);

/**
 * Destroy the verifier
 */
void jit_function_destroy(jit_function_t* function);
