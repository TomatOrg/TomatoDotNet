#pragma once

#include <tomatodotnet/disasm.h>
#include <tomatodotnet/except.h>

#include "jit_basic_block.h"
#include "jit_emit.h"

#define JIT_VERBOSE_VERIFY

typedef struct jit_verifier_stack {
    // the actual type of the slot
    RuntimeTypeInfo type;

    union {
        // the underlying type when the value is boxed (type == tObject)
        RuntimeTypeInfo boxed_type;

        // the underlying method when the value is a delegate (type.BaseType == tMulticastDelegate)
        RuntimeMethodInfo method;
    };

    // is the type an exact match, or could it maybe
    // be something higher up the chain
    bool is_exact_type;

    //
    // Reference related
    //

    // is this a read-only reference
    bool readonly_ref;

    // the reference is non-local
    bool non_local_ref;

    // the ref-struct is non-local, so the references it
    // contains are non-local as well
    bool non_local_ref_struct;
} jit_verifier_stack_t;

typedef struct jit_verifier_local {
    // the stack related attributes
    jit_verifier_stack_t stack;

    // if ldloca was used then this will be set to true, letting the rest of the function
    // know that it should be used by reference and not by value
    bool spilled;

    // was the value initialized, whenever stloc is called it will
    // be set to true, if ldloc/ldloca is called with this being false
    // we will zero initialize it at the start of the function
    bool initialized;
} jit_verifier_local_t;

typedef struct jit_verifier_block {
    // the basic block range
    jit_basic_block_t block;

    // the arguments at the start of the basic block
    jit_verifier_local_t* args;

    // the locals at the start of the basic block
    jit_verifier_local_t* locals;

    // the stack at the start of the basic block
    jit_verifier_stack_t* stack;

    // is this block already in the queue?
    bool in_queue;

    // does this block have multiple predecessors?
    bool multiple_predecessors;

    // was this block visited once
    bool visited;
} jit_verifier_block_t;

typedef struct jit_verifier {
    // the method that we are dealing with
    RuntimeMethodBase method;

    // map of basic blocks
    jit_basic_blocks_t* labels;

    // the verifier blocks
    jit_verifier_block_t* blocks;

    // the queue of blocks to verify
    jit_verifier_block_t** queue;
} jit_verifier_t;

/**
 * Initialize a new verifier, this will create the labels lookup
 * and initialize the attributes of the first block
 */
tdn_err_t jit_verifier_init(jit_verifier_t* verifier, RuntimeMethodBase method);

/**
 * Destroy the verifier
 */
void jit_verifier_destroy(jit_verifier_t* verifier);

/**
 * Fully run the verifier, until it reaches a stable type information
 * position.
 */
tdn_err_t jit_verify(jit_verifier_t* verifier);
