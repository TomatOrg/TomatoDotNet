#pragma once

#include <stdbool.h>

#include "dotnet/verifier/basic_block.h"
#include "spidir/module.h"
#include "tomatodotnet/types/type.h"
#include "tomatodotnet/util/stb_ds.h"
#include "util/string.h"

typedef struct stack_value {
    // the actual type we have on the stack right now
    RuntimeTypeInfo type;

    // for delegates tracks the instance method, so
    // it can be inlined when possible
    RuntimeMethodInfo method;

    // the kind of the type
    spidir_value_type_t kind;

    // the current value on the stack
    spidir_value_t value;

    // is the type exact or not, exact type
    // is equiv to being sealed
    bool exact_type;
} stack_value_t;

typedef struct block_local {
    // the type of the local
    stack_value_t value;

    // the phi of the local, if
    // not spilled
    spidir_phi_t phi;

    // was the value initialized whenever stloc is called it will
    // be set to true, if ldloc/ldloca is called with this being false
    // we will zero initialize it at the start of the function
    bool initialized;
} block_local_t;

typedef struct block {
    // the basic block this block represents
    basic_block_t block;

    // the leave targets of this block
    uint32_t* leave_target_stack;

    // the spidir block
    spidir_block_t jit_block;

    // the arguments at the start of the basic block
    block_local_t* args;

    // the locals at the start of the basic block
    block_local_t* locals;

    // the stack at the start of the basic block
    stack_value_t* stack;

    // does this block have multiple predecessors
    uint32_t multiple_predecessors : 1;

    // was this block visited once
    uint32_t visited : 1;

    // did we initialize the phis yet
    uint32_t  initialized_phis : 1;

    // is this block already in the queue
    uint32_t  in_queue : 1;
} block_t;

typedef struct leave_block_key {
    block_t* block;
    uint64_t leave_target;
} leave_block_key_t;

typedef struct local {
    // is zero-initialization required
    bool zero_initialize;

    // taken by reference, so we have a stack-slot
    // for this and we must use it no matter what
    bool spilled;
} local_t;

typedef struct function {
    // the method that we are dealing with
    RuntimeMethodBase method;

    // map of basic blocks
    basic_block_entry_t* labels;

    // linear list of blocks
    block_t* blocks;

    // blocks duplicated so tehy can be part of
    // a finally leave chain
    struct {
        leave_block_key_t key;
        block_t* value;
    }* leave_blocks;

    // the locals of this function
    local_t* locals;

    // the locals of this function
    local_t* args;

    // the block that is used to enter the function, this is initialized with
    // all of the initial type information, and for inline functions should
    // include the inline values
    block_t* entry_block;

    // the queue of blocks to jit
    block_t** queue;

    // are we in the emitting stage of the type propogation stage
    // mainly affects the ability to merge or not merge blocks
    bool emitting;

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
} function_t;

static inline uint32_t* jit_copy_leave_targets(uint32_t* targets) {
    if (arrlen(targets)) {
        return NULL;
    }

    uint32_t* new = NULL;
    arrsetlen(new, arrlen(targets));
    memcpy(new, targets, sizeof(uint32_t) * arrlen(targets));
    return new;
}



