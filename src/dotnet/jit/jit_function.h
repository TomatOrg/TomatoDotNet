#pragma once


#include <spidir/module.h>
#include <tomatodotnet/disasm.h>
#include <tomatodotnet/except.h>
#include <tomatodotnet/types/type.h>
#include <util/stb_ds.h>
#include <util/string.h>

#include "jit_basic_block.h"


typedef enum jit_stack_value_kind : uint8_t {
    JIT_KIND_UNKNOWN,
    JIT_KIND_INT32,
    JIT_KIND_INT64,
    JIT_KIND_NATIVE_INT,
    JIT_KIND_FLOAT,
    JIT_KIND_BY_REF,
    JIT_KIND_OBJ_REF,
    JIT_KIND_VALUE_TYPE,
} jit_stack_value_kind_t;

typedef struct jit_value_flags {
    // The reference is read-only
    bool ref_read_only;

    // The reference is non-local
    bool ref_non_local;

    // Is the instance the `this` of the method
    bool this_ptr;

    // was this pushed via ldftn
    bool ldftn;
} jit_value_flags_t;

typedef struct jit_block_local {
    // If the type of the local is delegate this
    // is the method behind it
    RuntimeMethodBase method;

    // the phi of the local
    spidir_phi_t phi;

    // the value of the local
    spidir_value_t value;

    // the flags of this value
    jit_value_flags_t flags;

    // was the value initialized, whenever stloc is called it will
    // be set to true, if ldloc/ldloca is called with this being false
    // we will zero initialize it at the start of the function
    bool initialized;
} jit_block_local_t;

typedef struct jit_stack_value {
    // The type of this stack value
    RuntimeTypeInfo type;

    // The method of this stack value,
    // if the type is delegate, this is
    // the method that is behind this
    // delegate
    RuntimeMethodBase method;

    // the spidir stack value for this
    spidir_value_t value;

    // The kind of value in this slot
    jit_stack_value_kind_t kind;

    // the flags of this stack value
    jit_value_flags_t flags;
} jit_stack_value_t;

/**
 * Check if a stack value represents a null-reference
 */
static bool jit_is_null_reference(jit_stack_value_t* value) { return value->kind == JIT_KIND_OBJ_REF && value->type == NULL; }
static bool jit_is_boxed_value_type(jit_stack_value_t* value) { return value->kind == JIT_KIND_OBJ_REF && value->type != NULL && tdn_type_is_valuetype(value->type); }

/**
 * Initialize a stack-value
 */
jit_stack_value_t* jit_stack_value_init(jit_stack_value_t* value, RuntimeTypeInfo type);

jit_stack_value_kind_t jit_get_type_kind(RuntimeTypeInfo type);

typedef struct jit_block {
    // the basic block range
    jit_basic_block_t block;

    // the leave targets of this block
    uint32_t* leave_target_stack;

    // the spidir block
    spidir_block_t spidir_block;

    // the arguments at the start of the basic block
    jit_block_local_t* args;

    // the locals at the start of the basic block
    jit_block_local_t* locals;

    // the stack at the start of the basic block
    jit_stack_value_t* stack;

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

    // did we initialize the this pointer
    // by calling its base ctor
    bool this_initialized;
} jit_block_t;

typedef struct jit_leave_block_key {
    jit_block_t* block;
    uint64_t leave_target;
} jit_leave_block_key_t;

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

    // blocks duplicated so they can be a part of
    // a finally leave chain
    struct {
        jit_leave_block_key_t key;
        jit_block_t* value;
    }* leave_blocks;

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

    // ensure that the base ctor is
    // called at some point
    bool track_ctor_state;

    // do we have a valid this object, we don't if
    // ldarga/starg is used on the first argument
    bool valid_this;

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

static inline uint32_t* jit_copy_leave_targets(uint32_t* targets) {
    if (arrlen(targets) == 0) {
        return NULL;
    }

    uint32_t* new = NULL;
    arrsetlen(new, arrlen(targets));
    memcpy(new, targets, sizeof(uint32_t) * arrlen(targets));
    return new;
}

/**
 * Returns the basic block at the target pc, if given a leave stack it will return the next finally block
 * that needs to run before going into the real target
 */
jit_block_t* jit_function_get_block(jit_function_t* function, uint32_t target_pc, uint32_t* leave_target_stack);

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

/**
 * Search for a clause in the function, if previous is given will continue from that point
 */
RuntimeExceptionHandlingClause jit_get_enclosing_try_clause(jit_function_t* function, uint32_t pc, int type, RuntimeExceptionHandlingClause previous);
RuntimeExceptionHandlingClause jit_get_enclosing_handler_clause(jit_function_t* function, uint32_t pc, int type, RuntimeExceptionHandlingClause previous);
RuntimeExceptionHandlingClause jit_get_enclosing_filter_clause(jit_function_t* function, uint32_t pc);
