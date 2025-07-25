#pragma once

#include <stdint.h>
#include <stdbool.h>

#include <tomatodotnet/types/reflection.h>

#include "basic_block.h"
#include "tomatodotnet/types/type.h"
#include "tomatodotnet/util/stb_ds.h"

typedef enum stack_value_kind : uint8_t {
    KIND_UNKNOWN,
    KIND_INT32,
    KIND_INT64,
    KIND_NATIVE_INT,
    KIND_FLOAT,
    KIND_BY_REF,
    KIND_OBJ_REF,
    KIND_VALUE_TYPE,
} stack_value_kind_t;

typedef struct value_flags {
    // The reference is read-only
    bool ref_read_only;

    // The reference is non-local
    bool ref_non_local;

    // The ref-struct contains only non-local references
    bool ref_struct_non_local;

    // Is the instance the `this` of the method
    bool this_ptr;

    // was this pushed via ldftn
    bool ldftn;
} value_flags_t;

typedef struct block_local {
    // If the type of the local is delegate this
    // is the method behind it
    RuntimeMethodBase method;

    // the flags of this value
    value_flags_t flags;

    // was the value initialized, whenever stloc is called it will
    // be set to true, if ldloc/ldloca is called with this being false
    // we will zero initialize it at the start of the function
    bool initialized;
} block_local_t;

typedef struct stack_value {
    // The type of this stack value
    RuntimeTypeInfo type;

    // The method of this stack value,
    // if the type is delegate, this is
    // the method that is behind this
    // delegate
    RuntimeMethodBase method;

    // the flags of this stack value
    value_flags_t flags;

    // The kind of value in this slot
    stack_value_kind_t kind;
} stack_value_t;


/**
 * Check if a stack value represents a null-reference
 */
static inline bool verifier_is_null_reference(stack_value_t* value) { return value->kind == KIND_OBJ_REF && value->type == NULL; }
static inline bool verifier_is_boxed_value_type(stack_value_t* value) { return value->kind == KIND_OBJ_REF && value->type != NULL && tdn_type_is_valuetype(value->type); }

/**
 * Initialize a stack-value
 */
stack_value_t* stack_value_init(stack_value_t* value, RuntimeTypeInfo type);

static inline stack_value_t stack_value_create(RuntimeTypeInfo type) {
    stack_value_t value = {};
    stack_value_init(&value, type);
    return value;
}

stack_value_kind_t get_type_kind(RuntimeTypeInfo type);

typedef struct block {
    // the basic block range
    basic_block_t block;

    // the arguments at the start of the basic block
    block_local_t* args;

    // the locals at the start of the basic block
    block_local_t* locals;

    // the stack at the start of the basic block
    stack_value_t* stack;

    // is this block already in the queue?
    bool in_queue;

    // was this block visited once
    bool visited;

    // did we initialize the this pointer
    // by calling its base ctor
    bool this_initialized;
} block_t;

typedef struct local {
    RuntimeTypeInfo type;
} local_t;

typedef struct function {
    // the method that we are dealing with
    RuntimeMethodBase method;

    // map of basic blocks
    basic_block_entry_t* labels;

    // the verifier blocks
    block_t* blocks;

    // the types of all the args
    local_t* args;

    // the types of all the locals
    local_t* locals;

    // the block that is used to enter the function, this is initialized with
    // all of the initial type information, and for inline functions should
    // include the inline values
    block_t entry_block;

    // the queue of blocks to verify
    block_t** queue;

    // ensure that the base ctor is
    // called at some point
    bool track_ctor_state;

    // do we have a valid this object, we don't if
    // ldarga/starg is used on the first argument
    bool modifies_this_type;

    // should we allow the function to use
    // unsafe operations
    bool allow_unsafe;
} function_t;

/**
 * Returns the basic block at the target pc, if given a leave stack it will return the next finally block
 * that needs to run before going into the real target
 */
static inline block_t* verifier_get_block(function_t* function, uint32_t target_pc) {
    basic_block_entry_t* b = hmgetp_null(function->labels, target_pc);
    if (b == NULL) {
        return NULL;
    }
    return &function->blocks[b->value.index];
}

static inline void verifier_queue_block(function_t* function, block_t* block) {
    if (!block->in_queue) {
        arrpush(function->queue, block);
        block->visited = true;
        block->in_queue = true;
    }
}


/**
 * Search for a clause in the function, if previous is given will continue from that point
 */
RuntimeExceptionHandlingClause verifier_get_enclosing_try_clause(function_t* function, uint32_t pc, int type, RuntimeExceptionHandlingClause previous);
RuntimeExceptionHandlingClause verifier_get_enclosing_handler_clause(function_t* function, uint32_t pc, int type, RuntimeExceptionHandlingClause previous);
RuntimeExceptionHandlingClause verifier_get_enclosing_filter_clause(function_t* function, uint32_t pc);

/**
 * Get the type definition, if a generic method
 */
RuntimeTypeInfo verifier_get_type_definition(RuntimeTypeInfo typ);

/**
 * Get the generic version of the function if the function is either generic
 * or is part of a generic type
 */
RuntimeMethodBase verifier_get_typical_method_definition(RuntimeMethodBase method);
