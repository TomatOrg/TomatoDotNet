#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <util/defs.h>

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

typedef union value_flags {
    struct {
        // Is this reference readonly
        uint8_t ref_read_only : 1;

        // Is this reference pointing to a non-local value
        uint8_t ref_non_local : 1;

        // Does the ref-struct have only non-local pointers, this is valid on a local variable
        uint8_t ref_struct_non_local : 1;

        // Is the instance the `this` of the method, this is
        uint8_t this_ptr : 1;

        // was this pushed via ldftn
        uint8_t ldftn : 1;

        uint8_t : 3;
    };
    uint8_t packed;
} value_flags_t;
STATIC_ASSERT(sizeof(value_flags_t) == sizeof(uint8_t));

typedef union block_local {
    struct {
        // Is there a read-only ref
        uint8_t ref_read_only : 1;

        // Is there a non-local ref
        uint8_t ref_non_local : 1;

        // Is there a ref-struct that contains only non-local refs
        uint8_t ref_struct_non_local : 1;

        uint8_t : 5;
    };
    uint8_t packed;
} block_local_t;
STATIC_ASSERT(sizeof(block_local_t) == sizeof(uint8_t));

static inline void block_local_from_value_flags(block_local_t* local, value_flags_t flags) {
    local->ref_read_only = flags.ref_read_only;
    local->ref_non_local = flags.ref_non_local;
    local->ref_struct_non_local = flags.ref_struct_non_local;
}

static inline void value_flags_from_block_local(value_flags_t* flags, block_local_t local) {
    flags->ref_read_only = local.ref_read_only;
    flags->ref_non_local = local.ref_non_local;
    flags->ref_struct_non_local = local.ref_struct_non_local;
}

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
 * Check if both stack values are the same
 */
static inline bool stack_values_same(const stack_value_t* a, const stack_value_t* b) {
    return a->kind == b->kind && a->type == b->type && a->flags.packed == b->flags.packed;
}

/**
 * Check if a stack value represents a null-reference
 */
static inline bool verifier_is_null_reference(const stack_value_t* value) {
    return value->kind == KIND_OBJ_REF && value->type == NULL;
}

/**
 * Check if a stack value represents a boxed value type
 */
static inline bool verifier_is_boxed_value_type(const stack_value_t* value) {
    return value->kind == KIND_OBJ_REF && value->type != NULL && tdn_type_is_valuetype(value->type);
}

static inline bool verifier_is_byref_struct(const stack_value_t* value) {
    return (value->kind == KIND_BY_REF || value->kind == KIND_VALUE_TYPE) && value->type->IsByRefStruct;
}

/**
 * Initialize a stack-value
 */
stack_value_t* stack_value_init(stack_value_t* value, RuntimeTypeInfo type);

static inline stack_value_t stack_value_create(RuntimeTypeInfo type) {
    stack_value_t value = {};
    stack_value_init(&value, type);
    return value;
}

typedef enum block_state {
    BLOCK_STATE_UNMARKED,
    BLOCK_STATE_IS_PENDING,
    BLOCK_STATE_WAS_VERIFIED,
} block_state_t;

typedef struct block {
    // the basic block range
    basic_block_t block;

    // the arguments at the start of the basic block
    block_local_t* args;

    // the locals at the start of the basic block
    block_local_t* locals;

    // the stack at the start of the basic block
    stack_value_t* stack;

    // the current state of the block
    block_state_t state;

    // did we initialize the this pointer
    // by calling its base ctor
    bool this_initialized;

    // did we visit this once or not?
    bool visited;
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

static inline void verifier_mark_block(function_t* function, block_t* block) {
    if (block->state == BLOCK_STATE_UNMARKED) {
        arrpush(function->queue, block);
        block->state = BLOCK_STATE_IS_PENDING;
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

/**
 * Check that the src is assignable to the dest
 */
bool verifier_is_assignable(stack_value_t* src, stack_value_t* dst);
