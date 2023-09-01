#pragma once

#include "tinydotnet/except.h"
#include "spidir/spidir.h"
#include "tinydotnet/disasm.h"
#include "util/stb_ds.h"
#include "tinydotnet/types/type.h"
#include "util/except.h"

tdn_err_t tdn_jit_init();

void tdn_jit_dump();


/**
 * Helper to check if a type is a struct type and not any other type
 */
static inline bool jit_is_struct_type(RuntimeTypeInfo type) {
    type = tdn_get_intermediate_type(type);
    return tdn_type_is_valuetype(type) &&
            type != tInt32 &&
            type != tInt64 &&
            type != tIntPtr;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Evaluation stack
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// fowrad
typedef struct jit_label jit_label_t;

typedef struct stack_meta {
    bool came_from_ldarg0;
} stack_meta_t;

typedef struct eval_stack_item {
    // the type of the item
    RuntimeTypeInfo type;

    // the value to access it
    spidir_value_t value;

    // metadata about the stack item
    stack_meta_t meta;
} eval_stack_item_t;

typedef struct eval_stack_value_instance {
    // the type these stack-slots are reserved for
    RuntimeTypeInfo key;

    // list of stack-slots for this type
    spidir_value_t* stack;

    // how much are currently in use
    int depth;
} eval_stack_value_instance_t;

typedef struct eval_stack {
    // used to track the stack items and where they are
    eval_stack_item_t* stack;

    // used to track allocations of structs on the stack
    eval_stack_value_instance_t* instance_stacks;

    // the max depth we can reach
    int max_depth;
} eval_stack_t;

typedef struct eval_stack_snapshot_item {
    // the type that was on the stack
    RuntimeTypeInfo type;

    // the phi that can be used to add more
    // inputs to the slot
    spidir_phi_t phi;

    // the value for accessing the item, can
    // either be the input value or a phi node
    spidir_value_t value;
} eval_stack_snapshot_item_t;

typedef struct eval_stack_snapshot {
    // the stack of items that were on the slot
    eval_stack_snapshot_item_t* stack;

    // did we initialize this stack
    bool initialized;
} eval_stack_snapshot_t;

/**
 * Push a new value to the eval stack
 */
tdn_err_t eval_stack_push(
    eval_stack_t* stack,
    RuntimeTypeInfo type,
    spidir_value_t value
);

/**
 * Push a new value to the eval stack, also adding metadata to the stack slot
 */
tdn_err_t eval_stack_push_with_meta(
    eval_stack_t* stack,
    RuntimeTypeInfo type,
    spidir_value_t value,
    stack_meta_t meta
);

/**
 * Pushes a new struct to the stack, giving
 */
tdn_err_t eval_stack_alloc(
    eval_stack_t* stack,
    spidir_builder_handle_t builder,
    RuntimeTypeInfo type,
    spidir_value_t* out_value
);

/**
 * Pop a value from the stack
 */
tdn_err_t eval_stack_pop(
    eval_stack_t* stack,
    spidir_builder_handle_t builder,
    RuntimeTypeInfo* out_type,
    spidir_value_t* out_value,
    stack_meta_t* meta
);

/**
 * Creates a snapshot using the current stack at the target label
 * assuming we are coming from the current label
 */
tdn_err_t eval_stack_snapshot(
    spidir_builder_handle_t builder,
    eval_stack_t* stack,
    jit_label_t* target,
    spidir_block_t current
);

/**
 * Update the stack to contain the phis of the given label
 */
tdn_err_t eval_stack_update_phis(
    eval_stack_t* stack,
    jit_label_t* label
);

/**
 * Merge an eval-stack with a snapshot of the eval stack.
 *
 * if modify is false then the merge will only do a check, and if the check fails
 * we will return an error, this is used whenever there is a backwards jump to a location
 * we already jitted.
 */
tdn_err_t eval_stack_merge(
    spidir_builder_handle_t builder,
    eval_stack_t* stack,
    jit_label_t* target,
    bool modify
);

/**
 * Clear the eval stack
 */
void eval_stack_clear(eval_stack_t* stack);

/**
 * Free the eval stack
 */
void eval_stack_free(eval_stack_t* stack);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit labels/blocks
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct jit_label {
    // the address of the label
    uint32_t address;

    // the spidir block of the label
    spidir_block_t block;

    // the stack snapshot at the label's location
    eval_stack_snapshot_t snapshot;

    // do we need a phi? set to true if there is more
    // than a single flow going into the label
    bool needs_phi;

    // did we visit this label already?
    bool visited;
} jit_label_t;

/**
 * Get a label, returning null if not found
 */
jit_label_t* jit_get_label(jit_label_t* labels, uint32_t address);

/**
 * Add a new label
 */
jit_label_t* jit_add_label(jit_label_t** labels, uint32_t address);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Context
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct jit_context {
    // the method we are jitting
    RuntimeMethodBase method;

    // the labels list
    jit_label_t* labels;

    // the eval stack we have currently
    eval_stack_t* stack;

    // the currently looked at opcode
    tdn_il_inst_t* inst;

    // the label going into the current block
    spidir_block_t current_block;

    // the start pc of the current instruction
    uint32_t pc;

    // the next PC of the instruction
    uint32_t next_pc;

    // the builder used right now
    spidir_builder_handle_t builder;
} jit_context_t;
