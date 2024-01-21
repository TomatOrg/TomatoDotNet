#pragma once

#include "tomatodotnet/types/type.h"
#include "tomatodotnet/except.h"
#include "tomatodotnet/disasm.h"

#include "util/stb_ds.h"
#include "util/except.h"
#include "jit_interface.h"

/**
 * Initialize the jit backend
 */
tdn_err_t tdn_jit_init();

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
    jit_value_t value;

    // metadata about the stack item
    stack_meta_t meta;
} eval_stack_item_t;

typedef struct eval_stack_value_instance {
    // the type these stack-slots are reserved for
    RuntimeTypeInfo key;

    // list of stack-slots for this type
    jit_value_t* stack;

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
    jit_phi_t phi;

    // the value for accessing the item, can
    // either be the input value or a phi node
    jit_value_t value;
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
    jit_value_t value
);

/**
 * Push a new value to the eval stack, also adding metadata to the stack slot
 */
tdn_err_t eval_stack_push_with_meta(
    eval_stack_t* stack,
    RuntimeTypeInfo type,
    jit_value_t value,
    stack_meta_t meta
);

/**
 * Pushes a new struct to the stack, giving
 */
tdn_err_t eval_stack_alloc(
    eval_stack_t* stack,
    jit_builder_t builder,
    RuntimeTypeInfo type,
    jit_value_t* out_value
);

/**
 * Pop a value from the stack
 */
tdn_err_t eval_stack_pop(
    eval_stack_t* stack,
    RuntimeTypeInfo* out_type,
    jit_value_t* out_value,
    stack_meta_t* meta
);

/**
 * Creates a snapshot using the current stack at the target label
 * assuming we are coming from the current label
 */
tdn_err_t eval_stack_snapshot(
    jit_builder_t builder,
    eval_stack_t* stack,
    jit_label_t* target
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
    jit_builder_t builder,
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
// Jit contexts
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct jit_label {
    // the address of the label
    uint32_t address;

    // the spidir block of the label
    jit_block_t block;

    // the stack snapshot at the label's location
    eval_stack_snapshot_t snapshot;

    // did we visit this label already?
    bool visited;

    // copied from the label location
    bool needs_phi;
} jit_label_t;

typedef struct jit_region jit_region_t;

typedef struct finally_handler {
    // the location this finally goes to
    uint32_t key;

    // the region this finally handler uses
    jit_region_t* region;
} finally_handler_t;

typedef struct finally_handlers {
    finally_handler_t* finally_paths;
} finally_handlers_t;

/**
 * Gives the context for a single region pass, this is needed because
 * in some cases we will have multiple passes over the same region
 */
typedef struct jit_region {
    // the labels within the region for this pass
    jit_label_t* labels;

    // the current label we are on
    int label_index;

    // the clause we are inside of
    RuntimeExceptionHandlingClause clause;

    // the index of the clause we are in
    int clause_index;

    // the end of this region
    uint32_t pc_start;
    uint32_t pc_end;

    // the block to use when entering the region
    jit_block_t entry_block;

    // is this the handler part of the clause
    bool is_handler;

    // do we have a current block to use
    bool has_block;

    // the finally handlers of this region
    finally_handlers_t* finally_handlers;

    // is this a non-faulting finally path
    bool is_finally_path;

    // the currently processed finally path, -1 at the start
    // or INT32_MAX if this has no finally paths
    int current_finally_path;

    // if this is a finally path then this is the next
    // block we need to go to after this finally
    jit_block_t next_block;
    bool has_next_block;
} jit_region_t;

/**
 * Free the resources related to a region
 */
void jit_region_free(jit_region_t* region);

/**
 * Get the index of a label location by its address
 */
jit_label_t* jit_get_label(jit_region_t* ctx, uint32_t address);

typedef struct jit_label_location {
    // the location of the label
    uint32_t pc;

    // do we need a phi at that location becuase
    // multiple people will jump into it
    bool needs_phi;

    // did we create this label yet?
    bool created;
} jit_label_location_t;

typedef struct jit_arg {
    // the value to get the argument, either a param-ref
    // or a stackslot, depending on spilled
    jit_value_t value;

    // the type of the argument
    RuntimeTypeInfo type;

    // did we spill this argument
    bool spilled;
} jit_arg_t;

typedef struct jit_context {
    // the method we are jitting
    RuntimeMethodBase method;

    // the current builder
    jit_builder_t builder;

    // the locations where labels reside, only used
    // as a reference from the first global pass
    jit_label_location_t* labels;

    // the argument of the currently jitted function
    jit_arg_t* args;

    // the stack-slots for each of the local variables
    // of this function
    jit_value_t* locals;

    // the eval stack, it is the same no matter where we are
    eval_stack_t stack;

    // the protected and handler regions we have
    jit_region_t* protected_regions;
    jit_region_t* handler_regions;

    // the stack of regions we are nested in
    jit_region_t** regions;
} jit_context_t;

/**
 * Get the index of a label location by its address
 */
int jit_get_label_location_index(jit_context_t* ctx, uint32_t address, bool exact);

/**
 * Create a new label location
 */
void jit_add_label_location(jit_context_t* ctx, uint32_t address);
