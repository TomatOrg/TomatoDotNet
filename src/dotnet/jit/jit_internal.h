#pragma once

#include "tinydotnet/except.h"
#include "spidir/spidir.h"
#include "tinydotnet/disasm.h"
#include "util/stb_ds.h"
#include "tinydotnet/types/type.h"
#include "util/except.h"

tdn_err_t tdn_jit_init();

void tdn_jit_dump();

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Evaluation stack
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct stack_meta {
    bool came_from_ldarg0;
} stack_meta_t;

typedef struct eval_stack_item {
    RuntimeTypeInfo type;
    spidir_value_t value;
    stack_meta_t meta;
    bool stack_slot;
} eval_stack_item_t;

typedef struct eval_stack_value_instance {
    RuntimeTypeInfo key;
    spidir_value_t* stack;
    int depth;
} eval_stack_value_instance_t;

typedef struct eval_stack {
    eval_stack_item_t* stack;

    spidir_value_t* i32stack;
    int i32depth;
    spidir_value_t* i64stack;
    int i64depth;
    spidir_value_t* ptrstack;
    int ptrdepth;

    eval_stack_value_instance_t* instance_stacks;

    int max_depth;
} eval_stack_t;

typedef struct eval_stack_snapshot_item {
    RuntimeTypeInfo type;
} eval_stack_snapshot_item_t;

typedef struct eval_stack_snapshot {
    eval_stack_snapshot_item_t* stack;
    bool initialized;
} eval_stack_snapshot_t;

/**
 * Push a new value to the eval stack
 */
tdn_err_t eval_stack_push(eval_stack_t* stack,
                          RuntimeTypeInfo type,
                          spidir_value_t value);

/**
 * Push a new value to the eval stack, also adding metadata to the stack slot
 */
tdn_err_t eval_stack_push_with_meta(eval_stack_t* stack,
                                    RuntimeTypeInfo type,
                                    spidir_value_t value,
                                    stack_meta_t meta);

/**
 * Pushes a new struct to the stack, giving
 */
tdn_err_t eval_stack_alloc(eval_stack_t* stack,
                           spidir_builder_handle_t builder,
                           RuntimeTypeInfo type,
                           spidir_value_t* out_value);

/**
 * Pop a value from the stack
 */
tdn_err_t eval_stack_pop(eval_stack_t* stack,
                         spidir_builder_handle_t builder,
                         RuntimeTypeInfo* out_type,
                         spidir_value_t* out_value,
                         stack_meta_t* meta);

/**
 * Move all the values on the stack to a stackslot, this is needed whenever we
 * have an intersection where more than a single jump can come from
 */
tdn_err_t eval_stack_move_to_slots(eval_stack_t* stack, spidir_builder_handle_t builder);

/**
 * Create a snapshot of the current eval stack, allowing
 * to save it for a label
 */
tdn_err_t eval_stack_snapshot(eval_stack_t* stack, eval_stack_snapshot_t* out);

/**
 * Merge an eval-stack with a snapshot of the eval stack.
 *
 * if modify is false then the merge will only do a check, and if the check fails
 * we will return an error, this is used whenever there is a backwards jump to a location
 * we already jitted.
 */
tdn_err_t eval_stack_merge(eval_stack_t* stack, eval_stack_snapshot_t* snapshot, bool modify);

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

    // did we visit this label already?
    bool visited;
} jit_label_t;

jit_label_t* jit_get_label(jit_label_t* labels, uint32_t address);
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

    // the start pc of the current instruction
    uint32_t pc;

    // the next PC of the instruction
    uint32_t next_pc;

    // the builder used right now
    spidir_builder_handle_t builder;
} jit_context_t;
