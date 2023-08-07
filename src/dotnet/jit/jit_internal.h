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

typedef struct eval_stack_item {
    RuntimeTypeInfo type;
    spidir_value_t value;
    bool stack_slot;
} eval_stack_item_t;

typedef struct eval_stack {
    eval_stack_item_t* stack;

    spidir_value_t* i32stack;
    int i32depth;
    spidir_value_t* i64stack;
    int i64depth;
    spidir_value_t* ptrstack;
    int ptrdepth;

    int max_depth;
} eval_stack_t;

/**
 * Push a new value to the eval stack
 */
tdn_err_t eval_stack_push(eval_stack_t* stack, RuntimeTypeInfo type, spidir_value_t value);

/**
 * Pop a value from the stack
 */
tdn_err_t eval_stack_pop(eval_stack_t* stack, spidir_builder_handle_t builder, RuntimeTypeInfo* out_type, spidir_value_t* out_value);

/**
 * Move all the values on the stack to a stackslot, this is needed whenever we
 * have an intersection where more than a single jump can come from
 */
tdn_err_t eval_stack_move_to_slots(eval_stack_t* stack, spidir_builder_handle_t builder);

/**
 * Clear the eval stack
 */
void eval_stack_clear(eval_stack_t* stack);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit labels/blocks
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct jit_label {
    uint32_t address;
    spidir_block_t block;
} jit_label_t;

// TODO: non-linear search

static jit_label_t* jit_get_label(jit_label_t* labels, uint32_t address) {
    for (int i = 0; i < arrlen(labels); i++) {
        if (labels[i].address == address) {
            // already has a label in here
            return &labels[i];
        }
    }
    return NULL;
}

static jit_label_t* jit_add_label(jit_label_t** labels, uint32_t address) {
    int i;
    for (i = 0; i < arrlen(*labels); i++) {
        if ((*labels)[i].address > address) {
            break;
        } else if ((*labels)[i].address == address) {
            // already has a label in here, we will signal
            // that we need to move to a stack slot
            return NULL;
        }
    }

    jit_label_t label = {
        .address = address
    };
    arrins(*labels, i, label);
    return &((*labels)[i]);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Context
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct jit_context {
    // the method we are jitting
    RuntimeMethodBase method;

    // the eval stack we have currently
    eval_stack_t* stack;

    // the currently looked at opcode
    tdn_il_inst_t* inst;

    // the start pc of the current instruction
    uint32_t pc;

    // the next PC of the instruction
    uint32_t next_pc;
} jit_context_t;

static tdn_err_t jit_get_this_type(jit_context_t* ctx, RuntimeTypeInfo* out_type) {
    tdn_err_t err = TDN_NO_ERROR;

    if (ctx->method->Attributes.Static) {
        *out_type = NULL;
    } else if (tdn_type_is_valuetype(ctx->method->DeclaringType)) {
        CHECK_AND_RETHROW(tdn_get_byref_type(ctx->method->DeclaringType, out_type));
    } else {
        *out_type = ctx->method->DeclaringType;
    }

cleanup:
    return err;
}

#define JIT_THIS_TYPE \
    ({ \
        RuntimeTypeInfo __type; \
        CHECK_AND_RETHROW(jit_get_this_type(ctx, &__type)); \
        __type; \
    })
