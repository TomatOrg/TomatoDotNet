#pragma once

#include <spidir/module.h>
#include <tomatodotnet/except.h>
#include <tomatodotnet/types/type.h>

typedef struct jit_emitter_block {
    // the entry block to jump into this block
    spidir_block_t block;

    // the phis for this block, NULL if there
    // is only one entry into this block
    spidir_phi_t* arg_phis;
    spidir_phi_t* local_phis;
    spidir_phi_t* stack_phis;

    // the values we actually use when looking
    // at the elements of this function
    spidir_value_t* arg_values;
    spidir_value_t* local_values;
    spidir_value_t* stack_values;

    // is this block already in the queue
    bool in_queue;
} jit_emitter_block_t;

typedef struct jit_emitter {

} jit_emitter_t;
