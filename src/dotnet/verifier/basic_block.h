#pragma once

#include <stdint.h>
#include <tomatodotnet/except.h>
#include <tomatodotnet/types/reflection.h>

typedef struct basic_block {
    // the clauses that are
    // directly around the block
    RuntimeExceptionHandlingClause try_clause;
    RuntimeExceptionHandlingClause handler_clause;
    RuntimeExceptionHandlingClause filter_clause;

    // the start and end range of this basic block
    uint32_t start;
    uint32_t end;

    // the index of the basic block, easier for when
    // wanting to reference another basic block datastructure
    int index;

    // mark the start of regions
    bool try_start;
    bool filter_start;
    bool handler_start;
} basic_block_t;

typedef struct basic_blocks {
    // the pc of the basic block
    uint32_t key;

    // the basic block range
    basic_block_t value;
} basic_block_entry_t;

/**
 * Parse the code to get a hashmap of pc -> basic block
 *
 * @param method                [IN] The method to find the basic blocks of
 * @param out_basic_blocks      [OUT] Hashmap of basic blocks
 */
tdn_err_t verifier_find_basic_blocks(RuntimeMethodBase method, basic_block_entry_t** out_basic_blocks, bool* modified_this_type);
