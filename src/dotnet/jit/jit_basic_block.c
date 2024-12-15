#include "jit_basic_block.h"

#include <tomatodotnet/disasm.h>
#include <util/except.h>
#include <util/stb_ds.h>
#include <util/string.h>

static void swap_basic_blocks(jit_basic_block_t* a, jit_basic_block_t* b) {
    jit_basic_block_t temp = *a;
    *a = *b;
    *b = temp;
}

static int partition_basic_blocks(jit_basic_block_t arr[], int low, int high) {
    uint32_t p = arr[low].start;
    int i = low;
    int j = high;

    while (i < j) {
        while (arr[i].start <= p && i <= high - 1) {
            i++;
        }

        while (arr[j].start > p && j >= low + 1) {
            j--;
        }
        if (i < j) {
            swap_basic_blocks(&arr[i], &arr[j]);
        }
    }
    swap_basic_blocks(&arr[low], &arr[j]);
    return j;
}

static void sort_basic_blocks(jit_basic_block_t arr[], int low, int high) {
    if (low < high) {
        int pi = partition_basic_blocks(arr, low, high);
        sort_basic_blocks(arr, low, pi - 1);
        sort_basic_blocks(arr, pi + 1, high);
    }
}

static void jit_find_enclosing_exception_regions(RuntimeMethodBody body, jit_basic_block_t* block) {
    if (body->ExceptionHandlingClauses == NULL) {
        return;
    }

    for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
        RuntimeExceptionHandlingClause clause = body->ExceptionHandlingClauses->Elements[i];

        // Check if the try range
        if (clause->TryOffset <= block->start && block->start < clause->TryOffset + clause->TryLength) {
            if (block->try_clause == NULL) {
                block->try_clause = clause;
            } else {
                RuntimeExceptionHandlingClause current = block->try_clause;
                if (
                    current->TryOffset < clause->TryOffset &&
                    current->TryOffset + current->TryLength > clause->TryOffset + clause->TryLength
                ) {
                    block->try_clause = clause;
                }
            }
        }

        // Check if the handler range
        if (clause->HandlerOffset <= block->start && block->start < clause->HandlerOffset + clause->HandlerLength) {
            if (block->handler_clause == NULL) {
                block->handler_clause = clause;
            } else {
                RuntimeExceptionHandlingClause current = block->handler_clause;
                if (
                    current->HandlerOffset < clause->HandlerOffset &&
                    current->HandlerOffset + current->HandlerLength > clause->HandlerOffset + clause->HandlerLength
                ) {
                    block->handler_clause = clause;
                }
            }
        }

        // Check if the filter range
        if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER && clause->FilterOffset <= block->start && block->start < clause->HandlerOffset) {
            if (block->filter_clause == NULL) {
                block->filter_clause = clause;
            }
        }
    }
}

static void jit_add_basic_block(jit_method_t* method, uint32_t pc) {
    if (hmgeti(method->labels, pc) < 0) {
        int bi = arraddnindex(method->basic_blocks, 1);
        jit_basic_block_t* block = &method->basic_blocks[bi];
        memset(block, 0, sizeof(*block));
        block->start = pc;
        block->end = -1;
        jit_find_enclosing_exception_regions(method->method->MethodBody, block);
        hmput(method->labels, pc, bi);
    }
}

tdn_err_t jit_find_basic_blocks(jit_method_t* jmethod) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = jmethod->method;
    RuntimeMethodBody body = method->MethodBody;

    // the first block always exists
    jit_add_basic_block(jmethod, 0);

    //
    // add all the finally/filter/fault/catch regions
    //
    if (body->ExceptionHandlingClauses != NULL) {
        for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
            RuntimeExceptionHandlingClause clause = body->ExceptionHandlingClauses->Elements[i];
            jit_add_basic_block(jmethod, clause->HandlerOffset);
            if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
                jit_add_basic_block(jmethod, clause->FilterOffset);
            }
        }
    }

    // go over the opcodes
    uint32_t pc = 0;
    while (pc < body->ILSize) {
        // get the instruction and normalize it for easier processing
        tdn_il_inst_t inst;
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));
        tdn_normalize_inst(&inst);
        pc += inst.length;

        // check for basic blocks created by
        if (inst.control_flow == TDN_IL_CF_BRANCH) {
            jit_add_basic_block(jmethod, inst.operand.branch_target);
        } else if (inst.control_flow == TDN_IL_CF_COND_BRANCH) {
            // and now add the new blocks, make sure the basic block at the current PC
            // will
            jit_add_basic_block(jmethod, inst.operand.branch_target);
            jit_add_basic_block(jmethod, pc);
        }

        // TODO: support for switch
    }

    // sort the basic blocks
    sort_basic_blocks(jmethod->basic_blocks, 0, arrlen(jmethod->basic_blocks) - 1);

    // rebuild the labels lookup and fill in the
    // end of each basic block
    hmfree(jmethod->labels);
    for (int i = 0; i < arrlen(jmethod->basic_blocks); i++) {
        if (i != arrlen(jmethod->basic_blocks) - 1) {
            jmethod->basic_blocks[i].end = jmethod->basic_blocks[i + 1].start;
        } else {
            jmethod->basic_blocks[i].end = body->ILSize;
        }
        hmput(jmethod->labels, jmethod->basic_blocks[i].start, i);
    }

cleanup:
    return err;
}
