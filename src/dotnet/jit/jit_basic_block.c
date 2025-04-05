#include "jit_basic_block.h"

#include <tomatodotnet/disasm.h>
#include <util/alloc.h>
#include <util/except.h>
#include <util/stb_ds.h>
#include <util/string.h>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Block parser
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void swap_basic_blocks(jit_basic_block_t* a, jit_basic_block_t* b) {
    jit_basic_block_t temp = *a;
    *a = *b;
    *b = temp;
}

static int partition_basic_blocks(jit_basic_block_t* arr, int low, int high) {
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

static void sort_basic_blocks(jit_basic_block_t* arr, int low, int high) {
    if (low < high) {
        int pi = partition_basic_blocks(arr, low, high);
        sort_basic_blocks(arr, low, pi - 1);
        sort_basic_blocks(arr, pi + 1, high);
    }
}

typedef enum block_flags {
    BLOCK_TRY_START = 1 << 0,
    BLOCK_HANDLER_START = 1 << 1,
    BLOCK_FILTER_START = 1 << 2,
} block_flags_t;

static void jit_add_basic_block(
    jit_basic_block_t** basic_blocks,
    jit_basic_block_entry_t** out_basic_blocks,
    uint32_t pc,
    block_flags_t flags
) {
    if (hmgeti(*out_basic_blocks, pc) < 0) {
        jit_basic_block_t block = {
            .start = pc,
            .end = -1,
            .try_start = (flags & BLOCK_TRY_START) != 0,
            .handler_start = (flags & BLOCK_HANDLER_START) != 0,
            .filter_start = (flags & BLOCK_FILTER_START) != 0,
        };

        arrpush(*basic_blocks, block);
        hmput(*out_basic_blocks, pc, block);
    }
}

static void jit_find_enclosing_exception_regions(RuntimeMethodBody body, jit_basic_block_entry_t* basic_blocks) {
    RuntimeExceptionHandlingClause_Array exception_regions = body->ExceptionHandlingClauses;
    if (exception_regions == NULL) {
        return;
    }

    for (int i = 0; i < hmlen(basic_blocks); i++) {
        jit_basic_block_t* block = &basic_blocks[i].value;

        uint32_t offset = block->start;
        for (int j = 0; j < exception_regions->Length; j++) {
            RuntimeExceptionHandlingClause r = exception_regions->Elements[j];

            // check if its inside the try region
            if (r->TryOffset <= offset && offset < r->TryOffset + r->TryLength) {
                if (block->try_clause == NULL) {
                    block->try_clause = r;
                } else {
                    RuntimeExceptionHandlingClause current = block->try_clause;
                    if (
                        current->TryOffset < r->TryOffset &&
                        current->TryOffset + current->TryLength > r->TryOffset + r->TryLength
                    ) {
                        block->try_clause = r;
                    }
                }
            }

            // check if its inside the handler region
            if (r->HandlerOffset <= offset && offset < r->HandlerOffset + r->HandlerLength) {
                if (block->handler_clause == NULL) {
                    block->handler_clause = r;
                } else {
                    RuntimeExceptionHandlingClause current = block->handler_clause;
                    if (
                        current->HandlerOffset < r->HandlerOffset &&
                        current->HandlerOffset + current->HandlerLength > r->HandlerOffset + r->HandlerLength
                    ) {
                        block->handler_clause = r;
                    }
                }
            }

            // Check if inside the filter region
            if (
                r->Flags == COR_ILEXCEPTION_CLAUSE_FILTER &&
                r->FilterOffset <= offset && offset < r->HandlerOffset
            ) {
                if (block->filter_clause == NULL) {
                    block->filter_clause = r;
                }
            }
        }
    }
}

tdn_err_t jit_find_basic_blocks(RuntimeMethodBase method, jit_basic_block_entry_t** out_basic_blocks) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBody body = method->MethodBody;
    jit_basic_block_t* ctx = {};

    // the first block always exists
    jit_add_basic_block(&ctx, out_basic_blocks, 0, 0);

    //
    // add all the finally/filter/fault/catch regions
    //
    if (body->ExceptionHandlingClauses != NULL) {
        for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
            RuntimeExceptionHandlingClause clause = body->ExceptionHandlingClauses->Elements[i];

            jit_add_basic_block(&ctx, out_basic_blocks, clause->TryOffset, BLOCK_TRY_START);
            jit_add_basic_block(&ctx, out_basic_blocks, clause->HandlerOffset, BLOCK_HANDLER_START);
            if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
                jit_add_basic_block(&ctx, out_basic_blocks, clause->FilterOffset, BLOCK_FILTER_START);
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
            jit_add_basic_block(&ctx, out_basic_blocks, inst.operand.branch_target, 0);

        } else if (inst.control_flow == TDN_IL_CF_COND_BRANCH) {
            // and now add the new blocks, make sure the basic block at the current PC
            // will
            jit_add_basic_block(&ctx, out_basic_blocks, inst.operand.branch_target, 0);
            jit_add_basic_block(&ctx, out_basic_blocks, pc, 0);
        }

        // TODO: support for switch
    }

    // sort the basic blocks
    sort_basic_blocks(ctx, 0, arrlen(ctx) - 1);

    // now that we have the sorted list of blocks we can set the index
    // and the end of each of the blocks
    for (int i = 0; i < arrlen(ctx); i++) {
        jit_basic_block_t* block = &hmgetp_null(*out_basic_blocks, ctx[i].start)->value;
        block->index = i;
        if (i != arrlen(ctx) - 1) {
            block->end = ctx[i + 1].start;
        } else {
            block->end = body->ILSize;
        }
    }

    // find the exception regions already
    jit_find_enclosing_exception_regions(body, *out_basic_blocks);

cleanup:
    if (IS_ERROR(err)) {
        hmfree(*out_basic_blocks);
    }
    arrfree(ctx);

    return err;
}
