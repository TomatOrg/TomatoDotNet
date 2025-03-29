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

static void jit_add_basic_block(jit_basic_block_t** basic_blocks, jit_basic_block_entry_t** out_basic_blocks, uint32_t pc) {
    if (hmgeti(*out_basic_blocks, pc) < 0) {
        jit_basic_block_t block = {
            .start = pc,
            .end = -1
        };

        arrpush(*basic_blocks, block);
        hmput(*out_basic_blocks, pc, block);
    }
}

tdn_err_t jit_find_basic_blocks(RuntimeMethodBase method, jit_basic_block_entry_t** out_basic_blocks) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBody body = method->MethodBody;
    jit_basic_block_t* ctx = {};

    // the first block always exists
    jit_add_basic_block(&ctx, out_basic_blocks, 0);

    //
    // add all the finally/filter/fault/catch regions
    //
    if (body->ExceptionHandlingClauses != NULL) {
        for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
            RuntimeExceptionHandlingClause clause = body->ExceptionHandlingClauses->Elements[i];
            jit_add_basic_block(&ctx, out_basic_blocks, clause->HandlerOffset);
            if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
                jit_add_basic_block(&ctx, out_basic_blocks, clause->FilterOffset);
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
            jit_add_basic_block(&ctx, out_basic_blocks, inst.operand.branch_target);
        } else if (inst.control_flow == TDN_IL_CF_COND_BRANCH) {
            // and now add the new blocks, make sure the basic block at the current PC
            // will
            jit_add_basic_block(&ctx, out_basic_blocks, inst.operand.branch_target);
            jit_add_basic_block(&ctx, out_basic_blocks, pc);
        }

        // TODO: support for switch
    }

    // sort the basic blocks
    sort_basic_blocks(ctx, 0, arrlen(ctx) - 1);

    // rebuild the labels lookup and fill in the
    // end of each basic block
    hmfree(*out_basic_blocks);
    for (int i = 0; i < arrlen(ctx); i++) {
        if (i != arrlen(ctx) - 1) {
            ctx[i].end = ctx[i + 1].start;
        } else {
            ctx[i].end = body->ILSize;
        }
        ctx[i].index = i;
        hmput(*out_basic_blocks, ctx[i].start, ctx[i]);
    }

cleanup:
    if (IS_ERROR(err)) {
        hmfree(*out_basic_blocks);
    }
    arrfree(ctx);

    return err;
}
