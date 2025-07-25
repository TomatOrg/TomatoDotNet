#include "basic_block.h"

#include <tomatodotnet/disasm.h>
#include <util/alloc.h>
#include <util/except.h>
#include "tomatodotnet/util/stb_ds.h"
#include <util/string.h>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Block parser
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void swap_basic_blocks(basic_block_t* a, basic_block_t* b) {
    basic_block_t temp = *a;
    *a = *b;
    *b = temp;
}

static int partition_basic_blocks(basic_block_t* arr, int low, int high) {
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

static void sort_basic_blocks(basic_block_t* arr, int low, int high) {
    if (low < high) {
        int pi = partition_basic_blocks(arr, low, high);
        sort_basic_blocks(arr, low, pi - 1);
        sort_basic_blocks(arr, pi + 1, high);
    }
}

static basic_block_t* verifier_add_basic_block(
    basic_block_t** basic_blocks,
    basic_block_entry_t** out_basic_blocks,
    uint32_t pc
) {
    if (hmgeti(*out_basic_blocks, pc) < 0) {
        basic_block_t block = {
            .start = pc,
            .end = -1,
        };

        arrpush(*basic_blocks, block);
        hmput(*out_basic_blocks, pc, block);
    }

    return &hmgetp_null(*out_basic_blocks, pc)->value;
}

static void verifier_find_enclosing_exception_regions(RuntimeMethodBody body, basic_block_entry_t* basic_blocks) {
    RuntimeExceptionHandlingClause_Array exception_regions = body->ExceptionHandlingClauses;
    if (exception_regions == NULL) {
        return;
    }

    for (int i = 0; i < hmlen(basic_blocks); i++) {
        basic_block_t* block = &basic_blocks[i].value;

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

tdn_err_t verifier_find_basic_blocks(RuntimeMethodBase method, basic_block_entry_t** out_basic_blocks, bool* modified_this_type) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBody body = method->MethodBody;
    basic_block_t* ctx = {};

    // start assuming nothing modified the `this`
    *modified_this_type = false;

    // the first block always exists
    verifier_add_basic_block(&ctx, out_basic_blocks, 0);

    //
    // add all the finally/filter/fault/catch regions
    //
    if (body->ExceptionHandlingClauses != NULL) {
        for (int i = 0; i < body->ExceptionHandlingClauses->Length; i++) {
            RuntimeExceptionHandlingClause clause = body->ExceptionHandlingClauses->Elements[i];

            verifier_add_basic_block(&ctx, out_basic_blocks, clause->TryOffset)->try_start = true;
            verifier_add_basic_block(&ctx, out_basic_blocks, clause->HandlerOffset)->handler_start = true;
            if (clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
                verifier_add_basic_block(&ctx, out_basic_blocks, clause->FilterOffset)->filter_start = true;
            }
        }
    }

    // go over the opcodes
    uint32_t pc = 0;
    while (pc < body->ILSize) {
        // get the instruction and normalize it for easier processing
        tdn_il_inst_t inst = {};
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));
        tdn_normalize_inst(&inst);
        pc += inst.length;

        // check if this invalidates the `this` because it takes it by-ref, in theory
        // something can be made which is much smarter, but we will do the dumb thing
        // because that is also what C# does
        if (inst.opcode == CEE_LDARGA || inst.opcode == CEE_STARG) {
            if (inst.operand.variable == 0) {
                *modified_this_type = true;
            }
        }

        // check for basic blocks created by
        if (inst.control_flow == TDN_IL_CF_BRANCH) {
            verifier_add_basic_block(&ctx, out_basic_blocks, inst.operand.branch_target);

        } else if (inst.control_flow == TDN_IL_CF_COND_BRANCH) {
            if (inst.operand_type == TDN_IL_SWITCH) {
                // the default case
                verifier_add_basic_block(&ctx, out_basic_blocks, pc);

                // the rest of the cases
                for (int i = 0; i < arrlen(inst.operand.switch_targets); i++) {
                    verifier_add_basic_block(&ctx, out_basic_blocks, inst.operand.switch_targets[i]);
                }

            } else {
                // and now add the new blocks, make sure the basic block at the current PC
                // will
                verifier_add_basic_block(&ctx, out_basic_blocks, inst.operand.branch_target);
                verifier_add_basic_block(&ctx, out_basic_blocks, pc);
            }
        }

        // TODO: support for switch
        tdn_free_inst(&inst);
    }

    // sort the basic blocks
    sort_basic_blocks(ctx, 0, arrlen(ctx) - 1);

    // now that we have the sorted list of blocks we can set the index
    // and the end of each of the blocks
    for (int i = 0; i < arrlen(ctx); i++) {
        basic_block_t* block = &hmgetp_null(*out_basic_blocks, ctx[i].start)->value;
        block->index = i;
        if (i != arrlen(ctx) - 1) {
            block->end = ctx[i + 1].start;
        } else {
            block->end = body->ILSize;
        }
    }

    // find the exception regions already
    verifier_find_enclosing_exception_regions(body, *out_basic_blocks);

cleanup:
    if (IS_ERROR(err)) {
        hmfree(*out_basic_blocks);
    }
    arrfree(ctx);

    return err;
}
