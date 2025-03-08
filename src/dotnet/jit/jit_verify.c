#include "jit_verify.h"

#include <dotnet/types.h>
#include <tomatodotnet/types/type.h>
#include <util/string.h>
#include <util/except.h>
#include <util/stb_ds.h>

typedef enum il_pop {
    VarPop = -1,
    Pop0 = 0,
    Pop1 = 1,
} il_pop_t;

// just alias the rest
#define VarPop  (-1)
#define Pop0    0
#define Pop1    1
#define PopRef  Pop1
#define PopI    Pop1
#define PopI8   Pop1
#define PopR4   Pop1
#define PopR8   Pop1

/**
 * Metadata for all the opcodes, used for decoding, this is turned
 * into another struct which is a bit more useful
 */
static int m_il_pop_count[] = {
#define OPDEF_REAL_OPCODES_ONLY
#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) [c] = pop,
#include "tomatodotnet/opcode.def"
#undef OPDEF
#undef OPDEF_REAL_OPCODES_ONLY
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void jit_clone_block(jit_verifier_block_t* in, jit_verifier_block_t* out) {
    out->block = in->block;

    arrsetlen(out->args, arrlen(in->args));
    memcpy(out->args, in->args, arrlen(in->args) * sizeof(*in->args));

    arrsetlen(out->locals, arrlen(in->locals));
    memcpy(out->locals, in->locals, arrlen(in->locals) * sizeof(*in->locals));

    arrsetlen(out->stack, arrlen(in->stack));
    memcpy(out->stack, in->stack, arrlen(in->stack) * sizeof(*in->stack));
}

static RuntimeTypeInfo get_method_this_type(RuntimeMethodBase method) {
    RuntimeTypeInfo type = method->DeclaringType;
    if (tdn_type_is_valuetype(type)) {
        ASSERT(!IS_ERROR(tdn_get_byref_type(type, &type)));
    }
    return type;
}

static void verifier_queue_block(jit_verifier_t* verifier, jit_verifier_block_t* block) {
    if (!block->in_queue) {
        arrpush(verifier->queue, block);
        block->visited = true;
        block->in_queue = true;
    }
}

static tdn_err_t verifier_merge_block(jit_verifier_t* verifier, jit_verifier_block_t* from, jit_verifier_block_t* target) {
    tdn_err_t err = TDN_NO_ERROR;

    if (target->visited) {
        // already visited once, need to merge with the
        // type instead of setting everything as is
        target->multiple_predecessors = true;

        CHECK_FAIL();

    } else {
        // first time being visited, copy over all the type information as is

        arrsetlen(target->stack, arrlen(from->stack));
        memcpy(target->stack, from->stack, arrlen(from->stack) * sizeof(*from->stack));

        arrsetlen(target->args, arrlen(from->args));
        memcpy(target->args, from->args, arrlen(from->args) * sizeof(*from->args));

        arrsetlen(target->locals, arrlen(from->locals));
        memcpy(target->locals, from->locals, arrlen(from->locals) * sizeof(*from->locals));

        verifier_queue_block(verifier, target);
    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Verifiers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define STACK_PUSH(_type) \
    ({ \
        jit_verifier_stack_t* __item = arraddnptr(block->stack, 1); \
        __item->type = _type; \
        __item; \
    })

//----------------------------------------------------------------------------------------------------------------------
// Misc instructions
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_nop(jit_verifier_t* verifier, jit_verifier_block_t* block, tdn_il_inst_t* inst, jit_verifier_stack_t* stack) {
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// Stack manipulation
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_ldc_i4(jit_verifier_t* verifier, jit_verifier_block_t* block, tdn_il_inst_t* inst, jit_verifier_stack_t* stack) {
    STACK_PUSH(tInt32);
    return TDN_NO_ERROR;
}

static tdn_err_t verify_ldc_i8(jit_verifier_t* verifier, jit_verifier_block_t* block, tdn_il_inst_t* inst, jit_verifier_stack_t* stack) {
    STACK_PUSH(tInt64);
    return TDN_NO_ERROR;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Block visitor
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef tdn_err_t (*verify_instruction_t)(jit_verifier_t* verifier, jit_verifier_block_t* block, tdn_il_inst_t* inst, jit_verifier_stack_t* stack);

/**
 * Opcode verification table
 */
static verify_instruction_t m_verify_table[] = {
    [CEE_NOP] = verify_nop,

    [CEE_LDC_I4] = verify_ldc_i4,
    [CEE_LDC_I8] = verify_ldc_i8,
};

static tdn_err_t jit_verify_basic_block(jit_verifier_t* verifier, jit_verifier_block_t* in_block) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = verifier->method;
    RuntimeMethodBody body = method->MethodBody;
    jit_verifier_stack_t* stack_items = NULL;

    // clone the block into the current frame, so we can modify it
    jit_verifier_block_t block = {};
    jit_clone_block(in_block, &block);

#ifdef JIT_VERBOSE_VERIFY
    int indent = 0;
#endif

    // get the pc
    tdn_il_inst_t inst = { .control_flow = TDN_IL_CF_FIRST };
    uint32_t pc = block.block.start;
    while (pc < block.block.end) {
        // can only parse more instructions if we had no block
        // terminating instruction
        CHECK(
            inst.control_flow == TDN_IL_CF_FIRST ||
            inst.control_flow == TDN_IL_CF_NEXT ||
            inst.control_flow == TDN_IL_CF_META ||
            inst.control_flow == TDN_IL_CF_CALL
        );

        // get the instruction
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));

#ifdef JIT_VERBOSE_VERIFY
        indent = tdn_disasm_print_start(body, pc, inst, indent);
#endif

        // normalize the instruction for easier processing
        tdn_normalize_inst(&inst);
        uint32_t current_pc = pc;
        pc += inst.length;

        // TODO: verify the stack push

        // figure how much we need to pop from the stack
        int pop_count = m_il_pop_count[inst.opcode];
        if (pop_count < 0) {
            // TODO: calculate how many needed
            CHECK_FAIL();
        } else {
            arrsetlen(stack_items, pop_count);
        }

        // pop from the stack
        CHECK(arrlen(block.stack) >= pop_count);
        for (int i = pop_count - 1; i >= 0; i--) {
            stack_items[i] = arrpop(block.stack);
        }

        // ensure we have a verifier for this opcode
        CHECK(inst.opcode < ARRAY_LENGTH(m_verify_table) && m_verify_table[inst.opcode] != NULL,
            "Unknown opcode %s", tdn_get_opcode_name(inst.opcode));

        // call the verify function, it will ensure the stack is correct
        CHECK_AND_RETHROW(m_verify_table[inst.opcode](verifier, &block, &inst, stack_items));

        // TODO: call the emitter (if need be)

#ifdef JIT_VERBOSE_VERIFY
        indent = tdn_disasm_print_end(body, pc, indent);
#endif
    }

    // we have a fallthrough
    if (inst.control_flow == TDN_IL_CF_NEXT || inst.control_flow == TDN_IL_CF_CALL) {
        CHECK_FAIL();
    }

    // last must be a valid instruction
    CHECK(
        inst.control_flow != TDN_IL_CF_FIRST &&
        inst.control_flow != TDN_IL_CF_META
    );

cleanup:
    arrfree(stack_items);

    return err;
}

tdn_err_t jit_verify(jit_verifier_t* verifier) {
    tdn_err_t err = TDN_NO_ERROR;

    TRACE("========================================");
    TRACE("VERIFY: %T::%U", verifier->method->DeclaringType, verifier->method->Name);

    // start with the entry block
    verifier_queue_block(verifier, &verifier->blocks[0]);

    // and run until all blocks are verifier
    while (arrlen(verifier->queue) != 0) {
        jit_verifier_block_t* block = arrpop(verifier->queue);
        block->in_queue = false;

#ifdef JIT_VERBOSE_VERIFY
        TRACE("\tBasic block (IL_%04x):", block->block.start);
#endif

        CHECK_AND_RETHROW(jit_verify_basic_block(verifier, block));
    }

cleanup:
    arrfree(verifier->queue);

    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Prepare the verifier
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t jit_verifier_init(jit_verifier_t* verifier, RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure this method has a body
    CHECK(method->MethodBody != NULL);
    verifier->method = method;

    // find all of the basic blocks
    CHECK_AND_RETHROW(jit_find_basic_blocks(method, &verifier->labels));

    // setup the blocks array
    arrsetlen(verifier->blocks, hmlen(verifier->labels));
    memset(verifier->blocks, 0, arrlen(verifier->blocks) * sizeof(*verifier->blocks));

    // copy over the block information
    for (int i = 0; i < hmlen(verifier->labels); i++) {
        verifier->blocks[verifier->labels[i].value.index].block = verifier->labels[i].value;
    }

    jit_verifier_block_t* entry_block = &verifier->blocks[0];

    //
    // Initialize the arguments
    //

    // handle the this parameter if non-static
    if (!method->Attributes.Static) {
        jit_verifier_local_t local = {
            .stack = {
                .type = get_method_this_type(method),
            },
        };

        // TODO: unscoped support

        // if this is a readonly struct, then the ref is readonly as well
        if (method->DeclaringType->IsReadOnly) {
            local.stack.readonly_ref = true;
        }

        // this of a ref-struct is only non-local in its ref arguments
        if (method->DeclaringType->IsByRefStruct) {
            local.stack.readonly_ref = true;
        }

        arrpush(entry_block->args, local);
    }

    // now do the same for the parameters
    for (int i = 0; i < method->Parameters->Length; i++) {
        ParameterInfo parameter = method->Parameters->Elements[i];
        RuntimeTypeInfo type = parameter->ParameterType;
        jit_verifier_local_t local = {
            .stack = {
                .type = type,
            },
        };

        // if this is a readonly parameter then mark it as such
        if (parameter->IsReadOnly) {
            CHECK(type->IsByRef);
            local.stack.readonly_ref = true;
        }

        // TODO: scoped references

        // references are non-local since they come from the outside
        // same is true for the ref-structs
        if (type->IsByRef) {
            local.stack.non_local_ref = true;

            if (type->ElementType->IsByRefStruct) {
                local.stack.non_local_ref_struct = true;
            }
        } else if (type->IsByRefStruct) {
            local.stack.non_local_ref_struct = true;
        }

        arrpush(entry_block->args, local);
    }

    //
    // Initialize the locals
    //

    arrsetlen(entry_block->locals, method->MethodBody->LocalVariables->Length);
    memset(entry_block->locals, 0, arrlen(entry_block->locals) * sizeof(*entry_block->locals));
    for (int i = 0; i < arrlen(entry_block->locals); i++) {
        entry_block->locals[i].stack.type = method->MethodBody->LocalVariables->Elements[i]->LocalType;
    }

cleanup:
    return err;
}

void jit_verifier_destroy(jit_verifier_t* verifier) {
    if (verifier == NULL)
        return;

    for (int i = 0; i < arrlen(verifier->blocks); i++) {
        arrfree(verifier->blocks[i].args);
        arrfree(verifier->blocks[i].locals);
        arrfree(verifier->blocks[i].stack);
    }
    arrfree(verifier->blocks);

    hmfree(verifier->labels);
    arrfree(verifier->queue);
}
