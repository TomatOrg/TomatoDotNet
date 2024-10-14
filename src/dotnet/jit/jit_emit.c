#include "jit_emit.h"

#include <util/except.h>
#include <util/stb_ds.h>

#include <spidir/module.h>
#include <spidir/codegen.h>
#include <spidir/x64.h>

#include <tomatodotnet/disasm.h>
#include <tomatodotnet/types/type.h>
#include <util/string.h>
#include <util/string_builder.h>

#include "jit_builtin.h"
#include "jit_helpers.h"

/**
 * The module used for jitting
 */
static spidir_module_handle_t m_jit_module = NULL;

static spidir_function_t m_jit_bzero;
static spidir_function_t m_jit_memcpy;
static spidir_function_t m_jit_gc_new;
static spidir_function_t m_jit_throw;

static struct {
    spidir_function_t key;
    void* value;
}* m_jit_helper_lookup = NULL;

static void create_jit_helpers() {
    m_jit_bzero = spidir_module_create_extern_function(m_jit_module,
        "jit_bzero",
        SPIDIR_TYPE_NONE,
        2,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_PTR,
            SPIDIR_TYPE_I64
        }
    );
    hmput(m_jit_helper_lookup, m_jit_bzero, jit_bzero);

    m_jit_memcpy = spidir_module_create_extern_function(m_jit_module,
        "jit_memcpy",
        SPIDIR_TYPE_NONE,
        3,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_PTR,
            SPIDIR_TYPE_PTR,
            SPIDIR_TYPE_I64
        }
    );
    hmput(m_jit_helper_lookup, m_jit_memcpy, jit_memcpy);

    m_jit_gc_new = spidir_module_create_extern_function(m_jit_module,
        "jit_gc_new",
        SPIDIR_TYPE_PTR,
        1,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_PTR
        }
    );
    hmput(m_jit_helper_lookup, m_jit_gc_new, jit_gc_new);

    m_jit_throw = spidir_module_create_extern_function(m_jit_module,
        "jit_throw",
        SPIDIR_TYPE_NONE,
        2,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_PTR,
            SPIDIR_TYPE_I32,
        }
    );
    hmput(m_jit_helper_lookup, m_jit_throw, jit_throw);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The code emitting itself
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct jit_emit_ctx {
    // the method we are working on
    jit_method_t* method;

    // where the locals are stored
    spidir_value_t* locals;

    // where the args are stored (in case
    // they were spilled)
    spidir_value_t* args;

    tdn_err_t err;
} jit_emit_ctx_t;

static spidir_value_type_t get_spidir_type(RuntimeTypeInfo info) {
    info = tdn_get_intermediate_type(info);
    if (info == tInt32) {
        return SPIDIR_TYPE_I32;
    } else if (info == tInt64 || info == tIntPtr) {
        return SPIDIR_TYPE_I64;
    } else {
        return SPIDIR_TYPE_PTR;
    }
}

static spidir_mem_size_t get_spidir_mem_size(RuntimeTypeInfo info) {
    switch (info->StackSize) {
        case 1: return SPIDIR_MEM_SIZE_1;
        case 2: return SPIDIR_MEM_SIZE_2;
        case 4: return SPIDIR_MEM_SIZE_4;
        case 8: return SPIDIR_MEM_SIZE_8;
        default: ASSERT(!"Invalid size");
    }
}

static spidir_value_type_t get_spidir_ret_type(RuntimeMethodBase method) {
    RuntimeTypeInfo type = tdn_get_intermediate_type(method->ReturnParameter->ParameterType);
    if (type == tInt32) {
        return SPIDIR_TYPE_I32;
    } else if (type == tInt64 || type == tIntPtr) {
        return SPIDIR_TYPE_I64;
    } else if (jit_is_struct_like(type)) {
        // things which act like a struct return by using reference
        return SPIDIR_TYPE_NONE;
    } else {
        ASSERT(tdn_type_is_referencetype(type) || type->IsByRef);
        return SPIDIR_TYPE_PTR;
    }
}

static spidir_value_type_t* get_spidir_arg_types(RuntimeMethodBase method) {
    spidir_value_type_t* types = NULL;

    // this pointer
    if (!method->Attributes.Static) {
        arrpush(types, SPIDIR_TYPE_PTR);
    }

    // and now all of the arguments
    for (int i = 0; i < method->Parameters->Length; i++) {
        spidir_value_type_t type = get_spidir_type(method->Parameters->Elements[i]->ParameterType);
        arrpush(types, type);
    }

    // implicit retval pointer
    RuntimeTypeInfo ret_type = method->ReturnParameter->ParameterType;
    if (ret_type != tVoid && !ret_type->IsByRef && jit_is_struct_like(ret_type)) {
        arrpush(types, SPIDIR_TYPE_PTR);
    }

    return types;
}

static void jit_queue_block(jit_emit_ctx_t* ctx, spidir_builder_handle_t builder, jit_basic_block_t* block) {
    if (block->state <= JIT_BLOCK_VERIFIED) {
        block->state = JIT_BLOCK_PENDING_EMIT;

        // create the block
        block->block = spidir_builder_create_block(builder);

        // create the phis for this block
        if (block->needs_phi) {
            for (int i = 0; i < arrlen(block->stack); i++) {
                block->stack[i].value = spidir_builder_build_phi(
                    builder,
                    get_spidir_type(block->stack[i].type),
                    0, NULL,
                    &block->stack[i].phi
                );
            }
        }

        // queue it
        long bi = block - ctx->method->basic_blocks;
        arrpush(ctx->method->block_queue, bi);
    }
}

static tdn_err_t emit_merge_basic_block(jit_emit_ctx_t* ctx, spidir_builder_handle_t builder, uint32_t target_pc, jit_stack_value_t* stack, spidir_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;

    int bi = hmgeti(ctx->method->labels, target_pc);
    CHECK(bi != 0);
    jit_basic_block_t* target = &ctx->method->basic_blocks[ctx->method->labels[bi].value];

    // queue the block, will also handle creating the phi if required
    jit_queue_block(ctx, builder, target);

    // add all the phi inputs
    // TODO: in theory we only need to do this on the entries that are at the top
    //       of the stack to the lowest this block goes
    if (target->needs_phi) {
        for (int i = 0; i < arrlen(stack); i++) {
            spidir_builder_add_phi_input(builder,
                target->stack[i].phi,
                stack[i].value);
        }
    } else {
        // copy all of the stack over
        memcpy(target->stack, stack, arrlen(stack) * sizeof(*stack));
    }

    // output the block if this entry
    *block = target->block;

cleanup:
    return err;
}

static RuntimeTypeInfo emit_get_stack_type(RuntimeTypeInfo type) {
    type = tdn_get_intermediate_type(type);
    if (type == tIntPtr) {
        type = tInt64;
    }
    return type;
}

#define EVAL_STACK_PUSH(_type, _value, ...) \
    do { \
        CHECK(arrlen(stack) < body->MaxStackSize); \
        jit_stack_value_t __item = { .type = emit_get_stack_type(_type), .value = _value, ## __VA_ARGS__ }; \
        arrpush(stack, __item); \
    } while (0)

#define EVAL_STACK_POP() \
    ({ \
        CHECK(arrlen(stack) > 0); \
        arrpop(stack); \
    })

#define SWAP(a, b) \
    do { \
        typeof(a) __tmp = a; \
        a = b; \
        b = __tmp; \
    } while(0)

#define GET_ARG_TYPE(_index) \
    ({ \
        typeof(_index) __index = _index; \
        RuntimeTypeInfo __arg_type; \
        if (this_type != NULL) { \
            if (__index == 0) { \
                __arg_type = this_type; \
            } else { \
                __index--; \
                CHECK(__index < method->Parameters->Length); \
                __arg_type = method->Parameters->Elements[__index]->ParameterType; \
            } \
        } else { \
            CHECK(__index < method->Parameters->Length); \
            __arg_type = method->Parameters->Elements[__index]->ParameterType; \
        } \
        __arg_type; \
    })

static spidir_value_t jit_push_struct(spidir_builder_handle_t builder, RuntimeTypeInfo type) {
    // TODO: how should we handle this the best
    return spidir_builder_build_stackslot(builder, type->StackSize, type->StackAlignment);
}

static void jit_pop_struct(spidir_value_t value) {
}

static void jit_emit_memcpy(spidir_builder_handle_t builder, spidir_value_t dst, spidir_value_t src, RuntimeTypeInfo type) {
    // TODO: replace with a jit builtin-memcpy
    // TODO: use gc_memcpy in case contains a refernce
    spidir_builder_build_call(builder,
        m_jit_memcpy,
        3,
        (spidir_value_t[]){
            dst,
            src,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, type->StackSize)
        }
    );
}

static void jit_emit_bzero(spidir_builder_handle_t builder, spidir_value_t dst, RuntimeTypeInfo type) {
    // TODO: replace with a jit builtin-bzero/memset
    // TODO: use gc_bzero in case contains a refernce
    spidir_builder_build_call(builder,
        m_jit_bzero,
        2,
        (spidir_value_t[]){
            dst,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, type->StackSize)
        }
    );
}

static void jit_emit_store(spidir_builder_handle_t builder, spidir_value_t dest, spidir_value_t value, RuntimeTypeInfo type) {
    // store something that is a struct
    if (jit_is_struct_like(type)) {
        jit_emit_memcpy(builder, dest, value, type);

    } else {
        // store something that is not a struct
        spidir_builder_build_store(
            builder,
            get_spidir_mem_size(type),
            value,
            dest);
    }
}

static spidir_value_t jit_emit_load(spidir_builder_handle_t builder, spidir_value_t src, RuntimeTypeInfo type) {
    // store something that is a struct
    if (jit_is_struct_like(type)) {
        spidir_value_t new_struct = jit_push_struct(builder, type);
        jit_emit_memcpy(builder, new_struct, src, type);
        return new_struct;

    } else {
        // store something that is not a struct
        return spidir_builder_build_load(
            builder,
            get_spidir_mem_size(type),
            get_spidir_type(type),
            src);
    }

}

static tdn_err_t jit_emit_basic_block(spidir_builder_handle_t builder, jit_emit_ctx_t* ctx, jit_basic_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = ctx->method->method;
    RuntimeMethodBody body = method->MethodBody;
    jit_stack_value_t* stack = NULL;
    RuntimeTypeInfo this_type = NULL;

    spidir_value_t* args = NULL;
    spidir_value_type_t* args_type = NULL;

    // figure the this type if this is a non-static method
    if (!method->Attributes.Static) {
        this_type = method->DeclaringType;
        if (tdn_type_is_valuetype(this_type)) {
            CHECK_AND_RETHROW(tdn_get_byref_type(this_type, &this_type));
        }
    }

    // copy the initial stack
    arrsetlen(stack, arrlen(block->stack));
    memcpy(stack, block->stack, arrlen(block->stack) * sizeof(*stack));

    // move to the block we are emitting
    spidir_builder_set_block(builder, block->block);

#ifdef JIT_VERBOSE_EMIT
    int indent = 0;
#endif

    block->state = JIT_BLOCK_FINISHED;

    // get the pc
    tdn_il_inst_t inst = { .control_flow = TDN_IL_CF_FIRST };
    uint32_t pc = block->start;
    while (pc < block->end) {
        // get the instruction
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));

#ifdef JIT_VERBOSE_EMIT
        indent = tdn_disasm_print_start(body, pc, inst, indent);
#endif

        tdn_normalize_inst(&inst);
        uint32_t current_pc = pc;
        pc += inst.length;

        switch (inst.opcode) {
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arguments
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STARG: {
                int index = inst.operand.variable;
                RuntimeTypeInfo arg_type = GET_ARG_TYPE(index);
                jit_stack_value_t value = EVAL_STACK_POP();

                // make sure the argument is spilled, this is easier than tracking
                // the value type across segments (and then we might as well do it
                // for locals as well)
                CHECK(ctx->method->args[index].spill_required);
                spidir_value_t dest = ctx->args[index];
                jit_emit_store(builder, dest, value.value, arg_type);
                jit_pop_struct(dest);
            }  break;

            case CEE_LDARG: {
                int index = inst.operand.variable;
                RuntimeTypeInfo arg_type = GET_ARG_TYPE(index);
                arg_type = tdn_get_intermediate_type(arg_type);

                spidir_value_t value = ctx->args[index];

                if (jit_is_struct_like(arg_type)) {
                    // struct, need to copy it
                    spidir_value_t new_struct = jit_push_struct(builder, arg_type);
                    jit_emit_memcpy(builder, new_struct, value, arg_type);
                    value = new_struct;

                } else if (ctx->method->args[index].spill_required) {
                    // spilled, need to load it
                    value = spidir_builder_build_load(builder,
                        get_spidir_mem_size(arg_type),
                        get_spidir_type(arg_type),
                        value);

                }

                EVAL_STACK_PUSH(arg_type, value);
            }  break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Locals
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STLOC: {
                int index = inst.operand.variable;
                RuntimeLocalVariableInfo local = body->LocalVariables->Elements[index];

                // verify the type
                jit_stack_value_t value = EVAL_STACK_POP();
                jit_emit_store(builder, ctx->locals[index], value.value, local->LocalType);
                jit_pop_struct(value.value);
            } break;

            case CEE_LDLOC: {
                int index = inst.operand.variable;
                RuntimeLocalVariableInfo local = body->LocalVariables->Elements[index];
                spidir_value_t value = jit_emit_load(builder, ctx->locals[index], local->LocalType);
                RuntimeTypeInfo type = tdn_get_intermediate_type(local->LocalType);
                EVAL_STACK_PUSH(type, value);
            } break;

            case CEE_LDLOCA: {
                int index = inst.operand.variable;
                RuntimeLocalVariableInfo local = body->LocalVariables->Elements[index];

                RuntimeTypeInfo type = tdn_get_verification_type(local->LocalType);
                CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));

                EVAL_STACK_PUSH(type, ctx->locals[index]);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Stack manipulation
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDC_I4: {
                spidir_value_t value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, inst.operand.uint32);
                EVAL_STACK_PUSH(tInt32, value);
            } break;

            case CEE_LDC_I8: {
                spidir_value_t value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst.operand.int64);
                EVAL_STACK_PUSH(tInt64, value);
            } break;

            case CEE_LDNULL: {
                spidir_value_t value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0);
                EVAL_STACK_PUSH(tObject, value);
            } break;

            case CEE_LDSTR: {
                spidir_value_t value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)inst.operand.string);
                EVAL_STACK_PUSH(tString, value);
            } break;

            case CEE_POP: {
                EVAL_STACK_POP();
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Math
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_CEQ:
            case CEE_CGT:
            case CEE_CGT_UN:
            case CEE_CLT:
            case CEE_CLT_UN: {
                jit_stack_value_t value2 = EVAL_STACK_POP();
                jit_stack_value_t value1 = EVAL_STACK_POP();
                bool is_unsigned = inst.opcode == CEE_CGT_UN || inst.opcode == CEE_CLT_UN;

                spidir_value_t val1 = value1.value;
                spidir_value_t val2 = value2.value;

                // check if we need to extend either
                if (value1.type == tIntPtr) {
                    if (value2.type == tInt32) {
                        val2 = spidir_builder_build_iext(builder, val2);
                        if (is_unsigned) {
                            val2 = spidir_builder_build_and(builder, val2,
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, UINT32_MAX));
                        } else {
                            val2 = spidir_builder_build_sfill(builder, 32, val2);
                        }
                    }

                } else if (value1.type == tInt32) {
                    if (value2.type == tIntPtr) {
                        val1 = spidir_builder_build_iext(builder, val1);
                        if (is_unsigned) {
                            val1 = spidir_builder_build_and(builder, val1,
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, UINT32_MAX));
                        } else {
                            val1 = spidir_builder_build_sfill(builder, 32, val1);
                        }
                    }
                }

                // choose the kind, spidir doesn't have greater than variants
                // so we are going to swap whenever there is a need for one
                spidir_icmp_kind_t kind;
                switch (inst.opcode) {
                    case CEE_CEQ: kind = SPIDIR_ICMP_EQ; break;

                    case CEE_CGT: SWAP(val1, val2);
                    case CEE_CLT: kind = SPIDIR_ICMP_SLT; break;

                    case CEE_CLT_UN: SWAP(val1, val2);
                    case CEE_CGT_UN: kind = SPIDIR_ICMP_ULT; break;

                    default: CHECK_FAIL();
                }

                spidir_value_t value = spidir_builder_build_icmp(builder, kind, SPIDIR_TYPE_I32, val1, val2);
                EVAL_STACK_PUSH(tInt32, value);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Branching
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_BEQ:
            case CEE_BGE:
            case CEE_BGT:
            case CEE_BLE:
            case CEE_BLT:
            case CEE_BNE_UN:
            case CEE_BGE_UN:
            case CEE_BGT_UN:
            case CEE_BLE_UN:
            case CEE_BLT_UN: {
                jit_stack_value_t value2 = EVAL_STACK_POP();
                jit_stack_value_t value1 = EVAL_STACK_POP();
                bool is_unsigned = inst.opcode == CEE_BNE_UN || inst.opcode == CEE_BGE_UN ||
                                    inst.opcode == CEE_BGT_UN || inst.opcode == CEE_BLE_UN ||
                                    inst.opcode == CEE_BLT_UN;

                // start by emitting the conditional

                spidir_value_t val1 = value1.value;
                spidir_value_t val2 = value2.value;

                // check if we need to extend either
                if (value1.type == tIntPtr) {
                    if (value2.type == tInt32) {
                        val2 = spidir_builder_build_iext(builder, val2);
                        if (is_unsigned) {
                            val2 = spidir_builder_build_and(builder, val2,
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, UINT32_MAX));
                        } else {
                            val2 = spidir_builder_build_sfill(builder, 32, val2);
                        }
                    }

                } else if (value1.type == tInt32) {
                    if (value2.type == tIntPtr) {
                        val1 = spidir_builder_build_iext(builder, val1);
                        if (is_unsigned) {
                            val1 = spidir_builder_build_and(builder, val1,
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, UINT32_MAX));
                        } else {
                            val1 = spidir_builder_build_sfill(builder, 32, val1);
                        }
                    }
                }

                // choose the kind, spidir doesn't have greater than variants
                // so we are going to swap whenever there is a need for one
                spidir_icmp_kind_t kind;
                switch (inst.opcode) {
                    case CEE_BEQ: kind = SPIDIR_ICMP_EQ; break;
                    case CEE_BNE_UN: kind = SPIDIR_ICMP_NE; break;

                    case CEE_BGE: SWAP(val1, val2);
                    case CEE_BLE: kind = SPIDIR_ICMP_SLE; break;

                    case CEE_BGT: SWAP(val1, val2);
                    case CEE_BLT: kind = SPIDIR_ICMP_SLT; break;

                    case CEE_BGE_UN: SWAP(val1, val2);
                    case CEE_BLE_UN: kind = SPIDIR_ICMP_ULE; break;

                    case CEE_BGT_UN: SWAP(val1, val2);
                    case CEE_BLT_UN: kind = SPIDIR_ICMP_ULT; break;

                    default: CHECK_FAIL();
                }

                spidir_value_t value = spidir_builder_build_icmp(builder, kind, SPIDIR_TYPE_I32, val1, val2);

                // get the blocks of each option
                spidir_block_t true_block;
                CHECK_AND_RETHROW(emit_merge_basic_block(
                    ctx, builder,
                    inst.operand.branch_target,
                    stack, &true_block));

                spidir_block_t false_block;
                CHECK_AND_RETHROW(emit_merge_basic_block(
                    ctx, builder,
                    pc,
                    stack, &false_block));

                // and finally emit the actual brcond
                spidir_builder_build_brcond(builder, value, true_block, false_block);
            } break;

            case CEE_BRTRUE:
            case CEE_BRFALSE: {
                jit_stack_value_t value = EVAL_STACK_POP();

                // get the blocks of each option
                spidir_block_t true_block;
                CHECK_AND_RETHROW(emit_merge_basic_block(
                    ctx, builder,
                    inst.operand.branch_target,
                    stack, &true_block));

                spidir_block_t false_block;
                CHECK_AND_RETHROW(emit_merge_basic_block(
                    ctx, builder,
                    pc,
                    stack, &false_block));

                // and finally emit the actual brcond
                spidir_builder_build_brcond(builder, value.value, true_block, false_block);
            } break;

            case CEE_BR: {
                spidir_block_t dest;
                CHECK_AND_RETHROW(emit_merge_basic_block(
                    ctx, builder,
                    inst.operand.branch_target,
                    stack, &dest));

                spidir_builder_build_branch(builder, dest);
            } break;

            case CEE_RET: {
                RuntimeTypeInfo type = method->ReturnParameter->ParameterType;

                if (type != tVoid) {
                    jit_stack_value_t ret_val = EVAL_STACK_POP();

                    if (jit_is_struct_like(ret_val.type)) {
                        // get the return pointer from the top of the stack implicitly
                        spidir_value_t ret_ptr = spidir_builder_build_param_ref(builder, arrlen(ctx->locals));
                        jit_emit_memcpy(builder, ret_ptr, ret_val.value, ret_val.type);
                        jit_pop_struct(ret_val.value);

                        spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
                    } else {
                        // just return it
                        spidir_builder_build_return(builder, ret_val.value);
                    }
                } else {
                    // nothing to return
                    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
                }

                CHECK(arrlen(stack) == 0);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Misc
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_NOP: break;

            default: CHECK_FAIL("Unknown opcode");
        }

        arrfree(args_type);
        arrfree(args);

#ifdef JIT_VERBOSE_EMIT
        indent = tdn_disasm_print_end(body, pc, indent);
#endif
    }

    // we have a fallthrough, handle it
    if (inst.control_flow == TDN_IL_CF_NEXT) {
        spidir_block_t new_block;
        CHECK_AND_RETHROW(emit_merge_basic_block(ctx, builder, pc, stack, &new_block));
        spidir_builder_build_branch(builder, new_block);
    }

cleanup:
    arrfree(stack);
    arrfree(args_type);
    arrfree(args);

    return err;
}

static bool jit_prepare_args(spidir_builder_handle_t builder, jit_method_t* method) {
    bool modified_block = false;

    for (int i = 0; i < arrlen(method->args); i++) {
        jit_arg_t* arg = &method->args[i];

        if (arg->spill_required) {
            // create the stack slot
            arg->value = spidir_builder_build_stackslot(builder,
                arg->type->StackSize, arg->type->StackAlignment);

            // and store the value to it
            spidir_value_t value = spidir_builder_build_param_ref(builder, i);
            spidir_mem_size_t size = get_spidir_mem_size(arg->type);
            spidir_builder_build_store(builder, size, arg->value, value);

            modified_block = true;
        } else {
            // just remember the param ref
            arg->value = spidir_builder_build_param_ref(builder, i);
        }
    }

    return modified_block;
}

static bool jit_prepare_locals(spidir_builder_handle_t builder, jit_method_t* method) {
    bool modified_block = false;

    for (int i = 0; i < arrlen(method->locals); i++) {
        jit_local_t* arg = &method->locals[i];

        arg->value = spidir_builder_build_stackslot(builder, arg->type->StackSize, arg->type->StackAlignment);
        if (jit_is_struct_like(arg->type)) {
            jit_emit_bzero(builder, arg->value, arg->type);
        } else {
            spidir_builder_build_store(builder,
                get_spidir_mem_size(arg->type),
                spidir_builder_build_iconst(builder, get_spidir_type(arg->type), 0),
                arg->value
            );
        }
    }

    return modified_block;
}

static void jit_emit_spidir_from_il(spidir_builder_handle_t builder, void* _ctx) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_emit_ctx_t* ctx = _ctx;
    jit_method_t* jmethod = ctx->method;

    // start from the first block
    jit_queue_block(ctx, builder, &jmethod->basic_blocks[0]);

    // set the entry block
    spidir_builder_set_entry_block(builder, jmethod->basic_blocks[0].block);
    spidir_builder_set_block(builder, jmethod->basic_blocks[0].block);

    bool modified_block = false;

    // prepare the args
    if (jit_prepare_args(builder, jmethod)) {
        modified_block = true;
    }

    // prepare te locals
    if (jit_prepare_locals(builder, jmethod)) {
        modified_block = true;
    }

    // did we modify the block? if so we need to allocate a new block
    // to be used for the main block
    if (modified_block) {
        // create a new block
        spidir_block_t new_block = spidir_builder_create_block(builder);
        spidir_builder_build_branch(builder, new_block);
        jmethod->basic_blocks[0].block = new_block;
    }

    // and dispatch them all
    while (arrlen(jmethod->block_queue)) {
        int bi = arrpop(jmethod->block_queue);
        CHECK_AND_RETHROW(jit_emit_basic_block(builder, ctx, &jmethod->basic_blocks[bi]));
    }

cleanup:
    ctx->err = err;
}

static tdn_err_t jit_emit_method(jit_method_t* method) {
    tdn_err_t err = TDN_NO_ERROR;

#ifdef JIT_DEBUG_EMIT
    TRACE("%T::%U", method->method->DeclaringType, method->method->Name);
#endif

    // TODO: implement runtime methods
    if (method->method->MethodBody == NULL) {
        jit_builtin_context_t ctx = {
            .method = method->method,
            .err = TDN_NO_ERROR
        };
        spidir_module_build_function(
            m_jit_module,
            method->function,
            jit_emit_builtin,
            &ctx
        );
        CHECK_AND_RETHROW(ctx.err);
    } else {
        jit_emit_ctx_t ctx = {
            .method = method,
            .err = TDN_NO_ERROR
        };
        spidir_module_build_function(
            m_jit_module,
            method->function,
            jit_emit_spidir_from_il,
            &ctx
        );
        CHECK_AND_RETHROW(ctx.err);
    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Top level API management
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * The methods left to be verified
 */
static jit_method_t** m_methods_to_emit = NULL;

/**
 * The types which need their vtables fixed
 */
static RuntimeTypeInfo* m_types_to_emit = NULL;

typedef struct jit_method_result {
    // offset within the map
    size_t offset;

    // the codegen blob
    spidir_codegen_blob_handle_t blob;
} jit_method_result_t;

/**
 * Blobs of the emitted methods
 */
static jit_method_result_t* m_method_jit_results = NULL;

/**
 * The mahcine used for jtiting
 */
static spidir_codegen_machine_handle_t m_spidir_machine = NULL;

tdn_err_t jit_init_emit() {
    tdn_err_t err = TDN_NO_ERROR;

    // create the backend used for jitting
    m_spidir_machine = spidir_codegen_create_x64_machine();

cleanup:
    return err;
}

static spidir_function_t create_spidir_function(RuntimeMethodBase method, bool external) {
    // build the name
    string_builder_t builder = {};
    string_builder_push_method_signature(&builder, method, true);
    const char* name = string_builder_build(&builder);

    // get the signature
    spidir_value_type_t ret_type = get_spidir_ret_type(method);
    spidir_value_type_t* arg_types = get_spidir_arg_types(method);

    spidir_function_t function;
    if (external) {
        function = spidir_module_create_extern_function(m_jit_module, name, ret_type, arrlen(arg_types), arg_types);
    } else {
        function = spidir_module_create_function(m_jit_module, name, ret_type, arrlen(arg_types), arg_types);
    }

    arrfree(arg_types);
    string_builder_free(&builder);

    return function;
}

void jit_queue_emit(jit_method_t* method) {
    // if required create a new spidir module
    if (m_jit_module == NULL) {
        TRACE("~~~ JIT START ~~~");

        // create the module
        m_jit_module = spidir_module_create();

        // create all the helpers we might need
        create_jit_helpers();
    }

    // emit it
    arrpush(m_methods_to_emit, method);

    // create the function itself
    method->function = create_spidir_function(method->method, false);
}

void jit_queue_emit_extern(jit_method_t* method) {
    // if required create a new spidir module
    if (m_jit_module == NULL) {
        m_jit_module = spidir_module_create();

    }

    // create the function itself
    method->function = create_spidir_function(method->method, true);
}

void jit_queue_emit_type(RuntimeTypeInfo type) {
    arrpush(m_types_to_emit, type);
}

static tdn_err_t jit_map_and_relocate(size_t map_size) {
    tdn_err_t err = TDN_NO_ERROR;

    // map it as read-write
    void* map = tdn_host_map(map_size);
    CHECK_ERROR(map != NULL, TDN_ERROR_OUT_OF_MEMORY);

    // copy over all of the code and set the method pointers
    for (int i = 0; i < arrlen(m_method_jit_results); i++) {
        jit_method_result_t* blob = &m_method_jit_results[i];
        jit_method_t* method = m_methods_to_emit[i];

        // skip if there is no blob
        if (blob->blob == NULL) {
            continue;
        }

        // TODO: generate method thunks

        // align methods to 16 bytes
        method->method->MethodPtr = map + blob->offset;
        method->method->MethodSize = spidir_codegen_blob_get_code_size(blob->blob);
        memcpy(
            method->method->MethodPtr,
            spidir_codegen_blob_get_code(blob->blob),
            method->method->MethodSize
        );
    }

    // now we can apply the relocations, this can technically be done in parallel but I think
    // its cheap enough that its not worth it
    for (int i = 0; i < arrlen(m_method_jit_results); i++) {
        jit_method_result_t* blob = &m_method_jit_results[i];
        jit_method_t* method = m_methods_to_emit[i];

        // skip if there is no blob
        if (blob->blob == NULL) {
            continue;
        }

        size_t reloc_count = spidir_codegen_blob_get_reloc_count(blob->blob);
        const spidir_codegen_reloc_t* relocs = spidir_codegen_blob_get_relocs(blob->blob);
        for (size_t j = 0; j < reloc_count; j++) {
            uint64_t P = (uint64_t)(method->method->MethodPtr + relocs[j].offset);
            int64_t A = relocs[j].addend;

            // resolve the target, will either be a builtin or a
            // method pointer we jitted
            uint64_t F;
            jit_method_t* target = jit_get_method_from_function(relocs[j].target);
            if (target == NULL) {
                void* ptr = hmget(m_jit_helper_lookup, relocs[j].target);
                CHECK(ptr != NULL);
                F = (uint64_t)ptr;
            } else {
                F = (uint64_t)(target->method->MethodPtr);
            }

            switch (relocs[j].kind) {
                case SPIDIR_RELOC_X64_PC32: {
                    uint64_t value = F + A - P;
                    CHECK(value <= UINT32_MAX);
                    uint32_t pc32 = value;
                    memcpy((void*)P, &pc32, sizeof(pc32));
                } break;

                case SPIDIR_RELOC_X64_ABS64: {
                    uint64_t value = F + A;
                    memcpy((void*)P, &value, sizeof(value));
                } break;

                default:
                    CHECK_FAIL("Unknown relocation kind: %d", relocs[j].kind);
            }
        }
    }

    // now remap it as read-execute
    tdn_host_map_rx(map, map_size);

cleanup:
    return err;
}

tdn_err_t jit_emit(void) {
    tdn_err_t err = TDN_NO_ERROR;

    // if there were no methods that need to be jitted don't do anything
    if (arrlen(m_methods_to_emit) == 0) {
        goto cleanup;
    }

    // emit all the methods, do it inline since its simpler
    for (int i = 0; i < arrlen(m_methods_to_emit); i++) {
        jit_method_t* method = m_methods_to_emit[i];
        CHECK_AND_RETHROW(jit_emit_method(method));
    }

    //
    // TODO: this is where we need to enable optimizations and such
    //

    // perform the codegen
    // TODO: do this in parallel in the future
    arrsetlen(m_method_jit_results, arrlen(m_methods_to_emit));
    memset(m_method_jit_results, 0, arrlen(m_method_jit_results) * sizeof(*m_method_jit_results));

    for (int i = 0; i < arrlen(m_methods_to_emit); i++) {
        jit_method_t* method = m_methods_to_emit[i];

        // if already was jitted before then don't jit it again
        if (method->method->MethodPtr != NULL) {
            continue;
        }

        spidir_codegen_config_t config = {
            .verify_ir = true,
            .verify_regalloc = true
        };
        spidir_codegen_status_t status = spidir_codegen_emit_function(
            m_spidir_machine, &config,
            m_jit_module,
            method->function,
            &m_method_jit_results[i].blob
        );
        CHECK(status == SPIDIR_CODEGEN_OK, "Failed to jit: %d", status);
    }

    // now that all the codegen is finished we can sum up the size
    // required and map it
    size_t map_size = 0;
    for (int i = 0; i < arrlen(m_method_jit_results); i++) {
        jit_method_result_t* blob = &m_method_jit_results[i];

        // skip if there is no blob
        if (blob->blob == NULL) {
            continue;
        }

        // TODO: generate method thunks

        // align methods to 16 bytes
        map_size = ALIGN_UP(map_size, 16);
        blob->offset = map_size;
        map_size += spidir_codegen_blob_get_code_size(blob->blob);
    }

    if (map_size > 0) {
        CHECK_AND_RETHROW(jit_map_and_relocate(map_size));
    }

    // now fill up all the vtables
    for (int i = 0; i < arrlen(m_types_to_emit); i++) {
        RuntimeTypeInfo type = m_types_to_emit[i];

        for (int j = 0; j < type->VTable->Length; j++) {
            RuntimeMethodInfo method = type->VTable->Elements[j];

            // if we have a thunk then use it instead
            void* ptr = method->MethodPtr;
            if (method->ThunkPtr != NULL) {
                ptr = method->ThunkPtr;
            }
            CHECK(ptr != NULL);

            // save it in the jit vtable
            type->JitVTable->Functions[i] = ptr;
        }
    }

cleanup:
    // destroy all the blobs and free the results
    for (int i = 0; i < arrlen(m_method_jit_results); i++) {
        if (m_method_jit_results[i].blob != NULL) {
            spidir_codegen_blob_destroy(m_method_jit_results[i].blob);
        }
    }

    arrfree(m_method_jit_results);
    arrfree(m_methods_to_emit);
    arrfree(m_types_to_emit);
    hmfree(m_jit_helper_lookup);

    // cleanup the module
    if (m_jit_module != NULL) {
        TRACE("~~~ JIT END ~~~");
        spidir_module_destroy(m_jit_module);
        m_jit_module = NULL;
    }

    return err;
}
