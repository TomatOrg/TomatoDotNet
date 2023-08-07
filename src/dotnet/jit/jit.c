#include "tinydotnet/types/type.h"
#include "tinydotnet/jit/jit.h"
#include "util/except.h"
#include "util/stb_ds.h"
#include "tinydotnet/disasm.h"
#include "util/string_builder.h"
#include "jit_internal.h"

// TODO: lock

#include <spidir/spidir.h>
#include <stdbool.h>

static spidir_module_handle_t m_spidir_module;

tdn_err_t tdn_jit_init() {
    tdn_err_t err = TDN_NO_ERROR;

    m_spidir_module = spidir_module_create();

cleanup:
    return err;
}

static spidir_dump_status_t stdout_dump_callback(const char* s, size_t size, void* ctx) {
    (void) ctx;
    tdn_host_printf("%.*s", size, s);
    return SPIDIR_DUMP_CONTINUE;
}

void tdn_jit_dump() {
    spidir_module_dump(m_spidir_module, stdout_dump_callback, NULL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// SPIDIR helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static spidir_value_type_t get_spidir_value_type(RuntimeTypeInfo type) {
    if (
        type == tSByte || type == tByte ||
        type == tInt16 || type == tUInt16 ||
        type == tInt32 || type == tUInt32 ||
        type == tBoolean
    ) {
        return SPIDIR_TYPE_I32;
//    } else if (type == tSingle) {
//
//    } else if (type == tDouble) {

    } else if (type->BaseType == tEnum) {
        return get_spidir_value_type(type->EnumUnderlyingType);
    } else if (
        type == tInt64 || type == tUInt64 ||
        type == tIntPtr || type == tUIntPtr
    ) {
        return SPIDIR_TYPE_I64;
    } else if (type == tVoid) {
        return SPIDIR_TYPE_NONE;
    } else if (tdn_type_is_valuetype(type)) {
        // TODO: handle value types
        __builtin_trap();
    } else {
        // this is def a pointer
        return SPIDIR_TYPE_PTR;
    }
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The jitter itself
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define SWAP(a, b) \
    do { \
        typeof(a) __temp = a; \
        a = b; \
        b = __temp; \
    } while (0)

static void jit_method_callback(spidir_builder_handle_t builder, void* _ctx) {
    jit_context_t* ctx = _ctx;
    tdn_err_t err = TDN_NO_ERROR;
    jit_label_t* labels = NULL;
    RuntimeMethodBase method = ctx->method;
    eval_stack_t stack = {
        .max_depth = method->MethodBody->MaxStackSize
    };

    TRACE("%U::%U", ctx->method->DeclaringType->Name, ctx->method->Name);

    // get the this_type for future use
    RuntimeTypeInfo this_type = NULL;
    if (!method->Attributes.Static) {
        this_type = method->DeclaringType;
        if (tdn_type_is_valuetype(this_type)) {
            // this is a valuetype, the this is a reference
            CHECK_AND_RETHROW(tdn_get_byref_type(this_type, &this_type));
        }
    }

    //
    // first pass, find all of the labels, this will
    // also create all the different basic blocks on
    // the way
    //

    // entry block
    jit_label_t* entry_label = jit_add_label(&labels, 0);
    CHECK(entry_label != NULL);
    entry_label->block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, entry_label->block);
    spidir_builder_set_block(builder, entry_label->block);

    // get the rest of the blocks by creating the labels
    uint32_t pc = 0;
    while (pc != ctx->method->MethodBody->ILSize) {
        // decode instruction
        tdn_il_inst_t inst;
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));

        pc += inst.length;

        // check it, we already verify the target is inside of the current function
        // TODO: switch
        if (inst.operand_type == TDN_IL_BRANCH_TARGET) {
            // add label for the destination
            jit_label_t* label = jit_add_label(&labels, inst.operand.branch_target);
            if (label != NULL) {
                label->block = spidir_builder_create_block(builder);
            }

            // if this is a conditional branch then we need
            // to set the label for the next opcode as well
            if (inst.control_flow == TDN_IL_COND_BRANCH) {
                label = jit_add_label(&labels, pc);
                if (label != NULL) {
                    label->block = spidir_builder_create_block(builder);
                }
            }
        }
    }

    // TODO: support protected blocks properly

    //
    // the main jit function
    //

    pc = 0;
    int label_idx = 1; // skip the entry label,
                       // since its a special case
    tdn_il_control_flow_t flow_control = TDN_IL_NEXT;
    while (pc != ctx->method->MethodBody->ILSize) {
        tdn_il_inst_t inst;
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));
        uint32_t next_pc = pc + inst.length;

        // For debug, print the instruction
        tdn_host_printf("[*] \t\t\tIL_%04x: %s", pc, tdn_get_opcode_name(inst.opcode));
        switch (inst.operand_type) {
            case TDN_IL_BRANCH_TARGET: tdn_host_printf(" IL_%04x", inst.operand.branch_target); break;
            case TDN_IL_NO_OPERAND: break;
            case TDN_IL_VARIABLE: tdn_host_printf(" %d", inst.operand.variable); break;
            case TDN_IL_INT8: tdn_host_printf(" %d", inst.operand.int8); break;
            case TDN_IL_INT32: tdn_host_printf(" %d", inst.operand.int32); break;
            case TDN_IL_INT64: tdn_host_printf(" %lld", (long long int)inst.operand.int64); break;
            case TDN_IL_FLOAT32: tdn_host_printf(" %f", inst.operand.float32); break;
            case TDN_IL_FLOAT64: tdn_host_printf(" %f", inst.operand.float64); break;
            case TDN_IL_METHOD: tdn_host_printf(" %U:%U", inst.operand.method->DeclaringType->Name, inst.operand.method->Name); break; // TODO: better name printing
            case TDN_IL_FIELD: tdn_host_printf(" %U:%U", inst.operand.field->DeclaringType->Name, inst.operand.field->Name); break; // TODO: better name printing
            case TDN_IL_TYPE: tdn_host_printf(" %U", inst.operand.type->Name); break; // TODO: better name printing
            case TDN_IL_STRING: tdn_host_printf(" %U", inst.operand.string); break;
        }
        tdn_host_printf("\n");

        // normalize the instruction for easier processing now that we printed it
        tdn_normalize_inst(&inst);

        // if we are coming from an instruction that can not jump to us
        // then we must first clear our stack
        if (
            flow_control == TDN_IL_BREAK ||
            flow_control == TDN_IL_RETURN ||
            flow_control == TDN_IL_BRANCH ||
            flow_control == TDN_IL_THROW
        ) {
            eval_stack_clear(&stack);
        }

        // check if there are more labels, if we are at a label we
        // need to properly switch to it
        bool has_label = false;
        if (label_idx < arrlen(labels)) {
            if (pc == labels[label_idx].address) {
                // found the current label
                jit_label_t* label = &labels[label_idx];
                spidir_block_t block = label->block;
                has_label = true;

                // can't have a label between a
                // prefix and instruction
                CHECK(flow_control != TDN_IL_META);

                // check the last opcode to see how we got to this
                // new label
                if (
                    flow_control == TDN_IL_NEXT ||
                    flow_control == TDN_IL_CALL
                ) {
                    CHECK_AND_RETHROW(eval_stack_move_to_slots(&stack, builder));

                    // we got from a normal instruction, insert a jump
                    spidir_builder_build_branch(builder, block);
                }

                // we are now at this block
                spidir_builder_set_block(builder, block);

                // and increment so we will check
                // against the next label
                label_idx++;
            }

            // make sure the label is always after or is at
            // the next pc, if there is another label
            if (label_idx < arrlen(labels)) {
                CHECK(labels[label_idx].address >= pc);
            }
        }

        // if the previous instruction is one that does not
        // fall through, make sure we have a label that gets
        // to us
        if (
            flow_control == TDN_IL_BREAK ||
            flow_control == TDN_IL_RETURN ||
            flow_control == TDN_IL_BRANCH ||
            flow_control == TDN_IL_THROW
        ) {
            CHECK(has_label);
        }

        // TODO: control flow handling

        //
        // Update the context for the jitting function
        //
        ctx->next_pc = next_pc;
        ctx->pc = pc;
        ctx->inst = &inst;
        ctx->stack = &stack;

        //
        // resolve the target and next label as needed
        //
        jit_label_t* target_label = NULL;
        jit_label_t* next_label = NULL;

        if (inst.operand_type == TDN_IL_BRANCH_TARGET) {
            target_label = jit_get_label(labels, inst.operand.branch_target);
            CHECK(target_label != NULL);
            // TODO: verify the stack consistency
        }

        if (inst.control_flow == TDN_IL_COND_BRANCH) {
            next_label = jit_get_label(labels, next_pc);
            CHECK(next_label != NULL);
            // TODO: verify the stack consistency
        }

        //
        // the main instruction jitting
        // TODO: split this to multiple functions in different places
        //
        switch (inst.opcode) {
            case CEE_LDARG: {
                uint16_t arg = inst.operand.variable;

                // resolve the argument type
                RuntimeTypeInfo arg_type = NULL;
                if (arg == 0 && !ctx->method->Attributes.Static) {
                    // non-static method's first arg is the this
                    arg_type = JIT_THIS_TYPE;
                } else {
                    // this is not included in Parameters list
                    uint16_t param_arg = arg - (ctx->method->Attributes.Static ? 0 : 1);

                    // get the correct argument
                    CHECK(param_arg < ctx->method->Parameters->Length);
                    arg_type = ctx->method->Parameters->Elements[param_arg]->ParameterType;
                }

                // push the value
                eval_stack_push(ctx->stack, arg_type, spidir_builder_build_param_ref(builder, arg));
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Control flow
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_BR: {
                CHECK_AND_RETHROW(eval_stack_move_to_slots(&stack, builder));

                // a branch, emit the branch
                spidir_builder_build_branch(builder, target_label->block);
            } break;

            case CEE_BRFALSE:
            case CEE_BRTRUE: {
                // pop the item
                spidir_value_t value;
                RuntimeTypeInfo value_type;
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value_type, &value));

                // ECMA-335 doesn't say brtrue takes in anything but
                // O and native int, but I think its just an oversight
                CHECK(
                    tdn_type_is_referencetype(value_type) ||
                    value_type == tInt32 ||
                    value_type == tInt64 ||
                    value_type == tIntPtr);

                // create the comparison
                spidir_icmp_kind_t kind = inst.opcode == CEE_BRTRUE ? SPIDIR_ICMP_NE : SPIDIR_ICMP_EQ;
                spidir_value_t cmp = spidir_builder_build_icmp(builder,
                                            SPIDIR_ICMP_NE,
                                            SPIDIR_TYPE_I32,
                                            value, spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 0));

                // check if one of the blocks needs to have the stack in stack slots
                CHECK_AND_RETHROW(eval_stack_move_to_slots(&stack, builder));

                // a branch, emit the branch
                spidir_builder_build_brcond(builder, cmp, target_label->block, next_label->block);
            } break;

            case CEE_BEQ:
            case CEE_BGE:
            case CEE_BGT:
            case CEE_BLE:
            case CEE_BLT:
            case CEE_BNE_UN:
            case CEE_BGE_UN:
            case CEE_BGT_UN:
            case CEE_BLE_UN:
            case CEE_BLT_UN:
            case CEE_CEQ:
            case CEE_CGT:
            case CEE_CGT_UN:
            case CEE_CLT:
            case CEE_CLT_UN: {
                // pop the items
                spidir_value_t value1, value2;
                RuntimeTypeInfo value1_type, value2_type;
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value2_type, &value2));
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value1_type, &value1));

                //
                // perform the binary comparison and branch operations check,
                // anything else can not be tested
                //
                if (value1_type == tInt32) {
                    CHECK(value2_type == tInt32 || value2_type == tIntPtr);

                } else if (value1_type == tInt64) {
                    CHECK(value2_type == tInt64);

                } else if (value1_type == tIntPtr) {
                    CHECK(value2_type == tInt32 || value2_type == tIntPtr);

                } else if (value1_type->IsByRef) {
                    // TODO: does this only apply to types
                    //       of the same reference? I assume
                    //       it does but we might need to change this
                    CHECK(value2_type == value1_type);

                } else if (tdn_type_is_referencetype(value1_type) && tdn_type_is_referencetype(value2_type)) {
                    CHECK(
                        inst.opcode == CEE_BEQ ||
                        inst.opcode == CEE_BNE_UN ||
                        inst.opcode == CEE_CEQ ||
                        inst.opcode == CEE_CGT_UN
                    );

                } else {
                    CHECK_FAIL();
                }

                // spidir only has the one side, need to flip for the other side
                spidir_icmp_kind_t kind;
                bool compare = false;
                switch (inst.opcode) {
                    case CEE_CEQ: compare = true;
                    case CEE_BEQ: kind = SPIDIR_ICMP_EQ; break;
                    case CEE_BGE: kind = SPIDIR_ICMP_SLE; SWAP(value1, value2); break;
                    case CEE_CGT: compare = true;
                    case CEE_BGT: kind = SPIDIR_ICMP_SLT; SWAP(value1, value2); break;
                    case CEE_BLE: kind = SPIDIR_ICMP_SLE; break;
                    case CEE_CLT: compare = true;
                    case CEE_BLT: kind = SPIDIR_ICMP_SLT; break;
                    case CEE_BNE_UN: kind = SPIDIR_ICMP_NE; break;
                    case CEE_BGE_UN: kind = SPIDIR_ICMP_ULE; SWAP(value1, value2); break;
                    case CEE_CGT_UN: compare = true;
                    case CEE_BGT_UN: kind = SPIDIR_ICMP_ULT; SWAP(value1, value2); break;
                    case CEE_BLE_UN: kind = SPIDIR_ICMP_ULE; break;
                    case CEE_CLT_UN: compare = true;
                    case CEE_BLT_UN: kind = SPIDIR_ICMP_ULT; break;
                    default: CHECK_FAIL();
                }

                // create the comparison
                spidir_value_t cmp = spidir_builder_build_icmp(builder,
                                                               kind,
                                                               SPIDIR_TYPE_I32,
                                                               value1, value2);

                // check if its a compare or not
                if (compare) {
                    // a compare, just push the result as an int32
                    eval_stack_push(&stack, tInt32, cmp);
                } else {
                    CHECK_AND_RETHROW(eval_stack_move_to_slots(&stack, builder));

                    // a branch, emit the branch
                    spidir_builder_build_brcond(builder, cmp, target_label->block, next_label->block);
                }
            } break;

            case CEE_RET: {
                RuntimeTypeInfo wanted_ret_type = method->ReturnParameter->ParameterType;
                if (wanted_ret_type == tVoid) {
                    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
                } else {
                    RuntimeTypeInfo ret_type;
                    spidir_value_t ret_value;
                    eval_stack_pop(&stack, builder, &ret_type, &ret_value);

                    // TODO: type check we can return this type;

                    spidir_builder_build_return(builder, ret_value);
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Math related
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDC_I4: {
                // NOTE: we are treating the value as a uint32 so it will not sign extend it
                eval_stack_push(&stack, tInt32,
                                spidir_builder_build_iconst(builder,
                                                            SPIDIR_TYPE_I32, inst.operand.uint32));
            } break;

            case CEE_ADD: {
                // pop the items
                spidir_value_t value1, value2;
                RuntimeTypeInfo value1_type, value2_type;
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value2_type, &value2));
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value1_type, &value1));

                // figure the type we are going to use
                RuntimeTypeInfo result = NULL;
                if (value1_type == tInt32) {
                    if (value2_type == tInt32) {
                        result = tInt32;
                    } else if (value2_type == tIntPtr) {
                        result = tIntPtr;
                    } else {
                        CHECK_FAIL();
                    }
                } else if (value1_type == tInt64) {
                    CHECK(value2_type == tInt64);
                    result = tInt64;
                } else if (value1_type == tIntPtr) {
                    if (value2_type == tInt32 || value2_type == tIntPtr) {
                        result = tIntPtr;
                    } else {
                        CHECK_FAIL();
                    }
                } else {
                    CHECK_FAIL();
                }

                // TODO: for floats make sure it is an instruction that can take floats

                // create the operation
                spidir_value_t result_value;
                switch (inst.opcode) {
                    case CEE_ADD: result_value = spidir_builder_build_iadd(builder, value1, value2); break;
                    case CEE_SUB: result_value = spidir_builder_build_isub(builder, value1, value2); break;
                    case CEE_AND: result_value = spidir_builder_build_and(builder, value1, value2); break;
                    case CEE_OR: result_value = spidir_builder_build_or(builder, value1, value2); break;
                    case CEE_XOR: result_value = spidir_builder_build_xor(builder, value1, value2); break;
                    case CEE_MUL: result_value = spidir_builder_build_imul(builder, value1, value2); break;
                    case CEE_DIV: result_value = spidir_builder_build_sdiv(builder, value1, value2); break;
                    case CEE_DIV_UN: result_value = spidir_builder_build_udiv(builder, value1, value2); break;
                    default: CHECK_FAIL();
                }

                // push it to the stack
                eval_stack_push(&stack, result, result_value);
            } break;

            default:
                CHECK_FAIL();
        }

        // move the pc forward
        pc = next_pc;
        flow_control = inst.control_flow;
    }

    // make sure we went over all of the labels
    CHECK(label_idx == arrlen(labels));

cleanup:
    arrfree(labels);
    arrfree(stack.stack);

    return;
}

static tdn_err_t jit_method(jit_context_t* ctx) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_type_t* params = NULL;
    RuntimeMethodBase method = ctx->method;

    //
    // create the spidir function
    //

    // handle the return value
    spidir_value_type_t ret_type = get_spidir_value_type(method->ReturnParameter->ParameterType);

    // handle the this argument
    RuntimeTypeInfo this_type = NULL;
    if (!method->Attributes.Static) {
        this_type = method->DeclaringType;
        if (tdn_type_is_valuetype(this_type)) {
            // this is a valuetype, the this is a reference
            CHECK_AND_RETHROW(tdn_get_byref_type(this_type, &this_type));
        }
        arrpush(params, get_spidir_value_type(this_type));
    }

    // handle the arguments
    for (size_t i = 0; i < method->Parameters->Length; i++) {
        arrpush(params, get_spidir_value_type(method->Parameters->Elements[i]->ParameterType));
    }

    // generate the name
    // TODO: something proper
    char test[6] = {0};
    static int i = 0;
    test[0] = 't';
    test[1] = 'e';
    test[2] = 's';
    test[3] = 't';
    test[4] = '0' + i++;
    test[5] = '\0';

    // create the function itself
    spidir_function_t func = spidir_module_create_function(
            m_spidir_module,
            test, ret_type, arrlen(params), params
    );

    spidir_module_build_function(m_spidir_module, func, jit_method_callback, ctx);

//    tdn_jit_dump();

cleanup:
    arrfree(params);

    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// High-level apis
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t tdn_jit_method(RuntimeMethodBase methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_context_t ctx = {
        .method = methodInfo
    };
    CHECK_AND_RETHROW(jit_method(&ctx));

cleanup:
    return err;
}

tdn_err_t tdn_jit_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // jit all the virtual methods, as those are the one that can be called
    // by other stuff unknowingly, the rest are going to be jitted lazyily
    for (int i = 0; i < type->DeclaredMethods->Length; i++) {
        RuntimeMethodBase method = (RuntimeMethodBase)type->DeclaredMethods->Elements[i];
        if (!method->Attributes.Virtual) continue;
        CHECK_AND_RETHROW(tdn_jit_method(method));
    }

cleanup:
    return err;
}
