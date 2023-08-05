#include "tinydotnet/types/type.h"
#include "tinydotnet/jit/jit.h"
#include "util/except.h"
#include "util/stb_ds.h"
#include "tinydotnet/disasm.h"
#include "util/string_builder.h"

// TODO: lock

#include <spidir/spidir.h>

typedef struct jit_context {
    RuntimeMethodBase method;
} jit_context_t;

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
// Evaluation stack
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The jitter itself
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct jit_label {
    uint32_t address;
    spidir_block_t block;
} jit_label_t;

static jit_label_t* add_label(jit_label_t** labels, uint32_t address) {
    int i;
    for (i = 0; i < arrlen(*labels); i++) {
        if ((*labels)[i].address > address) {
            break;
        } else if ((*labels)[i].address == address) {
            // already has a label in here
            return NULL;
        }
    }

    jit_label_t label = {
        .address = address
    };
    arrins(*labels, i, label);
    return &((*labels)[i]);
}

static void jit_method_callback(spidir_builder_handle_t builder, void* _ctx) {
    jit_context_t* ctx = _ctx;
    tdn_err_t err = TDN_NO_ERROR;
    jit_label_t* labels = NULL;
    RuntimeMethodBase method = ctx->method;

    TRACE("%U::%U", ctx->method->DeclaringType->Name, ctx->method->Name);

    //
    // first pass, find all of the labels, this will
    // also create all the different basic blocks on
    // the way
    //

    // entry block
    jit_label_t* entry_label = add_label(&labels, 0);
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
            jit_label_t* label = add_label(&labels, inst.operand.branch_target);
            if (label != NULL) {
                label->block = spidir_builder_create_block(builder);
            }

            // if this is a conditional branch then we need
            // to set the label for the next opcode as well
            if (inst.control_flow == TDN_IL_COND_BRANCH) {
                label = add_label(&labels, pc);
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

        // check if there are more labels, if we are at a label we
        // need to properly switch to it
        bool has_label = false;
        if (label_idx < arrlen(labels)) {
            if (pc == labels[label_idx].address) {
                // found the current label
                spidir_block_t block = labels[label_idx].block;
                has_label = true;

                // can't have a label between a
                // prefix and instruction
                CHECK(flow_control != TDN_IL_META);

                // check the last opcode to see how we got to this
                // new label
                if (
                    flow_control == TDN_IL_NEXT ||
                    flow_control == TDN_IL_COND_BRANCH ||
                    flow_control == TDN_IL_CALL
                ) {
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
            case TDN_IL_METHOD: tdn_host_printf(" %U:%U",
                                                inst.operand.method->DeclaringType->Name,
                                                inst.operand.method->Name); break; // TODO: better name printing
            case TDN_IL_FIELD: tdn_host_printf(" %U:%U",
                                               inst.operand.field->DeclaringType->Name,
                                               inst.operand.field->Name); break; // TODO: better name printing
            case TDN_IL_TYPE: tdn_host_printf(" %U",
                                              inst.operand.type->Name); break; // TODO: better name printing
            case TDN_IL_STRING: tdn_host_printf(" %U", inst.operand.string); break;
        }
        tdn_host_printf("\n");

        //
        // the main instruction jitting
        // TODO: split this to multiple functions in different places
        //
        switch (inst.opcode) {
            case CEE_RET: {
                if (method->ReturnParameter->ParameterType == tVoid) {
                    // no return value
                     spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
                } else {
                    // TODO: this
                    CHECK_FAIL();
                }
            } break;

            default:
                break;
        }

        // move the pc forward
        pc = next_pc;
        flow_control = inst.control_flow;
    }

    // make sure we went over all of the labels
    CHECK(label_idx == arrlen(labels));

cleanup:
    arrfree(labels);

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

    tdn_jit_dump();

cleanup:
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
