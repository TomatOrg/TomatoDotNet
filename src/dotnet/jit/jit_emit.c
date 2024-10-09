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

/**
 * The module used for jitting
 */
static spidir_module_handle_t m_jit_module = NULL;

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
    if (info == tInt32) {
        return SPIDIR_TYPE_I32;
    } else if (info == tInt64 || info == tIntPtr) {
        return SPIDIR_TYPE_I64;
    } else {
        return SPIDIR_TYPE_PTR;
    }
}

static spidir_mem_size_t get_spidir_size(RuntimeTypeInfo info) {
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

    for (int i = 0; i < method->Parameters->Length; i++) {
        RuntimeTypeInfo type = tdn_get_intermediate_type(method->ReturnParameter->ParameterType);
        if (type == tInt32) {
            arrpush(types, SPIDIR_TYPE_I32);
        } else if (type == tInt64 || type == tIntPtr) {
            arrpush(types, SPIDIR_TYPE_I64);
        } else {
            arrpush(types, SPIDIR_TYPE_PTR);
        }
    }

    // implicit retval pointer
    if (jit_is_struct_like(method->ReturnParameter->ParameterType)) {
        arrpush(types, SPIDIR_TYPE_PTR);
    }

    return types;
}

static void jit_queue_block(jit_emit_ctx_t* ctx, spidir_builder_handle_t builder, jit_basic_block_t* block) {
    if (block->state == JIT_BLOCK_VERIFIED) {
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

#define EVAL_STACK_PUSH(_type, _value, ...) \
    do { \
        CHECK(arrlen(stack) < body->MaxStackSize); \
        jit_stack_value_t __item = { .type = tdn_get_intermediate_type(_type), .value = _value, ## __VA_ARGS__ }; \
        arrpush(stack, __item); \
    } while (0)

#define EVAL_STACK_POP() \
    ({ \
        CHECK(arrlen(stack) > 0); \
        arrpop(stack); \
    })

static void jit_emit_memcpy(spidir_builder_handle_t builder, spidir_value_t dst, spidir_value_t src, size_t size) {
    ASSERT(!"TODO: jit_emit_memcpy");
}

static void jit_emit_bzero(spidir_builder_handle_t builder, spidir_value_t dst, size_t size) {
    ASSERT(!"TODO: jit_emit_bzero");
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
        pc += inst.length;

        switch (inst.opcode) {

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Argument access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDARG: {
                int index = inst.operand.variable;
                jit_arg_t* arg = &ctx->method->args[index];

                spidir_value_t value = SPIDIR_VALUE_INVALID;
                if (arg->spill_required) {
                    // load from the spill
                    value = spidir_builder_build_load(builder,
                        get_spidir_size(arg->type),
                        get_spidir_type(arg->type),
                        arg->value);

                } else if (jit_is_struct_like(arg->type)) {
                    // copy again
                    CHECK_FAIL("TODO");

                } else {
                    // just use the value directly
                    value = arg->value;

                }

                EVAL_STACK_PUSH(arg->type, value);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Local variable access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDLOC: {
                jit_item_attrs_t* local_attrs = &block->locals[inst.operand.variable];
                jit_local_t* local = &ctx->method->locals[inst.operand.variable];

                // load the local
                spidir_value_t value;
                if (jit_is_struct_like(local->type)) {
                    CHECK_FAIL();
                } else {
                    value = spidir_builder_build_load(builder,
                        get_spidir_size(local->type),
                        get_spidir_type(local->type),
                        local->value);
                }

                // we are going to pass the known type if any
                EVAL_STACK_PUSH(local->type, value, .attrs.known_type = local_attrs->known_type);
            } break;

            case CEE_LDLOCA: {
                jit_item_attrs_t* local_attrs = &block->locals[inst.operand.variable];
                jit_local_t* local = &ctx->method->locals[inst.operand.variable];

                // load the local with its attributes to the stack, this will fail
                // if this is already a byref
                RuntimeTypeInfo local_type = tdn_get_intermediate_type(local->type);
                CHECK_AND_RETHROW(tdn_get_byref_type(local_type, &local_type));

                // if we know the exact type then also set it
                RuntimeTypeInfo known_type = NULL;
                if (local_attrs->known_type != NULL) {
                    CHECK_AND_RETHROW(tdn_get_byref_type(local_attrs->known_type, &known_type));
                }

                // we are going to pass the known type if any
                EVAL_STACK_PUSH(local_type, local->value, .attrs.known_type = known_type);
            } break;

            case CEE_STLOC: {
                jit_stack_value_t value = EVAL_STACK_POP();

                // save the known type for later use
                block->locals[inst.operand.variable].known_type = value.attrs.known_type;

                // and now store it in the local
                jit_local_t* local = &ctx->method->locals[inst.operand.variable];
                if (jit_is_struct_like(local->type)) {
                    jit_emit_memcpy(builder, local->value, value.value, local->type->StackSize);
                } else {
                    spidir_builder_build_store(builder,
                        get_spidir_size(local->type),
                        value.value,
                        local->value);
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Method calling
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_CALL:
            case CEE_CALLVIRT: {
                RuntimeMethodBase target = inst.operand.method;
                RuntimeTypeInfo method_type = NULL;

                if (!target->Attributes.Static) {
                    method_type = target->DeclaringType;
                }

                // get the method
                jit_method_t* target_method;
                CHECK_AND_RETHROW(jit_get_or_create_method(target, &target_method));

                // pass the this
                if (method_type != NULL) {
                    jit_stack_value_t value = EVAL_STACK_POP();
                    arrpush(args, value.value);

                    // if this is a virt call and we know the exact
                    // type this is, then just use that method directly
                    if (inst.opcode == CEE_CALLVIRT && value.attrs.known_type != NULL) {
                        target = (RuntimeMethodBase)value.attrs.known_type->VTable->Elements[target->VTableOffset];
                        inst.opcode = CEE_CALL;
                    }
                }

                // pass all the other args
                for (int i = 0 ; i < target->Parameters->Length; i++) {
                    jit_stack_value_t value = EVAL_STACK_POP();
                    arrpush(args, value.value);
                }

                // pass the implicit return attribute
                RuntimeTypeInfo return_type = target->ReturnParameter->ParameterType;
                if (jit_is_struct_like(return_type)) {
                    // TODO: this
                    CHECK_FAIL();
                }

                // now perform the call
                spidir_value_t result;
                if (inst.opcode == CEE_CALLVIRT) {
                    // load the value from the vtable
                    spidir_value_t addr = SPIDIR_VALUE_INVALID;
                    if (jit_is_interface(method_type)) {
                        // load the vtable part
                        addr = spidir_builder_build_ptroff(builder,
                            args[0], spidir_builder_build_iconst(builder,
                                SPIDIR_TYPE_I64, offsetof(Interface, VTable)));
                        addr = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64, addr);

                        // now load the actual pointer from it
                        size_t offset = target->VTableOffset * sizeof(void*);
                        addr = spidir_builder_build_ptroff(builder, addr,
                            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offset));

                        // load the instance
                        args[0] = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, args[0]);

                    } else {
                        // load the vtable
                        addr = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64, args[0]);
                        addr = spidir_builder_build_inttoptr(builder, addr);

                        // add to the offset
                        size_t offset = offsetof(ObjectVTable, Functions) + target->VTableOffset * sizeof(void*);
                        addr = spidir_builder_build_ptroff(builder, addr,
                            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offset));

                        // and now read the pointer itself
                        addr = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, addr);
                    }

                    // perform the indirect call
                    args_type = get_spidir_arg_types(target);
                    result = spidir_builder_build_callind(builder,
                        get_spidir_ret_type(target),
                        arrlen(args_type),
                        args_type,
                        addr,
                        args
                    );
                } else {
                    result = spidir_builder_build_call(builder,
                        target_method->function,
                        arrlen(args),
                        args
                    );
                }

                // push the result
                if (return_type != tVoid && !jit_is_struct_like(return_type)) {
                    EVAL_STACK_PUSH(return_type, result);
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Math
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_ADD:
            case CEE_SUB:
            case CEE_MUL:
            case CEE_DIV:
            case CEE_REM:
            case CEE_AND:
            case CEE_OR:
            case CEE_XOR:
            case CEE_DIV_UN:
            case CEE_REM_UN: {
                jit_stack_value_t op1 = EVAL_STACK_POP();
                jit_stack_value_t op2 = EVAL_STACK_POP();

                // check the inputs and get the result type
                RuntimeTypeInfo result = NULL;
                if (op1.type == tInt32) {
                    if (op2.type == tInt32) {
                        result = tInt32;
                    } else if (op2.type == tIntPtr) {
                        result = tIntPtr;
                    } else {
                        CHECK_FAIL();
                    }

                } else if (op1.type == tInt64) {
                    CHECK(op2.type == tInt64);

                } else if (op1.type == tIntPtr) {
                    CHECK(op2.type == tInt32 || op2.type == tIntPtr);
                    result = tIntPtr;

                } else {
                    CHECK_FAIL();
                }

                spidir_value_t value;
                switch (inst.opcode) {
                    case CEE_ADD: value = spidir_builder_build_iadd(builder, op1.value, op2.value); break;
                    case CEE_SUB: value = spidir_builder_build_isub(builder, op1.value, op2.value); break;
                    case CEE_MUL: value = spidir_builder_build_imul(builder, op1.value, op2.value); break;
                    case CEE_DIV: value = spidir_builder_build_sdiv(builder, op1.value, op2.value); break;
                    case CEE_REM: value = spidir_builder_build_srem(builder, op1.value, op2.value); break;
                    case CEE_AND: value = spidir_builder_build_and(builder, op1.value, op2.value); break;
                    case CEE_OR: value = spidir_builder_build_or(builder, op1.value, op2.value); break;
                    case CEE_XOR: value = spidir_builder_build_xor(builder, op1.value, op2.value); break;
                    case CEE_DIV_UN: value = spidir_builder_build_udiv(builder, op1.value, op2.value); break;
                    case CEE_REM_UN: value = spidir_builder_build_urem(builder, op1.value, op2.value); break;
                    default: CHECK_FAIL();
                }

                EVAL_STACK_PUSH(result, value);
            } break;

            case CEE_CEQ:
            case CEE_CGT:
            case CEE_CLT:
            case CEE_CGT_UN:
            case CEE_CLT_UN: {
                jit_stack_value_t value1 = EVAL_STACK_POP();
                jit_stack_value_t value2 = EVAL_STACK_POP();

                spidir_icmp_kind_t kind;
                spidir_value_t val1 = value1.value;
                spidir_value_t val2 = value2.value;
                switch (inst.opcode) {
                    case CEE_CEQ: kind = SPIDIR_ICMP_EQ; break;
                    case CEE_CGT: val1 = value2.value; val2 = value1.value;
                    case CEE_CLT: kind = SPIDIR_ICMP_SLT; break;
                    case CEE_CGT_UN: val1 = value2.value; val2 = value1.value;
                    case CEE_CLT_UN: kind = SPIDIR_ICMP_ULT; break;
                    default: CHECK_FAIL();
                }
                spidir_value_t value = spidir_builder_build_icmp(builder, kind, SPIDIR_TYPE_I32, val1, val2);

                EVAL_STACK_PUSH(tInt32, value);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Stack manipulation
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDC_I4: {
                spidir_value_t value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, (uint32_t)inst.operand.int32);
                EVAL_STACK_PUSH(tInt32, value);
            } break;

            case CEE_LDC_I8: {
                spidir_value_t value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst.operand.int64);
                EVAL_STACK_PUSH(tInt64, value);
            } break;

            case CEE_LDNULL: {
                spidir_value_t value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0);
                EVAL_STACK_PUSH(NULL, value);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Branching
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_BR: {
                // resolve the branch target
                spidir_block_t target;
                CHECK_AND_RETHROW(emit_merge_basic_block(ctx, builder,
                    inst.operand.branch_target, stack, &target));

                // and now jump to it
                spidir_builder_build_branch(builder, target);
            } break;

            case CEE_RET: {
                RuntimeTypeInfo return_type = tdn_get_intermediate_type(method->ReturnParameter->ParameterType);

                if (return_type == tVoid) {
                    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
                } else {
                    jit_stack_value_t value = EVAL_STACK_POP();

                    if (jit_is_struct_like(value.type)) {
                        spidir_value_t ret_ptr = spidir_builder_build_param_ref(builder, 0);
                        jit_emit_memcpy(builder, ret_ptr, value.value, value.type->StackSize);
                        spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);

                    } else {
                        spidir_builder_build_return(builder, value.value);
                    }
                }
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
            spidir_mem_size_t size = get_spidir_size(arg->type);
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
            jit_emit_bzero(builder, arg->value, arg->type->StackSize);
        } else {
            spidir_builder_build_store(builder,
                get_spidir_size(arg->type),
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
    CHECK(method->method->MethodBody != NULL);

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
    string_builder_t builder;
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
        m_jit_module = spidir_module_create();
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
            uint64_t F = (uint64_t)(jit_get_method_from_function(relocs[j].target)->method->MethodPtr);
            int64_t A = relocs[j].addend;

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

    // cleanup the module
    if (m_jit_module != NULL) {
        spidir_module_destroy(m_jit_module);
        m_jit_module = NULL;
    }

    return err;
}
