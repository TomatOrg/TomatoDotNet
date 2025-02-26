#include "jit_emit.h"

#include <tomatodotnet/disasm.h>
#include <util/except.h>
#include <util/stb_ds.h>
#include <util/string.h>
#include <util/string_builder.h>

#include "jit_builtin.h"
#include "jit_verify.h"

spidir_value_type_t jit_get_spidir_type(RuntimeTypeInfo type) {
    if (
        type == tBoolean || type == tChar ||
        type == tSByte || type == tByte ||
        type == tInt16 || type == tUInt16 ||
        type == tInt32 || type == tUInt32
    ) {
        return SPIDIR_TYPE_I32;
    } else if (
        type == tInt64 || type == tUInt64 ||
        type == tIntPtr  || type == tUIntPtr
    ) {
        return SPIDIR_TYPE_I64;
    } else {
        // anything else is a pointer, be structs, refs, pointers, arrays or whatever
        return SPIDIR_TYPE_PTR;
    }
}

spidir_value_type_t jit_get_spidir_ret_type(RuntimeMethodBase method) {
    RuntimeTypeInfo type = method->ReturnParameter->ParameterType;
    if (jit_is_struct_like(type)) {
        // things which act like a struct return by using an implicit reference
        return SPIDIR_TYPE_NONE;
    }

    return jit_get_spidir_type(type);
}

spidir_value_type_t* jit_get_spidir_arg_types(RuntimeMethodBase method) {
    spidir_value_type_t* types = NULL;

    // this pointer
    if (!method->Attributes.Static) {
        arrpush(types, SPIDIR_TYPE_PTR);
    }

    // and now all of the arguments
    for (int i = 0; i < method->Parameters->Length; i++) {
        spidir_value_type_t type = jit_get_spidir_type(method->Parameters->Elements[i]->ParameterType);
        arrpush(types, type);
    }

    // implicit retval pointer
    RuntimeTypeInfo ret_type = method->ReturnParameter->ParameterType;
    if (
        ret_type != tVoid &&
        jit_is_struct_like(ret_type)
    ) {
        arrpush(types, SPIDIR_TYPE_PTR);
    }

    return types;
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Actual emit code
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//----------------------------------------------------------------------------------------------------------------------
// De-virt
//----------------------------------------------------------------------------------------------------------------------

static size_t jit_get_interface_offset(RuntimeTypeInfo type, RuntimeTypeInfo iface) {
    int idx = hmgeti(type->InterfaceImpls, iface);
    if (idx < 0) {
        return -1;
    }
    return type->InterfaceImpls[idx].value;
}

static RuntimeMethodBase jit_devirt_method(jit_value_t instance, RuntimeMethodBase method) {
    // not a virtual method, nothing to de-virt
    if (!method->Attributes.Virtual) {
        return method;
    }

    // choose either the knwon or the generic type
    RuntimeTypeInfo type = instance.attrs.known_type != NULL ? instance.attrs.known_type : instance.type;

    // perform a vtable lookup
    RuntimeMethodBase real_method;
    if (method->DeclaringType->Attributes.Interface) {
        size_t offset = jit_get_interface_offset(type, method->DeclaringType);
        ASSERT(offset != -1);
        // TODO: variance support
        real_method = (RuntimeMethodBase)type->VTable->Elements[offset + method->VTableOffset];
    } else {
        real_method = (RuntimeMethodBase)type->VTable->Elements[method->VTableOffset];
    }

    // if this is the final implementation, or we are a sealed
    // type then we can just return the real method
    if (real_method->Attributes.Final || type->Attributes.Sealed) {
        return real_method;
    }

    return method;
}

//----------------------------------------------------------------------------------------------------------------------
// Inlining
//----------------------------------------------------------------------------------------------------------------------

static bool jit_emit_should_inline(RuntimeMethodBase method) {
    return false;
}

//----------------------------------------------------------------------------------------------------------------------
// Block merging
//----------------------------------------------------------------------------------------------------------------------

static jit_basic_block_t* jit_emit_get_basic_block(jit_method_t* method, long target_pc, long leave_target) {
    int bi = hmgeti(method->labels, target_pc);
    if (bi < 0) {
        return NULL;
    }
    jit_basic_block_t* block = method->labels[bi].value;

    // lookup based on the leave target
    if (leave_target >= 0) {
        jit_leave_block_key_t key = {
            .block = block,
            .leave_target = leave_target
        };

        int bi = hmgeti(method->leave_blocks, key);
        if (bi < 0) {
            return NULL;
        }
        block = method->leave_blocks[bi].value;
    }

    return block;
}

static tdn_err_t jit_verify_merge(jit_value_t* incoming, jit_value_t* target) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(arrlen(incoming) == arrlen(target));
    for (int i = 0; i < arrlen(incoming); i++) {
        // sanity that everything ended up as we expected
        CHECK(incoming[i].type == target[i].type);
    }

    cleanup:
        return err;
}

static tdn_err_t jit_create_phis(spidir_builder_handle_t builder, jit_value_t* incoming, jit_value_t* target) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(arrlen(incoming) == arrlen(target));
    for (int i = 0; i < arrlen(incoming); i++) {
        // sanity that everything ended up as we expected
        CHECK(incoming[i].type == target[i].type);

        // and create the phi with the incoming value already
        if (!incoming->attrs.spilled) {
            target[i].value = spidir_builder_build_phi(builder,
                jit_get_spidir_type(incoming[i].type),
                1, &incoming[i].value,
                &target[i].phi
            );
        }
    }

cleanup:
    return err;
}

static tdn_err_t jit_merge_phis(spidir_builder_handle_t builder, jit_value_t* incoming, jit_value_t* target) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(arrlen(incoming) == arrlen(target));
    for (int i = 0; i < arrlen(incoming); i++) {
        // sanity that everything ended up as we expected
        CHECK(incoming[i].type == target[i].type);

        if (!incoming->attrs.spilled) {
            // and create the phi with the incoming value already
            spidir_builder_add_phi_input(builder, target[i].phi, incoming[i].value);
        }
    }

cleanup:
    return err;
}

static tdn_err_t jit_emit_merge_basic_block(
    jit_method_t* method,
    spidir_builder_handle_t builder,
    uint32_t target_pc, spidir_block_t* block,
    jit_value_t* stack, jit_value_t* locals, jit_value_t* args,
    long leave_target
) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_basic_block_t* target = jit_emit_get_basic_block(method, target_pc, leave_target);
    CHECK(target != NULL);
    CHECK(target->initialized);

    // push to the queue for emitting
    if (!target->in_queue) {
        arrpush(method->block_queue, target);
        target->in_queue = true;
    }

    // create the new block
    target->block = spidir_builder_create_block(builder);
    *block = target->block;

    // check if we need to generate and merge phis or not
    if (target->need_phis) {
        // will enter this code multiple times, prepare the phis and the new block
        if (!target->emitted) {
            // switch to the new block to create the phis
            spidir_block_t cur_block;
            bool has_current = spidir_builder_cur_block(builder, &cur_block);
            spidir_builder_set_block(builder, target->block);

            CHECK_AND_RETHROW(jit_create_phis(builder, stack, target->stack));
            CHECK_AND_RETHROW(jit_create_phis(builder, locals, target->locals));
            CHECK_AND_RETHROW(jit_create_phis(builder, args, target->args));

            // return to the current block
            if (has_current) {
                spidir_builder_set_block(builder, cur_block);
            }
        } else {
            // merge the phis
            CHECK_AND_RETHROW(jit_merge_phis(builder, stack, target->stack));
            CHECK_AND_RETHROW(jit_merge_phis(builder, locals, target->locals));
            CHECK_AND_RETHROW(jit_merge_phis(builder, args, target->args));
        }

    } else {
        // must only reach this once, so ensure that it is not yet initialized
        CHECK(!target->emitted);

        // for sanity perform merging, this is not required
        CHECK_AND_RETHROW(jit_verify_merge(stack, target->stack));
        CHECK_AND_RETHROW(jit_verify_merge(locals, target->locals));
        CHECK_AND_RETHROW(jit_verify_merge(args, target->args));
    }

    // mark as initialized
    target->emitted = true;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Actual emitting
//----------------------------------------------------------------------------------------------------------------------

#define EVAL_STACK_PUSH(_type, _value, ...) \
    do { \
        CHECK(arrlen(stack) < body->MaxStackSize); \
        jit_value_t __item = { .type = _type, .value = _value, ## __VA_ARGS__ }; \
        arrpush(stack, __item); \
    } while (0)

#define EVAL_STACK_POP() \
    ({ \
        CHECK(arrlen(stack) > 0); \
        arrpop(stack); \
    })

static long get_leave_target(uint32_t* leave_target_stack) {
    if (leave_target_stack == NULL) {
        return -1;
    }
    return arrlast(leave_target_stack);
}

static tdn_err_t jit_emit_basic_block(jit_method_t* jmethod, spidir_builder_handle_t builder, jit_basic_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = jmethod->method;
    RuntimeMethodBody body = method->MethodBody;

    // the context
    jit_value_t* stack = NULL;
    jit_value_t* locals = NULL;
    jit_value_t* args = NULL;
    spidir_value_t* spidir_args = NULL;
    RuntimeTypeInfo this_type = NULL;

    // figure the this type if this is a non-static method
    if (!method->Attributes.Static) {
        this_type = jmethod->args[0].type;
    }

    // ensure that the block we are seeing is already initialized
    // and copy over all the arrays
    CHECK(block->initialized);

    // set the spidir block to the current block
    spidir_builder_set_block(builder, block->block);

    // copy the locals state
    arrsetlen(stack, arrlen(block->stack));
    memcpy(stack, block->stack, arrlen(stack) * sizeof(*stack));
    arrsetlen(locals, arrlen(block->locals));
    memcpy(locals, block->locals, arrlen(locals) * sizeof(*locals));
    arrsetlen(args, arrlen(block->args));
    memcpy(args, block->args, arrlen(args) * sizeof(*args));

#ifdef JIT_VERBOSE_EMIT
    int indent = 0;
#endif

    // get the pc
    tdn_il_inst_t inst = { .control_flow = TDN_IL_CF_FIRST };
    uint32_t pc = block->start;
    while (pc < block->end) {
        // get the instruction
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));

#ifdef JIT_VERBOSE_VERIFY
        indent = tdn_disasm_print_start(body, pc, inst, indent);
#endif

        tdn_normalize_inst(&inst);
        uint32_t current_pc = pc;
        pc += inst.length;

        switch (inst.opcode) {

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arguments
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDARG: {
                CHECK(inst.operand.variable < arrlen(args));
                RuntimeTypeInfo type = jit_get_intermediate_type(args[inst.operand.variable].type);
                EVAL_STACK_PUSH(type, args[inst.operand.variable].value, .attrs = args[inst.operand.variable].attrs);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Locals
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDLOC: {
                CHECK(inst.operand.variable < arrlen(locals));
                RuntimeTypeInfo type = jit_get_intermediate_type(locals[inst.operand.variable].type);
                EVAL_STACK_PUSH(type, locals[inst.operand.variable].value, .attrs = locals[inst.operand.variable].attrs);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Calls
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_CALL:
            case CEE_CALLVIRT: {
                RuntimeMethodBase callee = inst.operand.method;
                RuntimeTypeInfo callee_this = NULL;

                if (!callee->Attributes.Static) {
                    callee_this = method->DeclaringType;
                    if (tdn_type_is_valuetype(callee_this)) {
                        CHECK_AND_RETHROW(tdn_get_byref_type(callee_this, &callee_this));
                    }
                }

                // check if we need an explicit null check
                bool explicit_null_check = false;
                if (inst.opcode == CEE_CALLVIRT && callee->Attributes.Static) {
                    explicit_null_check = true;
                }

                // add the parameters
                for (int i = callee->Parameters->Length - 1; i >= 0; i--) {
                    jit_value_t value = EVAL_STACK_POP();
                    arrins(spidir_args, 0, value.value);
                }

                // add the this type
                if (callee_this != NULL) {
                    jit_value_t value = EVAL_STACK_POP();
                    arrins(spidir_args, 0, value.value);

                    // now that we have the instance perform the de-virt, if we are
                    // successful then replace the callvirt with a normal call
                    if (inst.opcode == CEE_CALLVIRT) {
                        RuntimeMethodBase new_callee = jit_devirt_method(value, callee);
                        if (new_callee != callee) {
                            // needs an explicit null check now that we don't access the
                            // the vtable anymore
                            explicit_null_check = true;
                            callee = new_callee;
                            inst.opcode = CEE_CALL;
                        }
                    }
                }

                // get the ret type
                RuntimeTypeInfo ret_type = callee->ReturnParameter->ParameterType;
                spidir_value_t return_value = SPIDIR_VALUE_INVALID;

                if (inst.opcode == CEE_CALL) {
                    // attempt to call as a builtin
                    jit_builtin_emitter_t emitter = jit_get_builtin_emitter(callee);
                    if (emitter != NULL) {
                        return_value = emitter(builder, callee, spidir_args);

                    } else if (jit_emit_should_inline(callee)) {
                        // we should perform an inline of the new function
                        // TODO: this
                        CHECK_FAIL();

                    } else {
                        // nothing special to be done, just queue to emit it
                        CHECK_AND_RETHROW(jit_queue_emit_method(spidir_builder_get_module(builder), callee));

                        // get the jit function for it
                        jit_method_t* jit_callee = NULL;
                        CHECK_AND_RETHROW(jit_get_method(callee, &jit_callee));

                        // and now emit the call itself
                        return_value = spidir_builder_build_call(
                            builder,
                            jit_callee->function,
                            arrlen(spidir_args),
                            spidir_args
                        );
                    }

                } else {
                    // TODO: indirect call
                    CHECK_FAIL();
                }

                if (ret_type != tVoid) {
                    EVAL_STACK_PUSH(ret_type, return_value);
                }
            } break;

            case CEE_RET: {
                RuntimeTypeInfo type = method->ReturnParameter->ParameterType;

                spidir_value_t ret_value = SPIDIR_VALUE_INVALID;
                if (type != tVoid) {
                    ret_value = EVAL_STACK_POP().value;
                }

                if (jmethod->is_inline) {
                    // we are inside an inline, add output to the phi
                    // and branch to the continuation
                    if (type != tVoid) {
                        spidir_builder_add_phi_input(builder, jmethod->inline_return_phi, ret_value);
                    }
                    spidir_builder_build_branch(builder, jmethod->inline_return_block);
                } else {
                    spidir_builder_build_return(builder, ret_value);
                }

                CHECK(arrlen(stack) == 0);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Control flow
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_BR: {
                spidir_block_t new_block;
                CHECK_AND_RETHROW(jit_emit_merge_basic_block(
                    jmethod, builder,
                    inst.operand.branch_target, &new_block,
                    stack, locals, args,
                    get_leave_target(block->leave_target_stack)));
                spidir_builder_build_branch(builder, new_block);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Misc
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_NOP: {
            } break;

            default: CHECK_FAIL("Unknown opcode `%s`", tdn_get_opcode_name(inst.opcode));
        }

#ifdef JIT_VERBOSE_VERIFY
        indent = tdn_disasm_print_end(body, pc, indent);
#endif

        arrfree(spidir_args);
    }

    // we have a fallthrough
    if (inst.control_flow == TDN_IL_CF_NEXT || inst.control_flow == TDN_IL_CF_CALL) {
        spidir_block_t new_block;
        CHECK_AND_RETHROW(jit_emit_merge_basic_block(
            jmethod,
            builder,
            pc, &new_block,
            stack, locals, args,
            get_leave_target(block->leave_target_stack)));

        // branch into this block
        spidir_builder_build_branch(builder, new_block);
    }

    // last must be a valid instruction
    CHECK(
        inst.control_flow != TDN_IL_CF_FIRST &&
        inst.control_flow != TDN_IL_CF_META
    );

cleanup:
    arrfree(spidir_args);
    arrfree(stack);
    arrfree(locals);
    arrfree(args);

    return err;
}

static tdn_err_t jit_emit_prepare_method(jit_method_t* jmethod, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = jmethod->method;
    RuntimeMethodBody body = method->MethodBody;

    // setup the main entry block
    spidir_block_t entry_block = spidir_builder_create_block(builder);
    spidir_builder_set_block(builder, entry_block);
    spidir_builder_set_entry_block(builder, entry_block);

    CHECK(jmethod->basic_blocks != NULL);

    // TODO: check for spilled arguments/locals (taken by reference)

    // if we have a this add it to the local
    int param_i = 0;
    if (!method->Attributes.Static) {
        if (jmethod->args[param_i].attrs.spilled) {
            // spilled, means we need to copy it to a local
            // and store it in there
            CHECK_FAIL();
        } else {
            jmethod->args[param_i].value = spidir_builder_build_param_ref(builder, param_i);
        }
        param_i++;
    }

    // now prepare the rest of the arguments
    for (int i = 0; i < method->Parameters->Length; i++) {
        if (jmethod->args[param_i].attrs.spilled) {
            // spilled, means we need to copy it to a local
            // and store it in there
            CHECK_FAIL();
        } else {
            jmethod->args[param_i].value = spidir_builder_build_param_ref(builder, param_i);
        }
        param_i++;
    }

    // and now prepare the locals
    if (body->LocalVariables != NULL) {
        for (int i = 0; i < body->LocalVariables->Length; i++) {
            jit_value_t* local = &jmethod->locals[i];

            if (local->attrs.spilled || jit_is_struct_like(local->type)) {
                // the local needs to be stored in a stack slot
                local->value = spidir_builder_build_stackslot(builder, local->type->StackSize, local->type->StackAlignment);

                // if we need to initialize it emit a bzero
                if (local->attrs.needs_init) {
                    CHECK_FAIL();
                }

            } else {
                // non-spilled local and not a struct, initialize to either
                // an invalid value or to zero
                if (local->attrs.needs_init) {
                    local->value = spidir_builder_build_iconst(builder, jit_get_spidir_type(local->type), 0);
                } else {
                    local->value = SPIDIR_VALUE_INVALID;
                }
            }
        }
    }

cleanup:
    return err;
}

static tdn_err_t jit_emit_method(jit_method_t* method, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;

#ifdef JIT_DEBUG_EMIT
    TRACE("EMIT: %T::%U", method->method->DeclaringType, method->method->Name);
#endif

    // prepare the method, setting up the initial locals, arguments and so on
    CHECK_AND_RETHROW(jit_emit_prepare_method(method, builder));

    // "merge" with the first block, giving it the initial pc for everything
    spidir_block_t il_entry_block;
    CHECK_AND_RETHROW(jit_emit_merge_basic_block(
        method,
        builder,
        0, &il_entry_block,
        NULL, method->locals, method->args,
        -1)
    );

    // perform a branch from the entry block to this block
    spidir_builder_build_branch(builder, il_entry_block);

    while (arrlen(method->block_queue) > 0) {
        jit_basic_block_t* block = arrpop(method->block_queue);

#ifdef JIT_VERBOSE_EMIT
        TRACE("\tBlock (IL_%04x)", block->start);
#endif
        CHECK_AND_RETHROW(jit_emit_basic_block(method, builder, block));
    }

cleanup:
    arrfree(method->block_queue);

    return err;
}

typedef struct jit_emit_context {
    tdn_err_t err;
    jit_method_t* method;
} jit_emit_context_t;

static void jit_emit_il(spidir_builder_handle_t builder, void* _ctx) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_emit_context_t* ctx = _ctx;

    CHECK_AND_RETHROW(jit_emit_method(ctx->method, builder));

cleanup:
    ctx->err = err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Emit management code
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static jit_method_t** m_methods_to_emit = NULL;

tdn_err_t jit_init_emit() {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: stuff

cleanup:
    return err;
}

static spidir_function_t create_spidir_function(spidir_module_handle_t module, RuntimeMethodBase method, bool external) {
    // build the name
    string_builder_t builder = {};
    string_builder_push_method_signature(&builder, method, true);
    const char* name = string_builder_build(&builder);

    // get the signature
    spidir_value_type_t ret_type = jit_get_spidir_ret_type(method);
    spidir_value_type_t* arg_types = jit_get_spidir_arg_types(method);

    spidir_function_t function;
    if (external) {
        function = spidir_module_create_extern_function(module, name, ret_type, arrlen(arg_types), arg_types);
    } else {
        function = spidir_module_create_function(module, name, ret_type, arrlen(arg_types), arg_types);
    }

    arrfree(arg_types);
    string_builder_free(&builder);

    return function;
}


tdn_err_t jit_queue_emit_method(spidir_module_handle_t module, RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_method_t* jit_method = NULL;
    CHECK_AND_RETHROW(jit_get_method(method, &jit_method));

    // if the method was not emitted yet
    if (!jit_method->emitting) {
        if (method->MethodPtr != NULL) {
            // already emitted before, use the extern to it
            jit_method->function = create_spidir_function(module, method, true);

        } else {
            // normal/builtin function, create the method so it can be emitted
            jit_method->function = create_spidir_function(module, method, false);
            arrpush(m_methods_to_emit, jit_method);
        }

        jit_method->emitting = true;
    }

cleanup:
    return err;
}

static tdn_err_t jit_emit_all(spidir_module_handle_t module) {
    tdn_err_t err = TDN_NO_ERROR;

    while (arrlen(m_methods_to_emit) != 0) {
        jit_method_t* method = arrpop(m_methods_to_emit);

        // check if we need to verify it
        if (!method->verified && method->method->MethodBody != NULL) {
            CHECK_AND_RETHROW(jit_verify_method(method));
            method->verified = true;
        }

        // should not get something that is already jitted
        CHECK(method->method->MethodPtr == NULL);

        // and now we can do the real emitting
        if (method->method->MethodBody == NULL) {
            // builtin method, use the builtin emitter
            jit_builtin_context_t ctx = {
                .method = method->method,
                .err = TDN_NO_ERROR
            };
            spidir_module_build_function(module, method->function, jit_emit_builtin, &ctx);
            CHECK_AND_RETHROW(ctx.err);
        } else {
            // normal IL method
            jit_emit_context_t ctx = {
                .method = method,
                .err = TDN_NO_ERROR
            };
            spidir_module_build_function(module, method->function, jit_emit_il, &ctx);
            CHECK_AND_RETHROW(ctx.err);
        }
    }

cleanup:
    return err;
}

tdn_err_t jit_emit(spidir_module_handle_t module) {
    tdn_err_t err = TDN_NO_ERROR;

    // emit everything
    CHECK_AND_RETHROW(jit_emit_all(module));

    // TODO: code gen and optimizations and stuff

#ifdef JIT_DUMP_EMIT
    void* ctx = tdn_host_jit_start_dump();
    spidir_module_dump(module, tdn_host_jit_dump_callback, ctx);
    tdn_host_jit_end_dump(ctx);
#endif

cleanup:
    return err;
}

void jit_emit_clean(void) {
    arrfree(m_methods_to_emit);
}
