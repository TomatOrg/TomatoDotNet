#include "jit_emit.h"

#include <tomatodotnet/types/type.h>
#include <util/except.h>
#include <util/stb_ds.h>

#include "jit.h"
#include "jit_builtin.h"
#include "jit_helpers.h"
#include "jit_type.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Type helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static spidir_value_type_t get_spidir_type(RuntimeTypeInfo info) {
    info = verifier_get_intermediate_type(info);
    if (info == tInt32) {
        return SPIDIR_TYPE_I32;
    } else if (info == tInt64 || info == tIntPtr) {
        return SPIDIR_TYPE_I64;
    } else {
        return SPIDIR_TYPE_PTR;
    }
}

spidir_value_type_t jit_get_spidir_ret_type(RuntimeMethodBase method) {
    RuntimeTypeInfo type = verifier_get_intermediate_type(method->ReturnParameter->ParameterType);
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

spidir_value_type_t* jit_get_spidir_arg_types(RuntimeMethodBase method) {
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
    if (
        ret_type != tVoid &&
        jit_is_struct_like(ret_type)
    ) {
        arrpush(types, SPIDIR_TYPE_PTR);
    }

    return types;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Emit helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void jit_emit_memcpy(spidir_builder_handle_t builder, spidir_value_t dst, spidir_value_t src, RuntimeTypeInfo type) {
    // choose the best memcpy function for the job
    spidir_function_t memcpy_func;
    if (type->IsUnmanaged) {
        memcpy_func = jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_MEMCPY);
    } else {
        memcpy_func = jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_GC_MEMCPY);
    }

    // emit the copy
    spidir_builder_build_call(builder,
        memcpy_func, 3,
        (spidir_value_t[]) {
            dst, src,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, type->StackSize)
        }
    );
}

static void jit_emit_bzero(spidir_builder_handle_t builder, spidir_value_t dst, RuntimeTypeInfo type) {
    // choose the best bzero function for the job
    spidir_function_t memcpy_func;
    if (type->IsUnmanaged) {
        memcpy_func = jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_BZERO);
    } else {
        memcpy_func = jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_GC_BZERO);
    }

    // emit the zero
    spidir_builder_build_call(builder,
        memcpy_func, 2,
        (spidir_value_t[]) {
            dst, spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, type->StackSize)
        }
    );
}

static void jit_emit_store(
    spidir_builder_handle_t builder,
    RuntimeTypeInfo from_type, spidir_value_t from_value,
    RuntimeTypeInfo to_type, spidir_value_t to_value
) {
    if (from_type == NULL && to_type->Attributes.Interface) {
        // we need to convert from NULL -> interface
        jit_emit_bzero(builder, to_value, to_type);

    } else if (!jit_is_interface(from_type) && jit_is_interface(to_type)) {
        // we need to convert from object -> interface
        ASSERT(!"TODO: get the interface offset and move the vtable");

    } else if (jit_is_interface(from_type) && !jit_is_interface(to_type)) {
        // we need to perform interface -> object
        STATIC_ASSERT(offsetof(Interface, Instance) == 0);
        spidir_value_t instance = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, from_value);
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, instance, to_value);

    } else if (jit_is_interface(from_type) && jit_is_interface(to_type) && from_type != to_type) {
        // interface downcast
        ASSERT(!"TODO: get the interface offset and move the vtable");

    } else if (jit_is_struct_like(from_type)) {
        // only struct types in here and
        ASSERT(from_type == to_type);
        jit_emit_memcpy(builder, to_value, from_value, from_type);
    } else {
        // only primitive types in here
        spidir_mem_size_t mem_size;
        switch (from_type->StackSize) {
            case 1: mem_size = SPIDIR_MEM_SIZE_1; break;
            case 2: mem_size = SPIDIR_MEM_SIZE_2; break;
            case 4: mem_size = SPIDIR_MEM_SIZE_4; break;
            case 8: mem_size = SPIDIR_MEM_SIZE_8; break;
            default: ASSERT(!"Invalid size");
        }
        spidir_builder_build_store(builder, mem_size, from_value, to_value);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Merging stack values
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Convert a value, used by merge points and function calls
 * to ensure consistency
 */
static spidir_value_t jit_convert_value(
    spidir_builder_handle_t builder,
    RuntimeTypeInfo from_type, spidir_value_t from_value,
    RuntimeTypeInfo to_type
) {
    if (jit_is_interface(from_type) && jit_is_interface(to_type) && from_type != to_type) {
        // both stay as interface, but they are not the same interface
        jit_emit_store(builder, from_type, from_value, to_type, from_value);
        return from_value;

    } else if (!jit_is_interface(from_type) && jit_is_interface(to_type)) {
        // we need to turn non-interface to interface, put it in a new stackslot
        spidir_value_t to_value = spidir_builder_build_stackslot(builder,
            to_type->StackSize, to_type->StackAlignment);
        jit_emit_store(builder, from_type, from_value, to_type, to_value);
        return to_value;

    } else if (jit_is_interface(from_type) && !jit_is_interface(to_type)) {
        // we need to perform interface -> object
        STATIC_ASSERT(offsetof(Interface, Instance) == 0);
        return spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, from_value);

    } else {
        return from_value;
    }
}

static void emitter_merge_block(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* from, jit_block_t* block) {
    if (block->multiple_predecessors) {

        //
        // initialize the phis for this block
        //
        // NOTE: for spilled locals/args we don't need to actually use a phi
        //       because the stack slot is a pointer value that never changes
        //       so there will never be a need for a phi
        //

        if (!block->initialized_phis) {
            block->initialized_phis = true;

            arrsetlen(block->stack_phis, arrlen(block->stack));

            // switch to the block of the merge point
            spidir_block_t current_block;
            bool had_block = spidir_builder_cur_block(builder, &current_block);
            spidir_builder_set_block(builder, block->spidir_block);

            // create stack phis
            for (int i = 0; i < arrlen(block->stack); i++) {
                block->stack[i].value = spidir_builder_build_phi(builder,
                    get_spidir_type(block->stack[i].type),
                    0, NULL, &block->stack_phis[i]);
            }

            // create local phis
            for (int i = 0; i < arrlen(block->locals); i++) {
                if (function->locals[i].spilled) continue;
                block->locals[i].stack.value = spidir_builder_build_phi(builder,
                    get_spidir_type(block->locals[i].stack.type),
                    0, NULL, &block->locals[i].phi);
            }

            // create arg phis
            for (int i = 0; i < arrlen(block->args); i++) {
                if (function->args[i].spilled) continue;
                block->args[i].stack.value = spidir_builder_build_phi(builder,
                    get_spidir_type(block->args[i].stack.type),
                    0, NULL, &block->args[i].phi);
            }

            // switch back to the old block
            if (had_block) {
                spidir_builder_set_block(builder, current_block);
            }
        }

        // add the phi inputs
        for (int i = 0; i < arrlen(block->stack); i++) {
            spidir_builder_add_phi_input(builder, block->stack_phis[i],
                jit_convert_value(builder,
                    from->stack[i].type, from->stack[i].value,
                    block->stack[i].type));
        }

        for (int i = 0; i < arrlen(block->locals); i++) {
            if (function->locals[i].spilled) {
                block->locals[i].stack.value = from->locals[i].stack.value;
            } else {
                spidir_builder_add_phi_input(builder, block->locals[i].phi,
                    jit_convert_value(builder,
                        from->locals[i].stack.type, from->locals[i].stack.value,
                        block->locals[i].stack.type));
            }
        }

        for (int i = 0; i < arrlen(block->args); i++) {
            if (function->args[i].spilled) {
                block->args[i].stack.value = from->args[i].stack.value;
            } else {
                spidir_builder_add_phi_input(builder, block->args[i].phi,
                    jit_convert_value(builder,
                        from->args[i].stack.type, from->args[i].stack.value,
                        block->args[i].stack.type));
            }
        }

    } else {
        // just copy over the values
        for (int i = 0; i < arrlen(block->stack); i++) {
            block->stack[i].value = from->stack[i].value;
        }

        for (int i = 0; i < arrlen(block->locals); i++) {
            block->locals[i].stack.value = from->locals[i].stack.value;
        }

        for (int i = 0; i < arrlen(block->args); i++) {
            block->args[i].stack.value = from->args[i].stack.value;
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Emitters
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define STACK_TOP() \
    ({ \
        &arrlast(block->stack); \
    })

//----------------------------------------------------------------------------------------------------------------------
// Misc instructions
//----------------------------------------------------------------------------------------------------------------------

// Use as a template for adding new instructions
static tdn_err_t emit_nop(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Argument access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t emit_ldarg(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->args));
    jit_block_local_t* arg = &block->args[inst->operand.variable];

    // load it, if it was spilled then we need to create a copy, otherwise we can
    // just perform a normal data-flow load
    spidir_value_t value = SPIDIR_VALUE_INVALID;
    if (function->args[inst->operand.variable].spilled) {
        if (jit_is_struct_like(arg->stack.type)) {
            value = spidir_builder_build_stackslot(builder, arg->stack.type->StackSize, arg->stack.type->StackAlignment);
            jit_emit_memcpy(builder, value, arg->stack.value, arg->stack.type);
        } else {
            spidir_mem_size_t mem_size;
            switch (arg->stack.type->StackSize) {
                case 1: mem_size = SPIDIR_MEM_SIZE_1; break;
                case 2: mem_size = SPIDIR_MEM_SIZE_2; break;
                case 4: mem_size = SPIDIR_MEM_SIZE_4; break;
                case 8: mem_size = SPIDIR_MEM_SIZE_8; break;
                default: CHECK_FAIL();
            }
            value = spidir_builder_build_load(builder, mem_size, get_spidir_type(arg->stack.type), arg->stack.value);
        }
    } else {
        value = arg->stack.value;
    }

    STACK_TOP()->value = value;

cleanup:
    return err;
}

static tdn_err_t emit_starg(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->args));
    jit_block_local_t* arg = &block->args[inst->operand.variable];

    if (function->args[inst->operand.variable].spilled) {
        jit_emit_store(builder,
            stack[0].type, stack[0].value,
            arg->stack.type, arg->stack.value);
    } else {
        // data flow store
        // TODO: truncate values as required
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Local access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t emit_ldloc(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->locals));
    jit_block_local_t* local = &block->locals[inst->operand.variable];

    // load it, if it was spilled then we need to create a copy, otherwise we can
    // just perform a normal data-flow load
    spidir_value_t value = SPIDIR_VALUE_INVALID;
    if (function->locals[inst->operand.variable].spilled) {
        if (jit_is_struct_like(local->stack.type)) {
            value = spidir_builder_build_stackslot(builder, local->stack.type->StackSize, local->stack.type->StackAlignment);
            jit_emit_memcpy(builder, value, local->stack.value, local->stack.type);
        } else {
            spidir_mem_size_t mem_size;
            switch (local->stack.type->StackSize) {
                case 1: mem_size = SPIDIR_MEM_SIZE_1; break;
                case 2: mem_size = SPIDIR_MEM_SIZE_2; break;
                case 4: mem_size = SPIDIR_MEM_SIZE_4; break;
                case 8: mem_size = SPIDIR_MEM_SIZE_8; break;
                default: CHECK_FAIL();
            }
            value = spidir_builder_build_load(builder, mem_size, get_spidir_type(local->stack.type), local->stack.value);
        }
    } else {
        value = local->stack.value;
    }

    STACK_TOP()->value = value;

cleanup:
    return err;
}

static tdn_err_t emit_stloc(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->locals));
    jit_block_local_t* local = &block->locals[inst->operand.variable];

    if (function->locals[inst->operand.variable].spilled) {
        jit_emit_store(builder,
            stack[0].type, stack[0].value,
            local->stack.type, local->stack.value);
    } else {
        // data flow store
        // TODO: truncate values as required
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Stack manipulation
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t emit_ldc_i4(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    STACK_TOP()->value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, inst->operand.uint32);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_ldc_i8(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    STACK_TOP()->value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst->operand.uint64);
    return TDN_NO_ERROR;
}


//----------------------------------------------------------------------------------------------------------------------
// Method related
//----------------------------------------------------------------------------------------------------------------------

static size_t jit_get_interface_offset(RuntimeTypeInfo type, RuntimeTypeInfo iface) {
    int idx = hmgeti(type->InterfaceImpls, iface);
    if (idx < 0) {
        return -1;
    }
    return type->InterfaceImpls[idx].value;
}

// TODO: variant interface arguments

static RuntimeMethodBase devirt_method(jit_stack_item_t* item, RuntimeMethodBase target) {
    // not virtual, we know it exactly
    if (!target->Attributes.Virtual) {
        return target;
    }

    // has builtin emitter, we know what to call directly
    if (jit_get_builtin_emitter(target) != NULL) {
        return target;
    }

    // get the real type
    RuntimeTypeInfo cur_type = item->type;
    if (item->boxed_type != NULL) {
        cur_type = item->boxed_type;
    }

    // find the method that implements this
    if (target->DeclaringType->Attributes.Interface) {
        int offset = jit_get_interface_offset(cur_type, target->DeclaringType);
        ASSERT(offset >= 0);
        target = (RuntimeMethodBase)cur_type->VTable->Elements[offset + target->VTableOffset];
    } else {
        target = (RuntimeMethodBase)cur_type->VTable->Elements[target->VTableOffset];
    }

    // if this is a final type/method or we know that this is
    // the exact type we can devirt it
    if (cur_type->Attributes.Sealed || target->Attributes.Final || item->is_exact_type) {
        return target;
    }

    return NULL;
}

static spidir_value_t* emit_gather_arguments(spidir_builder_handle_t builder, RuntimeMethodBase callee, jit_stack_item_t* stack) {
    spidir_value_t* values = NULL;

    // check if we need to skip the this
    int convert_offset = 0;
    if (!callee->Attributes.Static) {
        convert_offset = 1;
    }

    // gather all arguments
    for (int i = 0; i < arrlen(stack); i++) {
        spidir_value_t value = stack[i].value;

        if (i > convert_offset) {
            // convert other arguments
            value = jit_convert_value(
                builder,
                stack[i].type, stack[i].value,
                callee->Parameters->Elements[i - convert_offset]->ParameterType
            );
        } else {
            // convert the this
            value = jit_convert_value(
                builder,
                stack[i].type, stack[i].value,
                jit_get_method_this_type(callee)
            );
        }

        arrpush(values, value);
    }

    // push return argument
    if (jit_is_struct_like(callee->ReturnParameter->ParameterType)) {
        arrpush(values, spidir_builder_build_stackslot(builder,
            callee->ReturnParameter->ParameterType->StackSize,
            callee->ReturnParameter->ParameterType->StackAlignment));
    }

    return values;
}

static tdn_err_t emit_call(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_t* values = NULL;

    RuntimeMethodBase callee = inst->operand.method;

    // TODO: check if we should inline, if so perform inline

    // gather all the parameters for a call
    values = emit_gather_arguments(builder, callee, stack);

    // first attempt and use a builtin emitter if available
    spidir_value_t return_value = SPIDIR_VALUE_INVALID;
    jit_builtin_emitter_t emitter = jit_get_builtin_emitter(callee);
    if (emitter != NULL) {
        return_value = emitter(builder, callee, values);
    } else {
        // lastly, get a direct function to it
        spidir_function_t function = jit_get_function(spidir_builder_get_module(builder), callee);
        return_value = spidir_builder_build_call(builder, function, arrlen(values), values);
    }

    // push it if needed
    if (callee->ReturnParameter->ParameterType != tVoid) {
        if (jit_is_struct_like(callee->ReturnParameter->ParameterType)) {
            return_value = arrlast(values);
        }
        STACK_TOP()->value = return_value;
    }

cleanup:
    arrfree(values);

    return err;
}

static tdn_err_t emit_callvirt(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_t* values = NULL;

    // attempt to de-virtualize the method, if we could then
    // replace the instruction and call the normal emit_call
    // method
    RuntimeMethodBase known = devirt_method(&stack[0], inst->operand.method);
    if (known != NULL) {
        // TODO: perform explicit null check

        // the devirt was into a valuetype, move the pointer
        // forward since this will call the non-thunk version
        if (tdn_type_is_valuetype(known->DeclaringType)) {
            stack[0].value = spidir_builder_build_ptroff(builder, stack[0].value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64,
                    jit_get_boxed_value_offset(known->DeclaringType)));
        }

        inst->operand.method = known;
        CHECK_AND_RETHROW(emit_call(function, builder, block, inst, stack));
        goto cleanup;
    }

    RuntimeMethodBase callee = inst->operand.method;
    RuntimeTypeInfo callee_this = callee->DeclaringType;

    // gather all the parameters for a call
    values = emit_gather_arguments(builder, callee, stack);

    // load the vtable of the object on the stack
    spidir_value_t vtable_base = SPIDIR_VALUE_INVALID;
    if (jit_is_interface(stack[0].type)) {
        vtable_base = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
            spidir_builder_build_ptroff(builder, stack[0].value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable))));
    } else {
        vtable_base = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, stack[0].value);
    }

    // now figure the offset of the function in the interface
    size_t base_offset = sizeof(void*) * callee->VTableOffset;
    if (jit_is_interface(callee_this) && !jit_is_interface(stack[0].type)) {
        // calling an interface method on an object, adjust the base offset to
        // represent the offset to the iface inside of the object's vtable
        size_t iface_offset = jit_get_interface_offset(stack[0].type, callee_this);
        ASSERT(iface_offset != -1);
        base_offset += iface_offset * sizeof(void*);
        base_offset += offsetof(ObjectVTable, Functions);
    }

    // and now load the function pointer
    spidir_value_t func_ptr = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
        spidir_builder_build_ptroff(builder, vtable_base,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, base_offset)));

    // perform an indirect call
    spidir_value_type_t* arg_types = jit_get_spidir_arg_types(callee);
    CHECK(arg_types != NULL);
    spidir_value_t return_value = spidir_builder_build_callind(builder,
        jit_get_spidir_ret_type(callee),
        arrlen(arg_types),
        arg_types,
        func_ptr,
        values
    );
    arrfree(arg_types);

    // push it if needed
    if (callee->ReturnParameter->ParameterType != tVoid) {
        if (jit_is_struct_like(callee->ReturnParameter->ParameterType)) {
            return_value = arrlast(values);
        }
        STACK_TOP()->value = return_value;
    }

cleanup:
    arrfree(values);

    return err;
}

// Use as a template for adding new instructions
static tdn_err_t emit_ret(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    ParameterInfo ret = function->method->ReturnParameter;

    spidir_value_t value = SPIDIR_VALUE_INVALID;
    RuntimeTypeInfo ret_type = ret->ParameterType;
    if (ret_type != tVoid) {
        // TODO: inline support

        if (jit_is_struct_like(ret_type)) {
            CHECK_FAIL("TODO: support for returning struct like");
        } else {
            value = stack[0].value;
        }
    }

    spidir_builder_build_return(builder, value);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Branching
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t emit_br(jit_function_t* verifier, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the target block
    jit_basic_block_entry_t* target_block = hmgetp_null(verifier->labels, inst->operand.branch_target);
    CHECK(target_block != NULL);
    jit_block_t* target = &verifier->blocks[target_block->value.index];

    // merge with that block
    emitter_merge_block(verifier, builder, block, target);

    // and branch to it
    spidir_builder_build_branch(builder, target->spidir_block);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Dispatch tables
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

emit_instruction_t g_emit_dispatch_table[] = {
    [CEE_NOP] = emit_nop,

    [CEE_LDARG] = emit_ldarg,
    [CEE_STARG] = emit_starg,

    [CEE_LDLOC] = emit_ldloc,
    [CEE_STLOC] = emit_stloc,

    [CEE_LDC_I4] = emit_ldc_i4,
    [CEE_LDC_I8] = emit_ldc_i8,

    [CEE_CALL] = emit_call,
    [CEE_CALLVIRT] = emit_callvirt,
    [CEE_RET] = emit_ret,

    [CEE_BR] = emit_br,
};
size_t g_emit_dispatch_table_size = ARRAY_LENGTH(g_emit_dispatch_table);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Entry points
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t emitter_on_entry_block(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;

    // create and set this as the entry block, we don't care if this will end up as
    // empty because it will just be cleaned up in the optimizer
    block->spidir_block = spidir_builder_create_block(builder);
    spidir_builder_set_block(builder, block->spidir_block);
    spidir_builder_set_entry_block(builder, block->spidir_block);

    //
    // initialize and spill the arguments
    // TODO: inline
    //
    for (int i = 0; i < arrlen(block->args); i++) {
        jit_block_local_t* arg = &block->args[i];

        // get the param for this slot
        spidir_value_t value = spidir_builder_build_param_ref(builder, i);

        // if we need to spill, spill it now
        if (function->args[i].spilled) {
            spidir_value_t stackslot = spidir_builder_build_stackslot(builder,
                arg->stack.type->StackSize, arg->stack.type->StackAlignment);

            jit_emit_store(
                builder,
                arg->stack.type, value,
                arg->stack.type, stackslot
            );
            value = stackslot;
        }

        // and store it
        arg->stack.value = value;
    }

    // initialize and spill locals as needed
    for (int i = 0; i < arrlen(block->locals); i++) {
        jit_block_local_t* local = &block->locals[i];

        bool spilled = function->locals[i].spilled;
        bool zero_initialize = function->locals[i].zero_initialize;

        spidir_value_t value = SPIDIR_VALUE_INVALID;

        // if we need to spill it, or this is a struct like that needs to be zero initialized
        // then allocate a stackslot
        if (spilled || (zero_initialize && jit_is_struct_like(local->stack.type))) {
            spidir_value_t stackslot = spidir_builder_build_stackslot(builder,
                local->stack.type->StackSize, local->stack.type->StackAlignment);

            // we need to zero init it
            if (zero_initialize) {
                jit_emit_bzero(builder, stackslot, local->stack.type);
            }
        } else if (zero_initialize) {
            // this is a value type, but we need to zero init it
            value = spidir_builder_build_iconst(builder, get_spidir_type(local->stack.type), 0);
        }

        // and store it
        local->stack.value = value;
    }

cleanup:
    return err;
}

tdn_err_t emitter_on_block_fallthrough(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* from, jit_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;

    // emit the merge with the block
    emitter_merge_block(function, builder, from, block);

    // and branch into the new block
    spidir_builder_build_branch(builder, block->spidir_block);

cleanup:
    return err;
}
