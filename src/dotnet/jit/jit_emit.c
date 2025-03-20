#include "jit_emit.h"

#include <dotnet/loader.h>
#include <tomatodotnet/types/type.h>
#include <util/except.h>
#include <util/stb_ds.h>
#include <util/string.h>

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
        switch (to_type->StackSize) {
            case 1: mem_size = SPIDIR_MEM_SIZE_1; break;
            case 2: mem_size = SPIDIR_MEM_SIZE_2; break;
            case 4: mem_size = SPIDIR_MEM_SIZE_4; break;
            case 8: mem_size = SPIDIR_MEM_SIZE_8; break;
            default: ASSERT(!"Invalid size");
        }
        spidir_builder_build_store(builder, mem_size, from_value, to_value);
    }
}

static spidir_value_t jit_emit_load(spidir_builder_handle_t builder, RuntimeTypeInfo from_type, spidir_value_t from) {
    spidir_value_t value = SPIDIR_VALUE_INVALID;
    if (jit_is_struct_like(from_type)) {
        value = spidir_builder_build_stackslot(builder, from_type->StackSize, from_type->StackAlignment);
        jit_emit_memcpy(builder, value, from, from_type);
    } else {
        spidir_mem_size_t mem_size;
        switch (from_type->StackSize) {
            case 1: mem_size = SPIDIR_MEM_SIZE_1; break;
            case 2: mem_size = SPIDIR_MEM_SIZE_2; break;
            case 4: mem_size = SPIDIR_MEM_SIZE_4; break;
            case 8: mem_size = SPIDIR_MEM_SIZE_8; break;
            default: ASSERT(!"WTF");
        }
        value = spidir_builder_build_load(builder, mem_size, get_spidir_type(from_type), from);

        // loaded from a signed integer, so sign extend it instead of the default zero extension
        if (from_type == tSByte || from_type == tInt16) {
            value = spidir_builder_build_sfill(builder, from_type->StackSize * 8, value);
        }
    }
    return value;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Exception helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void jit_emit_null_reference(spidir_builder_handle_t builder) {
    // emit a call to null reference exception
    spidir_function_t function = jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_THROW_NULL_REFERENCE);
    spidir_builder_build_call(builder, function, 0, NULL);
    spidir_builder_build_unreachable(builder);

    // create a new dummy block so we can actually continue with the emit even tho we reached an
    // explicit null pointer
    spidir_builder_set_block(builder, spidir_builder_create_block(builder));
}

static void jit_emit_invalid_cast(spidir_builder_handle_t builder) {
    // emit a call to null reference exception
    spidir_function_t function = jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_THROW_INVALID_CAST);
    spidir_builder_build_call(builder, function, 0, NULL);
    spidir_builder_build_unreachable(builder);
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

static spidir_value_t jit_convert_local(
    spidir_builder_handle_t builder,
    RuntimeTypeInfo from_type, spidir_value_t from_value,
    RuntimeTypeInfo to_type
) {
    // special case only needed for locals, truncate/sign extend as required
    if (from_type == tInt32 && from_type->StackSize != to_type->StackSize) {
        int bit_count = to_type->StackSize * 8;
        if (to_type == tSByte || to_type == tInt16) {
            from_value = spidir_builder_build_sfill(builder, bit_count, from_value);
        } else if (to_type == tByte || to_type == tUInt16 || to_type == tBoolean || to_type == tChar) {
            from_value = spidir_builder_build_and(builder, from_value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, (1 << bit_count) - 1));
        } else {
            TRACE("%T -> %T", from_type, to_type);
            ASSERT(!"Invalid");
        }
        return from_value;

    }

    // fallback to the normal convert logic
    return jit_convert_value(builder, from_type, from_value, to_type);
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
                if (function->locals[i].spilled) {
                    block->locals[i].stack.value = from->locals[i].stack.value;
                } else {
                    block->locals[i].stack.value = spidir_builder_build_phi(builder,
                        get_spidir_type(block->locals[i].stack.type),
                        0, NULL, &block->locals[i].phi);
                }
            }

            // create arg phis
            for (int i = 0; i < arrlen(block->args); i++) {
                if (function->args[i].spilled) {
                    block->args[i].stack.value = from->args[i].stack.value;
                } else {
                    block->args[i].stack.value = spidir_builder_build_phi(builder,
                        get_spidir_type(block->args[i].stack.type),
                        0, NULL, &block->args[i].phi);
                }
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

            } else if (block->locals[i].initialized || function->locals[i].zero_initialize) {
                // we only need to add the phi input if the value wants to be initialized
                // in the next block, or if its zero initialized
                spidir_builder_add_phi_input(builder, block->locals[i].phi,
                    jit_convert_value(builder,
                        from->locals[i].stack.type, from->locals[i].stack.value,
                        block->locals[i].stack.type));
            } else {
                // keep as invalid just in case
                block->locals[i].stack.value = SPIDIR_VALUE_INVALID;
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
        value = jit_emit_load(builder, arg->stack.type, arg->stack.value);
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
        arg->stack.value = jit_convert_local(builder, stack[0].type, stack[0].value, arg->stack.type);;
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
        value = jit_emit_load(builder, local->stack.type, local->stack.value);
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
        // data flow store, convert into the type that it should be
        local->stack.value = jit_convert_local(builder, stack[0].type, stack[0].value, local->stack.type);
    }

cleanup:
    return err;
}

static tdn_err_t emit_ldloca(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    CHECK(inst->operand.variable < arrlen(block->locals));
    jit_block_local_t* local = &block->locals[inst->operand.variable];

    // must be spilled
    CHECK(function->locals[inst->operand.variable].spilled);

    // just give the pointer
    STACK_TOP()->value = local->stack.value;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Misc instructions
//----------------------------------------------------------------------------------------------------------------------

static spidir_value_t jit_get_static_field(spidir_builder_handle_t builder, RuntimeFieldInfo field) {
    ASSERT(field->Attributes.Static);

    // TODO: use an external reference instead eventually

    // allocate it if required
    if (field->JitFieldPtr == NULL) {
        field->JitFieldPtr = tdn_host_mallocz(field->FieldType->StackSize, field->FieldType->StackAlignment);
        ASSERT(field->JitFieldPtr != NULL);

        // TODO: register gc roots
    }

    return spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uintptr_t)field->JitFieldPtr);
}

// Use as a template for adding new instructions
static tdn_err_t emit_ldfld(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the field offset
    spidir_value_t field_ptr = SPIDIR_VALUE_INVALID;

    if (inst->operand.field->Attributes.Static) {
        field_ptr = jit_get_static_field(builder, inst->operand.field);
    } else {
        // add the offset to the field and access it
        field_ptr = spidir_builder_build_ptroff(builder, stack[0].value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst->operand.field->FieldOffset));
    }

    // perform the load
    STACK_TOP()->value = jit_emit_load(builder, inst->operand.field->FieldType, field_ptr);


cleanup:
    return err;
}

// Use as a template for adding new instructions
static tdn_err_t emit_stfld(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the field offset
    spidir_value_t field_ptr = SPIDIR_VALUE_INVALID;

    if (inst->operand.field->Attributes.Static) {
        field_ptr = jit_get_static_field(builder, inst->operand.field);
    } else {
        // add the offset to the field and access it
        field_ptr = spidir_builder_build_ptroff(builder, stack[0].value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst->operand.field->FieldOffset));
    }

    // perform the store
    jit_emit_store(
        builder,
        stack[1].type, stack[1].value,
        inst->operand.field->FieldType,
        field_ptr
    );

cleanup:
    return err;
}

// Use as a template for adding new instructions
static tdn_err_t emit_ldsfld(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    STACK_TOP()->value = jit_emit_load(
        builder,
        inst->operand.field->FieldType,
        jit_get_static_field(builder, inst->operand.field)
    );

cleanup:
    return err;
}
// Use as a template for adding new instructions
static tdn_err_t emit_stsfld(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_emit_store(
        builder,
        stack[0].type, stack[0].value,
        inst->operand.field->FieldType,
        jit_get_static_field(builder, inst->operand.field)
    );

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Indirect reference access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t emit_ldind(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    STACK_TOP()->value = jit_emit_load(builder, inst->operand.type, stack[0].value);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_stind(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    // we just need to store into the given item
    jit_emit_store(builder, stack[1].type, stack[1].value, stack[0].type->ElementType, stack[0].value);
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// Stack manipulation
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t emit_ldnull(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    STACK_TOP()->value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_ldstr(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    // TODO: pin the string
    STACK_TOP()->value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uintptr_t)inst->operand.string);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_ldc_i4(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    STACK_TOP()->value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, inst->operand.uint32);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_ldc_i8(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    STACK_TOP()->value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst->operand.uint64);
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// Arith and compare operations
//----------------------------------------------------------------------------------------------------------------------

#define SWAP(a, b) \
    do { \
        typeof(a) __temp = a; \
        a = b; \
        b = __temp; \
    } while (0)

static spidir_value_t emit_extend_int(spidir_builder_handle_t builder, spidir_value_t value, bool sign_extend) {
    value = spidir_builder_build_iext(builder, value);
    if (sign_extend) {
        value = spidir_builder_build_sfill(builder, 32, value);
    } else {
        value = spidir_builder_build_and(builder, value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 0xFFFFFFFF));
    }
    return value;
}

static spidir_value_t emit_binary_compare(spidir_builder_handle_t builder, tdn_il_opcode_t opcode, spidir_value_t value1, spidir_value_t value2) {
    spidir_icmp_kind_t kind;
    switch (opcode) {
        case CEE_BEQ:
        case CEE_CEQ: kind = SPIDIR_ICMP_EQ; break;
        case CEE_BNE_UN: kind = SPIDIR_ICMP_NE; break;
        case CEE_BGT:
        case CEE_CGT: kind = SPIDIR_ICMP_SLT; SWAP(value1, value2); break;
        case CEE_BGE: kind = SPIDIR_ICMP_SLE; SWAP(value1, value2); break;
        case CEE_BGT_UN:
        case CEE_CGT_UN: kind = SPIDIR_ICMP_ULT; SWAP(value1, value2); break;
        case CEE_BGE_UN: kind = SPIDIR_ICMP_ULE; SWAP(value1, value2); break;
        case CEE_BLT:
        case CEE_CLT: kind = SPIDIR_ICMP_SLT; break;
        case CEE_BLE: kind = SPIDIR_ICMP_SLE; break;
        case CEE_BLT_UN:
        case CEE_CLT_UN: kind = SPIDIR_ICMP_ULT; break;
        case CEE_BLE_UN: kind = SPIDIR_ICMP_ULE; break;
        default: ASSERT(!"Invalid opcode");
    }
    return spidir_builder_build_icmp(builder, kind, SPIDIR_TYPE_I32, value1, value2);
}

static tdn_err_t emit_compare(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    bool is_unsigned = inst->opcode == CEE_CGT_UN || inst->opcode == CEE_CLT_UN;

    // sign extend as required
    if (stack[0].type == tInt32 && stack[1].type != tInt32) {
        stack[0].type = tIntPtr;
        stack[0].value = emit_extend_int(builder, stack[0].value, !is_unsigned);
    } else if (stack[1].type == tInt32 && stack[0].type != tInt32) {
        stack[1].type = tIntPtr;
        stack[1].value = emit_extend_int(builder, stack[1].value, !is_unsigned);
    }

    STACK_TOP()->value = emit_binary_compare(builder, inst->opcode, stack[0].value, stack[1].value);

    return TDN_NO_ERROR;
}

static tdn_err_t emit_conv_i4(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    spidir_value_t value = stack[0].value;

    // truncate the value if too big
    if (stack[0].type == tInt64 || stack[0].type == tIntPtr) {
        value = spidir_builder_build_itrunc(builder, value);
    }

    // for anything less than i32 we need to sign/zero extend
    // from the given type, for the i32 version we don't need
    // to do anything
    if (inst->opcode == CEE_CONV_I1) {
        value = spidir_builder_build_sfill(builder, 8, value);
    } else if (inst->opcode == CEE_CONV_I2) {
        value = spidir_builder_build_sfill(builder, 16, value);
    } else if (inst->opcode == CEE_CONV_U1) {
        value = spidir_builder_build_and(builder, value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 0xFF));
    } else if (inst->opcode == CEE_CONV_U2) {
        value = spidir_builder_build_and(builder, value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 0xFFFF));
    }

    STACK_TOP()->value = value;

    return TDN_NO_ERROR;
}

static tdn_err_t emit_conv_i8(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    spidir_value_t value = stack[0].value;

    // extend it to a 32bit value if too small
    if (stack[0].type == tInt32) {
        value = emit_extend_int(builder, stack[0].value,
            inst->opcode == CEE_CONV_I8 || inst->opcode == CEE_CONV_I);
    }

    STACK_TOP()->value = value;

    return TDN_NO_ERROR;
}

static tdn_err_t emit_binary_op(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    bool is_unsigned = inst->opcode == CEE_DIV_UN || inst->opcode == CEE_REM_UN;

    // sign extend as required
    if (stack[0].type == tInt32 && stack[1].type != tInt32) {
        stack[0].type = tIntPtr;
        stack[0].value = emit_extend_int(builder, stack[0].value, !is_unsigned);
    } else if (stack[1].type == tInt32 && stack[0].type != tInt32) {
        stack[1].type = tIntPtr;
        stack[1].value = emit_extend_int(builder, stack[1].value, !is_unsigned);
    }

    spidir_value_t value = SPIDIR_VALUE_INVALID;
    switch (inst->opcode) {
        case CEE_ADD: value = spidir_builder_build_iadd(builder, stack[0].value, stack[1].value); break;
        case CEE_SUB: value = spidir_builder_build_isub(builder, stack[0].value, stack[1].value); break;
        case CEE_MUL: value = spidir_builder_build_imul(builder, stack[0].value, stack[1].value); break;
        case CEE_DIV: value = spidir_builder_build_sdiv(builder, stack[0].value, stack[1].value); break;
        case CEE_DIV_UN: value = spidir_builder_build_udiv(builder, stack[0].value, stack[1].value); break;
        case CEE_REM: value = spidir_builder_build_srem(builder, stack[0].value, stack[1].value); break;
        case CEE_REM_UN: value = spidir_builder_build_urem(builder, stack[0].value, stack[1].value); break;
        case CEE_AND: value = spidir_builder_build_and(builder, stack[0].value, stack[1].value); break;
        case CEE_OR: value = spidir_builder_build_or(builder, stack[0].value, stack[1].value); break;
        case CEE_XOR: value = spidir_builder_build_xor(builder, stack[0].value, stack[1].value); break;
        default: CHECK_FAIL();
    }
    STACK_TOP()->value = value;

cleanup:
    return err;
}

static tdn_err_t emit_neg(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    // emulate neg via `0 - value`
    spidir_value_t zero = spidir_builder_build_iconst(builder, get_spidir_type(stack[0].type), 0);
    STACK_TOP()->value = spidir_builder_build_isub(builder, zero, stack[0].value);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_not(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    // emulate neg via `value ^ -1`
    spidir_value_t ones;
    if (stack[0].type == tInt32) {
        ones = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 0xFFFFFFFF);
    } else {
        ones = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 0xFFFFFFFFFFFFFFFF);
    }
    STACK_TOP()->value = spidir_builder_build_xor(builder, ones, stack[0].value);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_shift(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    spidir_value_t value = SPIDIR_VALUE_INVALID;
    switch (inst->opcode) {
        case CEE_SHL: value = spidir_builder_build_shl(builder, stack[0].value, stack[1].value); break;
        case CEE_SHR: value = spidir_builder_build_ashr(builder, stack[0].value, stack[1].value); break;
        case CEE_SHR_UN: value = spidir_builder_build_lshr(builder, stack[0].value, stack[1].value); break;
        default: CHECK_FAIL();
    }
    STACK_TOP()->value = value;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Array related
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t emit_newarr(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // sign extend an int32 for it to work
    if (stack[0].type == tInt32) {
        stack[0].value = spidir_builder_build_iext(builder, stack[0].value);
        stack[0].value = spidir_builder_build_sfill(builder, 32, stack[0].value);
    }

    // call the newarr helper
    spidir_value_t obj = spidir_builder_build_call(builder,
        jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_NEWARR),
        2, (spidir_value_t[]){
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uintptr_t)STACK_TOP()->type),
            stack[0].value
        });

    // store the length, done in the IR to allow the jit
    // to see the length nicely
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_4, stack[0].value,
        spidir_builder_build_ptroff(builder, obj,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64,
                offsetof(struct Array, Length))));

    // output the array
    STACK_TOP()->value = obj;

cleanup:
    return err;
}

static tdn_err_t emit_ldlen(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the length
    spidir_value_t length = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64,
        spidir_builder_build_ptroff(builder, stack[0].value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(struct Array, Length))));

    // output the array
    STACK_TOP()->value = length;

cleanup:
    return err;
}

static spidir_value_t emit_array_offset(spidir_builder_handle_t builder, RuntimeTypeInfo type, spidir_value_t array, spidir_value_t index) {
    spidir_block_t success = spidir_builder_create_block(builder);
    spidir_block_t out_of_range = spidir_builder_create_block(builder);

    // load the length, this performs a zero extension, which is fine since the array
    // length can never be negative
    spidir_value_t length = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64,
        spidir_builder_build_ptroff(builder, array,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(struct Array, Length))));

    // check the length is in bounds, use unsigned check
    spidir_value_t cond = spidir_builder_build_icmp(builder, SPIDIR_ICMP_ULT, SPIDIR_TYPE_I32, index, length);
    spidir_builder_build_brcond(builder, cond, success, out_of_range);

    // throw an out of range exception
    spidir_builder_set_block(builder, out_of_range);
    spidir_builder_build_call(builder, jit_get_helper(
        spidir_builder_get_module(builder), JIT_HELPER_THROW_INDEX_OUT_OF_RANGE), 0, NULL);
    spidir_builder_build_unreachable(builder);

    // continue normally
    spidir_builder_set_block(builder, success);

    // get the offset
    spidir_value_t offset = spidir_builder_build_iadd(builder,
        spidir_builder_build_imul(builder, index,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, type->StackSize)),
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, jit_get_array_elements_offset(type)));
    return spidir_builder_build_ptroff(builder, array, offset);
}

static tdn_err_t emit_ldelem(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // array is null, fail
    if (stack[0].type == NULL) {
        jit_emit_null_reference(builder);
        goto cleanup;
    }

    // sign extend the index
    if (stack[1].type == tInt32) {
        stack[1].value = emit_extend_int(builder, stack[1].value, true);
    }

    // get the value and store into it
    spidir_value_t ptr = emit_array_offset(builder, stack[0].type->ElementType, stack[0].value, stack[1].value);
    STACK_TOP()->value = jit_emit_load(builder, stack[0].type->ElementType, ptr);

cleanup:
    return err;
}

static tdn_err_t emit_stelem(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // array is null, fail
    if (stack[0].type == NULL) {
        jit_emit_null_reference(builder);
        goto cleanup;
    }

    // sign extend the index
    if (stack[1].type == tInt32) {
        stack[1].value = emit_extend_int(builder, stack[1].value, true);
    }

    // get the value and store into it
    spidir_value_t ptr = emit_array_offset(builder, stack[0].type->ElementType, stack[0].value, stack[1].value);
    jit_emit_store(builder, stack[2].type, stack[2].value, stack[0].type->ElementType, ptr);

cleanup:
    return err;
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

    // if this is a delegate with a known instance call then
    // we can perform a direct call
    if (jit_is_delegate(item->type) && item->method != NULL) {
        return item->method;
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
    if (cur_type->Attributes.Sealed || target->Attributes.Final || item->is_exact_type || item->type->IsArray) {
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
    RuntimeTypeInfo ret_type = callee->ReturnParameter->ParameterType;
    if (ret_type != tVoid && jit_is_struct_like(ret_type)) {
        arrpush(values, spidir_builder_build_stackslot(builder, ret_type->StackSize, ret_type->StackAlignment));
    }

    return values;
}

// these constants are inspired by openjdk
#define INLINE_SMALL_CODE       1000    /* native function size */
#define MAX_INLINE_SIZE         35      /* il bytecode size */
#define MAX_TRIVIAL_SIZE        6       /* il bytecode size */
#define MAX_INLINE_LEVEL        15      /* the max nesting of inline we allow */

static bool jit_should_inline(jit_function_t* caller, RuntimeMethodBase callee) {
    // can't inline something without a body
    if (callee->MethodBody == NULL) {
        return false;
    }

    // inline is requested
    if (callee->MethodImplFlags.AggressiveInlining) {
        return true;
    }

    // TODO: for now assume a method is cold, we might want some logic
    //       to check for hot methods

    // check if already compiled into a medium method
    if (callee->MethodPtr != NULL && callee->MethodSize > (INLINE_SMALL_CODE / 4)) {
        return false;
    }

    // method too big for inline
    if (callee->MethodBody->ILSize > MAX_INLINE_SIZE) {
        return false;
    }

    return true;
}

static bool jit_should_not_inline(jit_function_t* caller, RuntimeMethodBase callee) {
    // don't inline if marked as no inline
    if (callee->MethodImplFlags.NoInlining) {
        return true;
    }

    // don't allow to inline too much
    if (caller->inline_depth + 1 > MAX_INLINE_LEVEL) {
        return true;
    }

    // don't allow recursion
    if (caller->method == callee) {
        return true;
    }

    // requested inline, ignore the rest of the logic
    if (callee->MethodImplFlags.AggressiveInlining) {
        return false;
    }

    // don't inline big methods if they are already compiled
    if (callee->MethodPtr != NULL && callee->MethodSize > INLINE_SMALL_CODE) {
        return true;
    }

    // small methods should always get inlined
    if (callee->MethodBody->ILSize <= MAX_TRIVIAL_SIZE) {
        return false;
    }

    // TODO: something else?

    return false;
}

static tdn_err_t emit_ldftn(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeMethodBase callee = inst->operand.method;

    // get the spidir function
    spidir_function_t func;
    if (callee->Attributes.Static) {
        func = jit_generate_static_delegate_thunk(spidir_builder_get_module(builder), callee);
    } else {
        func = jit_get_function(spidir_builder_get_module(builder), callee);
    }

    // and load it as a function pointer
    STACK_TOP()->value = spidir_builder_build_funcaddr(builder, func);

cleanup:
    return err;
}

static tdn_err_t emit_call(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_t* values = NULL;
    jit_function_t inlinee = {};

    RuntimeMethodBase callee = inst->operand.method;
    jit_builtin_emitter_t emitter = jit_get_builtin_emitter(callee);

    // check if we should inline, special case for builtin emitters since they have a special inline semantic
    if (emitter == NULL && jit_should_inline(function, callee) && !jit_should_not_inline(function, callee)) {
        // initialize the function
        CHECK_AND_RETHROW(jit_function_init(&inlinee, callee));

        // create the entry block and jump into it
        inlinee.entry_block.spidir_block = spidir_builder_create_block(builder);
        spidir_builder_build_branch(builder, inlinee.entry_block.spidir_block);

        // pass the arguments and known type info
        for (int i = 0; i < arrlen(inlinee.entry_block.args); i++) {
            // copy the relevant information
            inlinee.entry_block.args[i].stack.value = stack[i].value;
            inlinee.entry_block.args[i].stack.type = stack[i].type;
            inlinee.entry_block.args[i].stack.boxed_type = stack[i].boxed_type;
            inlinee.entry_block.args[i].stack.is_exact_type = stack[i].is_exact_type;
        }

        // setup the inline information
        inlinee.inline_depth = function->inline_depth + 1;
        inlinee.return_block = spidir_builder_create_block(builder);
        spidir_builder_set_block(builder, inlinee.return_block);

        // prepare the return phi if needed
        spidir_value_t return_value = SPIDIR_VALUE_INVALID;
        if (callee->ReturnParameter->ParameterType != tVoid) {
            return_value = spidir_builder_build_phi(builder,
                get_spidir_type(callee->ReturnParameter->ParameterType),
                0, NULL, &inlinee.return_phi);
        }

        // now let it cook
        CHECK_AND_RETHROW(jit_function(&inlinee, builder));

        // and set the return value, unline a normal call, struct likes are
        // returned directly instead of by-reference
        if (callee->ReturnParameter->ParameterType != tVoid) {
            // propagate the new type info we have about the return
            // we will not fully propagate it but just enough so we can
            // have a handful of more optimizations
            *STACK_TOP() = inlinee.return_item;
            STACK_TOP()->value = return_value;
        }

        // continue at the return block
        spidir_builder_set_block(builder, inlinee.return_block);
    } else {
        // gather all the parameters for a call
        values = emit_gather_arguments(builder, callee, stack);

        // first attempt and use a builtin emitter if available
        spidir_value_t return_value = SPIDIR_VALUE_INVALID;
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
    }

cleanup:
    arrfree(values);
    jit_function_destroy(&inlinee);

    return err;
}

// Use as a template for adding new instructions
static tdn_err_t emit_newobj(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_stack_item_t* args = NULL;
    RuntimeMethodBase callee = inst->operand.method;

    // allocate the item
    spidir_value_t obj = SPIDIR_VALUE_INVALID;
    if (jit_is_struct_like(callee->DeclaringType)) {
        obj = spidir_builder_build_stackslot(builder,
            callee->DeclaringType->StackSize, callee->DeclaringType->StackAlignment);
        jit_emit_bzero(builder, obj, callee->DeclaringType);
    } else {
        // we initialize the type, ensure we have the full vtable available
        jit_queue_type(spidir_builder_get_module(builder), inst->operand.type);

        obj = spidir_builder_build_call(builder,
            jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_NEWOBJ), 1,
            (spidir_value_t[]){ spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uintptr_t)callee->DeclaringType) });
    }

    // build a new stack args, including the `this` that we just created
    arrsetlen(args, arrlen(stack) + 1);
    memcpy(args + 1, stack, arrlen(stack) * sizeof(*stack));
    args[0].is_exact_type = true;
    args[0].type = jit_get_method_this_type(callee);
    args[0].value = obj;

    // perform a normal call instead
    CHECK_AND_RETHROW(emit_call(function, builder, block, inst, args));

    // output the object to the top of the stack
    STACK_TOP()->value = obj;

cleanup:
    arrfree(args);

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

        // the devirt was from a delegate, load the this pointer properly
        if (jit_is_delegate(stack[0].type)) {
            if (known->Attributes.Static) {
                // no need for the `this`
                stack = &stack[1];
            } else {
                // load the this from the delegate
                stack[0].type = jit_get_method_this_type(known);
                stack[0].value = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
                            spidir_builder_build_ptroff(builder, stack[0].value,
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Instance))));
            }
        }

        // the devirt was into a valuetype, move the pointer
        // forward since this will call the non-thunk version
        if (tdn_type_is_valuetype(known->DeclaringType)) {
            stack[0].value = spidir_builder_build_ptroff(builder, stack[0].value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64,
                    jit_get_boxed_value_offset(known->DeclaringType)));
        }

        // and now we can perform the direct call
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

    RuntimeTypeInfo ret_type = ret->ParameterType;

    if (function->inline_depth) {
        // for inline we need to add a phi input and jump into the return block
        if (ret_type != tVoid) {
            spidir_builder_add_phi_input(builder, function->return_phi, stack[0].value);
        }
        spidir_builder_build_branch(builder, function->return_block);
    } else {
        // resolve the return value, for struct likes we need a full on copy
        spidir_value_t value = SPIDIR_VALUE_INVALID;
        if (ret_type != tVoid) {
            if (jit_is_struct_like(ret_type)) {
                // use a store to ensure we do any conversions correctly
                spidir_value_t ret_ref = spidir_builder_build_param_ref(builder, arrlen(function->args));
                jit_emit_store(builder, stack[0].type, stack[0].value, ret_type, ret_ref);
            } else {
                value = stack[0].value;
            }
        }

        spidir_builder_build_return(builder, value);
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Boxing
//----------------------------------------------------------------------------------------------------------------------

static spidir_value_t jit_emit_type_check(spidir_builder_handle_t builder, spidir_value_t obj, RuntimeTypeInfo target) {
    spidir_value_t vtable = SPIDIR_VALUE_INVALID;

    // load the vtable pointer from the object
    vtable = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, obj);

    // and now check
    if (jit_is_interface(target)) {
        // checking against an interface

        // load the interface product
        spidir_value_t product = spidir_builder_build_load(
            builder,
            SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_I64,
            spidir_builder_build_ptroff(
                builder,
                vtable,
                spidir_builder_build_iconst(
                    builder,
                    SPIDIR_TYPE_I64,
                    offsetof(ObjectVTable, InterfaceProduct)
                )
            )
        );

        // if this is the first time someone is doing type checking
        // against this interface, generate the prime for it, this recudes
        // the amount of primes needed to only those used by the type checking
        if (target->InterfacePrime == 0) {
            ASSERT(!IS_ERROR(tdn_generate_interface_prime(target)));
        }

        // and now check that the prime is dividable by the interface product, if it is (and the reminder
        // is zero) then we know that the type implements the interface, otherwise it does not implement it
        //  (obj->product % target->prime) == 0
        return spidir_builder_build_icmp(
            builder,
            SPIDIR_ICMP_EQ,
            SPIDIR_TYPE_I32,
            spidir_builder_build_urem(
                builder,
                product,
                spidir_builder_build_iconst(
                    builder,
                    SPIDIR_TYPE_I64,
                    target->InterfacePrime
                )
            ),
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 0)
        );
    } else if (tdn_type_is_valuetype(target)) {
        // checking against a boxed value type, just compare the vtables
        // themselves
        return spidir_builder_build_icmp(
            builder,
            SPIDIR_ICMP_EQ,
            SPIDIR_TYPE_I32,
            vtable,
            spidir_builder_build_iconst(
                builder,
                SPIDIR_TYPE_PTR,
                (uintptr_t)target->JitVTable
            )
        );

    } else {
        // checking against a normal class

        // load the type hierarchy
        spidir_value_t hierarchy = spidir_builder_build_load(
            builder,
            SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_I64,
            spidir_builder_build_ptroff(
                builder,
                vtable,
                spidir_builder_build_iconst(
                    builder,
                    SPIDIR_TYPE_I64,
                    offsetof(ObjectVTable, TypeHierarchy)
                )
            )
        );

        // the expected hierarchy
        spidir_value_t expected_hierarchy = spidir_builder_build_iconst(
            builder,
            SPIDIR_TYPE_I64,
            target->JitVTable->TypeHierarchy
        );

        // build the mask based on the target type
        spidir_value_t type_mask = spidir_builder_build_iconst(
            builder,
            SPIDIR_TYPE_I64,
            (1ull << target->TypeMaskLength) - 1ull
        );

        // and now check that the bits at the start of the hierarchy are the same
        // as the target type
        //  (obj->type_hierarchy & target->type_mask) == target->type_hierarchy
        return spidir_builder_build_icmp(
            builder,
            SPIDIR_ICMP_EQ,
            SPIDIR_TYPE_I32,
            spidir_builder_build_and(
                builder,
                hierarchy,
                type_mask
            ),
            expected_hierarchy
        );
    }
}

static tdn_err_t emit_box(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    if (tdn_type_is_nullable(inst->operand.type)) {
        // boxes the value instance
        // TODO: support this properly
        CHECK_FAIL();

    } else if (tdn_type_is_referencetype(inst->operand.type)) {
        // just keep the exact same value when its a reference type
        STACK_TOP()->value = stack[0].value;

    } else if (tdn_type_is_valuetype(inst->operand.type)) {
        // because we box the type we need an initialized vtable
        jit_queue_type(spidir_builder_get_module(builder), inst->operand.type);

        // set the type as a boxed one
        spidir_value_t obj = spidir_builder_build_call(builder,
            jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_NEWOBJ), 1,
            (spidir_value_t[]){ spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uintptr_t)inst->operand.type) });

        // get the offset to the value
        spidir_value_t value_ptr = spidir_builder_build_ptroff(builder, obj,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, jit_get_boxed_value_offset(inst->operand.type)));

        // store it
        jit_emit_store(builder, stack[0].type, stack[0].value, inst->operand.type, value_ptr);

        // point to the instance
        STACK_TOP()->value = obj;

    } else {
        CHECK_FAIL();
    }

cleanup:
    return err;
}

static tdn_err_t emit_unbox_any(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // the operand is a reference type
    if (tdn_type_is_referencetype(inst->operand.type)) {
        // TODO: perform castclass instead
        CHECK_FAIL();

    } else if (tdn_type_is_nullable(inst->operand.type)) {
        // we either get a null nullable, or a value nullable
        CHECK_FAIL();

    } else if (stack[0].type == NULL) {
        // we have a hard-coded null value, throw a null-reference
        jit_emit_null_reference(builder);

    } else {
        // get the object instance
        spidir_value_t obj = stack[0].value;
        if (jit_is_interface(stack[0].type)) {
            ASSERT(offsetof(Interface, Instance) == 0);
            obj = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, obj);
        }

        // normal logic, perform the type check, this will fail on
        // a null reference on its own
        spidir_value_t cond_result = jit_emit_type_check(builder, stack[0].value, inst->operand.type);
        spidir_block_t success = spidir_builder_create_block(builder);
        spidir_block_t invalid_cast = spidir_builder_create_block(builder);
        spidir_builder_build_brcond(builder, cond_result, success, invalid_cast);

        // if we failed then throw an invalid cast
        spidir_builder_set_block(builder, invalid_cast);
        jit_emit_invalid_cast(builder);

        // the value is correct, load it
        spidir_builder_set_block(builder, success);

        // get the pointer to the value and load it
        spidir_value_t ptr = spidir_builder_build_ptroff(builder, obj,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, jit_get_boxed_value_offset(inst->operand.type)));
        STACK_TOP()->value = jit_emit_load(builder, inst->operand.type, ptr);
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Branching
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t emit_br(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);

    // merge with that block
    emitter_merge_block(function, builder, block, target);

    // and branch to it
    spidir_builder_build_branch(builder, target->spidir_block);

cleanup:
    return err;
}

static void emit_load_reference(spidir_builder_handle_t builder, jit_stack_item_t* item) {
    if (jit_is_interface(item->type)) {
        STATIC_ASSERT(offsetof(Interface, Instance) == 0);
        item->value = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, item->value);
    } else if (jit_is_delegate(item->type)) {
        item->value = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
            spidir_builder_build_ptroff(builder, item->value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Function))));
    }
}

static tdn_err_t emit_br_unary_cond(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);
    jit_block_t* next = jit_function_get_block(function, block->end, block->leave_target_stack);
    CHECK(next != NULL);

    // merge with both blocks
    emitter_merge_block(function, builder, block, target);
    emitter_merge_block(function, builder, block, next);

    // interface and delegate are a fat pointer, we need to
    // load the instance/function and check against that
    emit_load_reference(builder, &stack[0]);

    // if this is a reference need to turn into an integer
    // TODO: use a compare instead?
    if (tdn_type_is_referencetype(stack[0].type)) {
        stack[0].value = spidir_builder_build_ptrtoint(builder, stack[0].value);
    }

    // use the same condition but swap args
    if (inst->opcode == CEE_BRFALSE) {
        SWAP(target, next);
    }

    // and branch to it
    spidir_builder_build_brcond(builder, stack[0].value, target->spidir_block, next->spidir_block);

cleanup:
    return err;
}



static tdn_err_t emit_br_binary_cond(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    bool is_unsigned = inst->opcode == CEE_CGT_UN || inst->opcode == CEE_CLT_UN;

    // sign extend as required
    if (stack[0].type == tInt32 && stack[1].type != tInt32) {
        stack[0].type = tIntPtr;
        stack[0].value = emit_extend_int(builder, stack[0].value, !is_unsigned);
    } else if (stack[1].type == tInt32 && stack[0].type != tInt32) {
        stack[1].type = tIntPtr;
        stack[1].value = emit_extend_int(builder, stack[1].value, !is_unsigned);
    }

    // interface and delegate are a fat pointer, we need to
    // load the instance/function and check against that
    emit_load_reference(builder, &stack[0]);
    emit_load_reference(builder, &stack[1]);

    // NOTE: in here we don't need to inttoptr since compare can
    //       just take in a pointer without a problem

    // perform the condition
    spidir_value_t cond = emit_binary_compare(builder, inst->opcode, stack[0].value, stack[1].value);

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);
    jit_block_t* next = jit_function_get_block(function, block->end, block->leave_target_stack);
    CHECK(next != NULL);

    // merge with both blocks
    emitter_merge_block(function, builder, block, target);
    emitter_merge_block(function, builder, block, next);

    // and branch to it
    spidir_builder_build_brcond(builder, cond, target->spidir_block, next->spidir_block);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Exceptions
//----------------------------------------------------------------------------------------------------------------------

//
// Both leave and endfinally act as a branch from the emitter viewpoint, and the verifier
// already fixes the instruction to have the correct jump target
//

static tdn_err_t emit_leave(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_AND_RETHROW(emit_br(function, builder, block, inst, stack));

cleanup:
    return err;
}

static tdn_err_t emit_endfinally(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_AND_RETHROW(emit_br(function, builder, block, inst, stack));

cleanup:
    return err;
}

static tdn_err_t emit_throw(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_item_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // call the runtime throw function
    spidir_function_t func = jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_THROW);
    spidir_builder_build_call(builder, func, 1, (spidir_value_t[]){ stack[0].value });

    spidir_builder_build_unreachable(builder);

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
    [CEE_LDLOCA] = emit_ldloca,

    [CEE_LDFLD] = emit_ldfld,
    [CEE_STFLD] = emit_stfld,
    [CEE_LDSFLD] = emit_ldsfld,
    [CEE_STSFLD] = emit_stsfld,

    [CEE_LDIND_I1] = emit_ldind,
    [CEE_LDIND_U1] = emit_ldind,
    [CEE_LDIND_I2] = emit_ldind,
    [CEE_LDIND_U2] = emit_ldind,
    [CEE_LDIND_I4] = emit_ldind,
    [CEE_LDIND_U4] = emit_ldind,
    [CEE_LDIND_I8] = emit_ldind,
    [CEE_LDIND_I] = emit_ldind,
    [CEE_LDIND_REF] = emit_ldind,
    [CEE_LDOBJ] = emit_ldind,

    [CEE_STIND_I] = emit_stind,
    [CEE_STIND_I1] = emit_stind,
    [CEE_STIND_I2] = emit_stind,
    [CEE_STIND_I4] = emit_stind,
    [CEE_STIND_REF] = emit_stind,
    [CEE_STOBJ] = emit_stind,

    [CEE_LDNULL] = emit_ldnull,
    [CEE_LDSTR] = emit_ldstr,
    [CEE_LDC_I4] = emit_ldc_i4,
    [CEE_LDC_I8] = emit_ldc_i8,
    [CEE_DUP] = emit_nop,
    [CEE_POP] = emit_nop,

    [CEE_ADD] = emit_binary_op,
    [CEE_SUB] = emit_binary_op,
    [CEE_MUL] = emit_binary_op,
    [CEE_DIV] = emit_binary_op,
    [CEE_DIV_UN] = emit_binary_op,
    [CEE_REM] = emit_binary_op,
    [CEE_REM_UN] = emit_binary_op,
    [CEE_AND] = emit_binary_op,
    [CEE_OR] = emit_binary_op,
    [CEE_XOR] = emit_binary_op,

    [CEE_NEG] = emit_neg,
    [CEE_NOT] = emit_not,

    [CEE_SHL] = emit_shift,
    [CEE_SHR] = emit_shift,
    [CEE_SHR_UN] = emit_shift,

    [CEE_CONV_I1] = emit_conv_i4,
    [CEE_CONV_I2] = emit_conv_i4,
    [CEE_CONV_I4] = emit_conv_i4,
    [CEE_CONV_U1] = emit_conv_i4,
    [CEE_CONV_U2] = emit_conv_i4,
    [CEE_CONV_U4] = emit_conv_i4,

    [CEE_CONV_I8] = emit_conv_i8,
    [CEE_CONV_U8] = emit_conv_i8,
    [CEE_CONV_I] = emit_conv_i8,
    [CEE_CONV_U] = emit_conv_i8,

    [CEE_CEQ] = emit_compare,
    [CEE_CGT] = emit_compare,
    [CEE_CGT_UN] = emit_compare,
    [CEE_CLT] = emit_compare,
    [CEE_CLT_UN] = emit_compare,

    [CEE_NEWARR] = emit_newarr,
    [CEE_LDLEN] = emit_ldlen,
    [CEE_LDELEM] = emit_ldelem,
    [CEE_STELEM] = emit_stelem,

    [CEE_LDFTN] = emit_ldftn,
    [CEE_NEWOBJ] = emit_newobj,
    [CEE_CALL] = emit_call,
    [CEE_CALLVIRT] = emit_callvirt,
    [CEE_RET] = emit_ret,

    [CEE_BOX] = emit_box,
    [CEE_UNBOX_ANY] = emit_unbox_any,

    [CEE_BR] = emit_br,
    [CEE_BRFALSE] = emit_br_unary_cond,
    [CEE_BRTRUE] = emit_br_unary_cond,
    [CEE_BEQ] = emit_br_binary_cond,
    [CEE_BGE] = emit_br_binary_cond,
    [CEE_BGT] = emit_br_binary_cond,
    [CEE_BLE] = emit_br_binary_cond,
    [CEE_BLT] = emit_br_binary_cond,
    [CEE_BNE_UN] = emit_br_binary_cond,
    [CEE_BGE_UN] = emit_br_binary_cond,
    [CEE_BGT_UN] = emit_br_binary_cond,
    [CEE_BLE_UN] = emit_br_binary_cond,
    [CEE_BLT_UN] = emit_br_binary_cond,

    [CEE_LEAVE] = emit_leave,
    [CEE_ENDFINALLY] = emit_endfinally,
    [CEE_THROW] = emit_throw,
};
size_t g_emit_dispatch_table_size = ARRAY_LENGTH(g_emit_dispatch_table);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Entry points
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t emitter_on_entry_block(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;

    // if we are not coming from an inline create the entry block, otherwise
    // assume the caller is in charge of that
    if (!function->inline_depth) {
        block->spidir_block = spidir_builder_create_block(builder);
        spidir_builder_set_entry_block(builder, block->spidir_block);
    }
    spidir_builder_set_block(builder, block->spidir_block);

    //
    // initialize and spill the arguments
    //
    for (int i = 0; i < arrlen(block->args); i++) {
        jit_block_local_t* arg = &block->args[i];

        // get the param for this slot, if we are inlining assume the value
        // is already stored inside
        spidir_value_t value = SPIDIR_VALUE_INVALID;
        if (!function->inline_depth) {
            value = spidir_builder_build_param_ref(builder, i);
        } else {
            value = block->args[i].stack.value;
        }

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
            value = spidir_builder_build_stackslot(builder,
                local->stack.type->StackSize, local->stack.type->StackAlignment);

            // we need to zero init it
            if (zero_initialize) {
                jit_emit_bzero(builder, value, local->stack.type);
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
