#include "emit.h"

#include <dotnet/loader.h>
#include <tomatodotnet/types/type.h>
#include <util/except.h>
#include "tomatodotnet/util/stb_ds.h"
#include <util/string.h>

#include "jit.h"
#include "builtin.h"
#include "helpers.h"
#include "type.h"
#include "dotnet/types.h"
#include "tomatodotnet/tdn.h"

// TODO: variant interface arguments

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Type helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static spidir_value_type_t get_spidir_type(RuntimeTypeInfo type) {
    jit_stack_value_kind_t kind = jit_get_type_kind(type);
    if (kind == JIT_KIND_INT32) {
        return SPIDIR_TYPE_I32;
    } else if (kind == JIT_KIND_INT64 || kind == JIT_KIND_NATIVE_INT) {
        return SPIDIR_TYPE_I64;
    } else if (kind == JIT_KIND_F64) {
        return SPIDIR_TYPE_F64;
    } else if (kind == JIT_KIND_F32) {
        return SPIDIR_TYPE_F32;
    } else {
        return SPIDIR_TYPE_PTR;
    }
}

spidir_value_type_t jit_get_spidir_ret_type(RuntimeMethodBase method) {
    RuntimeTypeInfo type = method->ReturnParameter->ParameterType;
    if (tdn_is_struct_like(type)) {
        return SPIDIR_TYPE_NONE;
    }

    jit_stack_value_kind_t kind = jit_get_type_kind(type);
    if (kind == JIT_KIND_INT32) {
        return SPIDIR_TYPE_I32;
    } else if (kind == JIT_KIND_INT64 || kind == JIT_KIND_NATIVE_INT) {
        return SPIDIR_TYPE_I64;
    } else if (kind == JIT_KIND_F64) {
        return SPIDIR_TYPE_F64;
    } else if (kind == JIT_KIND_F32) {
        return SPIDIR_TYPE_F32;
    } else {
        ASSERT(kind == JIT_KIND_BY_REF || kind == JIT_KIND_OBJ_REF);
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
        tdn_is_struct_like(ret_type)
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
    spidir_funcref_t memcpy_func;
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
    spidir_funcref_t memcpy_func;
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

static void jit_emit_store(spidir_builder_handle_t builder, jit_stack_value_t* from, RuntimeTypeInfo to_type, spidir_value_t to_value) {
    if (jit_is_null_reference(from) && tdn_is_struct_like(to_type)) {
        // we need to convert from NULL -> interface
        jit_emit_bzero(builder, to_value, to_type);

    } else if (!tdn_is_interface(from->type) && tdn_is_interface(to_type)) {
        // store the interface instance
        ASSERT(offsetof(Interface, Instance) == 0);
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, from->value, to_value);

        // load the vtable of the object on the stack
        // TODO: if the object is null we need to actually choose a non-null pointer
        spidir_value_t vtable_base = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, from->value);

        // calculate the amount needed
        int iface_offset = jit_get_interface_offset(from->type, to_type);
        ASSERT(iface_offset >= 0, "Failed to get interface offset %T from %T", to_type, from->type);
        iface_offset = iface_offset * (int)sizeof(void*) + (int)offsetof(ObjectVTable, Functions);

        // set the new vtable base
        vtable_base = spidir_builder_build_ptroff(builder, vtable_base,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, iface_offset));

        // and store it into the vtable field
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, vtable_base,
            spidir_builder_build_ptroff(builder, to_value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable))));

    } else if (tdn_is_interface(from->type) && !tdn_is_interface(to_type)) {
        // we need to perform interface -> object
        STATIC_ASSERT(offsetof(Interface, Instance) == 0);
        spidir_value_t instance = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, from->value);
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, instance, to_value);

    } else if (tdn_is_interface(from->type) && tdn_is_interface(to_type) && from->type != to_type) {
        // interface upcast

        // move the instance along
        ASSERT(offsetof(Interface, Instance) == 0);
        spidir_value_t instance = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, from->value);
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, instance, to_value);

        // load the vtable of the object on the stack
        spidir_value_t vtable_base = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
            spidir_builder_build_ptroff(builder, from->value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable))));

        // calculate the amount needed
        int iface_offset = jit_get_interface_offset(from->type, to_type);
        ASSERT(iface_offset >= 0);
        iface_offset *= (int)sizeof(void*);

        // set the new vtable base
        vtable_base = spidir_builder_build_ptroff(builder, vtable_base,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, iface_offset));

        // and store it into the vtable field
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, vtable_base,
            spidir_builder_build_ptroff(builder, to_value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable))));

    } else if (from->kind == JIT_KIND_VALUE_TYPE || (from->kind == JIT_KIND_OBJ_REF && tdn_is_struct_like(from->type))) {
        // only struct types in here and
        ASSERT(from->type == to_type);
        jit_emit_memcpy(builder, to_value, from->value, from->type);

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
        spidir_builder_build_store(builder, mem_size, from->value, to_value);
    }
}

static spidir_value_t jit_emit_load(spidir_builder_handle_t builder, RuntimeTypeInfo from_type, spidir_value_t from) {
    spidir_value_t value = SPIDIR_VALUE_INVALID;
    if (tdn_is_struct_like(from_type)) {
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

static void emit_load_reference(spidir_builder_handle_t builder, jit_stack_value_t* item) {
    if (tdn_is_interface(item->type)) {
        // load the instance from an interface
        STATIC_ASSERT(offsetof(Interface, Instance) == 0);
        item->value = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, item->value);

    } else if (tdn_is_delegate(item->type)) {
        // load the instance from a delegate
        item->value = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
            spidir_builder_build_ptroff(builder, item->value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Function))));

    }
}

static void emit_convert_to_reference(spidir_builder_handle_t builder, jit_stack_value_t* item) {
    if (item->kind == JIT_KIND_NATIVE_INT) {
        item->value = spidir_builder_build_inttoptr(builder, item->value);
    } else {
        emit_load_reference(builder, item);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Exception helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void jit_emit_null_reference(spidir_builder_handle_t builder) {
    // emit a call to null reference exception
    spidir_funcref_t function = jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_THROW_NULL_REFERENCE);
    spidir_builder_build_call(builder, function, 0, NULL);
    spidir_builder_build_unreachable(builder);

    // create a new dummy block so we can actually continue with the emit even tho we reached an
    // explicit null pointer
    spidir_builder_set_block(builder, spidir_builder_create_block(builder));
}

static void jit_emit_invalid_cast(spidir_builder_handle_t builder) {
    // emit a call to null reference exception
    spidir_funcref_t function = jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_THROW_INVALID_CAST);
    spidir_builder_build_call(builder, function, 0, NULL);
    spidir_builder_build_unreachable(builder);
}

static void jit_emit_overflow(spidir_builder_handle_t builder) {
    // emit a call to null reference exception
    spidir_funcref_t function = jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_THROW_OVERFLOW);
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
static spidir_value_t jit_convert_value(spidir_builder_handle_t builder, jit_stack_value_t* from, RuntimeTypeInfo to_type) {
    if (tdn_is_interface(to_type) && from->type != to_type) {
        // if we need to cast to an interface that is not the exact same type then we are going
        // to allocate a new stackslot for the result, even if its an interface input fat pointers
        ASSERT(from->kind == JIT_KIND_OBJ_REF, "Got kind %d of %T", from->kind, from->type);
        spidir_value_t value = spidir_builder_build_stackslot(builder, sizeof(Interface), _Alignof(Interface));
        jit_emit_store(builder, from, to_type, value);
        return value;

    } else if (tdn_is_interface(from->type) && !tdn_is_interface(to_type)) {
        ASSERT(from->kind == JIT_KIND_OBJ_REF, "Got kind %d of %T", from->kind, from->type);

        // we need to perform interface -> object
        STATIC_ASSERT(offsetof(Interface, Instance) == 0);
        return spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, from->value);

    } else if (
        (from->kind == JIT_KIND_BY_REF || from->kind == JIT_KIND_OBJ_REF) &&
        (to_type == tIntPtr || to_type == tUIntPtr)
    ) {
        // by-ref/pointer -> native-int
        return spidir_builder_build_ptrtoint(builder, from->value);

    } else if (from->kind == JIT_KIND_NATIVE_INT && (to_type->IsByRef || to_type->IsPointer)) {
        // pointer native-int -> by-ref/pointer
        return spidir_builder_build_inttoptr(builder, from->value);

    } else if (from->kind == JIT_KIND_F32 && to_type == tDouble) {
        return spidir_builder_build_fwiden(builder, from->value);

    } else if (from->kind == JIT_KIND_F64 && to_type == tSingle) {
        return spidir_builder_build_fnarrow(builder, from->value);

    } else {
        return from->value;
    }
}

static spidir_value_t jit_convert_local(
    spidir_builder_handle_t builder,
    jit_stack_value_t* from,
    RuntimeTypeInfo to_type
) {
    // get the enum type when converting
    if (to_type->BaseType == tEnum) {
        to_type = to_type->EnumUnderlyingType;
    }

    // special case only needed for locals, truncate/sign extend as required
    if (from->kind == JIT_KIND_INT32 && to_type->StackSize < 4) {
        int bit_count = to_type->StackSize * 8;
        if (to_type == tSByte || to_type == tInt16) {
            return spidir_builder_build_sfill(builder, bit_count, from->value);
        } else if (to_type == tByte || to_type == tUInt16 || to_type == tBoolean || to_type == tChar) {
            return spidir_builder_build_and(builder, from->value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, (1 << bit_count) - 1));
        } else {
            ASSERT(!"Invalid", "%T -> %T", from->type, to_type);
        }

    }

    // fallback to the normal convert logic
    return jit_convert_value(builder, from, to_type);
}

static spidir_value_type_t get_spidir_stack_phi_type(jit_stack_value_t* value) {
    switch (value->kind) {
        case JIT_KIND_INT32: return SPIDIR_TYPE_I32;
        case JIT_KIND_INT64: return SPIDIR_TYPE_I64;
        case JIT_KIND_NATIVE_INT: return SPIDIR_TYPE_I64;
        case JIT_KIND_F32: return SPIDIR_TYPE_F32;
        case JIT_KIND_F64: return SPIDIR_TYPE_F64;
        case JIT_KIND_BY_REF: return SPIDIR_TYPE_PTR;
        case JIT_KIND_OBJ_REF: return SPIDIR_TYPE_PTR;
        case JIT_KIND_VALUE_TYPE: return SPIDIR_TYPE_PTR;
        default: ASSERT(!"Invalid value");
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
                    get_spidir_stack_phi_type(&block->stack[i]),
                    0, NULL, &block->stack_phis[i]);
            }

            for (int i = 0; i < arrlen(block->locals); i++) {
                if (!function->locals[i].spilled) {
                    block->locals[i].value = spidir_builder_build_phi(builder,
                        get_spidir_type(function->locals[i].type),
                        0, NULL, &block->locals[i].phi);
                }
            }

            for (int i = 0; i < arrlen(block->args); i++) {
                if (!function->args[i].spilled) {
                    block->args[i].value = spidir_builder_build_phi(builder,
                        get_spidir_type(function->args[i].type),
                        0, NULL, &block->args[i].phi);
                }
            }

            // switch back to the old block
            if (had_block) {
                spidir_builder_set_block(builder, current_block);
            }
        }

        // add the phi inputs to the stack entries
        for (int i = 0; i < arrlen(block->stack); i++) {
            spidir_builder_add_phi_input(builder, block->stack_phis[i],
                jit_convert_value(builder,
                    &from->stack[i],
                    block->stack[i].type));
        }

        // add locals inputs
        for (int i = 0; i < arrlen(block->locals); i++) {
            if (function->locals[i].spilled) {
                // for spilled we just move the same value over all the blocks
                // because the data is not SSAd
                block->locals[i].value = from->locals[i].value;

            } else if (block->locals[i].initialized || function->locals[i].zero_initialize) {
                // we only need to add the phi input if the value wants to be initialized
                // in the next block, or if its zero initialized
                spidir_builder_add_phi_input(builder, block->locals[i].phi, from->locals[i].value);

            } else {
                // keep as invalid just in case, so we can catch bugs
                block->locals[i].value = SPIDIR_VALUE_INVALID;
            }
        }

        for (int i = 0; i < arrlen(block->args); i++) {
            if (function->args[i].spilled) {
                block->args[i].value = from->args[i].value;
            } else {
                spidir_builder_add_phi_input(builder, block->args[i].phi, from->args[i].value);
            }
        }

    } else {
        // just copy over the values
        for (int i = 0; i < arrlen(block->stack); i++) {
            block->stack[i].value = from->stack[i].value;
        }

        for (int i = 0; i < arrlen(block->locals); i++) {
            block->locals[i].value = from->locals[i].value;
        }

        for (int i = 0; i < arrlen(block->args); i++) {
            block->args[i].value = from->args[i].value;
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
static tdn_err_t emit_nop(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

cleanup:
    return err;
}

// Use as a template for adding new instructions
static tdn_err_t emit_sizeof(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    STACK_TOP()->value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, inst->operand.type->StackSize);
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// Argument access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t emit_load_local(jit_function_t *function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_block_local_t* block_locals = is_arg ? block->args : block->locals;
    jit_local_t* function_locals = is_arg ? function->args : function->locals;
    CHECK(inst->operand.variable < arrlen(block_locals));
    jit_block_local_t* block_local = &block_locals[inst->operand.variable];
    jit_local_t* func_local = &function_locals[inst->operand.variable];

    spidir_value_t value = SPIDIR_VALUE_INVALID;
    if (func_local->spilled) {
        // its spilled, load it as the correct type, we will never have a different value in the
        // local itself, and rely on devirt + SROA to optimize away interfaces when applicable
        value = jit_emit_load(builder, func_local->type, block_local->value);
    } else {
        // use the SSAd value as is
        value = block_local->value;
    }

    // and store it
    STACK_TOP()->value = value;

cleanup:
    return err;
}

static tdn_err_t emit_store_local(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_block_local_t* block_locals = is_arg ? block->args : block->locals;
    jit_local_t* function_locals = is_arg ? function->args : function->locals;
    CHECK(inst->operand.variable < arrlen(block_locals));
    jit_block_local_t* block_local = &block_locals[inst->operand.variable];
    jit_local_t* func_local = &function_locals[inst->operand.variable];

    if (func_local->spilled) {
        // need to store into the buffer, from whatever type we had
        jit_emit_store(builder, stack, func_local->type, block_local->value);
    } else {
        // SSA store, ensure we truncate as required
        block_local->value = jit_convert_local(builder, stack, func_local->type);
    }

cleanup:
    return err;
}

static tdn_err_t emit_load_local_address(jit_function_t* function, jit_block_t* block, tdn_il_inst_t* inst, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the local slot
    jit_block_local_t* block_locals = is_arg ? block->args : block->locals;
    jit_local_t* function_locals = is_arg ? function->args : function->locals;
    CHECK(inst->operand.variable < arrlen(block_locals));
    jit_block_local_t* block_local = &block_locals[inst->operand.variable];
    jit_local_t* func_local = &function_locals[inst->operand.variable];
    CHECK(func_local->spilled);

    // just give the pointer
    STACK_TOP()->value = block_local->value;

cleanup:
    return err;
}

static tdn_err_t emit_ldarg(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return emit_load_local(function, builder, block, inst, true);
}

static tdn_err_t emit_starg(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return emit_store_local(function, builder, block, inst, stack, true);
}

static tdn_err_t emit_ldarga(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return emit_load_local_address(function, block, inst, true);
}

static tdn_err_t emit_ldloc(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return emit_load_local(function, builder, block, inst, false);
}

static tdn_err_t emit_stloc(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return emit_store_local(function, builder, block, inst, stack, false);
}

static tdn_err_t emit_ldloca(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    return emit_load_local_address(function, block, inst, false);
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

        // Register gc roots
        if (tdn_type_is_valuetype(field->FieldType)) {
            // register the children of the struct
            for (int i = 0; i < arrlen(field->FieldType->ManagedPointers); i++) {
                tdn_host_gc_register_root(field->JitFieldPtr + field->FieldType->ManagedPointers[i]);
            }
        } else {
            // a pointer to an object, register it directly
            tdn_host_gc_register_root(field->JitFieldPtr);
        }
    }

    return spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uintptr_t)field->JitFieldPtr);
}

static tdn_err_t emit_ldflda(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the field offset
    spidir_value_t field_ptr = SPIDIR_VALUE_INVALID;

    if (inst->operand.field->Attributes.Static) {
        jit_queue_cctor(spidir_builder_get_module(builder), inst->operand.field->DeclaringType);
        field_ptr = jit_get_static_field(builder, inst->operand.field);
    } else {
        // add the offset to the field and access it
        field_ptr = spidir_builder_build_ptroff(builder, stack[0].value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst->operand.field->FieldOffset));
    }

    // perform the load
    STACK_TOP()->value = field_ptr;

cleanup:
    return err;
}

static tdn_err_t emit_ldfld(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the field offset
    spidir_value_t field_ptr = SPIDIR_VALUE_INVALID;

    if (inst->operand.field->Attributes.Static) {
        jit_queue_cctor(spidir_builder_get_module(builder), inst->operand.field->DeclaringType);
        field_ptr = jit_get_static_field(builder, inst->operand.field);
    } else {
        // for pointer access convert it to a pointer first
        emit_convert_to_reference(builder, &stack[0]);

        // add the offset to the field and access it
        field_ptr = spidir_builder_build_ptroff(builder, stack[0].value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst->operand.field->FieldOffset));
    }

    // perform the load
    STACK_TOP()->value = jit_emit_load(builder, inst->operand.field->FieldType, field_ptr);

cleanup:
    return err;
}

static tdn_err_t emit_stfld(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the field offset
    spidir_value_t field_ptr = SPIDIR_VALUE_INVALID;

    jit_stack_value_t* value = NULL;
    if (inst->operand.field->Attributes.Static) {
        jit_queue_cctor(spidir_builder_get_module(builder), inst->operand.field->DeclaringType);
        field_ptr = jit_get_static_field(builder, inst->operand.field);
        value = &stack[0];
    } else {
        // add the offset to the field and access it
        field_ptr = spidir_builder_build_ptroff(builder, stack[0].value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst->operand.field->FieldOffset));
        value = &stack[1];
    }

    // perform the store
    jit_emit_store(builder, value, inst->operand.field->FieldType, field_ptr);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Indirect reference access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t emit_initobj(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeTypeInfo dest_type = stack[0].type;
    if (dest_type == tByte || dest_type == tSByte || dest_type == tBoolean) {
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_1,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 0), stack[0].value);
    } else if (dest_type == tInt16 || dest_type == tUInt16 || dest_type == tChar) {
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_2,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 0), stack[0].value);
    } else if (dest_type == tInt32 || dest_type == tUInt32) {
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_4,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 0), stack[0].value);
    } else if (dest_type == tInt64 || dest_type == tUInt64 || dest_type == tIntPtr || dest_type == tUIntPtr || tdn_type_is_gc_pointer(dest_type)) {
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 0), stack[0].value);

    } else if (dest_type == tSingle) {
        CHECK_FAIL();
    } else if (dest_type == tDouble) {
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8,
            spidir_builder_build_fconst64(builder, 0), stack[0].value);

    } else {
        CHECK(tdn_type_is_valuetype(dest_type));
        jit_emit_bzero(builder, stack[0].value, dest_type);
    }

cleanup:
    return err;
}

static tdn_err_t emit_localloc(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // For now we only support constant localloc
    CHECK(inst->operand_type == TDN_IL_INT32);

    // TODO: verify we are not in a loop or something because we
    //       don't have support for real alloca

    // calculate some make sense alignment, but default to 16 for anything too large
    // because we can't know what would be stored in here
    size_t align = 16;
    if (inst->operand.int32 <= 1) align = 1;
    if (inst->operand.int32 <= 2) align = 2;
    if (inst->operand.int32 <= 4) align = 4;
    if (inst->operand.int32 <= 8) align = 8;

    // create the stackslot
    spidir_value_t value = spidir_builder_build_stackslot(builder, inst->operand.int32, align);

    // zero it
    spidir_funcref_t bzero_func = jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_BZERO);
    spidir_builder_build_call(builder,
        bzero_func, 2,
        (spidir_value_t[]) {
            value, spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst->operand.int32)
        }
    );

    // and remember the pointer
    STACK_TOP()->value = value;

cleanup:
    return err;
}

static tdn_err_t emit_ldind(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    RuntimeTypeInfo type = inst->operand.type;
    if (type == NULL) {
        type = stack[0].type;
    }

    emit_convert_to_reference(builder, &stack[0]);

    STACK_TOP()->value = jit_emit_load(builder, type, stack[0].value);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_stind(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    emit_convert_to_reference(builder, &stack[0]);

    RuntimeTypeInfo typ = inst->operand.type;
    if (typ == NULL) {
        typ = tObject;
    }

    // we just need to store into the given item
    // NOTE: we can use the type directly because this must be a by-ref at this point
    jit_emit_store(builder, &stack[1], typ, stack[0].value);
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// Stack manipulation
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t emit_ldnull(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    STACK_TOP()->value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_ldstr(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    // TODO: pin the string
    STACK_TOP()->value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uintptr_t)inst->operand.string);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_ldtoken(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // allocate the struct and fill it with the type pointer
    spidir_value_t value = spidir_builder_build_stackslot(builder,
        STACK_TOP()->type->StackSize, STACK_TOP()->type->StackAlignment);

    // choose the correct ptr
    // TODO: pin the type
    uint64_t ptr;
    switch (inst->operand_type) {
        case TDN_IL_TYPE: ptr = (uint64_t)inst->operand.type; break;
        case TDN_IL_METHOD: ptr = (uint64_t)inst->operand.method; break;
        case TDN_IL_FIELD: ptr = (uint64_t)inst->operand.field; break;
        default: CHECK_FAIL();
    }

    // and just store it
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8,
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, ptr),
        value);
    STACK_TOP()->value = value;

cleanup:
    return err;
}

static tdn_err_t emit_ldc_i4(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    STACK_TOP()->value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, inst->operand.uint32);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_ldc_i8(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    STACK_TOP()->value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst->operand.uint64);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_ldc_r4(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    STACK_TOP()->value = spidir_builder_build_fconst32(builder, inst->operand.float32);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_ldc_r8(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    STACK_TOP()->value = spidir_builder_build_fconst64(builder, inst->operand.float64);
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

static spidir_value_t emit_float_binary_compare(spidir_builder_handle_t builder, tdn_il_opcode_t opcode, spidir_value_t value1, spidir_value_t value2) {
    spidir_fcmp_kind_t kind;
    switch (opcode) {
        case CEE_BEQ:
        case CEE_CEQ: kind = SPIDIR_FCMP_OEQ; break;
        case CEE_BNE_UN: kind = SPIDIR_FCMP_UNE; break;
        case CEE_BGT:
        case CEE_CGT: kind = SPIDIR_FCMP_OLT; SWAP(value1, value2); break;
        case CEE_BGE: kind = SPIDIR_FCMP_OLE; SWAP(value1, value2); break;
        case CEE_BGT_UN:
        case CEE_CGT_UN: kind = SPIDIR_FCMP_ULT; SWAP(value1, value2); break;
        case CEE_BGE_UN: kind = SPIDIR_FCMP_ULE; SWAP(value1, value2); break;
        case CEE_BLT:
        case CEE_CLT: kind = SPIDIR_FCMP_OLT; break;
        case CEE_BLE: kind = SPIDIR_FCMP_OLE; break;
        case CEE_BLT_UN:
        case CEE_CLT_UN: kind = SPIDIR_FCMP_ULT; break;
        case CEE_BLE_UN: kind = SPIDIR_FCMP_ULE; break;
        default: ASSERT(!"Invalid opcode");
    }
    return spidir_builder_build_fcmp(builder, kind, SPIDIR_TYPE_I32, value1, value2);
}

static void emit_convert_for_op(spidir_builder_handle_t builder, jit_stack_value_t* a, jit_stack_value_t* b, bool is_unsigned, bool allow_ptr) {
    // ensure we have hte actual references and not interfaces
    emit_load_reference(builder, a);
    emit_load_reference(builder, b);

    if (!allow_ptr || (jit_is_pointer(a) != jit_is_pointer(b))) {
        // we are going to conver the pointer to native int if either we don't allow
        // pointers at all, or we allow them only if both of them are a pointer
        if (jit_is_pointer(a)) {
            a->value = spidir_builder_build_ptrtoint(builder, a->value);
        }
        if (jit_is_pointer(b)) {
            b->value = spidir_builder_build_ptrtoint(builder, b->value);
        }
    }

    // sign extend if we need to
    if (a->kind != b->kind) {
        if (a->kind == JIT_KIND_INT32) {
            a->value = emit_extend_int(builder, a->value, !is_unsigned);
        }

        if (b->kind == JIT_KIND_INT32) {
            b->value = emit_extend_int(builder, b->value, !is_unsigned);
        }
    }
}

static tdn_err_t emit_compare(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    bool is_unsigned = inst->opcode == CEE_CGT_UN || inst->opcode == CEE_CLT_UN;

    if (jit_is_float(&stack[0])) {
        // if the kinds are not the same we need to extend both into a full integer
        if (stack[0].kind != stack[1].kind) {
            if (stack[0].kind == JIT_KIND_F32) {
                stack[0].value = spidir_builder_build_fwiden(builder, stack[0].value);
            }

            if (stack[1].kind == JIT_KIND_F32) {
                stack[1].value = spidir_builder_build_fwiden(builder, stack[1].value);
            }
        }

        STACK_TOP()->value = emit_float_binary_compare(builder, inst->opcode, stack[0].value, stack[1].value);

    } else {
        // if the kinds are not the same we need to extend both into a full integer
        emit_convert_for_op(builder, &stack[0], &stack[1], is_unsigned, true);
        STACK_TOP()->value = emit_binary_compare(builder, inst->opcode, stack[0].value, stack[1].value);
    }

cleanup:
    return err;
}

//
// OVERFLOW CHECKING TABLE
//
// | to / from | uint32            | uint64                | int32                    | int64                    |
// |-----------|-------------------|-----------------------|--------------------------|--------------------------|
// | uint8     | value ult 0x100   | value ult 0x100       | value ult 0x100          | value ult 0x100          |
// | uint16    | value ult 0x10000 | value ult 0x10000     | value ult 0x10000        | value ult 0x10000        |
// | uint32    | nop               | value ult 0x100000000 | -1 slt value             | value ult 0x100000000    |
// | uint64    | nop               | nop                   | -1 slt value             | -1 slt value             |
// | int8      | value ult 0x80    | value ult 0x80        | sfill 8 and eq original  | sfill 8 and eq original  |
// | int16     | value ult 0x8000  | value ult 0x8000      | sfill 16 and eq original | sfill 16 and eq original |
// | int32     | -1 slt value      | -1 slt value          | nop                      | sfill 32 and eq original |
// | int64     | nop               | -1 slt value          | nop                      | nop                      |
//


static tdn_err_t emit_conv_ovf_i4(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_t value = stack[0].value;
    tdn_il_opcode_t opcode = inst->opcode;

    // nop case,
    spidir_value_type_t type;
    uint64_t minus_one;
    if (stack->kind == JIT_KIND_INT32) {
        if (opcode == CEE_CONV_OVF_U4 || opcode == CEE_CONV_OVF_U4_UN) {
            // special case of nop:
            //      uint32 -> uint32
            //      int32 -> int32
            goto skip_check;
        }

        type = SPIDIR_TYPE_I32;
        minus_one = (uint32_t)-1;
    } else if (stack->kind == JIT_KIND_INT64 || stack->kind == JIT_KIND_NATIVE_INT) {
        type = SPIDIR_TYPE_I64;
        minus_one = (uint64_t)-1;
    } else {
        // TODO: floats
        CHECK_FAIL();
    }

    spidir_value_t cond = SPIDIR_VALUE_INVALID;
    if (
        (stack->kind == JIT_KIND_INT32 && opcode == CEE_CONV_OVF_U4) ||
        (opcode == CEE_CONV_OVF_I4_UN)
    ) {
        // int32 -> uint32
        // uint32/uint64 -> int32
        cond = spidir_builder_build_icmp(builder, SPIDIR_ICMP_SLT, SPIDIR_TYPE_I32,
            spidir_builder_build_iconst(builder, type, minus_one), value);
    } else if (
        opcode == CEE_CONV_OVF_I1 ||
        opcode == CEE_CONV_OVF_I2 ||
        opcode == CEE_CONV_OVF_I4
    ) {
        // int32/int64 -> int8
        // int32/int64 -> int16
        // int64 -> int32

        // get the correct bit count
        size_t bit_width;
        if (opcode == CEE_CONV_OVF_I1) {
            bit_width = 8;
        } else if (opcode == CEE_CONV_OVF_I2) {
            bit_width = 16;
        } else if (opcode == CEE_CONV_OVF_I4) {
            bit_width = 32;
        } else {
            CHECK_FAIL();
        }

        // sign extend and check they are still the same
        spidir_value_t signed_value = spidir_builder_build_sfill(builder, bit_width, value);
        cond = spidir_builder_build_icmp(builder, SPIDIR_ICMP_EQ, SPIDIR_TYPE_I32, value, signed_value);
    } else {
        // get the correct max value
        uint64_t max_value;
        if (opcode == CEE_CONV_OVF_U1 || opcode == CEE_CONV_OVF_U1_UN) {
            max_value = 0x100;
        } else if (opcode == CEE_CONV_OVF_U2 || opcode == CEE_CONV_OVF_U2_UN) {
            max_value = 0x10000;
        } else if (opcode == CEE_CONV_OVF_I1_UN) {
            max_value = 0x80;
        } else if (opcode == CEE_CONV_OVF_I2_UN) {
            max_value = 0x8000;
        } else {
            CHECK_FAIL();
        }

        // perform an unsigned less than check, this is fine for both the signed and unsigned
        // input because we are using twos complement
        cond = spidir_builder_build_icmp(builder, SPIDIR_ICMP_ULT, SPIDIR_TYPE_I32, value,
            spidir_builder_build_iconst(builder, type, max_value));
    }

    // perform the branch
    spidir_block_t valid = spidir_builder_create_block(builder);
    spidir_block_t invalid = spidir_builder_create_block(builder);
    spidir_builder_build_brcond(builder, cond, valid, invalid);

    // invalid path, throw
    spidir_builder_set_block(builder, invalid);
    jit_emit_overflow(builder);

    // valid path, push the new value
    spidir_builder_set_block(builder, valid);

skip_check:

    // truncate if came from
    // a bigger value
    if (stack->kind != JIT_KIND_INT32) {
        value = spidir_builder_build_itrunc(builder, value);
    }

    STACK_TOP()->value = value;

cleanup:
    return err;
}

static tdn_err_t emit_conv_i4(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_t value = stack[0].value;

    // truncate the value if too big
    if (stack[0].kind == JIT_KIND_INT64 || stack[0].kind == JIT_KIND_NATIVE_INT) {
        value = spidir_builder_build_itrunc(builder, value);

    } else if (stack[0].kind == JIT_KIND_OBJ_REF || stack[0].kind == JIT_KIND_BY_REF) {
        value = spidir_builder_build_ptrtoint(builder, value);
        value = spidir_builder_build_itrunc(builder, value);

    } else if (jit_is_float(&stack[0])) {
        if (inst->opcode == CEE_CONV_I1 || inst->opcode == CEE_CONV_I2) {
            value = spidir_builder_build_floattosint(builder, SPIDIR_TYPE_I32, value);
        } else {
            value = spidir_builder_build_floattouint(builder, SPIDIR_TYPE_I32, value);
        }
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

cleanup:
    return err;
}

static tdn_err_t emit_conv_ovf_i8(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_t value = stack[0].value;

    // nop case,
    spidir_value_type_t type;
    uint64_t minus_one;
    if (stack->kind == JIT_KIND_INT32) {
        if (inst->opcode == CEE_CONV_OVF_I8_UN || inst->opcode == CEE_CONV_OVF_I_UN) {
            // special case, uint32 -> int64 is always valid
            goto skip_check;
        }

        type = SPIDIR_TYPE_I32;
        minus_one = (uint32_t)-1;
    } else if (stack->kind == JIT_KIND_INT64 || stack->kind == JIT_KIND_NATIVE_INT) {
        type = SPIDIR_TYPE_I64;
        minus_one = (uint64_t)-1;

    } else {
        // TODO: floats
        CHECK_FAIL();
    }

    // make sure is positive by doing a sign check
    spidir_value_t cond = spidir_builder_build_icmp(builder, SPIDIR_ICMP_SLT, SPIDIR_TYPE_I32,
        spidir_builder_build_iconst(builder, type, minus_one), value);

    // perform the branch
    spidir_block_t valid = spidir_builder_create_block(builder);
    spidir_block_t invalid = spidir_builder_create_block(builder);
    spidir_builder_build_brcond(builder, cond, valid, invalid);

    // invalid path, throw
    spidir_builder_set_block(builder, invalid);
    jit_emit_overflow(builder);

    // valid path, push the new value
    spidir_builder_set_block(builder, valid);

skip_check:
    // if the input was 32bit,
    // in this case we only either have unsigned -> signed or signed -> unsigned in both
    // cases we make sure that the value is positive so we can just do a normal zero extension
    if (stack->kind == JIT_KIND_INT32) {
        value = spidir_builder_build_iext(builder, value);
        value = spidir_builder_build_and(builder, value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 0xFFFFFFFF));
    }

    STACK_TOP()->value = value;

cleanup:
    return err;
}

static tdn_err_t emit_conv_i8(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_t value = stack[0].value;

    // extend it to a 32bit value if too small
    if (stack[0].kind == JIT_KIND_INT32) {
        value = emit_extend_int(builder, stack[0].value,
            inst->opcode == CEE_CONV_I8 || inst->opcode == CEE_CONV_I);

    } else if (jit_is_float(&stack[0])) {
        if (inst->opcode == CEE_CONV_I || inst->opcode == CEE_CONV_I8) {
            value = spidir_builder_build_floattosint(builder, SPIDIR_TYPE_I64, value);
        } else {
            value = spidir_builder_build_floattouint(builder, SPIDIR_TYPE_I64, value);
        }

    } else if (stack[0].kind == JIT_KIND_BY_REF || stack[0].kind == JIT_KIND_OBJ_REF) {
        value = spidir_builder_build_ptrtoint(builder, value);
    }

    STACK_TOP()->value = value;

cleanup:
    return err;
}

static tdn_err_t emit_conv_r4(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_t value = stack[0].value;

    if (stack[0].kind == JIT_KIND_F64) {
        value = spidir_builder_build_fnarrow(builder, value);
    } else if (stack[0].kind == JIT_KIND_F32) {
        // f32 -> f32 is a nop
    } else {
        value = spidir_builder_build_sinttofloat(builder, SPIDIR_TYPE_F32, value);
    }

    STACK_TOP()->value = value;

cleanup:
    return err;
}


static tdn_err_t emit_conv_r8(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_t value = stack[0].value;

    if (stack[0].kind == JIT_KIND_F64) {
        // f64 -> f64 is a nop
    } else if (stack[0].kind == JIT_KIND_F32) {
        value = spidir_builder_build_fwiden(builder, value);
    } else {
        if (inst->opcode == CEE_CONV_R_UN) {
            value = spidir_builder_build_uinttofloat(builder, SPIDIR_TYPE_F64, value);
        } else {
            value = spidir_builder_build_sinttofloat(builder, SPIDIR_TYPE_F64, value);
        }
    }

    STACK_TOP()->value = value;

cleanup:
    return err;
}

static tdn_err_t emit_binary_op(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    spidir_value_t value = SPIDIR_VALUE_INVALID;
    if (
        // optimization for the case of ptr +/- value => ptr
        (inst->opcode == CEE_ADD || inst->opcode == CEE_SUB) &&
        jit_is_pointer(&stack[0]) && !jit_is_pointer(&stack[1])
    ) {
        spidir_value_t op1 = stack[0].value;
        spidir_value_t op2 = stack[1].value;

        // extend the other value if needed
        if (stack[1].kind == JIT_KIND_INT32) {
            op2 = emit_extend_int(builder, op2, false);
        }

        // if we want to sub then just neg the second parameter
        if (inst->opcode == CEE_SUB) {
            spidir_value_t zero = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 0);
            op2 = spidir_builder_build_isub(builder, zero, op2);
        }

        // and now we can perform the ptroff
        value = spidir_builder_build_ptroff(builder, op1, op2);

    } else if (
        // optimization for the case of ptr +/- value => ptr
        inst->opcode == CEE_ADD &&
        !jit_is_pointer(&stack[0]) && jit_is_pointer(&stack[1])
    ) {
        spidir_value_t op1 = stack[0].value;
        spidir_value_t op2 = stack[1].value;

        // extend the other value if needed
        if (stack[0].kind == JIT_KIND_INT32) {
            op1 = emit_extend_int(builder, op1, false);
        }

        // and now we can perform the ptroff
        value = spidir_builder_build_ptroff(builder, op2, op1);

    } else if (jit_is_float(&stack[1])) {
        if (stack[0].kind != stack[1].kind) {
            if (stack[0].kind == JIT_KIND_F32) {
                stack[0].value = spidir_builder_build_fwiden(builder, stack[0].value);
            }

            if (stack[1].kind == JIT_KIND_F32) {
                stack[1].value = spidir_builder_build_fwiden(builder, stack[1].value);
            }
        }

        switch (inst->opcode) {
            case CEE_ADD: value = spidir_builder_build_fadd(builder, stack[0].value, stack[1].value); break;
            case CEE_SUB: value = spidir_builder_build_fsub(builder, stack[0].value, stack[1].value); break;
            case CEE_MUL: value = spidir_builder_build_fmul(builder, stack[0].value, stack[1].value); break;
            case CEE_DIV: value = spidir_builder_build_fdiv(builder, stack[0].value, stack[1].value); break;
            case CEE_REM: {
                // reminder is implemented as a native method because spidir does not have a builtin for it
                jit_helper_type_t helper = stack[0].kind == JIT_KIND_F32 ? JIT_HELPER_F32_REM : JIT_HELPER_F64_REM;
                spidir_funcref_t rem = jit_get_helper(spidir_builder_get_module(builder), helper);
                value = spidir_builder_build_call(builder, rem,
                    2, (spidir_value_t[]){ stack[0].value, stack[1].value });
            } break;
            default: CHECK_FAIL();
        }
    } else {
        bool is_unsigned = inst->opcode == CEE_DIV_UN || inst->opcode == CEE_REM_UN;
        emit_convert_for_op(builder, &stack[0], &stack[1], is_unsigned, false);

        // if the kinds are not the same we need to extend both into a full integer
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

        // this should never return a pointer back
        CHECK(!jit_is_pointer(STACK_TOP()));
    }
    STACK_TOP()->value = value;

cleanup:
    return err;
}

static tdn_err_t emit_binary_op_ovf(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    bool is_unsigned = inst->opcode == CEE_ADD_OVF_UN || inst->opcode == CEE_SUB_OVF_UN || inst->opcode == CEE_MUL_OVF_UN;

    // if the kinds are not the same we need to extend both into a full integer
    emit_convert_for_op(builder, &stack[0], &stack[1], is_unsigned, false);

    spidir_value_t value = SPIDIR_VALUE_INVALID;
    switch (inst->opcode) {
        case CEE_ADD_OVF: value = spidir_builder_build_iadd(builder, stack[0].value, stack[1].value); break;
        case CEE_ADD_OVF_UN: value = spidir_builder_build_iadd(builder, stack[0].value, stack[1].value); break;
        case CEE_SUB_OVF: value = spidir_builder_build_isub(builder, stack[0].value, stack[1].value); break;
        case CEE_SUB_OVF_UN: value = spidir_builder_build_isub(builder, stack[0].value, stack[1].value); break;
        case CEE_MUL_OVF: value = spidir_builder_build_imul(builder, stack[0].value, stack[1].value); break;
        case CEE_MUL_OVF_UN: value = spidir_builder_build_imul(builder, stack[0].value, stack[1].value); break;
        default: CHECK_FAIL();
    }
    STACK_TOP()->value = value;

cleanup:
    return err;
}

static tdn_err_t emit_neg(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    // emulate neg via `0 - value`
    if (jit_is_float(&stack[0])) {
        spidir_value_t zero = stack[0].kind == JIT_KIND_F32 ? spidir_builder_build_fconst32(builder, 0) :
                                                                spidir_builder_build_fconst64(builder, 0);
        STACK_TOP()->value = spidir_builder_build_fsub(builder, zero, stack[0].value);
    } else {
        spidir_value_t zero = spidir_builder_build_iconst(builder, get_spidir_type(stack[0].type), 0);
        STACK_TOP()->value = spidir_builder_build_isub(builder, zero, stack[0].value);
    }
    return TDN_NO_ERROR;
}

static tdn_err_t emit_not(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    // emulate neg via `value ^ -1`
    spidir_value_t ones;
    if (stack[0].kind == JIT_KIND_INT32) {
        ones = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 0xFFFFFFFF);
    } else {
        ones = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 0xFFFFFFFFFFFFFFFF);
    }
    STACK_TOP()->value = spidir_builder_build_xor(builder, ones, stack[0].value);
    return TDN_NO_ERROR;
}

static tdn_err_t emit_shift(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
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

static tdn_err_t emit_newarr(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // sign extend an int32 for it to work
    if (stack[0].kind == JIT_KIND_INT32) {
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

static tdn_err_t emit_ldlen(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
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
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, tdn_get_array_elements_offset(type)));
    return spidir_builder_build_ptroff(builder, array, offset);
}

static tdn_err_t emit_ldelema(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
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
    STACK_TOP()->value = emit_array_offset(builder, stack[0].type->ElementType, stack[0].value, stack[1].value);

cleanup:
    return err;
}

static tdn_err_t emit_ldelem(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
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

static tdn_err_t emit_stelem(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
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
    jit_emit_store(builder, &stack[2], stack[0].type->ElementType, ptr);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Method related
//----------------------------------------------------------------------------------------------------------------------

static spidir_value_t* emit_gather_arguments(spidir_builder_handle_t builder, RuntimeMethodBase callee, jit_stack_value_t* stack) {
    spidir_value_t* values = NULL;

    // check if we need to skip the this
    int convert_offset = 0;
    if (!callee->Attributes.Static) {
        convert_offset = 1;
    }

    // gather all arguments
    for (int i = 0; i < arrlen(stack); i++) {
        spidir_value_t value = SPIDIR_VALUE_INVALID;
        if (i >= convert_offset) {
            // convert other arguments
            value = jit_convert_local(
                builder,
                &stack[i],
                callee->Parameters->Elements[i - convert_offset]->ParameterType
            );
        } else {
            // convert the this
            RuntimeTypeInfo this_type = callee->DeclaringType;
            if (tdn_type_is_valuetype(this_type)) {
                ASSERT(!IS_ERROR(tdn_get_byref_type(this_type, &this_type)));
            }
            value = jit_convert_local(
                builder,
                &stack[i],
                this_type
            );
        }

        arrpush(values, value);
    }

    // push return argument
    RuntimeTypeInfo ret_type = callee->ReturnParameter->ParameterType;
    if (ret_type != tVoid && tdn_is_struct_like(ret_type)) {
        arrpush(values, spidir_builder_build_stackslot(builder, ret_type->StackSize, ret_type->StackAlignment));
    }

    return values;
}

static tdn_err_t emit_ldftn(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeMethodBase callee = inst->operand.method;

    // get the spidir function
    spidir_funcref_t func;
    if (callee->Attributes.Static) {
        jit_queue_cctor(spidir_builder_get_module(builder), callee->DeclaringType);
        func = jit_generate_static_delegate_thunk(spidir_builder_get_module(builder), callee);
    } else {
        func = jit_get_function(spidir_builder_get_module(builder), callee);
    }

    // and load it as a function pointer
    STACK_TOP()->value = spidir_builder_build_funcaddr(builder, func);

cleanup:
    return err;
}

static tdn_err_t emit_call(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_t* values = NULL;
    jit_function_t* inlinee = NULL;

    RuntimeMethodBase callee = inst->operand.method;
    jit_builtin_emitter_t emitter = jit_get_builtin_emitter(callee);
    if (emitter == NULL) {
        // if this is not an emitter attempt to resolve the inlinee
        // function, if any
        int idx = hmgeti(block->inlines, inst->pc);
        if (idx >= 0) {
            inlinee = block->inlines[idx].value;

            // ensure that the emitter logic arrived to the same method
            // as the inline code in the type propagation
            CHECK(inlinee->method == inst->operand.method);
        }
    }

    if (callee->Attributes.Static) {
        jit_queue_cctor(spidir_builder_get_module(builder), callee->DeclaringType);

        // we have a static interface function, "devirtualize" it, aka, get the actual implementation for it
        // use the same methods we use for the normal virtual stuff
        if (inst->constrained != NULL) {
            RuntimeMethodInfo base = (RuntimeMethodInfo)callee;
            CHECK_AND_RETHROW(tdn_find_explicit_implementation(inst->constrained,
                base, (RuntimeMethodInfo*)&callee));
            CHECK(callee != NULL,
                "%T::%U not found for %T", base->DeclaringType, base->Name, inst->constrained);
        }
    }

    // check if we decided to inline the method
    if (inlinee != NULL) {
        // pass the arguments, we need to convert them properly
        // to the correct types before we do so tho
        for (int i = 0; i < arrlen(inlinee->entry_block.args); i++) {
            inlinee->entry_block.args[i].value = jit_convert_local(
                builder,
                &stack[i],
                inlinee->args[i].type);
        }

        // create the entry block and jump into it
        inlinee->entry_block.spidir_block = spidir_builder_create_block(builder);
        spidir_builder_build_branch(builder, inlinee->entry_block.spidir_block);

        // setup the inline information
        inlinee->return_block = spidir_builder_create_block(builder);
        spidir_builder_set_block(builder, inlinee->return_block);

        // prepare the return phi if needed
        spidir_value_t return_value = SPIDIR_VALUE_INVALID;
        if (callee->ReturnParameter->ParameterType != tVoid) {
            return_value = spidir_builder_build_phi(builder,
                get_spidir_type(callee->ReturnParameter->ParameterType),
                0, NULL, &inlinee->return_phi);
        }

        // now let it cook
        CHECK_AND_RETHROW(jit_function(inlinee, builder));

        // and set the return value, unlike a normal call, struct likes are
        // returned directly instead of by-reference
        if (callee->ReturnParameter->ParameterType != tVoid) {
            STACK_TOP()->value = return_value;
        }

        // continue at the return block
        spidir_builder_set_block(builder, inlinee->return_block);
    } else {
        // gather all the parameters for a call
        values = emit_gather_arguments(builder, callee, stack);

        // first attempt and use a builtin emitter if available
        spidir_value_t return_value = SPIDIR_VALUE_INVALID;
        if (emitter != NULL) {
            return_value = emitter(builder, callee, values);
        } else {
            // lastly, get a direct function to it
            spidir_funcref_t function = jit_get_function(spidir_builder_get_module(builder), callee);
            return_value = spidir_builder_build_call(builder, function, arrlen(values), values);
        }

        // push it if needed
        if (callee->ReturnParameter->ParameterType != tVoid) {
            if (tdn_is_struct_like(callee->ReturnParameter->ParameterType)) {
                return_value = arrlast(values);
            }
            STACK_TOP()->value = return_value;
        }
    }

cleanup:
    arrfree(values);

    return err;
}

static tdn_err_t emit_newobj(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_stack_value_t* args = NULL;
    RuntimeMethodBase callee = inst->operand.method;

    jit_queue_cctor(spidir_builder_get_module(builder), callee->DeclaringType);

    // allocate the item
    spidir_value_t obj = SPIDIR_VALUE_INVALID;
    if (tdn_is_struct_like(callee->DeclaringType)) {
        obj = spidir_builder_build_stackslot(builder,
            callee->DeclaringType->StackSize, callee->DeclaringType->StackAlignment);
        jit_emit_bzero(builder, obj, callee->DeclaringType);

    } else if (callee->DeclaringType == tString) {
        // we initialize the type, ensure we have the full vtable available
        jit_queue_type(spidir_builder_get_module(builder), callee->DeclaringType);

        // we implement the string constructors by calling Ctor methods that get the same
        // args but return a string object (they usually use FastAllocateString internally)
        RuntimeMethodInfo ctor = NULL;
        for (int i = 0; i < tString->DeclaredMethods->Length; i++) {
            RuntimeMethodInfo method = tString->DeclaredMethods->Elements[i];

            // we have special methods called Ctor that are the real "constructor"
            if (!tdn_compare_string_to_cstr(method->Name, "Ctor"))
                continue;

            // match the parameters count
            if (method->Parameters->Length != callee->Parameters->Length)
                continue;

            // match the parameters
            bool match = true;
            for (int j = 0; j < callee->Parameters->Length; j++) {
                if (callee->Parameters->Elements[j]->ParameterType != method->Parameters->Elements[j]->ParameterType) {
                    match = false;
                    break;
                }
            }
            if (!match)
                continue;

            // found a full match, use it
            ctor = method;
            break;
        }
        CHECK(ctor != NULL);

        // we are going to perform a manual call to it instead
        inst->operand.method = (RuntimeMethodBase)ctor;
        CHECK_AND_RETHROW(emit_call(function, builder, block, inst, stack));
        goto cleanup;

    } else {
        // we initialize the type, ensure we have the full vtable available
        jit_queue_type(spidir_builder_get_module(builder), callee->DeclaringType);

        obj = spidir_builder_build_call(builder,
            jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_NEWOBJ), 1,
            (spidir_value_t[]){ spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uintptr_t)callee->DeclaringType) });
    }

    // build a new stack args, including the `this` that we just created
    arrsetlen(args, arrlen(stack) + 1);
    memcpy(args + 1, stack, arrlen(stack) * sizeof(*stack));
    args[0].type = callee->DeclaringType;
    args[0].kind = tdn_type_is_valuetype(callee->DeclaringType) ? JIT_KIND_BY_REF : JIT_KIND_OBJ_REF;
    args[0].value = obj;
    // TODO: mark as a known type

    // perform a normal call instead
    CHECK_AND_RETHROW(emit_call(function, builder, block, inst, args));

    // output the object to the top of the stack
    STACK_TOP()->value = obj;

cleanup:
    arrfree(args);

    return err;
}


static tdn_err_t emit_callvirt(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_t* values = NULL;

    // for a reference type that is constrained we need to deref it first
    if (inst->constrained != NULL && tdn_type_is_gc_pointer(inst->constrained)) {
        stack[0].value = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, stack[0].value);
    }

    // check if we have an emitter that handle this virtual method, this is a bit like
    // performing de-virt but more specific to builtin methods
    jit_builtin_emitter_t emitter = jit_get_builtin_emitter(inst->operand.method);
    if (emitter != NULL) {
        CHECK_AND_RETHROW(emit_call(function, builder, block, inst, stack));
        goto cleanup;
    }

    //
    // attempt to de-virtualize the method, if we could then
    // replace the instruction and call the normal emit_call
    // method
    //
    // if the devirt resulted in a method that is implemented by either ValueType
    // or Object, and the original value is a value type, then we need to perform
    // a box instead and can't call it directly
    //
    RuntimeMethodBase known = jit_devirt_method(&stack[0], inst->operand.method);
    if (known != NULL && tdn_type_is_valuetype(stack[0].type) == tdn_type_is_valuetype(known->DeclaringType)) {
        // TODO: perform explicit null check

        // the devirt was from a delegate, load the this pointer properly
        if (tdn_is_delegate(stack[0].type)) {
            if (known->Attributes.Static) {
                // no need for the `this`
                stack = &stack[1];
            } else {
                // load the this from the delegate
                stack[0].kind = JIT_KIND_OBJ_REF;
                stack[0].type = known->DeclaringType;
                stack[0].value = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
                            spidir_builder_build_ptroff(builder, stack[0].value,
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Instance))));
            }
        }

        // the devirt was into a valuetype, move the pointer
        // forward since this will call the non-thunk version
        if (tdn_type_is_valuetype(known->DeclaringType) && stack[0].kind == JIT_KIND_OBJ_REF) {
            stack[0].kind = JIT_KIND_BY_REF;
            stack[0].type = known->DeclaringType;
            stack[0].value = spidir_builder_build_ptroff(builder, stack[0].value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64,
                    tdn_get_boxed_value_offset(known->DeclaringType)));
        }

        // and now we can perform the direct call
        inst->operand.method = known;
        CHECK_AND_RETHROW(emit_call(function, builder, block, inst, stack));
        goto cleanup;

    }

    // if its constrained but its a valuetype, we need to deref and box it before
    // passing it to the function (the devirt takes care of the case that the type
    // does implement the function)
    if (inst->constrained != NULL && tdn_type_is_valuetype(inst->constrained)) {
        CHECK(known != NULL);
        CHECK(inst->constrained == stack[0].type);

        // we don't actually need to allocate the object because it won't leak
        // from the functions, so we will allocate them on the stack
        RuntimeTypeInfo type = inst->constrained;
        spidir_value_t obj = spidir_builder_build_stackslot(builder,
            type->HeapSize + tdn_get_boxed_value_offset(type),
            MAX(type->HeapAlignment, tObject->HeapAlignment));

        // store the vtable
        ASSERT(offsetof(struct Object, VTable) == 0);
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uintptr_t)type->JitVTable), obj);

        // store the value itself
        spidir_value_t value_ptr = spidir_builder_build_ptroff(builder, obj,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, tdn_get_boxed_value_offset(inst->constrained)));
        jit_emit_memcpy(builder, value_ptr, stack[0].value, inst->constrained);

        // and now we have it as an object ref
        stack[0].kind = JIT_KIND_OBJ_REF;
        stack[0].value = obj;

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
    if (tdn_is_interface(stack[0].type)) {
        vtable_base = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
            spidir_builder_build_ptroff(builder, stack[0].value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable))));

        // adjust the `this` pointer to point to the interface instance
        // and not to the interface on the stack
        STATIC_ASSERT(offsetof(Interface, Instance) == 0);
        values[0] = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, stack[0].value);
    } else {
        vtable_base = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, stack[0].value);
    }

    // now figure the offset of the function in the interface
    size_t base_offset = sizeof(void*) * callee->VTableOffset;

    // calling on an object, take the vtable header into account
    if (!tdn_is_interface(stack[0].type)) {
        base_offset += offsetof(ObjectVTable, Functions);
    }

    // adjust the interface offset to be of the correct interface based on the type
    if (tdn_is_interface(callee_this)) {
        int iface_offset = jit_get_interface_offset(stack[0].type, callee_this);
        ASSERT(iface_offset >= 0);
        base_offset += iface_offset * sizeof(void*);
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
        if (tdn_is_struct_like(callee->ReturnParameter->ParameterType)) {
            return_value = arrlast(values);
        }
        STACK_TOP()->value = return_value;
    }

cleanup:
    arrfree(values);

    return err;
}

static tdn_err_t emit_ret(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
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
            if (tdn_is_struct_like(ret_type)) {
                // use a store to ensure we do any conversions correctly
                spidir_value_t ret_ref = spidir_builder_build_param_ref(builder, arrlen(function->args));
                jit_emit_store(builder, stack, ret_type, ret_ref);
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
// Boxing and casting
//----------------------------------------------------------------------------------------------------------------------

static spidir_value_t jit_emit_type_check(spidir_builder_handle_t builder, spidir_value_t obj, RuntimeTypeInfo target) {
    spidir_value_t vtable = SPIDIR_VALUE_INVALID;

    // load the vtable pointer from the object
    vtable = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, obj);

    // and now check
    if (tdn_is_interface(target)) {
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

bool verifier_can_cast_to(RuntimeTypeInfo this_type, RuntimeTypeInfo other_type);

static tdn_err_t emit_castclass(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(!tdn_type_is_nullable(inst->operand.type));

    if (stack[0].type != NULL) {
        // check if this is an upcast known at compile time,
        // so we can hard-code the validity of this
        bool is_upcast = verifier_can_cast_to(stack[0].type, inst->operand.type);

        // is the downcast impossible, this can only be true when is_upcast is false
        bool is_impossible_downcast = false;
        if (!is_upcast) {
            if (tdn_is_interface(inst->operand.type)) {
                if (stack[0].type->Attributes.Sealed) {
                    // if we are casting sealed object to an interface then it
                    // must be an upcast, or its impossible
                    is_impossible_downcast = true;
                }

            } else if (!verifier_can_cast_to(inst->operand.type, stack[0].type)) {
                // if we are casting an object to another object, and that other object doesn't
                // have the first type in its inheritance tree, then its impossible to get
                // to the second type
                is_impossible_downcast = true;
            }
        }

        // if null then return null
        spidir_block_t type_check = spidir_builder_create_block(builder);
        spidir_block_t invalid = spidir_builder_create_block(builder);
        spidir_block_t cast_null = spidir_builder_create_block(builder);
        spidir_block_t cast = spidir_builder_create_block(builder);
        spidir_block_t valid = spidir_builder_create_block(builder);

        // load the object instance if its an interface
        spidir_value_t instance = stack->value;
        if (tdn_is_interface(stack->type)) {
            STATIC_ASSERT(offsetof(Interface, Instance) == 0);
            instance = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, instance);
        }

        // if its null skip the type check and perform a null cast (eitehr expand to a null
        // interface or do nothing)
        spidir_builder_build_brcond(builder,
            spidir_builder_build_icmp(builder,
                SPIDIR_ICMP_EQ, SPIDIR_TYPE_I32, instance,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0)),
                cast_null, type_check);

        //
        // the type check itself
        // we hard-code known upcast/impossible casts for making spidir's life easier
        //
        spidir_builder_set_block(builder, type_check);
        if (is_upcast) {
            // just go to the cast path, since its valid
            spidir_builder_build_branch(builder, cast);

        } else if (is_impossible_downcast) {
            // just go to the invalid (or cast-null) path
            spidir_builder_build_branch(builder, inst->opcode == CEE_ISINST ? cast_null : invalid);

        } else {
            // we need to make a runtime type verification, if this is an isinst
            // then we are going to return a null instead
            spidir_value_t is_type = jit_emit_type_check(builder, instance, inst->operand.type);
            spidir_builder_build_brcond(builder, is_type,
                cast,
                inst->opcode == CEE_ISINST ? cast_null : invalid);
        }

        //
        // throw invalid cast
        // only needed for castclass (isinst returns null instead)
        //
        if (inst->opcode == CEE_CASTCLASS) {
            spidir_builder_set_block(builder, invalid);
            jit_emit_invalid_cast(builder);
        }

        //
        // cast a non-null value
        //
        spidir_value_t cast_value = SPIDIR_VALUE_INVALID;
        if (is_upcast) {
            // upcast is simple
            spidir_builder_set_block(builder, cast);
            cast_value = jit_convert_value(builder, stack, inst->operand.type);
            spidir_builder_build_branch(builder, valid);
        } else if (!is_impossible_downcast) {
            // downcast we may need to perform a bit more stuff
            spidir_builder_set_block(builder, cast);

            // we need to perform a runtime lookup
            if (tdn_is_interface(inst->operand.type)) {
                // downcast to interface
                cast_value = spidir_builder_build_stackslot(builder,
                    sizeof(Interface), _Alignof(Interface));

                // store the interface instance
                ASSERT(offsetof(Interface, Instance) == 0);
                spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, instance, cast_value);

                // perform a runtime call to get the interface offset
                spidir_value_t vtable = spidir_builder_build_call(builder,
                    jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_GET_INTERFACE_VTABLE),
                    2, (spidir_value_t[]){
                        instance,
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uintptr_t)inst->operand.type),
                    });

                // and store it into the vtable field
                spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, vtable,
                    spidir_builder_build_ptroff(builder, cast_value,
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable))));

            } else {
                cast_value = instance;
            }
            spidir_builder_build_branch(builder, valid);
        }

        //
        // cast a null value, aka just create a null
        //
        spidir_builder_set_block(builder, cast_null);
        spidir_value_t null_cast_value = SPIDIR_VALUE_INVALID;
        if (tdn_is_interface(inst->operand.type) || tdn_is_delegate(inst->operand.type)) {
            // TODO: cache an interface null value and re-use it when possible
            null_cast_value = spidir_builder_build_stackslot(builder, sizeof(Interface), _Alignof(Interface));
            jit_emit_bzero(builder, null_cast_value, inst->operand.type);
        } else {
            null_cast_value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0);
        }
        spidir_builder_build_branch(builder, valid);

        //
        // The valid path after everything, select the correct value
        //
        spidir_builder_set_block(builder, valid);
        if (is_upcast) {
            STACK_TOP()->value = cast_value;

        } else if (is_impossible_downcast) {
            // always has a null-cast value
            STACK_TOP()->value = null_cast_value;

        } else {
            // might have either, choose
            STACK_TOP()->value = spidir_builder_build_phi(builder, SPIDIR_TYPE_PTR, 2,
                (spidir_value_t[]){
                    cast_value,
                    null_cast_value
                }, NULL);
        }
    } else {
        // hardcode a null cast
        spidir_value_t value = SPIDIR_VALUE_INVALID;
        if (tdn_is_struct_like(inst->operand.type)) {
            // TODO: cache an interface null value and re-use it when possible
            value = spidir_builder_build_stackslot(builder, sizeof(Interface), _Alignof(Interface));
            jit_emit_bzero(builder, value, inst->operand.type);
        } else {
            value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0);
        }
        STACK_TOP()->value = value;
    }

cleanup:
    return err;
}

static tdn_err_t emit_box(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    if (tdn_type_is_nullable(inst->operand.type)) {
        // boxes the value instance
        spidir_block_t is_null = spidir_builder_create_block(builder);
        spidir_block_t is_not_null = spidir_builder_create_block(builder);
        spidir_block_t done = spidir_builder_create_block(builder);

        RuntimeTypeInfo underlying = inst->operand.type->GenericArguments->Elements[0];

        // Branch into the correct path depending on the hasValue field
        spidir_value_t has_value_ptr = spidir_builder_build_ptroff(builder, stack->value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, tdn_get_nullable_has_offset(inst->operand.type)));
        spidir_value_t has_value = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_1, SPIDIR_TYPE_I32, has_value_ptr);
        spidir_builder_build_brcond(builder, has_value, is_not_null, is_null);

        //
        // Path when the nullable is not null
        //
        spidir_builder_set_block(builder, is_not_null);

        // set the type as a boxed one
        spidir_value_t obj = spidir_builder_build_call(builder,
            jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_NEWOBJ), 1,
            (spidir_value_t[]){ spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uintptr_t)underlying) });

        // get the offset to the value
        spidir_value_t dst_value_ptr = spidir_builder_build_ptroff(builder, obj,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, tdn_get_boxed_value_offset(underlying)));

        // get the offset to the value of the nullable
        spidir_value_t src_value_ptr = spidir_builder_build_ptroff(builder, stack->value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, tdn_get_nullable_value_offset(inst->operand.type)));

        // and we can just copy it nicely
        jit_emit_memcpy(builder, dst_value_ptr, src_value_ptr, underlying);

        spidir_builder_build_branch(builder, done);

        //
        // Path when nullable is null
        //
        spidir_builder_set_block(builder, is_null);
        spidir_value_t null = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0);
        spidir_builder_build_branch(builder, done);

        //
        // And merge again
        //
        spidir_builder_set_block(builder, done);
        STACK_TOP()->value = spidir_builder_build_phi(builder, SPIDIR_TYPE_PTR, 2, (spidir_value_t[]){
            obj, null
        }, NULL);

    } else if (tdn_type_is_gc_pointer(inst->operand.type)) {
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
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, tdn_get_boxed_value_offset(inst->operand.type)));

        // store it
        jit_emit_store(builder, stack, inst->operand.type, value_ptr);

        // point to the instance
        STACK_TOP()->value = obj;

    } else {
        CHECK_FAIL();
    }

cleanup:
    return err;
}

static tdn_err_t emit_unbox_any(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // the operand is a reference type
    if (tdn_type_is_gc_pointer(inst->operand.type)) {
        // perform castclass instead
        inst->opcode = CEE_CASTCLASS;
        CHECK_AND_RETHROW(emit_castclass(function, builder, block, inst, stack));

    } else if (tdn_type_is_nullable(inst->operand.type)) {
        // we either get a null nullable, or a value nullable
        CHECK_FAIL();

    } else if (stack[0].type == NULL) {
        // we have a hard-coded null value, throw a null-reference
        jit_emit_null_reference(builder);

    } else {
        // get the object instance
        spidir_value_t obj = stack[0].value;
        if (tdn_is_interface(stack[0].type)) {
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
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, tdn_get_boxed_value_offset(inst->operand.type)));
        STACK_TOP()->value = jit_emit_load(builder, inst->operand.type, ptr);
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Branching
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t emit_br(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
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

static tdn_err_t emit_br_unary_cond(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);
    jit_block_t* next = jit_function_get_block(function, block->block.end, block->leave_target_stack);
    CHECK(next != NULL);

    // merge with both blocks
    emitter_merge_block(function, builder, block, target);
    emitter_merge_block(function, builder, block, next);

    // interface and delegate are a fat pointer, we need to
    // load the instance/function and check against that
    emit_load_reference(builder, &stack[0]);

    // if this is a reference need to turn into an integer
    if (jit_is_pointer(&stack[0])) {
        stack[0].value = spidir_builder_build_icmp(builder,
            inst->opcode == CEE_BRFALSE ? SPIDIR_ICMP_EQ : SPIDIR_ICMP_NE,
            SPIDIR_TYPE_I32,
            stack[0].value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0));
    } else if (inst->opcode == CEE_BRFALSE) {
        SWAP(target, next);
    }

    // and branch to it
    spidir_builder_build_brcond(builder, stack[0].value, target->spidir_block, next->spidir_block);

cleanup:
    return err;
}



static tdn_err_t emit_br_binary_cond(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // perform the condition
    spidir_value_t cond;
    if (jit_is_float(&stack[0])) {
        if (stack[0].kind != stack[1].kind) {
            if (stack[0].kind == JIT_KIND_F32) {
                stack[0].value = spidir_builder_build_fwiden(builder, stack[0].value);
            }

            if (stack[1].kind == JIT_KIND_F32) {
                stack[1].value = spidir_builder_build_fwiden(builder, stack[1].value);
            }
        }

        cond = emit_float_binary_compare(builder, inst->opcode, stack[0].value, stack[1].value);

    } else {
        bool is_unsigned = inst->opcode == CEE_CGT_UN || inst->opcode == CEE_CLT_UN;
        emit_convert_for_op(builder, &stack[0], &stack[1], is_unsigned, true);
        cond = emit_binary_compare(builder, inst->opcode, stack[0].value, stack[1].value);
    }

    // get the target block
    jit_block_t* target = jit_function_get_block(function, inst->operand.branch_target, block->leave_target_stack);
    CHECK(target != NULL);
    jit_block_t* next = jit_function_get_block(function, block->block.end, block->leave_target_stack);
    CHECK(next != NULL);

    // merge with both blocks
    emitter_merge_block(function, builder, block, target);
    emitter_merge_block(function, builder, block, next);

    // and branch to it
    spidir_builder_build_brcond(builder, cond, target->spidir_block, next->spidir_block);

cleanup:
    return err;
}

static tdn_err_t emit_switch(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    // TODO: once spidir has support for jump tables use that

    spidir_block_t try_next_case = spidir_builder_create_block(builder);

    // get the default case
    jit_block_t* next = jit_function_get_block(function, block->block.end, block->leave_target_stack);
    CHECK(next != NULL);
    emitter_merge_block(function, builder, block, next);

    // start with the simplest check, if its less than the case count start checking cases, otherwise
    // go directly to the default case
    spidir_builder_build_brcond(builder,
        spidir_builder_build_icmp(builder, SPIDIR_ICMP_ULT, SPIDIR_TYPE_I32, stack->value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, arrlen(inst->operand.switch_targets))),
            try_next_case, next->spidir_block);

    for (int i = 0; i < arrlen(inst->operand.switch_targets); i++) {
        // we are the next case
        spidir_builder_set_block(builder, try_next_case);

        jit_block_t* target = jit_function_get_block(function, inst->operand.switch_targets[i], block->leave_target_stack);
        CHECK(target != NULL);
        emitter_merge_block(function, builder, block, target);

        if (i == arrlen(inst->operand.switch_targets) - 1) {
            // we are the last case, we can just jump into the real target
            spidir_builder_build_branch(builder, target->spidir_block);
        } else {
            // create the block for the next case
            try_next_case = spidir_builder_create_block(builder);

            // and emit the check with the current value
            spidir_builder_build_brcond(builder,
            spidir_builder_build_icmp(builder, SPIDIR_ICMP_EQ, SPIDIR_TYPE_I32, stack->value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, i)),
                target->spidir_block, try_next_case);
        }
    }

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

static tdn_err_t emit_leave(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_AND_RETHROW(emit_br(function, builder, block, inst, stack));

cleanup:
    return err;
}

static tdn_err_t emit_endfinally(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_AND_RETHROW(emit_br(function, builder, block, inst, stack));

cleanup:
    return err;
}

static tdn_err_t emit_throw(jit_function_t* function, spidir_builder_handle_t builder, jit_block_t* block, tdn_il_inst_t* inst, jit_stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // call the runtime throw function
    spidir_funcref_t func = jit_get_helper(spidir_builder_get_module(builder), JIT_HELPER_THROW);
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
    [CEE_SIZEOF] = emit_sizeof,

    [CEE_LDARG] = emit_ldarg,
    [CEE_STARG] = emit_starg,
    [CEE_LDARGA] = emit_ldarga,

    [CEE_LDLOC] = emit_ldloc,
    [CEE_STLOC] = emit_stloc,
    [CEE_LDLOCA] = emit_ldloca,

    [CEE_LDFLDA] = emit_ldflda,
    [CEE_LDFLD] = emit_ldfld,
    [CEE_STFLD] = emit_stfld,
    [CEE_LDSFLDA] = emit_ldflda,
    [CEE_LDSFLD] = emit_ldfld,
    [CEE_STSFLD] = emit_stfld,

    [CEE_INITOBJ] = emit_initobj,
    [CEE_LOCALLOC] = emit_localloc,

    [CEE_LDIND_I1] = emit_ldind,
    [CEE_LDIND_U1] = emit_ldind,
    [CEE_LDIND_I2] = emit_ldind,
    [CEE_LDIND_U2] = emit_ldind,
    [CEE_LDIND_I4] = emit_ldind,
    [CEE_LDIND_U4] = emit_ldind,
    [CEE_LDIND_I8] = emit_ldind,
    [CEE_LDIND_I] = emit_ldind,
    [CEE_LDIND_REF] = emit_ldind,
    [CEE_LDIND_R4] = emit_ldind,
    [CEE_LDIND_R8] = emit_ldind,
    [CEE_LDOBJ] = emit_ldind,

    [CEE_STIND_I] = emit_stind,
    [CEE_STIND_I1] = emit_stind,
    [CEE_STIND_I2] = emit_stind,
    [CEE_STIND_I4] = emit_stind,
    [CEE_STIND_I8] = emit_stind,
    [CEE_STIND_REF] = emit_stind,
    [CEE_STIND_R4] = emit_stind,
    [CEE_STIND_R8] = emit_stind,
    [CEE_STOBJ] = emit_stind,

    [CEE_LDNULL] = emit_ldnull,
    [CEE_LDSTR] = emit_ldstr,
    [CEE_LDTOKEN] = emit_ldtoken,
    [CEE_LDC_I4] = emit_ldc_i4,
    [CEE_LDC_I8] = emit_ldc_i8,
    [CEE_LDC_R4] = emit_ldc_r4,
    [CEE_LDC_R8] = emit_ldc_r8,
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
    [CEE_ADD_OVF] = emit_binary_op_ovf,
    [CEE_ADD_OVF_UN] = emit_binary_op_ovf,
    [CEE_SUB_OVF] = emit_binary_op_ovf,
    [CEE_SUB_OVF_UN] = emit_binary_op_ovf,
    [CEE_MUL_OVF] = emit_binary_op_ovf,
    [CEE_MUL_OVF_UN] = emit_binary_op_ovf,

    [CEE_NEG] = emit_neg,
    [CEE_NOT] = emit_not,

    [CEE_SHL] = emit_shift,
    [CEE_SHR] = emit_shift,
    [CEE_SHR_UN] = emit_shift,

    [CEE_CONV_OVF_U1] = emit_conv_ovf_i4,
    [CEE_CONV_OVF_I1] = emit_conv_ovf_i4,
    [CEE_CONV_OVF_U1_UN] = emit_conv_ovf_i4,
    [CEE_CONV_OVF_I1_UN] = emit_conv_ovf_i4,
    [CEE_CONV_OVF_U2] = emit_conv_ovf_i4,
    [CEE_CONV_OVF_I2] = emit_conv_ovf_i4,
    [CEE_CONV_OVF_U2_UN] = emit_conv_ovf_i4,
    [CEE_CONV_OVF_I2_UN] = emit_conv_ovf_i4,
    [CEE_CONV_OVF_U4] = emit_conv_ovf_i4,
    [CEE_CONV_OVF_I4] = emit_conv_ovf_i4,
    [CEE_CONV_OVF_U4_UN] = emit_conv_ovf_i4,
    [CEE_CONV_OVF_I4_UN] = emit_conv_ovf_i4,

    [CEE_CONV_I1] = emit_conv_i4,
    [CEE_CONV_I2] = emit_conv_i4,
    [CEE_CONV_I4] = emit_conv_i4,
    [CEE_CONV_U1] = emit_conv_i4,
    [CEE_CONV_U2] = emit_conv_i4,
    [CEE_CONV_U4] = emit_conv_i4,

    [CEE_CONV_OVF_I8_UN] = emit_conv_ovf_i8,
    [CEE_CONV_OVF_I_UN] = emit_conv_ovf_i8,
    [CEE_CONV_OVF_U8] = emit_conv_ovf_i8,
    [CEE_CONV_OVF_U] = emit_conv_ovf_i8,

    // all of these cases are nops no matter
    // the input and they behave the exact same
    // as the normal converts
    [CEE_CONV_OVF_I8] = emit_conv_i8,
    [CEE_CONV_OVF_I] = emit_conv_i8,
    [CEE_CONV_OVF_U8_UN] = emit_conv_i8,
    [CEE_CONV_OVF_U_UN] = emit_conv_i8,
    [CEE_CONV_I8] = emit_conv_i8,
    [CEE_CONV_U8] = emit_conv_i8,
    [CEE_CONV_I] = emit_conv_i8,
    [CEE_CONV_U] = emit_conv_i8,

    [CEE_CONV_R4] = emit_conv_r4,
    [CEE_CONV_R8] = emit_conv_r8,
    [CEE_CONV_R_UN] = emit_conv_r8,

    [CEE_CEQ] = emit_compare,
    [CEE_CGT] = emit_compare,
    [CEE_CGT_UN] = emit_compare,
    [CEE_CLT] = emit_compare,
    [CEE_CLT_UN] = emit_compare,

    [CEE_NEWARR] = emit_newarr,
    [CEE_LDLEN] = emit_ldlen,
    [CEE_LDELEMA] = emit_ldelema,
    [CEE_LDELEM] = emit_ldelem,
    [CEE_LDELEM_REF] = emit_ldelem,
    [CEE_STELEM] = emit_stelem,
    [CEE_STELEM_REF] = emit_stelem,

    [CEE_LDFTN] = emit_ldftn,
    [CEE_NEWOBJ] = emit_newobj,
    [CEE_CALL] = emit_call,
    [CEE_CALLVIRT] = emit_callvirt,
    [CEE_RET] = emit_ret,

    [CEE_CASTCLASS] = emit_castclass,
    [CEE_ISINST] = emit_castclass,
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
    [CEE_SWITCH] = emit_switch,

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
        jit_local_t* func_arg = &function->args[i];

        // get the param for this slot, if we are inlining assume the value
        // is already stored inside
        spidir_value_t value = SPIDIR_VALUE_INVALID;
        if (!function->inline_depth) {
            value = spidir_builder_build_param_ref(builder, i);
        } else {
            value = block->args[i].value;
        }

        // if we need to spill, spill it now
        if (function->args[i].spilled) {
            spidir_value_t stackslot = spidir_builder_build_stackslot(builder,
                func_arg->type->StackSize, func_arg->type->StackAlignment);

            jit_stack_value_t val = jit_stack_value_create(func_arg->type);
            val.value = value;
            jit_emit_store(builder, &val, func_arg->type, stackslot);
            value = stackslot;
        }

        // and store it
        arg->value = value;
    }

    // initialize and spill locals as needed
    for (int i = 0; i < arrlen(block->locals); i++) {
        jit_block_local_t* local = &block->locals[i];
        jit_local_t* func_local = &function->locals[i];

        bool spilled = function->locals[i].spilled;
        bool zero_initialize = function->locals[i].zero_initialize;

        spidir_value_t value = SPIDIR_VALUE_INVALID;

        // if we need to spill it, or this is a struct like that needs to be zero initialized
        // then allocate a stackslot
        if (spilled || (zero_initialize && tdn_is_struct_like(func_local->type))) {
            value = spidir_builder_build_stackslot(builder,
                func_local->type->StackSize, func_local->type->StackAlignment);

            // we need to zero init it
            if (zero_initialize) {
                jit_emit_bzero(builder, value, func_local->type);
            }
        } else if (zero_initialize) {
            // this is a value type, but we need to zero init it
            spidir_value_type_t type = get_spidir_type(func_local->type);
            if (type == SPIDIR_TYPE_F64) {
                value = spidir_builder_build_fconst64(builder, 0.0f);
            } else {
                value = spidir_builder_build_iconst(builder, get_spidir_type(func_local->type), 0);
            }
        }

        // and store it
        local->value = value;
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
