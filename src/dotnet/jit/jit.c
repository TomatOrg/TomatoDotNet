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

#include "spidir/spidir_debug.h"
#include "dotnet/gc/gc.h"
#include "util/string.h"

/**
 * The spidir module, DON'T USE FROM WITHIN THE BUILDER
 */
static spidir_module_handle_t m_spidir_module;

/**
 * Stack of methods that we need to jit and are already
 * prepared
 */
static RuntimeMethodBase* m_methods_to_jit = NULL;

//
// builtin functions
//

// memory manipulation (mainly used for structs)
static spidir_function_t m_builtin_memset;
static spidir_function_t m_builtin_memcpy;

// GC related
static spidir_function_t m_builtin_gc_new;

// throwing related
static spidir_function_t m_builtin_throw_index_out_of_range_exception;
static spidir_function_t m_builtin_throw_overflow_exception;
static spidir_function_t m_builtin_throw;

// exception control flow related
static spidir_function_t m_builtin_rethrow;
static spidir_function_t m_builtin_get_exception;

// creates a null check by doing a deref without other side effects
static spidir_function_t m_builtin_null_check;

tdn_err_t tdn_jit_init() {
    tdn_err_t err = TDN_NO_ERROR;

    // create the dummy null type
    tNull = GC_NEW(RuntimeTypeInfo);
    CHECK_AND_RETHROW(tdn_create_string_from_cstr("<num>", &tNull->Name));

    m_spidir_module = spidir_module_create();

    //
    // Create built-in types
    //
    m_builtin_memset = spidir_module_create_extern_function(m_spidir_module,
                                         "memset",
                                         SPIDIR_TYPE_PTR,
                                         3, (spidir_value_type_t[]){
                                            SPIDIR_TYPE_PTR,
                                            SPIDIR_TYPE_I32,
                                            SPIDIR_TYPE_I64
                                        });

    m_builtin_memcpy = spidir_module_create_extern_function(m_spidir_module,
                                                "memcpy",
                                                SPIDIR_TYPE_PTR,
                                                3, (spidir_value_type_t[]){
                                                SPIDIR_TYPE_PTR,
                                                SPIDIR_TYPE_PTR,
                                                SPIDIR_TYPE_I64 });

    m_builtin_gc_new = spidir_module_create_extern_function(m_spidir_module,
                                                            "gc_new",
                                                            SPIDIR_TYPE_PTR,
                                                            2, (spidir_value_type_t[]){ SPIDIR_TYPE_PTR, SPIDIR_TYPE_I64 });

    m_builtin_throw_index_out_of_range_exception = spidir_module_create_extern_function(
            m_spidir_module, "throw_index_out_of_range_exception", SPIDIR_TYPE_NONE, 0, NULL);

    m_builtin_throw_overflow_exception = spidir_module_create_extern_function(
            m_spidir_module, "throw_overflow_exception", SPIDIR_TYPE_NONE, 0, NULL);

    m_builtin_throw = spidir_module_create_extern_function(
            m_spidir_module, "throw", SPIDIR_TYPE_NONE,
            1, (spidir_value_type_t[]){ SPIDIR_TYPE_PTR });

    m_builtin_rethrow = spidir_module_create_extern_function(
            m_spidir_module, "rethrow", SPIDIR_TYPE_NONE, 0, NULL);

    m_builtin_get_exception = spidir_module_create_extern_function(
            m_spidir_module, "get_exception", SPIDIR_TYPE_PTR, 0, NULL);

    m_builtin_null_check = spidir_module_create_extern_function(
            m_spidir_module, "null_check", SPIDIR_TYPE_NONE,
            1, (spidir_value_type_t[]){ SPIDIR_TYPE_PTR });

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

static spidir_value_type_t get_spidir_argument_type(RuntimeTypeInfo type) {
    if (
        type == tSByte || type == tByte ||
        type == tInt16 || type == tUInt16 ||
        type == tInt32 || type == tUInt32 ||
        type == tBoolean
    ) {
        return SPIDIR_TYPE_I32;
    } else if (type->BaseType == tEnum) {
        return get_spidir_argument_type(type->EnumUnderlyingType);
    } else if (
        type == tInt64 || type == tUInt64 ||
        type == tIntPtr || type == tUIntPtr
    ) {
        return SPIDIR_TYPE_I64;
    } else if (type == tVoid) {
        return SPIDIR_TYPE_NONE;
    } else if (tdn_type_is_valuetype(type)) {
        // pass by-reference, copied by the caller
        return SPIDIR_TYPE_PTR;
    } else {
        // this is def a pointer
        return SPIDIR_TYPE_PTR;
    }
}


static spidir_value_type_t get_spidir_return_type(RuntimeTypeInfo type) {
    if (
        type == tSByte || type == tByte ||
        type == tInt16 || type == tUInt16 ||
        type == tInt32 || type == tUInt32 ||
        type == tBoolean
    ) {
        return SPIDIR_TYPE_I32;
    } else if (type->BaseType == tEnum) {
        return get_spidir_return_type(type->EnumUnderlyingType);
    } else if (
        type == tInt64 || type == tUInt64 ||
        type == tIntPtr || type == tUIntPtr
    ) {
        return SPIDIR_TYPE_I64;
    } else if (type == tVoid) {
        return SPIDIR_TYPE_NONE;
    } else if (tdn_type_is_valuetype(type)) {
        // argument is passed by reference
        return SPIDIR_TYPE_NONE;
    } else {
        // this is def a pointer
        return SPIDIR_TYPE_PTR;
    }
}

static spidir_mem_size_t get_spidir_mem_size(RuntimeTypeInfo info) {
    switch (info->StackSize) {
        case 1: return SPIDIR_MEM_SIZE_1;
        case 2: return SPIDIR_MEM_SIZE_2;
        case 4: return SPIDIR_MEM_SIZE_4;
        case 8: return SPIDIR_MEM_SIZE_8;
        default: ASSERT(!"get_spidir_mem_size: invalid memory type");
    }
}

static spidir_value_type_t get_spidir_mem_type(RuntimeTypeInfo info) {
    info = tdn_get_intermediate_type(info);
    if (info == tInt32) {
        return SPIDIR_TYPE_I32;
    } else if (info == tInt64 || info == tIntPtr) {
        return SPIDIR_TYPE_I64;
    } else if (tdn_type_is_referencetype(info)) {
        return SPIDIR_TYPE_PTR;
    } else {
        ASSERT(!"get_spidir_mem_type: invalid memory type");
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static tdn_err_t jit_prepare_method(RuntimeMethodBase method, spidir_module_handle_t module);

#define SWAP(a, b) \
    do { \
        typeof(a) __temp = a; \
        a = b; \
        b = __temp; \
    } while (0)

/**
 * Emit a memcpy with a known size
 */
static void jit_emit_memcpy(spidir_builder_handle_t builder, spidir_value_t ptr1, spidir_value_t ptr2, size_t size) {
    spidir_builder_build_call(builder, m_builtin_memcpy, 3,
                              (spidir_value_t[]){
                                      ptr1, ptr2,
                                      spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, size)
                              });
}

/**
 * Resolve the parameter type, taking into account that for non-static arg0 is the this
 */
static tdn_err_t jit_resolve_parameter_type(RuntimeMethodBase method, int arg, RuntimeTypeInfo* type) {
    tdn_err_t err = TDN_NO_ERROR;

    // resolve the argument type
    RuntimeTypeInfo arg_type = NULL;
    if (arg == 0 && !method->Attributes.Static) {
        // non-static method's first arg is the this
        if (tdn_type_is_valuetype(method->DeclaringType)) {
            CHECK_AND_RETHROW(tdn_get_byref_type(method->DeclaringType, &arg_type));
        } else {
            arg_type = method->DeclaringType;
        }

    } else {
        // this is not included in Parameters list
        uint16_t param_arg = arg - (method->Attributes.Static ? 0 : 1);

        // get the correct argument
        CHECK(param_arg < method->Parameters->Length);
        arg_type = method->Parameters->Elements[param_arg]->ParameterType;
    }

    // return it
    *type = arg_type;

cleanup:
    return err;
}

/**
 * Create the labels of all the jump locations in the given region
 */
static tdn_err_t create_region_labels(jit_context_t* ctx, jit_region_t* region) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(region->labels == NULL);

    // get the index ranges
    int start_index = jit_get_label_location_index(ctx, region->pc_start, false);
    int end_index = jit_get_label_location_index(ctx, region->pc_end, false);
    arrsetcap(region->labels, end_index - start_index);

    // get all the labels
    for (int i = start_index; i < end_index; i++) {
        jit_label_location_t* location = &ctx->labels[i];
        if (location->created) {
            continue;
        }
        location->created = true;

        jit_label_t label = {
            .block = spidir_builder_create_block(ctx->builder),
            .needs_phi = location->needs_phi,
            .address = location->pc,
        };
        arrpush(region->labels, label);
    }

    // setup the entry block, if we have a label at the start of it then use that, otherwise
    // just create a new one
    if (arrlen(region->labels) > 0 && region->labels[0].address == region->pc_start) {
        region->entry_block = region->labels[0].block;
    } else {
        region->entry_block = spidir_builder_create_block(ctx->builder);
    }

cleanup:
    return err;
}

/**
 * Resolve the label of a
 *
 */
static tdn_err_t resolve_and_verify_branch_target(
    jit_context_t* ctx,
    jit_region_t* region,
    uint32_t target,
    jit_label_t** out_label
) {
    tdn_err_t err = TDN_NO_ERROR;

    // if we have a branch target make sure we have the target label
    jit_label_t* target_label = jit_get_label(region, target);
    CHECK(target_label != NULL);

    // stack consistency check
    if (target_label->snapshot.initialized) {
        // we have a snapshot, perform a merge as needed, only modify if we have not visited it yet
        CHECK_AND_RETHROW(eval_stack_merge(ctx->builder, &ctx->stack, target_label, !target_label->visited));
    } else {
        // otherwise create a snapshot of our stack
        CHECK_AND_RETHROW(eval_stack_snapshot(ctx->builder, &ctx->stack, target_label));
    }

    // give it back
    *out_label = target_label;

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The jit itself
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Jit a single instruction, this is to separate control flow and exceptions logic from instruction specific
 * logic and make the code more readable
 */
static tdn_err_t jit_instruction(
    jit_context_t* ctx,
    jit_region_t* region,
    tdn_il_inst_t inst,
    uint32_t pc
) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeMethodBase method = ctx->method;
    spidir_builder_handle_t builder = ctx->builder;
    eval_stack_t* stack = &ctx->stack;

    RuntimeTypeInfo* call_args_types = NULL;
    spidir_value_t* call_args_values = NULL;

//
    // the main instruction jitting
    // TODO: split this to multiple functions in different places
    //
    switch (inst.opcode) {
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Arguments
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // load an argument
        case CEE_LDARG: {
            uint16_t argi = inst.operand.variable;
            CHECK(argi < arrlen(ctx->args));
            jit_arg_t* arg = &ctx->args[argi];

            // get the argument we are loading
            RuntimeTypeInfo arg_type = tdn_get_intermediate_type(arg->type);

            if (arg->spilled) {
                // was spilled, this is a stack slot
                if (jit_is_struct_type(arg_type)) {
                    // use memcpy
                    spidir_value_t location;
                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, arg_type, &location));
                    jit_emit_memcpy(builder, location, arg->value, arg_type->StackSize);
                } else {
                    // use a proper load
                    CHECK_AND_RETHROW(eval_stack_push(stack, arg_type,
                                                      spidir_builder_build_load(builder,
                                                                                get_spidir_mem_size(arg->type),
                                                                                get_spidir_mem_type(arg->type),
                                                                                arg->value)));
                }
            } else {
                // was not spilled, this is a param-ref
                if (jit_is_struct_type(arg_type)) {
                    // passed by pointer, memcpy to the stack
                    spidir_value_t location;
                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, arg_type, &location));
                    jit_emit_memcpy(builder, location, arg->value, arg_type->StackSize);
                } else {
                    // just push it
                    stack_meta_t meta = {
                        .came_from_ldarg0 = arg == 0
                    };
                    CHECK_AND_RETHROW(eval_stack_push_with_meta(stack, arg_type, arg->value, meta));
                }
            }
        } break;

        case CEE_STARG: {
            uint16_t argi = inst.operand.variable;
            CHECK(argi < arrlen(ctx->args));
            jit_arg_t* arg = &ctx->args[argi];
            CHECK(arg->spilled);

            RuntimeTypeInfo value_type;
            spidir_value_t value;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // check type
            CHECK(tdn_type_verifier_assignable_to(value_type, arg->type));

            // was spilled, this is a stack slot
            if (jit_is_struct_type(value_type)) {
                // use memcpy
                jit_emit_memcpy(builder, arg->value, value, value_type->StackSize);
            } else {
                // use a proper load
                spidir_builder_build_store(builder,
                                          get_spidir_mem_size(arg->type),
                                          value, arg->value);
            }
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Locals
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // load a local variable
        case CEE_LDLOC: {
            // verify the argument and get the stack type
            int var = inst.operand.variable;
            CHECK(var < arrlen(ctx->locals));

            RuntimeTypeInfo type = method->MethodBody->LocalVariables->Elements[var]->LocalType;
            RuntimeTypeInfo tracked_type = tdn_get_intermediate_type(type);

            if (jit_is_struct_type(type)) {
                // struct type, copy the stack slot to the eval stack
                spidir_value_t loc;
                CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, type, &loc));
                jit_emit_memcpy(builder, loc, ctx->locals[var], type->StackSize);
            } else {
                // not a struct type, load it from the stack slot
                spidir_value_t value = spidir_builder_build_load(builder,
                                                                 get_spidir_mem_size(type),
                                                                 get_spidir_mem_type(type),
                                                                 ctx->locals[var]);
                if (type == tSByte) {
                    value = spidir_builder_build_sfill(builder, 8, value);
                } else if (type == tInt16) {
                    value = spidir_builder_build_sfill(builder, 16, value);
                }
                CHECK_AND_RETHROW(eval_stack_push(stack, tracked_type, value));
            }
        } break;

            // load the pointer to a local variable
        case CEE_LDLOCA: {
            int var = inst.operand.variable;
            CHECK(var < method->MethodBody->LocalVariables->Length);

            RuntimeTypeInfo type = method->MethodBody->LocalVariables->Elements[var]->LocalType;
            type = tdn_get_verification_type(type);
            CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));

            CHECK_AND_RETHROW(eval_stack_push(stack, type, ctx->locals[var]));
        } break;

        // store to a local variable
        case CEE_STLOC: {
            // verify the argument and get the stack type
            int var = inst.operand.variable;
            CHECK(var < method->MethodBody->LocalVariables->Length);

            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // check the type
            RuntimeTypeInfo local_type = method->MethodBody->LocalVariables->Elements[var]->LocalType;
            CHECK(tdn_type_verifier_assignable_to(value_type, local_type));

            if (jit_is_struct_type(value_type)) {
                // struct type, copy the stack slot to the eval stack
                jit_emit_memcpy(builder, ctx->locals[var], value, value_type->StackSize);
            } else {
                // not a struct type, just store it
                spidir_builder_build_store(builder,
                                           get_spidir_mem_size(local_type),
                                           value,
                                           ctx->locals[var]);
            }
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Object related
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // load a field
        case CEE_LDFLD:
        case CEE_LDFLDA: {
            RuntimeFieldInfo field = inst.operand.field;
            bool ldsfld = inst.opcode == CEE_LDFLDA;

            // TODO: check field accessibility

            // pop the item
            spidir_value_t obj;
            RuntimeTypeInfo obj_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &obj_type, &obj, NULL));

            // check this is either an object or a managed pointer
            CHECK(
                obj_type->IsByRef ||
                tdn_type_is_referencetype(obj_type) ||
                (jit_is_struct_type(obj_type) && !ldsfld)
            );

            // TODO: verify the field is contained within the given object

            // get the stack type of the field
            RuntimeTypeInfo field_type = inst.operand.field->FieldType;

            // figure the pointer to the field itself
            spidir_value_t field_ptr;
            if (field->Attributes.Static) {
                // static field
                // TODO: get a pointer to the static field
                CHECK_FAIL();
            } else {
                // instance field
                if (field->FieldOffset == 0) {
                    // field is at offset zero, just load it
                    field_ptr = obj;
                } else {
                    // build an offset to the field
                    field_ptr = spidir_builder_build_ptroff(builder, obj,
                                                            spidir_builder_build_iconst(builder,
                                                                                        SPIDIR_TYPE_I64,
                                                                                        field->FieldOffset));
                }
            }

            if (ldsfld) {
                if (!field->Attributes.Static) {
                    // TODO: emit null check
                }

                // tracks as a managed pointer to the verification type
                RuntimeTypeInfo value_type = tdn_get_verification_type(field_type);
                CHECK_AND_RETHROW(tdn_get_byref_type(value_type, &value_type));

                // for reference to field we don't need the load
                CHECK_AND_RETHROW(eval_stack_push(stack, value_type, field_ptr));
            } else {
                // tracks as the intermediate type
                RuntimeTypeInfo value_type = tdn_get_intermediate_type(field_type);

                // perform the actual load
                if (jit_is_struct_type(field_type)) {
                    // we are copying a struct to the stack
                    spidir_value_t value;
                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, value_type, &value));
                    jit_emit_memcpy(builder, value, field_ptr, field_type->StackSize);
                } else {
                    // we are copying a simpler value
                    spidir_value_t value = spidir_builder_build_load(builder,
                                                                     get_spidir_mem_size(field_type),
                                                                     get_spidir_mem_type(field_type),
                                                                     field_ptr);
                    if (field_type == tSByte) {
                        value = spidir_builder_build_sfill(builder, 8, value);
                    } else if (field_type == tInt16) {
                        value = spidir_builder_build_sfill(builder, 16, value);
                    }
                    CHECK_AND_RETHROW(eval_stack_push(stack, value_type, value));
                }
            }
        } break;

        // store to a field
        case CEE_STFLD: {
            RuntimeFieldInfo field = inst.operand.field;

            // TODO: check field accessibility

            // pop the item
            spidir_value_t obj, value;
            RuntimeTypeInfo obj_type, value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &obj_type, &obj, NULL));

            // check this is either an object or a managed pointer
            CHECK(
                obj_type->IsByRef ||
                tdn_type_is_referencetype(obj_type)
            );

            // TODO: verify the field is contained within the given object

            // get the stack type of the field
            RuntimeTypeInfo field_type = inst.operand.field->FieldType;
            CHECK(tdn_type_verifier_assignable_to(value_type, field_type));

            // figure the pointer to the field itself
            spidir_value_t field_ptr;
            if (field->Attributes.Static) {
                // static field
                // TODO: get a pointer to the static field
                CHECK_FAIL();
            } else {
                // instance field
                if (field->FieldOffset == 0) {
                    // field is at offset zero, just load it
                    field_ptr = obj;
                } else {
                    // build an offset to the field
                    field_ptr = spidir_builder_build_ptroff(builder, obj,
                                                            spidir_builder_build_iconst(builder,
                                                                                        SPIDIR_TYPE_I64,
                                                                                        field->FieldOffset));
                }
            }

            // TODO: perform write barrier as needed

            // perform the actual store
            if (jit_is_struct_type(field_type)) {
                // we are copying a struct to the stack
                jit_emit_memcpy(builder, field_ptr, value, field_type->StackSize);
            } else {
                // we are copying a simpler value
                spidir_builder_build_store(builder,
                                           get_spidir_mem_size(field_type),
                                           value,
                                           field_ptr);
            }
        } break;

        //----------------------------------------------------------------------------------------------------------

        // load a static field
        case CEE_LDSFLD:
        case CEE_LDSFLDA: {
            RuntimeFieldInfo field = inst.operand.field;
            CHECK(field->Attributes.Static);
            bool ldsflda = inst.opcode == CEE_LDSFLDA;

            // TODO: check field accessibility

            // get the stack type of the field
            RuntimeTypeInfo field_type = inst.operand.field->FieldType;

            // figure the pointer to the field itself
            // TODO: get a pointer to the static field
            spidir_value_t field_ptr;
            CHECK_FAIL();

            if (ldsflda) {
                // tracks as a managed pointer to the verification type
                RuntimeTypeInfo value_type = tdn_get_verification_type(field_type);
                CHECK_AND_RETHROW(tdn_get_byref_type(value_type, &value_type));

                // for reference to field we don't need the load
                CHECK_AND_RETHROW(eval_stack_push(stack, value_type, field_ptr));
            } else {
                // tracks as the intermediate type
                RuntimeTypeInfo value_type = tdn_get_intermediate_type(field_type);

                // perform the actual load
                if (jit_is_struct_type(field_type)) {
                    // we are copying a struct to the stack
                    spidir_value_t value;
                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, value_type, &value));
                    jit_emit_memcpy(builder, value, field_ptr, field_type->StackSize);
                } else {
                    // we are copying a simpler value
                    spidir_value_t value = spidir_builder_build_load(builder,
                                                                     get_spidir_mem_size(field_type),
                                                                     get_spidir_mem_type(field_type),
                                                                     field_ptr);
                    if (field_type == tSByte) {
                        value = spidir_builder_build_sfill(builder, 8, value);
                    } else if (field_type == tInt16) {
                        value = spidir_builder_build_sfill(builder, 16, value);
                    }
                    CHECK_AND_RETHROW(eval_stack_push(stack, value_type, value));
                }
            }
        } break;

        // store to a static field
        case CEE_STSFLD: {
            RuntimeFieldInfo field = inst.operand.field;
            CHECK(field->Attributes.Static);

            // TODO: check field accessibility

            // pop the item
            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // get the stack type of the field
            RuntimeTypeInfo field_type = inst.operand.field->FieldType;
            CHECK(tdn_type_verifier_assignable_to(value_type, field_type));

            // figure the pointer to the field itself
            // TODO: get a pointer to the static field
            spidir_value_t field_ptr;
            CHECK_FAIL();

            // perform the actual store
            if (jit_is_struct_type(field_type)) {
                // we are copying a struct to the stack
                jit_emit_memcpy(builder, field_ptr, value, field_type->StackSize);
            } else {
                // we are copying a simpler value
                spidir_builder_build_store(builder,
                                           get_spidir_mem_size(field_type),
                                           value,
                                           field_ptr);
            }
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Array related
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // load the length of an array
        case CEE_LDLEN: {
            // pop the items
            spidir_value_t array;
            RuntimeTypeInfo array_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &array_type, &array, NULL));

            // push the length, the load automatically zero extends it
            spidir_value_t length_offset = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(struct Array, Length));
            spidir_value_t length_ptr = spidir_builder_build_ptroff(builder, array, length_offset);
            spidir_value_t length = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64, length_ptr);
            CHECK_AND_RETHROW(eval_stack_push(stack, tIntPtr, length));
        } break;

        // load an element from an array
        case CEE_LDELEMA:
        case CEE_LDELEM:
        case CEE_LDELEM_REF: {
            // pop the items
            spidir_value_t index, array;
            RuntimeTypeInfo index_type, array_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &index_type, &index, NULL));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &array_type, &array, NULL));

            // verify the element type
            CHECK(array_type->IsArray);
            RuntimeTypeInfo T = array_type->ElementType;
            RuntimeTypeInfo type = inst.operand.type;
            if (inst.opcode == CEE_LDELEM_REF) {
                // figure from the array, must be a pointer type
                CHECK(tdn_type_is_referencetype(T));
                type = T;
            } else {
                // make sure the wanted type matches the array type
                CHECK(tdn_type_array_element_compatible_with(array_type->ElementType, type));
            }

            // verify the index type
            CHECK(index_type == tInt32 || index_type == tIntPtr);

            // sign extend the index to a 64bit value
            if (index_type == tInt32) {
                index = spidir_builder_build_iext(builder, index);
                index = spidir_builder_build_sfill(builder, 32, index);
            }

            spidir_block_t length_is_valid = spidir_builder_create_block(builder);
            spidir_block_t length_is_invalid = spidir_builder_create_block(builder);

            // length check, this will also take care of the NULL check since if
            // we have a null value we will just fault in here
            spidir_value_t length_offset = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(struct Array, Length));
            spidir_value_t length_ptr = spidir_builder_build_ptroff(builder, array, length_offset);
            spidir_value_t length = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64, length_ptr);

            // make sure the index < length
            spidir_value_t length_check = spidir_builder_build_icmp(builder, SPIDIR_ICMP_SLT, SPIDIR_TYPE_I32, index, length);
            spidir_builder_build_brcond(builder, length_check, length_is_valid, length_is_invalid);

            // perform the invalid length branch, throw an exception
            spidir_builder_set_block(builder, length_is_invalid);
            spidir_builder_build_call(builder, m_builtin_throw_index_out_of_range_exception, 0, NULL);
            spidir_builder_build_unreachable(builder);

            // perform the valid length branch, load the value from the array
            spidir_builder_set_block(builder, length_is_valid);
            spidir_value_t element_size = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, type->StackSize);
            spidir_value_t array_size = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, tArray->HeapSize);
            spidir_value_t byte_offset = spidir_builder_build_imul(builder, element_size, index);
            spidir_value_t abs_offset = spidir_builder_build_iadd(builder, array_size, byte_offset);
            spidir_value_t element_ptr = spidir_builder_build_ptroff(builder, array, abs_offset);

            if (inst.opcode == CEE_LDELEMA) {
                RuntimeTypeInfo tracked = tdn_get_verification_type(type);
                CHECK_AND_RETHROW(tdn_get_byref_type(tracked, &tracked));

                // just need the address, we don't need an explicit null check
                // since we are going to get a fault on the length check if
                // the array is null
                CHECK_AND_RETHROW(eval_stack_push(stack, tracked, element_ptr));
            } else {
                RuntimeTypeInfo tracked = tdn_get_intermediate_type(type);

                // perform the actual load
                if (jit_is_struct_type(tracked)) {
                    // we are copying a struct to the stack
                    spidir_value_t value;
                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, tracked, &value));
                    jit_emit_memcpy(builder, value, element_ptr, tracked->StackSize);
                } else {
                    // we are copying a simpler value
                    spidir_value_t value = spidir_builder_build_load(builder,
                                                                     get_spidir_mem_size(type),
                                                                     get_spidir_mem_type(type),
                                                                     element_ptr);
                    if (type == tSByte) {
                        value = spidir_builder_build_sfill(builder, 8, value);
                    } else if (type == tInt16) {
                        value = spidir_builder_build_sfill(builder, 16, value);
                    }
                    CHECK_AND_RETHROW(eval_stack_push(stack, tracked, value));
                }
            }
        } break;

            // load an element from an array
        case CEE_STELEM:
        case CEE_STELEM_REF: {
            // pop the items
            spidir_value_t value, index, array;
            RuntimeTypeInfo value_type, index_type, array_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &index_type, &index, NULL));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &array_type, &array, NULL));

            // verify the array type
            CHECK(array_type->IsArray);
            RuntimeTypeInfo T = array_type->ElementType;
            if (inst.opcode == CEE_LDELEM_REF) {
                // figure from the array, must be a pointer type
                CHECK(tdn_type_is_referencetype(T));
                CHECK(tdn_type_array_element_compatible_with(value_type, T));
            } else {
                CHECK(tdn_type_array_element_compatible_with(value_type, inst.operand.type));
                CHECK(tdn_type_array_element_compatible_with(inst.operand.type, T));
            }

            // verify the index type
            CHECK(index_type == tInt32 || index_type == tIntPtr);

            // sign extend the index to a 64bit value
            if (index_type == tInt32) {
                index = spidir_builder_build_iext(builder, index);
                index = spidir_builder_build_sfill(builder, 32, index);
            }

            spidir_block_t length_is_valid = spidir_builder_create_block(builder);
            spidir_block_t length_is_invalid = spidir_builder_create_block(builder);

            // length check, this will also take care of the NULL check since if
            // we have a null value we will just fault in here
            spidir_value_t length_offset = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(struct Array, Length));
            spidir_value_t length_ptr = spidir_builder_build_ptroff(builder, array, length_offset);
            spidir_value_t length = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64, length_ptr);

            // make sure the index < length
            spidir_value_t length_check = spidir_builder_build_icmp(builder, SPIDIR_ICMP_SLT, SPIDIR_TYPE_I32, index, length);
            spidir_builder_build_brcond(builder, length_check, length_is_valid, length_is_invalid);

            // perform the invalid length branch, throw an exception
            spidir_builder_set_block(builder, length_is_invalid);
            spidir_builder_build_call(builder, m_builtin_throw_index_out_of_range_exception, 0, NULL);
            spidir_builder_build_unreachable(builder);

            // perform the valid length branch, load the value from the array
            spidir_builder_set_block(builder, length_is_valid);
            spidir_value_t element_size = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, T->StackSize);
            spidir_value_t array_size = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, tArray->HeapSize);
            spidir_value_t byte_offset = spidir_builder_build_imul(builder, element_size, index);
            spidir_value_t abs_offset = spidir_builder_build_iadd(builder, array_size, byte_offset);
            spidir_value_t element_ptr = spidir_builder_build_ptroff(builder, array, abs_offset);

            // TODO: perform write barrier as needed

            // perform the actual store
            if (jit_is_struct_type(T)) {
                // we are copying a struct to the stack
                jit_emit_memcpy(builder, element_ptr, value, T->StackSize);
            } else {
                // we are copying a simpler value
                spidir_builder_build_store(builder,
                                           get_spidir_mem_size(T),
                                           value,
                                           element_ptr);
            }
        } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Control flow
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

        case CEE_NEWOBJ:
        case CEE_CALL:
        case CEE_CALLVIRT: {
            RuntimeMethodBase target = inst.operand.method;
            bool is_static = target->Attributes.Static;
            bool is_call = inst.opcode == CEE_CALL;
            bool is_callvirt = inst.opcode == CEE_CALLVIRT;
            bool is_newobj = inst.opcode == CEE_NEWOBJ;
            size_t object_size = 0;

            // TODO: check method accessibility

            // verify we can call it
            CHECK(!target->Attributes.Abstract);
            if (is_newobj) {
                CHECK(!target->Attributes.Static);
                CHECK(target->Attributes.RTSpecialName);

                // TODO: for strings we need to calculate the string size
                object_size = target->DeclaringType->HeapSize;

            } else if (is_callvirt) {
                CHECK(!target->Attributes.Static);
            }

            // get all the arguments
            int call_args_count = target->Parameters->Length + (is_static ? 0 : 1);
            arrsetlen(call_args_types, call_args_count);
            arrsetlen(call_args_values, call_args_count);
            for (int i = call_args_count - 1; i >= 0; i--) {
                if (is_newobj && i == 0) {
                    // special case for the first argument for newobj, we need to allocate it, either
                    // on the stack if its a value type or on the heap if its a reference type
                    RuntimeTypeInfo target_this_type;
                    spidir_value_t obj = SPIDIR_VALUE_INVALID;
                    if (tdn_type_is_referencetype(target->DeclaringType)) {
                        // call gc_new to allocate it
                        target_this_type = target->DeclaringType;
                        obj = spidir_builder_build_call(builder, m_builtin_gc_new, 2, (spidir_value_t[]){
                            spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)target_this_type),
                            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, object_size)
                        });
                        CHECK_AND_RETHROW(eval_stack_push(stack, target->DeclaringType, obj));
                    } else {
                        // allocate it on the stack
                        CHECK_AND_RETHROW(tdn_get_byref_type(target->DeclaringType, &target_this_type));
                        CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, target->DeclaringType, &obj));
                    }

                    call_args_types[i] = target_this_type;
                    call_args_values[i] = obj;
                    break;
                }

                // pop it
                stack_meta_t meta;
                CHECK_AND_RETHROW(eval_stack_pop(stack, &call_args_types[i], &call_args_values[i], &meta));

                // validate the stack type
                RuntimeTypeInfo target_type;
                if (!is_static) {
                    if (i == 0) {
                        // if we are in a callvirt update the target
                        // to be from the instance type, this will
                        // help us to perform a de-virt in case the
                        // class is either sealed or the method is final
                        if (is_callvirt) {
                            // TODO: this
                        }

                        // figure the correct one for value types
                        if (tdn_type_is_valuetype(target->DeclaringType)) {
                            CHECK_AND_RETHROW(tdn_get_byref_type(target->DeclaringType, &target_type));
                        } else {
                            target_type = target->DeclaringType;
                        }

                        if (is_call) {
                            // TODO: verify that we don't bypass the override of a function
                            //       unless we are part of the class tree
                        } else if (
                            is_callvirt &&
                            (
                                !target->Attributes.Virtual || // method is not virtual
                                target->DeclaringType->Attributes.Sealed || // the type on the stack is sealed
                                (target->Attributes.Virtual && target->Attributes.Final) // the method is final
                            )
                        ) {
                            spidir_builder_build_call(builder, m_builtin_null_check, 1,
                                                      (spidir_value_t[]){ call_args_values[0] });
                        }
                    } else {
                        target_type = target->Parameters->Elements[i - 1]->ParameterType;
                    }
                } else {
                    target_type = target->Parameters->Elements[i]->ParameterType;
                }

                // check that we can do the assignment
                CHECK(tdn_type_verifier_assignable_to(call_args_types[i], target_type));
            }

            // TODO: indirect calls, de-virt

            // make sure that we can actually call the target
            CHECK_AND_RETHROW(jit_prepare_method(target, spidir_builder_get_module(builder)));

            RuntimeTypeInfo ret_type = tdn_get_intermediate_type(target->ReturnParameter->ParameterType);

            // for struct instances allocate and push it beforehand
            if (ret_type != tVoid && jit_is_struct_type(ret_type)) {
                // need to allocate the space in the caller, insert it as the first argument, already pushed
                // to the stack
                CHECK(!is_newobj);
                spidir_value_t ret_buffer;
                CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, ret_type, &ret_buffer));
                        arrins(call_args_values, 0, ret_buffer);
            }

            // emit the actual call
            spidir_value_t value = spidir_builder_build_call(builder,
                                                             (spidir_function_t){ target->JitMethodId },
                                                             arrlen(call_args_values), call_args_values);

            // for primitive types push it now
            if (ret_type != tVoid && !jit_is_struct_type(ret_type)) {
                CHECK(!is_newobj);
                CHECK_AND_RETHROW(eval_stack_push(stack, ret_type, value));
            }
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Control flow
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // unconditional branch
        case CEE_BR: {
            // get and validate the label
            jit_label_t* target_label = NULL;
            CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, inst.operand.branch_target, &target_label));

            // a branch, emit the branch
            spidir_builder_build_branch(builder, target_label->block);

            // because we don't fall-through we clear the stack
            eval_stack_clear(stack);
        } break;

        // conditional branches
        case CEE_BRFALSE:
        case CEE_BRTRUE: {
            // pop the item
            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // ECMA-335 doesn't say brtrue takes in anything but
            // O and native int, but I think its just an oversight
            CHECK(
                tdn_type_is_referencetype(value_type) ||
                value_type == tInt32 ||
                value_type == tInt64 ||
                value_type == tIntPtr
            );

            // get the jump locations
            jit_label_t* target_label = NULL;
            CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, inst.operand.branch_target, &target_label));

            jit_label_t* next_label = NULL;
            CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, pc + inst.length, &next_label));

            // choose the target to fit the brcond
            spidir_block_t true_dest;
            spidir_block_t false_dest;
            if (inst.opcode == CEE_BRTRUE) {
                // jump if non-zero
                true_dest = target_label->block;
                false_dest = next_label->block;
            } else {
                // jump if zero
                true_dest = next_label->block;
                false_dest = target_label->block;
            }

            // a branch, emit the branch
            spidir_builder_build_brcond(builder, value, true_dest, false_dest);
        } break;

        // all the different compare and compare-and-branches
        // that we have
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
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value2_type, &value2, NULL));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value1_type, &value1, NULL));

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
                eval_stack_push(stack, tInt32, cmp);
            } else {
                // get the jump locations
                jit_label_t* target_label = NULL;
                CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, inst.operand.branch_target, &target_label));

                jit_label_t* next_label = NULL;
                CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, pc + inst.length, &next_label));

                // a branch, emit the branch
                spidir_builder_build_brcond(builder, cmp, target_label->block, next_label->block);
            }
        } break;

        // return value from the function
        case CEE_RET: {
            // return must be from the root region
            CHECK(region->clause == NULL);

            RuntimeTypeInfo wanted_ret_type = method->ReturnParameter->ParameterType;
            if (wanted_ret_type == tVoid) {
                spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
            } else {
                RuntimeTypeInfo ret_type;
                spidir_value_t ret_value;
                eval_stack_pop(stack, &ret_type, &ret_value, NULL);

                // make sure the type is a valid return target
                CHECK(tdn_type_verifier_assignable_to(ret_type, wanted_ret_type));

                if (jit_is_struct_type(ret_type)) {
                    // returning a struct, need to use the implicit
                    // ret pointer
                    jit_emit_memcpy(builder,
                                    spidir_builder_build_param_ref(builder, 0),
                                    ret_value,
                                    ret_type->StackSize);

                    // and return without a ret value
                    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
                } else {
                    // returning a normal pointer sized thing
                    spidir_builder_build_return(builder, ret_value);
                }
            }

            // eval stack must be empty at this point
            CHECK(arrlen(stack->stack) == 0);
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Exception control flow
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        case CEE_LEAVE: {
            // start with emptying the eval stack
            eval_stack_clear(stack);

            // the target
            uint32_t branch_target = inst.operand.branch_target;

            // if we are in this path make sure we have a clause
            CHECK(region->clause != NULL);

            // and make sure it is not a fault/finally one
            if (region->is_handler) {
                CHECK(
                    region->clause->Flags != COR_ILEXCEPTION_CLAUSE_FINALLY &&
                    region->clause->Flags != COR_ILEXCEPTION_CLAUSE_FAULT
                );
            }
            // TODO: check not within a filter

            // this exits the block, lets figure to where
            jit_label_t* target_label = NULL;
            finally_handler_t* first_handle = NULL;
            finally_handler_t* last_handler = NULL;
            for (int i = arrlen(ctx->regions) - 1; i >= 0; i--) {
                jit_region_t* cur_region = ctx->regions[i];
                if (cur_region->is_handler) continue; // we must leave into another protected block

                if (cur_region->pc_start <= branch_target && branch_target < cur_region->pc_end) {
                    // found the target try-region
                    target_label = jit_get_label(cur_region, branch_target);
                    CHECK(target_label != NULL);
                    break;

                } else if (cur_region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
                    // we need to go through this finally
                    finally_handler_t* path = hmgetp_null(cur_region->finally_handlers->finally_paths, branch_target);
                    if (path == NULL) {
                        jit_region_t* new_region = tdn_host_mallocz(sizeof(jit_region_t));
                        CHECK(new_region != NULL);

                        // create the new region and set
                        // all the fields, including creating
                        // the labels for that run
                        new_region->clause = cur_region->clause;
                        new_region->clause_index = cur_region->clause_index;
                        new_region->pc_start = cur_region->clause->HandlerOffset;
                        new_region->pc_end = cur_region->clause->HandlerOffset + cur_region->clause->HandlerLength;
                        new_region->is_finally_path = true;
                        new_region->is_handler = true;
                        new_region->finally_handlers = cur_region->finally_handlers;
                        CHECK_AND_RETHROW(create_region_labels(ctx, new_region));

                        // create a new handler
                        finally_handler_t new_path = {
                            .region = new_region,
                            .key = branch_target
                        };
                        hmputs(cur_region->finally_handlers->finally_paths, new_path);
                        path = hmgetp_null(cur_region->finally_handlers->finally_paths, branch_target);
                        CHECK(path != NULL);
                    }

                    // if we already have another finally on the stack
                    // set its next block to the current block, also
                    // verify the consistency
                    if (last_handler != NULL) {
                        if (last_handler->region->has_next_block) {
                            CHECK(last_handler->region->next_block.id == path->region->entry_block.id);
                        } else {
                            last_handler->region->next_block = path->region->entry_block;
                            last_handler->region->has_next_block = true;
                        }
                    }

                    last_handler = path;
                    if (first_handle == NULL) first_handle = path;
                }
            }

            // make sure we got a label at all
            CHECK(target_label != NULL);

            // update the last handler to jump into the target label
            if (last_handler != NULL) {
                if (last_handler->region->has_next_block) {
                    CHECK(last_handler->region->next_block.id == target_label->block.id);
                } else {
                    last_handler->region->next_block = target_label->block;
                    last_handler->region->has_next_block = true;
                }
            }

            // if we have a first handle then get the label
            // from that region to jump to
            if (first_handle != NULL) {
                target_label = jit_get_label(first_handle->region, first_handle->region->pc_start);
                if (target_label == NULL) {
                    // no label, so just go into the entry and don't process the target label stuff
                    spidir_builder_build_branch(builder, first_handle->region->entry_block);
                    break;
                }
            }

            // create a branch into the handler, we know that, verify the stack
            // on the way
            if (target_label->snapshot.initialized) {
                CHECK(arrlen(target_label->snapshot.stack) == 0);
            } else {
                target_label->snapshot.initialized = true;
            }

            spidir_builder_build_branch(builder, target_label->block);
        } break;

        // this needs to jump into the next block, for fault and finally's fault path
        // this simply goes into the next handler, for non-faulting finally path this
        // goes into
        case CEE_ENDFINALLY: {
            // must be inside fault or finally
            CHECK(
                region->is_handler &&
                (
                    region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY ||
                    region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT
                )
            );

            // empty the stack
            eval_stack_clear(stack);

            // figure where we need to go
            if (!region->is_finally_path) {
                // get the region of the next handler, skipping
                // the current one
                for (int i = arrlen(ctx->regions) - 2; i >= 0; i--) {
                    if (ctx->regions[i]->is_handler)
                        continue;

                    if (ctx->regions[i]->clause == NULL) {
                        // we got to the root region, meaning there is nothing
                        // else in the fault path to handle, so just rethrow the
                        // error
                        spidir_builder_build_call(builder, m_builtin_rethrow, 0, NULL);
                        spidir_builder_build_unreachable(builder);
                        break;
                    }

                    jit_region_t* handler = &ctx->handler_regions[ctx->regions[i]->clause_index];
                    spidir_builder_build_branch(builder, handler->entry_block);
                }
            } else {
                // we need to go to the next entry finally of this path
                CHECK(region->has_next_block);
                spidir_builder_build_branch(builder, region->next_block);
            }
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Stack manipulation
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // Push an int32 to the stack
        case CEE_LDC_I4: {
            // NOTE: we are treating the value as a uint32 so it will not sign extend it
            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32,
                                              spidir_builder_build_iconst(builder,
                                                                          SPIDIR_TYPE_I32, inst.operand.uint32)));
        } break;

        // Push an int64
        case CEE_LDC_I8: {
            CHECK_AND_RETHROW(eval_stack_push(stack, tInt64,
                                              spidir_builder_build_iconst(builder,
                                                                          SPIDIR_TYPE_I64, inst.operand.uint64)));
        } break;

        // push a string
        case CEE_LDSTR: {
            // TODO: actually create the string or something
            CHECK_AND_RETHROW(eval_stack_push(stack, tString,
                                              spidir_builder_build_iconst(builder,
                                                                          SPIDIR_TYPE_PTR, 0)));
        } break;

        // Push a null object
        case CEE_LDNULL: {
            CHECK_AND_RETHROW(eval_stack_push(stack, tNull,
                                              spidir_builder_build_iconst(builder,
                                                                          SPIDIR_TYPE_PTR, 0)));
        } break;

        // Pop a value and dup it
        case CEE_DUP: {
            // pop the value and ignore it
            RuntimeTypeInfo type;
            spidir_value_t value;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &type, &value, NULL));

            // and now push it twice
            CHECK_AND_RETHROW(eval_stack_push(stack, type, value));
            CHECK_AND_RETHROW(eval_stack_push(stack, type, value));
        } break;

        // Pop a value and ignore it
        case CEE_POP: {
            // pop the value and ignore it
            CHECK_AND_RETHROW(eval_stack_pop(stack, NULL, NULL, NULL));
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Math related
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // shifts
        case CEE_SHL:
        case CEE_SHR:
        case CEE_SHR_UN: {
            // pop the items
            spidir_value_t value, shift_amount;
            RuntimeTypeInfo value_type, shift_amount_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &shift_amount_type, &shift_amount, NULL));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // type check
            CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);
            CHECK(shift_amount_type == tInt32 || shift_amount_type == tIntPtr);

            // perform the operation
            spidir_value_t result_value;
            switch (inst.opcode) {
                case CEE_SHL: result_value = spidir_builder_build_shl(builder, value, shift_amount); break;
                case CEE_SHR: result_value = spidir_builder_build_ashr(builder, value, shift_amount); break;
                case CEE_SHR_UN: result_value = spidir_builder_build_lshr(builder, value, shift_amount); break;
                default: CHECK_FAIL();
            }

            // push it to the stack
            CHECK_AND_RETHROW(eval_stack_push(stack, value_type, result_value));
        } break;

        // binary operations on either integers or floats
        case CEE_ADD:
        case CEE_SUB:
        case CEE_AND:
        case CEE_OR:
        case CEE_XOR:
        case CEE_MUL:
        case CEE_DIV:
        case CEE_DIV_UN:
        case CEE_REM:
        case CEE_REM_UN: {
            // pop the items
            spidir_value_t value1, value2;
            RuntimeTypeInfo value1_type, value2_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value2_type, &value2, NULL));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value1_type, &value1, NULL));

            // figure the type we are going to use
            RuntimeTypeInfo result = NULL;
            if (value1_type == tInt32) {
                if (value2_type == tInt32) {
                    result = tInt32;
                } else if (value2_type == tIntPtr) {
                    CHECK_FAIL(); // TODO: how to expand properly
                    result = tIntPtr;
                } else {
                    CHECK_FAIL();
                }
            } else if (value1_type == tInt64) {
                CHECK(value2_type == tInt64);
                result = tInt64;
            } else if (value1_type == tIntPtr) {
                if (value2_type == tInt32) {
                    CHECK_FAIL(); // TODO: how to expand properly
                    result = tIntPtr;
                } else if (value2_type == tIntPtr) {
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
                case CEE_REM: CHECK_FAIL("TODO: spidir is missing spidir_builder_build_irem");
                case CEE_REM_UN: CHECK_FAIL("TODO: spidir is missing spidir_builder_build_urem");
                default: CHECK_FAIL();
            }

            // push it to the stack
            CHECK_AND_RETHROW(eval_stack_push(stack, result, result_value));
        } break;

        // bitwise not, emulate with value ^ ~0
        case CEE_NOT: {
            // pop the item
            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // type check, also get the ones value for the width
            // for the xor operation
            spidir_value_t ones;
            if (value_type == tInt32) {
                ones = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, (uint32_t)~0u);
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                ones = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, (uint64_t)~0ull);
            } else {
                CHECK_FAIL();
            }

            // create the operation
            spidir_value_t result_value = spidir_builder_build_xor(builder, value, ones);

            // push it to the stack
            CHECK_AND_RETHROW(eval_stack_push(stack, value_type, result_value));
        } break;

        // negation, emulate with 0 - value
        case CEE_NEG: {
            // pop the item
            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // type check, also get the ones value for the width
            // for the xor operation
            spidir_value_type_t type;
            if (value_type == tInt32) {
                type = SPIDIR_TYPE_I32;
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                type = SPIDIR_TYPE_I64;
            } else {
                CHECK_FAIL();
            }

            // TODO: floating point

            // create the operation
            spidir_value_t zero = spidir_builder_build_iconst(builder, type, 0);
            spidir_value_t result_value = spidir_builder_build_isub(builder, zero, value);

            // push it to the stack
            CHECK_AND_RETHROW(eval_stack_push(stack, value_type, result_value));
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Integer conversions
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        case CEE_CONV_I1:
        case CEE_CONV_U1: {
            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // make sure it is an integer type
            CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);

            if (value_type != tInt32) {
                // truncate to a 32bit value
                value = spidir_builder_build_itrunc(builder, value);
            }

            if (inst.opcode == CEE_CONV_U1) {
                value = spidir_builder_build_and(builder, value,
                                                 spidir_builder_build_iconst(builder,
                                                                             SPIDIR_TYPE_I32,
                                                                             0xFF));
            } else {
                value = spidir_builder_build_sfill(builder, 8, value);
            }

            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
        } break;


        case CEE_CONV_I2:
        case CEE_CONV_U2: {
            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // make sure it is an integer type
            CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);

            if (value_type != tInt32) {
                // truncate to a 32bit value
                value = spidir_builder_build_itrunc(builder, value);
            }

            if (inst.opcode == CEE_CONV_U2) {
                value = spidir_builder_build_and(builder, value,
                                                 spidir_builder_build_iconst(builder,
                                                                             SPIDIR_TYPE_I32,
                                                                             0xFFFF));
            } else {
                value = spidir_builder_build_sfill(builder, 16, value);
            }

            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
        } break;

        case CEE_CONV_I4:
        case CEE_CONV_U4: {
            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            if (value_type == tInt32) {
                // nothing to do, push it again
                CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                // truncate to a 32bit value
                value = spidir_builder_build_itrunc(builder, value);
                CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
            } else {
                // invalid type
                CHECK_FAIL();
            }
        } break;

        case CEE_CONV_I:
        case CEE_CONV_I8: {
            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // choose based on the opcode
            RuntimeTypeInfo push_type = inst.opcode == CEE_CONV_I ? tIntPtr : tInt64;

            if (value_type == tInt32) {
                // comes from a 32bit integer, first extend to a 64bit integer
                // and then sign extend it
                value = spidir_builder_build_iext(builder, value);
                value = spidir_builder_build_sfill(builder, 32, value);
                CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                // nothing to do, push it again
                CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
            } else {
                // invalid type
                CHECK_FAIL();
            }
        } break;

            // we can also handle in here the overflow versions because
            // converting an unsigned i32/i64 to an unsigned i64 can never fail
        case CEE_CONV_U:
        case CEE_CONV_U8: {
            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // choose based on the opcode
            RuntimeTypeInfo push_type = inst.opcode == CEE_CONV_U ? tIntPtr : tInt64;

            if (value_type == tInt32) {
                // comes from a 32bit integer, first extend to a 64bit integer
                // and then zero extend it
                value = spidir_builder_build_iext(builder, value);
                value = spidir_builder_build_and(builder, value,
                                                 spidir_builder_build_iconst(builder,
                                                                             SPIDIR_TYPE_I64,
                                                                             0xFFFFFFFF));
                CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                // nothing to do, push it again
                CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
            } else {
                // invalid type
                CHECK_FAIL();
            }
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Type conversions with overflow checking
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
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

        case CEE_CONV_OVF_U1:
        case CEE_CONV_OVF_I1:
        case CEE_CONV_OVF_U1_UN:
        case CEE_CONV_OVF_I1_UN:
        case CEE_CONV_OVF_U2:
        case CEE_CONV_OVF_I2:
        case CEE_CONV_OVF_U2_UN:
        case CEE_CONV_OVF_I2_UN:
        case CEE_CONV_OVF_U4:
        case CEE_CONV_OVF_I4:
        case CEE_CONV_OVF_U4_UN:
        case CEE_CONV_OVF_I4_UN: {
            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // type check
            spidir_value_type_t type;
            uint64_t minus_one;
            if (value_type == tInt32) {
                // special case of nop
                //      uint32 -> uint32
                //      int32 -> int32
                if (inst.opcode == CEE_CONV_OVF_U4_UN || inst.opcode == CEE_CONV_OVF_I4) {
                    CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
                    break;
                }

                type = SPIDIR_TYPE_I32;
                minus_one = (uint32_t)-1;
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                type = SPIDIR_TYPE_I64;
                minus_one = (uint64_t)-1;
            } else {
                CHECK_FAIL();
            }

            spidir_value_t cond;
            if (
                (value_type == tInt32 && inst.opcode == CEE_CONV_OVF_U4) ||
                (inst.opcode == CEE_CONV_OVF_I4_UN)
            ) {
                // special case:
                //      int32 -> uint32
                //      uint32/uint64 -> int32
                // perform a signed positive check
                cond = spidir_builder_build_icmp(builder, SPIDIR_ICMP_SLT, SPIDIR_TYPE_I32,
                                                 spidir_builder_build_iconst(builder, type, minus_one),
                                                 value);
            } else if (
                inst.opcode == CEE_CONV_OVF_I1 ||
                inst.opcode == CEE_CONV_OVF_I2 ||
                inst.opcode == CEE_CONV_OVF_I4
            ) {
                // int32/int64 -> int8
                // int32/int64 -> int16
                // int64 -> int32

                // get the correct bit count
                uint64_t bit_width;
                if (inst.opcode == CEE_CONV_OVF_I1) {
                    bit_width = 8;
                } else if (inst.opcode == CEE_CONV_OVF_I2) {
                    bit_width = 16;
                } else if (inst.opcode == CEE_CONV_OVF_I4) {
                    bit_width = 32;
                } else {
                    CHECK_FAIL();
                }

                // sign extend and check they are still the same
                spidir_value_t signed_value = spidir_builder_build_sfill(builder, bit_width, value);
                cond = spidir_builder_build_icmp(builder, SPIDIR_ICMP_EQ, SPIDIR_TYPE_I32,
                                                 value, signed_value);
            } else {
                // get the correct max value
                uint64_t max_value;
                if (inst.opcode == CEE_CONV_OVF_U1 || inst.opcode == CEE_CONV_OVF_U1_UN) {
                    max_value = 0x100;
                } else if (inst.opcode == CEE_CONV_OVF_U2 || inst.opcode == CEE_CONV_OVF_U2_UN) {
                    max_value = 0x10000;
                } else if (inst.opcode == CEE_CONV_OVF_U4 || inst.opcode == CEE_CONV_OVF_U4_UN) {
                    max_value = 0x100000000;
                } else if (inst.opcode == CEE_CONV_OVF_I1_UN) {
                    max_value = 0x80;
                } else if (inst.opcode == CEE_CONV_OVF_I2_UN) {
                    max_value = 0x8000;
                } else {
                    CHECK_FAIL();
                }

                // perform an unsigned less than check, this is fine for both the signed and unsigned
                // input because we are using twos complement
                cond = spidir_builder_build_icmp(builder, SPIDIR_ICMP_ULT, SPIDIR_TYPE_I32, value,
                                                 spidir_builder_build_iconst(builder, type,
                                                                             max_value));
            }

            // perform the branch
            spidir_block_t valid = spidir_builder_create_block(builder);
            spidir_block_t invalid = spidir_builder_create_block(builder);
            spidir_builder_build_brcond(builder, cond, valid, invalid);

            // invalid path, throw
            spidir_builder_set_block(builder, invalid);
            spidir_builder_build_call(builder, m_builtin_throw_overflow_exception, 0, NULL);
            spidir_builder_build_unreachable(builder);

            // valid path, push the new valid
            spidir_builder_set_block(builder, valid);

            // truncate if came from a bigger value
            if (value_type != tInt32) {
                value = spidir_builder_build_itrunc(builder, value);
            }
            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
        } break;

        case CEE_CONV_OVF_I8:
        case CEE_CONV_OVF_I:
        case CEE_CONV_OVF_U8_UN:
        case CEE_CONV_OVF_U_UN: {
            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // choose the type to push
            RuntimeTypeInfo push_type;
            if (inst.opcode == CEE_CONV_OVF_U_UN || inst.opcode == CEE_CONV_OVF_I) {
                push_type = tIntPtr;
            } else {
                push_type = tInt64;
            }

            // type check
            CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);

            // if the input was 32bit zero/sign extend it,
            // based on the signed -> signed or the unsigned -> unsigned
            if (value_type == tInt32) {
                value = spidir_builder_build_iext(builder, value);
                if (inst.opcode == CEE_CONV_OVF_I8 || inst.opcode == CEE_CONV_OVF_I) {
                    value = spidir_builder_build_sfill(builder, 32, value);
                } else {
                    value = spidir_builder_build_and(builder, value,
                                                     spidir_builder_build_iconst(builder,
                                                                                 SPIDIR_TYPE_I64, 0xFFFFFFFF));
                }
            }

            // just push the same as the wanted type
            CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
        } break;

        case CEE_CONV_OVF_I8_UN:
        case CEE_CONV_OVF_I_UN:
        case CEE_CONV_OVF_U8:
        case CEE_CONV_OVF_U: {
            spidir_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // choose the type to push
            RuntimeTypeInfo push_type;
            if (inst.opcode == CEE_CONV_OVF_U || inst.opcode == CEE_CONV_OVF_I_UN) {
                push_type = tIntPtr;
            } else {
                push_type = tInt64;
            }

            // type check
            spidir_value_type_t type;
            uint64_t minus_one;
            if (value_type == tInt32) {
                if (inst.opcode == CEE_CONV_OVF_I8_UN || inst.opcode == CEE_CONV_OVF_I_UN) {
                    // special case, uint32 -> int64 is always valid
                    goto skip_positive_check;
                }

                type = SPIDIR_TYPE_I32;
                minus_one = (uint32_t)-1;
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                type = SPIDIR_TYPE_I64;
                minus_one = (uint64_t)-1;
            } else {
                CHECK_FAIL();
            }

            // make sure is positive by doing a sign check
            spidir_value_t cond = spidir_builder_build_icmp(builder, SPIDIR_ICMP_SLT, SPIDIR_TYPE_I32,
                                                            spidir_builder_build_iconst(builder, type, minus_one),
                                                            value);

            // perform the branch
            spidir_block_t valid = spidir_builder_create_block(builder);
            spidir_block_t invalid = spidir_builder_create_block(builder);
            spidir_builder_build_brcond(builder, cond, valid, invalid);

            // invalid path, throw
            spidir_builder_set_block(builder, invalid);
            spidir_builder_build_call(builder, m_builtin_throw_overflow_exception, 0, NULL);
            spidir_builder_build_unreachable(builder);

            // valid path, push the new valid
            spidir_builder_set_block(builder, valid);

        skip_positive_check:
            // if the input was 32bit,
            // in this case we only either have unsigned -> signed or signed -> unsigned, in both
            // cases we make sure that the value is positive, so we can just do a normal zero extension
            if (value_type == tInt32) {
                value = spidir_builder_build_iext(builder, value);
                value = spidir_builder_build_and(builder, value,
                                                 spidir_builder_build_iconst(builder,
                                                                             SPIDIR_TYPE_I64, 0xFFFFFFFF));
            }

            // just push the same as the wanted type
            CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Allocation
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        case CEE_NEWARR: {
            spidir_value_t num_elems;
            RuntimeTypeInfo num_elems_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &num_elems_type, &num_elems, NULL));

            // type check
            CHECK(num_elems_type == tInt32 || num_elems_type == tIntPtr);

            // extend to 64bit if needed
            if (num_elems_type == tInt32) {
                num_elems = spidir_builder_build_iext(builder, num_elems);
                num_elems = spidir_builder_build_sfill(builder, 32, num_elems);
            }

            // get the array type we are allocating
            RuntimeTypeInfo array_type = NULL;
            CHECK_AND_RETHROW(tdn_get_array_type(inst.operand.type, &array_type));

            // calculate the array size we will need
            spidir_value_t array_size = spidir_builder_build_imul(builder, num_elems,
                                                                  spidir_builder_build_iconst(builder,
                                                                                              SPIDIR_TYPE_I64,
                                                                                              inst.operand.type->StackSize));
            array_size = spidir_builder_build_iadd(builder, array_size, spidir_builder_build_iconst(builder,
                                                                                                    SPIDIR_TYPE_I64,
                                                                                                    sizeof(struct Array)));

            // call the gc_new to allocate the new object
            spidir_value_t array = spidir_builder_build_call(builder, m_builtin_gc_new, 2, (spidir_value_t[]){
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)array_type), array_size
            });

            // and finally set the length of the array
            spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_4, num_elems,
                                       spidir_builder_build_ptroff(builder, array,
                                                                   spidir_builder_build_iconst(builder,
                                                                                               SPIDIR_TYPE_I64,
                                                                                               offsetof(struct Array, Length))));

            // push the array pointer
            CHECK_AND_RETHROW(eval_stack_push(stack, array_type, array));
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Misc operations
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // push the size in bytes of the given type
        case CEE_SIZEOF: {
            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32,
                                              spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32,
                                                                          inst.operand.type->StackSize)));
        } break;

        case CEE_NOP: {
            // do nothing
        } break;

        default:
            CHECK_FAIL();
    }

cleanup:
    arrfree(call_args_types);
    arrfree(call_args_values);

    return err;
}

typedef struct jit_builder_ctx {
    RuntimeMethodBase method;
    tdn_err_t err;
} jit_builder_ctx_t;

static void jit_method_callback(spidir_builder_handle_t builder, void* _ctx) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_builder_ctx_t* builder_ctx = _ctx;
    RuntimeMethodBase method = builder_ctx->method;
    RuntimeExceptionHandlingClause_Array clauses = method->MethodBody->ExceptionHandlingClauses;

    TRACE("%U::%U", method->DeclaringType->Name, method->Name);

    // setup the context that will be used for this method jitting
    jit_context_t ctx = {
        .method = method,
        .builder = builder,
    };
    eval_stack_t* stack = &ctx.stack;
    stack->max_depth = method->MethodBody->MaxStackSize;

    // take into account the first parameter might be an implicit
    // struct return pointer, we will just check if the stack size
    // is larger than 64bit, which can't be anything other than a
    // struct
    int args_offset = 0;
    if (method->ReturnParameter->ParameterType->StackSize > sizeof(uint64_t)) {
        args_offset = 1;
    }

    // get the this_type for future use
    RuntimeTypeInfo this_type = NULL;
    if (!method->Attributes.Static) {
        this_type = method->DeclaringType;
        if (tdn_type_is_valuetype(this_type)) {
            // this is a valuetype, the this is a reference
            CHECK_AND_RETHROW(tdn_get_byref_type(this_type, &this_type));
        }
    }

    // prepare the argument list by setting up their types, we
    // will later spill and set the value as needed
    int arg_count = (method->Parameters->Length + (this_type == NULL ? 0 : 1));
    arrsetlen(ctx.args, arg_count);
    for (int i = 0; i < arg_count; i++) {
        // resolve the parameter type
        RuntimeTypeInfo type;
        CHECK_AND_RETHROW(jit_resolve_parameter_type(method, i, &type));

        // set it up initially
        ctx.args[i].value = SPIDIR_VALUE_INVALID;
        ctx.args[i].type = type;
        ctx.args[i].spilled = false;
    }

    // create the entry block
    bool created_entry_block = false;
    spidir_block_t entry_block;

    // prepare the locals by allocating their stack slots already
    if (method->MethodBody->LocalVariables != NULL) {
        // using the entry block to zero the variables
        created_entry_block = true;
        entry_block = spidir_builder_create_block(builder);
        spidir_builder_set_block(builder, entry_block);
        spidir_builder_set_entry_block(builder, entry_block);

        // setup the variables
        arrsetlen(ctx.locals, method->MethodBody->LocalVariables->Length);
        for (int i = 0; i < method->MethodBody->LocalVariables->Length; i++) {
            RuntimeLocalVariableInfo var = method->MethodBody->LocalVariables->Elements[i];

            // create it
            ctx.locals[i] = spidir_builder_build_stackslot(builder,
                                                           var->LocalType->StackSize,
                                                           var->LocalType->StackAlignment);

            // clear it
            if (jit_is_struct_type(var->LocalType)) {
                spidir_builder_build_call(builder, m_builtin_memset, 3,
                                          (spidir_value_t[]){
                                                  ctx.locals[i],
                                                  spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, 0),
                                                  spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, var->LocalType->StackSize)
                                          });
            } else {
                spidir_builder_build_store(builder,
                                           get_spidir_mem_size(var->LocalType),
                                           spidir_builder_build_iconst(builder, get_spidir_mem_type(var->LocalType), 0),
                                           ctx.locals[i]);
            }
        }
    }

    //------------------------------------------------------------------------------------------------------------------
    // first pass, find all of the labels, this will
    // also create all the different basic blocks on
    // the way
    //------------------------------------------------------------------------------------------------------------------

    // get the rest of the blocks by creating the labels
    bool has_starg0_or_ldarga0 = false;
    uint32_t pc = 0;
    tdn_il_control_flow_t flow_control = TDN_IL_CF_FIRST;
    while (pc != method->MethodBody->ILSize) {
        // decode instruction
        tdn_il_inst_t inst;
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));
        tdn_normalize_inst(&inst);

        // don't support break for now
        CHECK(inst.control_flow != TDN_IL_CF_BREAK);

        //
        // check if we need to spill an argument
        //
        if (
            inst.opcode == CEE_LDARGA ||
            inst.opcode == CEE_STARG
        ) {
            int arg = inst.operand.variable;
            CHECK(arg < arg_count);
            RuntimeTypeInfo type = ctx.args[arg].type;

            // using the entry block to spill the parameters
            if (!created_entry_block) {
                created_entry_block = true;
                entry_block = spidir_builder_create_block(builder);
                spidir_builder_set_block(builder, entry_block);
                spidir_builder_set_entry_block(builder, entry_block);
            }

            // create a stackslot for the spill
            if (!ctx.args[arg].spilled) {
                ctx.args[arg].value = spidir_builder_build_stackslot(builder, type->StackSize, type->StackAlignment);
                ctx.args[arg].spilled = true;

                // store it, if its a value-type we need to copy it instead
                spidir_value_t param_ref = spidir_builder_build_param_ref(builder, args_offset + arg);
                if (jit_is_struct_type(type)) {
                    jit_emit_memcpy(builder,
                                    ctx.args[arg].value,
                                    param_ref,
                                    type->StackSize);
                } else {
                    spidir_builder_build_store(builder,
                                               get_spidir_mem_size(type),
                                               param_ref,
                                               ctx.args[arg].value);
                }
            }

            // needed for verifying call
            if (arg == 0) {
                has_starg0_or_ldarga0 = true;
            }
        }

        //
        // Handle label creation
        //

        // we have a branch target, create the label at that location
        if (inst.operand_type == TDN_IL_BRANCH_TARGET) {
            jit_add_label_location(&ctx, inst.operand.branch_target);
        }

        // if we are coming from a conditional branch then we can get to here
        // as well, so mark as a label
        if (flow_control == TDN_IL_CF_COND_BRANCH) {
            jit_add_label_location(&ctx, pc);
        }

        // check if we already have a label at this location and the last opcode
        // can flow into this location
        bool has_label = jit_get_label_location_index(&ctx, pc, true) >= 0;
        if (
            has_label &&
            (
                flow_control == TDN_IL_CF_NEXT ||
                flow_control == TDN_IL_CF_CALL ||
                flow_control == TDN_IL_CF_BREAK
            )
        ) {
            jit_add_label_location(&ctx, pc);
        }

        pc += inst.length;
        flow_control = inst.control_flow;
    }

    // finish up the args
    for (int i = 0; i < arrlen(ctx.args); i++) {
        if (!ctx.args[i].spilled) {
            ctx.args[i].value = spidir_builder_build_param_ref(builder, args_offset + i);
        }
    }

    //------------------------------------------------------------------------------------------------------------------
    // The second pass
    //------------------------------------------------------------------------------------------------------------------

    // setup the try and fault handler regions
    size_t region_count = clauses != NULL ? clauses->Length : 0;
    arrsetlen(ctx.protected_regions, region_count);
    arrsetlen(ctx.handler_regions, region_count);

    memset(ctx.protected_regions, 0, region_count * sizeof(jit_region_t));
    memset(ctx.handler_regions, 0, region_count * sizeof(jit_region_t));

    // setup all the regions
    for (int i = 0; i < region_count; i++) {
        RuntimeExceptionHandlingClause c = clauses->Elements[i];

        // if this is a finally we need to create a common finally handlers block
        finally_handlers_t* handler = NULL;
        if (c->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
            handler = tdn_host_mallocz(sizeof(finally_handlers_t));
            CHECK_ERROR(handler != NULL, TDN_ERROR_OUT_OF_MEMORY);
        }

        memset(&ctx.protected_regions[i], 0, sizeof(jit_region_t));
        ctx.protected_regions[i].clause = c;
        ctx.protected_regions[i].clause_index = i;
        ctx.protected_regions[i].pc_start = c->TryOffset;
        ctx.protected_regions[i].pc_end = c->TryOffset + c->TryLength;
        ctx.protected_regions[i].finally_handlers = handler;
        CHECK_AND_RETHROW(create_region_labels(&ctx, &ctx.protected_regions[i]));

        memset(&ctx.handler_regions[i], 0, sizeof(jit_region_t));
        ctx.handler_regions[i].clause = c;
        ctx.handler_regions[i].clause_index = i;
        ctx.handler_regions[i].pc_start = c->HandlerOffset;
        ctx.handler_regions[i].pc_end = c->HandlerOffset + c->HandlerLength;
        ctx.handler_regions[i].is_handler = true;
        ctx.handler_regions[i].finally_handlers = handler;
        CHECK_AND_RETHROW(create_region_labels(&ctx, &ctx.handler_regions[i]));
    }

    // TODO: finally paths

    // finally create the top most region
    jit_region_t root_region = {
        .pc_start = 0,
        .pc_end = method->MethodBody->ILSize,
        .clause = NULL,
        .clause_index = -1,
    };
    CHECK_AND_RETHROW(create_region_labels(&ctx, &root_region));

    // either branch from the entry to the region entry or set the region
    // entry as the actual entry
    if (created_entry_block) {
        spidir_builder_build_branch(builder, root_region.entry_block);
    } else {
        spidir_builder_set_entry_block(builder, root_region.entry_block);
    }

    // and push it as the starting region
    arrpush(ctx.regions, &root_region);

    // the root is the first block
    root_region.has_block = true;
    spidir_builder_set_block(builder, root_region.entry_block);

    // for debug
    int indent = 0;

    // which of the finallys we are looking at
    int finally_path_index = -1;

    pc = 0;
    flow_control = TDN_IL_CF_FIRST;
    while (pc < method->MethodBody->ILSize) {
        // decode the instruction
        tdn_il_inst_t inst;
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));

        //--------------------------------------------------------------------------------------------------------------
        // debug prints
        //--------------------------------------------------------------------------------------------------------------

        for (int i = 0; clauses != NULL && i < clauses->Length; i++) {
            RuntimeExceptionHandlingClause c = clauses->Elements[i];
            if (c->TryOffset == pc) {
                tdn_host_printf("[*] \t\t\t%*s.try\n", indent, "");
                tdn_host_printf("[*] \t\t\t%*s{\n", indent, "");
                indent += 4;

            } else if (c->HandlerOffset == pc) {
                if (c->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
                    string_builder_t temp_builder = {};
                    string_builder_push_type_signature(&temp_builder, c->CatchType);
                    tdn_host_printf("[*] \t\t\t%*scatch %s\n", indent, "", string_builder_build(&temp_builder));
                    string_builder_free(&temp_builder);
                } else if (c->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
                    tdn_host_printf("[*] \t\t\t%*sfinally\n", indent, "");
                } else {
                    CHECK_FAIL();
                }
                tdn_host_printf("[*] \t\t\t%*s{\n", indent, "");
                indent += 4;
            }
        }

        // check if we are getting nested

        tdn_host_printf("[*] \t\t\t%*sIL_%04x: %s", indent, "", pc, tdn_get_opcode_name(inst.opcode));
        switch (inst.operand_type) {
            case TDN_IL_BRANCH_TARGET: tdn_host_printf(" IL_%04x", inst.operand.branch_target); break;
            case TDN_IL_NO_OPERAND: break;
            case TDN_IL_VARIABLE: tdn_host_printf(" %d", inst.operand.variable); break;
            case TDN_IL_INT8: tdn_host_printf(" %d", inst.operand.int8); break;
            case TDN_IL_INT32: tdn_host_printf(" %d", inst.operand.int32); break;
            case TDN_IL_INT64: tdn_host_printf(" %lld", (long long int)inst.operand.int64); break;
            case TDN_IL_FLOAT32: tdn_host_printf(" %f", inst.operand.float32); break;
            case TDN_IL_FLOAT64: tdn_host_printf(" %f", inst.operand.float64); break;

            case TDN_IL_METHOD: {
                string_builder_t tmp_builder = {};
                string_builder_push_method_signature(&tmp_builder, inst.operand.method, true);
                tdn_host_printf(" %s", string_builder_build(&tmp_builder));
                string_builder_free(&tmp_builder);
            } break;

            case TDN_IL_FIELD: {
                string_builder_t tmp_builder = {};
                string_builder_push_type_signature(&tmp_builder, inst.operand.field->DeclaringType);
                tdn_host_printf(" %s::%U", string_builder_build(&tmp_builder), inst.operand.field->Name);
                string_builder_free(&tmp_builder);
            } break;

            case TDN_IL_TYPE: {
                string_builder_t tmp_builder = {};
                string_builder_push_type_signature(&tmp_builder, inst.operand.type);
                tdn_host_printf(" %s", string_builder_build(&tmp_builder));
                string_builder_free(&tmp_builder);
            } break;

            case TDN_IL_STRING: tdn_host_printf(" %U", inst.operand.string); break;
            case TDN_IL_SWITCH: CHECK_FAIL();
        }
        tdn_host_printf("\n");

        //--------------------------------------------------------------------------------------------------------------
        // Actual jit handling
        //--------------------------------------------------------------------------------------------------------------
        jit_region_t* region = arrlast(ctx.regions);
        jit_region_t* new_region = NULL;

        // check if we entered a new region or not
        for (int i = region->clause_index + 1; i < arrlen(ctx.protected_regions); i++) {
            jit_region_t* protected_region = &ctx.protected_regions[i];
            jit_region_t* handler_region = &ctx.handler_regions[i];

            if (pc == protected_region->pc_start) {
                new_region = protected_region;

                // we got into a protected region
                if (
                    flow_control == TDN_IL_CF_NEXT ||
                    flow_control == TDN_IL_CF_CALL ||
                    flow_control == TDN_IL_CF_COND_BRANCH ||
                    flow_control == TDN_IL_CF_BREAK ||
                    flow_control == TDN_IL_CF_FIRST
                ) {
                    // we got a fallthrough, stack must be empty
                    CHECK(arrlen(stack->stack) == 0);

                    // put a branch into this region
                    spidir_builder_build_branch(builder, new_region->entry_block);
                }

                // we will already set it in here
                new_region->has_block = true;
                spidir_builder_set_block(builder, new_region->entry_block);
            } else if (pc == handler_region->pc_start) {
                // for finally we have duplicate paths for each of the valid entries
                // first is the fault path, the rest are the valid paths, get the correct
                // region to pass on
                if (handler_region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
                    if (finally_path_index >= 0) {
                        handler_region = handler_region->finally_handlers->finally_paths[finally_path_index].region;
                    }

                    // increment to the next one
                    finally_path_index++;
                }
                new_region = handler_region;

                // the current region no longer has a block it can use
                region->has_block = false;

                // we got into a fault handler, we are not allowed to fall into it
                CHECK(
                    flow_control == TDN_IL_CF_RETURN ||
                    flow_control == TDN_IL_CF_BRANCH ||
                    flow_control == TDN_IL_CF_THROW
                );

                // make sure it is empty
                CHECK(arrlen(stack->stack) == 0);

                // we will already set it in here
                new_region->has_block = true;
                spidir_builder_set_block(builder, new_region->entry_block);

                // for exception handlers we need to set the first item as the exception
                // TODO: handler filters
                if (handler_region->clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
                    CHECK_AND_RETHROW(eval_stack_push(stack, new_region->clause->CatchType,
                                    spidir_builder_build_call(builder,
                                                              m_builtin_get_exception, 0, NULL)));
                }
            } else {
                // continue on
                continue;
            }

            // means we found something
            break;
        }

        // if we entered a new region
        if (new_region != NULL) {
            // no longer has a block to continue on, must continue from a label or something
            region->has_block = false;
            region = new_region;
            arrpush(ctx.regions, new_region);
        }

        // check if there are more labels
        if (region->label_index < arrlen(region->labels)) {
            // check if we have a new label in here
            if (region->labels[region->label_index].address == pc) {
                CHECK(flow_control != TDN_IL_CF_META); // no label between prefix and opcode
                jit_label_t* current = &region->labels[region->label_index++];
                current->visited = true; // we are now visiting it

                // merge with the current eval stack
                if (current->snapshot.initialized) {
                    // only need to merge if we got here from fallthrough, because otherwise the stack will
                    // be implicitly empty
                    if (
                        flow_control == TDN_IL_CF_NEXT ||
                        flow_control == TDN_IL_CF_BREAK ||
                        flow_control == TDN_IL_CF_CALL ||
                        flow_control == TDN_IL_CF_FIRST
                    ) {
                        CHECK_AND_RETHROW(eval_stack_merge(builder, stack, current, true));
                    }
                } else {
                    CHECK_AND_RETHROW(eval_stack_snapshot(builder, stack, current));
                }

                // if we have full-through then create a branch
                // NOTE: we don't check FIRST since from the first
                //       opcode we don't need this branch
                if (
                    flow_control == TDN_IL_CF_NEXT ||
                    flow_control == TDN_IL_CF_BREAK ||
                    flow_control == TDN_IL_CF_CALL
                ) {
                    spidir_builder_build_branch(builder, current->block);
                }

                // set the current block
                region->has_block = true;
                spidir_builder_set_block(builder, current->block);

                // if this has phis then update the eval stack to have
                // the phi values instead of whatever it currently has
                if (current->needs_phi) {
                    CHECK_AND_RETHROW(eval_stack_update_phis(stack, current));
                }
            } else {
                // make sure the label is after this pc, to make sure we don't cross segments
                CHECK(region->labels[region->label_index].address > pc);
            }
        }

        // convert to spidir
        tdn_normalize_inst(&inst);
        CHECK(region->has_block);
        CHECK_AND_RETHROW(jit_instruction(&ctx, region, inst, pc));

        //--------------------------------------------------------------------------------------------------------------
        // last handling
        //--------------------------------------------------------------------------------------------------------------

        // prepare for next iteration
        flow_control = inst.control_flow;
        pc += inst.length;

        // for verification, these should have no stack after wards
        if (
            flow_control == TDN_IL_CF_THROW ||
            flow_control == TDN_IL_CF_BRANCH ||
            flow_control == TDN_IL_CF_RETURN
        ) {
            CHECK(arrlen(stack->stack) == 0);
        }

        if (pc == region->pc_end) {
            // we got to the end of a region, make sure we can't fallthrough from here,
            // if its the root then allow for return as well
            CHECK(
                flow_control == TDN_IL_CF_BRANCH ||
                flow_control == TDN_IL_CF_THROW ||
                flow_control == TDN_IL_CF_RETURN
            );

            // pop the region and clear it
            arrpop(ctx.regions);

            // finally is duplicated multiple times, the first path is a fault path
            // and the others are success paths, handle them right now if there are
            // more to handle
            if (
                region->is_handler &&
                region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY
            ) {
                if (finally_path_index < hmlen(region->finally_handlers->finally_paths)) {
                    pc = region->pc_start;
                } else {
                    finally_path_index = -1;
                }
            }
        }

        //--------------------------------------------------------------------------------------------------------------
        // debug prints
        //--------------------------------------------------------------------------------------------------------------

        for (int i = 0; clauses != NULL && i < clauses->Length; i++) {
            RuntimeExceptionHandlingClause c = clauses->Elements[i];
            if (c->TryOffset + c->TryLength == pc) {
                indent -= 4;
                tdn_host_printf("[*] \t\t\t%*s} // end .try\n", indent, "");
            } else if (c->HandlerOffset + c->HandlerLength == pc) {
                indent -= 4;
                tdn_host_printf("[*] \t\t\t%*s} // end handler\n", indent, "");
            }
        }
    }

    // make sure we have no more regions in our stack
    CHECK(arrlen(ctx.regions) == 0);

cleanup:
    arrfree(ctx.regions);

    for (int i = 0; i < arrlen(ctx.protected_regions); i++) {
        jit_region_free(&ctx.protected_regions[i]);
    }
    arrfree(ctx.protected_regions);

    for (int i = 0; i < arrlen(ctx.handler_regions); i++) {
        jit_region_free(&ctx.handler_regions[i]);
    }
    arrfree(ctx.handler_regions);

    jit_region_free(&root_region);

    arrfree(ctx.locals);
    arrfree(ctx.args);
    arrfree(ctx.labels);
    eval_stack_free(&ctx.stack);

    // make the error go out
    builder_ctx->err = err;
}

/**
 * Prepares a method for jitting, this essentially creates the spidir function
 * so it will be ready for when we call it
 */
static tdn_err_t jit_prepare_method(RuntimeMethodBase method, spidir_module_handle_t handle) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_type_t* params = NULL;
    string_builder_t builder = {};

    // check if already jitted
    if (method->JitPrepared) {
        goto cleanup;
    }
    method->JitPrepared = 1;

    // TODO: external methods need special handling

    // handle the return value, we have a special case of returning a struct, if it can't be returned
    // by value then it will be returned by passing an implicit pointer
    spidir_value_type_t ret_type = get_spidir_return_type(method->ReturnParameter->ParameterType);
    if (ret_type == SPIDIR_TYPE_NONE && method->ReturnParameter->ParameterType != tVoid) {
        arrpush(params, SPIDIR_TYPE_PTR);
    }

    // handle the `this` argument, its always a
    // pointer (byref for struct types)
    if (!method->Attributes.Static) {
        arrpush(params, SPIDIR_TYPE_PTR);
    }

    // handle the arguments
    for (size_t i = 0; i < method->Parameters->Length; i++) {
        arrpush(params, get_spidir_argument_type(method->Parameters->Elements[i]->ParameterType));
    }

    // generate the name
    string_builder_push_method_signature(&builder, method, true);
    const char* name = string_builder_build(&builder);

    // create the function itself
    spidir_function_t func = spidir_module_create_function(
        handle,
        name, ret_type, arrlen(params), params
    );
    method->JitMethodId = func.id;

    // queue to methods to jit if we didn't start with this already
    if (!method->JitStarted) {
        arrpush(m_methods_to_jit, method);
    }

cleanup:
    string_builder_free(&builder);
    arrfree(params);

    return err;
}

/**
 * Actually jits a method, prepares it if needed
 */
static tdn_err_t jit_method(RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;

    // check if already jitted
    if (method->JitStarted) {
        goto cleanup;
    }
    method->JitStarted = 1;

    // make sure the method is already prepared at this point
    CHECK(method->JitPrepared);

    // now call the builder so we can actually build it
    jit_builder_ctx_t ctx = {
        .method = method,
        .err = TDN_NO_ERROR
    };
    spidir_module_build_function(m_spidir_module,
                                 (spidir_function_t){ method->JitMethodId },
                                 jit_method_callback, &ctx);
    CHECK_AND_RETHROW(ctx.err);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// High-level apis
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t tdn_jit_method(RuntimeMethodBase methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    // prepare it, this should queue the method
    CHECK_AND_RETHROW(jit_prepare_method(methodInfo, m_spidir_module));

    // and now dequeue all the methods we need to jit
    while (arrlen(m_methods_to_jit) != 0) {
        RuntimeMethodBase method = arrpop(m_methods_to_jit);
        CHECK_AND_RETHROW(jit_method(method));
    }

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
