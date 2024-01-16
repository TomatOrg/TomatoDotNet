#include "tomatodotnet/types/type.h"
#include "tomatodotnet/jit/jit.h"
#include "tomatodotnet/disasm.h"

#include "util/except.h"
#include "util/stb_ds.h"
#include "util/string_builder.h"
#include "jit_internal.h"

// TODO: lock

#include <stdbool.h>

#include <libfirm/firm.h>
#include "dotnet/gc/gc.h"
#include "util/string.h"

/**
 * Stack of methods that we need to jit and are already
 * prepared
 */
static RuntimeMethodBase* m_methods_to_jit = NULL;

static struct {
    RuntimeTypeInfo key;
    ir_type* value;
}* m_firm_type_map = NULL;

static ir_type* get_ir_type(RuntimeTypeInfo type);

static ir_type* get_class_ir_type(RuntimeTypeInfo type) {
    ir_type* typ = get_ir_type(type);
    if (tdn_type_is_referencetype(type)) {
        typ = get_pointer_points_to_type(typ);
    }
    return typ;
}

static ir_type* init_class_type(RuntimeTypeInfo type) {
    // create the ident
    string_builder_t builder = {0};
    string_builder_push_type_signature(&builder, type);
    ident* ident = new_id_from_str(string_builder_build(&builder));
    string_builder_free(&builder);

    // create the type
    // TODO: do we maybe want both to be a class? for type checking?
    ir_type* typ = jit_is_struct_type(type) ? new_type_struct(ident) : new_type_class(ident);

    ir_type* ptrtyp = NULL;
    if (!jit_is_struct_type(type)) {
        ptrtyp = new_type_pointer(typ);
        hmput(m_firm_type_map, type, ptrtyp);

        // Set the base type
        if (type->BaseType != NULL) {
            add_class_supertype(typ, get_class_ir_type(type->BaseType));
        }
    }


    // TODO: set the interfaces nicely

    // set the declared fields properly
    for (int i = 0; i < type->DeclaredFields->Length; i++) {
        RuntimeFieldInfo field = type->DeclaredFields->Elements[i];
        if (field->Attributes.Static) continue;
        ir_type* field_type = get_ir_type(field->FieldType);

        memset(&builder, 0, sizeof(builder));
        string_builder_push_string(&builder, field->Name);
        ident = new_id_from_str(string_builder_build(&builder));
        string_builder_free(&builder);

        ir_entity* field_entity = new_entity(typ, ident, field_type);
        set_entity_offset(field_entity, field->FieldOffset);
    }

    // setup the type alignment correctly
    set_type_alignment(typ, type->HeapAlignment);
    set_type_size(typ, type->HeapSize);
    set_type_state(typ, layout_fixed);

    return ptrtyp == NULL ? typ : ptrtyp;
}

ir_type* get_ir_type(RuntimeTypeInfo type) {
    ASSERT(type != NULL);

    if (type == tVoid) {
        return NULL;
    }

    ir_type* typ = hmget(m_firm_type_map, type);
    if (typ == NULL) {
        if (type == tSByte) {
            typ = new_type_primitive(mode_Bs);
            hmput(m_firm_type_map, type, typ);
        } else if (type == tByte) {
            typ = new_type_primitive(mode_Bu);
            hmput(m_firm_type_map, type, typ);
        } else if (type == tInt16) {
            typ = new_type_primitive(mode_Hs);
            hmput(m_firm_type_map, type, typ);
        } else if (type == tUInt16) {
            typ = new_type_primitive(mode_Hu);
            hmput(m_firm_type_map, type, typ);
        } else if (type == tInt32) {
            typ = new_type_primitive(mode_Is);
            hmput(m_firm_type_map, type, typ);
        } else if (type == tUInt32) {
            typ = new_type_primitive(mode_Iu);
            hmput(m_firm_type_map, type, typ);
        } else if (type == tInt64) {
            typ = new_type_primitive(mode_Ls);
            hmput(m_firm_type_map, type, typ);
        } else if (type == tUInt64) {
            typ = new_type_primitive(mode_Lu);
            hmput(m_firm_type_map, type, typ);
        } else if (type->IsPointer || type->IsByRef) {
            if (type->ElementType == tVoid) {
                typ = new_type_primitive(mode_P);
                        hmput(m_firm_type_map, type, typ);
            } else {
                typ = new_type_pointer(get_ir_type(type->ElementType));
                        hmput(m_firm_type_map, type, typ);
            }
        } else if (type->BaseType == tEnum) {
            typ = get_ir_type(type->EnumUnderlyingType);
        } else {
            typ = init_class_type(type);
        }
    }
    return typ;
}

tdn_err_t tdn_jit_init() {
    tdn_err_t err = TDN_NO_ERROR;

    // create the dummy null type
    tNull = GC_NEW(RuntimeTypeInfo);
    CHECK_AND_RETHROW(tdn_create_string_from_cstr("<null>", &tNull->Name));

    ir_init();
    set_optimize(5);

cleanup:
    return err;
}

void tdn_jit_dump() {
//    FILE* f = fopen("/home/tomato/projects/tinydotnet/test.vcg", "w");
//    dump_typegraph(f);
//    fclose(f);
    dump_all_ir_graphs("");
    be_main(stdout, "test");
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// JIT helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//static tdn_err_t jit_prepare_method(RuntimeMethodBase method);
//
//#define SWAP(a, b) \
//    do { \
//        typeof(a) __temp = a; \
//        a = b; \
//        b = __temp; \
//    } while (0)

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
            .block = new_immBlock(),
            .address = location->pc,
        };
        arrpush(region->labels, label);
    }

    // setup the entry block, if we have a label at the start of it then use that, otherwise
    // just get the current one
    if (arrlen(region->labels) > 0 && region->labels[0].address == region->pc_start) {
        region->entry_block = region->labels[0].block;
    } else {
        region->entry_block = new_immBlock();
    }

cleanup:
    return err;
}

///**
// * Resolve the label of a
// *
// */
//static tdn_err_t resolve_and_verify_branch_target(
//    jit_context_t* ctx,
//    jit_region_t* region,
//    uint32_t target,
//    jit_label_t** out_label
//) {
//    tdn_err_t err = TDN_NO_ERROR;
//
//    // if we have a branch target make sure we have the target label
//    jit_label_t* target_label = jit_get_label(region, target);
//    CHECK(target_label != NULL);
//
//    // stack consistency check
//    if (target_label->snapshot.initialized) {
//        // we have a snapshot, perform a merge as needed, only modify if we have not visited it yet
//        CHECK_AND_RETHROW(eval_stack_merge(ctx->builder, &ctx->stack, target_label, !target_label->visited));
//    } else {
//        // otherwise create a snapshot of our stack
//        CHECK_AND_RETHROW(eval_stack_snapshot(ctx->builder, &ctx->stack, target_label));
//    }
//
//    // give it back
//    *out_label = target_label;
//
//cleanup:
//    return err;
//}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The jit itself
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static ir_mode* get_stack_mode(ir_type* typ) {
    ir_mode* mode = get_type_mode(typ);
    if (mode == mode_Bs || mode == mode_Bu || mode == mode_Hs || mode == mode_Hu) {
        return mode;
    } else {
        return mode;
    }
}

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
    eval_stack_t* stack = &ctx->stack;

//    RuntimeTypeInfo* call_args_types = NULL;
//    jit_value_t* call_args_values = NULL;

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
                    ASSERT(!"TODO: this");
//                    jit_value_t location;
//                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, arg_type, &location));
//                    jit_emit_memcpy(builder, location, arg->value, arg_type->StackSize);
                } else {
                    // use a proper load
                    ir_type* typ = get_ir_type(arg_type);
                    ir_mode* mode = get_stack_mode(typ);
                    ir_node* load = new_Load(get_store(), arg->value, mode, typ, cons_none);
                    set_store(new_Proj(load, mode_M, pn_Load_M));
                    CHECK_AND_RETHROW(eval_stack_push(stack, arg_type,
                                                      new_Proj(load, mode, pn_Load_res)));
                }
            } else {
                // was not spilled, this is a param-ref
                if (jit_is_struct_type(arg_type)) {
                    // passed by pointer, memcpy to the stack
                    ASSERT(!"TODO: this");
//                    jit_value_t location;
//                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, arg_type, &location));
//                    jit_emit_memcpy(builder, location, arg->value, arg_type->StackSize);
                } else {
                    // just push it
                    stack_meta_t meta = {
                        .came_from_ldarg0 = arg == 0
                    };
                    CHECK_AND_RETHROW(eval_stack_push_with_meta(stack, arg_type, arg->value, meta));
                }
            }
        } break;

//        case CEE_LDARGA: {
//            uint16_t argi = inst.operand.variable;
//            CHECK(argi < arrlen(ctx->args));
//            jit_arg_t* arg = &ctx->args[argi];
//            CHECK(arg->spilled);
//
//            // get the argument we are loading
//            RuntimeTypeInfo arg_type;
//            CHECK_AND_RETHROW(tdn_get_byref_type(arg->type, &arg_type));
//
//            // push the stack slot to the stack
//            CHECK_AND_RETHROW(eval_stack_push(stack, arg_type, arg->value));
//        } break;

        case CEE_STARG: {
            uint16_t argi = inst.operand.variable;
            CHECK(argi < arrlen(ctx->args));
            jit_arg_t* arg = &ctx->args[argi];
            CHECK(arg->spilled);

            RuntimeTypeInfo value_type;
            ir_node* value;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // check type
            CHECK(tdn_type_verifier_assignable_to(value_type, arg->type));

            // was spilled, this is a stack slot
            if (jit_is_struct_type(value_type)) {
                // use memcpy
                ASSERT(!"");
//                jit_emit_memcpy(builder, arg->value, value, value_type->StackSize);
            } else {
                // use a proper load
                ir_node* store = new_Store(get_store(), arg->value, value, get_ir_type(arg->type), cons_none);
                set_store(new_Proj(store, mode_M, pn_Store_M));
            }
        } break;

//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//        // Locals
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//        // load a local variable
//        case CEE_LDLOC: {
//            // verify the argument and get the stack type
//            int var = inst.operand.variable;
//            CHECK(var < arrlen(ctx->locals));
//
//            RuntimeTypeInfo type = method->MethodBody->LocalVariables->Elements[var]->LocalType;
//            RuntimeTypeInfo tracked_type = tdn_get_intermediate_type(type);
//
//            if (jit_is_struct_type(type)) {
//                // struct type, copy the stack slot to the eval stack
//                jit_value_t loc;
//                CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, type, &loc));
//                jit_emit_memcpy(builder, loc, ctx->locals[var], type->StackSize);
//            } else {
//                // not a struct type, load it from the stack slot
//                jit_value_t value = jit_builder_build_load(builder,
//                                                                 get_jit_mem_size(type),
//                                                                 get_jit_mem_type(type),
//                                                                 ctx->locals[var]);
//                if (type == tSByte) {
//                    value = jit_builder_build_sfill(builder, 8, value);
//                } else if (type == tInt16) {
//                    value = jit_builder_build_sfill(builder, 16, value);
//                }
//                CHECK_AND_RETHROW(eval_stack_push(stack, tracked_type, value));
//            }
//        } break;
//
//            // load the pointer to a local variable
//        case CEE_LDLOCA: {
//            int var = inst.operand.variable;
//            CHECK(var < method->MethodBody->LocalVariables->Length);
//
//            RuntimeTypeInfo type = method->MethodBody->LocalVariables->Elements[var]->LocalType;
//            type = tdn_get_verification_type(type);
//            CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));
//
//            CHECK_AND_RETHROW(eval_stack_push(stack, type, ctx->locals[var]));
//        } break;
//
//        // store to a local variable
//        case CEE_STLOC: {
//            // verify the argument and get the stack type
//            int var = inst.operand.variable;
//            CHECK(var < method->MethodBody->LocalVariables->Length);
//
//            jit_value_t value;
//            RuntimeTypeInfo value_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//
//            // check the type
//            RuntimeTypeInfo local_type = method->MethodBody->LocalVariables->Elements[var]->LocalType;
//            CHECK(tdn_type_verifier_assignable_to(value_type, local_type));
//
//            if (jit_is_struct_type(value_type)) {
//                // struct type, copy the stack slot to the eval stack
//                jit_emit_memcpy(builder, ctx->locals[var], value, value_type->StackSize);
//            } else {
//                // not a struct type, just store it
//                jit_builder_build_store(builder,
//                                           get_jit_mem_size(local_type),
//                                           value,
//                                           ctx->locals[var]);
//            }
//        } break;
//
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//        // Object related
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//        // load a field
//        case CEE_LDFLD:
//        case CEE_LDFLDA: {
//            RuntimeFieldInfo field = inst.operand.field;
//            bool ldsfld = inst.opcode == CEE_LDFLDA;
//
//            // TODO: check field accessibility
//
//            // pop the item
//            jit_value_t obj;
//            RuntimeTypeInfo obj_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &obj_type, &obj, NULL));
//
//            // check this is either an object or a managed pointer
//            CHECK(
//                obj_type->IsByRef ||
//                tdn_type_is_referencetype(obj_type) ||
//                (jit_is_struct_type(obj_type) && !ldsfld)
//            );
//
//            // TODO: verify the field is contained within the given object
//
//            // get the stack type of the field
//            RuntimeTypeInfo field_type = inst.operand.field->FieldType;
//
//            // figure the pointer to the field itself
//            jit_value_t field_ptr;
//            if (field->Attributes.Static) {
//                // static field
//                // TODO: get a pointer to the static field
//                CHECK_FAIL();
//            } else {
//                // instance field
//                if (field->FieldOffset == 0) {
//                    // field is at offset zero, just load it
//                    field_ptr = obj;
//                } else {
//                    // build an offset to the field
//                    field_ptr = jit_builder_build_ptroff(builder, obj,
//                                                            jit_builder_build_iconst(builder,
//                                                                                        JIT_TYPE_I64,
//                                                                                        field->FieldOffset));
//                }
//            }
//
//            if (ldsfld) {
//                if (!field->Attributes.Static) {
//                    // TODO: emit null check
//                }
//
//                // tracks as a managed pointer to the verification type
//                RuntimeTypeInfo value_type = tdn_get_verification_type(field_type);
//                CHECK_AND_RETHROW(tdn_get_byref_type(value_type, &value_type));
//
//                // for reference to field we don't need the load
//                CHECK_AND_RETHROW(eval_stack_push(stack, value_type, field_ptr));
//            } else {
//                // tracks as the intermediate type
//                RuntimeTypeInfo value_type = tdn_get_intermediate_type(field_type);
//
//                // perform the actual load
//                if (jit_is_struct_type(field_type)) {
//                    // we are copying a struct to the stack
//                    jit_value_t value;
//                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, value_type, &value));
//                    jit_emit_memcpy(builder, value, field_ptr, field_type->StackSize);
//                } else {
//                    // we are copying a simpler value
//                    jit_value_t value = jit_builder_build_load(builder,
//                                                                     get_jit_mem_size(field_type),
//                                                                     get_jit_mem_type(field_type),
//                                                                     field_ptr);
//                    if (field_type == tSByte) {
//                        value = jit_builder_build_sfill(builder, 8, value);
//                    } else if (field_type == tInt16) {
//                        value = jit_builder_build_sfill(builder, 16, value);
//                    }
//                    CHECK_AND_RETHROW(eval_stack_push(stack, value_type, value));
//                }
//            }
//        } break;
//
//        // store to a field
//        case CEE_STFLD: {
//            RuntimeFieldInfo field = inst.operand.field;
//
//            // TODO: check field accessibility
//
//            // pop the item
//            jit_value_t obj, value;
//            RuntimeTypeInfo obj_type, value_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &obj_type, &obj, NULL));
//
//            // check this is either an object or a managed pointer
//            CHECK(
//                obj_type->IsByRef ||
//                tdn_type_is_referencetype(obj_type)
//            );
//
//            // TODO: verify the field is contained within the given object
//
//            // get the stack type of the field
//            RuntimeTypeInfo field_type = inst.operand.field->FieldType;
//            CHECK(tdn_type_verifier_assignable_to(value_type, field_type));
//
//            // figure the pointer to the field itself
//            jit_value_t field_ptr;
//            if (field->Attributes.Static) {
//                // static field
//                // TODO: get a pointer to the static field
//                CHECK_FAIL();
//            } else {
//                // instance field
//                if (field->FieldOffset == 0) {
//                    // field is at offset zero, just load it
//                    field_ptr = obj;
//                } else {
//                    // build an offset to the field
//                    field_ptr = jit_builder_build_ptroff(builder, obj,
//                                                            jit_builder_build_iconst(builder,
//                                                                                        JIT_TYPE_I64,
//                                                                                        field->FieldOffset));
//                }
//            }
//
//            // TODO: perform write barrier as needed
//
//            // perform the actual store
//            if (jit_is_struct_type(field_type)) {
//                // we are copying a struct to the stack
//                if (field_type->IsUnmanaged) {
//                    jit_emit_memcpy(builder, field_ptr, value, field_type->StackSize);
//                } else {
//                    jit_emit_gc_memcpy(builder, field_type, field_ptr, value);
//                }
//            } else {
//                // we are copying a simpler value
//                jit_builder_build_store(builder,
//                                           get_jit_mem_size(field_type),
//                                           value,
//                                           field_ptr);
//            }
//        } break;
//
//        //----------------------------------------------------------------------------------------------------------
//
//        // load a static field
//        case CEE_LDSFLD:
//        case CEE_LDSFLDA: {
//            RuntimeFieldInfo field = inst.operand.field;
//            CHECK(field->Attributes.Static);
//            bool ldsflda = inst.opcode == CEE_LDSFLDA;
//
//            // TODO: check field accessibility
//
//            // get the stack type of the field
//            RuntimeTypeInfo field_type = inst.operand.field->FieldType;
//
//            // figure the pointer to the field itself
//            // TODO: get a pointer to the static field
//            jit_value_t field_ptr;
//            CHECK_FAIL();
//
//            if (ldsflda) {
//                // tracks as a managed pointer to the verification type
//                RuntimeTypeInfo value_type = tdn_get_verification_type(field_type);
//                CHECK_AND_RETHROW(tdn_get_byref_type(value_type, &value_type));
//
//                // for reference to field we don't need the load
//                CHECK_AND_RETHROW(eval_stack_push(stack, value_type, field_ptr));
//            } else {
//                // tracks as the intermediate type
//                RuntimeTypeInfo value_type = tdn_get_intermediate_type(field_type);
//
//                // perform the actual load
//                if (jit_is_struct_type(field_type)) {
//                    // we are copying a struct to the stack
//                    jit_value_t value;
//                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, value_type, &value));
//                    jit_emit_memcpy(builder, value, field_ptr, field_type->StackSize);
//                } else {
//                    // we are copying a simpler value
//                    jit_value_t value = jit_builder_build_load(builder,
//                                                                     get_jit_mem_size(field_type),
//                                                                     get_jit_mem_type(field_type),
//                                                                     field_ptr);
//                    if (field_type == tSByte) {
//                        value = jit_builder_build_sfill(builder, 8, value);
//                    } else if (field_type == tInt16) {
//                        value = jit_builder_build_sfill(builder, 16, value);
//                    }
//                    CHECK_AND_RETHROW(eval_stack_push(stack, value_type, value));
//                }
//            }
//        } break;
//
//        // store to a static field
//        case CEE_STSFLD: {
//            RuntimeFieldInfo field = inst.operand.field;
//            CHECK(field->Attributes.Static);
//
//            // TODO: check field accessibility
//
//            // pop the item
//            jit_value_t value;
//            RuntimeTypeInfo value_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//
//            // get the stack type of the field
//            RuntimeTypeInfo field_type = inst.operand.field->FieldType;
//            CHECK(tdn_type_verifier_assignable_to(value_type, field_type));
//
//            // figure the pointer to the field itself
//            // TODO: get a pointer to the static field
//            jit_value_t field_ptr;
//            CHECK_FAIL();
//
//            // perform the actual store
//            if (jit_is_struct_type(field_type)) {
//                // we are copying a struct to the stack
//                jit_emit_memcpy(builder, field_ptr, value, field_type->StackSize);
//            } else {
//                // we are copying a simpler value
//                jit_builder_build_store(builder,
//                                           get_jit_mem_size(field_type),
//                                           value,
//                                           field_ptr);
//            }
//        } break;
//
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//        // Array related
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//        // load the length of an array
//        case CEE_LDLEN: {
//            // pop the items
//            jit_value_t array;
//            RuntimeTypeInfo array_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &array_type, &array, NULL));
//
//            // push the length, the load automatically zero extends it
//            jit_value_t length_offset = jit_builder_build_iconst(builder, JIT_TYPE_I64, offsetof(struct Array, Length));
//            jit_value_t length_ptr = jit_builder_build_ptroff(builder, array, length_offset);
//            jit_value_t length = jit_builder_build_load(builder, JIT_MEM_SIZE_4, JIT_TYPE_I64, length_ptr);
//            CHECK_AND_RETHROW(eval_stack_push(stack, tIntPtr, length));
//        } break;
//
//        // load an element from an array
//        case CEE_LDELEMA:
//        case CEE_LDELEM:
//        case CEE_LDELEM_REF: {
//            // pop the items
//            jit_value_t index, array;
//            RuntimeTypeInfo index_type, array_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &index_type, &index, NULL));
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &array_type, &array, NULL));
//
//            // verify the element type
//            CHECK(array_type->IsArray);
//            RuntimeTypeInfo T = array_type->ElementType;
//            RuntimeTypeInfo type = inst.operand.type;
//            if (inst.opcode == CEE_LDELEM_REF) {
//                // figure from the array, must be a pointer type
//                CHECK(tdn_type_is_referencetype(T));
//                type = T;
//            } else {
//                // make sure the wanted type matches the array type
//                CHECK(tdn_type_array_element_compatible_with(array_type->ElementType, type));
//            }
//
//            // verify the index type
//            CHECK(index_type == tInt32 || index_type == tIntPtr);
//
//            // sign extend the index to a 64bit value
//            if (index_type == tInt32) {
//                index = jit_builder_build_iext(builder, index);
//                index = jit_builder_build_sfill(builder, 32, index);
//            }
//
//            jit_block_t length_is_valid = jit_builder_create_block(builder);
//            jit_block_t length_is_invalid = jit_builder_create_block(builder);
//
//            // length check, this will also take care of the NULL check since if
//            // we have a null value we will just fault in here
//            jit_value_t length_offset = jit_builder_build_iconst(builder, JIT_TYPE_I64, offsetof(struct Array, Length));
//            jit_value_t length_ptr = jit_builder_build_ptroff(builder, array, length_offset);
//            jit_value_t length = jit_builder_build_load(builder, JIT_MEM_SIZE_4, JIT_TYPE_I64, length_ptr);
//
//            // make sure the index < length
//            jit_value_t length_check = jit_builder_build_icmp(builder, JIT_ICMP_SLT, JIT_TYPE_I32, index, length);
//            jit_builder_build_brcond(builder, length_check, length_is_valid, length_is_invalid);
//
//            // perform the invalid length branch, throw an exception
//            jit_builder_set_block(builder, length_is_invalid);
//            jit_builder_build_call(builder, m_builtin_throw_index_out_of_range_exception, 0, NULL);
//            jit_builder_build_unreachable(builder);
//
//            // perform the valid length branch, load the value from the array
//            jit_builder_set_block(builder, length_is_valid);
//            jit_value_t element_size = jit_builder_build_iconst(builder, JIT_TYPE_I64, type->StackSize);
//            jit_value_t array_size = jit_builder_build_iconst(builder, JIT_TYPE_I64, tArray->HeapSize);
//            jit_value_t byte_offset = jit_builder_build_imul(builder, element_size, index);
//            jit_value_t abs_offset = jit_builder_build_iadd(builder, array_size, byte_offset);
//            jit_value_t element_ptr = jit_builder_build_ptroff(builder, array, abs_offset);
//
//            if (inst.opcode == CEE_LDELEMA) {
//                RuntimeTypeInfo tracked = tdn_get_verification_type(type);
//                CHECK_AND_RETHROW(tdn_get_byref_type(tracked, &tracked));
//
//                // just need the address, we don't need an explicit null check
//                // since we are going to get a fault on the length check if
//                // the array is null
//                CHECK_AND_RETHROW(eval_stack_push(stack, tracked, element_ptr));
//            } else {
//                RuntimeTypeInfo tracked = tdn_get_intermediate_type(type);
//
//                // perform the actual load
//                if (jit_is_struct_type(tracked)) {
//                    // we are copying a struct to the stack
//                    jit_value_t value;
//                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, tracked, &value));
//                    jit_emit_memcpy(builder, value, element_ptr, tracked->StackSize);
//                } else {
//                    // we are copying a simpler value
//                    jit_value_t value = jit_builder_build_load(builder,
//                                                                     get_jit_mem_size(type),
//                                                                     get_jit_mem_type(type),
//                                                                     element_ptr);
//                    if (type == tSByte) {
//                        value = jit_builder_build_sfill(builder, 8, value);
//                    } else if (type == tInt16) {
//                        value = jit_builder_build_sfill(builder, 16, value);
//                    }
//                    CHECK_AND_RETHROW(eval_stack_push(stack, tracked, value));
//                }
//            }
//        } break;
//
//        // load an element from an array
//        case CEE_STELEM:
//        case CEE_STELEM_REF: {
//            // pop the items
//            jit_value_t value, index, array;
//            RuntimeTypeInfo value_type, index_type, array_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &index_type, &index, NULL));
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &array_type, &array, NULL));
//
//            // verify the array type
//            CHECK(array_type->IsArray);
//            RuntimeTypeInfo T = array_type->ElementType;
//            if (inst.opcode == CEE_LDELEM_REF) {
//                // figure from the array, must be a pointer type
//                CHECK(tdn_type_is_referencetype(T));
//                CHECK(tdn_type_array_element_compatible_with(value_type, T));
//            } else {
//                CHECK(tdn_type_array_element_compatible_with(value_type, inst.operand.type));
//                CHECK(tdn_type_array_element_compatible_with(inst.operand.type, T));
//            }
//
//            // verify the index type
//            CHECK(index_type == tInt32 || index_type == tIntPtr);
//
//            // sign extend the index to a 64bit value
//            if (index_type == tInt32) {
//                index = jit_builder_build_iext(builder, index);
//                index = jit_builder_build_sfill(builder, 32, index);
//            }
//
//            jit_block_t length_is_valid = jit_builder_create_block(builder);
//            jit_block_t length_is_invalid = jit_builder_create_block(builder);
//
//            // length check, this will also take care of the NULL check since if
//            // we have a null value we will just fault in here
//            jit_value_t length_offset = jit_builder_build_iconst(builder, JIT_TYPE_I64, offsetof(struct Array, Length));
//            jit_value_t length_ptr = jit_builder_build_ptroff(builder, array, length_offset);
//            jit_value_t length = jit_builder_build_load(builder, JIT_MEM_SIZE_4, JIT_TYPE_I64, length_ptr);
//
//            // make sure the index < length
//            jit_value_t length_check = jit_builder_build_icmp(builder, JIT_ICMP_SLT, JIT_TYPE_I32, index, length);
//            jit_builder_build_brcond(builder, length_check, length_is_valid, length_is_invalid);
//
//            // perform the invalid length branch, throw an exception
//            jit_builder_set_block(builder, length_is_invalid);
//            jit_builder_build_call(builder, m_builtin_throw_index_out_of_range_exception, 0, NULL);
//            jit_builder_build_unreachable(builder);
//
//            // perform the valid length branch, load the value from the array
//            jit_builder_set_block(builder, length_is_valid);
//            jit_value_t element_size = jit_builder_build_iconst(builder, JIT_TYPE_I64, T->StackSize);
//            jit_value_t array_size = jit_builder_build_iconst(builder, JIT_TYPE_I64, tArray->HeapSize);
//            jit_value_t byte_offset = jit_builder_build_imul(builder, element_size, index);
//            jit_value_t abs_offset = jit_builder_build_iadd(builder, array_size, byte_offset);
//            jit_value_t element_ptr = jit_builder_build_ptroff(builder, array, abs_offset);
//
//            // TODO: perform write barrier as needed
//
//            // perform the actual store
//            if (jit_is_struct_type(T)) {
//                // we are copying a struct to the stack
//                if (T->IsUnmanaged) {
//                    jit_emit_memcpy(builder, element_ptr, value, T->StackSize);
//                } else {
//                    jit_emit_gc_memcpy(builder, T, element_ptr, value);
//                }
//            } else {
//                // we are copying a simpler value
//                jit_builder_build_store(builder,
//                                           get_jit_mem_size(T),
//                                           value,
//                                           element_ptr);
//            }
//        } break;
//
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
//        // Function calls
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//        case CEE_NEWOBJ:
//        case CEE_CALL:
//        case CEE_CALLVIRT: {
//            RuntimeMethodBase target = inst.operand.method;
//            bool is_static = target->Attributes.Static;
//            bool is_call = inst.opcode == CEE_CALL;
//            bool is_callvirt = inst.opcode == CEE_CALLVIRT;
//            bool is_newobj = inst.opcode == CEE_NEWOBJ;
//            size_t object_size = 0;
//
//            // TODO: check method accessibility
//
//            // verify we can call it
//            if (is_newobj) {
//                // newobj cannot call static or abstract methods
//                CHECK(!target->Attributes.Abstract);
//                CHECK(!target->Attributes.Static);
//
//                // ctor must be a special name
//                CHECK(target->Attributes.RTSpecialName);
//
//                // TODO: for strings we need to calculate the string size
//                object_size = target->DeclaringType->HeapSize;
//            } else if (is_callvirt) {
//                // callvirt can not call static methods
//                CHECK(!target->Attributes.Static);
//            } else {
//                // call can not call abstract methods
//                CHECK(!target->Attributes.Abstract);
//            }
//
//            // get all the arguments
//            int call_args_count = target->Parameters->Length + (is_static ? 0 : 1);
//            arrsetlen(call_args_types, call_args_count);
//            arrsetlen(call_args_values, call_args_count);
//            for (int i = call_args_count - 1; i >= 0; i--) {
//                if (is_newobj && i == 0) {
//                    // special case for the first argument for newobj, we need to allocate it, either
//                    // on the stack if its a value type or on the heap if its a reference type
//                    RuntimeTypeInfo target_this_type;
//                    jit_value_t obj = JIT_VALUE_INVALID;
//                    if (tdn_type_is_referencetype(target->DeclaringType)) {
//                        // call gc_new to allocate it
//                        target_this_type = target->DeclaringType;
//                        obj = jit_builder_build_call(builder, m_builtin_gc_new, 2, (jit_value_t[]){
//                            jit_builder_build_iconst(builder, JIT_TYPE_PTR, (uint64_t)target_this_type),
//                            jit_builder_build_iconst(builder, JIT_TYPE_I64, object_size)
//                        });
//                        CHECK_AND_RETHROW(eval_stack_push(stack, target->DeclaringType, obj));
//                    } else {
//                        // allocate it on the stack
//                        CHECK_AND_RETHROW(tdn_get_byref_type(target->DeclaringType, &target_this_type));
//                        CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, target->DeclaringType, &obj));
//                    }
//
//                    call_args_types[i] = target_this_type;
//                    call_args_values[i] = obj;
//                    break;
//                }
//
//                // pop it
//                stack_meta_t meta;
//                CHECK_AND_RETHROW(eval_stack_pop(stack, &call_args_types[i], &call_args_values[i], &meta));
//
//                // validate the stack type
//                RuntimeTypeInfo target_type;
//                if (!is_static) {
//                    if (i == 0) {
//                        // if we are in a callvirt update the target
//                        // to be from the instance type, this will
//                        // help us to perform a de-virt in case the
//                        // class is either sealed or the method is final
//                        if (is_callvirt) {
//                            // TODO: this
//                        }
//
//                        // figure the correct one for value types
//                        if (tdn_type_is_valuetype(target->DeclaringType)) {
//                            CHECK_AND_RETHROW(tdn_get_byref_type(target->DeclaringType, &target_type));
//                        } else {
//                            target_type = target->DeclaringType;
//                        }
//
//                        if (is_call) {
//                            // TODO: verify that we don't bypass the override of a function
//                            //       unless we are part of the class tree
//                        } else if (
//                            is_callvirt &&
//                            (
//                                !target->Attributes.Virtual || // method is not virtual
//                                target->DeclaringType->Attributes.Sealed || // the type on the stack is sealed
//                                (target->Attributes.Virtual && target->Attributes.Final) // the method is final
//                            )
//                        ) {
//                            jit_builder_build_call(builder, m_builtin_null_check, 1,
//                                                      (jit_value_t[]){ call_args_values[0] });
//                        }
//                    } else {
//                        target_type = target->Parameters->Elements[i - 1]->ParameterType;
//                    }
//                } else {
//                    target_type = target->Parameters->Elements[i]->ParameterType;
//                }
//
//                // check that we can do the assignment
//                CHECK(tdn_type_verifier_assignable_to(call_args_types[i], target_type),
//                      "%T verifier-assignable-to %T", call_args_types[i], target_type);
//            }
//
//            // TODO: indirect calls, de-virt
//
//            // make sure that we can actually call the target
//            CHECK_AND_RETHROW(jit_prepare_method(target, jit_builder_get_module(builder)));
//
//            RuntimeTypeInfo ret_type = tdn_get_intermediate_type(target->ReturnParameter->ParameterType);
//
//            // for struct instances allocate and push it beforehand
//            if (ret_type != tVoid && jit_is_struct_type(ret_type)) {
//                // need to allocate the space in the caller, insert it as the first argument, already pushed
//                // to the stack
//                CHECK(!is_newobj);
//                jit_value_t ret_buffer;
//                CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, ret_type, &ret_buffer));
//                        arrins(call_args_values, 0, ret_buffer);
//            }
//
//            // emit the actual call
//            jit_value_t value = jit_builder_build_call(builder,
//                                                             jit_get_function_from_id(target->JitMethodId),
//                                                             arrlen(call_args_values), call_args_values);
//
//            // for primitive types push it now
//            if (ret_type != tVoid && !jit_is_struct_type(ret_type)) {
//                CHECK(!is_newobj);
//                CHECK_AND_RETHROW(eval_stack_push(stack, ret_type, value));
//            }
//        } break;
//
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//        // Control flow
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//        // unconditional branch
//        case CEE_BR: {
//            // get and validate the label
//            jit_label_t* target_label = NULL;
//            CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, inst.operand.branch_target, &target_label));
//
//            // a branch, emit the branch
//            jit_builder_build_branch(builder, target_label->block);
//            region->has_block = false;
//
//            // because we don't fall-through we clear the stack
//            eval_stack_clear(stack);
//        } break;
//
//        // conditional branches
//        case CEE_BRFALSE:
//        case CEE_BRTRUE: {
//            // pop the item
//            jit_value_t value;
//            RuntimeTypeInfo value_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//
//            // ECMA-335 doesn't say brtrue takes in anything but
//            // O and native int, but I think its just an oversight
//            CHECK(
//                tdn_type_is_referencetype(value_type) ||
//                value_type == tInt32 ||
//                value_type == tInt64 ||
//                value_type == tIntPtr
//            );
//
//            // get the jump locations
//            jit_label_t* target_label = NULL;
//            CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, inst.operand.branch_target, &target_label));
//
//            jit_label_t* next_label = NULL;
//            CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, pc + inst.length, &next_label));
//
//            // choose the target to fit the brcond
//            jit_block_t true_dest;
//            jit_block_t false_dest;
//            if (inst.opcode == CEE_BRTRUE) {
//                // jump if non-zero
//                true_dest = target_label->block;
//                false_dest = next_label->block;
//            } else {
//                // jump if zero
//                true_dest = next_label->block;
//                false_dest = target_label->block;
//            }
//
//            // a branch, emit the branch
//            jit_builder_build_brcond(builder, value, true_dest, false_dest);
//        } break;
//
//        // all the different compare and compare-and-branches
//        // that we have
//        case CEE_BEQ:
//        case CEE_BGE:
//        case CEE_BGT:
//        case CEE_BLE:
//        case CEE_BLT:
//        case CEE_BNE_UN:
//        case CEE_BGE_UN:
//        case CEE_BGT_UN:
//        case CEE_BLE_UN:
//        case CEE_BLT_UN:
//        case CEE_CEQ:
//        case CEE_CGT:
//        case CEE_CGT_UN:
//        case CEE_CLT:
//        case CEE_CLT_UN: {
//            // pop the items
//            jit_value_t value1, value2;
//            RuntimeTypeInfo value1_type, value2_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value2_type, &value2, NULL));
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value1_type, &value1, NULL));
//
//            //
//            // perform the binary comparison and branch operations check,
//            // anything else can not be tested
//            //
//            if (value1_type == tInt32) {
//                CHECK(value2_type == tInt32 || value2_type == tIntPtr);
//
//            } else if (value1_type == tInt64) {
//                CHECK(value2_type == tInt64);
//
//            } else if (value1_type == tIntPtr) {
//                CHECK(value2_type == tInt32 || value2_type == tIntPtr);
//
//            } else if (value1_type->IsByRef) {
//                // TODO: does this only apply to types
//                //       of the same reference? I assume
//                //       it does but we might need to change this
//                CHECK(value2_type == value1_type);
//
//            } else if (tdn_type_is_referencetype(value1_type) && tdn_type_is_referencetype(value2_type)) {
//                CHECK(
//                    inst.opcode == CEE_BEQ ||
//                    inst.opcode == CEE_BNE_UN ||
//                    inst.opcode == CEE_CEQ ||
//                    inst.opcode == CEE_CGT_UN
//                );
//
//            } else {
//                CHECK_FAIL();
//            }
//
//            // jit only has the one side, need to flip for the other side
//            jit_icmp_kind_t kind;
//            bool compare = false;
//            switch (inst.opcode) {
//                case CEE_CEQ: compare = true;
//                case CEE_BEQ: kind = JIT_ICMP_EQ; break;
//                case CEE_BGE: kind = JIT_ICMP_SLE; SWAP(value1, value2); break;
//                case CEE_CGT: compare = true;
//                case CEE_BGT: kind = JIT_ICMP_SLT; SWAP(value1, value2); break;
//                case CEE_BLE: kind = JIT_ICMP_SLE; break;
//                case CEE_CLT: compare = true;
//                case CEE_BLT: kind = JIT_ICMP_SLT; break;
//                case CEE_BNE_UN: kind = JIT_ICMP_NE; break;
//                case CEE_BGE_UN: kind = JIT_ICMP_ULE; SWAP(value1, value2); break;
//                case CEE_CGT_UN: compare = true;
//                case CEE_BGT_UN: kind = JIT_ICMP_ULT; SWAP(value1, value2); break;
//                case CEE_BLE_UN: kind = JIT_ICMP_ULE; break;
//                case CEE_CLT_UN: compare = true;
//                case CEE_BLT_UN: kind = JIT_ICMP_ULT; break;
//                default: CHECK_FAIL();
//            }
//
//            // create the comparison
//            jit_value_t cmp = jit_builder_build_icmp(builder,
//                                                           kind,
//                                                           JIT_TYPE_I32,
//                                                           value1, value2);
//
//            // check if its a compare or not
//            if (compare) {
//                // a compare, just push the result as an int32
//                eval_stack_push(stack, tInt32, cmp);
//            } else {
//                // get the jump locations
//                jit_label_t* target_label = NULL;
//                CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, inst.operand.branch_target, &target_label));
//
//                jit_label_t* next_label = NULL;
//                CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, pc + inst.length, &next_label));
//
//                // a branch, emit the branch
//                jit_builder_build_brcond(builder, cmp, target_label->block, next_label->block);
//            }
//        } break;

        // return value from the function
        case CEE_RET: {
            // return must be from the root region
            CHECK(region->clause == NULL);

            ir_node* end = get_irg_end_block(ctx->graph);

            RuntimeTypeInfo wanted_ret_type = method->ReturnParameter->ParameterType;
            if (wanted_ret_type == tVoid) {
                add_immBlock_pred(end, new_Return(get_store(), 0, NULL));
            } else {
                RuntimeTypeInfo ret_type;
                ir_node* ret_value;
                eval_stack_pop(stack, &ret_type, &ret_value, NULL);

                // make sure the type is a valid return target
                CHECK(tdn_type_verifier_assignable_to(ret_type, wanted_ret_type));

                if (jit_is_struct_type(ret_type)) {
                    // returning a struct, need to use the implicit
                    // ret pointer
                    ASSERT(!"TODO: this");
//                    jit_emit_memcpy(builder,
//                                    jit_builder_build_param_ref(builder, 0),
//                                    ret_value,
//                                    ret_type->StackSize);
//
//                    // and return without a ret value
//                    jit_builder_build_return(builder, JIT_VALUE_INVALID);
                } else {
                    // returning a normal pointer sized thing
                    add_immBlock_pred(end, new_Return(get_store(), 1, &ret_value));
                }
            }

            // eval stack must be empty at this point
            CHECK(arrlen(stack->stack) == 0);
        } break;

//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//        // Exception control flow
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//        case CEE_LEAVE: {
//            // start with emptying the eval stack
//            eval_stack_clear(stack);
//
//            // the target
//            uint32_t branch_target = inst.operand.branch_target;
//
//            // if we are in this path make sure we have a clause
//            CHECK(region->clause != NULL);
//
//            // and make sure it is not a fault/finally one
//            if (region->is_handler) {
//                CHECK(
//                    region->clause->Flags != COR_ILEXCEPTION_CLAUSE_FINALLY &&
//                    region->clause->Flags != COR_ILEXCEPTION_CLAUSE_FAULT
//                );
//            }
//            // TODO: check not within a filter
//
//            // this exits the block, lets figure to where
//            jit_label_t* target_label = NULL;
//            finally_handler_t* first_handle = NULL;
//            finally_handler_t* last_handler = NULL;
//            for (int i = arrlen(ctx->regions) - 1; i >= 0; i--) {
//                jit_region_t* cur_region = ctx->regions[i];
//                if (cur_region->is_handler) continue; // we must leave into another protected block
//
//                if (cur_region->pc_start <= branch_target && branch_target < cur_region->pc_end) {
//                    // found the target try-region
//                    target_label = jit_get_label(cur_region, branch_target);
//                    CHECK(target_label != NULL);
//                    break;
//
//                } else if (cur_region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
//                    // we need to go through this finally
//                    finally_handler_t* path = hmgetp_null(cur_region->finally_handlers->finally_paths, branch_target);
//                    if (path == NULL) {
//                        jit_region_t* new_region = tdn_host_mallocz(sizeof(jit_region_t));
//                        CHECK(new_region != NULL);
//
//                        // create the new region and set
//                        // all the fields, including creating
//                        // the labels for that run
//                        new_region->clause = cur_region->clause;
//                        new_region->clause_index = cur_region->clause_index;
//                        new_region->pc_start = cur_region->clause->HandlerOffset;
//                        new_region->pc_end = cur_region->clause->HandlerOffset + cur_region->clause->HandlerLength;
//                        new_region->is_finally_path = true;
//                        new_region->is_handler = true;
//                        new_region->current_finally_path = INT32_MAX;
//                        new_region->finally_handlers = cur_region->finally_handlers;
//                        CHECK_AND_RETHROW(create_region_labels(ctx, new_region));
//
//                        // create a new handler
//                        finally_handler_t new_path = {
//                            .region = new_region,
//                            .key = branch_target
//                        };
//                        hmputs(cur_region->finally_handlers->finally_paths, new_path);
//                        path = hmgetp_null(cur_region->finally_handlers->finally_paths, branch_target);
//                        CHECK(path != NULL);
//                    }
//
//                    // if we already have another finally on the stack
//                    // set its next block to the current block, also
//                    // verify the consistency
//                    if (last_handler != NULL) {
//                        if (last_handler->region->has_next_block) {
//                            CHECK(JIT_IS_SAME_BLOCK(last_handler->region->next_block, path->region->entry_block));
//                        } else {
//                            last_handler->region->next_block = path->region->entry_block;
//                            last_handler->region->has_next_block = true;
//                        }
//                    }
//
//                    last_handler = path;
//                    if (first_handle == NULL) first_handle = path;
//                }
//            }
//
//            // make sure we got a label at all
//            CHECK(target_label != NULL);
//
//            // update the last handler to jump into the target label
//            if (last_handler != NULL) {
//                if (last_handler->region->has_next_block) {
//                    CHECK(JIT_IS_SAME_BLOCK(last_handler->region->next_block, target_label->block));
//                } else {
//                    last_handler->region->next_block = target_label->block;
//                    last_handler->region->has_next_block = true;
//                }
//            }
//
//            // if we have a first handle then get the label
//            // from that region to jump to
//            if (first_handle != NULL) {
//                target_label = jit_get_label(first_handle->region, first_handle->region->pc_start);
//                if (target_label == NULL) {
//                    // no label, so just go into the entry and don't process the target label stuff
//                    jit_builder_build_branch(builder, first_handle->region->entry_block);
//                    region->has_block = false;
//                    break;
//                }
//            }
//
//            // create a branch into the handler, we know that, verify the stack
//            // on the way
//            if (target_label->snapshot.initialized) {
//                CHECK(arrlen(target_label->snapshot.stack) == 0);
//            } else {
//                target_label->snapshot.initialized = true;
//            }
//
//            jit_builder_build_branch(builder, target_label->block);
//            region->has_block = false;
//        } break;
//
//        // this needs to jump into the next block, for fault and finally's fault path
//        // this simply goes into the next handler, for non-faulting finally path this
//        // goes into
//        case CEE_ENDFINALLY: {
//            // must be inside fault or finally
//            CHECK(
//                region->is_handler &&
//                (
//                    region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY ||
//                    region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT
//                )
//            );
//
//            // empty the stack
//            eval_stack_clear(stack);
//
//            // figure where we need to go
//            if (!region->is_finally_path) {
//                // get the region of the next handler, skipping
//                // the current one
//                for (int i = arrlen(ctx->regions) - 2; i >= 0; i--) {
//                    if (ctx->regions[i]->is_handler)
//                        continue;
//
//                    if (ctx->regions[i]->clause == NULL) {
//                        // we got to the root region, meaning there is nothing
//                        // else in the fault path to handle, so just rethrow the
//                        // error
//                        jit_builder_build_call(builder, m_builtin_rethrow, 0, NULL);
//                        jit_builder_build_unreachable(builder);
//                    } else {
//                        jit_region_t* handler = &ctx->handler_regions[ctx->regions[i]->clause_index];
//                        jit_builder_build_branch(builder, handler->entry_block);
//                    }
//
//                    region->has_block = false;
//                    break;
//                }
//            } else {
//                // we need to go to the next entry finally of this path
//                CHECK(region->has_next_block);
//                jit_builder_build_branch(builder, region->next_block);
//                region->has_block = false;
//            }
//        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Stack manipulation
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // Push an int32 to the stack
        case CEE_LDC_I4: {
            // NOTE: we are treating the value as a uint32 so it will not sign extend it
            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32,
                                              new_Const_long(mode_Is, inst.operand.int32)));
        } break;

        // Push an int64
        case CEE_LDC_I8: {
            CHECK_AND_RETHROW(eval_stack_push(stack, tInt64,
                                              new_Const_long(mode_Ls, inst.operand.int64)));
        } break;

//        // push a string
//        case CEE_LDSTR: {
//            // TODO: actually create the string or something
//            CHECK_AND_RETHROW(eval_stack_push(stack, tString,
//                                              jit_builder_build_iconst(builder,
//                                                                          JIT_TYPE_PTR, 0)));
//        } break;

        // Push a null object
        case CEE_LDNULL: {
            CHECK_AND_RETHROW(eval_stack_push(stack, tNull,
                                              new_Const(get_mode_null(mode_P))));
        } break;

        // Pop a value and dup it
        case CEE_DUP: {
            // pop the value and ignore it
            RuntimeTypeInfo type;
            ir_node* value;
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

//        // shifts
//        case CEE_SHL:
//        case CEE_SHR:
//        case CEE_SHR_UN: {
//            // pop the items
//            jit_value_t value, shift_amount;
//            RuntimeTypeInfo value_type, shift_amount_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &shift_amount_type, &shift_amount, NULL));
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//
//            // type check
//            CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);
//            CHECK(shift_amount_type == tInt32 || shift_amount_type == tIntPtr);
//
//            // perform the operation
//            jit_value_t result_value;
//            switch (inst.opcode) {
//                case CEE_SHL: result_value = jit_builder_build_shl(builder, value, shift_amount); break;
//                case CEE_SHR: result_value = jit_builder_build_ashr(builder, value, shift_amount); break;
//                case CEE_SHR_UN: result_value = jit_builder_build_lshr(builder, value, shift_amount); break;
//                default: CHECK_FAIL();
//            }
//
//            // push it to the stack
//            CHECK_AND_RETHROW(eval_stack_push(stack, value_type, result_value));
//        } break;
//
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
            ir_node* value1;
            ir_node* value2;
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
            ir_node* result_value;
            switch (inst.opcode) {
                case CEE_ADD: result_value = new_Add(value1, value2); break;
                case CEE_SUB: result_value = new_Sub(value1, value2); break;
                case CEE_AND: result_value = new_And(value1, value2); break;
                case CEE_OR: result_value = new_Or(value1, value2); break;
                case CEE_XOR: result_value = new_Eor(value1, value2); break;
                case CEE_MUL: result_value = new_Mul(value1, value2); break;
//                case CEE_DIV: result_value = jit_builder_build_sdiv(builder, value1, value2); break;
//                case CEE_DIV_UN: result_value = jit_builder_build_udiv(builder, value1, value2); break;
//                case CEE_REM: result_value = jit_builder_build_srem(builder, value1, value2); break;
//                case CEE_REM_UN: result_value = jit_builder_build_urem(builder, value1, value2); break;
                default: CHECK_FAIL();
            }

            // push it to the stack
            CHECK_AND_RETHROW(eval_stack_push(stack, result, result_value));
        } break;

        // bitwise not, emulate with value ^ ~0
        case CEE_NOT: {
            // pop the item
            ir_node* value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // push it to the stack
            CHECK_AND_RETHROW(eval_stack_push(stack, value_type, new_Not(value)));
        } break;

        // negation, emulate with 0 - value
        case CEE_NEG: {
            // pop the item
            ir_node* value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));

            // push it to the stack
            CHECK_AND_RETHROW(eval_stack_push(stack, value_type, new_Minus(value)));
        } break;

//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//        // Integer conversions
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//        case CEE_CONV_I1:
//        case CEE_CONV_U1: {
//            jit_value_t value;
//            RuntimeTypeInfo value_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//
//            // make sure it is an integer type
//            CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);
//
//            if (value_type != tInt32) {
//                // truncate to a 32bit value
//                value = jit_builder_build_itrunc(builder, value);
//            }
//
//            if (inst.opcode == CEE_CONV_U1) {
//                value = jit_builder_build_and(builder, value,
//                                                 jit_builder_build_iconst(builder,
//                                                                             JIT_TYPE_I32,
//                                                                             0xFF));
//            } else {
//                value = jit_builder_build_sfill(builder, 8, value);
//            }
//
//            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
//        } break;
//
//
//        case CEE_CONV_I2:
//        case CEE_CONV_U2: {
//            jit_value_t value;
//            RuntimeTypeInfo value_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//
//            // make sure it is an integer type
//            CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);
//
//            if (value_type != tInt32) {
//                // truncate to a 32bit value
//                value = jit_builder_build_itrunc(builder, value);
//            }
//
//            if (inst.opcode == CEE_CONV_U2) {
//                value = jit_builder_build_and(builder, value,
//                                                 jit_builder_build_iconst(builder,
//                                                                             JIT_TYPE_I32,
//                                                                             0xFFFF));
//            } else {
//                value = jit_builder_build_sfill(builder, 16, value);
//            }
//
//            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
//        } break;
//
//        case CEE_CONV_I4:
//        case CEE_CONV_U4: {
//            jit_value_t value;
//            RuntimeTypeInfo value_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//
//            if (value_type == tInt32) {
//                // nothing to do, push it again
//                CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
//            } else if (value_type == tInt64 || value_type == tIntPtr) {
//                // truncate to a 32bit value
//                value = jit_builder_build_itrunc(builder, value);
//                CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
//            } else {
//                // invalid type
//                CHECK_FAIL();
//            }
//        } break;
//
//        case CEE_CONV_I:
//        case CEE_CONV_I8: {
//            jit_value_t value;
//            RuntimeTypeInfo value_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//
//            // choose based on the opcode
//            RuntimeTypeInfo push_type = inst.opcode == CEE_CONV_I ? tIntPtr : tInt64;
//
//            if (value_type == tInt32) {
//                // comes from a 32bit integer, first extend to a 64bit integer
//                // and then sign extend it
//                value = jit_builder_build_iext(builder, value);
//                value = jit_builder_build_sfill(builder, 32, value);
//                CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
//            } else if (value_type == tInt64 || value_type == tIntPtr) {
//                // nothing to do, push it again
//                CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
//            } else {
//                // invalid type
//                CHECK_FAIL();
//            }
//        } break;
//
//            // we can also handle in here the overflow versions because
//            // converting an unsigned i32/i64 to an unsigned i64 can never fail
//        case CEE_CONV_U:
//        case CEE_CONV_U8: {
//            jit_value_t value;
//            RuntimeTypeInfo value_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//
//            // choose based on the opcode
//            RuntimeTypeInfo push_type = inst.opcode == CEE_CONV_U ? tIntPtr : tInt64;
//
//            if (value_type == tInt32) {
//                // comes from a 32bit integer, first extend to a 64bit integer
//                // and then zero extend it
//                value = jit_builder_build_iext(builder, value);
//                value = jit_builder_build_and(builder, value,
//                                                 jit_builder_build_iconst(builder,
//                                                                             JIT_TYPE_I64,
//                                                                             0xFFFFFFFF));
//                CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
//            } else if (value_type == tInt64 || value_type == tIntPtr) {
//                // nothing to do, push it again
//                CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
//            } else {
//                // invalid type
//                CHECK_FAIL();
//            }
//        } break;
//
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
//        // Type conversions with overflow checking
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
//        //
//        // | to / from | uint32            | uint64                | int32                    | int64                    |
//        // |-----------|-------------------|-----------------------|--------------------------|--------------------------|
//        // | uint8     | value ult 0x100   | value ult 0x100       | value ult 0x100          | value ult 0x100          |
//        // | uint16    | value ult 0x10000 | value ult 0x10000     | value ult 0x10000        | value ult 0x10000        |
//        // | uint32    | nop               | value ult 0x100000000 | -1 slt value             | value ult 0x100000000    |
//        // | uint64    | nop               | nop                   | -1 slt value             | -1 slt value             |
//        // | int8      | value ult 0x80    | value ult 0x80        | sfill 8 and eq original  | sfill 8 and eq original  |
//        // | int16     | value ult 0x8000  | value ult 0x8000      | sfill 16 and eq original | sfill 16 and eq original |
//        // | int32     | -1 slt value      | -1 slt value          | nop                      | sfill 32 and eq original |
//        // | int64     | nop               | -1 slt value          | nop                      | nop                      |
//        //
//
//        case CEE_CONV_OVF_U1:
//        case CEE_CONV_OVF_I1:
//        case CEE_CONV_OVF_U1_UN:
//        case CEE_CONV_OVF_I1_UN:
//        case CEE_CONV_OVF_U2:
//        case CEE_CONV_OVF_I2:
//        case CEE_CONV_OVF_U2_UN:
//        case CEE_CONV_OVF_I2_UN:
//        case CEE_CONV_OVF_U4:
//        case CEE_CONV_OVF_I4:
//        case CEE_CONV_OVF_U4_UN:
//        case CEE_CONV_OVF_I4_UN: {
//            jit_value_t value;
//            RuntimeTypeInfo value_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//
//            // type check
//            jit_value_type_t type;
//            uint64_t minus_one;
//            if (value_type == tInt32) {
//                // special case of nop
//                //      uint32 -> uint32
//                //      int32 -> int32
//                if (inst.opcode == CEE_CONV_OVF_U4_UN || inst.opcode == CEE_CONV_OVF_I4) {
//                    CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
//                    break;
//                }
//
//                type = JIT_TYPE_I32;
//                minus_one = (uint32_t)-1;
//            } else if (value_type == tInt64 || value_type == tIntPtr) {
//                type = JIT_TYPE_I64;
//                minus_one = (uint64_t)-1;
//            } else {
//                CHECK_FAIL();
//            }
//
//            jit_value_t cond;
//            if (
//                (value_type == tInt32 && inst.opcode == CEE_CONV_OVF_U4) ||
//                (inst.opcode == CEE_CONV_OVF_I4_UN)
//            ) {
//                // special case:
//                //      int32 -> uint32
//                //      uint32/uint64 -> int32
//                // perform a signed positive check
//                cond = jit_builder_build_icmp(builder, JIT_ICMP_SLT, JIT_TYPE_I32,
//                                                 jit_builder_build_iconst(builder, type, minus_one),
//                                                 value);
//            } else if (
//                inst.opcode == CEE_CONV_OVF_I1 ||
//                inst.opcode == CEE_CONV_OVF_I2 ||
//                inst.opcode == CEE_CONV_OVF_I4
//            ) {
//                // int32/int64 -> int8
//                // int32/int64 -> int16
//                // int64 -> int32
//
//                // get the correct bit count
//                uint64_t bit_width;
//                if (inst.opcode == CEE_CONV_OVF_I1) {
//                    bit_width = 8;
//                } else if (inst.opcode == CEE_CONV_OVF_I2) {
//                    bit_width = 16;
//                } else if (inst.opcode == CEE_CONV_OVF_I4) {
//                    bit_width = 32;
//                } else {
//                    CHECK_FAIL();
//                }
//
//                // sign extend and check they are still the same
//                jit_value_t signed_value = jit_builder_build_sfill(builder, bit_width, value);
//                cond = jit_builder_build_icmp(builder, JIT_ICMP_EQ, JIT_TYPE_I32,
//                                                 value, signed_value);
//            } else {
//                // get the correct max value
//                uint64_t max_value;
//                if (inst.opcode == CEE_CONV_OVF_U1 || inst.opcode == CEE_CONV_OVF_U1_UN) {
//                    max_value = 0x100;
//                } else if (inst.opcode == CEE_CONV_OVF_U2 || inst.opcode == CEE_CONV_OVF_U2_UN) {
//                    max_value = 0x10000;
//                } else if (inst.opcode == CEE_CONV_OVF_U4 || inst.opcode == CEE_CONV_OVF_U4_UN) {
//                    max_value = 0x100000000;
//                } else if (inst.opcode == CEE_CONV_OVF_I1_UN) {
//                    max_value = 0x80;
//                } else if (inst.opcode == CEE_CONV_OVF_I2_UN) {
//                    max_value = 0x8000;
//                } else {
//                    CHECK_FAIL();
//                }
//
//                // perform an unsigned less than check, this is fine for both the signed and unsigned
//                // input because we are using twos complement
//                cond = jit_builder_build_icmp(builder, JIT_ICMP_ULT, JIT_TYPE_I32, value,
//                                                 jit_builder_build_iconst(builder, type,
//                                                                             max_value));
//            }
//
//            // perform the branch
//            jit_block_t valid = jit_builder_create_block(builder);
//            jit_block_t invalid = jit_builder_create_block(builder);
//            jit_builder_build_brcond(builder, cond, valid, invalid);
//
//            // invalid path, throw
//            jit_builder_set_block(builder, invalid);
//            jit_builder_build_call(builder, m_builtin_throw_overflow_exception, 0, NULL);
//            jit_builder_build_unreachable(builder);
//
//            // valid path, push the new valid
//            jit_builder_set_block(builder, valid);
//
//            // truncate if came from a bigger value
//            if (value_type != tInt32) {
//                value = jit_builder_build_itrunc(builder, value);
//            }
//            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
//        } break;
//
//        case CEE_CONV_OVF_I8:
//        case CEE_CONV_OVF_I:
//        case CEE_CONV_OVF_U8_UN:
//        case CEE_CONV_OVF_U_UN: {
//            jit_value_t value;
//            RuntimeTypeInfo value_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//
//            // choose the type to push
//            RuntimeTypeInfo push_type;
//            if (inst.opcode == CEE_CONV_OVF_U_UN || inst.opcode == CEE_CONV_OVF_I) {
//                push_type = tIntPtr;
//            } else {
//                push_type = tInt64;
//            }
//
//            // type check
//            CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);
//
//            // if the input was 32bit zero/sign extend it,
//            // based on the signed -> signed or the unsigned -> unsigned
//            if (value_type == tInt32) {
//                value = jit_builder_build_iext(builder, value);
//                if (inst.opcode == CEE_CONV_OVF_I8 || inst.opcode == CEE_CONV_OVF_I) {
//                    value = jit_builder_build_sfill(builder, 32, value);
//                } else {
//                    value = jit_builder_build_and(builder, value,
//                                                     jit_builder_build_iconst(builder,
//                                                                                 JIT_TYPE_I64, 0xFFFFFFFF));
//                }
//            }
//
//            // just push the same as the wanted type
//            CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
//        } break;
//
//        case CEE_CONV_OVF_I8_UN:
//        case CEE_CONV_OVF_I_UN:
//        case CEE_CONV_OVF_U8:
//        case CEE_CONV_OVF_U: {
//            jit_value_t value;
//            RuntimeTypeInfo value_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value, NULL));
//
//            // choose the type to push
//            RuntimeTypeInfo push_type;
//            if (inst.opcode == CEE_CONV_OVF_U || inst.opcode == CEE_CONV_OVF_I_UN) {
//                push_type = tIntPtr;
//            } else {
//                push_type = tInt64;
//            }
//
//            // type check
//            jit_value_type_t type;
//            uint64_t minus_one;
//            if (value_type == tInt32) {
//                if (inst.opcode == CEE_CONV_OVF_I8_UN || inst.opcode == CEE_CONV_OVF_I_UN) {
//                    // special case, uint32 -> int64 is always valid
//                    goto skip_positive_check;
//                }
//
//                type = JIT_TYPE_I32;
//                minus_one = (uint32_t)-1;
//            } else if (value_type == tInt64 || value_type == tIntPtr) {
//                type = JIT_TYPE_I64;
//                minus_one = (uint64_t)-1;
//            } else {
//                CHECK_FAIL();
//            }
//
//            // make sure is positive by doing a sign check
//            jit_value_t cond = jit_builder_build_icmp(builder, JIT_ICMP_SLT, JIT_TYPE_I32,
//                                                            jit_builder_build_iconst(builder, type, minus_one),
//                                                            value);
//
//            // perform the branch
//            jit_block_t valid = jit_builder_create_block(builder);
//            jit_block_t invalid = jit_builder_create_block(builder);
//            jit_builder_build_brcond(builder, cond, valid, invalid);
//
//            // invalid path, throw
//            jit_builder_set_block(builder, invalid);
//            jit_builder_build_call(builder, m_builtin_throw_overflow_exception, 0, NULL);
//            jit_builder_build_unreachable(builder);
//
//            // valid path, push the new valid
//            jit_builder_set_block(builder, valid);
//
//        skip_positive_check:
//            // if the input was 32bit,
//            // in this case we only either have unsigned -> signed or signed -> unsigned, in both
//            // cases we make sure that the value is positive, so we can just do a normal zero extension
//            if (value_type == tInt32) {
//                value = jit_builder_build_iext(builder, value);
//                value = jit_builder_build_and(builder, value,
//                                                 jit_builder_build_iconst(builder,
//                                                                             JIT_TYPE_I64, 0xFFFFFFFF));
//            }
//
//            // just push the same as the wanted type
//            CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
//        } break;
//
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//        // Allocation
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//        case CEE_NEWARR: {
//            jit_value_t num_elems;
//            RuntimeTypeInfo num_elems_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &num_elems_type, &num_elems, NULL));
//
//            // type check
//            CHECK(num_elems_type == tInt32 || num_elems_type == tIntPtr);
//
//            // extend to 64bit if needed
//            if (num_elems_type == tInt32) {
//                num_elems = jit_builder_build_iext(builder, num_elems);
//                num_elems = jit_builder_build_sfill(builder, 32, num_elems);
//            }
//
//            // get the array type we are allocating
//            RuntimeTypeInfo array_type = NULL;
//            CHECK_AND_RETHROW(tdn_get_array_type(inst.operand.type, &array_type));
//
//            // calculate the array size we will need
//            jit_value_t array_size = jit_builder_build_imul(builder, num_elems,
//                                                                  jit_builder_build_iconst(builder,
//                                                                                              JIT_TYPE_I64,
//                                                                                              inst.operand.type->StackSize));
//            array_size = jit_builder_build_iadd(builder, array_size, jit_builder_build_iconst(builder,
//                                                                                                    JIT_TYPE_I64,
//                                                                                                    sizeof(struct Array)));
//
//            // call the gc_new to allocate the new object
//            jit_value_t array = jit_builder_build_call(builder, m_builtin_gc_new, 2, (jit_value_t[]){
//                jit_builder_build_iconst(builder, JIT_TYPE_PTR, (uint64_t)array_type), array_size
//            });
//
//            // and finally set the length of the array
//            jit_builder_build_store(builder, JIT_MEM_SIZE_4, num_elems,
//                                       jit_builder_build_ptroff(builder, array,
//                                                                   jit_builder_build_iconst(builder,
//                                                                                               JIT_TYPE_I64,
//                                                                                               offsetof(struct Array, Length))));
//
//            // push the array pointer
//            CHECK_AND_RETHROW(eval_stack_push(stack, array_type, array));
//        } break;
//
//        case CEE_BOX: {
//            jit_value_t val;
//            RuntimeTypeInfo val_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &val_type, &val, NULL));
//
//            CHECK(tdn_type_verifier_assignable_to(val_type, inst.operand.type));
//
//            if (tdn_type_is_referencetype(inst.operand.type)) {
//                // for reference type there is nothing special todo, it stays as is
//                CHECK_AND_RETHROW(eval_stack_push(stack, val_type, val));
//
//            } else {
//                // this is the struct path, it may have nullable and may not have nullable
//                bool is_nullable = val_type->GenericTypeDefinition == tNullable;
//
//                // if we have a Nullable<> then prepare the allocation and movement
//                size_t has_value_offset = -1;
//                size_t val_offset = -1;
//                jit_block_t next;
//                if (is_nullable) {
//                    // get the needed offsets for the given type
//                    RuntimeFieldInfo_Array fields = val_type->DeclaredFields;
//                    for (int i = 0; i < fields->Length; i++) {
//                        RuntimeFieldInfo field = fields->Elements[i];
//                        if (tdn_compare_string_to_cstr(field->Name, "_hasValue")) {
//                            has_value_offset = field->FieldOffset;
//                        } else if (tdn_compare_string_to_cstr(field->Name, "_value")) {
//                            val_offset = field->FieldOffset;
//                        }
//                    }
//                    CHECK(has_value_offset != -1);
//                    CHECK(val_offset != -1);
//
//                    // the needed pointers
//                    jit_value_t has_value_ptr = jit_builder_build_ptroff(builder, val,
//                                                                               jit_builder_build_iconst(builder,
//                                                                                                           JIT_TYPE_I64,
//                                                                                                           has_value_offset));
//
//                    jit_value_t value_ptr = jit_builder_build_ptroff(builder, val,
//                                                                           jit_builder_build_iconst(builder,
//                                                                                                       JIT_TYPE_I64,
//                                                                                                       has_value_offset));
//
//                    // check if we have a value
//                    jit_value_t has_value = jit_builder_build_load(builder, JIT_MEM_SIZE_1, JIT_TYPE_I32,
//                                                                         has_value_ptr);
//                    has_value = jit_builder_build_icmp(builder,
//                                                          JIT_ICMP_NE,
//                                                          JIT_TYPE_I32,
//                                                          has_value,
//                                                          jit_builder_build_iconst(builder,
//                                                                                      JIT_TYPE_I32,
//                                                                                      0));
//
//                    // we are using a phi to choose the correct
//                    next = jit_builder_create_block(builder);
//                    jit_block_t allocate = jit_builder_create_block(builder);
//                    jit_builder_build_brcond(builder, has_value, allocate, next);
//
//                    // perform the copy path
//                    jit_builder_set_block(builder, allocate);
//
//                    // if we have an integer type we need to read it, to make it consistent with
//                    // whatever we will have in the non-nullable case
//                    val_type = tdn_get_verification_type(val_type->GenericArguments->Elements[0]);
//                    if (val_type == tByte) {
//                        val = jit_builder_build_load(builder, JIT_MEM_SIZE_1, JIT_TYPE_I32, value_ptr);
//                    } else if (val_type == tInt16) {
//                        val = jit_builder_build_load(builder, JIT_MEM_SIZE_2, JIT_TYPE_I32, value_ptr);
//                    } else if (val_type == tInt32) {
//                        val = jit_builder_build_load(builder, JIT_MEM_SIZE_4, JIT_TYPE_I32, value_ptr);
//                    } else if (val_type == tInt64) {
//                        val = jit_builder_build_load(builder, JIT_MEM_SIZE_8, JIT_TYPE_I32, value_ptr);
//                    } else {
//                        // otherwise it is just the pointer since its a struct
//                        val = value_ptr;
//                    }
//                }
//
//                // allocate it
//                jit_value_t args[2] = {
//                    jit_builder_build_iconst(builder, JIT_TYPE_PTR, (uint64_t)inst.operand.type),
//                    jit_builder_build_iconst(builder, JIT_TYPE_I64, sizeof(struct Object) + inst.operand.type->HeapSize),
//                };
//                jit_value_t obj = jit_builder_build_call(builder, m_builtin_gc_new, 2, args);
//                jit_value_t obj_value = jit_builder_build_ptroff(builder, obj,
//                                                                       jit_builder_build_iconst(builder,
//                                                                                                   JIT_TYPE_I64,
//                                                                                                   sizeof(struct Object)));
//
//                // copy it, if its an integer copy properly, which will most likely truncate it
//                if (val_type == tByte) {
//                    jit_builder_build_store(builder, JIT_MEM_SIZE_1, val, obj_value);
//                } else if (val_type == tInt16) {
//                    jit_builder_build_store(builder, JIT_MEM_SIZE_2, val, obj_value);
//                } else if (val_type == tInt32) {
//                    jit_builder_build_store(builder, JIT_MEM_SIZE_4, val, obj_value);
//                } else if (val_type == tInt64) {
//                    jit_builder_build_store(builder, JIT_MEM_SIZE_8, val, obj_value);
//                } else {
//                    if (val_type->IsUnmanaged) {
//                        jit_emit_memcpy(builder, obj_value, val, val_type->StackSize);
//                    } else {
//                        jit_emit_gc_memcpy(builder, val_type, obj_value, val);
//                    }
//                }
//
//                // and finally now that it is properly allocated we can setup the next path
//                // with the proper phi
//                if (is_nullable) {
//                    jit_builder_build_branch(builder, next);
//
//                    // now setup the phi
//                    //  first input is the allocation path
//                    //  second input is the no allocation path
//                    jit_builder_set_block(builder, next);
//                    jit_value_t inputs[2] = {
//                        jit_builder_build_iconst(builder, JIT_TYPE_PTR, 0),
//                        obj,
//                    };
//                    obj = jit_builder_build_phi(builder, JIT_TYPE_PTR, 2, inputs, NULL);
//                }
//
//                // finally push the object, will either be a phi result or just the object
//                stack_meta_t meta = {
//                    .BoxedType = val_type
//                };
//                CHECK_AND_RETHROW(eval_stack_push_with_meta(stack, tObject, obj, meta));
//            }
//        } break;
//
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//        // Misc operations
//        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//        // push the size in bytes of the given type
//        case CEE_SIZEOF: {
//            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32,
//                                              jit_builder_build_iconst(builder, JIT_TYPE_I32,
//                                                                          inst.operand.type->StackSize)));
//        } break;
//
//        case CEE_INITOBJ: {
//            jit_value_t dest;
//            RuntimeTypeInfo dest_type;
//            CHECK_AND_RETHROW(eval_stack_pop(stack, &dest_type, &dest, NULL));
//
//            // must be a ByRef
//            CHECK(dest_type->IsByRef);
//            dest_type = dest_type->ElementType;
//
//            // must be assignable to properly
//            CHECK(tdn_type_assignable_to(inst.operand.type, dest_type),
//                  "%T assignable-to %T", inst.operand.type, dest_type);
//
//            // get the base type so we know how to best assign it
//            dest_type = tdn_get_verification_type(dest_type);
//            if (dest_type == tByte) {
//                jit_builder_build_store(builder, JIT_MEM_SIZE_1,
//                                           jit_builder_build_iconst(builder, JIT_TYPE_I32, 0), dest);
//
//            } else if (dest_type == tInt16) {
//                jit_builder_build_store(builder, JIT_MEM_SIZE_2,
//                                           jit_builder_build_iconst(builder, JIT_TYPE_I32, 0), dest);
//
//            } else if (dest_type == tInt32) {
//                jit_builder_build_store(builder, JIT_MEM_SIZE_4,
//                                           jit_builder_build_iconst(builder, JIT_TYPE_I32, 0), dest);
//
//            } else if (dest_type == tInt64) {
//                jit_builder_build_store(builder, JIT_MEM_SIZE_8,
//                                           jit_builder_build_iconst(builder, JIT_TYPE_I64, 0), dest);
//
//            } else if (tdn_type_is_referencetype(dest_type) || dest_type == tIntPtr) {
//                jit_builder_build_store(builder, JIT_MEM_SIZE_8,
//                                           jit_builder_build_iconst(builder, JIT_TYPE_PTR, 0), dest);
//
//            } else if (dest_type->IsUnmanaged) {
//                jit_builder_build_call(builder, m_builtin_bzero, 2,
//                                          (jit_value_t[]){
//                                              dest,
//                                              jit_builder_build_iconst(builder, JIT_TYPE_I64, dest_type->StackSize)
//                                          });
//
//            } else {
//                jit_builder_build_call(builder, m_builtin_gc_bzero, 2,
//                                          (jit_value_t[]){
//                                                  jit_builder_build_iconst(builder, JIT_TYPE_PTR, (uint64_t)dest_type),
//                                                  dest,
//                                          });
//            }
//        } break;

        case CEE_NOP: {
            // do nothing
        } break;

        default:
            CHECK_FAIL();
    }

cleanup:
//    arrfree(call_args_types);
//    arrfree(call_args_values);

    return err;
}

typedef struct jit_builder_ctx {
    tdn_err_t err;
} jit_builder_ctx_t;

static void mature_region(jit_region_t* region) {
    mature_immBlock(region->entry_block);
    for (int i = 0; i < arrlen(region->labels); i++) {
        mature_immBlock(region->labels[i].block);
    }
}

static tdn_err_t internal_jit_method(RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeExceptionHandlingClause_Array clauses = method->MethodBody->ExceptionHandlingClauses;
    ir_entity* entity = (ir_entity*)method->JitMethodId;

    TRACE("%U::%U", method->DeclaringType->Name, method->Name);

    // setup the context that will be used for this method jitting
    jit_context_t ctx = {
        .method = method,
    };
    eval_stack_t* stack = &ctx.stack;
    stack->max_depth = method->MethodBody->MaxStackSize;

    // create the graph and set the max amount of locals we will need
    ctx.graph = new_ir_graph(entity, stack->max_depth + get_method_n_params(get_entity_type(entity)));
    set_current_ir_graph(ctx.graph);

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

    int arg_count = (method->Parameters->Length + (this_type == NULL ? 0 : 1));

    // prepare the argument list by setting up their types, we
    // will later spill and set the value as needed

    if (arg_count != 0) {
        ir_node* first_block = get_cur_block();
        set_cur_block(get_irg_start_block(ctx.graph));

        ir_node* args = get_irg_args(ctx.graph);

        arrsetlen(ctx.args, arg_count);
        for (int i = 0; i < arg_count; i++) {
            // resolve the parameter type
            RuntimeTypeInfo type;
            CHECK_AND_RETHROW(jit_resolve_parameter_type(method, i, &type));

            // set it up initially
            ctx.args[i].value = new_Proj(args, get_type_mode(get_ir_type(type)), i);
            ctx.args[i].type = type;
            ctx.args[i].spilled = false;
        }

        // return to the first block
        set_cur_block(first_block);
    }


    // create the entry block
    bool created_entry_block = false;
    ir_type* frame = NULL;

    // prepare the locals by allocating their stack slots already
    if (method->MethodBody->LocalVariables != NULL) {
        // using the entry block to zero the variables
        created_entry_block = true;

        frame = new_type_frame();
        set_irg_frame_type(ctx.graph, frame);

        // setup the variables
        arrsetlen(ctx.locals, method->MethodBody->LocalVariables->Length);
        for (int i = 0; i < method->MethodBody->LocalVariables->Length; i++) {
            RuntimeLocalVariableInfo var = method->MethodBody->LocalVariables->Elements[i];

            // create it
            ident* id = new_id_fmt("local%d", var->LocalIndex);
            ir_type* typ = get_ir_type(var->LocalType);
            ir_entity* lentity = new_entity(frame, id, typ);
            ctx.locals[i] = new_Member(get_irg_frame(ctx.graph), lentity);

            // clear it
            if (jit_is_struct_type(var->LocalType)) {
                // TODO: call memset zero
//                jit_builder_build_call(builder, m_builtin_bzero, 2,
//                                          (jit_value_t[]){
//                                                  ctx.locals[i],
//                                                  jit_builder_build_iconst(builder, JIT_TYPE_I64, var->LocalType->StackSize)
//                                          });
            } else {
                ir_node* s = new_Store(get_store(), ctx.locals[i],
                                           new_Const_long(get_type_mode(typ), 0),
                                           typ, cons_none);
                set_store(new_Proj(s, mode_M, pn_Store_M));
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
                frame = new_type_frame();
                set_irg_frame_type(ctx.graph, frame);
            }

            // create a stackslot for the spill
            if (!ctx.args[arg].spilled) {
                // create it
                ident* id = new_id_fmt("arg%d", arg);
                ir_type* typ = get_ir_type(type);
                ir_entity* lentity = new_entity(frame, id, typ);
                ir_node* old_arg = ctx.args[arg].value;
                ctx.args[arg].value = new_Member(get_irg_frame(ctx.graph), lentity);
                ctx.args[arg].spilled = true;

                // store it, if its a value-type we need to copy it instead
                if (jit_is_struct_type(type)) {
                    ir_node* s = new_CopyB(get_store(), ctx.args[arg].value, old_arg, typ, cons_none);
                    set_store(new_Proj(s, mode_M, pn_Store_M));
                } else {
                    ir_node* s = new_Store(get_store(), ctx.args[arg].value, old_arg, typ, cons_none);
                    set_store(new_Proj(s, mode_M, pn_Store_M));
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
        ctx.protected_regions[i].current_finally_path = INT32_MAX;
        CHECK_AND_RETHROW(create_region_labels(&ctx, &ctx.protected_regions[i]));

        memset(&ctx.handler_regions[i], 0, sizeof(jit_region_t));
        ctx.handler_regions[i].clause = c;
        ctx.handler_regions[i].clause_index = i;
        ctx.handler_regions[i].pc_start = c->HandlerOffset;
        ctx.handler_regions[i].pc_end = c->HandlerOffset + c->HandlerLength;
        ctx.handler_regions[i].is_handler = true;
        ctx.handler_regions[i].finally_handlers = handler;
        ctx.handler_regions[i].current_finally_path = handler == NULL ? INT32_MAX : -1;
        CHECK_AND_RETHROW(create_region_labels(&ctx, &ctx.handler_regions[i]));
    }

    // finally create the top most region
    // the index is going to be set to the index of the last
    // clause so we will search all the handlers if we are
    // in the root
    jit_region_t root_region = {
        .pc_start = 0,
        .pc_end = method->MethodBody->ILSize,
        .clause = NULL,
        .clause_index = region_count - 1,
    };
    CHECK_AND_RETHROW(create_region_labels(&ctx, &root_region));

    // either branch from the entry to the region entry or set the region
    // entry as the actual entry
    add_immBlock_pred(root_region.entry_block, new_Jmp());

    // and push it as the starting region
    arrpush(ctx.regions, &root_region);

    // the root is the first block
    root_region.has_block = true;
    set_cur_block(root_region.entry_block);

    // for debug
    int indent = 0;

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
        CHECK(arrlen(ctx.regions) > 0);
        jit_region_t* region = arrlast(ctx.regions);
        jit_region_t* new_region = NULL;

        // check if we entered a new region or not
        int start_index = region->clause_index == -1 ? 0 : region->clause_index;
        for (int i = 0; i <= region->clause_index; i++) {
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
                    add_immBlock_pred(new_region->entry_block, new_Jmp());
                }

                // we will already set it in here
                new_region->has_block = true;
                set_cur_block(new_region->entry_block);
            } else if (pc == handler_region->pc_start) {
                // for finally we have duplicate paths for each of the valid entries
                // first is the fault path, the rest are the valid paths, get the correct
                // region to pass on
                if (handler_region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
                    int finaly_path_index = handler_region->current_finally_path++;
                    CHECK(finaly_path_index < hmlen(handler_region->finally_handlers->finally_paths));
                    if (finaly_path_index >= 0) {
                        handler_region = handler_region->finally_handlers->finally_paths[finaly_path_index].region;
                    }
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
                set_cur_block(new_region->entry_block);

                // for exception handlers we need to set the first item as the exception
                // TODO: handler filters
                if (handler_region->clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
                    // TODO: get the exception object projection thing
                    CHECK_AND_RETHROW(eval_stack_push(stack, new_region->clause->CatchType, new_Unknown(mode_P)));
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
                        CHECK_AND_RETHROW(eval_stack_merge(stack, current, true));
                    }
                } else {
                    CHECK_AND_RETHROW(eval_stack_snapshot(stack, current));
                }

                // if we have full-through then create a branch
                // NOTE: we don't check FIRST since from the first
                //       opcode we don't need this branch
                if (
                    flow_control == TDN_IL_CF_NEXT ||
                    flow_control == TDN_IL_CF_BREAK ||
                    flow_control == TDN_IL_CF_CALL
                ) {
                    add_immBlock_pred(current->block, new_Jmp());
                }

                // set the current block
                region->has_block = true;
                set_cur_block(current->block);
            } else {
                // make sure the label is after this pc, to make sure we don't cross segments
                CHECK(region->labels[region->label_index].address > pc);
            }
        }

        // convert to jit
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

        // we might be reaching the end of multiple nested blocks, so until we find a region which
        // does not end we need to pop it
        while (arrlen(ctx.regions) != 0 && pc == region->pc_end) {
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
                int finaly_path_index = ctx.handler_regions[region->clause_index].current_finally_path;
                if (finaly_path_index < hmlen(region->finally_handlers->finally_paths)) {
                    pc = region->pc_start;

                    // we don't want to go completely out just yet, we have some
                    // more paths to take
                    break;
                }
            }

            // get the new last region for the while condition to get
            // updated
            region = arrlast(ctx.regions);
        }

        //--------------------------------------------------------------------------------------------------------------
        // debug prints
        //--------------------------------------------------------------------------------------------------------------

        for (int i = 0; clauses != NULL && i < clauses->Length; i++) {
            RuntimeExceptionHandlingClause c = clauses->Elements[i];
            if (c->TryOffset + c->TryLength == pc) {
                indent -= 4;
                tdn_host_printf("[*] \t\t\t%*s} // end .try - %04x\n", indent, "");
            } else if (c->HandlerOffset + c->HandlerLength == pc) {
                indent -= 4;
                tdn_host_printf("[*] \t\t\t%*s} // end handler\n", indent, "");
            }
        }
    }

    // make sure we have no more regions in our stack
    CHECK(arrlen(ctx.regions) == 0);

    //
    // we now need to finalize everything
    //

    // mature all the blocks
    mature_region(&root_region);
    for (int i = 0; i < arrlen(ctx.protected_regions); i++) {
        mature_region(&ctx.protected_regions[i]);
    }
    for (int i = 0; i < arrlen(ctx.handler_regions); i++) {
        mature_region(&ctx.handler_regions[i]);
    }

    // we are done
    irg_finalize_cons(ctx.graph);

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
    return err;
}

/**
 * Prepares a method for jitting, this essentially creates the jit function
 * so it will be ready for when we call it
 */
static tdn_err_t jit_prepare_method(RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;

    // check if already jitted
    if (method->JitPrepared) {
        goto cleanup;
    }
    method->JitPrepared = 1;

    // TODO: external methods need special handling

    // set the parameter count
    int cc = cc_fastcall_set;
    int count = method->Parameters->Length;
    if (!method->Attributes.Static) {
        count += 1;
        cc |= cc_this_call;
    }

    //
    ir_type* ret_type = get_ir_type(method->ReturnParameter->ParameterType);

    // set the properties
    mtp_additional_properties properties = mtp_no_property;
    if (method->MethodImplFlags.NoInlining) properties |= mtp_property_noinline;
    if (method->MethodImplFlags.AggressiveInlining) properties |= mtp_property_always_inline;
    if (method->Object.ObjectType == tRuntimeConstructorInfo) properties |= mtp_property_is_constructor;
    ir_type* method_type = new_type_method(count, ret_type == NULL ? 0 : 1, 0, cc, properties);

    if (ret_type != NULL) {
        set_method_res_type(method_type, 0, ret_type);
    }

    // add the this parameter
    int j = 0;
    if (!method->Attributes.Static) {
        ir_type* typ = get_ir_type(method->DeclaringType);
        if (jit_is_struct_type(method->DeclaringType)) {
            RuntimeTypeInfo this_type = NULL;
            CHECK_AND_RETHROW(tdn_get_pointer_type(method->DeclaringType, &this_type));
            typ = get_ir_type(this_type);
        }
        set_method_param_type(method_type, 0, typ);
        j++;
    }

    // add the method args
    for (int i = j; i < count; i++) {
        set_method_param_type(method_type, i, get_ir_type(method->Parameters->Elements[i - j]->ParameterType));
    }

    // generate the name
    string_builder_t builder = {};
    string_builder_push_method_signature(&builder, method, true);
    const char* name = string_builder_build(&builder);
    ident* id = new_id_from_str(name);
    string_builder_free(&builder);

    ir_entity* entity = new_entity(get_class_ir_type(method->DeclaringType), id, method_type);

    // create the function itself
    method->JitMethodId = (uintptr_t)entity;

    // queue to methods to jit if we didn't start with this already
    if (!method->JitStarted) {
        arrpush(m_methods_to_jit, method);
    }

cleanup:
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
    CHECK_AND_RETHROW(internal_jit_method(method));

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// High-level apis
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t tdn_jit_method(RuntimeMethodBase methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    // prepare it, this should queue the method
    CHECK_AND_RETHROW(jit_prepare_method(methodInfo));

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
