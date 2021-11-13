#include <string.h>
#include "jit.h"

#include "assembly_internal.h"
#include "cil_opcodes.h"
#include "metadata_spec.h"

static MIR_var_t get_param_mir_type(type_t* type) {
    if (type == g_sbyte) {
        return (MIR_var_t) { .type = MIR_T_I8 };
    } else if (type == g_byte) {
        return (MIR_var_t) { .type = MIR_T_U8 };
    } else if (type == g_int16) {
        return (MIR_var_t) { .type = MIR_T_I16 };
    } else if (type == g_uint16) {
        return (MIR_var_t) { .type = MIR_T_U16 };
    } else if (type == g_int32) {
        return (MIR_var_t) { .type = MIR_T_I32 };
    } else if (type == g_uint32) {
        return (MIR_var_t) { .type = MIR_T_U32 };
    } else if (type == g_int64) {
        return (MIR_var_t) { .type = MIR_T_I64 };
    } else if (type == g_uint64) {
        return (MIR_var_t) { .type = MIR_T_U64 };
    } else if (type == g_float) {
        return (MIR_var_t) { .type = MIR_T_F };
    } else if (type == g_double) {
        return (MIR_var_t) { .type = MIR_T_D };
    } else if (type == g_uintptr) {
        return (MIR_var_t) { .type = type->stack_size == 4 ? MIR_T_U32 : MIR_T_U64 };
    } else if (type == g_intptr) {
        return (MIR_var_t) { .type = type->stack_size == 4 ? MIR_T_I32 : MIR_T_I64 };
    } else if (type->mod == TYPE_PTR || type->mod == TYPE_BY_REF) {
        return (MIR_var_t) { .type = MIR_T_P };
    } else if (type->is_value_type) {
        return (MIR_var_t) { .type = MIR_T_BLK, .size = type->stack_size };
    } else {
        return (MIR_var_t) { .type = MIR_T_P };
    }
}

static MIR_type_t get_local_mir_type(type_t* type) {
    switch (type->stack_type) {
        case STACK_TYPE_INT32:
        case STACK_TYPE_INT64: return MIR_T_I64;
        case STACK_TYPE_F: return MIR_T_D;
        case STACK_TYPE_NATIVE_INT: return MIR_T_I64; // TODO: porting
        case STACK_TYPE_T: return MIR_T_I64; // TODO: porting
        default: return MIR_T_UNDEF;
    }
}

static bool requires_stack_allocation(type_t* type) {
    if (type == g_sbyte ||
        type == g_byte ||
        type == g_int16 ||
        type == g_uint16 ||
        type == g_int32 ||
        type == g_uint32 ||
        type == g_int64 ||
        type == g_uint64 ||
        type == g_float ||
        type == g_double ||
        type == g_uintptr ||
        type == g_intptr ||
        type->mod == TYPE_BY_REF ||
        type->mod == TYPE_PTR
    ) {
        return false;
    } else if (type->is_value_type) {
        return true;
    } else {
        return false;
    }
}

#define CIL_FETCH(type) \
    ({ \
        CHECK(sizeof(type) <= cil_left); \
        type __value = *(type*)cil; \
        cil += sizeof(type); \
        cil_left -= sizeof(type); \
        __value; \
    })
#define CIL_FETCH_UINT8() CIL_FETCH(uint8_t)
#define CIL_FETCH_UINT16() CIL_FETCH(uint16_t)
#define CIL_FETCH_UINT32() CIL_FETCH(uint32_t)
#define CIL_FETCH_TOKEN() CIL_FETCH(token_t)

typedef struct arg_item {
    MIR_reg_t reg;
    type_t* type;
} arg_item_t;

typedef struct stack_item {
    MIR_reg_t reg_f;
    MIR_reg_t reg_int64;
    type_t* type;
} stack_item_t;

//TRACE("\tpush: %zd> %s:%d", stack_pointer, __FILE__, __LINE__);


#define STACK_PUSH(push_type) \
    ({ \
        MIR_reg_t __reg = 0; \
        CHECK(stack_pointer + 1 < stack_max); \
        stack_item_t* __stack_item = &stack[stack_pointer++]; \
        __stack_item->type = push_type; \
        if (__stack_item->type->stack_type == STACK_TYPE_F) { \
            if (__stack_item->reg_f == 0) { \
                char name_buffer[16]; \
                snprintf(name_buffer, sizeof(name_buffer), "sf%ld", stack_pointer - 1); \
                __stack_item->reg_f = MIR_new_func_reg(ctx, func->u.func, MIR_T_D, name_buffer); \
            } \
            __reg = __stack_item->reg_f; \
        } else { \
            if (__stack_item->reg_int64 == 0) { \
                char name_buffer[16]; \
                snprintf(name_buffer, sizeof(name_buffer), "si%ld", stack_pointer - 1); \
                __stack_item->reg_int64 = MIR_new_func_reg(ctx, func->u.func, MIR_T_I64, name_buffer); \
            } \
            __reg = __stack_item->reg_int64; \
        } \
        CHECK(__reg != 0);\
        __reg; \
    })

// TRACE("\tpop: %zd> %s:%d", stack_pointer, __FILE__, __LINE__);

#define STACK_POP(popped_type) \
    ({ \
        MIR_reg_t __reg = 0; \
        CHECK(stack_pointer >= 1, "Stack underflow"); \
        stack_item_t __stack_item = stack[--stack_pointer]; \
        if (__stack_item.type->stack_type == STACK_TYPE_F) { \
            __reg = __stack_item.reg_f; \
        } else { \
            __reg = __stack_item.reg_int64; \
        } \
        CHECK(__reg != 0, "Failed to get the register"); \
        if (popped_type != NULL) { \
            *(type_t**)(popped_type) = __stack_item.type; \
        } \
        __reg; \
    })

static void emit_inline_memcpy(
    MIR_context_t ctx, MIR_item_t func,
    MIR_reg_t to_base, size_t to_offset,
    MIR_reg_t from_base, size_t from_offset,
    size_t size
) {
    size_t size_left = size;
    while (size_left >= 8) {
        MIR_append_insn(ctx, func,
                        MIR_new_insn(ctx, MIR_MOV,
                                     MIR_new_mem_op(ctx, MIR_T_I64,
                                                    to_offset + size - size_left,
                                                    to_base, 0, 1),
                                     MIR_new_mem_op(ctx, MIR_T_I64,
                                                    from_offset + size - size_left,
                                                    from_base, 0, 1)));
        size_left -= 8;
    }
    if (size_left >= 4) {
        MIR_append_insn(ctx, func,
                        MIR_new_insn(ctx, MIR_MOV,
                                     MIR_new_mem_op(ctx, MIR_T_I32,
                                                    to_offset + size - size_left,
                                                    to_base, 0, 1),
                                     MIR_new_mem_op(ctx, MIR_T_I32,
                                                    from_offset + size - size_left,
                                                    from_base, 0, 1)));
        size_left -= 4;
    }
    if (size_left >= 2) {
        MIR_append_insn(ctx, func,
                        MIR_new_insn(ctx, MIR_MOV,
                                     MIR_new_mem_op(ctx, MIR_T_I16,
                                                    to_offset + size - size_left,
                                                    to_base, 0, 1),
                                     MIR_new_mem_op(ctx, MIR_T_I16,
                                                    from_offset + size - size_left,
                                                    from_base, 0, 1)));
        size_left -= 2;
    }
    if (size_left >= 1) {
        MIR_append_insn(ctx, func,
                        MIR_new_insn(ctx, MIR_MOV,
                                     MIR_new_mem_op(ctx, MIR_T_I8,
                                                    to_offset + size - size_left,
                                                    to_base, 0, 1),
                                     MIR_new_mem_op(ctx, MIR_T_I8,
                                                    from_offset + size - size_left,
                                                    from_base, 0, 1)));
        size_left -= 1;
    }
}

static err_t emit_binary_op(
        MIR_context_t ctx, MIR_item_t func,
        MIR_insn_code_t insn_code,
        stack_item_t* stack, size_t* stack_pointer_ptr, size_t stack_max) {
    err_t err = NO_ERROR;
    size_t stack_pointer = *stack_pointer_ptr;

    type_t* value2_type;
    MIR_reg_t value2_reg = STACK_POP(&value2_type);

    type_t* value1_type;
    MIR_reg_t value1_reg = STACK_POP(&value1_type);

    switch (value1_type->stack_type) {
        case STACK_TYPE_INT32: {
            switch (value2_type->stack_type) {
                case STACK_TYPE_INT32: {
                    MIR_reg_t result = STACK_PUSH(g_int32);
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, insn_code + 1,
                                                 MIR_new_reg_op(ctx, result),
                                                 MIR_new_reg_op(ctx, value1_reg),
                                                 MIR_new_reg_op(ctx, value2_reg)));
                } break;

                case STACK_TYPE_NATIVE_INT: {
                    MIR_reg_t result = STACK_PUSH(g_intptr);
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, insn_code,
                                                 MIR_new_reg_op(ctx, result),
                                                 MIR_new_reg_op(ctx, value1_reg),
                                                 MIR_new_reg_op(ctx, value2_reg)));
                } break;

                default: CHECK_FAIL();
            }
        } break;

        case STACK_TYPE_INT64: {
            switch (value2_type->stack_type) {
                case STACK_TYPE_INT32: {
                    MIR_reg_t result = STACK_PUSH(g_int32);
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, insn_code,
                                                 MIR_new_reg_op(ctx, result),
                                                 MIR_new_reg_op(ctx, value1_reg),
                                                 MIR_new_reg_op(ctx, value2_reg)));
                } break;

                default: CHECK_FAIL();
            }
        } break;

        case STACK_TYPE_NATIVE_INT: {
            switch (value2_type->stack_type) {
                case STACK_TYPE_NATIVE_INT:
                case STACK_TYPE_INT32: {
                    MIR_reg_t result = STACK_PUSH(g_intptr);
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, insn_code,
                                                 MIR_new_reg_op(ctx, result),
                                                 MIR_new_reg_op(ctx, value1_reg),
                                                 MIR_new_reg_op(ctx, value2_reg)));
                } break;
                default: CHECK_FAIL();
            }
        } break;

        case STACK_TYPE_F: {
            switch (value2_type->stack_type) {
                case STACK_TYPE_F: {
                    MIR_reg_t result = STACK_PUSH(g_double);
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, insn_code + 3,
                                                 MIR_new_reg_op(ctx, result),
                                                 MIR_new_reg_op(ctx, value1_reg),
                                                 MIR_new_reg_op(ctx, value2_reg)));
                } break;

                default: CHECK_FAIL();
            }
        } break;

        default: CHECK_FAIL();
    }

cleanup:
    *stack_pointer_ptr = stack_pointer;
    return err;
}

static err_t jit_prepare_method(jit_instance_t* instance, method_t* method, int method_index) {
    err_t err = NO_ERROR;
    MIR_context_t ctx = instance->context;
    size_t nargs = 0;
    MIR_var_t* args_temp_before = NULL;
    MIR_var_t* args_temp = NULL;
    MIR_reg_t* locals = NULL;
    stack_item_t* stack = NULL;
    arg_item_t* args = NULL;
    size_t stack_pointer = 0;
    size_t stack_max = 0;

    // some unique name for the function...
    char func_name[256] = {0 };
    snprintf(func_name, sizeof(func_name), "%s.%s::%s#%d", method->parent->namespace, method->parent->name, method->name, method_index);

    TRACE("%s", func_name);

    // setup the return value
    size_t nres = 0;
    MIR_type_t res_type = MIR_T_UNDEF;
    if (method->return_type != g_void) {
        res_type = get_local_mir_type(method->return_type);
        nres = 1;
    }

    // setup all the arguments properly
    nargs = (method->is_static ? 0 : 1) + method->parameter_count;
    args_temp = NULL;
    if (nargs > 0) {
        args_temp = calloc(nargs, sizeof(MIR_var_t));
        args_temp_before = calloc(nargs, sizeof(MIR_var_t));
        args = calloc(nargs, sizeof(stack_item_t));

        if (!method->is_static) {
            args_temp[0].type = MIR_T_P;
            args_temp[0].name = strdup("this");

            // set the real this type
            if (method->parent->is_value_type) {
                args[0].type = get_by_ref_type(method->parent);
            } else {
                args[0].type = method->parent;
            }
        }

        for (int i = (method->is_static ? 0 : 1); i < nargs; i++) {
            args[i].type = method->parameters[i - (method->is_static ? 0 : 1)].type;

            char name_buffer[16] = { 0 };
            snprintf(name_buffer, sizeof(name_buffer), "arg%d", i);
            args_temp[i] = get_param_mir_type(args[i].type);
            args_temp[i].name = strdup(name_buffer);
        }
    }

    // create the function, it is a bit annoying but the
    memcpy(args_temp_before, args_temp, sizeof(MIR_var_t) * nargs);
    MIR_item_t func = MIR_new_func_arr(ctx, func_name, nres, &res_type, nargs, args_temp);
    for (int i = 0; i < nargs; i++) {
        args[i].reg = MIR_reg(ctx, args_temp[i].name, func->u.func);
        SAFE_FREE(args_temp_before[i].name);
    }
    SAFE_FREE(args_temp_before);
    SAFE_FREE(args_temp);

    // setup all the locals as registers
    locals = NULL;
    if (method->locals_count > 0) {
        locals = calloc(method->locals_count, sizeof(MIR_reg_t));
        for (int i = 0; i < method->locals_count; i++) {
            local_t* local = &method->locals[i];

            // setup the variable for this register
            char name_buffer[16] = { 0 };
            snprintf(name_buffer, sizeof(name_buffer), "local%d", i);
            locals[i] = MIR_new_func_reg(ctx, func->u.func, get_local_mir_type(local->type), name_buffer);
        }
    }

    // setup the stack itself
    stack_max = method->max_stack_depth;
    if (stack_max > 0) {
        stack = calloc(stack_max, sizeof(stack_item_t));
    }

    // pass over the IL
    uint8_t* cil = method->cil;
    size_t cil_left = method->cil_size;
    while (cil_left) {

        int32_t value = 0;
        uint8_t opcode = CIL_FETCH_UINT8();

        switch (opcode) {
            case CIL_NOP: break;

            case CIL_LDARG_0: value = 0; goto do_ldarg;
            case CIL_LDARG_1: value = 1; goto do_ldarg;
            case CIL_LDARG_2: value = 2; goto do_ldarg;
            case CIL_LDARG_3: value = 3; goto do_ldarg;
            do_ldarg: {
                // make sure the argument exists
                CHECK(value < nargs);

                // push the value to the stack
                type_t* type = args[value].type;
                MIR_reg_t reg = STACK_PUSH(type);

                // append instruction
                if (type->stack_type == STACK_TYPE_INT32) {
                    // this is an int32 or intn, copy to stack slot
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, MIR_EXT32,
                                                 MIR_new_reg_op(ctx, reg),
                                                 MIR_new_reg_op(ctx, args[value].reg)));
                } else if (type->stack_type == STACK_TYPE_INT64 || type->stack_type == STACK_TYPE_NATIVE_INT) {
                    // this is an int64 or intn, copy to stack slot
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, MIR_MOV,
                                                 MIR_new_reg_op(ctx, reg),
                                                 MIR_new_reg_op(ctx, args[value].reg)));
                } else if (type->stack_type == STACK_TYPE_F) {
                    if (type == g_float) {
                        // this is a float, convert to double and copy to stack slot
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_F2D,
                                                     MIR_new_reg_op(ctx, reg),
                                                     MIR_new_reg_op(ctx, args[value].reg)));
                    } else {
                        // this is a double, just copy to stack slot
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_DMOV,
                                                     MIR_new_reg_op(ctx, reg),
                                                     MIR_new_reg_op(ctx, args[value].reg)));
                    }
                }
            } break;

            case CIL_LDARGA_S: {
                value = CIL_FETCH_UINT8();
                CHECK(value < nargs);

                // push the value to the stack
                MIR_reg_t reg = STACK_PUSH(get_by_ref_type(args[value].type));

                if (args[value].type->stack_type == STACK_TYPE_T && args[value].type->is_value_type) {
                    // for value types this is simple, we always have them on the stack
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, MIR_MOV,
                                                 MIR_new_reg_op(ctx, reg),
                                                 MIR_new_reg_op(ctx, args[value].reg)));
                } else {
                    CHECK_FAIL("TODO: addrof native value");
                }
            } break;

            case CIL_LDC_I4_M1: value = -1; goto do_ldc_i4;
            case CIL_LDC_I4_0: value = 0; goto do_ldc_i4;
            case CIL_LDC_I4_1: value = 1; goto do_ldc_i4;
            case CIL_LDC_I4_2: value = 2; goto do_ldc_i4;
            case CIL_LDC_I4_3: value = 3; goto do_ldc_i4;
            case CIL_LDC_I4_4: value = 4; goto do_ldc_i4;
            case CIL_LDC_I4_5: value = 5; goto do_ldc_i4;
            case CIL_LDC_I4_6: value = 6; goto do_ldc_i4;
            case CIL_LDC_I4_7: value = 7; goto do_ldc_i4;
            case CIL_LDC_I4_8: value = 8; goto do_ldc_i4;
            do_ldc_i4: {
                // push the value to the stack
                MIR_reg_t reg = STACK_PUSH(g_int32);

                MIR_append_insn(ctx, func,
                                MIR_new_insn(ctx, MIR_MOV,
                                             MIR_new_reg_op(ctx, reg),
                                             MIR_new_int_op(ctx, value)));
            } break;

            case CIL_CALL: {
                token_t target_token = CIL_FETCH_TOKEN();
                method_t* target = assembly_get_method_by_token(method->assembly, target_token);
                CHECK(target != NULL);

                // Pop it from the stack
                for (int i = 0; i < target->parameter_count + (target->is_static ? 0 : 1); i++) {
                    MIR_reg_t stack_reg = STACK_POP(NULL);
                }

                // push the return value if any
                if (target->return_type != g_void) {
                    MIR_reg_t ret_reg = STACK_PUSH(target->return_type);
                    // TODO: properly figure the function and shit...
                }
            } break;

            case CIL_RET: {
                if (method->return_type != g_void) {
                    type_t* ret_type;
                    MIR_reg_t ret_reg = STACK_POP(&ret_type);

                    MIR_append_insn(ctx, func,
                                    MIR_new_ret_insn(ctx, 1,
                                                     MIR_new_reg_op(ctx, ret_reg)));
                } else {
                    MIR_append_insn(ctx, func,
                                    MIR_new_ret_insn(ctx, 0));
                }
            } break;

            // all of these convert to the same value...
            case CIL_CONV_I:
            case CIL_CONV_U:
            case CIL_CONV_I8:
            case CIL_CONV_U8: {
                type_t* from_type;
                MIR_reg_t from_reg = STACK_POP(&from_type);
                MIR_reg_t to_reg = STACK_PUSH(
                opcode == CIL_CONV_U ? g_uintptr :
                        (opcode == CIL_CONV_I ? g_intptr :
                        (opcode == CIL_CONV_I8 ? g_int64 : g_uint64));
                );

                switch (from_type->stack_type) {
                    case STACK_TYPE_NATIVE_INT:
                    case STACK_TYPE_INT32:
                    case STACK_TYPE_INT64: {
                        // Currently, we only have 64bit ints in our stack so we
                        // have no need to convert to anything in here
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_MOV,
                                                     MIR_new_reg_op(ctx, to_reg),
                                                     MIR_new_reg_op(ctx, from_reg)));
                    } break;

                    case STACK_TYPE_F: {
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_D2I,
                                                     MIR_new_reg_op(ctx, to_reg),
                                                     MIR_new_reg_op(ctx, from_reg)));
                    } break;

                    default: CHECK_FAIL();
                }
            } break;

            case CIL_LDFLD: {
                token_t field_token = CIL_FETCH_TOKEN();
                CHECK(field_token.table == METADATA_FIELD);
                CHECK(field_token.index - 1 < method->assembly->fields_count);
                field_t* field = &method->assembly->fields[field_token.index - 1];

                // pop/push
                type_t* obj_type;
                MIR_reg_t obj_reg = STACK_POP(&obj_type);
                MIR_reg_t val_reg = STACK_PUSH(obj_type);

                // TODO: verify object is valid for field

                switch (field->type->stack_type) {
                    case STACK_TYPE_T: {
                        // if this is a value type then we need to allocate it
                        // on the stack first
                        if (field->type->is_value_type) {
                            // first allocate space for this type
                            MIR_append_insn(ctx, func,
                                            MIR_new_insn(ctx, MIR_ALLOCA,
                                                         MIR_new_reg_op(ctx, val_reg),
                                                         MIR_new_int_op(ctx, field->type->stack_size)));

                            // now we will memcpy it
                            emit_inline_memcpy(ctx, func, val_reg, 0, obj_reg, field->offset, field->type->stack_size);
                            break;
                        }

                        // anything else we falltrough to a value copy rather than a memcpy
                    }

                    default: {
                        MIR_type_t type;
                        MIR_insn_code_t insn_code = MIR_INVALID_INSN;
                        switch (field->type->stack_size) {
                            case 8: {
                                if (field->type == g_double) {
                                    type = MIR_T_D;
                                    insn_code = MIR_DMOV;
                                } else {
                                    type = MIR_T_I64;
                                    insn_code = MIR_MOV;
                                }
                            } break;

                            case 4: {
                                if (field->type == g_float) {
                                    type = MIR_T_F;
                                    insn_code = MIR_F2D;
                                } else if (field->type == g_int32) {
                                    type = MIR_T_I32;
                                    insn_code = MIR_EXT32;
                                } else {
                                    type = MIR_T_U32;
                                    insn_code = MIR_UEXT32;
                                }
                            } break;

                            case 2: {
                                if (field->type == g_int16) {
                                    type = MIR_T_I16;
                                    insn_code = MIR_EXT16;
                                } else {
                                    type = MIR_T_U16;
                                    insn_code = MIR_UEXT16;
                                }
                            } break;

                            case 1: {
                                if (field->type == g_sbyte) {
                                    type = MIR_T_I8;
                                    insn_code = MIR_EXT8;
                                } else {
                                    type = MIR_T_U8;
                                    insn_code = MIR_UEXT8;
                                }
                            } break;

                            default: CHECK_FAIL("Invalid stack size");
                        }
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, insn_code,
                                                     MIR_new_reg_op(ctx, val_reg),
                                                     MIR_new_mem_op(ctx, type,
                                                                    field->offset,
                                                                    obj_reg, 0, 1)));
                    } break;
                }
            } break;

            case CIL_STFLD: {
                token_t field_token = CIL_FETCH_TOKEN();
                CHECK(field_token.table == METADATA_FIELD);
                CHECK(field_token.index - 1 < method->assembly->fields_count);
                field_t* field = &method->assembly->fields[field_token.index - 1];

                type_t* val_type;
                MIR_reg_t val_reg = STACK_POP(&val_type);

                type_t* obj_type;
                MIR_reg_t obj_reg = STACK_POP(&obj_type);

                // TODO: type verification

                switch (field->type->stack_type) {
                    case STACK_TYPE_T: {
                        if (field->type->is_value_type) {
                            // we need to do a memcpy in this case
                            emit_inline_memcpy(ctx, func, obj_reg, field->offset, val_reg, 0, field->type->stack_size);
                            break;
                        }

                        // anything else we falltrough to a value copy rather than a memcpy
                    }

                    default: {
                        MIR_type_t type;
                        MIR_insn_code_t insn_code = MIR_MOV;
                        switch (field->type->stack_size) {
                            case 8: {
                                if (field->type == g_double) {
                                    type = MIR_T_D;
                                    insn_code = MIR_DMOV;
                                } else {
                                    type = MIR_T_I64;
                                }
                            } break;

                            case 4: {
                                if (field->type == g_float) {
                                    type = MIR_T_F;
                                    insn_code = MIR_D2F;
                                } else if (field->type == g_int32) {
                                    type = MIR_T_I32;
                                } else {
                                    type = MIR_T_U32;
                                }
                            } break;

                            case 2: {
                                if (field->type == g_int16) {
                                    type = MIR_T_I16;
                                } else {
                                    type = MIR_T_U16;
                                }
                            } break;

                            case 1: {
                                if (field->type == g_sbyte) {
                                    type = MIR_T_I8;
                                } else {
                                    type = MIR_T_U8;
                                }
                            } break;

                            default: CHECK_FAIL("Invalid stack size");
                        }

                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, insn_code,
                                                     MIR_new_mem_op(ctx, type,
                                                                    field->offset,
                                                                    obj_reg, 0, 1),
                                                     MIR_new_reg_op(ctx, val_reg)));
                    }
                }
            } break;

            case CIL_EXTENDED: {
                opcode = CIL_FETCH_UINT8();
                switch (opcode) {
                    case CILX_CEQ: {
                        CHECK_AND_RETHROW(emit_binary_op(ctx, func, MIR_EQ, stack, &stack_pointer, stack_max));
                    } break;

                    default: WARN("Invalid extended opcode fe %x", opcode); goto cleanup;
                }
            } break;

            default: WARN("Invalid opcode %x", opcode); goto cleanup;
        }
    }

cleanup:

    // we done with functions
    MIR_finish_func(ctx);

    if (IS_ERROR(err)) {
        MIR_output_item(ctx, stdout, func);
    }

    SAFE_FREE(locals);
    SAFE_FREE(stack);
    SAFE_FREE(args);

    return err;
}

static err_t jit_prepare_type(jit_instance_t* instance, type_t* type) {
    err_t err = NO_ERROR;

    for (int i = 0; i < type->methods_count; i++) {
        method_t* method = &type->methods[i];
        CHECK_AND_RETHROW(jit_prepare_method(instance, method, i));
    }

cleanup:
    return err;
}

err_t jit_prepare_assembly(jit_instance_t* instance, assembly_t* assembly) {
    err_t err = NO_ERROR;

    instance->context = MIR_init();
    CHECK(instance->context != NULL);

    MIR_module_t mod = MIR_new_module(instance->context, assembly->name);

    for (int i = 0; i < assembly->types_count; i++) {
        type_t* type = &assembly->types[i];
        CHECK_AND_RETHROW(jit_prepare_type(instance, type));
    }

    MIR_finish_module(instance->context);

    MIR_output(instance->context, stdout);

    // write it out
    FILE* out = open_memstream(&assembly->module_data, &assembly->module_data_size);
    MIR_write_module(instance->context, out, mod);
    fclose(out);

cleanup:
    if (IS_ERROR(err)) {
        MIR_finish(instance->context);
        instance->context = NULL;
    }
    return err;
}
