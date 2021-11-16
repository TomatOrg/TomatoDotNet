#include <string.h>
#include <util/stb_ds.h>
#include "jit.h"

#include "assembly_internal.h"
#include "cil_opcodes.h"
#include "metadata_spec.h"

static MIR_type_t get_param_mir_type(type_t* type) {
    if (type == g_sbyte) {
        return MIR_T_I8;
    } else if (type == g_byte || type == g_boolean) {
        return MIR_T_U8;
    } else if (type == g_int16) {
        return MIR_T_I16;
    } else if (type == g_uint16 || type == g_char) {
        return MIR_T_U16;
    } else if (type == g_int32) {
        return MIR_T_I32;
    } else if (type == g_uint32) {
        return MIR_T_U32;
    } else if (type == g_int64) {
        return MIR_T_I64;
    } else if (type == g_uint64) {
        return MIR_T_U64;
    } else if (type == g_float) {
        return MIR_T_F;
    } else if (type == g_double) {
        return MIR_T_D;
    } else if (type == g_uintptr) {
        return type->stack_size == 4 ? MIR_T_U32 : MIR_T_U64;
    } else if (type == g_intptr) {
        return type->stack_size == 4 ? MIR_T_I32 : MIR_T_I64;
    } else if (type->mod == TYPE_PTR || type->mod == TYPE_BY_REF) {
        return MIR_T_P;
    } else if (type->is_value_type) {
        // TODO: pass value types by value...
        return MIR_T_UNDEF;
    } else {
        return MIR_T_P;
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
#define CIL_FETCH_UINT64() CIL_FETCH(uint64_t)
#define CIL_FETCH_FLOAT32() CIL_FETCH(float)
#define CIL_FETCH_FLOAT64() CIL_FETCH(double)
#define CIL_FETCH_TOKEN() CIL_FETCH(token_t)

typedef struct arg_item {
    MIR_op_t op;
    type_t* type;
} arg_item_t;

typedef struct stack_item {
    MIR_reg_t reg_f;
    MIR_reg_t reg_int64;
    type_t* type;
} stack_item_t;

typedef struct method_proto {
    const char* key;
    MIR_proto_t value;
} method_proto_t;

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

static void emit_inline_memset(
    MIR_context_t ctx, MIR_item_t func,
    MIR_reg_t to_base, size_t to_offset,
    uint8_t value8, size_t size
) {
    uint16_t value16 = ((uint16_t)value8) | ((uint16_t)value8 << 8);
    uint32_t value32 = ((uint32_t)value16) | ((uint32_t)value16 << 16);
    uint64_t value64 = ((uint64_t)value32) | ((uint64_t)value32 << 32);
    size_t size_left = size;
    while (size_left >= 8) {
        MIR_append_insn(ctx, func,
                        MIR_new_insn(ctx, MIR_MOV,
                                     MIR_new_mem_op(ctx, MIR_T_I64,
                                                    to_offset + size - size_left,
                                                    to_base, 0, 1),
                                     MIR_new_uint_op(ctx, value64)));
        size_left -= 8;
    }
    if (size_left >= 4) {
        MIR_append_insn(ctx, func,
                        MIR_new_insn(ctx, MIR_MOV,
                                     MIR_new_mem_op(ctx, MIR_T_I32,
                                                    to_offset + size - size_left,
                                                    to_base, 0, 1),
                                     MIR_new_uint_op(ctx, value32)));
        size_left -= 4;
    }
    if (size_left >= 2) {
        MIR_append_insn(ctx, func,
                        MIR_new_insn(ctx, MIR_MOV,
                                     MIR_new_mem_op(ctx, MIR_T_I16,
                                                    to_offset + size - size_left,
                                                    to_base, 0, 1),
                                     MIR_new_uint_op(ctx, value16)));
        size_left -= 2;
    }
    if (size_left >= 1) {
        MIR_append_insn(ctx, func,
                        MIR_new_insn(ctx, MIR_MOV,
                                     MIR_new_mem_op(ctx, MIR_T_I8,
                                                    to_offset + size - size_left,
                                                    to_base, 0, 1),
                                     MIR_new_uint_op(ctx, value8)));
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

static err_t emit_func_and_proto(MIR_context_t ctx, method_t* method, bool prototype_only, MIR_item_t* out_func) {
    err_t err = NO_ERROR;
    MIR_var_t* mir_args = NULL;
    const char** arg_names = NULL;

    // some unique name for the function...
    char func_name[256];
    CHECK_AND_RETHROW(jit_mangle_name(method, func_name, sizeof(func_name)));

    char proto_func_name[sizeof(func_name) + 10];
    snprintf(proto_func_name, sizeof(proto_func_name), "proto$%s", func_name);

    // setup the return value
    size_t nres = 0;
    MIR_type_t res_type = MIR_T_UNDEF;
    if (method->return_type != g_void) {
        res_type = get_local_mir_type(method->return_type);
        nres = 1;
    }

    // setup the argument types correctly
    size_t nargs = method->parameter_count;
    mir_args = NULL;
    if (nargs > 0) {
        mir_args = calloc(nargs, sizeof(*mir_args));
        for (int i = 0; i < nargs; i++) {
            mir_args[i].type = get_param_mir_type(method->parameters[i].type);
            mir_args[i].name = "";
        }
    }

    // create the proto
    MIR_item_t proto = MIR_get_global_item(ctx, proto_func_name);
    if (proto == NULL) {
        proto = MIR_new_proto_arr(ctx, proto_func_name, nres, &res_type, nargs, mir_args);
    }

    if (prototype_only) {
        // out the proto item
        *out_func = proto;
    } else {
        // setup the names correctly, these are only needed for function declaration
        if (nargs > 0) {
            arg_names = calloc(nargs, sizeof(*arg_names));
            for (int i = 0; i < nargs; i++) {
                const char* name_ptr;
                char name_buffer[64];
                if (method->parameters[i].name != NULL) {
                    name_ptr = method->parameters[i].name;
                } else {
                    snprintf(name_buffer, sizeof(name_buffer), "arg%d", i);
                    name_ptr = arg_names[i] = strdup(name_buffer);
                }
                mir_args[i].name = name_ptr;
            }
        }

        // out the new func
        *out_func = MIR_new_func_arr(ctx, func_name, nres, &res_type, nargs, mir_args);
    }

cleanup:
    if (arg_names != NULL) {
        for (int i = 0; i < nargs; i++) {
            SAFE_FREE(arg_names[i]);
        }
        SAFE_FREE(arg_names);
    }
    SAFE_FREE(mir_args);
    return err;
}

static err_t emit_call(
        MIR_context_t ctx, MIR_item_t func, method_t* method,
        stack_item_t* stack, size_t* stack_pointer_ptr, size_t stack_max) {
    err_t err = NO_ERROR;
    size_t stack_pointer = *stack_pointer_ptr;
    MIR_op_t* call_args = NULL;

    // the call arguments
    char func_name[256];
    CHECK_AND_RETHROW(jit_mangle_name(method, func_name, sizeof(func_name)));

    char proto_func_name[sizeof(func_name) + 10];
    snprintf(proto_func_name, sizeof(proto_func_name), "proto$%s", func_name);

    // get the prototype, if it does not exist then emit it
    MIR_item_t proto = MIR_get_global_item(ctx, proto_func_name);
    if (proto == NULL) {
        CHECK_AND_RETHROW(emit_func_and_proto(ctx, method, true, &proto));
    }
    arrpush(call_args, MIR_new_ref_op(ctx, proto));

    // forward declaration for function
    MIR_item_t target_func = MIR_new_forward(ctx, func_name);
    arrpush(call_args, MIR_new_ref_op(ctx, target_func));

    // pop all the items that we need to pass
    for (int i = 0; i < method->parameter_count; i++) {
        MIR_reg_t stack_reg = STACK_POP(NULL);

        // TODO: type checking

        arrpush(call_args, MIR_new_reg_op(ctx, stack_reg));
    }

    // push the return value if any
    if (method->return_type != g_void) {
        MIR_reg_t ret_reg = STACK_PUSH(method->return_type);
        arrins(call_args, 2, MIR_new_reg_op(ctx, ret_reg));
    }

    MIR_append_insn(ctx, func,
                    MIR_new_insn_arr(ctx, MIR_CALL, arrlen(call_args), call_args));

cleanup:
    arrfree(call_args);

    *stack_pointer_ptr = stack_pointer;
    return err;
}

static err_t jit_prepare_method(jit_instance_t* instance, method_t* method) {
    err_t err = NO_ERROR;
    MIR_context_t ctx = instance->context;
    stack_item_t* stack = NULL;
    arg_item_t* locals = NULL;
    arg_item_t* args = NULL;
    size_t stack_pointer = 0;
    size_t stack_max = 0;

//    TRACE("%s.%s::%s", method->parent->namespace, method->parent->name, method->name);

    // setup all the arguments properly
    if (method->parameter_count > 0) {
        args = calloc(method->parameter_count, sizeof(*args));
        for (int i = 0; i < method->parameter_count; i++) {
            args[i].type = method->parameters[i].type;
            CHECK(args[i].type != NULL);
        }
    }

    MIR_item_t func;
    CHECK_AND_RETHROW(emit_func_and_proto(ctx, method, false, &func));

    // create the function, it is a bit annoying but the
    for (int i = 0; i < method->parameter_count; i++) {
        const char* name_ptr;
        char name_buffer[64];
        if (method->parameters[i].name != NULL) {
            name_ptr = method->parameters[i].name;
        } else {
            snprintf(name_buffer, sizeof(name_buffer), "arg%d", i);
            name_ptr = name_buffer;
        }
        args[i].op = MIR_new_reg_op(ctx, MIR_reg(ctx, name_ptr, func->u.func));
    }

    // setup all the locals as registers
    locals = NULL;
    if (method->locals_count > 0) {
        locals = calloc(method->locals_count, sizeof(*locals));
        for (int i = 0; i < method->locals_count; i++) {
            local_t* local = &method->locals[i];

            // setup the variable for this register
            char name_buffer[16] = { 0 };
            snprintf(name_buffer, sizeof(name_buffer), "local%d", i);
            locals[i].op = MIR_new_reg_op(ctx, MIR_new_func_reg(ctx, func->u.func, get_local_mir_type(local->type), name_buffer));
            locals[i].type = local->type;

            if (type_is_valuetype(local->type)) {
                // need to allocate some space for this...
                MIR_append_insn(ctx, func,
                                MIR_new_insn(ctx, MIR_ALLOCA,
                                             locals[i].op,
                                             MIR_new_uint_op(ctx, local->type->stack_size)));
            }
        }
    }

    // setup the stack itself
    stack_max = method->max_stack_depth;
    if (stack_max > 0) {
        stack = calloc(stack_max, sizeof(*stack));
    }

    // pass over the IL
    uint8_t* cil = method->cil;
    size_t cil_left = method->cil_size;
    while (cil_left) {

        int32_t value32 = 0;
        int64_t value64 = 0;
        arg_item_t* items = NULL;
        size_t items_count = 0;
        const char* str = NULL;
        uint8_t opcode = CIL_FETCH_UINT8();

        switch (opcode) {
            case CIL_NOP: break;

            case CIL_LDARG_0: value32 = 0; items = args; items_count = method->parameter_count; goto do_ld;
            case CIL_LDARG_1: value32 = 1; items = args; items_count = method->parameter_count; goto do_ld;
            case CIL_LDARG_2: value32 = 2; items = args; items_count = method->parameter_count; goto do_ld;
            case CIL_LDARG_3: value32 = 3; items = args; items_count = method->parameter_count; goto do_ld;

            case CIL_LDLOC_0: value32 = 0; items = locals; items_count = method->locals_count; goto do_ld;
            case CIL_LDLOC_1: value32 = 1; items = locals; items_count = method->locals_count; goto do_ld;
            case CIL_LDLOC_2: value32 = 2; items = locals; items_count = method->locals_count; goto do_ld;
            case CIL_LDLOC_3: value32 = 3; items = locals; items_count = method->locals_count; goto do_ld;

            do_ld: {
                // make sure the argument exists
                CHECK(value32 < items_count);

                // push the value32 to the stack
                type_t* type = items[value32].type;
                MIR_reg_t reg = STACK_PUSH(type);

                // append instruction
                if (type->stack_type == STACK_TYPE_F) {
                    if (type == g_float) {
                        // this is a float, convert to double and copy to stack slot
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_F2D,
                                                     MIR_new_reg_op(ctx, reg),
                                                     items[value32].op));
                    } else {
                        // this is a double, just copy to stack slot
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_DMOV,
                                                     MIR_new_reg_op(ctx, reg),
                                                     items[value32].op));
                    }
                } else {
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, MIR_MOV,
                                                 MIR_new_reg_op(ctx, reg),
                                                 items[value32].op));
                }
            } break;

            case CIL_STLOC_0: value32 = 0; items = locals; items_count = method->locals_count; goto do_st;
            case CIL_STLOC_1: value32 = 1; items = locals; items_count = method->locals_count; goto do_st;
            case CIL_STLOC_2: value32 = 2; items = locals; items_count = method->locals_count; goto do_st;
            case CIL_STLOC_3: value32 = 3; items = locals; items_count = method->locals_count; goto do_st;

            do_st: {
                // make sure the argument exists
                CHECK(value32 < items_count);

                // push the value32 to the stack
                type_t* type = items[value32].type;
                type_t* got_type;
                MIR_reg_t reg = STACK_POP(&got_type);

                // TODO: type validation

                // append instruction
                if (type->stack_type == STACK_TYPE_F) {
                    if (type == g_float) {
                        // this is a float, convert to double and copy to stack slot
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_D2F,
                                                     items[value32].op,
                                                     MIR_new_reg_op(ctx, reg)));
                    } else {
                        // this is a double, just copy to stack slot
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_DMOV,
                                                     items[value32].op,
                                                     MIR_new_reg_op(ctx, reg)));
                    }
                } else {
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, MIR_MOV,
                                                 items[value32].op,
                                                 MIR_new_reg_op(ctx, reg)));
                }
            } break;

            // TODO: these are very similar, abstract them somehow?

            case CIL_LDARGA_S: {
                value32 = CIL_FETCH_UINT8();
                items_count = method->parameter_count;
                items = args;
                str = "arga%d";
            } goto do_lda;

            case CIL_LDLOCA_S: {
                value32 = CIL_FETCH_UINT8();
                items_count = method->locals_count;
                items = locals;
                str = "loca%d";
            } goto do_lda;

            do_lda: {
                CHECK(value32 < items_count);

                // push the value32 to the stack
                MIR_reg_t reg = STACK_PUSH(get_by_ref_type(items[value32].type));

                if (type_is_valuetype(items[value32].type)) {
                    // for value32 types this is simple, we always have them on the stack
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, MIR_MOV,
                                                 MIR_new_reg_op(ctx, reg),
                                                 items[value32].op));
                } else {
                    char name_buffer[16];
                    snprintf(name_buffer, sizeof(name_buffer), str, value32);

                    // check if the value32 needs to be spilled
                    if (items[value32].op.mode == MIR_OP_REG) {
                        // allocate the new variable space
                        MIR_reg_t new_base = MIR_new_func_reg(ctx, func->u.func, MIR_T_I64, name_buffer);
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_ALLOCA,
                                                     MIR_new_reg_op(ctx, new_base),
                                                     MIR_new_int_op(ctx, items[value32].type->stack_size)));

                        // setup the new operands
                        MIR_op_t prev_op = items[value32].op;
                        items[value32].op = MIR_new_mem_op(ctx, get_param_mir_type(items[value32].type), 0, new_base, 0, 1);

                        // actually move it
                        MIR_insn_code_t insn_code;
                        if (items[value32].type == g_double) {
                            // use the double move
                            insn_code = MIR_DMOV;
                        } else if (items[value32].type == g_float) {
                            // move it by converting from a double (stack value32) to a float
                            insn_code = MIR_D2F;
                        } else {
                            // just move it
                            insn_code = MIR_MOV;
                        }
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, insn_code,
                                                     items[value32].op,
                                                     prev_op));
                    }

                    // now pass the base
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, MIR_MOV,
                                                 MIR_new_reg_op(ctx, reg),
                                                 MIR_new_reg_op(ctx, MIR_reg(ctx, name_buffer, func->u.func))));
                }
            } break;

            case CIL_LDC_I4_M1: value32 = -1; goto do_ldc_i4;
            case CIL_LDC_I4_0: value32 = 0; goto do_ldc_i4;
            case CIL_LDC_I4_1: value32 = 1; goto do_ldc_i4;
            case CIL_LDC_I4_2: value32 = 2; goto do_ldc_i4;
            case CIL_LDC_I4_3: value32 = 3; goto do_ldc_i4;
            case CIL_LDC_I4_4: value32 = 4; goto do_ldc_i4;
            case CIL_LDC_I4_5: value32 = 5; goto do_ldc_i4;
            case CIL_LDC_I4_6: value32 = 6; goto do_ldc_i4;
            case CIL_LDC_I4_7: value32 = 7; goto do_ldc_i4;
            case CIL_LDC_I4_8: value32 = 8; goto do_ldc_i4;
            case CIL_LDC_I4_S: value32 = CIL_FETCH_UINT8(); goto do_ldc_i4;
            case CIL_LDC_I4: value32 = CIL_FETCH_UINT32(); goto do_ldc_i4;
            do_ldc_i4: {
                MIR_reg_t reg = STACK_PUSH(g_int32);
                MIR_append_insn(ctx, func,
                                MIR_new_insn(ctx, MIR_MOV,
                                             MIR_new_reg_op(ctx, reg),
                                             MIR_new_int_op(ctx, value32)));
            } break;

            case CIL_LDC_I8: {
                uint64_t value64 = CIL_FETCH_UINT64();
                MIR_reg_t reg = STACK_PUSH(g_int64);
                MIR_append_insn(ctx, func,
                                MIR_new_insn(ctx, MIR_MOV,
                                             MIR_new_reg_op(ctx, reg),
                                             MIR_new_int_op(ctx, value32)));
            } break;


            case CIL_LDC_R4: {
                float fvalue = CIL_FETCH_FLOAT32();
                MIR_reg_t reg = STACK_PUSH(g_float);
                MIR_append_insn(ctx, func,
                                MIR_new_insn(ctx, MIR_DMOV,
                                             MIR_new_reg_op(ctx, reg),
                                             MIR_new_double_op(ctx, fvalue)));
            } break;

            case CIL_LDC_R8: {
                double dvalue = CIL_FETCH_FLOAT64();
                MIR_reg_t reg = STACK_PUSH(g_double);
                MIR_append_insn(ctx, func,
                                MIR_new_insn(ctx, MIR_DMOV,
                                             MIR_new_reg_op(ctx, reg),
                                             MIR_new_double_op(ctx, dvalue)));
            } break;

            case CIL_CALL: {
                token_t target_token = CIL_FETCH_TOKEN();
                method_t* target = assembly_get_method_by_token(method->assembly, target_token);
                CHECK(target != NULL);

                CHECK_AND_RETHROW(emit_call(ctx, func, method, stack, &stack_pointer, stack_max));
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

            case CIL_ADD: CHECK_AND_RETHROW(emit_binary_op(ctx, func, MIR_ADD, stack, &stack_pointer, stack_max)); break;
            case CIL_SUB: CHECK_AND_RETHROW(emit_binary_op(ctx, func, MIR_SUB, stack, &stack_pointer, stack_max)); break;
            case CIL_MUL: CHECK_AND_RETHROW(emit_binary_op(ctx, func, MIR_MUL, stack, &stack_pointer, stack_max)); break;
            case CIL_DIV: CHECK_AND_RETHROW(emit_binary_op(ctx, func, MIR_DIV, stack, &stack_pointer, stack_max)); break;

            // convert unsigned int to unsigned int with overflow
            case CIL_CONV_OVF_U1_UN: value64 = 0xff; goto do_conv_ovf_un;
            case CIL_CONV_OVF_U2_UN: value64 = 0xffff; goto do_conv_ovf_un;
            case CIL_CONV_OVF_U4_UN: value64 = 0xffffffff; goto do_conv_ovf_un;
            case CIL_CONV_OVF_U8_UN: value64 = 0; goto do_conv_ovf_un;
            case CIL_CONV_OVF_U_UN: value64 = 0; goto do_conv_ovf_un;
            do_conv_ovf_un: {
                type_t* from_type;
                MIR_reg_t from_reg = STACK_POP(&from_type);
                CHECK(from_type->stack_type != STACK_TYPE_T);

                // TODO: type checking

                // if we cast to something which is not 64bit, then add a check
                // for overflow and fail if it fails
                if (value64 != 0) {
                    MIR_insn_t after_throw = MIR_new_label(ctx);
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, MIR_UBLE,
                                                 MIR_new_label_op(ctx, after_throw),
                                                 MIR_new_reg_op(ctx, from_reg),
                                                 MIR_new_uint_op(ctx, value64)));
                    // TODO: throw exception
                    MIR_append_insn(ctx, func, after_throw);
                }

                // do the convertion now
                MIR_reg_t to_reg = STACK_PUSH(opcode == CIL_CONV_OVF_U_UN ? g_intptr : (value32 == 0 ? g_int64 : g_int32));
                if (from_type->stack_type == STACK_TYPE_F) {
                    // TODO: this
                } else {
                    if (value64 == 0xFF) {
                        // Truncate to 1 byte
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_UEXT8,
                                                     MIR_new_reg_op(ctx, to_reg),
                                                     MIR_new_reg_op(ctx, from_reg)));
                    } else if (value64 == 0xFFFF) {
                        // Truncate to 2 byte
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_UEXT16,
                                                     MIR_new_reg_op(ctx, to_reg),
                                                     MIR_new_reg_op(ctx, from_reg)));
                    } else if (value64 == 0xFFFFFFFF) {
                        // Truncate to 4 byte, we need to do it even for int32 because
                        // we assume signed integer on the stack, so we need to truncate
                        // it properly by zero extending
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_UEXT32,
                                                     MIR_new_reg_op(ctx, to_reg),
                                                     MIR_new_reg_op(ctx, from_reg)));
                    }
                    // for 64bit values there is nothing to do...
                }
            } break;

            case CIL_CONV_I:
            case CIL_CONV_U:
            case CIL_CONV_I8:
            case CIL_CONV_U8: {
                type_t* from_type;
                MIR_reg_t from_reg = STACK_POP(&from_type);
                MIR_reg_t to_reg = STACK_PUSH(g_int64);

                // figure which type of convertion is needed
                MIR_insn_code_t insn_code = MIR_MOV;
                switch (from_type->stack_type) {
                    case STACK_TYPE_INT32: {
                        if (opcode == CIL_CONV_U || opcode == CIL_CONV_U8) {
                            // zero extend
                            insn_code = MIR_UEXT32;
                        } else {
                            // sign extend
                            insn_code = MIR_EXT32;
                        }
                    } break;

                    case STACK_TYPE_NATIVE_INT:
                    case STACK_TYPE_INT64: {
                        // nop
                    } break;

                    case STACK_TYPE_F: {
                        // truncate to zero
                        insn_code = MIR_D2I;
                    } break;

                    default: CHECK_FAIL();
                }

                // do the convertion
                MIR_append_insn(ctx, func,
                                MIR_new_insn(ctx, insn_code,
                                             MIR_new_reg_op(ctx, to_reg),
                                             MIR_new_reg_op(ctx, from_reg)));
            } break;

            case CIL_CONV_I1:
            case CIL_CONV_I2:
            case CIL_CONV_U1:
            case CIL_CONV_U2: {
                type_t* from_type;
                MIR_reg_t from_reg = STACK_POP(&from_type);
                MIR_reg_t to_reg = STACK_PUSH(g_int64);

                // figure which type of convertion is needed
                MIR_insn_code_t insn_code = MIR_MOV;
                switch (from_type->stack_type) {
                    case STACK_TYPE_NATIVE_INT:
                    case STACK_TYPE_INT64:
                    case STACK_TYPE_INT32: {
                        // truncate
                        switch (opcode) {
                            case CIL_CONV_I1: insn_code = MIR_EXT8; break;
                            case CIL_CONV_I2: insn_code = MIR_EXT16; break;
                            case CIL_CONV_U1: insn_code = MIR_UEXT8; break;
                            case CIL_CONV_U2: insn_code = MIR_UEXT16; break;
                        }
                    } break;

                    case STACK_TYPE_F: {
                        // truncate to zero
                        insn_code = MIR_D2I;
                    } break;

                    default: CHECK_FAIL();
                }

                // do the convertion
                MIR_append_insn(ctx, func,
                                MIR_new_insn(ctx, insn_code,
                                             MIR_new_reg_op(ctx, to_reg),
                                             MIR_new_reg_op(ctx, from_reg)));
            } break;

            case CIL_NEWOBJ: {
                token_t target_ctor = CIL_FETCH_TOKEN();
                method_t* ctor = assembly_get_method_by_token(method->assembly, target_ctor);
                CHECK(ctor != NULL);

                // ctor should not have any return type
                CHECK(ctor->return_type == g_void);

                // allocate the object, handle properly value32 type/not value32 type
                MIR_reg_t newobj_ref;
                if (ctor->parent->is_value_type) {
                    if (type_is_valuetype(ctor->parent)) {
                        // this is a normal user struct
                        newobj_ref = STACK_PUSH(ctor->parent);
                    } else {
                        // this is a native type
                        newobj_ref = STACK_PUSH(get_by_ref_type(ctor->parent));
                    }
                    // allocate on the stack and zero out
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, MIR_ALLOCA,
                                                 MIR_new_reg_op(ctx, newobj_ref),
                                                 MIR_new_int_op(ctx, ctor->parent->stack_size)));
                    emit_inline_memset(ctx, func, newobj_ref, 0, 0, ctor->parent->stack_size);

                } else {
                    // TODO: allocate properly
                    CHECK_FAIL();
                }

                // call the ctor
                CHECK_AND_RETHROW(emit_call(ctx, func, ctor, stack, &stack_pointer, stack_max));

                // now push the value32 properly
                MIR_reg_t newobj_val = STACK_PUSH(ctor->parent);
                if (ctor->parent->is_value_type) {
                    if (type_is_valuetype(ctor->parent)) {
                        // this is a normal user defined value32 type
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_MOV,
                                                     MIR_new_reg_op(ctx, newobj_val),
                                                     MIR_new_int_op(ctx, newobj_ref)));
                    } else {
                        // this is a native type, deref it
                        MIR_append_insn(ctx, func,
                                        MIR_new_insn(ctx, MIR_MOV,
                                                     MIR_new_reg_op(ctx, newobj_val),
                                                     MIR_new_mem_op(ctx, get_param_mir_type(ctor->parent), 0, newobj_ref, 0, 1)));
                    }
                } else {
                    // this is a normal ref, copy it
                    MIR_append_insn(ctx, func,
                                    MIR_new_insn(ctx, MIR_MOV,
                                                 MIR_new_reg_op(ctx, newobj_val),
                                                 MIR_new_int_op(ctx, newobj_ref)));
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
                MIR_reg_t val_reg = STACK_PUSH(field->type);

                // TODO: verify object is valid for field

                switch (field->type->stack_type) {
                    case STACK_TYPE_T: {
                        // if this is a value32 type then we need to allocate it
                        // on the stack first
                        if (field->type->is_value_type) {
                            // now we will memcpy it
                            emit_inline_memcpy(ctx, func, val_reg, 0, obj_reg, field->offset, field->type->stack_size);
                            break;
                        }

                        // anything else we falltrough to a value32 copy rather than a memcpy
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
                                    insn_code = MIR_MOV;
                                } else {
                                    type = MIR_T_U32;
                                    insn_code = MIR_MOV;
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

                        // anything else we falltrough to a value32 copy rather than a memcpy
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

                    case CILX_INITOBJ: {
                        token_t type_token = CIL_FETCH_TOKEN();

                        // TODO: verify the type token is the same as the popped value32...

                        // just zero initialize everything, since default for every type
                        // is basically zero
                        type_t* type;
                        MIR_reg_t reg = STACK_POP(&type);
                        emit_inline_memset(ctx, func, reg, 0, 0, type->stack_size);
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
        CHECK_AND_RETHROW(jit_prepare_method(instance, method));
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

    MIR_gen_init(instance->context, 0);
    MIR_gen_set_optimize_level(instance->context, 0, 4);
    MIR_load_module(instance->context, mod);
    MIR_link(instance->context, MIR_set_gen_interface, NULL);

    MIR_item_t func = MIR_get_global_item(instance->context, "nuint[Corelib.dll]System.UIntPtr::op_Explicit(uint)");
    CHECK(func != NULL);
    _MIR_dump_code("lol", 0, func->u.func->call_addr, 256);

    MIR_gen_finish(instance->context);

    // write it out
    FILE* out = open_memstream(&assembly->module_data, &assembly->module_data_size);
    MIR_write_module(instance->context, out, mod);
    fclose(out);

cleanup:
    if (IS_ERROR(err)) {
        MIR_finish_module(instance->context);
        MIR_finish(instance->context);
        instance->context = NULL;
    }
    return err;
}

err_t jit_mangle_name(method_t* method, char* mangled_name, size_t buffer_size) {
    err_t err = NO_ERROR;
    int printed;

    printed = type_write_name(method->return_type, mangled_name, buffer_size);
    CHECK(printed < buffer_size);
    buffer_size -= printed;
    mangled_name += printed;

    printed = snprintf(mangled_name, buffer_size, "[%s]%s.%s::%s(",
             method->assembly->name, method->parent->namespace,
             method->parent->name, method->name);
    CHECK(printed < buffer_size);
    buffer_size -= printed;
    mangled_name += printed;

    for (int i = 0; i < method->parameter_count; i++) {
        printed = type_write_name(method->parameters[i].type, mangled_name, buffer_size);
        CHECK(printed < buffer_size);
        buffer_size -= printed;
        mangled_name += printed;
        if (i != method->parameter_count - 1) {
            printed = snprintf(mangled_name, buffer_size, ",");
            CHECK(printed < buffer_size);
            buffer_size -= printed;
            mangled_name += printed;
        }
    }

    printed = snprintf(mangled_name, buffer_size, ")");
    CHECK(printed < buffer_size);

cleanup:
    return err;
}
