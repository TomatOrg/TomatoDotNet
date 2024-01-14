#include <stdarg.h>
#include "dotnet/jit/jit_interface.h"
#include "util/string.h"
#include "util/string_builder.h"
#include "util/stb_ds.h"

// TODO: something better lol
extern jit_module_t m_jit_module;

static MIR_context_t m_mir_ctx = NULL;

static jit_function_inner_t** m_functions;

void tdn_jit_dump() {
    MIR_output(m_mir_ctx, stdout);
}

static void MIR_NO_RETURN error_func(MIR_error_type_t error_type, const char *format, ...) {
    va_list args;
    va_start(args, format);
    tdn_host_vprintf(format, args);
    va_end(args);
    __builtin_trap();
}

tdn_err_t jit_init() {
    m_mir_ctx = MIR_init();
    MIR_set_error_func(m_mir_ctx, error_func);
    return TDN_NO_ERROR;
}

jit_function_t jit_get_function_from_id(uint64_t id) {
    return m_functions[id];
}

uint64_t jit_get_function_id(jit_function_t func) {
    return func->index;
}

jit_module_t jit_module_create(void) {
    return MIR_new_module(m_mir_ctx, "lol");
}

void jit_module_destroy(jit_module_t module) {
    ASSERT(!"jit_module_destroy");
}

jit_function_t jit_module_create_function(jit_module_t module, const char* name,
                                          jit_value_type_t ret_type, size_t param_count,
                                          const jit_value_type_t* param_types) {
    MIR_var_t vars[param_count];
    string_builder_t vars_names[param_count];
    memset(vars_names, 0, sizeof(vars_names));
    memset(vars, 0, sizeof(vars));
    for (size_t i = 0; i < param_count; i++) {
        string_builder_push_cstr(&vars_names[i], "arg");
        string_builder_push_uint(&vars_names[i], i);
        vars[i].type = param_types[i];
        vars[i].name = string_builder_build(&vars_names[i]);
    }
    string_builder_t builder = {0};
    string_builder_push_cstr(&builder, name);
    string_builder_push_cstr(&builder, "$proto");
    MIR_item_t proto = MIR_new_proto_arr(m_mir_ctx, string_builder_build(&builder),
                                         ret_type == JIT_TYPE_NONE ? 0 : 1, &ret_type,
                                         param_count, vars);

    MIR_item_t func = MIR_new_func_arr(m_mir_ctx, name,
                                       ret_type == JIT_TYPE_NONE ? 0 : 1, &ret_type,
                                       param_count, vars);
    MIR_finish_func(m_mir_ctx);

    for (size_t i = 0; i < param_count; i++) {
        string_builder_free(&vars_names[i]);
    }
    string_builder_free(&builder);

    jit_function_t function = malloc(sizeof(jit_function_inner_t));
    function->index = arrlen(m_functions);
    function->func = func;
    function->proto = proto;
    function->has_return = ret_type != JIT_TYPE_NONE;
    function->is_32bit_return = ret_type == JIT_TYPE_I32;
    arrpush(m_functions, function);
    return function;
}

jit_function_t jit_module_create_extern_function(
        jit_module_t module, const char* name,
        jit_value_type_t ret_type, size_t param_count,
        const jit_value_type_t* param_types) {
    MIR_var_t vars[param_count];
    string_builder_t vars_names[param_count];
    memset(vars_names, 0, sizeof(vars_names));
    memset(vars, 0, sizeof(vars));
    for (size_t i = 0; i < param_count; i++) {
        string_builder_push_cstr(&vars_names[i], "arg");
        string_builder_push_uint(&vars_names[i], i);
        vars[i].type = param_types[i];
        vars[i].name = string_builder_build(&vars_names[i]);
    }
    string_builder_t builder = {0};
    string_builder_push_cstr(&builder, name);
    string_builder_push_cstr(&builder, "$proto");
    MIR_item_t proto = MIR_new_proto_arr(m_mir_ctx, string_builder_build(&builder),
                                         ret_type == JIT_TYPE_NONE ? 0 : 1, &ret_type,
                                         param_count, vars);

    MIR_item_t func = MIR_new_import(m_mir_ctx, name);

    for (size_t i = 0; i < param_count; i++) {
        string_builder_free(&vars_names[i]);
    }
    string_builder_free(&builder);

    jit_function_t function = malloc(sizeof(jit_function_inner_t));
    function->index = arrlen(m_functions);
    function->func = func;
    function->proto = proto;
    function->has_return = ret_type != JIT_TYPE_NONE;
    function->is_32bit_return = ret_type == JIT_TYPE_I32;
    arrpush(m_functions, function);
    return function;
}

void jit_module_build_function(jit_module_t module,
                               jit_function_t func,
                               jit_build_function_callback_t callback,
                               void* ctx) {
    struct jit_builder builder = {
        .func = func->func,
    };

    // generate everything
    callback(&builder, ctx);

    // and now we can prepend the actual jump into the first block
    MIR_prepend_insn(m_mir_ctx, func->func,
                     MIR_new_insn(m_mir_ctx, MIR_JMP,
                                  MIR_new_label_op(m_mir_ctx, builder.blocks[builder.entry_block])));

    arrfree(builder.blocks);
    arrfree(builder.blocks_cursors);
}

jit_module_t jit_builder_get_module(jit_builder_t builder) {
    return m_jit_module;
}

jit_block_t jit_builder_create_block(jit_builder_t builder) {
    // create the label and the cursor
    uint64_t id = arrlen(builder->blocks);
    MIR_label_t label = MIR_new_label(m_mir_ctx);
    arrpush(builder->blocks, label);
    arrpush(builder->blocks_cursors, label);

    // append it so it will be inside the instruction list
    MIR_append_insn(m_mir_ctx, builder->func, label);

    // return the id
    return id;
}

bool jit_builder_cur_block(jit_builder_t builder,
                           jit_block_t* out_block) {
    if (!builder->has_block) {
        return false;
    }
    *out_block = builder->current_block;
    return true;
}

void jit_builder_set_block(jit_builder_t builder,
                           jit_block_t block) {
    // just set the id of the block we are now modifying
    builder->current_block = block;
    builder->has_block = true;
}

void jit_builder_set_entry_block(jit_builder_t builder,
                                 jit_block_t block) {
    // just remember it, we will jump to it at the start later
    builder->entry_block = block;
}

static char int2hex(int a) {
    if (a <= 9) {
        return a + '0';
    } else {
        return a + 'a';
    }
}

static void append_instruction(jit_builder_t builder, MIR_insn_t insn) {
    // just print it nicely
    tdn_host_printf("[*] ");
    MIR_output_insn(m_mir_ctx, stdout, insn, builder->func->u.func, 1);

    // insert after the cursor and increment the cursor to be on the new
    // instruction
    MIR_insert_insn_after(m_mir_ctx, builder->func, builder->blocks_cursors[builder->current_block], insn);
    builder->blocks_cursors[builder->current_block] = insn;
}

static jit_value_t create_mir_reg(jit_builder_t builder, bool is_32bit) {
    char name[10] = "v";
    uint32_t current = builder->value_index;
    for (int i = 0; i < 8; i++) {
        name[1 + i] = int2hex(current & 0xF);
        current >>= 4;

        if (current == 0) {
            name[1 + i + 1] = '\0';
            break;
        }
    }
    builder->value_index++;
    MIR_op_t op = MIR_new_reg_op(m_mir_ctx,
                          MIR_new_func_reg(m_mir_ctx, builder->func->u.func, MIR_T_I64, name));
    return (jit_value_t){ .op = op, .is_32bit = is_32bit };
}

jit_value_t jit_builder_build_param_ref(jit_builder_t builder,
                                        uint32_t index) {
    ASSERT(index <= 9);
    char name[] = "arg_";
    name[3] = index + '0';
    MIR_reg_t reg = MIR_reg(m_mir_ctx, name, builder->func->u.func);
    MIR_op_t op = MIR_new_reg_op(m_mir_ctx, reg);
    MIR_type_t type = MIR_reg_type(m_mir_ctx, reg, builder->func->u.func);
    return (jit_value_t){ .op = op, .is_32bit = (type != MIR_T_I64 && type != MIR_T_U64 && type != MIR_T_P) };
}

jit_value_t jit_builder_build_call(jit_builder_t builder,
                                   jit_function_t func,
                                   size_t arg_count,
                                   const jit_value_t* args) {
    jit_value_t result = JIT_VALUE_INVALID;

    int ret_args = func->has_return ? 1 : 0;
    MIR_op_t mir_args[arg_count + 2 + ret_args];
    mir_args[0] = MIR_new_ref_op(m_mir_ctx, func->proto);
    mir_args[1] = MIR_new_ref_op(m_mir_ctx, func->func);

    if (func->has_return) {
        result = create_mir_reg(builder, func->is_32bit_return);
        mir_args[2] = result.op;
    }

    for (int i = 0; i < arg_count; i++) {
        mir_args[2 + ret_args + i] = args[i].op;
    }

    append_instruction(builder,
                       MIR_new_insn_arr(m_mir_ctx, MIR_CALL, arg_count + ret_args + 2, mir_args));

    return result;
}

void jit_builder_build_return(jit_builder_t builder,
                              jit_value_t value) {
    append_instruction(builder,
                       MIR_new_ret_insn(m_mir_ctx, builder->has_return ? 0 : 1, value));
}

void jit_builder_build_branch(jit_builder_t builder,
                              jit_block_t dest) {
    append_instruction(builder, MIR_new_insn(m_mir_ctx, MIR_JMP,
                                             MIR_new_label_op(m_mir_ctx, builder->blocks[dest])));
}

void jit_builder_build_brcond(jit_builder_t builder,
                              jit_value_t cond, jit_block_t true_dest,
                              jit_block_t false_dest) {
    append_instruction(builder, MIR_new_insn(m_mir_ctx, cond.is_32bit ? MIR_BTS : MIR_BT,
                                             cond.op,
                                             MIR_new_label_op(m_mir_ctx, builder->blocks[true_dest])));

    append_instruction(builder, MIR_new_insn(m_mir_ctx, MIR_JMP,
                                             MIR_new_label_op(m_mir_ctx, builder->blocks[false_dest])));
}

void jit_builder_build_unreachable(jit_builder_t builder) {
}

jit_value_t jit_builder_build_phi(jit_builder_t builder,
                                  jit_value_type_t type,
                                  size_t input_count,
                                  const jit_value_t* inputs,
                                  jit_phi_t* out_phi_handle) {
    ASSERT(input_count == 0);
    jit_value_t result = create_mir_reg(builder, type == JIT_TYPE_I32);
    *out_phi_handle = result.op;
    return result;
}

void jit_builder_add_phi_input(jit_builder_t builder,
                               jit_phi_t phi, jit_value_t input) {
    append_instruction(builder, MIR_new_insn(m_mir_ctx, MIR_MOV, phi, input.op));
}

jit_value_t jit_builder_build_iconst(jit_builder_t builder,
                                     jit_value_type_t type,
                                     uint64_t value) {
    return (jit_value_t) { .op = MIR_new_int_op(m_mir_ctx, value), .is_32bit = type == JIT_TYPE_I32 };
}

jit_value_t jit_builder_build_iadd(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_ADDS : MIR_ADD, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_isub(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_SUBS : MIR_SUB, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_and(jit_builder_t builder,
                                  jit_value_t lhs, jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_ANDS : MIR_AND, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_or(jit_builder_t builder,
                                 jit_value_t lhs, jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_ORS : MIR_OR, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_xor(jit_builder_t builder,
                                  jit_value_t lhs, jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_XORS : MIR_XOR, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_shl(jit_builder_t builder,
                                  jit_value_t lhs, jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_LSHS : MIR_LSH, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_lshr(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_URSHS : MIR_URSH, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_ashr(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_RSHS : MIR_RSH, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_imul(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_MULS : MIR_MUL, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_sdiv(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_DIVS : MIR_DIV, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_udiv(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_UDIVS : MIR_UDIV, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_srem(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_MODS : MIR_MOD, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_urem(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    jit_value_t result = create_mir_reg(builder, lhs.is_32bit);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, lhs.is_32bit ? MIR_UMODS : MIR_UMOD, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_iext(jit_builder_t builder,
                                   jit_value_t value) {
    jit_value_t result = create_mir_reg(builder, false);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, MIR_UEXT32, result.op, value.op));
    return result;
}

jit_value_t jit_builder_build_itrunc(jit_builder_t builder,
                                     jit_value_t value) {
    jit_value_t result = create_mir_reg(builder, true);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, MIR_MOV, result.op, value.op));
    return result;
}

jit_value_t jit_builder_build_sfill(jit_builder_t builder,
                                    uint8_t width, jit_value_t value) {
    MIR_insn_code_t code;
    switch (width) {
        case 1: code = MIR_EXT8; break;
        case 2: code = MIR_EXT16; break;
        case 4: code = MIR_EXT32; break;
        default: ASSERT(!"Invalid sfill length");
    }
    jit_value_t result = create_mir_reg(builder, false);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, code, result.op, value.op));
    return result;
}

jit_value_t jit_builder_build_icmp(jit_builder_t builder,
                                   jit_icmp_kind_t icmp_kind,
                                   jit_value_type_t output_type,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    MIR_insn_code_t code;
    switch (icmp_kind) {
        case JIT_ICMP_EQ: code = lhs.is_32bit ? MIR_EQS : MIR_EQ; break;
        case JIT_ICMP_NE: code = lhs.is_32bit ? MIR_NES : MIR_NE; break;
        case JIT_ICMP_SLT: code = lhs.is_32bit ? MIR_LTS : MIR_LT; break;
        case JIT_ICMP_SLE: code = lhs.is_32bit ? MIR_LES : MIR_LE; break;
        case JIT_ICMP_ULT: code = lhs.is_32bit ? MIR_ULTS : MIR_ULT; break;
        case JIT_ICMP_ULE: code = lhs.is_32bit ? MIR_ULES : MIR_ULE; break;
        default: ASSERT(!"Invalid compare type");
    }
    jit_value_t result = create_mir_reg(builder, output_type == JIT_TYPE_I32);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, code, result.op, lhs.op, rhs.op));
    return result;
}

jit_value_t jit_builder_build_ptroff(jit_builder_t builder,
                                     jit_value_t ptr,
                                     jit_value_t off) {
    jit_value_t result = create_mir_reg(builder, false);
    append_instruction(builder, MIR_new_insn(m_mir_ctx, MIR_ADD, result.op, ptr.op, off.op));
    return result;
}

jit_value_t jit_builder_build_load(jit_builder_t builder,
                                   jit_mem_size_t size,
                                   jit_value_type_t type,
                                   jit_value_t ptr) {
    MIR_type_t mir_load_type;
    switch (size) {
        case JIT_MEM_SIZE_1: mir_load_type = MIR_T_U8; break;
        case JIT_MEM_SIZE_2: mir_load_type = MIR_T_U16; break;
        case JIT_MEM_SIZE_4: mir_load_type = MIR_T_U32; break;
        case JIT_MEM_SIZE_8: mir_load_type = MIR_T_U64; break;
    }
    jit_value_t result = create_mir_reg(builder, false);

    MIR_disp_t base = 0;
    MIR_reg_t reg = 0;
    switch (ptr.op.mode) {
        case MIR_OP_REG: reg = ptr.op.u.reg; break;
        case MIR_OP_INT: base = ptr.op.u.i; break;
        default: ASSERT(!"Unknown operand type");
    }
    append_instruction(builder, MIR_new_insn(m_mir_ctx, MIR_MOV, result.op,
                                             MIR_new_mem_op(m_mir_ctx, mir_load_type,
                                                            base, reg, 0, 1)));
    return result;
}

void jit_builder_build_store(jit_builder_t builder,
                             jit_mem_size_t size, jit_value_t data,
                             jit_value_t ptr) {
    MIR_type_t mir_load_type;
    switch (size) {
        case JIT_MEM_SIZE_1: mir_load_type = MIR_T_U8; break;
        case JIT_MEM_SIZE_2: mir_load_type = MIR_T_U16; break;
        case JIT_MEM_SIZE_4: mir_load_type = MIR_T_U32; break;
        case JIT_MEM_SIZE_8: mir_load_type = MIR_T_U64; break;
    }
    jit_value_t result = create_mir_reg(builder, false);

    MIR_disp_t base = 0;
    MIR_reg_t reg = 0;
    switch (ptr.op.mode) {
        case MIR_OP_REG: reg = ptr.op.u.reg; break;
        case MIR_OP_INT: base = ptr.op.u.i; break;
        default: ASSERT(!"Unknown operand type");
    }
    append_instruction(builder, MIR_new_insn(m_mir_ctx, MIR_MOV,
                                             MIR_new_mem_op(m_mir_ctx, mir_load_type,
                                                            base, reg, 0, 1), data.op));
}

jit_value_t jit_builder_build_stackslot(jit_builder_t builder,
                                        uint32_t size, uint32_t align) {
    jit_value_t result = create_mir_reg(builder, false);
    MIR_prepend_insn(m_mir_ctx, builder->func,
                     MIR_new_insn(m_mir_ctx, MIR_ALLOCA, result.op,
                                             MIR_new_int_op(m_mir_ctx, size)));
    return result;
}
