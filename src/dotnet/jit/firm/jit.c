
#include "dotnet/jit/jit_interface.h"
#include "util/stb_ds.h"

// TODO: something better lol
extern jit_module_t m_jit_module;

ir_type* g_firm_i32 = NULL;
ir_type* g_firm_i64 = NULL;
ir_type* g_firm_ptr = NULL;

void tdn_jit_dump() {
    dump_all_ir_graphs("");
    be_main(stdout, "test");
}

static struct {
    ident* key;
    jit_function_t value;
}* m_entities;

jit_function_t jit_get_function_from_id(uint64_t id) {
    return hmget(m_entities, (ident*)id);
}

uint64_t jit_get_function_id(jit_function_t id) {
    return (uint64_t)get_entity_ident(id);
}

tdn_err_t jit_init() {
    ir_init();
    g_firm_i32 = new_type_primitive(mode_Is);
    g_firm_i64 = new_type_primitive(mode_Ls);
    g_firm_ptr = new_type_primitive(mode_P);
    return TDN_NO_ERROR;
}

jit_module_t jit_module_create(void) {
    return (jit_module_t){};
}

void jit_module_destroy(jit_module_t module) {
    ASSERT(!"jit_module_destroy");
}

static ir_node* get_as_unsigned(jit_value_t value) {
    ir_mode* mode = get_irn_mode(value);
    if (mode == mode_Is) {
        return new_Bitcast(value, mode_Iu);
    } else if (mode == mode_Ls) {
        return new_Bitcast(value, mode_Lu);
    }
    return value;
}

static ir_node* get_as_signed(jit_value_t value) {
    ir_mode* mode = get_irn_mode(value);
    if (mode == mode_Iu) {
        return new_Bitcast(value, mode_Is);
    } else if (mode == mode_Lu) {
        return new_Bitcast(value, mode_Ls);
    }
    return value;

}

jit_function_t jit_module_create_function(jit_module_t module, const char* name,
                                          jit_value_type_t ret_type, size_t param_count,
                                          const jit_value_type_t* param_types) {

    ir_type* proto_type = new_type_method(param_count,
                                          ret_type == JIT_TYPE_NONE ? 0 : 1,
                                          false, cc_fastcall_set, 0);

    for (size_t i = 0; i < param_count; i++) {
        set_method_param_type(proto_type, i, param_types[i]);
    }

    if (ret_type != JIT_TYPE_NONE) {
        set_method_res_type(proto_type, 0, ret_type);
    }

    ident* id = ir_platform_mangle_global(new_id_from_str(name));
    ir_entity* entity = new_entity(get_glob_type(), id, proto_type);
    hmput(m_entities, id, entity);

    return entity;
}

jit_function_t jit_module_create_extern_function(
        jit_module_t module, const char* name,
        jit_value_type_t ret_type, size_t param_count,
        const jit_value_type_t* param_types) {

    ir_type* proto_type = new_type_method(param_count,
                                          ret_type == JIT_TYPE_NONE ? 0 : 1,
                                          false, cc_fastcall_set, 0);

    for (size_t i = 0; i < param_count; i++) {
        set_method_param_type(proto_type, i, param_types[i]);
    }

    if (ret_type != JIT_TYPE_NONE) {
        set_method_res_type(proto_type, 0, ret_type);
    }

    ident* id = ir_platform_mangle_global(new_id_from_str(name));
    ir_entity* entity = new_entity(get_glob_type(), id, proto_type);
    hmput(m_entities, id, entity);

    return entity;
}

void jit_module_build_function(jit_module_t module,
                               jit_function_t func,
                               jit_build_function_callback_t callback,
                               void* ctx) {
    // create the graph
    ir_type* func_type = get_entity_type(func);
    int param_count = (int)get_method_n_params(func_type);
    struct jit_builder builder = {
        .graph = new_ir_graph(func, param_count)
    };
    set_current_ir_graph(builder.graph);

    //
    // Create projections for the arguments of the function
    //
    ir_node* block = get_r_cur_block(builder.graph);
    set_r_cur_block(builder.graph, get_irg_start_block(builder.graph));
    ir_node* args = get_irg_args(builder.graph);
    arrsetcap(builder.params, param_count);
    for (int i = 0; i < get_method_n_params(func_type); i++) {
        ir_type* type = get_method_param_type(func_type, i);
        arrpush(builder.params, new_Proj(args, get_type_mode(type), i));
    }
    set_r_cur_block(builder.graph, block);

    // create the projections
    callback(&builder, ctx);

    for (int i = 0; i < arrlen(builder.blocks); i++) {
        TRACE("TEST");
        mature_immBlock(builder.blocks[i]);
    }

    irg_finalize_cons(builder.graph);

    arrfree(builder.params);
    arrfree(builder.blocks);
}

jit_module_t jit_builder_get_module(jit_builder_t builder) {
    ASSERT(!"jit_builder_get_module");
}

jit_block_t jit_builder_create_block(jit_builder_t builder) {
    ir_node* block = new_immBlock();
    arrpush(builder->blocks, block);
    return block;
}

bool jit_builder_cur_block(jit_builder_t builder,
                           jit_block_t* out_block) {
    *out_block = get_r_cur_block(builder->graph);
    return true;
}

void jit_builder_set_block(jit_builder_t builder,
                           jit_block_t block) {
    set_r_cur_block(builder->graph, block);
}

void jit_builder_set_entry_block(jit_builder_t builder,
                                 jit_block_t block) {
    ir_node* jmp = new_r_Jmp(get_irg_start_block(builder->graph));
    add_immBlock_pred(block, jmp);
}

jit_value_t jit_builder_build_param_ref(jit_builder_t builder,
                                        uint32_t index) {
    return builder->params[index];
}

jit_value_t jit_builder_build_call(jit_builder_t builder,
                                   jit_function_t func,
                                   size_t arg_count,
                                   const jit_value_t* args) {
    ir_type* method = get_entity_type(func);
    bool has_ret = get_method_n_ress(method);

    ir_node* store = get_store();
    ir_node* callee = new_Address(func);
    ir_node* call_node = new_Call(store, callee, (int)arg_count, args, get_entity_type(func));
    ir_node *new_store = new_Proj(call_node, get_modeM(), pn_Call_M);
    set_store(new_store);

    if (has_ret) {
        ir_node *tuple  = new_Proj(call_node, get_modeT(), pn_Call_T_result);
        return new_Proj(tuple, get_type_mode(get_method_res_type(method, 0)), 0);
    } else {
        return NULL;
    }
}

void jit_builder_build_return(jit_builder_t builder,
                              jit_value_t value) {
    ir_node* store = get_store();
    ir_node* ret = new_Return(store, 1, &value);
    ir_node* end = get_irg_end_block(builder->graph);
    add_immBlock_pred(end, ret);
}

void jit_builder_build_branch(jit_builder_t builder,
                              jit_block_t dest) {
    ASSERT(!"jit_builder_build_branch");
}

void jit_builder_build_brcond(jit_builder_t builder,
                              jit_value_t cond, jit_block_t true_dest,
                              jit_block_t false_dest) {
    ASSERT(!"jit_builder_build_brcond");
}

void jit_builder_build_unreachable(jit_builder_t builder) {
    ASSERT(!"jit_builder_build_unreachable");
}

jit_value_t jit_builder_build_phi(jit_builder_t builder,
                                  jit_value_type_t type,
                                  size_t input_count,
                                  const jit_value_t* inputs,
                                  jit_phi_t* out_phi_handle) {
    ASSERT(!"jit_builder_build_phi");
}

void jit_builder_add_phi_input(jit_builder_t builder,
                               jit_phi_t phi, jit_value_t input) {
    ASSERT(!"jit_builder_add_phi_input");
}

jit_value_t jit_builder_build_iconst(jit_builder_t builder,
                                     jit_value_type_t type,
                                     uint64_t value) {
    return new_Const_long(get_type_mode(type), (long)value);
}

jit_value_t jit_builder_build_iadd(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return new_Add(lhs, rhs);
}

jit_value_t jit_builder_build_isub(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return new_Sub(lhs, rhs);
}

jit_value_t jit_builder_build_and(jit_builder_t builder,
                                  jit_value_t lhs, jit_value_t rhs) {
    return new_And(lhs, rhs);
}

jit_value_t jit_builder_build_or(jit_builder_t builder,
                                 jit_value_t lhs, jit_value_t rhs) {
    return new_Or(lhs, rhs);
}

jit_value_t jit_builder_build_xor(jit_builder_t builder,
                                  jit_value_t lhs, jit_value_t rhs) {
    return new_Eor(lhs, rhs);
}

jit_value_t jit_builder_build_shl(jit_builder_t builder,
                                  jit_value_t lhs, jit_value_t rhs) {
    return new_Shl(lhs, rhs);
}

jit_value_t jit_builder_build_lshr(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return new_Shrs(lhs, rhs);
}

jit_value_t jit_builder_build_ashr(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return new_Shr(lhs, rhs);
}

jit_value_t jit_builder_build_imul(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return new_Mul(lhs, rhs);
}

jit_value_t jit_builder_build_sdiv(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    ASSERT(!"jit_builder_build_sdiv");
}

jit_value_t jit_builder_build_udiv(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    ASSERT(!"jit_builder_build_udiv");
}

jit_value_t jit_builder_build_srem(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    ASSERT(!"jit_builder_build_srem");
}

jit_value_t jit_builder_build_urem(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    ASSERT(!"jit_builder_build_urem");
}

jit_value_t jit_builder_build_iext(jit_builder_t builder,
                                   jit_value_t value) {
    return get_as_signed(new_Conv(value, mode_Lu));
}

jit_value_t jit_builder_build_itrunc(jit_builder_t builder,
                                     jit_value_t value) {
    return new_Conv(value, mode_Is);
}

jit_value_t jit_builder_build_sfill(jit_builder_t builder,
                                    uint8_t width, jit_value_t value) {
    switch (width) {
        case 8: return new_Conv(value, mode_Bs);
        case 16: return new_Conv(value, mode_Hs);
        case 32: return new_Conv(value, mode_Is);
        default: ASSERT(!"Invalid sfill width");
    }
}

jit_value_t jit_builder_build_icmp(jit_builder_t builder,
                                   jit_icmp_kind_t icmp_kind,
                                   jit_value_type_t output_type,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    ir_relation rel;
    switch (icmp_kind) {
        case JIT_ICMP_EQ:
        case JIT_ICMP_NE: {
            rel = ir_relation_equal;
        } break;

        case JIT_ICMP_SLT: {
            rel = ir_relation_less;
            lhs = get_as_signed(lhs);
            rhs = get_as_signed(rhs);
        } break;

        case JIT_ICMP_SLE: {
            rel = ir_relation_less_equal;
            lhs = get_as_signed(lhs);
            rhs = get_as_signed(rhs);
        } break;

        case JIT_ICMP_ULT: {
            rel = ir_relation_less;
            lhs = get_as_unsigned(lhs);
            rhs = get_as_unsigned(rhs);
        } break;

        case JIT_ICMP_ULE: {
            rel = ir_relation_less_equal;
            lhs = get_as_unsigned(lhs);
            rhs = get_as_unsigned(rhs);
        } break;
    }

    ir_node* cmp = new_Cmp(lhs, rhs, rel);

    if (icmp_kind == JIT_ICMP_NE) {
        cmp = new_Not(cmp);
    }

    return cmp;
}

jit_value_t jit_builder_build_ptroff(jit_builder_t builder,
                                     jit_value_t ptr,
                                     jit_value_t off) {
    ASSERT(!"jit_builder_build_ptroff");
}

jit_value_t jit_builder_build_load(jit_builder_t builder,
                                   jit_mem_size_t size,
                                   jit_value_type_t type,
                                   jit_value_t ptr) {
    ASSERT(!"jit_builder_build_load");
}

void jit_builder_build_store(jit_builder_t builder,
                             jit_mem_size_t size, jit_value_t data,
                             jit_value_t ptr) {
    ASSERT(!"jit_builder_build_store");
}

jit_value_t jit_builder_build_stackslot(jit_builder_t builder,
                                        uint32_t size, uint32_t align) {
    ASSERT(!"jit_builder_build_stackslot");
}
