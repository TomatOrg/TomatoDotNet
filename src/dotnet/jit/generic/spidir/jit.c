
#include "spidir/spidir.h"
#include "spidir/log.h"
#include "dotnet/jit/generic/jit_interface.h"

static void spidir_log_callback(spidir_log_level_t level, const char* module, size_t module_len, const char* message, size_t message_len) {
    switch (level) {
        case SPIDIR_LOG_LEVEL_ERROR: ERROR("%.*s: %.*s", module_len, module, message_len, message); break;
        case SPIDIR_LOG_LEVEL_WARN: WARN("%.*s: %.*s", module_len, module, message_len, message); break;
        case SPIDIR_LOG_LEVEL_INFO: TRACE("%.*s: %.*s", module_len, module, message_len, message); break;
        case SPIDIR_LOG_LEVEL_DEBUG: TRACE("%.*s: %.*s", module_len, module, message_len, message); break;
        case SPIDIR_LOG_LEVEL_TRACE: TRACE("%.*s: %.*s", module_len, module, message_len, message); break;
    }
}

static spidir_dump_status_t stdout_dump_callback(const char* s, size_t size, void* ctx) {
    (void) ctx;
    tdn_host_printf("%.*s", size, s);
    return SPIDIR_DUMP_CONTINUE;
}

void tdn_jit_dump() {
//    spidir_module_dump(m_jit_module, stdout_dump_callback, NULL);
}

jit_function_t jit_get_function_from_id(uint64_t id) {
    return (jit_function_t){ .id = id };
}

uint64_t jit_get_function_id(jit_function_t id) {
        return id.id;
}

void* jit_get_function_addr(jit_function_t func) {
    // TODO: this
    return NULL;
}

tdn_err_t jit_init() {
    spidir_log_init(spidir_log_callback);
    spidir_log_set_max_level(SPIDIR_LOG_LEVEL_INFO);
    spidir_log_set_max_level(SPIDIR_LOG_LEVEL_TRACE);
    return TDN_NO_ERROR;
}

jit_module_t jit_module_create(void) {
    return spidir_module_create();
}

void jit_module_destroy(jit_module_t module) {
    spidir_module_destroy(module);
}

void jit_link_module(jit_module_t module) {
    TRACE("TODO: implement linking in spidir");
}

jit_function_t jit_module_create_function(jit_module_t module, const char* name,
                                          jit_value_type_t ret_type, size_t param_count,
                                          const jit_value_type_t* param_types) {
    return spidir_module_create_function(module, name, ret_type, param_count, param_types);
}

jit_function_t jit_module_create_extern_function(
        jit_module_t module, const char* name,
        jit_value_type_t ret_type, size_t param_count,
        const jit_value_type_t* param_types) {
    return spidir_module_create_extern_function(module, name, ret_type, param_count, param_types);
}

void jit_module_build_function(jit_module_t module,
                               jit_function_t func,
                               jit_build_function_callback_t callback,
                               void* ctx) {
    return spidir_module_build_function(module, func, callback, ctx);
}

jit_module_t jit_builder_get_module(jit_builder_t builder) {
    return spidir_builder_get_module(builder);
}

jit_block_t jit_builder_create_block(jit_builder_t builder) {
    return spidir_builder_create_block(builder);
}

bool jit_builder_cur_block(jit_builder_t builder,
                           jit_block_t* out_block) {
    return spidir_builder_cur_block(builder, out_block);
}

void jit_builder_set_block(jit_builder_t builder,
                           jit_block_t block) {
    return spidir_builder_set_block(builder, block);
}

void jit_builder_set_entry_block(jit_builder_t builder,
                                 jit_block_t block) {
    return spidir_builder_set_entry_block(builder, block);
}

jit_value_t jit_builder_build_param_ref(jit_builder_t builder,
                                        uint32_t index) {
    return spidir_builder_build_param_ref(builder, index);
}

jit_value_t jit_builder_build_call(jit_builder_t builder,
                                   jit_function_t func,
                                   size_t arg_count,
                                   const jit_value_t* args) {
    return spidir_builder_build_call(builder, func, arg_count, args);
}

void jit_builder_build_return(jit_builder_t builder,
                              jit_value_t value) {
    return spidir_builder_build_return(builder, value);
}

void jit_builder_build_branch(jit_builder_t builder,
                              jit_block_t dest) {
    return spidir_builder_build_branch(builder, dest);
}

void jit_builder_build_brcond(jit_builder_t builder,
                              jit_value_t cond, jit_block_t true_dest,
                              jit_block_t false_dest) {
    return spidir_builder_build_brcond(builder, cond, true_dest, false_dest);
}

void jit_builder_build_unreachable(jit_builder_t builder) {
    return spidir_builder_build_unreachable(builder);
}

jit_value_t jit_builder_build_phi(jit_builder_t builder,
                                  jit_value_type_t type,
                                  size_t input_count,
                                  const jit_value_t* inputs,
                                  jit_phi_t* out_phi_handle) {
    return spidir_builder_build_phi(builder, type, input_count, inputs, out_phi_handle);
}

void jit_builder_add_phi_input(jit_builder_t builder,
                               jit_phi_t phi, jit_value_t input) {
    return spidir_builder_add_phi_input(builder, phi, input);
}

jit_value_t jit_builder_build_iconst(jit_builder_t builder,
                                     jit_value_type_t type,
                                     uint64_t value) {
    if (type == JIT_TYPE_I32) {
        value &= 0xFFFFFFFF;
    }
    return spidir_builder_build_iconst(builder, type, value);
}

jit_value_t jit_builder_build_iadd(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return spidir_builder_build_iadd(builder, lhs, rhs);
}

jit_value_t jit_builder_build_isub(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return spidir_builder_build_isub(builder, lhs, rhs);
}

jit_value_t jit_builder_build_and(jit_builder_t builder,
                                  jit_value_t lhs, jit_value_t rhs) {
    return spidir_builder_build_and(builder, lhs, rhs);
}

jit_value_t jit_builder_build_or(jit_builder_t builder,
                                 jit_value_t lhs, jit_value_t rhs) {
    return spidir_builder_build_or(builder, lhs, rhs);
}

jit_value_t jit_builder_build_xor(jit_builder_t builder,
                                  jit_value_t lhs, jit_value_t rhs) {
    return spidir_builder_build_xor(builder, lhs, rhs);
}

jit_value_t jit_builder_build_shl(jit_builder_t builder,
                                  jit_value_t lhs, jit_value_t rhs) {
    return spidir_builder_build_shl(builder, lhs, rhs);
}

jit_value_t jit_builder_build_lshr(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return spidir_builder_build_lshr(builder, lhs, rhs);
}

jit_value_t jit_builder_build_ashr(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return spidir_builder_build_ashr(builder, lhs, rhs);
}

jit_value_t jit_builder_build_imul(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return spidir_builder_build_imul(builder, lhs, rhs);
}

jit_value_t jit_builder_build_sdiv(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return spidir_builder_build_sdiv(builder, lhs, rhs);
}

jit_value_t jit_builder_build_udiv(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return spidir_builder_build_udiv(builder, lhs, rhs);
}

jit_value_t jit_builder_build_srem(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return spidir_builder_build_srem(builder, lhs, rhs);
}

jit_value_t jit_builder_build_urem(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return spidir_builder_build_urem(builder, lhs, rhs);
}

jit_value_t jit_builder_build_iext(jit_builder_t builder,
                                   jit_value_t value) {
    return spidir_builder_build_iext(builder, value);
}

jit_value_t jit_builder_build_itrunc(jit_builder_t builder,
                                     jit_value_t value) {
    return spidir_builder_build_itrunc(builder, value);
}

jit_value_t jit_builder_build_sfill(jit_builder_t builder,
                                    uint8_t width, jit_value_t value) {
    return spidir_builder_build_sfill(builder, width, value);
}

jit_value_t jit_builder_build_icmp(jit_builder_t builder,
                                   jit_icmp_kind_t icmp_kind,
                                   jit_value_type_t output_type,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return spidir_builder_build_icmp(builder, icmp_kind, output_type, lhs, rhs);
}

jit_value_t jit_builder_build_ptroff(jit_builder_t builder,
                                     jit_value_t ptr,
                                     jit_value_t off) {
    return spidir_builder_build_ptroff(builder, ptr, off);
}

jit_value_t jit_builder_build_load(jit_builder_t builder,
                                   jit_mem_size_t size,
                                   jit_value_type_t type,
                                   jit_value_t ptr) {
    return spidir_builder_build_load(builder, size, type, ptr);
}

void jit_builder_build_store(jit_builder_t builder,
                             jit_mem_size_t size, jit_value_t data,
                             jit_value_t ptr) {
    spidir_builder_build_store(builder, size, data, ptr);
}

jit_value_t jit_builder_build_stackslot(jit_builder_t builder,
                                        uint32_t size, uint32_t align) {
    return spidir_builder_build_stackslot(builder, size, align);
}
