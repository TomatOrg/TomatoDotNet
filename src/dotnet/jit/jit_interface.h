#pragma once

#include <util/except.h>

#ifdef __JIT_SPIDIR__

#include "spidir/spidir.h"

typedef spidir_function_t jit_function_t;

typedef spidir_value_t jit_value_t;
#define JIT_VALUE_INVALID SPIDIR_VALUE_INVALID

typedef spidir_block_t jit_block_t;
#define JIT_IS_SAME_BLOCK(a, b) ((a).id == (b).id)

typedef spidir_builder_handle_t jit_builder_t;

typedef spidir_module_handle_t jit_module_t;

typedef spidir_phi_t jit_phi_t;

typedef spidir_value_type_t jit_value_type_t;
#define JIT_TYPE_NONE       SPIDIR_TYPE_NONE
#define JIT_TYPE_I32        SPIDIR_TYPE_I32
#define JIT_TYPE_I64        SPIDIR_TYPE_I64
#define JIT_TYPE_PTR        SPIDIR_TYPE_PTR

typedef spidir_mem_size_t jit_mem_size_t;
#define JIT_MEM_SIZE_1      SPIDIR_MEM_SIZE_1
#define JIT_MEM_SIZE_2      SPIDIR_MEM_SIZE_2
#define JIT_MEM_SIZE_4      SPIDIR_MEM_SIZE_4
#define JIT_MEM_SIZE_8      SPIDIR_MEM_SIZE_8

typedef spidir_icmp_kind_t jit_icmp_kind_t;
#define JIT_ICMP_EQ         SPIDIR_ICMP_EQ
#define JIT_ICMP_NE         SPIDIR_ICMP_NE
#define JIT_ICMP_SLT        SPIDIR_ICMP_SLT
#define JIT_ICMP_SLE        SPIDIR_ICMP_SLE
#define JIT_ICMP_ULT        SPIDIR_ICMP_ULT
#define JIT_ICMP_ULE        SPIDIR_ICMP_ULE



#else
    #error Unknown jit interface
#endif

typedef void (*jit_build_function_callback_t)(jit_builder_t builder, void* ctx);


/**
 * Choose and initialize the wanted jit interface
 */
tdn_err_t jit_init();

jit_module_t jit_module_create(void);

void jit_module_destroy(jit_module_t module);

jit_function_t jit_module_create_function(jit_module_t module, const char* name,
                              jit_value_type_t ret_type, size_t param_count,
                              const jit_value_type_t* param_types);

jit_function_t jit_module_create_extern_function(
        jit_module_t module, const char* name,
        jit_value_type_t ret_type, size_t param_count,
        const jit_value_type_t* param_types);

void jit_module_build_function(jit_module_t module,
                                  jit_function_t func,
                                  jit_build_function_callback_t callback,
                                  void* ctx);

jit_module_t jit_builder_get_module(jit_builder_t builder);

jit_block_t jit_builder_create_block(jit_builder_t builder);

bool jit_builder_cur_block(jit_builder_t builder,
                              jit_block_t* out_block);

void jit_builder_set_block(jit_builder_t builder,
                              jit_block_t block);

void jit_builder_set_entry_block(jit_builder_t builder,
                                    jit_block_t block);

jit_value_t jit_builder_build_param_ref(jit_builder_t builder,
                                              uint32_t index);

jit_value_t jit_builder_build_call(jit_builder_t builder,
                                         jit_function_t func,
                                         size_t arg_count,
                                         const jit_value_t* args);

void jit_builder_build_return(jit_builder_t builder,
                                 jit_value_t value);

void jit_builder_build_branch(jit_builder_t builder,
                                 jit_block_t dest);

void jit_builder_build_brcond(jit_builder_t builder,
                                 jit_value_t cond, jit_block_t true_dest,
                                 jit_block_t false_dest);

void jit_builder_build_unreachable(jit_builder_t builder);

jit_value_t jit_builder_build_phi(jit_builder_t builder,
                                        jit_value_type_t type,
                                        size_t input_count,
                                        const jit_value_t* inputs,
                                        jit_phi_t* out_phi_handle);

void jit_builder_add_phi_input(jit_builder_t builder,
                                  jit_phi_t phi, jit_value_t input);

jit_value_t jit_builder_build_iconst(jit_builder_t builder,
                                           jit_value_type_t type,
                                           uint64_t value);

jit_value_t jit_builder_build_iadd(jit_builder_t builder,
                                         jit_value_t lhs,
                                         jit_value_t rhs);

jit_value_t jit_builder_build_isub(jit_builder_t builder,
                                         jit_value_t lhs,
                                         jit_value_t rhs);

jit_value_t jit_builder_build_and(jit_builder_t builder,
                                        jit_value_t lhs, jit_value_t rhs);

jit_value_t jit_builder_build_or(jit_builder_t builder,
                                       jit_value_t lhs, jit_value_t rhs);

jit_value_t jit_builder_build_xor(jit_builder_t builder,
                                        jit_value_t lhs, jit_value_t rhs);

jit_value_t jit_builder_build_shl(jit_builder_t builder,
                                        jit_value_t lhs, jit_value_t rhs);

jit_value_t jit_builder_build_lshr(jit_builder_t builder,
                                         jit_value_t lhs,
                                         jit_value_t rhs);

jit_value_t jit_builder_build_ashr(jit_builder_t builder,
                                         jit_value_t lhs,
                                         jit_value_t rhs);

jit_value_t jit_builder_build_imul(jit_builder_t builder,
                                         jit_value_t lhs,
                                         jit_value_t rhs);

jit_value_t jit_builder_build_sdiv(jit_builder_t builder,
                                         jit_value_t lhs,
                                         jit_value_t rhs);

jit_value_t jit_builder_build_udiv(jit_builder_t builder,
                                         jit_value_t lhs,
                                         jit_value_t rhs);

jit_value_t jit_builder_build_srem(jit_builder_t builder,
                                         jit_value_t lhs,
                                         jit_value_t rhs);

jit_value_t jit_builder_build_urem(jit_builder_t builder,
                                         jit_value_t lhs,
                                         jit_value_t rhs);

jit_value_t jit_builder_build_iext(jit_builder_t builder,
                                         jit_value_t value);

jit_value_t jit_builder_build_itrunc(jit_builder_t builder,
                                           jit_value_t value);

jit_value_t jit_builder_build_sfill(jit_builder_t builder,
                                          uint8_t width, jit_value_t value);

jit_value_t jit_builder_build_icmp(jit_builder_t builder,
                                         jit_icmp_kind_t icmp_kind,
                                         jit_value_type_t output_type,
                                         jit_value_t lhs,
                                         jit_value_t rhs);

jit_value_t jit_builder_build_ptroff(jit_builder_t builder,
                                           jit_value_t ptr,
                                           jit_value_t off);

jit_value_t jit_builder_build_load(jit_builder_t builder,
                                         jit_mem_size_t size,
                                         jit_value_type_t type,
                                         jit_value_t ptr);

void jit_builder_build_store(jit_builder_t builder,
                                jit_mem_size_t size, jit_value_t data,
                                jit_value_t ptr);

jit_value_t jit_builder_build_stackslot(jit_builder_t builder,
                                              uint32_t size, uint32_t align);
