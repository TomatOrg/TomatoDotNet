#include <stdarg.h>
#include <time.h>
#include "dotnet/jit/jit_interface.h"
#include "util/string.h"
#include "util/string_builder.h"
#include "util/stb_ds.h"
#include "dotnet/gc/gc.h"

#include <llvm-c/ErrorHandling.h>
#include <llvm-c/LLJIT.h>

static jit_function_inner_t** m_functions = NULL;

LLVMTypeRef m_jit_type_i32 = NULL;
LLVMTypeRef m_jit_type_i64 = NULL;
LLVMTypeRef m_jit_type_ptr = NULL;

static void llvm_fatal_error_handler(const char *Reason) {
    ERROR("%s", Reason);
    ASSERT(0);
}

static LLVMOrcLLJITRef m_lljit = NULL;

static LLVMOrcThreadSafeContextRef m_orc_context;
static LLVMContextRef m_context;

tdn_err_t jit_init() {
    tdn_err_t err = TDN_NO_ERROR;
    LLVMErrorRef error = NULL;

    LLVMInstallFatalErrorHandler(llvm_fatal_error_handler);

    // Initialize native target codegen and asm printer.
    LLVMInitializeNativeTarget();
    LLVMInitializeNativeAsmPrinter();

    m_orc_context = LLVMOrcCreateNewThreadSafeContext();
    m_context = LLVMOrcThreadSafeContextGetContext(m_orc_context);

    m_jit_type_i32 = LLVMInt32TypeInContext(m_context);
    m_jit_type_i64 = LLVMInt32TypeInContext(m_context);
    m_jit_type_ptr = LLVMPointerTypeInContext(m_context, 0);

    error = LLVMOrcCreateLLJIT(&m_lljit, NULL);
    CHECK(error == NULL);

cleanup:
    if (IS_ERROR(err)) {
        if (error != NULL) {
            char* error_message = LLVMGetErrorMessage(error);
            ERROR("LLVM Error: %s", error_message);
            LLVMDisposeErrorMessage(error_message);
            LLVMConsumeError(error);
        }
    }

    return err;
}

jit_function_t jit_get_function_from_id(uint64_t id) {
    if (id >= arrlen(m_functions)) {
        return NULL;
    } else {
        return m_functions[id];
    }
}

uint64_t jit_get_function_id(jit_function_t func) {
    return func->index;
}

void* jit_get_function_addr(jit_function_t func) {
    LLVMOrcJITTargetAddress address;
    LLVMOrcLLJITLookup(m_lljit, &address, func->name);
    return (void*)address;
}

jit_module_t jit_module_create(void) {
    return LLVMModuleCreateWithName("whatever");
}

void jit_link_module(jit_module_t module) {
    LLVMOrcJITDylibRef mainjd = LLVMOrcLLJITGetMainJITDylib(m_lljit);
    LLVMErrorRef error = LLVMOrcLLJITAddLLVMIRModule(m_lljit, mainjd,
                                                     LLVMOrcCreateNewThreadSafeModule(module, m_orc_context));

    if (IS_ERROR(error)) {
        ERROR("LLVM Error: %s", LLVMGetErrorMessage(error));
        ASSERT(0);
    }

}

void jit_module_destroy(jit_module_t module) {
    LLVMDisposeModule(module);
}

jit_function_t jit_module_create_function(
    jit_module_t module, const char* name,
    jit_value_type_t ret_type, size_t param_count,
    const jit_value_type_t* param_types
) {
    if (ret_type == NULL) {
        ret_type = LLVMVoidTypeInContext(m_context);
    }

    LLVMTypeRef type = LLVMFunctionType(ret_type, (LLVMTypeRef*)param_types, param_count, 0);
    LLVMValueRef value = LLVMAddFunction(module, name, type);

    // create the entry already
    LLVMAppendBasicBlockInContext(m_context, value, "entry");

    jit_function_t function = malloc(sizeof(jit_function_inner_t));
    bzero(function, sizeof(*function));
    function->index = arrlen(m_functions);
    function->type = type;
    function->value = value;
    function->name = strdup(name);
    arrpush(m_functions, function);
    return function;
}

jit_function_t jit_module_create_extern_function(
    jit_module_t module, const char* name,
    jit_value_type_t ret_type, size_t param_count,
    const jit_value_type_t* param_types
) {
    if (ret_type == NULL) {
        ret_type = LLVMVoidTypeInContext(m_context);
    }

    LLVMTypeRef type = LLVMFunctionType(ret_type, (LLVMTypeRef*)param_types, param_count, 0);
    LLVMValueRef value = LLVMAddFunction(module, name, type);

    jit_function_t function = malloc(sizeof(jit_function_inner_t));
    bzero(function, sizeof(*function));
    function->index = arrlen(m_functions);
    function->type = type;
    function->value = value;
    function->name = strdup(name);
    arrpush(m_functions, function);
    return function;
}

void jit_module_build_function(jit_module_t module,
                               jit_function_t func,
                               jit_build_function_callback_t callback,
                               void* ctx) {
    struct jit_builder builder = {
        .function = func,
        .builder = LLVMCreateBuilderInContext(m_context),
        .has_block = false,
        .current_block = -1,
        .module = module
    };

    // generate everything
    callback(&builder, ctx);

    // build all the phis
    for (int block_index = 0; block_index < arrlen(builder.blocks); block_index++) {
        jit_block_instance_t* block = &builder.blocks[block_index];
        int pred_count = arrlen(block->preds);

        for (int phi_index = 0; phi_index < arrlen(block->phis); phi_index++) {
            jit_phi_instance_t* phi = &block->phis[phi_index];
            ASSERT(arrlen(phi->inputs) == pred_count);

            LLVMAddIncoming(phi->phi, phi->inputs, block->preds, pred_count);
        }
    }

    // free all the blocks
    for (int i = 0; i < arrlen(builder.blocks); i++) {
        arrfree(builder.blocks[i].preds);
        arrfree(builder.blocks[i].phis);
    }
    arrfree(builder.blocks);

    // no need for this anymore
    LLVMDisposeBuilder(builder.builder);
}

jit_module_t jit_builder_get_module(jit_builder_t builder) {
    return builder->module;
}

jit_block_t jit_builder_create_block(jit_builder_t builder) {
    LLVMBasicBlockRef new_block = LLVMAppendBasicBlockInContext(m_context, builder->function->value, "");

    jit_block_t block = arraddnindex(builder->blocks, 1);
    builder->blocks[block].entry = new_block;
    builder->blocks[block].preds = NULL;
    builder->blocks[block].phis = NULL;

    return block;
}

bool jit_builder_cur_block(jit_builder_t builder, jit_block_t* out_block) {
    *out_block = builder->current_block;
    return builder->has_block;
}

void jit_builder_set_block(jit_builder_t builder, jit_block_t block) {
    LLVMPositionBuilderAtEnd(builder->builder, builder->blocks[block].entry);
    builder->has_block = true;
    builder->current_block = block;
}

void jit_builder_set_entry_block(jit_builder_t builder, jit_block_t block) {
    // emulate by jumping to the block we want as the entry
    LLVMBasicBlockRef entry = LLVMGetEntryBasicBlock(builder->function->value);
    LLVMPositionBuilderAtEnd(builder->builder, entry);
    LLVMBuildBr(builder->builder, builder->blocks[block].entry);

    // return to where we were
    if (builder->has_block) {
        LLVMPositionBuilderAtEnd(builder->builder, builder->blocks[builder->current_block].entry);
    }
}

jit_value_t jit_builder_build_param_ref(jit_builder_t builder,
                                        uint32_t index) {
    return LLVMGetParam(builder->function->value, index);
}

jit_value_t jit_builder_build_call(jit_builder_t builder,
                                   jit_function_t func,
                                   size_t arg_count,
                                   const jit_value_t* args) {
    return LLVMBuildCall2(builder->builder, func->type, func->value, (jit_value_t*)args, arg_count, "");
}

void jit_builder_build_return(jit_builder_t builder,
                              jit_value_t value) {
    if (value != NULL) {
        LLVMBuildRet(builder->builder, value);
    } else {
        LLVMBuildRetVoid(builder->builder);
    }
}

void jit_builder_build_branch(jit_builder_t builder,
                              jit_block_t dest) {
    LLVMBuildBr(builder->builder, builder->blocks[dest].entry);
    arrpush(builder->blocks[dest].preds, builder->blocks[builder->current_block].entry);
}

void jit_builder_build_brcond(jit_builder_t builder,
                              jit_value_t cond, jit_block_t true_dest,
                              jit_block_t false_dest) {
    LLVMBasicBlockRef true_block = builder->blocks[true_dest].entry;
    LLVMBasicBlockRef false_block = builder->blocks[false_dest].entry;

    cond = LLVMBuildTrunc(builder->builder, cond, LLVMInt1TypeInContext(m_context), "");
    LLVMBuildCondBr(builder->builder, cond, true_block, false_block);

    arrpush(builder->blocks[true_dest].preds, builder->blocks[builder->current_block].entry);
    arrpush(builder->blocks[false_dest].preds, builder->blocks[builder->current_block].entry);
}

void jit_builder_build_unreachable(jit_builder_t builder) {
    LLVMBuildUnreachable(builder->builder);
}

jit_value_t jit_builder_build_phi(jit_builder_t builder,
                                  jit_value_type_t type,
                                  size_t input_count,
                                  const jit_value_t* inputs,
                                  jit_phi_t* out_phi_handle) {
    // make sure there are as many inputs as preds
    jit_block_instance_t* instance = &builder->blocks[builder->current_block];

    // add a new phi
    int phi_index = arraddnindex(instance->phis, 1);
    jit_phi_instance_t* phi = &instance->phis[phi_index];
    phi->inputs = NULL;

    // output the handle correctly
    out_phi_handle->block = builder->current_block;
    out_phi_handle->phi = phi_index;

    // setup the phi location
    jit_value_t result = LLVMBuildPhi(builder->builder, type, "");
    phi->phi = result;

    // add the current inputs
    for (int i = 0; i < input_count; i++) {
        arrpush(phi->inputs, inputs[i]);
    }

    return result;
}

void jit_builder_add_phi_input(jit_builder_t builder,
                               jit_phi_t phi, jit_value_t input) {
    // just add the phi into the list
    arrpush(builder->blocks[phi.block].phis[phi.phi].inputs, input);
}

jit_value_t jit_builder_build_iconst(jit_builder_t builder,
                                     jit_value_type_t type,
                                     uint64_t value) {
    return LLVMConstInt(type, value, false);
}

jit_value_t jit_builder_build_iadd(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return LLVMBuildAdd(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_isub(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return LLVMBuildSub(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_and(jit_builder_t builder,
                                  jit_value_t lhs, jit_value_t rhs) {
    return LLVMBuildAnd(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_or(jit_builder_t builder,
                                 jit_value_t lhs, jit_value_t rhs) {
    return LLVMBuildOr(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_xor(jit_builder_t builder,
                                  jit_value_t lhs, jit_value_t rhs) {
    return LLVMBuildXor(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_shl(jit_builder_t builder,
                                  jit_value_t lhs, jit_value_t rhs) {
    return LLVMBuildShl(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_lshr(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return LLVMBuildLShr(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_ashr(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return LLVMBuildAShr(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_imul(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return LLVMBuildMul(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_sdiv(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return LLVMBuildSDiv(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_udiv(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return LLVMBuildSDiv(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_srem(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return LLVMBuildSRem(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_urem(jit_builder_t builder,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    return LLVMBuildURem(builder->builder, lhs, rhs, "");
}

jit_value_t jit_builder_build_iext(jit_builder_t builder, jit_value_t value) {
    return LLVMBuildZExt(builder->builder, value, JIT_TYPE_I64, "");
}

jit_value_t jit_builder_build_itrunc(jit_builder_t builder, jit_value_t value) {
    return LLVMBuildTrunc(builder->builder, value, JIT_TYPE_I32, "");
}

jit_value_t jit_builder_build_sfill(jit_builder_t builder, uint8_t width, jit_value_t value) {
    value = LLVMBuildTrunc(builder->builder, value, LLVMIntTypeInContext(m_context, width), "");
    return LLVMBuildSExt(builder->builder, value, LLVMTypeOf(value), "");
}

jit_value_t jit_builder_build_icmp(jit_builder_t builder,
                                   jit_icmp_kind_t icmp_kind,
                                   jit_value_type_t output_type,
                                   jit_value_t lhs,
                                   jit_value_t rhs) {
    LLVMValueRef value = LLVMBuildICmp(builder->builder, icmp_kind, lhs, rhs, "");
    return LLVMBuildZExt(builder->builder, value, output_type, "");
}

jit_value_t jit_builder_build_ptroff(jit_builder_t builder, jit_value_t ptr, jit_value_t off) {
    return LLVMBuildInBoundsGEP2(builder->builder, LLVMInt8TypeInContext(m_context), ptr, &off, 1, "");
}

static LLVMTypeRef get_load_store_type(jit_mem_size_t size) {
    switch (size) {
        case JIT_MEM_SIZE_8: return LLVMInt64TypeInContext(m_context);
        case JIT_MEM_SIZE_4: return LLVMInt32TypeInContext(m_context);
        case JIT_MEM_SIZE_2: return LLVMInt16TypeInContext(m_context);
        case JIT_MEM_SIZE_1: return LLVMInt8TypeInContext(m_context);
        default: ASSERT(0);
    }
}

jit_value_t jit_builder_build_load(jit_builder_t builder, jit_mem_size_t size, jit_value_type_t type, jit_value_t ptr) {
    LLVMValueRef result = LLVMBuildLoad2(builder->builder, get_load_store_type(size), ptr, "");
    if (size != JIT_MEM_SIZE_8 && size != JIT_MEM_SIZE_4) {
        result = LLVMBuildZExt(builder->builder, result, JIT_TYPE_I32, "");
    }
    return result;
}

void jit_builder_build_store(jit_builder_t builder, jit_mem_size_t size, jit_value_t data, jit_value_t ptr) {
    data = LLVMBuildTrunc(builder->builder, data, get_load_store_type(size), "");
    LLVMBuildStore(builder->builder, data, ptr);
}

jit_value_t jit_builder_build_stackslot(jit_builder_t builder, uint32_t size, uint32_t align) {
    // go to right before the branch from the entry point
    LLVMBasicBlockRef tmp_block = LLVMGetEntryBasicBlock(builder->function->value);
    LLVMValueRef terminator = LLVMGetBasicBlockTerminator(tmp_block);
    LLVMPositionBuilderBefore(builder->builder, terminator);

    // place the alloca
    LLVMValueRef stackslot = LLVMBuildAlloca(builder->builder, LLVMArrayType(LLVMInt8TypeInContext(m_context), size), "");
    LLVMSetAlignment(stackslot, align);

    // return to where we were
    if (builder->has_block) {
        LLVMPositionBuilderAtEnd(builder->builder, builder->blocks[builder->current_block].entry);
    }

    return stackslot;
}
