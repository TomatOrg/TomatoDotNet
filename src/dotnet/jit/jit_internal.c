#include "jit_internal.h"

#include <util/alloc.h>
#include <util/except.h>
#include <util/stb_ds.h>

#include "jit_basic_block.h"
#include "jit_emit.h"
#include "jit_helpers.h"

static struct {
    RuntimeMethodBase key;
    jit_method_t* value;
}* m_jit_methods;

static struct {
    spidir_function_t key;
    jit_method_t* value;
}* m_jit_functions;

tdn_err_t jit_get_method(RuntimeMethodBase method, jit_method_t** result) {
    tdn_err_t err = TDN_NO_ERROR;

    int idx = hmgeti(m_jit_methods, method);
    if (idx >= 0) {
        *result = m_jit_methods[idx].value;
        goto cleanup;
    }

    jit_method_t* jmethod = tdn_mallocz(sizeof(*method));
    CHECK_ERROR(jmethod != NULL, TDN_ERROR_OUT_OF_MEMORY);
    hmput(m_jit_methods, method, jmethod);

    jmethod->method = method;

    // TODO: maybe we should just have a way to save the optimized spidir
    //       instead of re-emitting every time...
    if (method->MethodBody == NULL) {
        if (method->MethodImplFlags.CodeType == TDN_METHOD_IMPL_CODE_TYPE_RUNTIME) {
            // the method is a runtime method, we will
            // need to emit it manually, this will be done
            // later on
            CHECK_FAIL();

        } else {
            // idr when this path makes sense
            CHECK(method->MethodImplFlags.CodeType == TDN_METHOD_IMPL_CODE_TYPE_IL);
            CHECK(method->Attributes.Abstract);
            CHECK_FAIL();
        }

        // runtime methods don't need verification
        jmethod->verified = true;

    } else if (method->MethodPtr != NULL) {
        // we have a method pointer already, meaning this was jitted before
        // we are going to mark it as verified, and the code in the emitter
        // will know to create the extern for it
        jmethod->verified = true;
    }

    // we can now put the function -> method lookup for later use
    hmput(m_jit_functions, jmethod->function, jmethod);

    *result = jmethod;

cleanup:
    return err;
}

// void jit_method_register_thunk(jit_method_t* method) {
//     hmput(m_jit_functions, method->global_state.thunk, method);
// }

jit_method_t* jit_get_method_from_function(spidir_function_t function) {
    int idx = hmgeti(m_jit_functions, function);
    if (idx >= 0) {
        return m_jit_functions[idx].value;
    }
    return NULL;
}

RuntimeExceptionHandlingClause jit_get_enclosing_try_clause(jit_method_t* method, uint32_t pc, int type, RuntimeExceptionHandlingClause previous) {
    RuntimeExceptionHandlingClause_Array arr = method->method->MethodBody->ExceptionHandlingClauses;
    RuntimeExceptionHandlingClause matched = NULL;

    for (int i = 0; i < arr->Length; i++) {
        RuntimeExceptionHandlingClause clause = arr->Elements[i];
        if (previous == clause) {
            break;
        }

        if (clause->Flags != type) {
            continue;
        }

        if (clause->TryOffset <= pc && pc < clause->TryOffset + clause->TryLength) {
            if (
                matched == NULL ||
                (
                    matched->TryOffset < clause->TryOffset &&
                    matched->TryOffset + matched->TryLength > clause->TryOffset + clause->TryLength
                )
            ) {
                matched = clause;
            }
        }
    }

    return matched;
}

static void free_basic_block(jit_basic_block_t* block) {
    if (block != NULL) {
        TRACE("%p", block);
        arrfree(block->stack);
        arrfree(block->locals);
        arrfree(block->args);
        arrfree(block->leave_target_stack);
        tdn_host_free(block);
    }
}

void jit_destroy_method(jit_method_t* method) {
    arrfree(method->block_queue);
    arrfree(method->locals);
    arrfree(method->args);

    arrfree(method->basic_blocks);
    for (int i = 0; i < hmlen(method->labels); i++) {
        free_basic_block(method->labels[i].value);
    }
    hmfree(method->labels);

    for (int i = 0; i < hmlen(method->leave_blocks); i++) {
        free_basic_block(method->leave_blocks[i].value);
    }
    hmfree(method->leave_blocks);
}

void jit_clean() {
    // clean the helpers
    jit_helper_clean();

    for (int i = 0; i < hmlen(m_jit_methods); i++) {
        jit_destroy_method(m_jit_methods[i].value);
    }

    // and finally clear the functions table
    hmfree(m_jit_methods);
    hmfree(m_jit_functions);
}

RuntimeTypeInfo jit_get_reduced_type(RuntimeTypeInfo T) {
    if (T == NULL) return NULL;

    if (T->EnumUnderlyingType != NULL) {
        if (T->EnumUnderlyingType == tByte) return tSByte;
        if (T->EnumUnderlyingType == tUInt16) return tInt16;
        if (T->EnumUnderlyingType == tUInt32) return tInt32;
        if (T->EnumUnderlyingType == tUInt64) return tInt64;
        if (T->EnumUnderlyingType == tUIntPtr) return tIntPtr;
        return T->EnumUnderlyingType;
    }

    return T;
}

RuntimeTypeInfo jit_get_verification_type(RuntimeTypeInfo T) {
    if (T == NULL) return NULL;
    RuntimeTypeInfo T_reduced = jit_get_reduced_type(T);

    if (T_reduced == tSByte || T_reduced == tBoolean) return tSByte;
    if (T_reduced == tInt16 || T_reduced == tChar) return tInt16;
    if (T_reduced == tInt32) return tInt32;
    if (T_reduced == tInt64) return tInt64;

    if (T->IsByRef) {
        RuntimeTypeInfo S = T->ElementType;
        RuntimeTypeInfo S_reduced = jit_get_reduced_type(S);

        if (S_reduced == tSByte || S_reduced == tBoolean) {
            RuntimeTypeInfo result = NULL;
            ASSERT(!IS_ERROR(tdn_get_byref_type(tSByte, &result)));
            return result;
        }

        if (S_reduced == tInt16 || S_reduced == tChar) {
            RuntimeTypeInfo result = NULL;
            ASSERT(!IS_ERROR(tdn_get_byref_type(tInt16, &result)));
            return result;
        }

        if (S_reduced == tInt32) {
            RuntimeTypeInfo result = NULL;
            ASSERT(!IS_ERROR(tdn_get_byref_type(tInt32, &result)));
            return result;
        }

        if (S_reduced == tInt64) {
            RuntimeTypeInfo result = NULL;
            ASSERT(!IS_ERROR(tdn_get_byref_type(tInt64, &result)));
            return result;
        }

        if (S_reduced == tIntPtr) {
            RuntimeTypeInfo result = NULL;
            ASSERT(!IS_ERROR(tdn_get_byref_type(tIntPtr, &result)));
            return result;
        }
    }

    return T;
}

RuntimeTypeInfo jit_get_intermediate_type(RuntimeTypeInfo T) {
    RuntimeTypeInfo T_verification = jit_get_verification_type(T);
    if (
        T_verification == tSByte ||
        T_verification == tInt16 ||
        T_verification == tInt32
    ) {
        return tInt32;
    }

    // TODO: floating point

    return T_verification;
}
