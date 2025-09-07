#include "localloc.h"

#include "dotnet/types.h"
#include "tomatodotnet/disasm.h"
#include "tomatodotnet/types/type.h"
#include "util/except.h"

static bool is_unsafe_span_ctor(RuntimeMethodBase method) {
    // not span type, ignore
    if (method->DeclaringType == NULL) return false;
    if (method->DeclaringType->GenericTypeDefinition != tSpan) return false;

    // check the arguments
    if (method->Parameters->Length != 2) return false;
    if (!method->Parameters->Elements[0]->ParameterType->IsPointer) return false;
    if (method->Parameters->Elements[1]->ParameterType != tInt32) return false;

    // verified!
    return true;
}

tdn_err_t localloc_verifier_check(localloc_verifier_t* verifier, tdn_il_inst_t* inst, bool allow_unsafe) {
    tdn_err_t err = TDN_NO_ERROR;

    bool call_verified = allow_unsafe;
    switch (verifier->state) {
        case LOCALLOC_VERIFIER_STATE__NONE: {
            if (inst->opcode == CEE_LDC_I4) {
                verifier->state = LOCALLOC_VERIFIER_STATE__FOUND_LDC_I4;
                verifier->size = inst->operand.int32;
            } else {
                verifier->state = LOCALLOC_VERIFIER_STATE__NONE;
            }
        } break;

        case LOCALLOC_VERIFIER_STATE__FOUND_LDC_I4: {
            if (inst->opcode == CEE_CONV_U) {
                verifier->state = LOCALLOC_VERIFIER_STATE__FOUND_CONV_U;
            } else {
                verifier->state = LOCALLOC_VERIFIER_STATE__NONE;
            }
        } break;

        case LOCALLOC_VERIFIER_STATE__FOUND_CONV_U: {
            if (inst->opcode == CEE_LOCALLOC) {
                inst->operand_type = TDN_IL_INT32;
                inst->operand.int32 = verifier->size;
                verifier->state = LOCALLOC_VERIFIER_STATE__FOUND_LOCALLOC;

            } else if (inst->opcode == CEE_SIZEOF) {
                // TODO: overflow checking or something
                verifier->size *= inst->operand.type->StackSize;
                verifier->state = LOCALLOC_VERIFIER_STATE__FOUND_SIZEOF;

            } else {
                verifier->state = LOCALLOC_VERIFIER_STATE__NONE;
            }
        } break;

        case LOCALLOC_VERIFIER_STATE__FOUND_SIZEOF: {
            if (inst->opcode == CEE_MUL_OVF_UN) {
                verifier->state = LOCALLOC_VERIFIER_STATE__FOUND_MUL_OVF_UN;
            } else {
                verifier->state = LOCALLOC_VERIFIER_STATE__NONE;
            }
        } break;

        case LOCALLOC_VERIFIER_STATE__FOUND_MUL_OVF_UN: {
            if (inst->opcode == CEE_LOCALLOC) {
                inst->operand_type = TDN_IL_INT32;
                inst->operand.int32 = verifier->size;
                verifier->state = LOCALLOC_VERIFIER_STATE__FOUND_LOCALLOC;

            } else {
                verifier->state = LOCALLOC_VERIFIER_STATE__NONE;
            }
        } break;

        case LOCALLOC_VERIFIER_STATE__FOUND_LOCALLOC: {
            if (!allow_unsafe) {
                // we must have the opcode to load the length now
                CHECK(inst->opcode == CEE_LDC_I4);

                // at this point we already allocated, so we need to verify
                // that we are invoking the Span ctor correctly, to do so verify
                // in here that the size divides perfectly, and remember the per
                // element size that we expect to have
                CHECK((verifier->size % inst->operand.int32) == 0);
                verifier->size /= inst->operand.int32;

                // next we need to find ctor
                verifier->state = LOCALLOC_VERIFIER_STATE__FOUND_LDC_I4_LEN;
            } else {
                // we don't care whatever happens next
                verifier->state = LOCALLOC_VERIFIER_STATE__NONE;
            }
        } break;

        case LOCALLOC_VERIFIER_STATE__FOUND_LDC_I4_LEN: {
            // we can only reach here if we don't allow unsafe
            if (inst->opcode == CEE_NEWOBJ) {
                // ensure we are calling the unsafe span ctor
                CHECK(is_unsafe_span_ctor(inst->operand.method));

                // ensure the element size matches
                RuntimeTypeInfo span_type = inst->operand.method->DeclaringType->GenericArguments->Elements[0];
                CHECK(span_type->StackSize == verifier->size);

                // we verified the call, we can continue with
                // the normal pattern now
                call_verified = true;
                verifier->state = LOCALLOC_VERIFIER_STATE__NONE;

            } else {
                // we must have seen
                CHECK_FAIL();
            }
        } break;
    }

    // if we have not verified the call, then ensure that we don't call
    // the unsafe span ctor
    if (!call_verified && inst->operand_type == TDN_IL_METHOD) {
        CHECK(!is_unsafe_span_ctor(inst->operand.method));
    }

cleanup:
    return err;
}
