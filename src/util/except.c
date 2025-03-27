#include "except.h"

const char* tdn_get_error_string(tdn_err_t err) {
    if (err < 0) {
        return tdn_host_error_to_string(-err);
    }

    switch (err) {
        case TDN_NO_ERROR: return "Success";
        case TDN_ERROR_CHECK_FAILED: return "Check failed";
        case TDN_ERROR_OUT_OF_MEMORY: return "Out of memory";
        case TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE: return "VERIFIER_EXPECTED_NUMERIC_TYPE";
        case TDN_ERROR_VERIFIER_STACK_UNEXPECTED: return "VERIFIER_STACK_UNEXPECTED";
        case TDN_ERROR_VERIFIER_BRANCH_OUT_OF_TRY: return "VERIFIER_BRANCH_OUT_OF_TRY";
        case TDN_ERROR_VERIFIER_BRANCH_INTO_TRY: return "VERIFIER_BRANCH_INTO_TRY";
        case TDN_ERROR_VERIFIER_BRANCH_OUT_OF_FINALLY: return "VERIFIER_BRANCH_OUT_OF_FINALLY";
        case TDN_ERROR_VERIFIER_FALLTHROUGH_EXCEPTION: return "VERIFIER_FALLTHROUGH_EXCEPTION";
        case TDN_ERROR_VERIFIER_BAD_JUMP_TARGET: return "VERIFIER_BAD_JUMP_TARGET";
        case TDN_ERROR_VERIFIER_THIS_MISMATCH: return "VERIFIER_THIS_MISMATCH";
        case TDN_ERROR_VERIFIER_CTOR_EXPECTED: return "VERIFIER_CTOR_EXPECTED";
        case TDN_ERROR_VERIFIER_CTOR_SIG: return "VERIFIER_CTOR_SIG";
        case TDN_ERROR_VERIFIER_RETURN_VOID: return "VERIFIER_RETURN_VOID";
        case TDN_ERROR_VERIFIER_RETURN_MISSING: return "VERIFIER_RETURN_MISSING";
        case TDN_ERROR_VERIFIER_RETURN_EMPTY: return "VERIFIER_RETURN_EMPTY";
        case TDN_ERROR_VERIFIER_RETURN_FROM_TRY: return "VERIFIER_RETURN_FROM_TRY";
        case TDN_ERROR_VERIFIER_RETURN_FROM_HANDLER: return "VERIFIER_RETURN_FROM_HANDLER";
        case TDN_ERROR_VERIFIER_RETURN_PTR_TO_STACK: return "VERIFIER_RETURN_PTR_TO_STACK";
        case TDN_ERROR_VERIFIER_METHOD_FALLTHROUGH: return "VERIFIER_METHOD_FALLTHROUGH";
        default: return "<UNKNOWN>";
    }
}


void dump_hex(const void* data, size_t size) {
    char ascii[17];
    size_t i, j;
    ascii[16] = '\0';
    for (i = 0; i < size; ++i) {
        tdn_host_printf("%02X ", ((unsigned char*)data)[i]);
        if (((unsigned char*)data)[i] >= ' ' && ((unsigned char*)data)[i] <= '~') {
            ascii[i % 16] = ((unsigned char*)data)[i];
        } else {
            ascii[i % 16] = '.';
        }
        if ((i+1) % 8 == 0 || i+1 == size) {
            tdn_host_printf(" ");
            if ((i+1) % 16 == 0) {
                tdn_host_printf("|  %s \n", ascii);
            } else if (i+1 == size) {
                ascii[(i+1) % 16] = '\0';
                if ((i+1) % 16 <= 8) {
                    tdn_host_printf(" ");
                }
                for (j = (i+1) % 16; j < 16; ++j) {
                    tdn_host_printf("   ");
                }
                tdn_host_printf("|  %s \n", ascii);
            }
        }
    }
}
