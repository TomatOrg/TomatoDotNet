#pragma once

typedef enum tdn_err {
    TDN_USER_ERROR_START = -1,
    TDN_NO_ERROR = 0,
    TDN_ERROR_CHECK_FAILED = 1,
    TDN_ERROR_OUT_OF_MEMORY = 2,

    // verifier errors
    TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE,
} tdn_err_t;

const char* tdn_get_error_string(tdn_err_t err);
