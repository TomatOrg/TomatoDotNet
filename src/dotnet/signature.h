#pragma once

#include <stdint.h>
#include "types.h"
#include "type.h"
#include "method.h"

typedef struct sig {
    const uint8_t* entry;
} sig_t;

/**
 * Fill the field from the given field signature
 *
 * @param sig       [IN] The signature
 * @param assembly  [IN] The assembly this field is related to
 * @param field     [IN] The field
 */
err_t sig_parse_field(sig_t* sig, assembly_t* assembly, field_t* field);

/**
 * Fill the method's locals information from the given local var signature
 *
 * @param sig       [IN] The signature
 * @param method    [IN] The method
 */
err_t sig_parse_method_locals(sig_t* sig, method_t* method);

/**
 * Fill the method from the given method signature
 *
 * @param sig       [IN] The signature
 * @param method    [IN] The method
 */
err_t sig_parse_method(sig_t* sig, method_t* method);
