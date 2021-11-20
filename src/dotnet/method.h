#pragma once

#include "assembly.h"

#include <util/except.h>

// forward decl
struct type;

typedef struct param {
    // the name and type of the parameter
    const char* name;
    struct type* type;
} param_t;

typedef struct local {
    // the type of the local
    struct type* type;
} local_t;

typedef struct method {
    // the assembly this method is in
    assembly_t* assembly;
    token_t token;

    // The type this method belongs to
    struct type* parent;

    // the return type
    struct type* return_type;

    // the parameters to this method
    size_t parameter_count;
    param_t* parameters;

    // the locals of this method
    size_t locals_count;
    local_t* locals;

    // the name of the method
    const char* name;

    uint8_t is_static : 1;
    uint8_t is_virtual : 1;
    uint8_t is_final : 1;
    uint8_t is_abstract : 1;

    // the cil code
    uint8_t* cil;
    uint32_t cil_size;

    // the max eval stack size
    size_t max_stack_depth;
} method_t;

/**
 * Write a method's textual signature
 *
 * @param method            [IN] The method
 * @param mangled_name      [IN] The buffer to write to
 * @param buffer_size       [IN] The buffer's size
 */
err_t method_write_signature(method_t* method, char* buffer, size_t buffer_size);
