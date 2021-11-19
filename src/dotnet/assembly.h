#pragma once

#include <util/except.h>

#include <stddef.h>
#include <stdint.h>
#include "types.h"

// forward decl
typedef struct assembly assembly_t;
struct type;
struct method;

/**
 * This is the kernel's assembly, it contains all the base libraries
 * and abstractions required by others, all running inside a single
 * instance that is shared across all other binaries.
 */
extern assembly_t* g_corelib_assembly;

/**
 * Load an assembly from a binary blob
 *
 * @param blob          [IN] The blob data
 * @param blob_size     [IN] The blob size
 * @param assembly      [OUT] The loaded assembly
 */
err_t load_assembly_from_blob(uint8_t* blob, size_t blob_size, assembly_t** assembly);

err_t load_assembly(const char* file, assembly_t** assembly);

/**
 * Get the method by its token, returns NULL if the method was not found.
 *
 * This also handles method refs and alike
 *
 * @param assembly      [IN] The assembly we are calling from
 * @param token         [IN] The token of the method to find
 */
struct method* assembly_get_method_by_token(assembly_t* assembly, token_t token);

/**
 * Get the method by its token, returns NULL if the method was not found.
 *
 * This also handles method refs and alike
 *
 * @param assembly      [IN] The assembly we are calling from
 * @param token         [IN] The token of the method to find
 */
struct type* assembly_get_type_by_token(assembly_t* assembly, token_t token);

struct type* assembly_get_type_by_name(assembly_t* assembly, const char* namespace, const char* name);

/**
 * Free the assembly and everything related to it
 *
 * @param assembly      [IN] The assembly to free
 */
void free_assembly(assembly_t* assembly);
