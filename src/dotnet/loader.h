#pragma once

#include "tomatodotnet/except.h"
#include "tomatodotnet/types/basic.h"
#include "tomatodotnet/types/reflection.h"
#include "dotnet/metadata/metadata_tables.h"

tdn_err_t tdn_parser_method_body(
    RuntimeAssembly assembly,
    metadata_method_def_t* method_def,
    RuntimeMethodBase methodBase
);

tdn_err_t tdn_type_init(RuntimeTypeInfo type);

/**
 * Generate the prime for this interface
 */
tdn_err_t tdn_generate_interface_prime(RuntimeTypeInfo InterfaceType);

/**
 * Create a vtable for the given type
 */
tdn_err_t tdn_create_vtable(RuntimeTypeInfo type, int count);
