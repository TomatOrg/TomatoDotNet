#pragma once

#include "tomatodotnet/except.h"
#include "tomatodotnet/types/basic.h"
#include "tomatodotnet/types/reflection.h"
#include "dotnet/metadata/metadata_tables.h"

/**
 * The corelib assembly
 */
extern RuntimeAssembly gCoreAssembly;

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

tdn_err_t tdn_find_explicit_implementation(RuntimeTypeInfo type, RuntimeMethodInfo method, RuntimeMethodInfo* out_body);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Loader utilities, used by generic code
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Connect the interfaces that came from the parent of the type, can be done either before or after
 * the pass over the interface impls
 */
tdn_err_t loader_connect_interfaces_from_parent(RuntimeTypeInfo type);

/**
 * Connect to given interface to the given class, properly initializing everything
 * that needs to be initialized
 */
tdn_err_t loader_connect_single_interface_impl(RuntimeTypeInfo class, RuntimeTypeInfo interface);
