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

tdn_err_t tdn_size_init(RuntimeTypeInfo type);