#pragma once

#include "tomatodotnet/except.h"
#include "metadata_tables.h"
#include "tomatodotnet/types/reflection.h"

typedef struct method_signature {
    uint32_t generic_param_count;
    ParameterInfo_Array parameters;
    ParameterInfo return_parameter;
} method_signature_t;

tdn_err_t sig_parse_method_def(
        blob_entry_t _blob,
        RuntimeAssembly assembly,
        RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
        bool create_mvar_type,
        method_signature_t* signature);

tdn_err_t sig_parse_method_spec(
        blob_entry_t _blob,
        RuntimeAssembly assembly,
        RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
        RuntimeTypeInfo_Array* out_gen_args);

tdn_err_t sig_parse_field(
        blob_entry_t _blob,
        RuntimeFieldInfo field_info);

tdn_err_t sig_parse_field_type(
        blob_entry_t _blob,
        RuntimeAssembly assembly,
        RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
        RuntimeTypeInfo* type);

tdn_err_t sig_parse_type_spec(
        blob_entry_t _blob,
        RuntimeAssembly assembly,
        RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
        RuntimeTypeInfo* type);

tdn_err_t sig_parse_local_var_sig(
        blob_entry_t _blob,
        RuntimeAssembly assembly,
        RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
        RuntimeMethodBody body);

tdn_err_t sig_parse_compressed_int(
        blob_entry_t* blob,
        uint32_t* value);
