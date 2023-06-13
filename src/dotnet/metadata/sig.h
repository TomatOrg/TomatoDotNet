#pragma once

#include "tinydotnet/except.h"
#include "metadata_tables.h"
#include "tinydotnet/types/reflection.h"

typedef struct method_signature {
    uint32_t generic_param_count;
    System_Reflection_ParameterInfo_Array parameters;
    System_Reflection_ParameterInfo return_parameter;
} method_signature_t;

tdn_err_t sig_parse_method_def(blob_entry_t _blob, System_Reflection_Assembly assembly, System_Type_Array typeArgs, System_Type_Array methodArgs, method_signature_t* signature);

tdn_err_t sig_parse_field(blob_entry_t _blob, System_Reflection_FieldInfo field_info);

tdn_err_t sig_parse_type_spec(blob_entry_t _blob, System_Reflection_Assembly assembly, System_Type_Array typeArgs, System_Type_Array methodArgs, System_Type* type);

tdn_err_t sig_parse_compressed_int(blob_entry_t* blob, uint32_t* value);
