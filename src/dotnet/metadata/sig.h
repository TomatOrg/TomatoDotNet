#pragma once

#include "../types.h"

#include <util/except.h>

#define NEXT_BYTE \
    do { \
        sig->data++; \
        sig->size--; \
    } while (0)

#define CONSUME_BYTE() \
    ({ \
        CHECK(sig->size > 0); \
        uint8_t b = sig->data[0]; \
        NEXT_BYTE; \
        b; \
    })

#define EXPECT_BYTE(value) \
    do { \
        CHECK(sig->size > 0); \
        CHECK(sig->data[0] == (value), "Expected %d, but got %d", value, sig->data[0]); \
        NEXT_BYTE; \
    } while (0)

err_t parse_compressed_integer(blob_entry_t* sig, uint32_t* value);

err_t parse_field_sig(blob_entry_t sig, System_Reflection_FieldInfo field, pe_file_t* file, metadata_t* metadata);

err_t parse_method_def_sig(blob_entry_t sig, System_Reflection_MethodInfo method, pe_file_t* file, metadata_t* metadata);

err_t parse_method_ref_sig(blob_entry_t sig, System_Reflection_Assembly assembly, System_Reflection_MethodInfo* out_method, System_Type_Array typeArgs);

err_t parse_method_spec(blob_entry_t _sig, System_Reflection_Assembly assembly, System_Reflection_MethodInfo* out_method, System_Type_Array typeArgs, System_Type_Array methodArgs);

err_t parse_type_spec(blob_entry_t sig, System_Reflection_Assembly assembly, System_Type* out_type, System_Type_Array typeArgs, System_Type_Array methodArgs);

err_t parse_stand_alone_local_var_sig(blob_entry_t sig, System_Reflection_MethodInfo method, pe_file_t* file, metadata_t* metadata);
