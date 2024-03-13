#include "sig.h"
#include "util/except.h"
#include "dotnet/gc/gc.h"
#include "util/murmur64.h"
#include "util/stb_ds.h"

#define FIELD 0x6
#define LOCAL_SIG 0x7

#define HASTHIS 0x20
#define EXPLICITTHIS 0x40
#define DEFAULT 0x0
#define VARARG 0x5
#define GENERIC 0x10

#define ELEMENT_TYPE_END 0x00
#define ELEMENT_TYPE_VOID 0x01
#define ELEMENT_TYPE_BOOLEAN 0x02
#define ELEMENT_TYPE_CHAR 0x03
#define ELEMENT_TYPE_I1 0x04
#define ELEMENT_TYPE_U1 0x05
#define ELEMENT_TYPE_I2 0x06
#define ELEMENT_TYPE_U2 0x07
#define ELEMENT_TYPE_I4 0x08
#define ELEMENT_TYPE_U4 0x09
#define ELEMENT_TYPE_I8 0x0a
#define ELEMENT_TYPE_U8 0x0b
#define ELEMENT_TYPE_R4 0x0c
#define ELEMENT_TYPE_R8 0x0d
#define ELEMENT_TYPE_STRING 0x0e
#define ELEMENT_TYPE_PTR 0x0f
#define ELEMENT_TYPE_BYREF 0x10
#define ELEMENT_TYPE_VALUETYPE 0x11
#define ELEMENT_TYPE_CLASS 0x12
#define ELEMENT_TYPE_VAR 0x13
#define ELEMENT_TYPE_ARRAY 0x14
#define ELEMENT_TYPE_GENERICINST 0x15
#define ELEMENT_TYPE_TYPEDBYREF 0x16
#define ELEMENT_TYPE_I 0x18
#define ELEMENT_TYPE_U 0x19
#define ELEMENT_TYPE_FNPTR 0x1b
#define ELEMENT_TYPE_OBJECT 0x1c
#define ELEMENT_TYPE_SZARRAY 0x1d
#define ELEMENT_TYPE_MVAR 0x1e
#define ELEMENT_TYPE_CMOD_REQD 0x1f
#define ELEMENT_TYPE_CMOD_OPT 0x20
#define ELEMENT_TYPE_INTERNAL 0x21
#define ELEMENT_TYPE_MODIFIER 0x40
#define ELEMENT_TYPE_SENTINEL 0x41
#define ELEMENT_TYPE_PINNED 0x45

// TODO: 0x50 - indicates and argument of type System.Type
// TODO: 0x51 - Used in custom attributes to specify a boxed object
// TODO: 0x53 - Used in custom attributes to indicate a FIELD
// TODO: 0x54 - Used in custom attributes to indicate a PROPERTY
// TODO: 0x55 - Used in custom attributes to specify an enum

#define FETCH_BYTE \
    ({ \
        CHECK(blob->size >= 1); \
        uint8_t ___byte = *blob->data; \
        blob->data++; \
        blob->size--; \
        ___byte; \
    })

#define PEEK_BYTE \
    ({ \
        CHECK(blob->size >= 1); \
        *blob->data; \
    })

tdn_err_t sig_parse_compressed_int(blob_entry_t* blob, uint32_t* value) {
    tdn_err_t err = TDN_NO_ERROR;

    uint8_t a = FETCH_BYTE;
    if ((a & 0x80) == 0) {
        *value = a;
        goto cleanup;
    }

    uint8_t b = FETCH_BYTE;
    if ((a & 0xc0) == 0x80) {
        // 2-byte entry
        *value = ((int)(a & 0x3f)) << 8 | b;
        goto cleanup;
    }

    // 4-byte entry
    uint8_t c = FETCH_BYTE;
    uint8_t d = FETCH_BYTE;
    *value = ((int)(a & 0x1f)) << 24 | ((int)b) << 16 | ((int)c) << 8 | d;

cleanup:
    return err;
}

static int m_idx_to_table[] = {
    [0] = METADATA_TYPE_DEF,
    [1] = METADATA_TYPE_REF,
    [2] = METADATA_TYPE_SPEC,
    [3] = -1
};

static tdn_err_t sig_parse_type_def_or_ref_or_spec_encoded(
    blob_entry_t* blob, RuntimeAssembly assembly,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    RuntimeTypeInfo* type
) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the compressed index
    uint32_t index = 0;
    CHECK_AND_RETHROW(sig_parse_compressed_int(blob, &index));

    // get the table
    int table = m_idx_to_table[index & 0b11];
    CHECK(table != -1);

    // get the real index
    index >>= 2;

    // get the type
    CHECK_AND_RETHROW(tdn_assembly_lookup_type(
            assembly,
            (token_t){ .index = index, .table = table }.token,
            typeArgs, methodArgs,
            type));

cleanup:
    return err;
}



static tdn_err_t sig_get_next_custom_mod(
    blob_entry_t* blob, RuntimeAssembly assembly,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    RuntimeTypeInfo* type, bool* required
) {
    tdn_err_t err = TDN_NO_ERROR;

    uint8_t byte = PEEK_BYTE;
    if (byte == ELEMENT_TYPE_CMOD_OPT || byte == ELEMENT_TYPE_CMOD_REQD) {
        FETCH_BYTE;
        *required = byte == ELEMENT_TYPE_CMOD_REQD;
        CHECK_AND_RETHROW(sig_parse_type_def_or_ref_or_spec_encoded(blob, assembly, typeArgs, methodArgs, type));
    } else {
        *type = NULL;
    }

cleanup:
    return err;
}

static tdn_err_t sig_parse_type(
    blob_entry_t* blob,
    RuntimeAssembly assembly,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    bool create_mvar_type,
    RuntimeTypeInfo* type
);

static tdn_err_t sig_parse_genericinst(
    blob_entry_t* blob,
    RuntimeAssembly assembly, RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    RuntimeTypeInfo* type
) {
    tdn_err_t err = TDN_NO_ERROR;

    uint8_t value = FETCH_BYTE;
    CHECK(value == ELEMENT_TYPE_CLASS || value == ELEMENT_TYPE_VALUETYPE);

    // get the base type
    RuntimeTypeInfo base = NULL;
    CHECK_AND_RETHROW(sig_parse_type_def_or_ref_or_spec_encoded(blob, assembly, typeArgs, methodArgs, &base));

    // get how many arguments we need
    uint32_t gen_arg_count = 0;
    CHECK_AND_RETHROW(sig_parse_compressed_int(blob, &gen_arg_count));

    // get the arguments
    RuntimeTypeInfo_Array args = GC_NEW_ARRAY(RuntimeTypeInfo, gen_arg_count);
    for (size_t i = 0; i < gen_arg_count; i++) {
        RuntimeTypeInfo arg = NULL;
        CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, methodArgs, false, &arg));
        args->Elements[i] = arg;
    }

    // get the generic type instance
    CHECK_AND_RETHROW(tdn_type_make_generic(base, args, type));

cleanup:
    return err;
}

static tdn_err_t sig_parse_type(
    blob_entry_t* blob,
    RuntimeAssembly assembly,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    bool create_mvar_type,
    RuntimeTypeInfo* type
) {
    tdn_err_t err = TDN_NO_ERROR;

    uint8_t value = FETCH_BYTE;
    switch (value) {
        case ELEMENT_TYPE_BOOLEAN: *type = tBoolean; break;
        case ELEMENT_TYPE_CHAR: *type = tChar; break;
        case ELEMENT_TYPE_I1: *type = tSByte; break;
        case ELEMENT_TYPE_U1: *type = tByte; break;
        case ELEMENT_TYPE_I2: *type = tInt16; break;
        case ELEMENT_TYPE_U2: *type = tUInt16; break;
        case ELEMENT_TYPE_I4: *type = tInt32; break;
        case ELEMENT_TYPE_U4: *type = tUInt32; break;
        case ELEMENT_TYPE_I8: *type = tInt64; break;
        case ELEMENT_TYPE_U8: *type = tUInt64; break;
        // TODO: case ELEMENT_TYPE_R4: *type = tSystem_Single; break;
        // TODO: case ELEMENT_TYPE_R8: *type = tSystem_Double; break;
         case ELEMENT_TYPE_I: *type = tIntPtr; break;
         case ELEMENT_TYPE_U: *type = tUIntPtr; break;
        // TODO: case ELEMENT_TYPE_ARRAY: break;

        case ELEMENT_TYPE_VALUETYPE:
        case ELEMENT_TYPE_CLASS: {
            // NOTE: we don't do any check on the valuetype/class properties, we
            //       just go with whatever it resolves to
            CHECK_AND_RETHROW(sig_parse_type_def_or_ref_or_spec_encoded(blob, assembly, typeArgs, methodArgs, type));
        } break;

        // TODO: case ELEMENT_TYPE_FNPTR: break;

        case ELEMENT_TYPE_GENERICINST: {
            CHECK_AND_RETHROW(sig_parse_genericinst(blob, assembly, typeArgs, methodArgs, type));
        } break;

        case ELEMENT_TYPE_MVAR: {
            uint32_t number = 0;
            CHECK_AND_RETHROW(sig_parse_compressed_int(blob, &number));
            if (methodArgs == NULL) {
                CHECK(create_mvar_type);
                RuntimeTypeInfo typ = GC_NEW(RuntimeTypeInfo);
                typ->IsGenericParameter = 1;
                typ->IsGenericMethodParameter = 1;
                typ->GenericParameterPosition = number;
                *type = typ;
            } else {
                CHECK(number < methodArgs->Length);
                *type = methodArgs->Elements[number];
            }
        } break;

        case ELEMENT_TYPE_OBJECT: *type = tObject; break;

        case ELEMENT_TYPE_PTR: {
            // Parse custom modifiers
            RuntimeTypeInfo cmod_type = NULL;
            bool required = false;
            CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
            while(cmod_type != NULL) {
                WARN("Unknown mod %T", cmod_type);
                CHECK(!required);
                CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
            }

            // Get the element type
            RuntimeTypeInfo element_type = NULL;
            if (PEEK_BYTE == ELEMENT_TYPE_VOID) {
                FETCH_BYTE;
                element_type = tVoid;
            } else {
                CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, methodArgs, create_mvar_type, &element_type));
            }

            // Get the pointer
            CHECK_AND_RETHROW(tdn_get_pointer_type(element_type, type));
        } break;
        case ELEMENT_TYPE_STRING: *type = tString; break;
        case ELEMENT_TYPE_SZARRAY: {
            // Parse custom modifiers
            RuntimeTypeInfo cmod_type = NULL;
            bool required = false;
            CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
            while(cmod_type != NULL) {
                WARN("Unknown mod %T", cmod_type);
                CHECK(!required);
                CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
            }

            // Get the element type
            RuntimeTypeInfo element_type = NULL;
            CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, methodArgs, create_mvar_type, &element_type));

            // Get as array
            CHECK_AND_RETHROW(tdn_get_array_type(element_type, type));
        } break;

        case ELEMENT_TYPE_VAR: {
            uint32_t number = 0;
            CHECK_AND_RETHROW(sig_parse_compressed_int(blob, &number));
            CHECK(typeArgs != NULL);
            CHECK(number < typeArgs->Length);
            *type = typeArgs->Elements[number];
        } break;

        default:
            CHECK_FAIL("Unknown type element %02x", value);
            break;
    }

cleanup:
    if (!IS_ERROR(err)) {
        CHECK(*type != NULL);
    }

    return err;
}

static tdn_err_t sig_parse_ret_type(
    blob_entry_t* blob,
    RuntimeAssembly assembly,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    bool create_var_type,
    ParameterInfo* out_param
) {
    tdn_err_t err = TDN_NO_ERROR;

    ParameterInfo parameter_info = GC_NEW(ParameterInfo);
    parameter_info->Position = -1;

    // Parse custom modifiers
    RuntimeTypeInfo cmod_type = NULL;
    bool required = false;
    CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
    while(cmod_type != NULL) {
        if (cmod_type == tInAttribute) {
            parameter_info->IsReadonly = 1;
        } else {
            WARN("Unknown mod %T", cmod_type);
            CHECK(!required);
        }
        CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
    }

    // Now figure the correct type
    switch (PEEK_BYTE) {
        case ELEMENT_TYPE_BYREF: {
            FETCH_BYTE;
            RuntimeTypeInfo type = NULL;
            CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, methodArgs, create_var_type, &type));
            CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));
            parameter_info->ParameterType = type;
        } break;

        case ELEMENT_TYPE_TYPEDBYREF: {
            FETCH_BYTE;
            CHECK_FAIL();
        } break;

        case ELEMENT_TYPE_VOID: {
            FETCH_BYTE;
            parameter_info->ParameterType = tVoid;
        } break;

        default: {
            RuntimeTypeInfo type = NULL;
            CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, methodArgs, create_var_type, &type));
            parameter_info->ParameterType = type;
        } break;
    }

    *out_param = parameter_info;

cleanup:
    return err;
}

static tdn_err_t sig_parse_param(
    blob_entry_t* blob,
    RuntimeAssembly assembly,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    bool create_mvar_type,
    ParameterInfo parameter_info
) {
    tdn_err_t err = TDN_NO_ERROR;

    // Parse custom modifiers
    RuntimeTypeInfo cmod_type = NULL;
    bool required = false;
    CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
    while(cmod_type != NULL) {
        WARN("Unknown mod %T", cmod_type);
        CHECK(!required);
        CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
    }

    // figure the correct type
    switch(PEEK_BYTE) {
        case ELEMENT_TYPE_BYREF: {
            FETCH_BYTE;
            RuntimeTypeInfo type = NULL;
            CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, methodArgs, create_mvar_type, &type));
            CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));
            parameter_info->ParameterType = type;
        } break;

        case ELEMENT_TYPE_TYPEDBYREF: {
            FETCH_BYTE;
            CHECK_FAIL();
        } break;

        default: {
            RuntimeTypeInfo type = NULL;
            CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, methodArgs, create_mvar_type, &type));
            parameter_info->ParameterType = type;
        } break;
    }


cleanup:
    return err;
}

tdn_err_t sig_parse_field(blob_entry_t _blob, RuntimeFieldInfo field_info) {
    tdn_err_t err = TDN_NO_ERROR;
    blob_entry_t* blob = &_blob;

    RuntimeAssembly assembly = field_info->Module->Assembly;
    RuntimeTypeInfo_Array typeArgs = field_info->DeclaringType->GenericArguments;

    CHECK(FETCH_BYTE == FIELD);

    // Parse custom modifiers
    RuntimeTypeInfo cmod_type = NULL;
    bool required = false;
    CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, NULL, &cmod_type, &required));
    while(cmod_type != NULL) {
        if (cmod_type == tIsVolatile) {
            field_info->IsVolatile = 1;
        } else {
            WARN("Unknown mod %T", cmod_type);
            CHECK(!required);
        }

        CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, NULL, &cmod_type, &required));
    }

    // check if this is a ref field
    bool is_ref = false;
    if (PEEK_BYTE == ELEMENT_TYPE_BYREF) {
        FETCH_BYTE;
        is_ref = true;

        // make sure it is not defined in a class
        CHECK(tdn_type_is_valuetype(field_info->DeclaringType));

        // mark the owner as a ref-struct, we will check later if it has
        // any other problems
        field_info->DeclaringType->IsByRefStruct = 1;
    }

    // parse the type
    CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, NULL, false, &field_info->FieldType));

    if (is_ref) {
        // get the byref type
        CHECK_AND_RETHROW(tdn_get_byref_type(field_info->FieldType, &field_info->FieldType));
    }

cleanup:
    return err;
}

tdn_err_t sig_parse_field_type(
    blob_entry_t _blob,
    RuntimeAssembly assembly,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    RuntimeTypeInfo* type
) {
    tdn_err_t err = TDN_NO_ERROR;
    blob_entry_t* blob = &_blob;

    CHECK(FETCH_BYTE == FIELD);

    // Parse custom modifiers
    RuntimeTypeInfo cmod_type = NULL;
    bool required = false;
    CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
    while(cmod_type != NULL) {
        WARN("Unknown mod %T", cmod_type);
        CHECK(!required);
        CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
    }

    bool is_ref = false;
    if (PEEK_BYTE == ELEMENT_TYPE_BYREF) {
        FETCH_BYTE;
        is_ref = true;
    }

    // parse the type
    CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, methodArgs, false, type));

    if (is_ref) {
        // get the byref type
        CHECK_AND_RETHROW(tdn_get_byref_type(*type, type));
    }

cleanup:
    return err;
}

tdn_err_t sig_parse_type_spec(
    blob_entry_t _blob,
    RuntimeAssembly assembly,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    RuntimeTypeInfo* type
) {
    tdn_err_t err = TDN_NO_ERROR;
    blob_entry_t* blob = &_blob;

    uint8_t value = FETCH_BYTE;
    switch (value) {
        case ELEMENT_TYPE_PTR: {
            // Parse custom modifiers
            RuntimeTypeInfo cmod_type = NULL;
            bool required = false;
            CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
            while(cmod_type != NULL) {
                WARN("Unknown mod %T", cmod_type);
                CHECK(!required);
                CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
            }

            // can either be a VOID or Type
            if (PEEK_BYTE == ELEMENT_TYPE_VOID) {
                CHECK_AND_RETHROW(tdn_get_pointer_type(tVoid, type));
            } else {
                CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, methodArgs, false, type));
            }
        } break;

//        case ELEMENT_TYPE_FNPTR: {
//
//        } break;

//        case ELEMENT_TYPE_ARRAY: {
//
//        } break;

        case ELEMENT_TYPE_SZARRAY: {
            // Parse custom modifiers
            RuntimeTypeInfo cmod_type = NULL;
            bool required = false;
            CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
            while(cmod_type != NULL) {
                WARN("Unknown mod %T", cmod_type);
                CHECK(!required);
                CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
            }

            RuntimeTypeInfo elementType = NULL;
            CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, methodArgs, false, &elementType));
            CHECK_AND_RETHROW(tdn_get_array_type(elementType, type));
        } break;

        case ELEMENT_TYPE_GENERICINST: {
            CHECK_AND_RETHROW(sig_parse_genericinst(blob, assembly, typeArgs, methodArgs, type));
        } break;

        //
        // NOTE: both of these are not in the standard but are found in real binaries
        //

        case ELEMENT_TYPE_MVAR: {
            uint32_t number = 0;
            CHECK_AND_RETHROW(sig_parse_compressed_int(blob, &number));
            CHECK(methodArgs != NULL);
            CHECK(number < methodArgs->Length);
            *type = methodArgs->Elements[number];
        } break;

        case ELEMENT_TYPE_VAR: {
            uint32_t number = 0;
            CHECK_AND_RETHROW(sig_parse_compressed_int(blob, &number));
            CHECK(typeArgs != NULL);
            CHECK(number < typeArgs->Length);
            *type = typeArgs->Elements[number];
        } break;

        default:
            CHECK_FAIL("Unknown element type %02x", value);
    }

cleanup:
    return err;
}

tdn_err_t sig_parse_method_def(
    blob_entry_t _blob,
    RuntimeAssembly assembly,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    bool create_mvar_type,
    method_signature_t* signature
) {
    tdn_err_t err = TDN_NO_ERROR;
    blob_entry_t* blob = &_blob;

    uint8_t byte = FETCH_BYTE;
    CHECK(!(byte & EXPLICITTHIS)); // only used in fnptr, we ignore it
    CHECK(!(byte & VARARG));

    if (byte & GENERIC) {
        CHECK_AND_RETHROW(sig_parse_compressed_int(blob, &signature->generic_param_count));
    }

    // Get the ParamCount
    uint32_t param_count = 0;
    CHECK_AND_RETHROW(sig_parse_compressed_int(blob, &param_count));

    // Get the RetType
    CHECK_AND_RETHROW(sig_parse_ret_type(blob, assembly,
                                         typeArgs, methodArgs,
                                         create_mvar_type,
                                         &signature->return_parameter));

    // Get the Params
    signature->parameters = GC_NEW_ARRAY(ParameterInfo, param_count);
    for (int i = 0; i < param_count; i++) {
        ParameterInfo parameter_info = GC_NEW(ParameterInfo);
        parameter_info->Position = i;
        CHECK_AND_RETHROW(sig_parse_param(blob, assembly, typeArgs, methodArgs, create_mvar_type, parameter_info));
        signature->parameters->Elements[i] = parameter_info;
    }

cleanup:
    return err;
}


tdn_err_t sig_parse_method_spec(
    blob_entry_t _blob,
    RuntimeAssembly assembly,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    RuntimeTypeInfo_Array* out_gen_args
) {
    tdn_err_t err = TDN_NO_ERROR;
    blob_entry_t* blob = &_blob;

    CHECK(FETCH_BYTE == 0x0A);

    // get the argument count
    uint32_t gen_arg_count = 0;
    CHECK_AND_RETHROW(sig_parse_compressed_int(blob, &gen_arg_count));

    // Get the Params
    RuntimeTypeInfo_Array gen_args = GC_NEW_ARRAY(RuntimeTypeInfo, gen_arg_count);
    for (int i = 0; i < gen_arg_count; i++) {
        RuntimeTypeInfo gen_arg;
        CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, methodArgs, false, &gen_arg));
        gen_args->Elements[i] = gen_arg;
    }

    *out_gen_args = gen_args;

cleanup:
    return err;
}

tdn_err_t sig_parse_local_var_sig(
    blob_entry_t _blob,
    RuntimeAssembly assembly,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    RuntimeMethodBody body
) {
    tdn_err_t err = TDN_NO_ERROR;
    blob_entry_t* blob = &_blob;

    CHECK(FETCH_BYTE == LOCAL_SIG);

    uint32_t count = 0;
    CHECK_AND_RETHROW(sig_parse_compressed_int(blob, &count));
    CHECK(1 <= count && count <= 0xFFFE);

    RuntimeLocalVariableInfo_Array arr = GC_NEW_ARRAY(RuntimeLocalVariableInfo, count);
    for (size_t i = 0; i < count; i++) {
        RuntimeLocalVariableInfo variable = GC_NEW(RuntimeLocalVariableInfo);
        arr->Elements[i] = variable;
        variable->LocalIndex = (int)i;

        // we don't support this
        if (PEEK_BYTE == ELEMENT_TYPE_TYPEDBYREF) {
            CHECK_FAIL();
        }

        // Parse custom modifiers
        RuntimeTypeInfo cmod_type = NULL;
        bool required = false;
        CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
        while(cmod_type != NULL) {
            WARN("Unknown mod %T", cmod_type);
            CHECK(!required);
            CHECK_AND_RETHROW(sig_get_next_custom_mod(blob, assembly, typeArgs, methodArgs, &cmod_type, &required));
        }

        // check the Constraint
        if (PEEK_BYTE == ELEMENT_TYPE_PINNED) {
            FETCH_BYTE;
            variable->IsPinned = true;
        }

        // remember if its a byref
        bool byref = false;
        if (PEEK_BYTE == ELEMENT_TYPE_BYREF) {
            FETCH_BYTE;
            byref = true;
        }

        // parse it properly
        RuntimeTypeInfo type = NULL;
        CHECK_AND_RETHROW(sig_parse_type(blob, assembly, typeArgs, methodArgs, false, &type));

        // if it was byref actually get the byref type
        if (byref) {
            CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));
        }

        variable->LocalType = type;

    }

    body->LocalVariables = arr;

cleanup:
    return err;
}
