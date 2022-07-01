#include "sig.h"

#include "../gc/gc.h"
#include "sig_spec.h"
#include "dotnet/loader.h"

static int m_idx_to_table[] = {
    [0] = METADATA_TYPE_DEF,
    [1] = METADATA_TYPE_REF,
    [2] = METADATA_TYPE_SPEC,
    [3] = -1
};

err_t parse_compressed_integer(blob_entry_t* sig, uint32_t* value) {
    err_t err = NO_ERROR;

    uint8_t a = CONSUME_BYTE();
    if ((a & 0x80) == 0) {
        *value = a;
        goto cleanup;
    }

    uint8_t b = CONSUME_BYTE();
    if ((a & 0xc0) == 0x80) {
        // 2-byte entry
        *value = ((int)(a & 0x3f)) << 8 | b;
        goto cleanup;
    }

    // 4-byte entry
    uint8_t c = CONSUME_BYTE();
    uint8_t d = CONSUME_BYTE();
    *value = ((int)(a & 0x1f)) << 24 | ((int)b) << 16 | ((int)c) << 8 | d;

cleanup:
    return err;
}

static err_t parse_type_def_or_ref_or_spec_encoded(blob_entry_t* sig, token_t* token) {
    err_t err = NO_ERROR;

    // get the compressed index
    uint32_t index;
    CHECK_AND_RETHROW(parse_compressed_integer(sig, &index));

    // get the table
    int table = m_idx_to_table[index & 0b11];
    CHECK(table != -1);

    // get the real index
    index >>= 2;

    // encode nicely
    *token = (token_t) {
        .table = table,
        .index = index
    };

cleanup:
    return err;
}


static err_t parse_custom_mod(blob_entry_t* sig, System_Reflection_Assembly assembly, System_Type* out_type, bool* required) {
    err_t err = NO_ERROR;

    CHECK(sig->size > 0);

    if (sig->data[0] == ELEMENT_TYPE_CMOD_OPT || sig->data[0] == ELEMENT_TYPE_CMOD_REQD) {
        // set the required properly
        *required = sig->data[0] == ELEMENT_TYPE_CMOD_REQD;
        NEXT_BYTE;

        // get the token
        token_t token;
        CHECK_AND_RETHROW(parse_type_def_or_ref_or_spec_encoded(sig, &token));
        CHECK(token.table == METADATA_TYPE_DEF || token.table == METADATA_TYPE_REF);

        // resolve the token
        CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, token, NULL, NULL, out_type));

        TRACE("Got modifier %U - %s", (*out_type)->Name, required ? "required" : "optional");
    } else {
        *out_type = NULL;
    }

cleanup:
    return err;
}

static err_t parse_type(
    System_Reflection_Assembly assembly,
    blob_entry_t* sig, System_Type* out_type,
    System_Type_Array typeArgs, System_Type_Array methodArgs,
    pe_file_t* file, metadata_t* metadata
) {
    err_t err = NO_ERROR;

    // switchhh
    uint8_t element_type = CONSUME_BYTE();
    switch (element_type) {
        case ELEMENT_TYPE_BOOLEAN: *out_type = tSystem_Boolean; break;
        case ELEMENT_TYPE_CHAR: *out_type = tSystem_Char; break;
        case ELEMENT_TYPE_I1: *out_type = tSystem_SByte; break;
        case ELEMENT_TYPE_U1: *out_type = tSystem_Byte; break;
        case ELEMENT_TYPE_I2: *out_type = tSystem_Int16; break;
        case ELEMENT_TYPE_U2: *out_type = tSystem_UInt16; break;
        case ELEMENT_TYPE_I4: *out_type = tSystem_Int32; break;
        case ELEMENT_TYPE_U4: *out_type = tSystem_UInt32; break;
        case ELEMENT_TYPE_I8: *out_type = tSystem_Int64; break;
        case ELEMENT_TYPE_U8: *out_type = tSystem_UInt64; break;
        case ELEMENT_TYPE_R4: *out_type = tSystem_Single; break;
        case ELEMENT_TYPE_R8: *out_type = tSystem_Double; break;
        case ELEMENT_TYPE_I: *out_type = tSystem_IntPtr; break;
        case ELEMENT_TYPE_U: *out_type = tSystem_UIntPtr; break;

        case ELEMENT_TYPE_VALUETYPE:
        case ELEMENT_TYPE_CLASS: {
            token_t token;
            CHECK_AND_RETHROW(parse_type_def_or_ref_or_spec_encoded(sig, &token));
            CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, token, typeArgs, methodArgs, out_type));
            CHECK(*out_type != NULL);

            // make sure to setup the type if needed
            if (file != NULL && metadata != NULL) {
                CHECK_AND_RETHROW(loader_setup_type(file, metadata, *out_type));
            }
        } break;

        case ELEMENT_TYPE_GENERICINST: {
            uint8_t kind = CONSUME_BYTE();
            CHECK(kind == ELEMENT_TYPE_CLASS || kind == ELEMENT_TYPE_VALUETYPE);

            // get the base type
            token_t token;
            CHECK_AND_RETHROW(parse_type_def_or_ref_or_spec_encoded(sig, &token));
            System_Type type;
            CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, token, typeArgs, methodArgs, &type));
            CHECK(type_is_generic_definition(type));

            // make sure to init the type if needed
            if (file != NULL && metadata != NULL) {
                CHECK_AND_RETHROW(loader_setup_type(file, metadata, type));
            }

            uint32_t gen_arg_count = 0;
            CHECK_AND_RETHROW(parse_compressed_integer(sig, &gen_arg_count));
            CHECK(gen_arg_count == type->GenericArguments->Length);

            typeArgs = typeArgs ?: type->GenericArguments;

            System_Type_Array newTypeArgs = GC_NEW_ARRAY(tSystem_Type, gen_arg_count);
            for (int i = 0; i < gen_arg_count; i++) {
                System_Type type_arg = NULL;
                CHECK_AND_RETHROW(parse_type(assembly, sig, &type_arg, typeArgs, methodArgs, file, metadata));
                GC_UPDATE_ARRAY(newTypeArgs, i, type_arg);
            }

            // we done
            CHECK_AND_RETHROW(type_make_generic(type, newTypeArgs, out_type));
        } break;

        case ELEMENT_TYPE_OBJECT: *out_type = tSystem_Object; break;

        case ELEMENT_TYPE_PTR: {
            *out_type = tSystem_UIntPtr;

            // TODO: CustomMod*

            // TODO: pointer types that store their actual type?
            CHECK(sig->size >= 1);
            System_Type elementType;
            if (sig->data[0] == ELEMENT_TYPE_VOID) {
                NEXT_BYTE;
                elementType = NULL;
            } else {
                CHECK_AND_RETHROW(parse_type(assembly, sig, &elementType, typeArgs, methodArgs, file, metadata));
            }
        } break;

        case ELEMENT_TYPE_STRING: *out_type = tSystem_String; break;

        case ELEMENT_TYPE_SZARRAY: {

            // TODO: CustomMod*

            System_Type elementType = NULL;
            CHECK_AND_RETHROW(parse_type(assembly, sig, &elementType, typeArgs, methodArgs, file, metadata));
            *out_type = get_array_type(elementType);
        } break;

        case ELEMENT_TYPE_MVAR:
        case ELEMENT_TYPE_VAR: {
            System_Type_Array arr = element_type == ELEMENT_TYPE_VAR ? typeArgs : methodArgs;
            CHECK(arr != NULL, "Missing arguments for %s", element_type == ELEMENT_TYPE_VAR ? "Type" : "Method");
            uint32_t number;
            CHECK_AND_RETHROW(parse_compressed_integer(sig, &number));
            CHECK(number < arr->Length);
            *out_type = arr->Data[number];
        } break;

        default: CHECK_FAIL("Got invalid element type: 0x%02x", element_type);
    }

    ASSERT(*out_type != NULL);

cleanup:
    return err;
}

static err_t parse_ret_type(
    System_Reflection_Assembly assembly,
    blob_entry_t* sig, System_Type* out_type,
    System_Type_Array typeArgs,
    System_Type_Array methodArgs,
    pe_file_t* file, metadata_t* metadata
)  {
    err_t err = NO_ERROR;


    // Handle custom mod
    System_Type mod = NULL;
    bool required = false;
    while (true) {
        CHECK_AND_RETHROW(parse_custom_mod(sig, assembly, &mod, &required));
        if (mod == NULL) {
            break;
        }

        // TODO: wth
        CHECK(!required);
    }

    // actually get the type
    bool is_by_ref = false;
    CHECK(sig->size > 0);
    switch (sig->data[0]) {
        case ELEMENT_TYPE_TYPEDBYREF: {
            NEXT_BYTE;
            CHECK_FAIL("TODO: wtf is this");
        } break;

        // we got void, let it fall down
        case ELEMENT_TYPE_VOID: {
            NEXT_BYTE;
            *out_type = NULL;
        } break;

        // we got a byref element, let it fall down
        case ELEMENT_TYPE_BYREF:
            NEXT_BYTE;
            is_by_ref = true;

        default: {
            System_Type type;
            CHECK_AND_RETHROW(parse_type(assembly, sig, &type, typeArgs, methodArgs, file, metadata));

            if (is_by_ref) {
                type = get_by_ref_type(type);
            }

            *out_type = type;
        } break;
    }

cleanup:
    return err;
}

static err_t parse_param(
    System_Reflection_Assembly assembly,
    blob_entry_t* sig,
    System_Reflection_MethodInfo method,
    System_Reflection_ParameterInfo parameter,
    System_Type_Array typeArgs, System_Type_Array methodArgs,
    pe_file_t* file, metadata_t* metadata
) {
    err_t err = NO_ERROR;


    // Handle custom mod
    System_Type mod = NULL;
    bool required = false;
    while (true) {
        CHECK_AND_RETHROW(parse_custom_mod(sig, assembly, &mod, &required));
        if (mod == NULL) {
            break;
        }

        // TODO: wth
        CHECK(!required);
    }

    // actually get the type
    bool is_by_ref = false;
    CHECK(sig->size > 0);
    switch (sig->data[0]) {
        case ELEMENT_TYPE_TYPEDBYREF: {
            NEXT_BYTE;
            CHECK_FAIL("TODO: wtf is this");
        } break;

        case ELEMENT_TYPE_BYREF:
            NEXT_BYTE;
            is_by_ref = true;

        default: {
            System_Type type;
            CHECK_AND_RETHROW(parse_type(
                    assembly, sig, &type,
                    typeArgs, methodArgs,
                    file, metadata));

            if (is_by_ref) {
                type = get_by_ref_type(type);
            }

            GC_UPDATE(parameter, ParameterType, type);
        } break;
    }

cleanup:
    return err;
}

err_t parse_field_sig(blob_entry_t _sig, System_Reflection_FieldInfo field, pe_file_t* file, metadata_t* metadata) {
    err_t err = NO_ERROR;
    blob_entry_t* sig = &_sig;

    // make sure this even points to a field
    EXPECT_BYTE(FIELD);

    // Handle custom mod
    System_Type mod = NULL;
    bool required = false;
    while (true) {
        CHECK_AND_RETHROW(parse_custom_mod(sig, field->Module->Assembly, &mod, &required));
        if (mod == NULL) {
            break;
        }

        // TODO: wth
        CHECK(!required);
    }

    // parse the actual field
    CHECK_AND_RETHROW(parse_type(
            field->Module->Assembly, sig, &field->FieldType,
            field->DeclaringType->GenericArguments, NULL,
            file, metadata));

cleanup:
    return err;
}

err_t parse_method_def_sig(blob_entry_t _sig, System_Reflection_MethodInfo method, pe_file_t* file, metadata_t* metadata) {
    err_t err = NO_ERROR;
    blob_entry_t* sig = &_sig;

    uint8_t header = CONSUME_BYTE();

    uint8_t cc = header & 0xf;
    CHECK(cc == DEFAULT || cc == VARARG);
    if (header & EXPLICITTHIS) {
        CHECK(header & HASTHIS, "Can't have an explicit `this` parameter without a `this` parameter");
        CHECK_FAIL("The EXPLICITTHIS bit can be set only in signatures for function pointers: signatures whose MethodDefSig is preceded by FNPTR");
    }

    if (header & GENERIC) {
        uint32_t generic_param_count = 0;
        CHECK_AND_RETHROW(parse_compressed_integer(sig, &generic_param_count));
        CHECK(method->GenericArguments != NULL);
        CHECK(generic_param_count == method->GenericArguments->Length);
    }

    // get the param count
    uint32_t param_count = 0;
    CHECK_AND_RETHROW(parse_compressed_integer(sig, &param_count));

    // get the return type
    System_Type retType;
    CHECK_AND_RETHROW(parse_ret_type(
            method->Module->Assembly, sig,
            &retType,
            method->DeclaringType->GenericArguments,
            method->GenericArguments,
            file, metadata));
    GC_UPDATE(method, ReturnType, retType);

    // allocate the parameters and update it
    GC_UPDATE(method, Parameters, GC_NEW_ARRAY(tSystem_Reflection_ParameterInfo, param_count));
    for (int i = 0; i < param_count; i++) {
        System_Reflection_ParameterInfo parameter = GC_NEW(tSystem_Reflection_ParameterInfo);
        CHECK_AND_RETHROW(parse_param(
                method->Module->Assembly, sig,
                method, parameter,
                method->DeclaringType->GenericArguments,
                method->GenericArguments,
                file, metadata));
        GC_UPDATE_ARRAY(method->Parameters, i, parameter);
    }

cleanup:
    return err;
}

static System_String get_empty_string() {
    static System_String str = NULL;
    if (str == NULL) {
        gc_add_root(&str);
        str = GC_NEW_STRING(0);
    }
    return str;
}

err_t parse_method_ref_sig(blob_entry_t _sig, System_Reflection_Assembly assembly, System_Reflection_MethodInfo* out_method, System_Type_Array typeArgs) {
    err_t err = NO_ERROR;
    blob_entry_t* sig = &_sig;

    System_Reflection_MethodInfo mi = GC_NEW(tSystem_Reflection_MethodInfo);

    uint8_t header = CONSUME_BYTE();

    // check the calling convention
    uint8_t cc = header & 0xf;
    CHECK(cc == VARARG || cc == DEFAULT);
    if (header & EXPLICITTHIS) {
        CHECK(header & HASTHIS, "Can't have an explicit `this` parameter without a `this` parameter");
        CHECK_FAIL("The EXPLICITTHIS bit can be set only in signatures for function pointers: signatures whose MethodDefSig is preceded by FNPTR");
    }

    if (!(header & HASTHIS)) {
        mi->Attributes |= 0x10;
    }

    if (header & GENERIC) {
        // handle generic method signature
        uint32_t generic_param_count = 0;
        CHECK_AND_RETHROW(parse_compressed_integer(sig, &generic_param_count));
        System_Type_Array generic_params = GC_NEW_ARRAY(tSystem_Type, generic_param_count);
        for (int i = 0; i < generic_param_count; i++) {
            System_Type typeParam = GC_NEW(tSystem_Type);
            GC_UPDATE(typeParam, Name, get_empty_string());
            typeParam->GenericParameterPosition = i;
            GC_UPDATE_ARRAY(generic_params, i, typeParam);
        }
        GC_UPDATE(mi, GenericArguments, generic_params);
    }

    // get the param count
    uint32_t param_count = 0;
    CHECK_AND_RETHROW(parse_compressed_integer(sig, &param_count));

    // get the return type
    System_Type retType;
    CHECK_AND_RETHROW(parse_ret_type(
            assembly, sig,
            &retType,
            typeArgs, mi->GenericArguments,
            NULL, NULL));
    GC_UPDATE(mi, ReturnType, retType);

    // allocate the parameters and update it
    GC_UPDATE(mi, Parameters, GC_NEW_ARRAY(tSystem_Reflection_ParameterInfo, param_count));
    for (int i = 0; i < param_count; i++) {
        System_Reflection_ParameterInfo parameter = GC_NEW(tSystem_Reflection_ParameterInfo);
        CHECK_AND_RETHROW(parse_param(assembly, sig, mi, parameter,
                                      typeArgs, mi->GenericArguments,
                                      NULL, NULL));
        GC_UPDATE_ARRAY(mi->Parameters, i, parameter);
    }

    if (header & SENTINEL) {
        CHECK_FAIL("I HAVE NO IDEA WHATS GOING ON");
    }

    // output it
    *out_method = mi;

cleanup:
    return err;
}

err_t parse_method_spec(blob_entry_t _sig, System_Reflection_Assembly assembly, System_Reflection_MethodInfo* out_method, System_Type_Array typeArgs, System_Type_Array methodArgs) {
    err_t err = NO_ERROR;
    blob_entry_t* sig = &_sig;
    System_Reflection_MethodInfo method = *out_method;

    EXPECT_BYTE(0x0A);

    uint32_t gen_arg_count = 0;
    CHECK_AND_RETHROW(parse_compressed_integer(sig, &gen_arg_count));
    CHECK(gen_arg_count == method->GenericArguments->Length);

    System_Type_Array newTypeArgs = GC_NEW_ARRAY(tSystem_Type, gen_arg_count);
    for (int i = 0; i < gen_arg_count; i++) {
        System_Type type_arg = NULL;
        CHECK_AND_RETHROW(parse_type(assembly, sig, &type_arg, typeArgs, methodArgs, NULL, NULL));
        GC_UPDATE_ARRAY(newTypeArgs, i, type_arg);
    }

    // we're done
    CHECK_AND_RETHROW(method_make_generic(method, newTypeArgs, out_method));

cleanup:
    return err;
}

err_t parse_type_spec(blob_entry_t _sig, System_Reflection_Assembly assembly, System_Type* out_type, System_Type_Array typeArgs, System_Type_Array methodArgs) {
    err_t err = NO_ERROR;
    blob_entry_t* sig = &_sig;

    CHECK_AND_RETHROW(parse_type(assembly, sig, out_type, typeArgs, methodArgs, NULL, NULL));

cleanup:
    return err;
}

err_t parse_local_var_sig(blob_entry_t _sig, System_Reflection_MethodInfo method, pe_file_t* file, metadata_t* metadata) {
    err_t err = NO_ERROR;
    blob_entry_t* sig = &_sig;

    // we expect this to be a local sig
    EXPECT_BYTE(LOCAL_SIG);

    // get the count
    uint32_t count = 0;
    CHECK_AND_RETHROW(parse_compressed_integer(sig, &count));

    // create the array of local variables and set all their types
    GC_UPDATE(method->MethodBody, LocalVariables, GC_NEW_ARRAY(tSystem_Reflection_LocalVariableInfo, count));
    for (int i = 0; i < count; i++) {
        System_Reflection_LocalVariableInfo variable = GC_NEW(tSystem_Reflection_LocalVariableInfo);
        GC_UPDATE_ARRAY(method->MethodBody->LocalVariables, i, variable);
        variable->LocalIndex = i;

        CHECK(sig->size > 0);
        if (sig->data[0] == ELEMENT_TYPE_TYPEDBYREF) {
            NEXT_BYTE;
            CHECK_FAIL("TODO: wtf is this");
            continue;
        }

        // handle custom mod
        // TODO:

        // handle constraint
        // TODO:

        // actually get the type
        bool is_by_ref = false;
        CHECK(sig->size > 0);
        switch (sig->data[0]) {
            case ELEMENT_TYPE_BYREF:
                NEXT_BYTE;
                is_by_ref = true;

            default: {
                System_Type type;
                CHECK_AND_RETHROW(parse_type(
                        method->Module->Assembly, sig, &type,
                        method->DeclaringType->GenericArguments,
                        method->GenericArguments,
                        file, metadata));

                if (is_by_ref) {
                    type = get_by_ref_type(type);
                }

                GC_UPDATE(variable, LocalType, type);
            } break;
        }
    }


cleanup:
    return err;
}