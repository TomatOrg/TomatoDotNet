#include "sig.h"

#include "../gc/gc.h"
#include "sig_spec.h"


static err_t parse_custom_mod(blob_entry_t* sig, bool* found) {
    err_t err = NO_ERROR;

    CHECK(sig->size > 0);

    if (sig->data[0] == ELEMENT_TYPE_CMOD_OPT || sig->data[0] == ELEMENT_TYPE_CMOD_REQD) {
        // got a custom mod
        NEXT_BYTE;

        // TODO: this
        ASSERT(!"TODO: handle this!");

        *found = true;
    } else {
        *found = false;
    }

cleanup:
    return err;
}

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

static err_t parse_type(
    System_Reflection_Assembly assembly,
    blob_entry_t* sig, System_Type* out_type, bool allow_void,
    System_Type_Array typeArgs, System_Type_Array methodArgs
) {
    err_t err = NO_ERROR;

    // switchhh
    uint8_t element_type = CONSUME_BYTE();
    switch (element_type) {
        case ELEMENT_TYPE_VOID: CHECK(allow_void); *out_type = NULL; break;
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
        // TODO: array

        case ELEMENT_TYPE_VALUETYPE:
        case ELEMENT_TYPE_CLASS: {
            token_t token;
            CHECK_AND_RETHROW(parse_type_def_or_ref_or_spec_encoded(sig, &token));
            CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, token, typeArgs, methodArgs, out_type));
            CHECK(*out_type != NULL);
        } break;

        // TODO: fnptr
        // TODO: genericinst
        case ELEMENT_TYPE_OBJECT: *out_type = tSystem_Object; break;

        case ELEMENT_TYPE_PTR: {
            *out_type = tSystem_UIntPtr;

            // TODO: pointer types that store their actual type?
            System_Type elementType;
            CHECK_AND_RETHROW(parse_type(assembly, sig, &elementType, true, NULL, NULL));
        } break;

        case ELEMENT_TYPE_STRING: *out_type = tSystem_String; break;

        case ELEMENT_TYPE_SZARRAY: {
            System_Type elementType = NULL;
            CHECK_AND_RETHROW(parse_type(assembly, sig, &elementType, false, NULL, NULL));
            *out_type = get_array_type(elementType);
        } break;

        case ELEMENT_TYPE_MVAR:
        case ELEMENT_TYPE_VAR: {
            System_Type_Array arr = element_type == ELEMENT_TYPE_VAR ? typeArgs : methodArgs;
            CHECK(arr != NULL);
            uint32_t number;
            CHECK_AND_RETHROW(parse_compressed_integer(sig, &number));
            CHECK(number < arr->Length);
            *out_type = arr->Data[number];
        } break;

        default: CHECK_FAIL("Got invalid element type: 0x%02x", element_type);
    }

cleanup:
    return err;
}

static err_t parse_ret_type(System_Reflection_Assembly assembly, blob_entry_t* sig, System_Type* out_type)  {
    err_t err = NO_ERROR;

    // get custom mods
    // TODO: wtf is a custom mod
    bool found = false;
    do {
        CHECK_AND_RETHROW(parse_custom_mod(sig, &found));
    } while (found);

    // actually get the type
    bool is_by_ref = false;
    CHECK(sig->size > 0);
    switch (sig->data[0]) {
        case ELEMENT_TYPE_TYPEDBYREF: {
            NEXT_BYTE;
            CHECK_FAIL("TODO: wtf is this");
        } break;

        // we got a byref element, let it fall down
        case ELEMENT_TYPE_BYREF:
            NEXT_BYTE;
            is_by_ref = true;

        // we got void, let it fall down
        case ELEMENT_TYPE_VOID:
        default: {
            System_Type type;
            CHECK_AND_RETHROW(parse_type(assembly, sig, &type, true, NULL, NULL));

            if (is_by_ref) {
                type = get_by_ref_type(type);
            }

            gc_update_ref(out_type, type);
        } break;
    }

cleanup:
    return err;
}

static err_t parse_param(System_Reflection_Assembly assembly, blob_entry_t* sig, System_Reflection_MethodInfo method, System_Reflection_ParameterInfo parameter) {
    err_t err = NO_ERROR;

    // get custom mods
    // TODO: wtf is a custom mod
    bool found = false;
    do {
        CHECK_AND_RETHROW(parse_custom_mod(sig, &found));
    } while (found);

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
            CHECK_AND_RETHROW(parse_type(assembly, sig, &type, false, method->DeclaringType->GenericArguments, method->GenericArguments));

            if (is_by_ref) {
                type = get_by_ref_type(type);
            }

            GC_UPDATE(parameter, ParameterType, type);
        } break;
    }

cleanup:
    return err;
}

err_t parse_field_sig(blob_entry_t _sig, System_Reflection_FieldInfo field) {
    err_t err = NO_ERROR;
    blob_entry_t* sig = &_sig;

    // make sure this even points to a field
    EXPECT_BYTE(FIELD);

    // get custom mods
    // TODO: wtf is a custom mod
    bool found = false;
    do {
        CHECK_AND_RETHROW(parse_custom_mod(sig, &found));
    } while (found);

    // parse the actual field
    CHECK_AND_RETHROW(parse_type(field->Module->Assembly, sig, &field->FieldType, false, field->DeclaringType->GenericArguments, NULL));

cleanup:
    return err;
}

err_t parse_stand_alone_method_sig(blob_entry_t _sig, System_Reflection_MethodInfo method, bool ref) {
    err_t err = NO_ERROR;
    blob_entry_t* sig = &_sig;

    uint8_t header = CONSUME_BYTE();

    // check the calling convention
    uint8_t cc = header & 0xf;
    CHECK(cc == DEFAULT || cc == VARARG || cc == C || cc == STDCALL || cc == THISCALL || cc == FASTCALL);
    if (header & EXPLICITTHIS) {
        CHECK(header & HASTHIS, "Can't have an explicit `this` parameter without a `this` parameter");
        CHECK_FAIL("The EXPLICITTHIS bit can be set only in signatures for function pointers: signatures whose MethodDefSig is preceded by FNPTR");
    }

    // get the param count
    uint32_t param_count = 0;

    if (header & HASTHIS) {
        if (!ref) {
            CHECK(!method_is_static(method), "Methods with this must not be static");
        }
    } else {
        if (!ref) {
            method->Attributes |= 0x0010;
        }
    }

    CHECK_AND_RETHROW(parse_compressed_integer(sig, &param_count));

    // get the return type
    CHECK_AND_RETHROW(parse_ret_type(method->Module->Assembly, sig, &method->ReturnType));

    // allocate the parameters and update it
    GC_UPDATE(method, Parameters, GC_NEW_ARRAY(tSystem_Reflection_ParameterInfo, param_count));
    for (int i = 0; i < param_count; i++) {
        System_Reflection_ParameterInfo parameter = GC_NEW(tSystem_Reflection_ParameterInfo);
        CHECK_AND_RETHROW(parse_param(method->Module->Assembly, sig, method, parameter));
        GC_UPDATE_ARRAY(method->Parameters, i, parameter);
    }

    if (header & SENTINEL) {
        CHECK_FAIL("I HAVE NO IDEA WHATS GOING ON");
    }


cleanup:
    return err;
}

err_t parse_stand_alone_local_var_sig(blob_entry_t _sig, System_Reflection_MethodInfo method) {
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
        bool found = false;
        CHECK_AND_RETHROW(parse_custom_mod(sig, &found));

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
                CHECK_AND_RETHROW(parse_type(method->Module->Assembly, sig, &type, false, NULL, NULL));

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

err_t parse_type_spec(blob_entry_t _sig, System_Reflection_Assembly assembly, System_Type* out_type, System_Type_Array typeArgs, System_Type_Array methodArgs) {
    err_t err = NO_ERROR;
    blob_entry_t* sig = &_sig;

    switch (CONSUME_BYTE()) {
        case ELEMENT_TYPE_PTR: {
            CHECK_FAIL("TODO: PTR");
        } break;

        case ELEMENT_TYPE_FNPTR: {
            CHECK_FAIL("TODO FNPTR");
        } break;

        case ELEMENT_TYPE_ARRAY: {
            CHECK_FAIL("TODO: ARRAY");
        } break;

        case ELEMENT_TYPE_SZARRAY: {
            // get custom mods
            // TODO: wtf is a custom mod
            bool found = false;
            do {
                CHECK_AND_RETHROW(parse_custom_mod(sig, &found));
            } while (found);

            // get the type
            System_Type type = NULL;
            CHECK_AND_RETHROW(parse_type(assembly, sig, &type, false, typeArgs, methodArgs));

            // get the array type
            *out_type = get_array_type(type);
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

            uint32_t gen_arg_count = 0;
            CHECK_AND_RETHROW(parse_compressed_integer(sig, &gen_arg_count));
            CHECK(gen_arg_count == type->GenericArguments->Length);

            System_Type_Array newTypeArgs = GC_NEW_ARRAY(tSystem_Type, gen_arg_count);
            for (int i = 0; i < gen_arg_count; i++) {
                System_Type type_arg = NULL;
                CHECK_AND_RETHROW(parse_type(assembly, sig, &type_arg, false, typeArgs, methodArgs));
                GC_UPDATE_ARRAY(newTypeArgs, i, type_arg);
            }

            // we done
            *out_type = type_make_generic(type, newTypeArgs);
        } break;
    }

cleanup:
    return err;
}
