#include "signature.h"
#include "metadata_spec.h"
#include "signature_spec.h"
#include "assembly_internal.h"

#include <stdlib.h>

static int sig_get_entry(sig_t* sig) {
    unsigned char a,b,c,d;
    a = *sig->entry++;
    if ((a & 0x80) == 0) {
        // 1-byte entry
        return a;
    }

    // Special case
    if (a == 0xff) {
        return 0;
    }

    b = *sig->entry++;
    if ((a & 0xc0) == 0x80) {
        // 2-byte entry
        return ((int)(a & 0x3f)) << 8 | b;
    }

    // 4-byte entry
    c = *sig->entry++;
    d = *sig->entry++;
    return ((int)(a & 0x1f)) << 24 | ((int)b) << 16 | ((int)c) << 8 | d;
}

static uint8_t m_table_id[] = {
    METADATA_TYPE_DEF,
    METADATA_TYPE_REF,
    METADATA_TYPE_SPEC,
    0
};

static token_t sig_get_type_def_or_ref_or_spec(sig_t* sig) {
    uint32_t entry = sig_get_entry(sig);
    return (token_t){ .table = m_table_id[entry & 0x3], .index = entry >> 2 };
}

static err_t sig_get_type(assembly_t* assembly, sig_t* sig, type_t** type) {
    err_t err = NO_ERROR;

    int entry = sig_get_entry(sig);
    switch (entry) {
        // short forms
        case ELEMENT_TYPE_VOID: *type = g_void; break;
        case ELEMENT_TYPE_BOOLEAN: *type = g_boolean; break;
        case ELEMENT_TYPE_CHAR: *type = g_char; break;
        case ELEMENT_TYPE_I1: *type = g_sbyte; break;
        case ELEMENT_TYPE_U1: *type = g_byte; break;
        case ELEMENT_TYPE_I2: *type = g_int16; break;
        case ELEMENT_TYPE_U2: *type = g_uint16; break;
        case ELEMENT_TYPE_I4: *type = g_int32; break;
        case ELEMENT_TYPE_U4: *type = g_uint32; break;
        case ELEMENT_TYPE_I8: *type = g_int64; break;
        case ELEMENT_TYPE_U8: *type = g_uint64; break;
        case ELEMENT_TYPE_R4: *type = g_float; break;
        case ELEMENT_TYPE_R8: *type = g_double; break;
        case ELEMENT_TYPE_STRING: *type = g_string; break;
        case ELEMENT_TYPE_I: *type = g_intptr; break;
        case ELEMENT_TYPE_U: *type = g_uintptr; break;
        case ELEMENT_TYPE_OBJECT: *type = g_object; break;

        // class reference
        case ELEMENT_TYPE_VALUETYPE:
        case ELEMENT_TYPE_CLASS: {
            token_t token = sig_get_type_def_or_ref_or_spec(sig);
            switch (token.table) {
                // a type from our assembly
                case METADATA_TYPE_DEF: {
                    if (1 <= token.index && token.index - 1 < assembly->types_count) {
                        *type = &assembly->types[token.index - 1];
                        goto cleanup;
                    }
                } break;

                // TODO: type spec / type ref

                // unknown table
                default: CHECK_FAIL("Unknown table %d", token.table);
            }
        } break;

        // pointer type
        case ELEMENT_TYPE_PTR: {
            type_t* element = NULL;
            CHECK_AND_RETHROW(sig_get_type(assembly, sig, &element));
            *type = get_ptr_type(element);
        } break;

        // unknown entry
        default: CHECK_FAIL("Invalid entry: %x", entry);
    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static size_t sig_get_length(sig_t* sig) {
    return sig_get_entry(sig);
}

static err_t sig_parse_ret_type(sig_t* sig, assembly_t* assembly, type_t** type) {
    err_t err = NO_ERROR;
    bool by_ref = false;

    sig_t temp = *sig;
    int entry = sig_get_entry(sig);
    switch (entry) {
        // TODO: TYPEBYREF

        // VOID
        case ELEMENT_TYPE_VOID:
            *type = g_void;
            break;

        // BYREF
        case ELEMENT_TYPE_BYREF:
            temp = *sig;
            by_ref = true;

            // fallthrough to Type

        // Type
        default: {
            *sig = temp;
            CHECK_AND_RETHROW(sig_get_type(assembly, sig, type));
            if (by_ref) {
                *type = get_by_ref_type(*type);
                CHECK(*type != NULL);
            }
        }

    }

cleanup:
    return err;
}

static err_t sig_parse_param(sig_t* sig, assembly_t* assembly, type_t** type) {
    err_t err = NO_ERROR;
    bool by_ref = false;

    sig_t temp = *sig;
    int entry = sig_get_entry(sig);
    switch (entry) {
        // TODO: TYPEBYREF

        // BYREF
        case ELEMENT_TYPE_BYREF:
            temp = *sig;
            by_ref = true;
            // fallthrough to Type

        // Type
        default: {
            *sig = temp;
            CHECK_AND_RETHROW(sig_get_type(assembly, sig, type));
            if (by_ref) {
                *type = get_by_ref_type(*type);
                CHECK(*type != NULL);
            }
        }
    }

cleanup:
    return err;
}

err_t sig_parse_field(sig_t* sig, assembly_t* assembly, field_t* field) {
    err_t err = NO_ERROR;

    sig_get_length(sig);

    // must start with field spec
    CHECK(sig_get_entry(sig) == 0x06);

    // get the type
    CHECK_AND_RETHROW(sig_get_type(assembly, sig, &field->type));

cleanup:
    return err;
}

err_t sig_parse_method_locals(sig_t* sig, method_t* method) {
    err_t err = NO_ERROR;

    sig_get_length(sig);

    // LOCAL_SIG
    CHECK(sig_get_entry(sig) == 0x07);

    // Count
    method->locals_count = sig_get_entry(sig);
    method->locals = malloc(sizeof(local_t) * method->locals_count);

    for (int i = 0; i < method->locals_count; i++) {
        CHECK_AND_RETHROW(sig_parse_param(sig, method->assembly, &method->locals[i].type));
    }

cleanup:
    return err;
}

err_t sig_parse_method(sig_t* sig, method_t* method) {
    err_t err = NO_ERROR;

    sig_get_length(sig);

    // starting from the first block
    int entry = *sig->entry++;

    // this stuff
    if (entry & 0x20) {
        // HASTHIS

        if (entry & 0x40) {
            // EXPLICITTHIS
        }
    }

    // calling convention
    switch (entry & 0x1f) {
        case 0x00: break; // DEFAULT
        case 0x05: CHECK_FAIL("TODO: VARARG"); break; // VARARG
        case 0x10: CHECK_FAIL("TODO: GENERIC"); break; // GENERIC
    }

    // ParamCount
    method->parameter_count = sig_get_entry(sig);
    method->parameters = malloc(sizeof(param_t) * method->parameter_count);

    // RetType
    CHECK_AND_RETHROW(sig_parse_ret_type(sig, method->assembly, &method->return_type));

    // Params
    for (int i = 0; i < method->parameter_count; i++) {
        CHECK_AND_RETHROW(sig_parse_param(sig, method->assembly, &method->parameters[i].type));
    }

cleanup:
    return err;
}

