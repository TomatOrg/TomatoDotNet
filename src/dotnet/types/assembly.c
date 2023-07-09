
#include "tinydotnet/types/type.h"
#include "dotnet/metadata/metadata_tables.h"
#include "util/except.h"
#include "dotnet/metadata/sig.h"
#include "dotnet/metadata/metadata.h"
#include "dotnet/loader.h"

tdn_err_t tdn_assembly_lookup_type(
    RuntimeAssembly assembly,
    int metadata_token,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    RuntimeTypeInfo* type
) {
    tdn_err_t err = TDN_NO_ERROR;

    token_t token = { .token = metadata_token };
    switch (token.table) {
        case METADATA_TYPE_DEF: {
            CHECK(token.index != 0 && token.index <= assembly->TypeDefs->Length);
            *type = assembly->TypeDefs->Elements[token.index - 1];
        } break;

        case METADATA_TYPE_SPEC: {
            CHECK(token.index != 0 && token.index <= assembly->Metadata->type_specs_count);
            blob_entry_t entry = assembly->Metadata->type_specs[token.index - 1].signature;
            CHECK_AND_RETHROW(sig_parse_type_spec(entry, assembly, typeArgs, methodArgs, type));
        } break;

        default:
            CHECK_FAIL("tdn_assembly_lookup_type: called with invalid table %02x", token.table);
    }

    // make sure the type is fully initialized
    // TODO: initialize the size properly

cleanup:
    return err;
}

tdn_err_t tdn_assembly_lookup_method(
    RuntimeAssembly assembly,
    int metadata_token,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    RuntimeMethodBase* method
) {
    tdn_err_t err = TDN_NO_ERROR;

    token_t token = { .token = metadata_token };
    switch (token.table) {
        case METADATA_METHOD_DEF: {
            CHECK(token.index != 0 && token.index <= assembly->MethodDefs->Length);
            *method = assembly->MethodDefs->Elements[token.index - 1];
        } break;

        case METADATA_MEMBER_REF: {
            CHECK(token.index != 0 && token.index <= assembly->Metadata->member_refs_count);
            metadata_member_ref_t* ref = &assembly->Metadata->member_refs[token.index - 1];

            // get the enclosing type
            RuntimeTypeInfo parent = NULL;
            CHECK_AND_RETHROW(tdn_assembly_lookup_type(assembly, ref->class.token, typeArgs, methodArgs, &parent));

            // get the expected field
            method_signature_t signature = {0};
            blob_entry_t blob = ref->signature;
            CHECK_AND_RETHROW(sig_parse_method_def(blob, assembly, parent->GenericArguments, NULL, &signature));

            // now search for a method with that signature
            bool found = false;
            int method_count = parent->DeclaredMethods->Length;
            int ctor_count = parent->DeclaredConstructors->Length;
            for (int i = 0; i < method_count + ctor_count; i++) {
                RuntimeMethodBase m = NULL;
                if (i >= ctor_count) {
                    m = (RuntimeMethodBase)parent->DeclaredMethods->Elements[i - ctor_count];
                } else {
                    m = (RuntimeMethodBase)parent->DeclaredConstructors->Elements[i];
                }

                if (!tdn_compare_string_to_cstr(m->Name, ref->name)) continue;
                if (m->Parameters->Length != signature.parameters->Length) continue;
                if (m->GenericArguments->Length != signature.generic_param_count) continue;
                if (m->ReturnParameter->ParameterType != signature.return_parameter->ParameterType) continue;

                // compare the parameter types
                bool valid = true;
                for (int pi = 0; pi < m->Parameters->Length; pi++) {
                    ParameterInfo p1 = m->Parameters->Elements[pi];
                    ParameterInfo p2 = signature.parameters->Elements[pi];
                    if (p1->ParameterType != p2->ParameterType) {
                        valid = false;
                        break;
                    }
                }
                if (!valid) continue;

                // found it!
                *method = m;
                found = true;
                break;
            }

            // we didn't find it
            CHECK(found);
        } break;

        default:
            CHECK_FAIL("tdn_assembly_lookup_method: called with invalid table %02x", token.table);
    }

cleanup:
    return err;
}

tdn_err_t tdn_assembly_lookup_field(
    RuntimeAssembly assembly,
    int metadata_token,
    RuntimeFieldInfo* field
) {
    tdn_err_t err = TDN_NO_ERROR;

//    token_t token = { .token = metadata_token };
//    CHECK(token.table == METADATA_FIELD, "tdn_assembly_lookup_field: called with invalid table %02x", token.table);
//    CHECK(token.index != 0 && token.index <= assembly->Fields->Length);

//    *field = assembly->Fields->Elements[token.index - 1];
    CHECK_FAIL();

cleanup:
    return err;
}

tdn_err_t tdn_assembly_lookup_type_by_cstr(RuntimeAssembly assembly, const char* namespace, const char* name, RuntimeTypeInfo* out_type) {
    tdn_err_t err = TDN_NO_ERROR;

    for (int i = 0; i < assembly->TypeDefs->Length; i++) {
        RuntimeTypeInfo type = assembly->TypeDefs->Elements[i];
        if (
            tdn_compare_string_to_cstr(type->Namespace, namespace) &&
            tdn_compare_string_to_cstr(type->Name, name)
        ) {
            *out_type = type;
            goto cleanup;
        }
    }

    CHECK_FAIL();

cleanup:
    return err;
}
