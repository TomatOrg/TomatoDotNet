
#include "tinydotnet/types/type.h"
#include "dotnet/metadata/metadata_tables.h"
#include "util/except.h"
#include "dotnet/metadata/sig.h"

tdn_err_t tdn_assembly_lookup_type(
    System_Reflection_Assembly assembly,
    int metadata_token,
    System_Type_Array typeArgs, System_Type_Array methodArgs,
    System_Type* type
) {
    tdn_err_t err = TDN_NO_ERROR;

    token_t token = { .token = metadata_token };
    switch (token.table) {
        case METADATA_TYPE_DEF: {
            CHECK(token.index != 0 && token.index <= assembly->TypeDefs->Length);
            *type = assembly->TypeDefs->Elements[token.index - 1];
        } break;

        case METADATA_TYPE_SPEC: {
            CHECK(token.index != 0 && token.index <= assembly->TypeSpecs->Length);
            System_Byte_Array sig = assembly->TypeSpecs->Elements[token.index - 1].Signature;
            blob_entry_t entry = {
                .data = sig->Elements,
                .size = sig->Length
            };
            CHECK_AND_RETHROW(sig_parse_type_spec(entry, assembly, typeArgs, methodArgs, type));
        } break;

        default:
            CHECK_FAIL("tdn_assembly_lookup_type: called with invalid table %02x", token.table);
    }

cleanup:
    return err;
}

tdn_err_t tdn_assembly_lookup_method(
    System_Reflection_Assembly assembly,
    int metadata_token,
    System_Type_Array typeArgs, System_Type_Array methodArgs,
    System_Reflection_MethodInfo* method
) {
    tdn_err_t err = TDN_NO_ERROR;

    token_t token = { .token = metadata_token };
    switch (token.table) {
        case METADATA_METHOD_DEF: {
            CHECK(token.index != 0 && token.index <= assembly->MethodDefs->Length);
            *method = assembly->MethodDefs->Elements[token.index - 1];
        } break;

        case METADATA_MEMBER_REF: {
            CHECK(token.index != 0 && token.index <= assembly->MemberRefs->Length);
            TinyDotNet_Reflection_MemberReference ref = assembly->MemberRefs->Elements[token.index - 1];

            // get the enclosing type
            System_Type parent = NULL;
            CHECK_AND_RETHROW(tdn_assembly_lookup_type(assembly, ref.ParentToken, typeArgs, methodArgs, &parent));

            // get the expected field
            method_signature_t signature = {0};
            blob_entry_t blob = {
                .data = ref.Signature->Elements,
                .size = ref.Signature->Length
            };
            CHECK_AND_RETHROW(sig_parse_method_def(blob, assembly, parent->GenericArguments, NULL, &signature));

            // now search for a method with that signature
            for (int i = 0; i < parent->Methods->Length; i++) {
                System_Reflection_MethodInfo m = parent->Methods->Elements[i];

                // compare the generic and argument count
                if (m->GenericArguments->Length != signature.generic_param_count) continue;
                if (m->Parameters->Length != signature.parameters->Length) continue;

                // compare the return parameter
                if (m->ReturnParameter != signature.return_parameter) continue;

                // compare the parameter types
                for (int pi = 0; pi < m->Parameters->Length; pi++) {
                    System_Reflection_ParameterInfo p1 = m->Parameters->Elements[pi];
                    System_Reflection_ParameterInfo p2 = signature.parameters->Elements[pi];
                    if (p1->ParameterType != p2->ParameterType) goto next;
                }

                // found it!
                *method = m;

            next:
                continue;
            }

            // we didn't find it
            CHECK_FAIL();
        } break;

        // TODO: other options

        default:
            CHECK_FAIL("tdn_assembly_lookup_method: called with invalid table %02x", token.table);
    }

cleanup:
    return err;
}

tdn_err_t tdn_assembly_lookup_field(System_Reflection_Assembly assembly, int metadata_token, System_Reflection_FieldInfo* field) {
    tdn_err_t err = TDN_NO_ERROR;

    token_t token = { .token = metadata_token };
    CHECK(token.table == METADATA_FIELD, "tdn_assembly_lookup_field: called with invalid table %02x", token.table);
    CHECK(token.index != 0 && token.index <= assembly->Fields->Length);

    *field = assembly->Fields->Elements[token.index - 1];

cleanup:
    return err;
}

System_Type tdn_assembly_lookup_type_by_cstr(System_Reflection_Assembly assembly, const char* namespace, const char* name) {
    for (int i = 0; i < assembly->TypeDefs->Length; i++) {
        System_Type type = assembly->TypeDefs->Elements[i];
        if (
            tdn_compare_string_to_cstr(type->Namespace, namespace) &&
            tdn_compare_string_to_cstr(type->Name, name)
        ) {
            return type;
        }
    }

    return NULL;
}
