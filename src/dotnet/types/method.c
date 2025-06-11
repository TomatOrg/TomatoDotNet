
#include "tomatodotnet/except.h"
#include "tomatodotnet/types/basic.h"
#include "tomatodotnet/types/reflection.h"
#include "dotnet/metadata/metadata.h"
#include "util/except.h"
#include "util/stb_ds.h"
#include "dotnet/loader.h"
#include "dotnet/types.h"
#include "dotnet/metadata/sig.h"
#include "tomatodotnet/tdn.h"

static tdn_err_t create_generic_method(RuntimeMethodInfo base, RuntimeTypeInfo_Array args, RuntimeMethodInfo new_method) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeAssembly assembly = base->Module->Assembly;

    token_t token = (token_t){ .token = base->MetadataToken };
    CHECK(token.table == METADATA_METHOD_DEF);
    metadata_method_def_t* method_def = &base->Module->Assembly->Metadata->method_defs[token.index - 1];

    new_method->MetadataToken = base->MetadataToken;
    new_method->DeclaringType = base->DeclaringType;
    new_method->Module = base->Module;
    new_method->Name = base->Name;
    new_method->GenericMethodDefinition = base;
    new_method->GenericArguments = args;
    new_method->Attributes = base->Attributes;
    new_method->MethodImplFlags = base->MethodImplFlags;
    new_method->IsReadOnly = base->IsReadOnly;
    new_method->VTableOffset = VTABLE_INVALID;

    // if it has a body copy it over
    if (method_def->rva != 0) {
        CHECK_AND_RETHROW(tdn_parser_method_body(assembly, method_def, (RuntimeMethodBase)new_method));
    }

    // and finally get the signature
    method_signature_t signature = {};
    CHECK_AND_RETHROW(sig_parse_method_def(
            method_def->signature, assembly,
            new_method->DeclaringType->GenericArguments, new_method->GenericArguments,
            false,
            &signature));
    new_method->Parameters = signature.parameters;
    new_method->ReturnParameter = signature.return_parameter;

cleanup:
    return err;
}

tdn_err_t tdn_method_make_generic(
    RuntimeMethodInfo base,
    RuntimeTypeInfo_Array args,
    RuntimeMethodInfo* method
) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(base->GenericMethodDefinition == base);

    // check if already has one
    size_t hash = stbds_hash_bytes(args->Elements, sizeof(RuntimeTypeInfo) * args->Length, 0);
    int idx = hmgeti(base->GenericMethodInstances, hash);
    if (idx == -1) {
        // create it and set it incase we need it again
        // for expansion
        RuntimeMethodInfo new_method = TDN_GC_NEW(RuntimeMethodInfo);

        hmput(base->GenericMethodInstances, hash, new_method);

        // validate the make generic
        CHECK(base->GenericArguments->Length == args->Length);
        for (int i = 0; i < args->Length; i++) {
            CHECK_AND_RETHROW(tdn_check_generic_argument_constraints(
                    args->Elements[i],
                    base->GenericArguments->Elements[i]->GenericParameterAttributes,
                    base->GenericArguments->Elements[i]->GenericParameterConstraints,
                    base->DeclaringType->GenericArguments,
                    args));
        }

        // and now fill it up
        CHECK_AND_RETHROW(create_generic_method(base, args, new_method));

        // and out it goes
        *method = new_method;
    } else {
        // found it! check we don't have a collision by accident
        RuntimeMethodInfo instance = base->GenericMethodInstances[idx].value;
        for (int i = 0; i < args->Length; i++) {
            CHECK(instance->GenericArguments->Elements[i] == args->Elements[i]);
        }
        *method = instance;
    }

cleanup:
    return err;
}
