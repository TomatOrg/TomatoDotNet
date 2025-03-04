#include "tomatodotnet/types/type.h"
#include "dotnet/gc/gc.h"
#include "dotnet/loader.h"
#include "util/except.h"
#include "dotnet/metadata/metadata_tables.h"
#include "dotnet/types.h"
#include "util/stb_ds.h"
#include "dotnet/metadata/sig.h"
#include "dotnet/metadata/metadata.h"
#include <stdatomic.h>

bool tdn_type_is_valuetype(RuntimeTypeInfo type) {
    return type != NULL && (type->BaseType == tValueType || type->BaseType == tEnum);
}

bool tdn_type_is_referencetype(RuntimeTypeInfo type) {
    return type == NULL || (!tdn_type_is_valuetype(type) && !type->IsPointer && !type->IsByRef);
}

tdn_err_t tdn_get_array_type(RuntimeTypeInfo type, RuntimeTypeInfo* out_type) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(!type->IsByRef);
    CHECK(!type->IsByRefStruct);

    RuntimeTypeInfo new_type = type->ArrayType;
    if (new_type != NULL) {
        *out_type = new_type;
        goto cleanup;
    }

    //
    // create the array type
    //
    // for the metadata token we replicate what coreclr does
    // which is to set it to an invalid token into the TypeDef
    // table
    //
    new_type = GC_NEW(RuntimeTypeInfo);
    new_type->MetadataToken = (token_t){ .table = METADATA_TYPE_DEF }.token;
    new_type->DeclaringType = NULL;
    new_type->Module = type->Module;
    CHECK_AND_RETHROW(tdn_append_cstr_to_string(type->Name, "[]", &new_type->Name));
    new_type->Namespace = type->Namespace;
    new_type->BaseType = tArray;
    new_type->ElementType = type;
    new_type->DeclaredFields = tArray->DeclaredFields;
    new_type->DeclaredMethods = tArray->DeclaredMethods;
    new_type->DeclaredConstructors = tArray->DeclaredConstructors;
    new_type->Attributes = (TypeAttributes){
        .Visibility = TDN_TYPE_VISIBILITY_PUBLIC,
        .Sealed = true,
        .Serializable = true,
    };
    new_type->StackSize = sizeof(Array);
    new_type->StackAlignment = _Alignof(Array);
    new_type->HeapSize = sizeof(struct Array);
    new_type->HeapAlignment = _Alignof(struct Array);
    new_type->EndFillingHeapSize = 1;
    new_type->FillingHeapSize = 1;
    new_type->EndFillingStackSize = 1;
    new_type->FillingStackSize = 1;
    new_type->IsArray = 1;

    // TODO: handle the vtable correctly....
    new_type->JitVTable = tArray->JitVTable;

    // set the array type, because in the mean time someone could
    // have created the instance already, we are going to just let
    // the GC clean after ourselves and use the real one
    RuntimeTypeInfo result = NULL;
    if (atomic_compare_exchange_strong(&type->ArrayType, &result, new_type)) {
        result = new_type;
    } else {
        tdn_host_free_low(new_type->JitVTable);
    }

    *out_type = result;

cleanup:
    return err;
}

tdn_err_t tdn_get_byref_type(RuntimeTypeInfo type, RuntimeTypeInfo* out_type) {
    tdn_err_t err = TDN_NO_ERROR;

    // can't have a byref to a byref
    CHECK(!type->IsByRef);

    RuntimeTypeInfo new_type = type->ByRefType;
    if (new_type != NULL) {
        *out_type = new_type;
        goto cleanup;
    }


    //
    // create the byref type
    //
    // for the metadata token we replicate what coreclr does
    // which is to set it to an invalid token into the TypeDef
    // table
    //
    new_type = GC_NEW(RuntimeTypeInfo);
    new_type->MetadataToken = (token_t){ .table = METADATA_TYPE_DEF }.token;
    new_type->DeclaringType = NULL;
    new_type->Module = type->Module;
    CHECK_AND_RETHROW(tdn_append_cstr_to_string(type->Name, "&", &new_type->Name));
    new_type->Namespace = type->Namespace;
    new_type->BaseType = NULL;
    new_type->ElementType = type;
    new_type->DeclaredFields = NULL;
    new_type->DeclaredMethods = NULL;
    new_type->DeclaredConstructors = NULL;
    new_type->Attributes = (TypeAttributes){
        .Visibility = TDN_TYPE_VISIBILITY_PUBLIC,
    };
    new_type->StackSize = sizeof(void*);
    new_type->StackAlignment = _Alignof(void*);
    new_type->HeapSize = sizeof(void*);
    new_type->HeapAlignment = _Alignof(void*);
    new_type->FillingHeapSize = 1;
    new_type->EndFillingHeapSize = 1;
    new_type->FillingStackSize = 1;
    new_type->EndFillingStackSize = 1;
    new_type->IsByRef = 1;

    // set the array type, because in the mean time someone could
    // have created the instance already, we are going to just let
    // the GC clean after ourselves and use the real one
    RuntimeTypeInfo result = NULL;
    if (atomic_compare_exchange_strong(&type->ByRefType, &result, new_type)) {
        result = new_type;
    }

    *out_type = result;

cleanup:
    return err;
}

tdn_err_t tdn_get_pointer_type(RuntimeTypeInfo type, RuntimeTypeInfo* out_type) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeTypeInfo new_type = type->PointerType;
    if (new_type != NULL) {
        *out_type = new_type;
        goto cleanup;
    }

    //
    // create the byref type
    //
    // for the metadata token we replicate what coreclr does
    // which is to set it to an invalid token into the TypeDef
    // table
    //
    new_type = GC_NEW(RuntimeTypeInfo);
    new_type->MetadataToken = (token_t){ .table = METADATA_TYPE_DEF }.token;
    new_type->DeclaringType = NULL;
    new_type->Module = type->Module;
    CHECK_AND_RETHROW(tdn_append_cstr_to_string(type->Name, "*", &new_type->Name));
    new_type->Namespace = type->Namespace;
    new_type->BaseType = NULL;
    new_type->ElementType = type;
    new_type->DeclaredFields = NULL;
    new_type->DeclaredMethods = NULL;
    new_type->DeclaredConstructors = NULL;
    new_type->Attributes = (TypeAttributes){
        .Visibility = TDN_TYPE_VISIBILITY_PUBLIC,
    };
    new_type->StackSize = sizeof(void*);
    new_type->StackAlignment = _Alignof(void*);
    new_type->HeapSize = sizeof(void*);
    new_type->HeapAlignment = _Alignof(void*);
    new_type->FillingHeapSize = 1;
    new_type->EndFillingHeapSize = 1;
    new_type->FillingStackSize = 1;
    new_type->EndFillingStackSize = 1;
    new_type->IsPointer = 1;

    // set the array type, because in the mean time someone could
    // have created the instance already, we are going to just let
    // the GC clean after ourselves and use the real one
    RuntimeTypeInfo result = NULL;
    if (atomic_compare_exchange_strong(&type->PointerType, &result, new_type)) {
        result = new_type;
    }

    *out_type = result;

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Generic instance expansion
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

bool tdn_type_contains_generic_parameters(RuntimeTypeInfo type) {
    if (type->IsGenericParameter) {
        return true;
    } else if (type->ElementType != NULL) {
        return tdn_type_contains_generic_parameters(type);
    } else if (type->BaseType != NULL) {
        return tdn_type_contains_generic_parameters(type->BaseType);
    } else {
        return false;
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Expand TypeDef based types
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static bool is_module_type(RuntimeTypeInfo type) {
    return tdn_compare_string_to_cstr(type->Name, "<Module>") && type->Namespace == NULL;
}

static tdn_err_t expand_type_from_typedef(RuntimeTypeInfo type, RuntimeTypeInfo original_type) {
    tdn_err_t err = TDN_NO_ERROR;
    token_t token = { .token = type->MetadataToken };
    RuntimeAssembly assembly = type->Module->Assembly;
    CHECK(token.table == METADATA_TYPE_DEF);
    CHECK(token.index != 0);
    metadata_type_def_t* type_def = &assembly->Metadata->type_defs[token.index - 1];

    // resolve it
    if (type_def->extends.index != 0) {
        CHECK_AND_RETHROW(tdn_assembly_lookup_type(
                assembly, type_def->extends.token, type->GenericArguments, NULL, &type->BaseType));
    }

    // make sure the array is built correctly
    if (token.index != 1) {
        CHECK(type_def[-1].field_list.index <= type_def->field_list.index);
        CHECK(type_def[-1].method_list.index <= type_def->method_list.index);
    }

    // get the fields and methods count for later access
    size_t fields_count = (token.index == assembly->Metadata->type_defs_count ?
                           assembly->Metadata->fields_count :
                           type_def[1].field_list.index - 1) - (type_def->field_list.index - 1);
    size_t methods_count = (token.index == assembly->Metadata->type_defs_count ?
                            assembly->Metadata->method_defs_count :
                            type_def[1].method_list.index - 1) - (type_def->method_list.index - 1);

    // initialize all the fields, we just need the stack size from them for now
    type->DeclaredFields = GC_NEW_ARRAY(RuntimeFieldInfo, fields_count);
    for (int i = 0; i < fields_count; i++) {
        metadata_field_t* field = &assembly->Metadata->fields[type_def->field_list.index - 1 + i];
        RuntimeFieldInfo field_info = GC_NEW(RuntimeFieldInfo);
        field_info->DeclaringType = type;
        CHECK_AND_RETHROW(tdn_create_string_from_cstr(field->name, &field_info->Name));
        field_info->Attributes = (FieldAttributes){ .Attributes = field->flags };
        field_info->Module = type->Module;
        field_info->MetadataToken = ((token_t){ .table = METADATA_FIELD, .index = i + 1 }).token;
        CHECK_AND_RETHROW(sig_parse_field(field->signature, field_info));
        type->DeclaredFields->Elements[i] = field_info;
    }

    //
    // setup the methods
    //

    // count the ctors, we already verified the attributes properly
    int ctors = 0;
    int methods = 0;
    for (size_t i = 0; i < methods_count; i++) {
        metadata_method_def_t* method_def = &assembly->Metadata->method_defs[type_def->method_list.index - 1 + i];
        MethodAttributes attributes = { .Attributes = method_def->flags };
        if (attributes.RTSpecialName) {
            ctors++;
        } else {
            methods++;
        }
    }

    // now we can allocate and init all of them
    type->DeclaredConstructors = GC_NEW_ARRAY(RuntimeConstructorInfo, ctors);
    type->DeclaredMethods = GC_NEW_ARRAY(RuntimeMethodInfo, methods);
    ctors = 0;
    methods = 0;
    for (size_t i = 0; i < methods_count; i++) {
        metadata_method_def_t* method_def = &assembly->Metadata->method_defs[type_def->method_list.index - 1 + i];
        MethodAttributes attributes = { .Attributes = method_def->flags };
        RuntimeMethodBase original_method = assembly->MethodDefs->Elements[type_def->method_list.index - 1 + i];

        // get the correct version
        RuntimeMethodBase base = NULL;
        if (attributes.RTSpecialName) {
            base = (RuntimeMethodBase)GC_NEW(RuntimeConstructorInfo);
            type->DeclaredConstructors->Elements[ctors++] = (RuntimeConstructorInfo)base;

            // set this as the type initialized
            if (original_type->TypeInitializer == (RuntimeConstructorInfo)original_method) {
                type->TypeInitializer = (RuntimeConstructorInfo)base;
            }
        } else {
            base = (RuntimeMethodBase)GC_NEW(RuntimeMethodInfo);
            type->DeclaredMethods->Elements[methods++] = (RuntimeMethodInfo)base;
        }

        // setup most of the type
        base->MetadataToken = original_method->MetadataToken;
        base->DeclaringType = type;
        base->Module = type->Module;
        base->Name = original_method->Name;
        base->Attributes = original_method->Attributes;
        base->MethodImplFlags = original_method->MethodImplFlags;
        base->VTableOffset = VTABLE_INVALID;
        CHECK_AND_RETHROW(tdn_create_string_from_cstr(method_def->name, &base->Name));

        // if its generic setup the arguments and method definition
        if (original_method->GenericMethodDefinition != NULL) {
            base->GenericMethodDefinition = (RuntimeMethodInfo)base;
            base->GenericArguments = original_method->GenericArguments;
        }

        // if it has a body copy it over
        if (method_def->rva != 0) {
            CHECK_AND_RETHROW(tdn_parser_method_body(assembly, method_def, base));
        }

        // and finally get the signature
        method_signature_t signature = {};
        CHECK_AND_RETHROW(sig_parse_method_def(
                method_def->signature, assembly,
                type->GenericArguments, base->GenericArguments,
                false,
                &signature));
        base->Parameters = signature.parameters;
        base->ReturnParameter = signature.return_parameter;
    }

    // the nested types are just copied over without
    // actually applying the generic construction on
    // them
    type->DeclaredNestedTypes = original_type;

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Create generic type
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static tdn_err_t create_generic_type(RuntimeTypeInfo base, RuntimeTypeInfo_Array args, RuntimeTypeInfo new_type) {
    tdn_err_t err = TDN_NO_ERROR;

    // create the base type
    new_type->MetadataToken = base->MetadataToken;
    new_type->DeclaringType = base->DeclaringType;
    new_type->Module = base->Module;
    new_type->Namespace = base->Namespace;
    new_type->Name = base->Name;
    new_type->BaseType = NULL;
    new_type->Attributes = base->Attributes;
    new_type->GenericTypeDefinition = base;
    new_type->GenericArguments = args;
    new_type->IsReadOnly = base->IsReadOnly;
    new_type->IsByRefStruct = base->IsByRefStruct;

    // get all the other stuff
    token_t token = { .token = base->MetadataToken };
    CHECK(token.table == METADATA_TYPE_DEF && token.index != 0);
    CHECK_AND_RETHROW(expand_type_from_typedef(new_type, base));

cleanup:
    return err;
}

bool tdn_has_generic_parameters(RuntimeTypeInfo type) {
    if (type->IsGenericParameter) {
        return true;
    }

    if (type->IsArray || type->IsByRef || type->IsPointer) {
        return tdn_has_generic_parameters(type->ElementType);
    }

    if (type->GenericArguments != NULL) {
        for (int i = 0; i < type->GenericArguments->Length; i++) {
            if (tdn_has_generic_parameters(type->GenericArguments->Elements[i])) {
                return true;
            }
        }
    }

    return false;
}

tdn_err_t tdn_type_make_generic(RuntimeTypeInfo base, RuntimeTypeInfo_Array args, RuntimeTypeInfo* type) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(tdn_is_generic_type_definition(base));

    // check if already has one
    size_t hash = stbds_hash_bytes(args->Elements, sizeof(RuntimeTypeInfo) * args->Length, 0);
    int idx = hmgeti(base->GenericTypeInstances, hash);
    if (idx == -1) {
        // create it and set it incase we need it again
        // for expansion
        RuntimeTypeInfo new_type = GC_NEW(RuntimeTypeInfo);

        hmput(base->GenericTypeInstances, hash, new_type);

        CHECK_AND_RETHROW(create_generic_type(base, args, new_type));

        // check for generic arguments
        bool has_generic_params = false;
        for (int i = 0; i < args->Length; i++) {
            if (tdn_has_generic_parameters(args->Elements[i])) {
                has_generic_params = true;
                break;
            }
        }

        // if it does not then we can properly init the type instead
        // of keeping it uninitialized
        if (!has_generic_params) {
            CHECK_AND_RETHROW(tdn_type_init(new_type));
        }

        // and out it goes
        *type = new_type;
    } else {
        // found it! check we don't have a collision by accident
        RuntimeTypeInfo instance = base->GenericTypeInstances[idx].value;
        for (int i = 0; i < args->Length; i++) {
            CHECK(instance->GenericArguments->Elements[i] == args->Elements[i]);
        }
        *type = instance;
    }

cleanup:
    return err;
}

