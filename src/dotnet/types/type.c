#include "tinydotnet/types/type.h"
#include "dotnet/gc/gc.h"
#include "dotnet/loader.h"
#include "util/except.h"
#include "dotnet/metadata/metadata_tables.h"
#include "dotnet/types.h"
#include "util/stb_ds.h"
#include <stdatomic.h>

bool tdn_type_is_valuetype(System_Type type) {
    return type == tSystem_ValueType ||
            type->BaseType == tSystem_ValueType ||
            type->BaseType == tSystem_Enum;
}

tdn_err_t tdn_get_array_type(System_Type type, System_Type* out_type) {
    tdn_err_t err = TDN_NO_ERROR;

    System_Type new_type = type->ArrayType;
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
    new_type = GC_NEW(System_Type);
    new_type->MetadataToken = (token_t){ .table = METADATA_TYPE_DEF }.token;
    new_type->DeclaringType = NULL;
    new_type->Module = type->Module;
    CHECK_AND_RETHROW(tdn_append_cstr_to_string(type->Name, "[]", &new_type->Name));
    new_type->Assembly = type->Assembly;
    new_type->Namespace = type->Namespace;
    new_type->BaseType = tSystem_Array;
    new_type->ElementType = type;
    new_type->Fields = tSystem_Array->Fields;
    new_type->Methods = tSystem_Array->Methods;
    new_type->Attributes = (System_Reflection_TypeAttributes){
        .Visibility = TDN_TYPE_VISIBILITY_PUBLIC,
        .Sealed = true,
        .Serializable = true,
    };
    new_type->StackSize = sizeof(System_Array);
    new_type->StackAlignment = _Alignof(System_Array);
    new_type->HeapSize = sizeof(struct System_Array);
    new_type->HeapAlignment = _Alignof(struct System_Array);
    new_type->StartedFillingSize = 1;
    new_type->FinishedFillingSize = 1;

    // set the array type, because in the mean time someone could
    // have created the instance already, we are going to just let
    // the GC clean after ourselves and use the real one
    System_Type result = NULL;
    if (atomic_compare_exchange_strong(&new_type->ArrayType, &result, new_type)) {
        result = new_type;
    }

    *out_type = result;

cleanup:
    return err;
}

tdn_err_t tdn_get_byref_type(System_Type type, System_Type* out_type) {
    tdn_err_t err = TDN_NO_ERROR;

    System_Type new_type = type->ByRefType;
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
    new_type = GC_NEW(System_Type);
    new_type->MetadataToken = (token_t){ .table = METADATA_TYPE_DEF }.token;
    new_type->DeclaringType = NULL;
    new_type->Module = type->Module;
    CHECK_AND_RETHROW(tdn_append_cstr_to_string(type->Name, "&", &new_type->Name));
    new_type->Assembly = type->Assembly;
    new_type->Namespace = type->Namespace;
    new_type->BaseType = NULL;
    new_type->ElementType = type;
    new_type->Fields = GC_NEW_ARRAY(System_Reflection_FieldInfo, 0);
    new_type->Methods = GC_NEW_ARRAY(System_Reflection_MethodInfo, 0);
    new_type->Attributes = (System_Reflection_TypeAttributes){
        .Visibility = TDN_TYPE_VISIBILITY_PUBLIC,
    };
    new_type->StackSize = sizeof(void*);
    new_type->StackAlignment = _Alignof(void*);
    new_type->HeapSize = sizeof(void*);
    new_type->HeapAlignment = _Alignof(void*);
    new_type->StartedFillingSize = 1;
    new_type->FinishedFillingSize = 1;

    // set the array type, because in the mean time someone could
    // have created the instance already, we are going to just let
    // the GC clean after ourselves and use the real one
    System_Type result = NULL;
    if (atomic_compare_exchange_strong(&new_type->ByRefType, &result, new_type)) {
        result = new_type;
    }

    *out_type = result;

cleanup:
    return err;
}

tdn_err_t tdn_get_pointer_type(System_Type type, System_Type* out_type) {
    tdn_err_t err = TDN_NO_ERROR;

    System_Type new_type = type->PointerType;
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
    new_type = GC_NEW(System_Type);
    new_type->MetadataToken = (token_t){ .table = METADATA_TYPE_DEF }.token;
    new_type->DeclaringType = NULL;
    new_type->Module = type->Module;
    CHECK_AND_RETHROW(tdn_append_cstr_to_string(type->Name, "*", &new_type->Name));
    new_type->Assembly = type->Assembly;
    new_type->Namespace = type->Namespace;
    new_type->BaseType = NULL;
    new_type->ElementType = type;
    new_type->Fields = GC_NEW_ARRAY(System_Reflection_FieldInfo, 0);
    new_type->Methods = GC_NEW_ARRAY(System_Reflection_MethodInfo, 0);
    new_type->Attributes = (System_Reflection_TypeAttributes){
        .Visibility = TDN_TYPE_VISIBILITY_PUBLIC,
    };
    new_type->StackSize = sizeof(void*);
    new_type->StackAlignment = _Alignof(void*);
    new_type->HeapSize = sizeof(void*);
    new_type->HeapAlignment = _Alignof(void*);
    new_type->StartedFillingSize = 1;
    new_type->FinishedFillingSize = 1;

    // set the array type, because in the mean time someone could
    // have created the instance already, we are going to just let
    // the GC clean after ourselves and use the real one
    System_Type result = NULL;
    if (atomic_compare_exchange_strong(&new_type->PointerType, &result, new_type)) {
        result = new_type;
    }

    *out_type = result;

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Generic instance expansion
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static tdn_err_t create_generic_type(System_Type base, System_Type_Array args, System_Type new_type) {
    tdn_err_t err = TDN_NO_ERROR;

    // create the base type
    new_type->MetadataToken = base->MetadataToken;
    new_type->DeclaringType = NULL;
    new_type->Module = base->Module;
    new_type->Assembly = base->Assembly;
    new_type->Namespace = base->Namespace;
    new_type->Name = base->Name;
    new_type->BaseType = NULL;
    new_type->Attributes = base->Attributes;
    new_type->GenericTypeDefinition = base;
    new_type->GenericArguments = args;

    // TODO: expand the fields

    // TODO: expand the methods

//    CHECK_AND_RETHROW(tdn_fill_size(new_type));

cleanup:
    return err;
}

tdn_err_t tdn_type_make_generic(System_Type base, System_Type_Array args, System_Type* type) {
    tdn_err_t err = TDN_NO_ERROR;

    // check if already has one
    size_t hash = stbds_hash_bytes(args->Elements, sizeof(System_Type) * args->Length, 0);
    int idx = hmgeti(base->GenericTypeInstances, hash);
    if (idx == -1) {
        // create it and set it incase we need it again
        // for expansion
        System_Type new_type = GC_NEW(System_Type);
        hmput(base->GenericTypeInstances, hash, new_type);

        // and now fill it up
        CHECK_AND_RETHROW(create_generic_type(base, args, new_type));

        // and out it goes
        *type = new_type;
    } else {
        // found it! check we don't have a collision by accident
        System_Type instance = base->GenericTypeInstances[idx].value;
        for (int i = 0; i < args->Length; i++) {
            CHECK(instance->GenericArguments->Elements[i] == args->Elements[i]);
        }
        *type = instance;
    }

cleanup:
    return err;
}

