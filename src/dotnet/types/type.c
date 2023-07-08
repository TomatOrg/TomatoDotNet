#include "tinydotnet/types/type.h"
#include "dotnet/gc/gc.h"
#include "dotnet/loader.h"
#include "util/except.h"
#include "dotnet/metadata/metadata_tables.h"
#include "dotnet/types.h"
#include "util/stb_ds.h"
#include <stdatomic.h>

bool tdn_type_is_valuetype(RuntimeTypeInfo type) {
    return type == tValueType ||
            type->BaseType == tValueType ||
            type->BaseType == tEnum;
}

tdn_err_t tdn_get_array_type(RuntimeTypeInfo type, RuntimeTypeInfo* out_type) {
    tdn_err_t err = TDN_NO_ERROR;

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

    // set the array type, because in the mean time someone could
    // have created the instance already, we are going to just let
    // the GC clean after ourselves and use the real one
    RuntimeTypeInfo result = NULL;
    if (atomic_compare_exchange_strong(&new_type->ArrayType, &result, new_type)) {
        result = new_type;
    }

    *out_type = result;

cleanup:
    return err;
}

tdn_err_t tdn_get_byref_type(RuntimeTypeInfo type, RuntimeTypeInfo* out_type) {
    tdn_err_t err = TDN_NO_ERROR;

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

    // set the array type, because in the mean time someone could
    // have created the instance already, we are going to just let
    // the GC clean after ourselves and use the real one
    RuntimeTypeInfo result = NULL;
    if (atomic_compare_exchange_strong(&new_type->ByRefType, &result, new_type)) {
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

    // set the array type, because in the mean time someone could
    // have created the instance already, we are going to just let
    // the GC clean after ourselves and use the real one
    RuntimeTypeInfo result = NULL;
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

//static void type_replace_generic_parameter(RuntimeTypeInfo type, RuntimeTypeInfo_Array args) {
//
//}
//
//static tdn_err_t expand_generic_type(RuntimeTypeInfo base, RuntimeTypeInfo_Array args, RuntimeTypeInfo* new_type) {
//    tdn_err_t err = TDN_NO_ERROR;
//
//    // make sure it is actually a generic type
//    if (!tdn_type_contains_generic_parameters(base)) {
//        *new_type = base;
//        goto cleanup;
//    }
//
//    // it is, create it
//    CHECK_AND_RETHROW(tdn_type_make_generic(base, args, new_type));
//
//cleanup:
//    return err;
//}
//
//static tdn_err_t expand_generic_parameter(System_Reflection_ParameterInfo base, RuntimeTypeInfo_Array args, System_Reflection_ParameterInfo* out_parameter) {
//    tdn_err_t err = TDN_NO_ERROR;
//
//    System_Reflection_ParameterInfo new = GC_NEW(System_Reflection_ParameterInfo);
//    new->Name = base->Name;
//    CHECK_AND_RETHROW(expand_generic_type(base->ParameterType, args, &new->ParameterType));
//    new->Position = base->Position;
//    new->MetadataToken = base->MetadataToken;
//    new->Attributes = base->Attributes;
//
//cleanup:
//    return err;
//}
//
//static tdn_err_t expand_generic_method(System_Reflection_MethodInfo base, RuntimeTypeInfo_Array args, System_Reflection_MethodInfo* out_method) {
//    tdn_err_t err = TDN_NO_ERROR;
//
//    // the basics
//    System_Reflection_MethodInfo new_method = GC_NEW(System_Reflection_MethodInfo);
//    new_method->Name = base->Name;
//    new_method->ImplementationFlags = base->ImplementationFlags;
//    new_method->Attributes = base->Attributes;
//
//    // TODO: expand the body
//
//    // TODO: how do generic methods play out with this
//
//    // expand the return parameter
//    CHECK_AND_RETHROW(expand_generic_parameter(base->ReturnParameter, args, &new_method->ReturnParameter));
//    new_method->ReturnParameter->Member = (System_Reflection_MemberInfo)new_method;
//
//    // expand method parameters
//    new_method->Parameters = GC_NEW_ARRAY(System_Reflection_ParameterInfo, base->Parameters->Length);
//    for (int i = 0; i < base->Parameters->Length; i++) {
//        CHECK_AND_RETHROW(expand_generic_parameter(base->Parameters->Elements[i], args, &new_method->Parameters->Elements[i]));
//        new_method->Parameters->Elements[i]->Member = (System_Reflection_MemberInfo)new_method;
//    }
//
//cleanup:
//    return err;
//}

static tdn_err_t create_generic_type(RuntimeTypeInfo base, RuntimeTypeInfo_Array args, RuntimeTypeInfo new_type) {
    tdn_err_t err = TDN_NO_ERROR;

    // create the base type
    new_type->MetadataToken = base->MetadataToken;
    new_type->DeclaringType = NULL;
    new_type->Module = base->Module;
    new_type->Namespace = base->Namespace;
    new_type->Name = base->Name;
    new_type->BaseType = NULL;
    new_type->Attributes = base->Attributes;
    new_type->GenericTypeDefinition = base;
    new_type->GenericArguments = args;

//    // expand the base
//    if (base->BaseType != NULL) {
//        CHECK_AND_RETHROW(expand_generic_type(base->BaseType, args, &new_type->BaseType));
//    }
//
//    // TODO: expand the fields
//
//    for (int i = 0; i < ) {
//
//    }

    CHECK_FAIL();

cleanup:
    return err;
}

tdn_err_t tdn_type_make_generic(RuntimeTypeInfo base, RuntimeTypeInfo_Array args, RuntimeTypeInfo* type) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(tdn_is_generic_type_definition(base));

    // check if already has one
    size_t hash = stbds_hash_bytes(args->Elements, sizeof(RuntimeTypeInfo) * args->Length, 0);
    CHECK_FAIL();
//    int idx = hmgeti(base->GenericTypeInstances, hash);
//    if (idx == -1) {
//        // create it and set it incase we need it again
//        // for expansion
//        RuntimeTypeInfo new_type = GC_NEW(RuntimeTypeInfo);
//
//        // TODO: validate the arguments are valid
//        hmput(base->GenericTypeInstances, hash, new_type);
//
//        // and now fill it up
//        CHECK_AND_RETHROW(create_generic_type(base, args, new_type));
//
//        // and out it goes
//        *type = new_type;
//    } else {
//        // found it! check we don't have a collision by accident
//        RuntimeTypeInfo instance = base->GenericTypeInstances[idx].value;
//        for (int i = 0; i < args->Length; i++) {
//            CHECK(instance->GenericArguments->Elements[i] == args->Elements[i]);
//        }
//        *type = instance;
//    }

cleanup:
    return err;
}

