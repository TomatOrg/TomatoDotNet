#include "types.h"
#include "opcodes.h"
#include "monitor.h"

#include "gc/gc.h"
#include "dotnet/metadata/sig.h"
#include "encoding.h"
#include "loader.h"

#include <util/strbuilder.h>
#include <util/stb_ds.h>

#include <stdalign.h>
#include <stdlib.h>
#include <string.h>

System_Type tSystem_Enum = NULL;
System_Type tSystem_Exception = NULL;
System_Type tSystem_ValueType = NULL;
System_Type tSystem_Object = NULL;
System_Type tSystem_Type = NULL;
System_Type tSystem_Array = NULL;
System_Type tSystem_String = NULL;
System_Type tSystem_Boolean = NULL;
System_Type tSystem_Char = NULL;
System_Type tSystem_SByte = NULL;
System_Type tSystem_Byte = NULL;
System_Type tSystem_Int16 = NULL;
System_Type tSystem_UInt16 = NULL;
System_Type tSystem_Int32 = NULL;
System_Type tSystem_UInt32 = NULL;
System_Type tSystem_Int64 = NULL;
System_Type tSystem_UInt64 = NULL;
System_Type tSystem_Single = NULL;
System_Type tSystem_Double = NULL;
System_Type tSystem_IntPtr = NULL;
System_Type tSystem_UIntPtr = NULL;
System_Type tSystem_Reflection_Module = NULL;
System_Type tSystem_Reflection_Assembly = NULL;
System_Type tSystem_Reflection_FieldInfo = NULL;
System_Type tSystem_Reflection_MemberInfo = NULL;
System_Type tSystem_Reflection_ParameterInfo = NULL;
System_Type tSystem_Reflection_LocalVariableInfo = NULL;
System_Type tSystem_Reflection_ExceptionHandlingClause = NULL;
System_Type tSystem_Reflection_MethodBase = NULL;
System_Type tSystem_Reflection_MethodBody = NULL;
System_Type tSystem_Reflection_MethodInfo = NULL;
System_Type tSystem_ArithmeticException = NULL;
System_Type tSystem_DivideByZeroException = NULL;
System_Type tSystem_ExecutionEngineException = NULL;
System_Type tSystem_IndexOutOfRangeException = NULL;
System_Type tSystem_NullReferenceException = NULL;
System_Type tSystem_InvalidCastException = NULL;
System_Type tSystem_OutOfMemoryException = NULL;
System_Type tSystem_OverflowException = NULL;

System_Type tTinyDotNet_Reflection_InterfaceImpl = NULL;
System_Type tTinyDotNet_Reflection_MemberReference = NULL;
System_Type tTinyDotNet_Reflection_MethodImpl = NULL;
System_Type tTinyDotNet_Reflection_MethodSpec = NULL;

bool string_equals_cstr(System_String a, const char* b) {
    if (a->Length != strlen(b)) {
        return false;
    }

    for (int i = 0; i < a->Length; i++) {
        if (a->Chars[i] != b[i]) {
            return false;
        }
    }

    return true;
}

bool string_equals(System_String a, System_String b) {
    if (a == b) {
        return true;
    }

    if (a->Length != b->Length) {
        return false;
    }

    for (int i = 0; i < a->Length; i++) {
        if (a->Chars[i] != b->Chars[i]) {
            return false;
        }
    }

    return true;
}

System_String string_append_cstr(System_String old, const char* str) {
    size_t len = strlen(str);

    // copy the old chars
    System_String new = GC_NEW_STRING(old->Length + len);
    memcpy(new->Chars, old->Chars, sizeof(System_Char) * old->Length);

    // copy the new chars
    for (int i = 0; i < len; i++) {
        new->Chars[old->Length + i] = str[i];
    }

    return new;
}

err_t assembly_get_type_by_token(System_Reflection_Assembly assembly, token_t token, System_Type_Array typeArgs, System_Type_Array methodArgs, System_Type* out_type) {
    err_t err = NO_ERROR;
    System_Type type = NULL;

    if (token.index != 0) {
        switch (token.table) {
            case METADATA_TYPE_DEF: {
                CHECK(token.index - 1 < assembly->DefinedTypes->Length);
                type = assembly->DefinedTypes->Data[token.index - 1];
            } break;

            case METADATA_TYPE_REF: {
                CHECK(token.index - 1 < assembly->ImportedTypes->Length);
                type = assembly->ImportedTypes->Data[token.index - 1];
            } break;

            case METADATA_TYPE_SPEC: {
                CHECK(token.index - 1 < assembly->DefinedTypeSpecs->Length);

                // not found, so parse it
                System_Byte_Array blob = assembly->DefinedTypeSpecs->Data[token.index - 1];
                blob_entry_t entry = {
                    .data = blob->Data,
                    .size = blob->Length
                };
                CHECK_AND_RETHROW(parse_type_spec(entry, assembly, &type, typeArgs, methodArgs));
            } break;

            default:
                CHECK_FAIL("Invalid table for type %04x");
                break;
        }
    }

    *out_type = type;

cleanup:
    return err;
}

static bool match_generic_type(System_Type a, System_Type b) {
    if (a == b) {
        return true;
    } else if (a->IsArray && b->IsArray) {
        return match_generic_type(a->ElementType, b->ElementType);
    } else if (a->IsByRef && b->IsByRef) {
        return match_generic_type(a->BaseType, b->BaseType);
    } else if (a->GenericParameterPosition >= 0 && b->GenericParameterPosition >= 0) {
        return a->GenericParameterPosition == b->GenericParameterPosition;
    } else if (a->GenericArguments != NULL && b->GenericArguments != NULL) {
        if (a->GenericArguments->Length != b->GenericArguments->Length) {
            return false;
        }
        for (int i = 0; i < a->GenericArguments->Length; i++) {
            if (!match_generic_type(a->GenericArguments->Data[i], b->GenericArguments->Data[i])) {
                return false;
            }
        }
        return true;
    } else {
        return false;
    }
}

err_t assembly_get_method_by_token(System_Reflection_Assembly assembly, token_t token, System_Type_Array typeArgs, System_Type_Array methodArgs, System_Reflection_MethodInfo* out_method) {
    err_t err = NO_ERROR;

    if (token.index == 0) {
        // null token is valid for our case
        *out_method = NULL;
        goto cleanup;
    }

    switch (token.table) {
        case METADATA_METHOD_DEF: {
            CHECK(token.index - 1 < assembly->DefinedMethods->Length);
            System_Reflection_MethodInfo method = assembly->DefinedMethods->Data[token.index - 1];
            if (method->GenericArguments != NULL) {
                CHECK_FAIL("TODO: instantiate generic method");
            }
            *out_method = method;
        } break;

        case METADATA_METHOD_SPEC: {
            CHECK(token.index - 1 < assembly->DefinedMethodSpecs->Length);

            // not found, so parse it
            TinyDotNet_Reflection_MethodSpec spec = assembly->DefinedMethodSpecs->Data[token.index - 1];

            // create it
            *out_method = spec->Method;
            blob_entry_t entry = {
                .data = spec->Instantiation->Data,
                .size = spec->Instantiation->Length
            };
            CHECK_AND_RETHROW(parse_method_spec(entry, assembly, out_method, typeArgs, methodArgs));
        } break;

        case METADATA_MEMBER_REF: {
            CHECK(token.index - 1 < assembly->DefinedMemberRefs->Length);
            TinyDotNet_Reflection_MemberReference ref = assembly->DefinedMemberRefs->Data[token.index - 1];

            // get the enclosing type
            System_Type type;
            CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, ref->Class, typeArgs, methodArgs, &type));

            // get the expected field
            System_Reflection_MethodInfo wantedInfo;
            blob_entry_t blob = {
                .data = ref->Signature->Data,
                .size = ref->Signature->Length
            };
            CHECK_AND_RETHROW(parse_method_ref_sig(blob, assembly, &wantedInfo, type->GenericArguments, NULL));

            // if this is null it means that this is a generic definition that is not complete, so we are
            // going to look at the declaration instead
            if (type->Methods == NULL) {
                type = type->GenericTypeDefinition;
            }

            // find a method with that type
            int index = 0;
            System_Reflection_MethodInfo methodInfo = NULL;
            while ((methodInfo = type_iterate_methods(type, ref->Name, &index)) != NULL) {
                // make sure both either have or don't have this
                if (method_is_static(methodInfo) != method_is_static(wantedInfo)) continue;

                // check the return type and parameters count is the same
                if (!match_generic_type(methodInfo->ReturnType, wantedInfo->ReturnType)) continue;
                if (methodInfo->Parameters->Length != wantedInfo->Parameters->Length) continue;

                // check that the parameters are the same
                bool found = true;
                for (int pi = 0; pi < methodInfo->Parameters->Length; pi++) {
                    if (!match_generic_type(methodInfo->Parameters->Data[pi]->ParameterType, wantedInfo->Parameters->Data[pi]->ParameterType)) {
                        found = false;
                        break;
                    }
                }

                if (found) {
                    break;
                }
            }

            // found it
            CHECK(methodInfo != NULL);
            *out_method = methodInfo;
        } break;

        default:
            CHECK_FAIL("Invalid token %02x.%d", token.table, token.index);
            break;
    }

cleanup:
    return err;
}

err_t assembly_get_field_by_token(System_Reflection_Assembly assembly, token_t token, System_Type_Array typeArgs, System_Type_Array methodArgs, System_Reflection_FieldInfo* out_field) {
    err_t err = NO_ERROR;

    if (token.index == 0) {
        // null token is valid for our case
        *out_field = NULL;
        goto cleanup;
    }

    switch (token.table) {
        case METADATA_FIELD: {
            CHECK(token.index - 1 < assembly->DefinedFields->Length);
            *out_field = assembly->DefinedFields->Data[token.index - 1];
        } break;

        case METADATA_MEMBER_REF: {
            CHECK(token.index - 1 < assembly->DefinedMemberRefs->Length);
            TinyDotNet_Reflection_MemberReference ref = assembly->DefinedMemberRefs->Data[token.index - 1];

            // get the enclosing type
            System_Type type;
            CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, ref->Class, typeArgs, methodArgs, &type));

            // get the expected field
            System_Reflection_FieldInfo wantedInfo = GC_NEW(tSystem_Reflection_FieldInfo);
            blob_entry_t blob = {
                .data = ref->Signature->Data,
                .size = ref->Signature->Length
            };
            GC_UPDATE(wantedInfo, DeclaringType, type);
            GC_UPDATE(wantedInfo, Module, assembly->Module);
            CHECK_AND_RETHROW(parse_field_sig(blob, wantedInfo, NULL, NULL));

            // get the actual field to verify compatibility
            System_Reflection_FieldInfo fieldInfo = type_get_field(type, ref->Name);

            // check we got what we wanted
            CHECK(fieldInfo->FieldType == wantedInfo->FieldType);

            // out it
            *out_field = fieldInfo;
        } break;

        default:
            CHECK_FAIL("Invalid table %02x", token.table);
            break;
    }

cleanup:
    return err;
}

System_Type assembly_get_type_by_name(System_Reflection_Assembly assembly, const char* name, const char* namespace) {
    for (int i = 0; i < assembly->DefinedTypes->Length; i++) {
        System_Type type = assembly->DefinedTypes->Data[i];
        if (string_equals_cstr(type->Namespace, namespace) && string_equals_cstr(type->Name, name)) {
            return type;
        }
    }
    return NULL;
}

System_String assembly_get_string_by_token(System_Reflection_Assembly assembly, token_t token) {
    if (token.table != 0x70) {
        ASSERT(!"assembly_get_string_by_token: invalid table for type");
        return NULL;
    }
    return hmget(assembly->UserStringsTable, token.index);
}

System_Type get_array_type(System_Type type) {
    if (type->ArrayType != NULL) {
        return type->ArrayType;
    }

    monitor_enter(type);

    if (type->ArrayType != NULL) {
        monitor_exit(type);
        return type->ArrayType;
    }

    // allocate the new type
    System_Type ArrayType = GC_NEW(tSystem_Type);

    // make sure this was called after system array was initialized
    ASSERT(tSystem_Array->Assembly != NULL);

    // set the type information to look as type[]
    GC_UPDATE(ArrayType, Module, type->Module);
    GC_UPDATE(ArrayType, Name, string_append_cstr(type->Name, "[]"));
    GC_UPDATE(ArrayType, Assembly, type->Assembly);
    GC_UPDATE(ArrayType, BaseType, tSystem_Array);
    GC_UPDATE(ArrayType, Namespace, type->Namespace);

    // this is an array
    ArrayType->IsArray = true;
    ArrayType->IsFilled = true;
    ArrayType->GenericParameterPosition = -1;
    ArrayType->StackType = STACK_TYPE_O;

    // set the sizes properly
    ArrayType->StackSize = tSystem_Array->StackSize;
    ArrayType->ManagedSize = tSystem_Array->ManagedSize;
    ArrayType->StackAlignment = tSystem_Array->StackAlignment;
    ArrayType->ManagedAlignment = tSystem_Array->ManagedAlignment;

    // allocate the vtable
    ArrayType->VTable = malloc(sizeof(object_vtable_t) + sizeof(void*) * 3);
    ArrayType->VTable->type = ArrayType;

    // There are no managed pointers in here (The gc will handle array
    // stuff on its own)
    ArrayType->ManagedPointersOffsets = NULL;

    // Set the element type
    GC_UPDATE(ArrayType, ElementType, type);

    // Set the array type
    GC_UPDATE(type, ArrayType, ArrayType);
    monitor_exit(type);

    return type->ArrayType;
}

System_Type get_by_ref_type(System_Type type) {
    if (type->ByRefType != NULL) {
        return type->ByRefType;
    }

    monitor_enter(type);

    if (type->ByRefType != NULL) {
        monitor_exit(type);
        return type->ByRefType;
    }

    // must not be a byref
    ASSERT(!type->IsByRef);

    // allocate the new ref type
    System_Type ByRefType = GC_NEW(tSystem_Type);

    // this is an array
    ByRefType->IsByRef = 1;
    ByRefType->IsFilled = 1;
    ByRefType->StackType = STACK_TYPE_REF;
    ByRefType->GenericParameterPosition = -1;

    // set the type information to look as ref type
    GC_UPDATE(ByRefType, Module, type->Module);
    GC_UPDATE(ByRefType, Name, string_append_cstr(type->Name, "&"));
    GC_UPDATE(ByRefType, Assembly, type->Assembly);
    GC_UPDATE(ByRefType, Namespace, type->Namespace);
    GC_UPDATE(ByRefType, BaseType, type);

    // set the sizes properly
    ByRefType->StackSize = sizeof(void*);
    ByRefType->ManagedSize = type->StackSize;
    ByRefType->StackAlignment = alignof(void*);
    ByRefType->ManagedAlignment = type->StackAlignment;

    // Set the array type
    GC_UPDATE(type, ByRefType, ByRefType);
    monitor_exit(type);

    return type->ByRefType;
}

System_Type get_boxed_type(System_Type type) {
    if (type->BoxedType != NULL) {
        return type->BoxedType;
    }

    monitor_enter(type);

    if (type->BoxedType != NULL) {
        monitor_exit(type);
        return type->BoxedType;
    }

    // must not be a byref
    ASSERT(!type->BoxedType);

    // allocate the new ref type
    System_Type BoxedType = GC_NEW(tSystem_Type);
    GC_UPDATE(BoxedType, DeclaringType, type->DeclaringType);
    GC_UPDATE(BoxedType, Module, type->Module);
    GC_UPDATE(BoxedType, Name, type->Name);
    GC_UPDATE(BoxedType, Assembly, type->Assembly);
    GC_UPDATE(BoxedType, BaseType, type->BaseType);
    GC_UPDATE(BoxedType, Namespace, type->Namespace);
    GC_UPDATE(BoxedType, Fields, type->Fields);
    GC_UPDATE(BoxedType, Methods, type->Methods);
    BoxedType->ManagedSize = type->ManagedSize;
    BoxedType->ManagedAlignment = type->ManagedAlignment;
    BoxedType->StackSize = tSystem_Object->StackSize;
    BoxedType->StackAlignment = tSystem_Object->StackAlignment;
    BoxedType->StackType = STACK_TYPE_O;
    GC_UPDATE(BoxedType, InterfaceImpls, type->InterfaceImpls);

    // Set the array type
    GC_UPDATE(type, BoxedType, BoxedType);
    monitor_exit(type);

    return type->BoxedType;
}

const char* method_access_str(method_access_t access) {
    static const char* strs[] = {
        [METHOD_COMPILER_CONTROLLED] = "compilercontrolled",
        [METHOD_PRIVATE] = "private",
        [METHOD_FAMILY_AND_ASSEMBLY] = "private protected",
        [METHOD_ASSEMBLY] = "internal",
        [METHOD_FAMILY] = "protected",
        [METHOD_FAMILY_OR_ASSEMBLY] = "protected internal",
        [METHOD_PUBLIC] = "public",
    };
    return strs[access];
}

const char* field_access_str(field_access_t access) {
    static const char* strs[] = {
        [FIELD_COMPILER_CONTROLLED] = "compilercontrolled",
        [FIELD_PRIVATE] = "private",
        [FIELD_FAMILY_AND_ASSEMBLY] = "private protected",
        [FIELD_ASSEMBLY] = "internal",
        [FIELD_FAMILY] = "protected",
        [FIELD_FAMILY_OR_ASSEMBLY] = "protected internal",
        [FIELD_PUBLIC] = "public",
    };
    return strs[access];
}

const char* type_visibility_str(type_visibility_t visibility) {
    static const char* strs[] = {
        [TYPE_NOT_PUBLIC] = "private",
        [TYPE_PUBLIC] = "public",
        [TYPE_NESTED_PUBLIC] = "nested public",
        [TYPE_NESTED_PRIVATE] = "nested private",
        [TYPE_NESTED_FAMILY] = "protected",
        [TYPE_NESTED_ASSEMBLY] = "internal",
        [TYPE_NESTED_FAMILY_AND_ASSEMBLY] = "private protected",
        [TYPE_NESTED_FAMILY_OR_ASSEMBLY] = "protected internal",
    };
    return strs[visibility];
}


static bool type_is_integer(System_Type type) {
    return type == tSystem_Byte || type == tSystem_Int16 || type == tSystem_Int32 || type == tSystem_Int64 ||
           type == tSystem_SByte || type == tSystem_UInt16 || type == tSystem_UInt32 || type == tSystem_UInt64 ||
           type == tSystem_UIntPtr || type == tSystem_IntPtr || type == tSystem_Char || type == tSystem_Boolean;
}

System_Type type_get_underlying_type(System_Type T) {
    if (type_is_enum(T)) {
        return T->ElementType;
    } else {
        return T;
    }
}

static System_Type type_get_reduced_type(System_Type T) {
    T = type_get_underlying_type(T);
    if (T == tSystem_Byte) {
        return tSystem_SByte;
    } else if (T == tSystem_UInt16) {
        return tSystem_Int16;
    } else if (T == tSystem_UInt32) {
        return tSystem_Int32;
    } else if (T == tSystem_UInt64) {
        return tSystem_Int64;
    } else if (T == tSystem_UIntPtr) {
        return tSystem_IntPtr;
    } else {
        return T;
    }
}

System_Type type_get_verification_type(System_Type T) {
    T = type_get_reduced_type(T);
    if (T == tSystem_Boolean) {
        return tSystem_SByte;
    } else if (T == tSystem_Char) {
        return tSystem_Int16;
    } else if (T != NULL && T->IsByRef) {
        return get_by_ref_type(type_get_verification_type(T->BaseType));
    } else {
        return T;
    }
}

System_Type type_get_intermediate_type(System_Type T) {
    T = type_get_verification_type(T);
    if (T == tSystem_SByte || T == tSystem_Int16) {
        return tSystem_Int32;
    } else {
        return T;
    }
}

bool type_is_array_element_compatible_with(System_Type T, System_Type U) {
    System_Type V = type_get_underlying_type(T);
    System_Type W = type_get_underlying_type(U);

    if (type_is_compatible_with(V, W)) {
        return true;

    } else if (type_get_verification_type(V) == type_get_verification_type(W)) {
        // spec says it should be reduced-type, but then bool and int8 are not the same
        // and there is valid code where this happens...
        return true;
    } else {
        return false;
    }
}

bool type_is_pointer_element_compatible_with(System_Type T, System_Type U) {
    System_Type V = type_get_verification_type(T);
    System_Type W = type_get_verification_type(U);
    return V == W;
}

static System_Type type_get_direct_base_class(System_Type T) {
    if (T != NULL && T->IsArray) {
        return tSystem_Array;
    } else if (type_is_object_ref(T) || (T != NULL && type_is_interface(T))) {
        return tSystem_Object;
    } else if (T != NULL && T->IsValueType) {
        return tSystem_ValueType;
    } else {
        return NULL;
    }
}

static bool type_is_interface_directly_implemented_by(System_Type I, System_Type T) {
    if (!type_is_interface(I)) {
        return false;
    }

    if (T->InterfaceImpls == NULL) {
        return false;
    }

    for (int i = 0; i < T->InterfaceImpls->Length; i++) {
        if (T->InterfaceImpls->Data[i]->InterfaceType == I) {
            return true;
        }
    }

    return false;
}

bool type_is_compatible_with(System_Type T, System_Type U) {
    // T is identical to U.
    if (T == U) {
        return true;
    }

    // doesn't make sense to have a null type in here
    if (T == NULL || U == NULL) {
        return false;
    }

    if (type_is_object_ref(T)) {
        if (U == type_get_direct_base_class(T)) {
            return true;
        }

        if (type_is_interface_directly_implemented_by(U, T)) {
            return true;
        }
    }

    if (type_is_object_ref(T)) {
        System_Type Base = T->BaseType;
        while (Base != NULL) {
            if (Base == U) {
                return true;
            }
            Base = Base->BaseType;
        }
    }

    if (T->IsArray && U->IsArray && type_is_array_element_compatible_with(T->ElementType, U->ElementType)) {
        return true;
    }

    if (T->IsByRef && U->IsByRef) {
        if (type_is_pointer_element_compatible_with(T, U)) {
            return true;
        }
    }

    return false;
}

static bool type_is_assignable_to(System_Type T, System_Type U) {
    if (T == U) {
        return true;
    }

    System_Type V = type_get_intermediate_type(T);
    System_Type W = type_get_intermediate_type(U);

    if (V == W) {
        return true;
    }

    // TODO: This rule seems really wtf
//    if (
//        (V == tSystem_IntPtr && W == tSystem_Int32) ||
//        (V == tSystem_Int32 && W == tSystem_IntPtr)
//    ) {
//        return true;
//    }

    if (type_is_compatible_with(T, U)) {
        return true;
    }

    if (T == NULL && type_is_object_ref(U)) {
        return true;
    }

    return false;
}

bool type_is_verifier_assignable_to(System_Type Q, System_Type R) {
    System_Type T = type_get_verification_type(Q);
    System_Type U = type_get_verification_type(R);

    if (T == U) {
        return true;
    }

    if (type_is_assignable_to(T, U)) {
        return true;
    }

    return false;
}

void type_print_name(System_Type type, strbuilder_t* builder) {
    if (type->DeclaringType != NULL) {
        type_print_name(type->DeclaringType, builder);
        strbuilder_char(builder, '+');
    } else {
        if (type->Namespace->Length > 0) {
            strbuilder_utf16(builder, type->Namespace->Chars, type->Namespace->Length);
            strbuilder_char(builder, '.');
        }
    }
    strbuilder_utf16(builder, type->Name->Chars, type->Name->Length);
}


void type_print_full_name(System_Type type, strbuilder_t* builder) {
    if (type->GenericParameterPosition >= 0) {
        strbuilder_utf16(builder, type->Name->Chars, type->Name->Length);
    } else {
        strbuilder_char(builder, '[');
        strbuilder_utf16(builder, type->Assembly->Name->Chars, type->Assembly->Name->Length);
        strbuilder_char(builder, '-');
        strbuilder_char(builder, 'v');
        strbuilder_uint(builder, type->Assembly->MajorVersion);
        strbuilder_char(builder, ']');
        type_print_name(type, builder);
    }
}

void method_print_name(System_Reflection_MethodInfo method, strbuilder_t* builder) {
    strbuilder_utf16(builder, method->Name->Chars, method->Name->Length);
    strbuilder_char(builder, '(');
    for (int i = 0; i < method->Parameters->Length; i++) {
        type_print_full_name(method->Parameters->Data[i]->ParameterType, builder);
        if (i + 1 != method->Parameters->Length) {
            strbuilder_char(builder, ',');
        }
    }
    strbuilder_char(builder, ')');
}

void method_print_full_name(System_Reflection_MethodInfo method, strbuilder_t* builder) {
    type_print_full_name(method->DeclaringType, builder);
    strbuilder_char(builder, ':');
    strbuilder_char(builder, ':');
    method_print_name(method, builder);
}

System_Reflection_FieldInfo type_get_field(System_Type type, System_String name) {
    for (int i = 0; i < type->Fields->Length; i++) {
        if (string_equals(type->Fields->Data[i]->Name, name)) {
            return type->Fields->Data[i];
        }
    }
    return NULL;
}

System_Reflection_MethodInfo type_iterate_methods(System_Type type, System_String name, int* index) {
    for (int i = *index; i < type->Methods->Length; i++) {
        if (string_equals(type->Methods->Data[i]->Name, name)) {
            *index = i + 1;
            return type->Methods->Data[i];
        }
    }
    return NULL;
}

System_Reflection_MethodInfo type_get_interface_method_impl(System_Type targetType, System_Reflection_MethodInfo targetMethod) {
    TinyDotNet_Reflection_InterfaceImpl interface = type_get_interface_impl(targetType, targetMethod->DeclaringType);
    if (interface == NULL) {
        return NULL;
    }
    return targetType->VirtualMethods->Data[interface->VTableOffset + targetMethod->VTableOffset];
}

TinyDotNet_Reflection_InterfaceImpl type_get_interface_impl(System_Type targetType, System_Type interfaceType) {
    if (targetType->InterfaceImpls == NULL) {
        return NULL;
    }

    for (int i = 0; i < targetType->InterfaceImpls->Length; i++) {
        if (targetType->InterfaceImpls->Data[i]->InterfaceType == interfaceType) {
            return targetType->InterfaceImpls->Data[i];
        }
    }
    return NULL;
}

bool isinstance(System_Object object, System_Type type) {
    if (object == NULL) {
        return true;
    }
    return type_is_verifier_assignable_to(object->vtable->type, type);
}

void type_dump(System_Type type) {
    printf("[*] \t%s %s ", type_visibility_str(type_visibility(type)), type_is_interface(type) ? "interface" : "class");
    strbuilder_t name = strbuilder_new();
    type_print_full_name(type, &name);
    if (type->BaseType != NULL) {
        strbuilder_cstr(&name, " : ");
        type_print_full_name(type->BaseType, &name);
    }
    printf("%s\r\n", strbuilder_get(&name));
    strbuilder_free(&name);

    for (int j = 0; j < type->Fields->Length; j++) {
        strbuilder_t field = strbuilder_new();
        strbuilder_cstr(&field, field_access_str(field_access(type->Fields->Data[j])));
        strbuilder_char(&field, ' ');
        strbuilder_cstr(&field, field_is_static(type->Fields->Data[j]) ? "static " : "");
        type_print_full_name(type->Fields->Data[j]->FieldType, &field);
        strbuilder_char(&field, ' ');
        strbuilder_utf16(&field, type->Fields->Data[j]->Name->Chars, type->Fields->Data[j]->Name->Length);
        TRACE("\t\t%s; // offset 0x%02x", strbuilder_get(&field), type->Fields->Data[j]->MemoryOffset);
        strbuilder_free(&field);
    }

    for (int j = 0; j < type->Methods->Length; j++) {
        System_Reflection_MethodInfo mi = type->Methods->Data[j];

        printf("[*] \t\t");

        strbuilder_t method = strbuilder_new();

        strbuilder_cstr(&method, method_access_str(method_get_access(mi)));
        strbuilder_char(&method, ' ');

        if (method_is_static(mi)) {
            strbuilder_cstr(&method, "static ");
        }

        if (method_is_abstract(mi)) {
            strbuilder_cstr(&method, "abstract ");
        }

        if (method_is_final(mi)) {
            strbuilder_cstr(&method, "final ");
        }

        if (method_is_virtual(mi)) {
            strbuilder_cstr(&method, "virtual[");
            strbuilder_uint(&method, mi->VTableOffset);
            strbuilder_cstr(&method, "] ");
        }

        if (mi->ReturnType == NULL) {
            strbuilder_cstr(&method, "void");
        } else {
            type_print_full_name(mi->ReturnType, &method);
        }
        strbuilder_char(&method, ' ');
        method_print_full_name(mi, &method);
        printf("%s\r\n", strbuilder_get(&method));
        strbuilder_free(&method);
    }
}

void assembly_dump(System_Reflection_Assembly assembly) {
    strbuilder_t name = strbuilder_new();
    strbuilder_utf16(&name, assembly->Module->Name->Chars, assembly->Module->Name->Length);
    TRACE("Assembly `%s`:", strbuilder_get(&name));
    strbuilder_free(&name);
    for (int i = 0; i < assembly->DefinedTypes->Length; i++) {
        System_Type type = assembly->DefinedTypes->Data[i];

        printf("[*] \t%s %s ", type_visibility_str(type_visibility(type)), type_is_interface(type) ? "interface" : "class");
        strbuilder_t name = strbuilder_new();
        type_print_full_name(type, &name);
        if (type->BaseType != NULL) {
            strbuilder_cstr(&name, " : ");
            type_print_full_name(type->BaseType, &name);
        }
        printf("%s\r\n", strbuilder_get(&name));
        strbuilder_free(&name);

        for (int j = 0; j < type->Fields->Length; j++) {
            strbuilder_t field = strbuilder_new();
            strbuilder_cstr(&field, field_access_str(field_access(type->Fields->Data[j])));
            strbuilder_char(&field, ' ');
            strbuilder_cstr(&field, field_is_static(type->Fields->Data[j]) ? "static " : "");
            type_print_full_name(type->Fields->Data[j]->FieldType, &field);
            strbuilder_char(&field, ' ');
            strbuilder_utf16(&field, type->Fields->Data[j]->Name->Chars, type->Fields->Data[j]->Name->Length);
            TRACE("\t\t%s; // offset 0x%02x", strbuilder_get(&field), type->Fields->Data[j]->MemoryOffset);
            strbuilder_free(&field);
        }

        for (int j = 0; j < type->Methods->Length; j++) {
            System_Reflection_MethodInfo mi =  type->Methods->Data[j];

            printf("[*] \t\t");

            strbuilder_t method = strbuilder_new();

            strbuilder_cstr(&method, method_access_str(method_get_access(mi)));
            strbuilder_char(&method, ' ');

            if (method_is_static(mi)) {
                strbuilder_cstr(&method, "static ");
            }

            if (method_is_abstract(mi)) {
                strbuilder_cstr(&method, "abstract ");
            }

            if (method_is_final(mi)) {
                strbuilder_cstr(&method, "final ");
            }

            if (method_is_virtual(mi)) {
                strbuilder_cstr(&method, "virtual[");
                strbuilder_uint(&method, mi->VTableOffset);
                strbuilder_cstr(&method, "] ");
            }

            if (mi->ReturnType == NULL) {
                strbuilder_cstr(&method, "void");
            } else {
                type_print_full_name(mi->ReturnType, &method);
            }
            strbuilder_char(&method, ' ');
            method_print_full_name(mi, &method);
            printf("%s\r\n", strbuilder_get(&method));
            strbuilder_free(&method);
            
            if (
                method_get_code_type(mi) == METHOD_IL &&
                !method_is_unmanaged(mi) &&
                !method_is_abstract(mi) &&
                !method_is_internal_call(mi)
            ) {
                // handle locals
                for (int li = 0; li < mi->MethodBody->LocalVariables->Length; li++) {
                    printf("[*] \t\t\t");
                    strbuilder_t local = strbuilder_new();
                    type_print_full_name(mi->MethodBody->LocalVariables->Data[li]->LocalType, &local);
                    strbuilder_cstr(&local, " V_");
                    strbuilder_uint(&local, mi->MethodBody->LocalVariables->Data[li]->LocalIndex);
                    printf("%s\r\n", strbuilder_get(&local));
                    strbuilder_free(&local);
                }

                opcode_disasm_method(mi);
            } else if (method_get_code_type(mi) == METHOD_NATIVE) {
                TRACE("\t\t\t<native method>");
            } else if (method_get_code_type(mi) == METHOD_RUNTIME) {
                TRACE("\t\t\t<runtime method>");
            }
        }

        TRACE("");
    }
}

static bool is_same_family(System_Type from, System_Type to) {
    while (from != to) {
        if (from == NULL) {
            return false;
        }
        from = from->BaseType;
    }
    return true;
}

bool check_field_accessibility(System_Type from, System_Reflection_FieldInfo to) {
    if (!check_type_visibility(from, to->DeclaringType)) {
        return false;
    }

    bool family = is_same_family(from, to->DeclaringType);
    bool assembly = from->Assembly == to->DeclaringType->Assembly;

    switch (field_access(to)) {
        case FIELD_COMPILER_CONTROLLED: ASSERT(!"TODO: METHOD_COMPILER_CONTROLLED"); return false;
        case FIELD_PRIVATE: {
            // get the raw to type
            System_Type to_type = to->DeclaringType;
            if (to_type->GenericTypeDefinition != NULL) {
                to_type = to_type->GenericTypeDefinition;
            }

            // get the raw declaring type
            System_Type declaring = from;
            if (declaring->GenericTypeDefinition != NULL) {
                declaring = declaring->GenericTypeDefinition;
            }

            // now go ahead a match it
            while (declaring != NULL) {
                if (declaring == to_type) {
                    return true;
                }
                declaring = declaring->DeclaringType;
            }
            return false;
        }
        case FIELD_FAMILY: return family;
        case FIELD_ASSEMBLY: return assembly;
        case FIELD_FAMILY_AND_ASSEMBLY: return family && assembly;
        case FIELD_FAMILY_OR_ASSEMBLY: return family || assembly;
        case FIELD_PUBLIC: return true;
        default:
            ASSERT(!"Invalid method access");
            return false;
    }

}

bool check_method_accessibility(System_Type from, System_Reflection_MethodInfo to) {
    if (!check_type_visibility(from, to->DeclaringType)) {
        return false;
    }

    bool family = is_same_family(from, to->DeclaringType);
    bool assembly = from->Assembly == to->DeclaringType->Assembly;

    switch (method_get_access(to)) {
        case METHOD_COMPILER_CONTROLLED: ASSERT(!"TODO: METHOD_COMPILER_CONTROLLED"); return false;
        case METHOD_PRIVATE: return from == to->DeclaringType;
        case METHOD_FAMILY: return family;
        case METHOD_ASSEMBLY: return assembly;
        case METHOD_FAMILY_AND_ASSEMBLY: return family && assembly;
        case METHOD_FAMILY_OR_ASSEMBLY: return family || assembly;
        case METHOD_PUBLIC: return true;
        default:
            ASSERT(!"Invalid method access");
            return false;
    }
}

bool check_type_visibility(System_Type from, System_Type to) {
    type_visibility_t visibility = type_visibility(to);

    // start with easy cases
    if (visibility == TYPE_PUBLIC) {
        // anyone can access this
        return true;
    } else if (visibility == TYPE_NOT_PUBLIC) {
        // only the same assembly may access this
        return from->Assembly == to->Assembly;
    }

    // the rest only works on nested types
    if (to->DeclaringType == NULL) {
        ASSERT(!"Must be nested");
        return false;
    }

    bool family = is_same_family(from, to->DeclaringType);
    bool assembly = from->Assembly == to->DeclaringType->Assembly;

    switch (visibility) {
        case TYPE_NESTED_PRIVATE:{
            // get the raw to type
            System_Type to_type = to->DeclaringType;
            if (to_type->GenericTypeDefinition != NULL) {
                to_type = to_type->GenericTypeDefinition;
            }

            // get the raw declaring type
            System_Type declaring = from;
            if (declaring->GenericTypeDefinition != NULL) {
                declaring = declaring->GenericTypeDefinition;
            }

            // now go ahead a match it
            while (declaring != NULL) {
                if (declaring == to_type) {
                    return true;
                }
                declaring = declaring->DeclaringType;
            }
            return false;
        }
        case TYPE_NESTED_FAMILY: return family;
        case TYPE_NESTED_ASSEMBLY: return assembly;
        case TYPE_NESTED_FAMILY_AND_ASSEMBLY: return family && assembly;
        case TYPE_NESTED_FAMILY_OR_ASSEMBLY: return family || assembly;
        case TYPE_NESTED_PUBLIC: return true;
        case TYPE_NOT_PUBLIC:
        case TYPE_PUBLIC:
            ASSERT(!"We should have already handled this?");
            return false;
    }

    return true;
}

static System_Type expand_type(System_Type type, System_Type_Array arguments);

static System_Reflection_FieldInfo expand_field(System_Type type, System_Reflection_FieldInfo field, System_Type_Array arguments) {
    System_Reflection_FieldInfo instance = GC_NEW(tSystem_Reflection_FieldInfo);
    instance->FieldType = expand_type(field->FieldType, arguments);
    instance->Attributes = field->Attributes;
    GC_UPDATE(instance, Module, field->Module);
    GC_UPDATE(instance, DeclaringType, type);
    GC_UPDATE(instance, Name, field->Name);
    return instance;
}

static System_Reflection_MethodInfo expand_method(System_Type type, System_Reflection_MethodInfo method, System_Type_Array arguments) {
    System_Reflection_MethodInfo instance = GC_NEW(tSystem_Reflection_MethodInfo);
    GC_UPDATE(instance, Module, method->Module);
    GC_UPDATE(instance, DeclaringType, type);
    GC_UPDATE(instance, Name, method->Name);
    GC_UPDATE(instance, ReturnType, expand_type(method->ReturnType, arguments));
    instance->Attributes = method->Attributes;
    instance->ImplAttributes = method->ImplAttributes;

    // expand body
    if (method->MethodBody != NULL) {
        System_Reflection_MethodBody methodBody = method->MethodBody;

        // check exception clauses
        bool expand_exceptions = false;
        for (int i = 0; i < methodBody->ExceptionHandlingClauses->Length; i++) {
            System_Reflection_ExceptionHandlingClause clause = methodBody->ExceptionHandlingClauses->Data[i];
            if (clause->CatchType != NULL && type_is_generic_parameter(clause->CatchType)) {
                expand_exceptions = true;
                break;
            }
        }

        // check local variables
        bool expand_locals = false;
        for (int i = 0; i < methodBody->LocalVariables->Length; i++) {
            System_Reflection_LocalVariableInfo variable = methodBody->LocalVariables->Data[i];
            if (type_is_generic_parameter(variable->LocalType)) {
                expand_locals = true;
                break;
            }
        }

        if (expand_locals || expand_exceptions) {
            // create new and expand as needed
            System_Reflection_MethodBody new_body = GC_NEW(tSystem_Reflection_MethodBody);
            GC_UPDATE(new_body, Il, methodBody->Il);
            new_body->InitLocals = methodBody->InitLocals;
            new_body->MaxStackSize = methodBody->MaxStackSize;

            if (expand_locals) {
                GC_UPDATE(new_body, LocalVariables, GC_NEW_ARRAY(tSystem_Reflection_LocalVariableInfo, methodBody->LocalVariables->Length));
                for (int i = 0; i < new_body->LocalVariables->Length; i++) {
                    if (type_is_generic_parameter(methodBody->LocalVariables->Data[i]->LocalType)) {
                        // expand
                        System_Reflection_LocalVariableInfo variable = GC_NEW(tSystem_Reflection_LocalVariableInfo);
                        GC_UPDATE(variable, LocalType, expand_type(methodBody->LocalVariables->Data[i]->LocalType, arguments));
                        variable->LocalIndex = methodBody->LocalVariables->Data[i]->LocalIndex;
                        GC_UPDATE_ARRAY(new_body->LocalVariables, i, variable);
                    } else {
                        // no need to expand this one
                        GC_UPDATE_ARRAY(new_body->LocalVariables, i, methodBody->LocalVariables->Data[i]);
                    }
                }
            } else {
                GC_UPDATE(new_body, LocalVariables, methodBody->LocalVariables);
            }

            if (expand_exceptions) {
                GC_UPDATE(new_body, ExceptionHandlingClauses, GC_NEW_ARRAY(tSystem_Reflection_ExceptionHandlingClause, methodBody->ExceptionHandlingClauses->Length));
                for (int i = 0; i < new_body->ExceptionHandlingClauses->Length; i++) {
                    System_Reflection_ExceptionHandlingClause clause = methodBody->ExceptionHandlingClauses->Data[i];
                    if (clause->CatchType != NULL && type_is_generic_parameter(clause->CatchType)) {
                        // expand
                        System_Reflection_ExceptionHandlingClause new_clause = GC_NEW(tSystem_Reflection_ExceptionHandlingClause);
                        GC_UPDATE(new_clause, CatchType, expand_type(clause->CatchType, arguments));
                        new_clause->Flags = clause->Flags;
                        new_clause->FilterOffset = clause->FilterOffset;
                        new_clause->HandlerLength = clause->HandlerLength;
                        new_clause->HandlerOffset = clause->HandlerOffset;
                        new_clause->TryLength = clause->TryLength;
                        new_clause->TryOffset = clause->TryOffset;
                        GC_UPDATE_ARRAY(new_body->ExceptionHandlingClauses, i, new_clause);
                    } else {
                        // no need to expand this one
                        GC_UPDATE_ARRAY(new_body->ExceptionHandlingClauses, i, clause);
                    }
                }
            } else {
                GC_UPDATE(new_body, ExceptionHandlingClauses, methodBody->ExceptionHandlingClauses);
            }

            GC_UPDATE(instance, MethodBody, new_body);
        } else {
            // no need to expand anything, keep the original stuff
            GC_UPDATE(instance, MethodBody, method->MethodBody);
        }
    }


    // method parameters
    GC_UPDATE(instance, Parameters, GC_NEW_ARRAY(tSystem_Reflection_ParameterInfo, method->Parameters->Length));
    for (int i = 0; i < instance->Parameters->Length; i++) {
        System_Reflection_ParameterInfo parameter = GC_NEW(tSystem_Reflection_ParameterInfo);
        System_Reflection_ParameterInfo fieldParameter = method->Parameters->Data[i];
        parameter->Attributes = fieldParameter->Attributes;
        GC_UPDATE(parameter, Name, fieldParameter->Name);
        GC_UPDATE(parameter, ParameterType, expand_type(fieldParameter->ParameterType, arguments));
        GC_UPDATE_ARRAY(instance->Parameters, i, parameter);
    }

    return instance;
}

static System_Type expand_type(System_Type type, System_Type_Array arguments) {
    if (type == NULL) {
        return NULL;
    } else if (!type_is_generic_parameter(type)) {
        return type;
    } else if (type->GenericParameterPosition >= 0) {
        // type is a generic argument, resolve it
        return arguments->Data[type->GenericParameterPosition];
    } else if (type->IsArray) {
        // type is an array, resolve the element type
        System_Type real_array_type = expand_type(type->ElementType, arguments);
        return get_array_type(real_array_type);
    } else if (type->IsByRef) {
        // type is a byref, resolve the base type
        System_Type real_byref_base = expand_type(type->BaseType, arguments);
        return get_by_ref_type(real_byref_base);
    } else if (type->GenericArguments != NULL) {
        // type has generic arguments, expand them and create the
        // new generic type
        System_Type definition = type;
        if (type->GenericTypeDefinition != NULL) {
            definition = type->GenericTypeDefinition;
        }
        System_Type_Array new_arguments = GC_NEW_ARRAY(tSystem_Type, type->GenericArguments->Length);
        for (int i = 0; i < new_arguments->Length; i++) {
            GC_UPDATE_ARRAY(new_arguments, i, expand_type(type->GenericArguments->Data[i], arguments));
        }
        return type_make_generic(definition, new_arguments);
    } else {
        ASSERT(!"Invalid generic type");
    }
}

static System_Reflection_MethodInfo find_expanded_method(System_Reflection_MethodInfo method, System_Type_Array arguments) {
    System_Type type = expand_type(method->DeclaringType, arguments);
    TRACE("find_expanded_method: %U", type->Name);

    int index = 0;
    System_Reflection_MethodInfo methodInfo = NULL;
    while ((methodInfo = type_iterate_methods(type, method->Name, &index)) != NULL) {
        // make sure both either have or don't have this
        if (method_is_static(methodInfo) != method_is_static(method)) continue;

        // check the return type and parameters count is the same
        if (methodInfo->ReturnType != expand_type(method->ReturnType, arguments)) continue;
        if (methodInfo->Parameters->Length != method->Parameters->Length) continue;

        // check that the parameters are the same
        bool found = true;
        for (int pi = 0; pi < methodInfo->Parameters->Length; pi++) {
            if (methodInfo->Parameters->Data[pi]->ParameterType != expand_type(method->Parameters->Data[pi]->ParameterType, arguments)) {
                found = false;
                break;
            }
        }

        if (found) {
            break;
        }
    }

    ASSERT(methodInfo != NULL);

    return methodInfo;
}

System_Type type_make_generic(System_Type type, System_Type_Array arguments) {
    ASSERT(type_is_generic_definition(type));

    monitor_enter(type);

    // add it
    bool is_full_instantiation = true;
    for (int i = 0; i < arguments->Length; i++) {
        if (type_is_generic_parameter(arguments->Data[i])) {
            is_full_instantiation = false;
            break;
        }
    }

    if (is_full_instantiation) {
        // check for an existing instance
        System_Type inst = type->NextGenericInstance;
        bool found = false;
        while (inst != NULL) {
            found = true;
            for (int i = 0; i < arguments->Length; i++) {
                if (arguments->Data[i] != inst->GenericArguments->Data[i]) {
                    found = false;
                    break;
                }
            }

            if (found) {
                break;
            }

            inst = inst->NextGenericInstance;
        }

        if (found) {
            monitor_exit(type);
            return inst;
        }
    }

    // instance not found, create one
    System_Type instance = GC_NEW(tSystem_Type);
    GC_UPDATE(instance, DeclaringType, type->DeclaringType);
    GC_UPDATE(instance, Module, type->Module);
    GC_UPDATE(instance, Assembly, type->Assembly);
    GC_UPDATE(instance, GenericArguments, arguments);
    GC_UPDATE(instance, GenericTypeDefinition, type);
    GC_UPDATE(instance, Namespace, type->Namespace);
    instance->StackType = type->StackType;
    instance->Attributes = type->Attributes;
    instance->GenericParameterPosition = -1;

    // create the unique name
    strbuilder_t builder = strbuilder_new();
    strbuilder_utf16(&builder, type->Name->Chars, type->Name->Length);
    strbuilder_char(&builder, '<');
    for (int i = 0; i < arguments->Length; i++) {
        type_print_full_name(arguments->Data[i], &builder);
        if (i + 1 != arguments->Length) {
            strbuilder_char(&builder, ',');
        }
    }
    strbuilder_char(&builder, '>');
    GC_UPDATE(instance, Name, new_string_from_cstr(strbuilder_get(&builder)));
    strbuilder_free(&builder);

    // if this is not a full generic instantiation (it takes generic arguments)
    // then we are not going to actually expand it, if it is all real types then
    // we are going to add it, and only after leaving the lock expand it, so we
    // allow the other person to expand it properly
    if (!is_full_instantiation) {
        monitor_exit(type);
        return instance;
    } else {
        GC_UPDATE(instance, NextGenericInstance, type->NextGenericInstance);
        GC_UPDATE(type, NextGenericInstance, instance);

        monitor_exit(type);
    }

    // base type
    GC_UPDATE(instance, BaseType, expand_type(type->BaseType, arguments));

    // fields
    GC_UPDATE(instance, Fields, GC_NEW_ARRAY(tSystem_Reflection_FieldInfo, type->Fields->Length));
    for (int i = 0; i < instance->Fields->Length; i++) {
        GC_UPDATE_ARRAY(instance->Fields, i, expand_field(instance, type->Fields->Data[i], arguments));
    }

    // methods
    GC_UPDATE(instance, Methods, GC_NEW_ARRAY(tSystem_Reflection_MethodInfo, type->Methods->Length));
    for (int i = 0; i < instance->Methods->Length; i++) {
        GC_UPDATE_ARRAY(instance->Methods, i, expand_method(instance, type->Methods->Data[i], arguments));
    }

    // interfaces
    if (type->InterfaceImpls != NULL) {
        GC_UPDATE(instance, InterfaceImpls, GC_NEW_ARRAY(tTinyDotNet_Reflection_InterfaceImpl, type->InterfaceImpls->Length));
        for (int i = 0; i < instance->InterfaceImpls->Length; i++) {
            TinyDotNet_Reflection_InterfaceImpl impl = GC_NEW(tTinyDotNet_Reflection_InterfaceImpl);
            impl->InterfaceType = expand_type(type->InterfaceImpls->Data[i]->InterfaceType, arguments);
            GC_UPDATE_ARRAY(instance->InterfaceImpls, i, impl);
        }
    }

    if (type->MethodImpls != NULL) {
        GC_UPDATE(instance, MethodImpls, GC_NEW_ARRAY(tTinyDotNet_Reflection_MethodImpl, type->MethodImpls->Length));
        for (int i = 0; i < instance->MethodImpls->Length; i++) {
            TinyDotNet_Reflection_MethodImpl impl = GC_NEW(tTinyDotNet_Reflection_MethodImpl);
            GC_UPDATE(impl, Body, find_expanded_method(type->MethodImpls->Data[i]->Body, arguments));
            GC_UPDATE(impl, Declaration, find_expanded_method(type->MethodImpls->Data[i]->Declaration, arguments));
            GC_UPDATE_ARRAY(instance->MethodImpls, i, impl);
        }
    }

    return instance;
}

System_Reflection_MethodInfo method_make_generic(System_Reflection_MethodInfo method, System_Type_Array arguments) {
    monitor_enter(method);

    // check for an existing instance
    System_Reflection_MethodInfo inst = method->NextGenericInstance;
    bool found = false;
    while (inst != NULL) {
        found = true;
        for (int i = 0; i < arguments->Length; i++) {
            if (arguments->Data[i] != inst->GenericArguments->Data[i]) {
                found = false;
                break;
            }
        }

        if (found) {
            break;
        }

        inst = inst->NextGenericInstance;
    }

    if (found) {
        monitor_exit(method);
        return inst;
    }

    System_Reflection_MethodInfo instance = expand_method(method->DeclaringType, method, arguments);

    GC_UPDATE(instance, GenericArguments, arguments);
    GC_UPDATE(instance, NextGenericInstance, method->NextGenericInstance);
    GC_UPDATE(method, NextGenericInstance, instance);

    monitor_exit(method);

    return instance;
}

bool type_is_generic_parameter(System_Type type) {
    if (type == NULL) {
        return false;
    } else if (type->GenericParameterPosition >= 0) {
        return true;
    } else if (type->IsArray) {
        return type_is_generic_parameter(type->ElementType);
    } else if (type->IsByRef) {
        return type_is_generic_parameter(type->BaseType);
    } else if (type->GenericArguments != NULL) {
        for (int i = 0; i < type->GenericArguments->Length; i++) {
            if (type_is_generic_parameter(type->GenericArguments->Data[i])) {
                return true;
            }
        }
        return false;
    } else {
        return false;
    }
}

