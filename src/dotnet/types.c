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

System_Type tSystem_Void = NULL;
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
System_Type tSystem_Delegate = NULL;
System_Type tSystem_MulticastDelegate = NULL;
System_Type tSystem_Reflection_Module = NULL;
System_Type tSystem_Reflection_Assembly = NULL;
System_Type tSystem_Reflection_FieldInfo = NULL;
System_Type tSystem_Reflection_MemberInfo = NULL;
System_Type tSystem_Reflection_ParameterInfo = NULL;
System_Type tSystem_Reflection_PropertyInfo = NULL;
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
System_Type tSystem_RuntimeTypeHandle = NULL;
System_Type tSystem_Nullable = NULL;
System_Type tSystem_ReadOnlySpan = NULL;
System_Type tSystem_Span = NULL;

System_Type tSystem_GenericArray = NULL;

System_Type tTinyDotNet_Reflection_InterfaceImpl = NULL;
System_Type tTinyDotNet_Reflection_MemberReference = NULL;
System_Type tTinyDotNet_Reflection_MethodImpl = NULL;
System_Type tTinyDotNet_Reflection_MethodSpec = NULL;

System_Type tSystem_Runtime_CompilerServices_Unsafe = NULL;
System_Type tSystem_Runtime_CompilerServices_RuntimeHelpers = NULL;
System_Type tSystem_Runtime_CompilerServices_IsVolatile = NULL;
System_Type tSystem_Runtime_InteropServices_InAttribute = NULL;
System_Type tSystem_ThreadStaticAttribute = NULL;

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
        if (a->GenericTypeDefinition != b->GenericTypeDefinition) {
            return false;
        }

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

bool method_compare_name_and_sig(System_Reflection_MethodInfo method, System_Reflection_MethodInfo signature) {
    // make sure the name matches
    if (!string_equals(method->Name, signature->Name)) {
        return false;
    }

    // make sure both are static or not static
    if (method_is_static(signature) != method_is_static(method)) {
        return false;
    }

    // make sure generic arguments count is the same
    if (signature->GenericArguments != NULL) {
        if (
            method->GenericArguments == NULL ||
            method->GenericArguments->Length != signature->GenericArguments->Length
        ) {
            return false;
        }
    }

    // check the return type
    if (!match_generic_type(signature->ReturnType, method->ReturnType)) {
        return false;
    }

    // make sure the count is the same
    if (signature->Parameters->Length != method->Parameters->Length) {
        return false;
    }

    // check all the parameters
    for (int i = 0; i < signature->Parameters->Length; i++) {
        if (!match_generic_type(method->Parameters->Data[i]->ParameterType, signature->Parameters->Data[i]->ParameterType)) {
            return false;
        }
    }

    return true;
}

System_Reflection_MethodInfo type_find_method_in_type(System_Type type, System_Reflection_MethodInfo signature) {
    System_Type lookInType = type;
    do {
        for (int i = 0; i < lookInType->Methods->Length; i++) {
            if (method_compare_name_and_sig(lookInType->Methods->Data[i], signature)) {
                return lookInType->Methods->Data[i];
            }
        }

        lookInType = lookInType->BaseType;
    } while (lookInType != NULL);

    return NULL;
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
            System_Reflection_MethodInfo signature;
            blob_entry_t blob = {
                .data = ref->Signature->Data,
                .size = ref->Signature->Length
            };
            CHECK_AND_RETHROW(parse_method_ref_sig(blob, assembly, &signature, type->GenericArguments));
            GC_UPDATE(signature, Name, ref->Name);

            // now search for it in the type
            System_Reflection_MethodInfo methodInfo = type_find_method_in_type(type, signature);

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

static bool m_enable_generic_arrays = false;

static bool THREAD_LOCAL m_nesting_setup_generic_array;

void enable_generic_arrays() {
    m_enable_generic_arrays = true;
}

void setup_generic_array(System_Type type) {
    if (m_nesting_setup_generic_array) {
        return;
    }

    PANIC_ON(monitor_enter(type));

    // already inited by the time we took the lock
    if (type->VTable != NULL) {
        return;
    }

    // don't nest the generic array creation
    m_nesting_setup_generic_array = true;

    // create the args
    System_Type_Array args = GC_NEW_ARRAY(tSystem_Type, 1);
    args->Data[0] = type->ElementType;

    // instantiate
    System_Type genericArray = NULL;
    PANIC_ON(type_make_generic(tSystem_GenericArray, args, &genericArray));
    PANIC_ON(loader_fill_type(genericArray));

    // now set the methods so the array type will have all the needed stuff
    GC_UPDATE(type, Methods, genericArray->Methods);
    GC_UPDATE(type, VirtualMethods, genericArray->VirtualMethods);
    GC_UPDATE(type, VTable, genericArray->VTable);
    GC_UPDATE(type, InterfaceImpls, genericArray->InterfaceImpls);
    GC_UPDATE(type, MethodImpls, genericArray->MethodImpls);
    type->VTableSize = genericArray->VTableSize;

    // we no longer nest,
    m_nesting_setup_generic_array = false;

    PANIC_ON(monitor_exit(type));
}

static System_Type create_array_type(System_Type type) {
    // TODO: panic on fail
    PANIC_ON(monitor_enter(type));

    if (type->ArrayType != NULL) {
        PANIC_ON(monitor_exit(type));
        return type->ArrayType;
    }

    // allocate the new type
    System_Type ArrayType = UNSAFE_GC_NEW(tSystem_Type);
    if (ArrayType == NULL) {
        return ArrayType;
    }

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

    // There are no managed pointers in here (The gc will handle array
    // stuff on its own)
    ArrayType->ManagedPointersOffsets = NULL;

    // Set the element type
    GC_UPDATE(ArrayType, ElementType, type);

    // Set the array type
    GC_UPDATE(type, ArrayType, ArrayType);
    PANIC_ON(monitor_exit(type));

    return type->ArrayType;
}

System_Type get_array_type(System_Type type) {
    System_Type arrayType = type->ArrayType ?: create_array_type(type);

    if (m_enable_generic_arrays && arrayType->VTable == NULL) {
        setup_generic_array(arrayType);
    }

    return arrayType;
}

System_Type get_by_ref_type(System_Type type) {
    if (type->ByRefType != NULL) {
        return type->ByRefType;
    }

    PANIC_ON(monitor_enter(type));

    if (type->ByRefType != NULL) {
        PANIC_ON(monitor_exit(type));
        return type->ByRefType;
    }

    // must not be a byref
    ASSERT(!type->IsByRef);

    // allocate the new ref type
    System_Type ByRefType = UNSAFE_GC_NEW(tSystem_Type);
    ASSERT(ByRefType != NULL);

    // this is an array
    ByRefType->IsByRef = true;
    ByRefType->IsFilled = true;
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
    PANIC_ON(monitor_exit(type));

    return type->ByRefType;
}

System_Type get_pointer_type(System_Type type) {
    if (type->PointerType != NULL) {
        return type->PointerType;
    }

    PANIC_ON(monitor_enter(type));

    if (type->PointerType != NULL) {
        PANIC_ON(monitor_exit(type));
        return type->PointerType;
    }

    // must not be a byref
    ASSERT(!type->IsByRef);

    // allocate the new ref type
    System_Type PointerType = UNSAFE_GC_NEW(tSystem_Type);
    if (PointerType == NULL) {
        return PointerType;
    }

    GC_UPDATE(PointerType, Module, type->Module);
    GC_UPDATE(PointerType, Name, string_append_cstr(type->Name, "*"));
    GC_UPDATE(PointerType, Assembly, type->Assembly);
    GC_UPDATE(PointerType, BaseType, tSystem_UIntPtr->BaseType);
    GC_UPDATE(PointerType, Namespace, type->Namespace);
    PointerType->ManagedSize = tSystem_UIntPtr->ManagedSize;
    PointerType->ManagedAlignment = tSystem_UIntPtr->ManagedAlignment;
    PointerType->StackSize = tSystem_UIntPtr->StackSize;
    PointerType->StackAlignment = tSystem_UIntPtr->StackAlignment;
    PointerType->StackType = STACK_TYPE_INTPTR;
    PointerType->GenericParameterPosition = -1;
    PointerType->IsFilled = true;
    PointerType->IsPointer = true;
    GC_UPDATE(PointerType, ElementType, type);

    // Set the array type
    GC_UPDATE(type, PointerType, PointerType);
    PANIC_ON(monitor_exit(type));

    return type->PointerType;
}

System_Type get_boxed_type(System_Type type) {
    if (type->BoxedType != NULL) {
        return type->BoxedType;
    }

    PANIC_ON(monitor_enter(type));

    if (type->BoxedType != NULL) {
        PANIC_ON(monitor_exit(type));
        return type->BoxedType;
    }

    // must not be a byref
    ASSERT(!type->IsByRef);

    // TODO: error handling?
    PANIC_ON(loader_fill_type(type));

    // allocate the new ref type
    System_Type BoxedType = UNSAFE_GC_NEW(tSystem_Type);
    if (BoxedType == NULL) {
        return BoxedType;
    }

    GC_UPDATE(BoxedType, DeclaringType, type->DeclaringType);
    GC_UPDATE(BoxedType, Module, type->Module);
    GC_UPDATE(BoxedType, Name, string_append_cstr(type->Name, "{boxed}"));
    GC_UPDATE(BoxedType, Assembly, type->Assembly);
    GC_UPDATE(BoxedType, BaseType, type->BaseType);
    GC_UPDATE(BoxedType, Namespace, type->Namespace);
    GC_UPDATE(BoxedType, Fields, type->Fields);
    GC_UPDATE(BoxedType, Methods, type->Methods);
    GC_UPDATE(BoxedType, UnboxedType, type);
    BoxedType->ManagedSize = type->ManagedSize;
    BoxedType->ManagedAlignment = type->ManagedAlignment;
    BoxedType->StackSize = tSystem_Object->StackSize;
    BoxedType->StackAlignment = tSystem_Object->StackAlignment;
    BoxedType->StackType = STACK_TYPE_O;
    BoxedType->GenericParameterPosition = -1;
    BoxedType->IsFilled = true;
    BoxedType->IsBoxed = true;
    GC_UPDATE(BoxedType, InterfaceImpls, type->InterfaceImpls);
    GC_UPDATE(BoxedType, VirtualMethods, type->VirtualMethods);

    // Set the array type
    GC_UPDATE(type, BoxedType, BoxedType);
    PANIC_ON(monitor_exit(type));

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

bool field_is_thread_static(System_Reflection_FieldInfo field) {
    // TODO: maybe have this as a better thing
    System_Reflection_Assembly assembly = field->Module->Assembly;

    int idx = hmgeti(assembly->CustomAttributeMap, (System_Object)field);
    if (idx >= 0) {
        System_Object* attributes = assembly->CustomAttributeMap[idx].value;
        for (int index = 0; index < arrlen(attributes); index++) {
            if (OBJECT_TYPE(attributes[index]) == tSystem_ThreadStaticAttribute) {
                return true;
            }
        }
    }

    return false;
}



static bool type_is_integer(System_Type type) {
    return type == tSystem_Byte || type == tSystem_Int16 || type == tSystem_Int32 || type == tSystem_Int64 ||
           type == tSystem_SByte || type == tSystem_UInt16 || type == tSystem_UInt32 || type == tSystem_UInt64 ||
           type == tSystem_UIntPtr || type == tSystem_IntPtr || type == tSystem_Char || type == tSystem_Boolean;
}

System_Type type_get_underlying_type(System_Type T) {
    if (type_is_enum(T)) {
        ASSERT(T->ElementType != NULL);
        return T->ElementType;
    } else if (T != NULL && T->IsPointer) {
        return tSystem_UIntPtr;
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
    // if T is a managed pointer type S&
    if (T != NULL && T->IsByRef) {
        System_Type TR = type_get_reduced_type(T->BaseType);
        if (TR == tSystem_SByte || TR == tSystem_Boolean) {
            return get_by_ref_type(tSystem_SByte);
        } else if (TR == tSystem_Int16 || TR == tSystem_Char) {
            return get_by_ref_type(tSystem_Int16);
        } else if (TR == tSystem_Int32) {
            return get_by_ref_type(tSystem_Int32);
        } else if (TR == tSystem_Int64) {
            return get_by_ref_type(tSystem_Int64);
        } else if (TR == tSystem_IntPtr) {
            return get_by_ref_type(tSystem_IntPtr);
        } else {
            return T;
        }
    } else {
        System_Type TR = type_get_reduced_type(T);
        if (TR == tSystem_SByte || TR == tSystem_Boolean) {
            return tSystem_SByte;
        } else if (TR == tSystem_Int16 || TR == tSystem_Char) {
            return tSystem_Int16;
        } else if (TR == tSystem_Int32) {
            return tSystem_Int32;
        } else if (TR == tSystem_Int64) {
            return tSystem_Int64;
        } else if (TR == tSystem_IntPtr) {
            return tSystem_IntPtr;
        } else {
            return T;
        }
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
    // T has underlying type V
    System_Type V = type_get_underlying_type(T);

    // U has underlying type W
    System_Type W = type_get_underlying_type(U);

    // T is the null type, and U is a reference type
    if (T == NULL && type_is_object_ref(U)) {
        return true;
    }

    // V is compatible-with W
    if (type_is_compatible_with(V, W)) {
        return true;

    }

    // V amd W jave the same reduced type
//    if (type_get_reduced_type(V) == type_get_reduced_type(W)) {
    if (type_get_verification_type(V) == type_get_verification_type(W)) {
        // spec says it should be reduced-type, but then bool and int8 are not the same
        // and there is valid code where this happens...
        return true;
    }

    return false;
}

bool type_is_pointer_element_compatible_with(System_Type T, System_Type U) {
    // A managed pointer type T is pointer-element-compatible-with a managed pointer type U if and
    // only if T has verification type V and U has verification type W and V is identical to W.
    System_Type V = type_get_verification_type(T);
    System_Type W = type_get_verification_type(U);

    // for value types we are going to make sure
    // that the inheritance work (in reality it is only
    // for System.Enum and System.ValueType to work)
    if (V != NULL && V->BaseType->IsValueType) {
        System_Type base = V;
        while (base != NULL) {
            if (base == W->BaseType) {
                return true;
            }
            base = base->BaseType;
        }

        return false;
    } else {
        return V == W;
    }
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

    while (T != NULL) {
        if (T->InterfaceImpls == NULL) {
            T = T->BaseType;
            continue;
        }

        for (int i = 0; i < T->InterfaceImpls->Length; i++) {
            if (T->InterfaceImpls->Data[i]->InterfaceType == I) {
                return true;
            }
        }

        T = T->BaseType;
    }

    return false;
}

static bool type_is_compatible_with_I_8_7_1(System_Type T, System_Type U) {
    // T is identical to U
    if (T == U) {
        return true;
    }

    // TODO: idk if this is correct
    if (T == NULL || U == NULL) {
        return false;
    }

    // T is a reference type
    if (type_is_object_ref(T)) {
        // and U is the direct base class of T
        if (U == type_get_direct_base_class(T)) {
            return true;
        }

        // and U is an interface directly implemented by T
        if (type_is_interface_directly_implemented_by(U, T)) {
            return true;
        }
    }

    // T s a zero-based rank-1 array V[] and
    // U is a zero-based rank-1 array W[]
    if (T->IsArray && U->IsArray) {
        System_Type V = T->ElementType;
        System_Type W = U->ElementType;

        // and V is array-element-compatible-with W
        if (type_is_array_element_compatible_with(V, W)) {
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

    return false;
}

bool type_is_compatible_with(System_Type T, System_Type U) {
    // T and U are not managed pointer types
    if ((T == NULL || !T->IsByRef) && (U == NULL || !U->IsByRef)) {
        // and T is compatible-with U according to definition in I.8.7.1
        return type_is_compatible_with_I_8_7_1(T, U);
    }

    // T and U are both managed pointer types
    if ((T != NULL && T->IsByRef) && (U != NULL && U->IsByRef)) {
        // and T is pointer-element-compatible-with U.
        return type_is_pointer_element_compatible_with(T, U);
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

    if (
        (V == tSystem_IntPtr && W == tSystem_Int32) ||
        (V == tSystem_Int32 && W == tSystem_IntPtr)
    ) {
        return true;
    }

    if (type_is_compatible_with(T, U)) {
        return true;
    }

    return false;
}

bool type_is_verifier_assignable_to(System_Type Q, System_Type R) {
    // T is the verification type of Q
    System_Type T = type_get_verification_type(Q);

    // U is the verification type of R,
    System_Type U = type_get_verification_type(R);

    // T is identical to U
    if (T == U) {
        return true;
    }

    // T is assignable-to U according to the rules in I.8.7.3
    if (type_is_assignable_to(T, U)) {
        return true;
    }

    // T is boxed V
    if (T != NULL && T->IsBoxed && U != NULL) {
        System_Type V = T->UnboxedType;

        // and U is the immediate base class of V
        if (V->BaseType == U) {
            return true;
        }

        // and U is an interface directly implemented by V
        if (type_is_interface_directly_implemented_by(U, V)) {
            return true;
        }
    }

    // T is the null type, and U is a reference type
    if (T == NULL && type_is_object_ref(U)) {
        return true;
    }

    return false;
}

static const char* handle_builtin(System_Type type) {
    if (type == tSystem_SByte) return "int8";
    else if (type == tSystem_Byte) return "uint8";
    else if (type == tSystem_Int16) return "int16";
    else if (type == tSystem_UInt16) return "uint16";
    else if (type == tSystem_Int32) return "int32";
    else if (type == tSystem_UInt32) return "uint32";
    else if (type == tSystem_Int64) return "int64";
    else if (type == tSystem_UInt64) return "uint64";
    else if (type == tSystem_Object) return "object";
    else if (type == tSystem_Char) return "char";
    else if (type == tSystem_Boolean) return "bool";
    else if (type == tSystem_IntPtr) return "nint";
    else if (type == tSystem_UIntPtr) return "nuint";
    else if (type == tSystem_String) return "string";
    else if (type == tSystem_Single) return "float32";
    else if (type == tSystem_Double) return "float64";
    else if (type == NULL || type == tSystem_Void) return "void";
    else return NULL;
}

void type_print_name(System_Type type, strbuilder_t* builder) {
    const char* builtin = handle_builtin(type);
    if (builtin != NULL) {
        strbuilder_cstr(builder, builtin);
        return;
    }

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
    const char* builtin = handle_builtin(type);
    if (builtin != NULL) {
        strbuilder_cstr(builder, builtin);
        return;
    }

    if (type_is_generic_parameter(type)) {
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
    if (method->ReturnType != NULL) {
        type_print_full_name(method->ReturnType, builder);
        strbuilder_char(builder, ' ');
    }
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

System_Reflection_MethodInfo type_iterate_methods_cstr(System_Type type, const char* name, int* index) {
    for (int i = *index; i < type->Methods->Length; i++) {
        if (string_equals_cstr(type->Methods->Data[i]->Name, name)) {
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
    while (targetType != NULL) {
        if (targetType->InterfaceImpls == NULL) {
            targetType = targetType->BaseType;
            continue;
        }

        for (int i = 0; i < targetType->InterfaceImpls->Length; i++) {
            if (targetType->InterfaceImpls->Data[i]->InterfaceType == interfaceType) {
                return targetType->InterfaceImpls->Data[i];
            }
        }

        targetType = targetType->BaseType;
    }

    return NULL;
}

System_Exception assembly_finalizer(System_Reflection_Assembly assembly) {
    // Free the string table
    hmfree(assembly->UserStringsTable);

    // free the custom attributes arrays
    for (int i = 0; i < arrlen(assembly->CustomAttributeMap); i++) {
        arrfree(assembly->CustomAttributeMap[i].value);
    }
    hmfree(assembly->CustomAttributeMap);

    return NULL;
}

bool isinstance(System_Object object, System_Type type) {
    if (object == NULL) {
        return true;
    }
    return type_is_verifier_assignable_to(OBJECT_TYPE(object), type);
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

bool check_field_accessibility(System_Reflection_MethodInfo from_method, System_Reflection_FieldInfo to) {
    if (!check_type_visibility(from_method, to->DeclaringType)) {
        return false;
    }

    System_Type from_type = from_method->DeclaringType;
    bool family = is_same_family(from_type, to->DeclaringType);
    bool assembly = from_type->Assembly == to->DeclaringType->Assembly;

    switch (field_access(to)) {
        case FIELD_COMPILER_CONTROLLED: ASSERT(!"TODO: METHOD_COMPILER_CONTROLLED"); return false;
        case FIELD_PRIVATE: {
            // get the raw to type
            System_Type to_type = to->DeclaringType;
            if (to_type->GenericTypeDefinition != NULL) {
                to_type = to_type->GenericTypeDefinition;
            }

            // get the raw declaring type
            System_Type declaring = from_type;
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

bool check_method_accessibility(System_Reflection_MethodInfo from_method, System_Reflection_MethodInfo to) {
    if (!check_type_visibility(from_method, to->DeclaringType)) {
        return false;
    }

    System_Type from_type = from_method->DeclaringType;
    bool family = is_same_family(from_type, to->DeclaringType);
    bool assembly = from_type->Assembly == to->DeclaringType->Assembly;

    // make sure all the arguments are known
    if (to->GenericArguments != NULL && to->GenericMethodDefinition != NULL) {
        for (int i = 0; i < to->GenericArguments->Length; i++) {
            if (!check_type_visibility(from_method, to->GenericArguments->Data[i])) {
                return false;
            }
        }
    }

    System_Type to_type = to->DeclaringType;
    if (to_type->GenericArguments != NULL && !type_is_generic_definition(to_type)) {
        for (int i = 0; i < to_type->GenericArguments->Length; i++) {
            if (!check_type_visibility(from_method, to_type->GenericArguments->Data[i])) {
                return false;
            }
        }
    }

    switch (method_get_access(to)) {
        case METHOD_COMPILER_CONTROLLED: ASSERT(!"TODO: METHOD_COMPILER_CONTROLLED"); return false;
        case METHOD_PRIVATE: {
            // get the raw to type
            if (to_type->GenericTypeDefinition != NULL) {
                to_type = to_type->GenericTypeDefinition;
            }

            // get the raw declaring type
            System_Type declaring = from_type;
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

bool check_type_visibility(System_Reflection_MethodInfo from_method, System_Type to) {
    type_visibility_t visibility = type_visibility(to);

    // start with easy cases
    if (visibility == TYPE_PUBLIC) {
        // anyone can access this
        return true;
    }

    // check if the type is part of the generic type arguments, if it is then
    // it is visible
    if (from_method->GenericArguments != NULL) {
        for (int i = 0; i < from_method->GenericArguments->Length; i++) {
            if (from_method->GenericArguments->Data[i] == to) {
                return true;
            }
        }
    }

    System_Type from_type = from_method->DeclaringType;
    if (from_type->GenericArguments != NULL) {
        for (int i = 0; i < from_type->GenericArguments->Length; i++) {
            if (from_type->GenericArguments->Data[i] == to) {
                return true;
            }
        }
    }

    if (visibility == TYPE_NOT_PUBLIC) {
        // only the same assembly may access this
        return from_type->Assembly == to->Assembly;
    }

    // the rest only works on nested types
    if (to->DeclaringType == NULL) {
        ASSERT(!"Must be nested");
        return false;
    }

    bool family = is_same_family(from_type, to->DeclaringType);
    bool assembly = from_type->Assembly == to->DeclaringType->Assembly;

    switch (visibility) {
        case TYPE_NESTED_PRIVATE:{
            // get the raw to type
            System_Type to_type = to->DeclaringType;
            if (to_type->GenericTypeDefinition != NULL) {
                to_type = to_type->GenericTypeDefinition;
            }

            // get the raw declaring type
            System_Type declaring = from_type;
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

static err_t expand_field(System_Type type, System_Reflection_FieldInfo field, System_Type_Array arguments, System_Reflection_FieldInfo* out_instance) {
    err_t err = NO_ERROR;
    System_Type fieldType;

    System_Reflection_FieldInfo instance = GC_NEW(tSystem_Reflection_FieldInfo);
    CHECK_AND_RETHROW(expand_type(field->FieldType, arguments, NULL, &fieldType));
    GC_UPDATE(instance, FieldType, fieldType);
    GC_UPDATE(instance, Module, field->Module);
    GC_UPDATE(instance, DeclaringType, type);
    GC_UPDATE(instance, Name, field->Name);
    instance->Attributes = field->Attributes;

    *out_instance = instance;

cleanup:
    return err;
}

static err_t expand_method(System_Type type, System_Reflection_MethodInfo method, System_Type_Array arguments, bool include_method_args, System_Reflection_MethodInfo* out_method) {
    err_t err = NO_ERROR;
    System_Type expanded = NULL;
    System_Type_Array ignore_arguments = include_method_args ? NULL : method->GenericArguments;

    System_Reflection_MethodInfo instance = GC_NEW(tSystem_Reflection_MethodInfo);

    GC_UPDATE(instance, Module, method->Module);
    GC_UPDATE(instance, DeclaringType, type);
    GC_UPDATE(instance, Name, method->Name);
    GC_UPDATE(instance, GenericArguments, method->GenericArguments);
    GC_UPDATE(instance, GenericMethodDefinition, method->GenericMethodDefinition);

    CHECK_AND_RETHROW(expand_type(method->ReturnType, arguments, ignore_arguments, &expanded));
    GC_UPDATE(instance, ReturnType, expanded);

    instance->MethodIndex = method->MethodIndex;
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
                        CHECK_AND_RETHROW(expand_type(methodBody->LocalVariables->Data[i]->LocalType, arguments, ignore_arguments, &expanded));
                        GC_UPDATE(variable, LocalType, expanded);
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
                        CHECK_AND_RETHROW(expand_type(clause->CatchType, arguments, ignore_arguments, &expanded));
                        GC_UPDATE(new_clause, CatchType, expanded);
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
        CHECK_AND_RETHROW(expand_type(fieldParameter->ParameterType, arguments, ignore_arguments, &expanded));
        GC_UPDATE(parameter, ParameterType, expanded);
        GC_UPDATE_ARRAY(instance->Parameters, i, parameter);
    }

    *out_method = instance;

cleanup:
    return err;
}

err_t expand_type(System_Type type, System_Type_Array arguments, System_Type_Array ignore_arguments, System_Type* out_type) {
    err_t err = NO_ERROR;

    if (type == NULL) {
        // type stays as null

    } else if (!type_is_generic_parameter(type)) {
        // type stays as it was

    } else if (type->GenericParameterPosition >= 0) {
        // type is a generic argument, resolve it

        bool ignore_argument = false;
        if (ignore_arguments != NULL) {
            // we have an ignored list, so ignore anything that matches it
            for (int i = 0; i < ignore_arguments->Length; i++) {
                if (ignore_arguments->Data[i] == type) {
                    ignore_argument = true;
                    break;
                }
            }
        }

        if (!ignore_argument) {
            // not in the ignore list, expand it
            ASSERT(type->GenericParameterPosition < arguments->Length);
            type = arguments->Data[type->GenericParameterPosition];
        }

    } else if (type->IsArray) {
        // type is an array, resolve the element type
        System_Type real_array_type;
        CHECK_AND_RETHROW(expand_type(type->ElementType, arguments, ignore_arguments, &real_array_type));
        type = get_array_type(real_array_type);

    } else if (type->IsByRef) {
        // type is a byref, resolve the base type
        System_Type real_byref_base;
        CHECK_AND_RETHROW(expand_type(type->BaseType, arguments, ignore_arguments, &real_byref_base));
        type = get_by_ref_type(real_byref_base);

    } else if (type->GenericArguments != NULL) {
        // type has generic arguments, expand them and create the
        // new generic type
        System_Type definition = type;
        if (type->GenericTypeDefinition != NULL) {
            definition = type->GenericTypeDefinition;
        }

        // expand the types
        System_Type_Array new_arguments = GC_NEW_ARRAY(tSystem_Type, type->GenericArguments->Length);
        for (int i = 0; i < new_arguments->Length; i++) {
            System_Type argt;
            CHECK_AND_RETHROW(expand_type(type->GenericArguments->Data[i], arguments, ignore_arguments, &argt));
            GC_UPDATE_ARRAY(new_arguments, i, argt);
        }

        // create it
        CHECK_AND_RETHROW(type_make_generic(definition, new_arguments, &type));

    } else {
        ASSERT(!"Invalid generic type");
    }

    *out_type = type;

cleanup:
    return err;
}

static err_t find_expanded_method(System_Reflection_MethodInfo method, System_Type_Array arguments, System_Reflection_MethodInfo* out_method) {
    err_t err = NO_ERROR;

    System_Type type;
    CHECK_AND_RETHROW(expand_type(method->DeclaringType, arguments, NULL, &type));

    int index = 0;
    System_Reflection_MethodInfo methodInfo = NULL;
    while ((methodInfo = type_iterate_methods(type, method->Name, &index)) != NULL) {
        // make sure both either have or don't have this
        if (method_is_static(methodInfo) != method_is_static(method)) continue;

        // check the return type and parameters count is the same
        System_Type returnType = NULL;
        CHECK_AND_RETHROW(expand_type(method->ReturnType, arguments, NULL, &returnType));

        if (methodInfo->ReturnType != returnType) continue;
        if (methodInfo->Parameters->Length != method->Parameters->Length) continue;

        // check that the parameters are the same
        bool found = true;
        for (int pi = 0; pi < methodInfo->Parameters->Length; pi++) {

            System_Type parameterType;
            CHECK_AND_RETHROW(expand_type(method->Parameters->Data[pi]->ParameterType, arguments, NULL, &parameterType));
            if (methodInfo->Parameters->Data[pi]->ParameterType != parameterType) {
                found = false;
                break;
            }
        }

        if (found) {
            break;
        }
    }

    ASSERT(methodInfo != NULL);

    *out_method = methodInfo;

cleanup:
    return err;
}

err_t type_expand_interface_impls(System_Type instance, TinyDotNet_Reflection_InterfaceImpl_Array interfaceImpls) {
    err_t err = NO_ERROR;

    GC_UPDATE(instance, InterfaceImpls, GC_NEW_ARRAY(tTinyDotNet_Reflection_InterfaceImpl, interfaceImpls->Length));
    for (int i = 0; i < instance->InterfaceImpls->Length; i++) {
        TinyDotNet_Reflection_InterfaceImpl impl = GC_NEW(tTinyDotNet_Reflection_InterfaceImpl);
        System_Type interfaceType;
        CHECK_AND_RETHROW(expand_type(interfaceImpls->Data[i]->InterfaceType, instance->GenericArguments, NULL, &interfaceType));
        GC_UPDATE(impl, InterfaceType, interfaceType);
        GC_UPDATE_ARRAY(instance->InterfaceImpls, i, impl);
    }

cleanup:
    return err;
}

err_t type_expand_method_impls(System_Type instance, TinyDotNet_Reflection_MethodImpl_Array impls) {
    err_t err = NO_ERROR;

    GC_UPDATE(instance, MethodImpls, GC_NEW_ARRAY(tTinyDotNet_Reflection_MethodImpl, impls->Length));
    for (int i = 0; i < instance->MethodImpls->Length; i++) {
        TinyDotNet_Reflection_MethodImpl impl = GC_NEW(tTinyDotNet_Reflection_MethodImpl);

        System_Reflection_MethodInfo body;
        CHECK_AND_RETHROW(find_expanded_method(impls->Data[i]->Body, instance->GenericArguments, &body));

        System_Reflection_MethodInfo declaration;
        CHECK_AND_RETHROW(find_expanded_method(impls->Data[i]->Declaration, instance->GenericArguments, &declaration));

        GC_UPDATE(impl, Body, body);
        GC_UPDATE(impl, Declaration, declaration);
        GC_UPDATE_ARRAY(instance->MethodImpls, i, impl);
    }

cleanup:
    return err;
}

err_t type_expand_generic(System_Type instance) {
    err_t err = NO_ERROR;
    System_Type type = instance->GenericTypeDefinition;
    System_Type_Array arguments = instance->GenericArguments;

    CHECK(instance->GenericTypeDefinition->IsSetup);

    // base type
    System_Type baseType;
    CHECK_AND_RETHROW(expand_type(type->BaseType, arguments, NULL, &baseType));
    GC_UPDATE(instance, BaseType, baseType);

    // fields
    GC_UPDATE(instance, Fields, GC_NEW_ARRAY(tSystem_Reflection_FieldInfo, type->Fields->Length));
    for (int i = 0; i < instance->Fields->Length; i++) {
        System_Reflection_FieldInfo field;
        CHECK_AND_RETHROW(expand_field(instance, type->Fields->Data[i], arguments, &field));
        GC_UPDATE_ARRAY(instance->Fields, i, field);
    }

    // methods
    GC_UPDATE(instance, Methods, GC_NEW_ARRAY(tSystem_Reflection_MethodInfo, type->Methods->Length));
    for (int i = 0; i < instance->Methods->Length; i++) {
        System_Reflection_MethodInfo method;
        CHECK_AND_RETHROW(expand_method(instance, type->Methods->Data[i], arguments, false, &method));
        GC_UPDATE_ARRAY(instance->Methods, i, method);

        // setup the static ctor if needed
//        if (instance->GenericTypeDefinition->StaticCtor == type->Methods->Data[i]) {
//            GC_UPDATE(instance, StaticCtor, method);
//        }
    }

    // interfaces
    if (type->InterfaceImpls != NULL) {
        CHECK_AND_RETHROW(type_expand_interface_impls(instance, type->InterfaceImpls));
    }

    // method impls
    if (type->MethodImpls != NULL) {
        CHECK_AND_RETHROW(type_expand_method_impls(instance, type->MethodImpls));
    }

cleanup:
    return err;
}

err_t type_make_generic(System_Type type, System_Type_Array arguments, System_Type* out_type) {
    err_t err = NO_ERROR;
    bool locked = false;

    monitor_enter(type);
    locked = true;

    CHECK(type_is_generic_definition(type));
    CHECK(type->GenericArguments->Length == arguments->Length);

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
        *out_type = inst;
        goto cleanup;
    }

    // add it
    bool is_full_instantiation = true;
    for (int i = 0; i < arguments->Length; i++) {
        if (type_is_generic_parameter(arguments->Data[i])) {
            is_full_instantiation = false;
            break;
        }
    }

    // if this is a full generic instance then check if all the arguments
    // properly match the constraints on the generic arguments
    if (is_full_instantiation) {
        for (int i = 0; i < arguments->Length; i++) {
            System_Type arg = type->GenericArguments->Data[i];
            System_Type argType = arguments->Data[arg->GenericParameterPosition];

            // check that we have a reference type
            if (generic_argument_is_reference_type(arg)) {
                CHECK(type_is_object_ref(argType));
            }

            // check that we have a not-nullable value type
            if (generic_argument_is_not_nullable_value_type(arg)) {
                CHECK(type_is_value_type(argType));
            }

            // check that we have a default ctor
            if (generic_argument_is_default_constructor(arg)) {
                if (!type_is_value_type(argType)) {
                    bool found_ctor = false;
                    int index = 0;
                    System_Reflection_MethodInfo info = NULL;
                    while ((info = type_iterate_methods_cstr(argType, ".ctor", &index)) != NULL) {
                        if (!method_is_rt_special_name(info)) continue;
                        if (info->Parameters->Length == 0) {
                            found_ctor = true;
                            break;
                        }
                    }
                    CHECK(found_ctor);
                }
            }
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
    instance->Attributes = type->Attributes;
    instance->GenericParameterPosition = -1;

    // handle the value type memes in here
    if (type->BaseType == tSystem_ValueType) {
        instance->IsValueType = true;
        instance->StackType = STACK_TYPE_VALUE_TYPE;
    }

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
    GC_UPDATE(instance, NextGenericInstance, type->NextGenericInstance);
    GC_UPDATE(type, NextGenericInstance, instance);

    // unlock it
    locked = false;
    monitor_exit(type);

    // only expand if the type is setup properly, otherwise we are going
    // to expand it in a later stage once it is actually initialized
    if (type->IsSetupFinished) {
        CHECK_AND_RETHROW(type_expand_generic(instance));
    }

    *out_type = instance;

cleanup:
    if (locked) {
        monitor_exit(type);
    }

    return err;
}

err_t method_make_generic(System_Reflection_MethodInfo method, System_Type_Array arguments, System_Reflection_MethodInfo* out_method) {
    err_t err = NO_ERROR;

    monitor_enter(method);

    CHECK(!type_is_generic_definition(method->DeclaringType));

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
        *out_method = inst;
        goto cleanup;
    }

    System_Reflection_MethodInfo instance;
    CHECK_AND_RETHROW(expand_method(method->DeclaringType, method, arguments, true, &instance));

    // create the unique name
    strbuilder_t builder = strbuilder_new();
    strbuilder_utf16(&builder, instance->Name->Chars, instance->Name->Length);
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

    // set the generic arguments
    GC_UPDATE(instance, GenericArguments, arguments);
    GC_UPDATE(instance, GenericMethodDefinition, method);

    // insert to the list
    GC_UPDATE(instance, NextGenericInstance, method->NextGenericInstance);
    GC_UPDATE(method, NextGenericInstance, instance);

    *out_method = instance;

cleanup:
    monitor_exit(method);

    return err;
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

