#pragma once

#include <stddef.h>
#include "reflection.h"
#include "tinydotnet/except.h"
#include "dotnet/types.h"

#define TDN_TYPE_VISIBILITY_NOT_PUBLIC 0
#define TDN_TYPE_VISIBILITY_PUBLIC 1
#define TDN_TYPE_VISIBILITY_NESTED_PUBLIC 2
#define TDN_TYPE_VISIBILITY_NESTED_PRIVATE 3
#define TDN_TYPE_VISIBILITY_NESTED_FAMILY 4
#define TDN_TYPE_VISIBILITY_NESTED_ASSEMBLY 5
#define TDN_TYPE_VISIBILITY_NESTED_FAMILY_AND_ASSEMBLY 6
#define TDN_TYPE_VISIBILITY_NESTED_FAMILY_OR_ASSEMBLY 7

#define TDN_TYPE_LAYOUT_AUTO 0
#define TDN_TYPE_LAYOUT_SEQUENTIAL 1
#define TDN_TYPE_LAYOUT_EXPLICIT 2

#define TDN_TYPE_STRING_FORMAT_ANSI 0
#define TDN_TYPE_STRING_FORMAT_UNICODE 1
#define TDN_TYPE_STRING_FORMAT_AUTO 2
#define TDN_TYPE_STRING_FORMAT_CUSTOM 3

typedef union System_Reflection_TypeAttributes {
    struct {
        uint32_t Visibility : 3;
        uint32_t Layout : 2;
        uint32_t Interface : 1;
        uint32_t : 1;
        uint32_t Abstract : 1;
        uint32_t Sealed : 1;
        uint32_t : 1;
        uint32_t SpecialName : 1;
        uint32_t RTSpecialName : 1;
        uint32_t Import : 1;
        uint32_t Serializable : 1;
        uint32_t WindowsRuntime : 1;
        uint32_t : 1;
        uint32_t StringFormat : 2;
        uint32_t HasSecurity : 1;
        uint32_t : 1;
        uint32_t BeforeFieldInit : 1;
        uint32_t : 1;
        uint32_t CustomFormat : 2;
        uint32_t : 8;
    };
    uint32_t Value;
} System_Reflection_TypeAttributes;

#define TDN_GENERIC_PARAM_VARIANCE_COVARIANT                    1
#define TDN_GENERIC_PARAM_VARIANCE_CONTRAVARIANT                2

#define TDN_GENERIC_PARAM_CONSTRAINT_REFERENCE_TYPE             1
#define TDN_GENERIC_PARAM_CONSTRAINT_NON_NULLABLE_VALUE_TYPE    2
#define TDN_GENERIC_PARAM_CONSTRAINT_DEFAULT_CONSTRUCTOR        3

typedef union System_Reflection_GenericParameterAttributes {
    struct {
        uint16_t Variance : 2;
        uint16_t SpecialConstraint : 3;
        uint16_t : 11;
    };
    uint16_t value;
} System_Reflection_GenericParameterAttributes;

typedef struct tdn_generic_type_instance {
    uint64_t key;
    System_Type value;
} tdn_generic_type_instance_t;

typedef struct System_Type {
    struct System_Reflection_MemberInfo;

    // cached
    _Atomic(System_Type) ArrayType;
    _Atomic(System_Type) ByRefType;
    _Atomic(System_Type) PointerType;

    System_Reflection_Assembly Assembly;
    System_String Namespace;
    System_Type BaseType;
    System_Type ElementType;
    System_Type EnumUnderlyingType;

    System_Reflection_FieldInfo_Array Fields;
    System_Reflection_MethodInfo_Array Methods;

    System_Type_Array GenericArguments;
    System_Type GenericTypeDefinition;
    System_Reflection_MethodInfo DeclaringMethod;

    tdn_generic_type_instance_t* GenericTypeInstances;

    System_Reflection_TypeAttributes Attributes;
    uint32_t StackSize;
    uint32_t StackAlignment;
    uint32_t HeapSize;
    uint32_t HeapAlignment;
    uint32_t Packing;

    uint32_t StartedFillingSize : 1;
    uint32_t FinishedFillingSize : 1;
    uint32_t : 30;

    int GenericParameterPosition;
    System_Reflection_GenericParameterAttributes GenericParameterAttributes;
}* System_Type;

static inline bool tdn_type_is_generic(System_Type type) { return type->GenericArguments != NULL; }
static inline bool tdn_is_generic_type_definition(System_Type type) { return type->GenericTypeDefinition == type; }
static inline bool tdn_is_generic_parameter(System_Type type) { return type->GenericParameterPosition != -1; }
static inline bool tdn_is_generic_method_parameter(System_Type type) { return type->DeclaringMethod != NULL; }
static inline bool tdn_is_generic_type_parameter(System_Type type) { return  type->DeclaringType != NULL; }

bool tdn_type_is_valuetype(System_Type type);

tdn_err_t tdn_get_array_type(System_Type type, System_Type* out_type);

tdn_err_t tdn_get_byref_type(System_Type type, System_Type* out_type);

tdn_err_t tdn_get_pointer_type(System_Type type, System_Type* out_type);

tdn_err_t tdn_type_make_generic(System_Type type, System_Type_Array arguments, System_Type* instance);
