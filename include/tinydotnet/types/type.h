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

typedef union TypeAttributes {
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
    };
    uint32_t Value;
} TypeAttributes;

#define TDN_GENERIC_PARAM_VARIANCE_COVARIANT                    1
#define TDN_GENERIC_PARAM_VARIANCE_CONTRAVARIANT                2

#define TDN_GENERIC_PARAM_CONSTRAINT_REFERENCE_TYPE             1
#define TDN_GENERIC_PARAM_CONSTRAINT_NON_NULLABLE_VALUE_TYPE    2
#define TDN_GENERIC_PARAM_CONSTRAINT_DEFAULT_CONSTRUCTOR        3

typedef union GenericParameterAttributes {
    struct {
        uint32_t Variance : 2;
        uint32_t SpecialConstraint : 3;
    };
    uint32_t value;
} GenericParameterAttributes;

typedef struct RuntimeTypeInfo {
    struct RuntimeMemberInfo;

    // the subtypes that can be created from this
    _Atomic(RuntimeTypeInfo) ArrayType;
    _Atomic(RuntimeTypeInfo) ByRefType;
    _Atomic(RuntimeTypeInfo) PointerType;

    // The namespace of the type
    String Namespace;
    TypeAttributes Attributes;

    // The base of the type
    RuntimeTypeInfo BaseType;

    // the declared stuff
    RuntimeConstructorInfo_Array DeclaredConstructors;
    RuntimeMethodInfo_Array DeclaredMethods;
    RuntimeFieldInfo_Array DeclaredFields;

    // elements
    RuntimeTypeInfo ElementType;
    RuntimeTypeInfo EnumUnderlyingType;

    // size related
    uint32_t StackSize;
    uint32_t StackAlignment;
    uint32_t HeapSize;
    uint32_t HeapAlignment;
    uint32_t Packing;

    // Generics related
    RuntimeMethodBase DeclaringMethod;
    RuntimeTypeInfo_Array GenericArguments;
    RuntimeTypeInfo GenericTypeDefinition;
    GenericParameterAttributes GenericParameterAttributes;
    int GenericParameterPosition;

    // initialization stage
    uint32_t FillingStackSize : 1;
    uint32_t EndFillingStackSize : 1;
    uint32_t FillingHeapSize : 1;
    uint32_t EndFillingHeapSize : 1;
    uint32_t QueuedTypeInit : 1;
    uint32_t IsGenericParameter : 1;
}* RuntimeTypeInfo;

static inline bool tdn_type_is_generic(RuntimeTypeInfo type) { return type->GenericArguments != NULL; }
static inline bool tdn_is_generic_type_definition(RuntimeTypeInfo type) { return type->GenericTypeDefinition == type; }
static inline bool tdn_is_generic_parameter(RuntimeTypeInfo type) { return type->GenericParameterPosition != -1; }
static inline bool tdn_is_generic_method_parameter(RuntimeTypeInfo type) { return type->DeclaringMethod != NULL; }
static inline bool tdn_is_generic_type_parameter(RuntimeTypeInfo type) { return  type->DeclaringType != NULL; }

bool tdn_type_contains_generic_parameters(RuntimeTypeInfo type);

bool tdn_type_is_valuetype(RuntimeTypeInfo type);

tdn_err_t tdn_get_array_type(RuntimeTypeInfo type,
                             RuntimeTypeInfo* out_type);

tdn_err_t tdn_get_byref_type(RuntimeTypeInfo type,
                             RuntimeTypeInfo* out_type);

tdn_err_t tdn_get_pointer_type(RuntimeTypeInfo type,
                               RuntimeTypeInfo* out_type);

tdn_err_t tdn_type_make_generic(RuntimeTypeInfo type,
                                RuntimeTypeInfo_Array arguments,
                                RuntimeTypeInfo* instance);
