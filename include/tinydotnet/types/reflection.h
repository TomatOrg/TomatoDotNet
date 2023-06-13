#pragma once

#include "basic.h"

typedef struct System_Reflection_Assembly* System_Reflection_Assembly;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_Module {
    struct System_Object;
    System_Reflection_Assembly Assembly;
    System_String Name;
    System_Guid ModuleVersionId;
    int MetadataToken;
}* System_Reflection_Module;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_MemberInfo {
    struct System_Object;
    System_Type DeclaringType;
    System_Reflection_Module Module;
    System_String Name;
    int MetadataToken;
}* System_Reflection_MemberInfo;
DEFINE_ARRAY(System_Reflection_MemberInfo);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum System_Reflection_ExceptionHandlingClauseOptions {
    TDN_EXCEPTION_HANDLING_CLAUSE = 0,
    TDN_EXCEPTION_HANDLING_FAULT = 4,
    TDN_EXCEPTION_HANDLING_FILTER = 1,
    TDN_EXCEPTION_HANDLING_FINALLY = 2
} System_Reflection_ExceptionHandlingClauseOptions;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_ExceptionHandlingClause {
    struct System_Object;
    System_Type CatchType;
    int TryLength;
    int TryOffset;
    int FilterOffset;
    int HandlerLength;
    int HandlerOffset;
    System_Reflection_ExceptionHandlingClauseOptions Flags;
}* System_Reflection_ExceptionHandlingClause;
DEFINE_ARRAY(System_Reflection_ExceptionHandlingClause);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_LocalVariableInfo {
    struct System_Object;
    System_Type Type;
    int Index;
}* System_Reflection_LocalVariableInfo;
DEFINE_ARRAY(System_Reflection_LocalVariableInfo);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_MethodBody {
    struct System_Object;
    System_Byte_Array IL;
    System_Reflection_ExceptionHandlingClause_Array ExceptionHandlingClauses;
    System_Reflection_LocalVariableInfo_Array LocalVariables;
    int LocalSignatureMetadataToken;
    int MaxStackSize;
    bool InitLocals;
}* System_Reflection_MethodBody;
DEFINE_ARRAY(System_Reflection_MethodBody);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef union System_Reflection_ParameterAttributes {
    struct {
        uint16_t In : 1;
        uint16_t Out : 1;
        uint16_t Lcid : 1;
        uint16_t Retval : 1;
        uint16_t Optional : 1;
        uint16_t : 7;
        uint16_t HasDefault : 1;
        uint16_t HasFieldMarshal : 1;
        uint16_t : 2;
    };
    uint16_t Attributes;
} System_Reflection_ParameterAttributes;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_ParameterInfo {
    struct System_Object;
    System_String Name;
    System_Type ParameterType;
    System_Reflection_MemberInfo Member;
    int Position;
    int MetadataToken;
    System_Reflection_ParameterAttributes Attributes;
}* System_Reflection_ParameterInfo;
DEFINE_ARRAY(System_Reflection_ParameterInfo);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef union System_Reflection_MethodImplAttributes {
    struct {
        uint16_t CodeType : 2;
#define TDN_METHOD_IMPL_CODE_TYPE_IL        0
#define TDN_METHOD_IMPL_CODE_TYPE_NATIVE    1
#define TDN_METHOD_IMPL_CODE_TYPE_OPTIL     2
#define TDN_METHOD_IMPL_CODE_TYPE_RUNTIME   3
        uint16_t Unmanaged : 1;
        uint16_t NoInlining : 1;
        uint16_t ForwardRef : 1;
        uint16_t Synchronized : 1;
        uint16_t NoOptimization : 1;
        uint16_t PreserveSig : 1;
        uint16_t AggressiveInlining : 1;
        uint16_t AggressiveOptimization : 1;
        uint16_t : 2;
        uint16_t InternalCall : 1;
        uint16_t : 3;
    };
    uint16_t Attributes;
} System_Reflection_MethodImplAttributes;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef union System_Reflection_MethodAttributes {
    struct {
        uint16_t MemberAccess : 3;
#define TDN_METHOD_ACCESS_PRIVATE_SCOPE 0
#define TDN_METHOD_ACCESS_PRIVATE 1
#define TDN_METHOD_ACCESS_FAMILY_AND_ASSEMBLY 2
#define TDN_METHOD_ACCESS_ASSEMBLY 3
#define TDN_METHOD_ACCESS_FAMILY 4
#define TDN_METHOD_ACCESS_FAMILY_OR_ASSEMBLY 5
#define TDN_METHOD_ACCESS_PUBLIC 6
        uint16_t UnmanagedExport : 1;
        uint16_t Static : 1;
        uint16_t Final : 1;
        uint16_t Virtual : 1;
        uint16_t HideBySig : 1;
        uint16_t VtableNewSlot : 1;
        uint16_t CheckAccessOnOverride : 1;
        uint16_t Abstract : 1;
        uint16_t SpecialName : 1;
        uint16_t RTSpecialName : 1;
        uint16_t PinvokeImpl : 1;
        uint16_t HasSecurity : 1;
        uint16_t RequireSecObject : 1;
    };
    uint16_t Attributes;
} System_Reflection_MethodAttributes;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_MethodBase {
    struct System_Reflection_MemberInfo;
    System_Reflection_MethodBody MethodBody;
    System_Reflection_ParameterInfo_Array Parameters;
    System_Reflection_MethodImplAttributes ImplementationFlags;
    System_Reflection_MethodAttributes Attributes;
}* System_Reflection_MethodBase;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_MethodInfo {
    struct System_Reflection_MethodBase;
    System_Reflection_ParameterInfo ReturnParameter;
    struct System_Reflection_MethodInfo* BaseDefinition;
    struct System_Reflection_MethodInfo* GenericMethodDefinition;
    System_Type_Array GenericArguments;
}* System_Reflection_MethodInfo;

DEFINE_ARRAY(System_Reflection_MethodInfo);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef union System_Reflection_FieldAttributes {
    struct {
        uint16_t FieldAccess : 3;
#define TDN_FIELD_ACCESS_PRIVATE_SCOPE 0
#define TDN_FIELD_ACCESS_PRIVATE 1
#define TDN_FIELD_ACCESS_FAMILY_AND_ASSEMBLY 2
#define TDN_FIELD_ACCESS_ASSEMBLY 3
#define TDN_FIELD_ACCESS_FAMILY 4
#define TDN_FIELD_ACCESS_FAMILY_OR_ASSEMBLY 5
#define TDN_FIELD_ACCESS_PUBLIC 6
        uint16_t : 1;
        uint16_t Static : 1;
        uint16_t InitOnly : 1;
        uint16_t Literal : 1;
        uint16_t NotSerialized : 1;
        uint16_t HasFieldRVA : 1;
        uint16_t SpecialName : 1;
        uint16_t RTSpecialName : 1;
        uint16_t : 1;
        uint16_t HasFieldMarshal : 1;
        uint16_t PinvokeImpl : 1;
        uint16_t : 1;
        uint16_t HasDefault : 1;
    };
    uint16_t Attributes;
} System_Reflection_FieldAttributes;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_FieldInfo {
    struct System_Reflection_MemberInfo;
    System_Type FieldType;
    System_Reflection_FieldAttributes Attributes;
    uint32_t FieldOffset;
    uint8_t FilledOffset : 1;
    uint8_t : 7;
}* System_Reflection_FieldInfo;
DEFINE_ARRAY(System_Reflection_FieldInfo);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct TinyDotNet_Reflection_MemberReference {
    System_String Name;
    int ParentToken;
    System_Byte_Array Signature;
} TinyDotNet_Reflection_MemberReference;
DEFINE_ARRAY(TinyDotNet_Reflection_MemberReference);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct TinyDotNet_Reflection_TypeSpecification {
    System_Byte_Array Signature;
} TinyDotNet_Reflection_TypeSpecification;
DEFINE_ARRAY(TinyDotNet_Reflection_TypeSpecification);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_Assembly {
    struct System_Object;
    System_Reflection_Module Module;
    System_Type_Array TypeDefs;
    System_Reflection_MethodInfo_Array MethodDefs;
    System_Reflection_FieldInfo_Array Fields;
    TinyDotNet_Reflection_MemberReference_Array MemberRefs;
    TinyDotNet_Reflection_TypeSpecification_Array TypeSpecs;
}* System_Reflection_Assembly;
DEFINE_ARRAY(System_Reflection_Assembly);

tdn_err_t tdn_assembly_lookup_type(
    System_Reflection_Assembly assembly,
    int metadata_token,
    System_Type_Array typeArgs, System_Type_Array methodArgs,
    System_Type* type);

tdn_err_t tdn_assembly_lookup_method(
    System_Reflection_Assembly assembly,
    int metadata_token,
    System_Type_Array typeArgs, System_Type_Array methodArgs,
    System_Reflection_MethodInfo* method);

tdn_err_t tdn_assembly_lookup_field(
    System_Reflection_Assembly assembly,
    int metadata_token,
    System_Reflection_FieldInfo* field);

System_Type tdn_assembly_lookup_type_by_cstr(
    System_Reflection_Assembly assembly,
    const char* namespace, const char* name);
