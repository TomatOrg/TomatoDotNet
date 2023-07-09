#pragma once

#include "basic.h"
#include <stddef.h>

typedef struct RuntimeAssembly* RuntimeAssembly;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct RuntimeModule {
    struct Object Object;
    RuntimeAssembly Assembly;
}* RuntimeModule;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Common MemberInfo
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// NOTE: does not exists, just used as a common type
//       for our classes
typedef struct RuntimeMemberInfo {
    struct Object Object;
    RuntimeTypeInfo DeclaringType;
    RuntimeModule Module;
    String Name;
    int MetadataToken;
}* RuntimeMemberInfo;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The parameter information we have
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef union ParameterAttributes {
    struct {
        uint32_t In : 1;
        uint32_t Out : 1;
        uint32_t Lcid : 1;
        uint32_t Retval : 1;
        uint32_t Optional : 1;
        uint32_t : 7;
        uint32_t HasDefault : 1;
        uint32_t HasFieldMarshal : 1;
        uint32_t : 2;
    };
    uint32_t Attributes;
} ParameterAttributes;

typedef struct ParameterInfo {
    struct Object Object;
    ParameterAttributes Attributes;
    RuntimeTypeInfo ParameterType;
    RuntimeMemberInfo Member;
    String Name;
    int Position;
}* ParameterInfo;
DEFINE_ARRAY(ParameterInfo);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The field information we have
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef union FieldAttributes {
    struct {
        uint32_t FieldAccess : 3;
#define TDN_FIELD_ACCESS_PRIVATE_SCOPE 0
#define TDN_FIELD_ACCESS_PRIVATE 1
#define TDN_FIELD_ACCESS_FAMILY_AND_ASSEMBLY 2
#define TDN_FIELD_ACCESS_ASSEMBLY 3
#define TDN_FIELD_ACCESS_FAMILY 4
#define TDN_FIELD_ACCESS_FAMILY_OR_ASSEMBLY 5
#define TDN_FIELD_ACCESS_PUBLIC 6
        uint32_t : 1;
        uint32_t Static : 1;
        uint32_t InitOnly : 1;
        uint32_t Literal : 1;
        uint32_t NotSerialized : 1;
        uint32_t HasFieldRVA : 1;
        uint32_t SpecialName : 1;
        uint32_t RTSpecialName : 1;
        uint32_t : 1;
        uint32_t HasFieldMarshal : 1;
        uint32_t PinvokeImpl : 1;
        uint32_t : 1;
        uint32_t HasDefault : 1;
    };
    uint32_t Attributes;
} FieldAttributes;

typedef struct RuntimeFieldInfo {
    struct RuntimeMemberInfo;
    FieldAttributes Attributes;
    RuntimeTypeInfo FieldType;
    int FieldOffset;
}* RuntimeFieldInfo;
DEFINE_ARRAY(RuntimeFieldInfo);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The method body
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct RuntimeLocalVariableInfo {
    struct Object Object;
    RuntimeTypeInfo LocalType;
    int LocalIndex;
    bool IsPinned;
}* RuntimeLocalVariableInfo;
DEFINE_ARRAY(RuntimeLocalVariableInfo);

typedef struct RuntimeMethodBody {
    struct Object Object;
    RuntimeLocalVariableInfo_Array LocalVariables;
    uint8_t* IL;
    int ILSize;
    int LocalSignatureMetadataToken;
    int MaxStackSize;
    bool InitLocals;
}* RuntimeMethodBody;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The method information we have
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef union MethodAttributes {
    struct {
        uint32_t MemberAccess : 3;
#define TDN_METHOD_ACCESS_PRIVATE_SCOPE 0
#define TDN_METHOD_ACCESS_PRIVATE 1
#define TDN_METHOD_ACCESS_FAMILY_AND_ASSEMBLY 2
#define TDN_METHOD_ACCESS_ASSEMBLY 3
#define TDN_METHOD_ACCESS_FAMILY 4
#define TDN_METHOD_ACCESS_FAMILY_OR_ASSEMBLY 5
#define TDN_METHOD_ACCESS_PUBLIC 6
        uint32_t UnmanagedExport : 1;
        uint32_t Static : 1;
        uint32_t Final : 1;
        uint32_t Virtual : 1;
        uint32_t HideBySig : 1;
        uint32_t VtableNewSlot : 1;
        uint32_t Strict : 1;
        uint32_t Abstract : 1;
        uint32_t SpecialName : 1;
        uint32_t RTSpecialName : 1;
        uint32_t PinvokeImpl : 1;
        uint32_t HasSecurity : 1;
        uint32_t RequireSecObject : 1;
    };
    uint32_t Attributes;
} MethodAttributes;

typedef union MethodImplAttributes {
    struct {
        uint32_t CodeType : 2;
#define TDN_METHOD_IMPL_CODE_TYPE_IL        0
#define TDN_METHOD_IMPL_CODE_TYPE_NATIVE    1
#define TDN_METHOD_IMPL_CODE_TYPE_OPTIL     2
#define TDN_METHOD_IMPL_CODE_TYPE_RUNTIME   3
        uint32_t Unmanaged : 1;
        uint32_t NoInlining : 1;
        uint32_t ForwardRef : 1;
        uint32_t Synchronized : 1;
        uint32_t NoOptimization : 1;
        uint32_t PreserveSig : 1;
        uint32_t AggressiveInlining : 1;
        uint32_t AggressiveOptimization : 1;
        uint32_t : 2;
        uint32_t InternalCall : 1;
        uint32_t : 3;
    };
    uint32_t Attributes;
} MethodImplAttributes;

// This is an abstract type, just used for proper array creation
typedef struct MethodBase {
    struct Object Object;
}* MethodBase;
DEFINE_ARRAY(MethodBase)

typedef struct RuntimeMethodInfo* RuntimeMethodInfo;

typedef struct generic_method_instance {
    size_t key;
    RuntimeMethodInfo value;
} generic_method_instance_t;

// NOTE: does not exists, just used as a common type
//       for our classes
typedef struct RuntimeMethodBase {
    struct RuntimeMemberInfo;
    ParameterInfo_Array Parameters;
    MethodAttributes Attributes;
    MethodImplAttributes MethodImplFlags;
    RuntimeMethodBody MethodBody;
    ParameterInfo ReturnParameter;

    // TODO: can we move this to be in RuntimeMethodInfo? do we want to?
    RuntimeTypeInfo_Array GenericArguments;
    RuntimeMethodInfo GenericMethodDefinition;
    generic_method_instance_t* GenericMethodInstances;
}* RuntimeMethodBase;
DEFINE_ARRAY(RuntimeMethodBase);

struct RuntimeMethodInfo {
    struct RuntimeMethodBase;
};
DEFINE_ARRAY(RuntimeMethodInfo);

typedef struct RuntimeConstructorInfo {
    struct RuntimeMethodBase;
}* RuntimeConstructorInfo;
DEFINE_ARRAY(RuntimeConstructorInfo);

tdn_err_t tdn_make_generic_method(RuntimeMethodInfo type,
                                  RuntimeTypeInfo_Array arguments,
                                  RuntimeMethodInfo* instance);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The assembly information we have
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct dotnet_file dotnet_file_t;

typedef struct RuntimeAssembly {
    struct Object Object;

    // the prepared functions
    RuntimeTypeInfo_Array TypeDefs;
    RuntimeMethodBase_Array MethodDefs;
    RuntimeFieldInfo_Array Fields;

    // the metadata of this assembly
    dotnet_file_t* Metadata;
    RuntimeModule Module;
}* RuntimeAssembly;

tdn_err_t tdn_assembly_lookup_type(
    RuntimeAssembly assembly,
    int metadata_token,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    RuntimeTypeInfo* type);

tdn_err_t tdn_assembly_lookup_method(
    RuntimeAssembly assembly,
    int metadata_token,
    RuntimeTypeInfo_Array typeArgs, RuntimeTypeInfo_Array methodArgs,
    RuntimeMethodBase* method);

tdn_err_t tdn_assembly_lookup_field(
    RuntimeAssembly assembly,
    int metadata_token,
    RuntimeFieldInfo* field);

tdn_err_t tdn_assembly_lookup_type_by_cstr(
    RuntimeAssembly assembly,
    const char* namespace, const char* name,
    RuntimeTypeInfo* type);
