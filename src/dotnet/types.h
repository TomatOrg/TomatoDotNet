#pragma once

#include "metadata/metadata_spec.h"
#include "metadata/metadata.h"

#include <util/strbuilder.h>
#include <sync/mutex.h>

#include <mir/mir.h>

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Object *System_Object;
typedef struct System_Type *System_Type;
typedef struct System_Reflection_MethodInfo *System_Reflection_MethodInfo;
typedef struct System_Reflection_FieldInfo *System_Reflection_FieldInfo;

typedef struct System_Guid {
    uint32_t a;
    uint16_t b;
    uint16_t c;
    uint8_t d;
    uint8_t e;
    uint8_t f;
    uint8_t g;
    uint8_t h;
    uint8_t i;
    uint8_t j;
    uint8_t k;
} System_Guid;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_GCMemoryInfo {
    uint64_t FinalizationPendingCount;
    uint64_t FragmentedBytes;
    int Generation;
    // TODO: generation info
    uint64_t HeapSizeBytes;
    uint64_t HighMemoryLoadThresholdBytes;
    uint64_t Index;
    uint64_t MemoryLoadBytes;
    // TODO: Pause Duration
    double PauseTimePercentage;
    uint64_t TotalAvailableMemoryBytes;
    uint64_t TotalCommittedBytes;
} System_GCMemoryInfo;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_ValueType {
    // empty...
} System_ValueType;

typedef struct System_Enum {
    // empty...
} System_Enum;

typedef bool System_Boolean;
typedef __CHAR16_TYPE__ System_Char;
typedef int8_t System_SByte;
typedef uint8_t System_Byte;
typedef int16_t System_Int16;
typedef uint16_t System_UInt16;
typedef int32_t System_Int32;
typedef uint32_t System_UInt32;
typedef int64_t System_Int64;
typedef uint64_t System_UInt64;
typedef float System_Single;
typedef double System_Double;
typedef intptr_t System_IntPtr;
typedef uintptr_t System_UIntPtr;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Represents a dotnet object
 */

// Store only the lower 48 bits of a pointer
// if this is hosted usermode TDN, the remaning bits will be zero (only without LA57, TODO:)
// if this is Pentagon, all allocations are in the higher half kernel heap
#ifdef PENTAGON_HOSTED
#define OBJECT_TYPE(obj) ((System_Type)((obj)->type))
#else
#define OBJECT_TYPE(obj) ((System_Type)((obj)->type | 0xFFFF000000000000))
#endif
struct System_Object {
    // the vtable of the object
    void** vtable;

    // the type of the object
    uint64_t type : 48;

    // the color of the object
    uint64_t color : 3;
#define COLOR_BLUE      0   /* unallocated object */
#define COLOR_WHITE     1   /* object that has not been traced */
#define COLOR_GRAY      2   /* object that has been traced, but its children have not been traced yet */
#define COLOR_BLACK     3   /* object that has been traced, and its children have been traced as well */
#define COLOR_YELLOW    4   /* object that has not been traced (for color switching) */
#define COLOR_GREEN     5   /* object that should be finalized */
#define COLOR_RESERVED0 6   /* reserved for future use */
#define COLOR_RESERVED1 7   /* reserved for future use */

    // should finalizer be called or not
    uint64_t suppress_finalizer : 1;

    // unused for now
    uint64_t _reserved : 12;
};
STATIC_ASSERT(sizeof(struct System_Object) == sizeof(void*) * 2);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Span {
    uintptr_t Ptr;
    int32_t Length;
} System_Span;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Nullable {
    bool HasValue;
} System_Nullable;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Array {
    struct System_Object;
    int Length;
} *System_Array;

#define DEFINE_ARRAY(type) \
    typedef struct type##_Array { \
        struct System_Array; \
        type Data[0];\
    } *type##_Array;

DEFINE_ARRAY(System_Type);
DEFINE_ARRAY(System_Reflection_MethodInfo);
DEFINE_ARRAY(System_Byte);
DEFINE_ARRAY(System_Object);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_String {
    struct System_Object;
    int Length;
    System_Char Chars[];
} *System_String;

DEFINE_ARRAY(System_String);

bool string_equals_cstr(System_String a, const char* b);

bool string_equals(System_String a, System_String b);

/**
 * Append a c null terminated ascii string to the given string, this
 * creates a new copy of the string
 *
 * @param old   [IN] The old string to append to
 * @param str   [IN] The string to append
 */
System_String string_append_cstr(System_String old, const char* str);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_Module *System_Reflection_Module;
typedef struct System_Reflection_Assembly *System_Reflection_Assembly;
typedef struct System_Reflection_MemberInfo *System_Reflection_MemberInfo;

DEFINE_ARRAY(System_Reflection_Module);
DEFINE_ARRAY(System_Reflection_Assembly);
DEFINE_ARRAY(System_Reflection_FieldInfo);
DEFINE_ARRAY(System_Reflection_MemberInfo);
DEFINE_ARRAY(System_Int32);
DEFINE_ARRAY(System_Byte_Array);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct System_Reflection_Module {
    struct System_Object;
    System_Reflection_Assembly Assembly;
    System_String Name;
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct System_Reflection_MemberInfo {
    struct System_Object;
    System_Type DeclaringType;
    System_Reflection_Module Module;
    System_String Name;
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct System_Reflection_FieldInfo {
    struct System_Reflection_MemberInfo;
    System_Type FieldType;
    union {
        // for instance fields
        uintptr_t MemoryOffset;

        // for static fields
        MIR_item_t MirField;

        // for thread statics
        uintptr_t ThreadStaticIndex;
    };
    uint16_t Attributes;
};

typedef enum field_access {
    FIELD_COMPILER_CONTROLLED,
    FIELD_PRIVATE,
    FIELD_FAMILY_AND_ASSEMBLY,
    FIELD_ASSEMBLY,
    FIELD_FAMILY,
    FIELD_FAMILY_OR_ASSEMBLY,
    FIELD_PUBLIC,
} field_access_t;

static inline field_access_t field_access(System_Reflection_FieldInfo field) { return field->Attributes & 0b111; }
static inline bool field_is_static(System_Reflection_FieldInfo field) { return field->Attributes & 0x0010; }
static inline bool field_is_init_only(System_Reflection_FieldInfo field) { return field->Attributes & 0x0020; }
static inline bool field_is_literal(System_Reflection_FieldInfo field) { return field->Attributes & 0x0040; }

bool field_is_thread_static(System_Reflection_FieldInfo field);

const char* field_access_str(field_access_t access);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_ParameterInfo {
    struct System_Object;
    uint16_t Attributes;
    System_String Name;
    System_Type ParameterType;
} *System_Reflection_ParameterInfo;

DEFINE_ARRAY(System_Reflection_ParameterInfo);

static inline bool parameter_is_in(System_Reflection_ParameterInfo param) { return param->Attributes & 0x0001; }
static inline bool parameter_is_out(System_Reflection_ParameterInfo param) { return param->Attributes & 0x0002; }
static inline bool parameter_is_optional(System_Reflection_ParameterInfo param) { return param->Attributes & 0x0010; }
static inline bool parameter_has_default(System_Reflection_ParameterInfo param) { return param->Attributes & 0x1000; }
static inline bool parameter_has_field_marshal(System_Reflection_ParameterInfo param) { return param->Attributes & 0x2000; }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_LocalVariableInfo {
    struct System_Object;
    int LocalIndex;
    System_Type LocalType;
} *System_Reflection_LocalVariableInfo;

DEFINE_ARRAY(System_Reflection_LocalVariableInfo);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum System_Reflection_ExceptionHandlingClauseOptions {
    ExceptionHandlingClauseOptions_Clause = 0,
    ExceptionHandlingClauseOptions_Fault = 4,
    ExceptionHandlingClauseOptions_Filter = 1,
    ExceptionHandlingClauseOptions_Finally = 2,
} System_Reflection_ExceptionHandlingClauseOptions;

typedef struct System_Reflection_ExceptionHandlingClause {
    struct System_Object;
    System_Type CatchType;
    int FilterOffset;
    System_Reflection_ExceptionHandlingClauseOptions Flags;
    int HandlerLength;
    int HandlerOffset;
    int TryLength;
    int TryOffset;
} *System_Reflection_ExceptionHandlingClause;

DEFINE_ARRAY(System_Reflection_ExceptionHandlingClause);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_MethodBody {
    struct System_Object;
    System_Reflection_ExceptionHandlingClause_Array ExceptionHandlingClauses;
    System_Reflection_LocalVariableInfo_Array LocalVariables;
    bool InitLocals;
    int32_t MaxStackSize;
    System_Byte_Array Il;
} *System_Reflection_MethodBody;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_MethodBase {
    struct System_Reflection_MemberInfo;
    uint16_t ImplAttributes;
    uint16_t Attributes;
    System_Reflection_MethodBody MethodBody;
    System_Reflection_ParameterInfo_Array Parameters;
    System_Type_Array GenericArguments;
    System_Reflection_MethodInfo GenericMethodDefinition;
} *System_Reflection_MethodBase;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

struct System_Reflection_MethodInfo {
    struct System_Reflection_MethodBase;
    System_Type ReturnType;
    bool IsFilled;
    int VTableOffset;
    int MethodIndex;
    MIR_item_t MirFunc;
    MIR_item_t MirUnboxerFunc;
    MIR_item_t MirProto;
    System_Reflection_MethodInfo NextGenericInstance;
};

typedef enum method_access {
    METHOD_COMPILER_CONTROLLED = 0x0000,
    METHOD_PRIVATE = 0x0001,
    METHOD_FAMILY_AND_ASSEMBLY = 0x0002,
    METHOD_ASSEMBLY = 0x0003,
    METHOD_FAMILY = 0x0004,
    METHOD_FAMILY_OR_ASSEMBLY = 0x0005,
    METHOD_PUBLIC = 0x0006
} method_access_t;

/**
 * Get the access of the method
 */
static inline method_access_t method_get_access(System_Reflection_MethodInfo method) { return (method_access_t)(method->Attributes & 0x0007); }

/**
 * Get the method access as a string
 */
const char* method_access_str(method_access_t access);

// method attribute impl helpers

typedef enum method_code_type {
    METHOD_IL = 0x0000,
    METHOD_NATIVE = 0x0001,
    METHOD_RUNTIME = 0x0003,
} method_code_type_t;

static inline method_code_type_t method_get_code_type(System_Reflection_MethodInfo method) { return (method_code_type_t)(method->ImplAttributes & 0x0003); }
static inline bool method_is_aggressive_inlining(System_Reflection_MethodInfo method) { return method->ImplAttributes & 256; }
static inline bool method_is_unmanaged(System_Reflection_MethodInfo method) { return method->ImplAttributes & 0x0004; }
static inline bool method_is_forward_ref(System_Reflection_MethodInfo method) { return method->ImplAttributes & 0x0010; }
static inline bool method_is_preserve_sig(System_Reflection_MethodInfo method) { return method->ImplAttributes & 0x0080; }
static inline bool method_is_internal_call(System_Reflection_MethodInfo method) { return method->ImplAttributes & 0x1000; }
static inline bool method_is_synchronized(System_Reflection_MethodInfo method) { return method->ImplAttributes & 0x0020; }
static inline bool method_is_no_inlining(System_Reflection_MethodInfo method) { return method->ImplAttributes & 0x0008; }
static inline bool method_is_no_optimization(System_Reflection_MethodInfo method) { return method->ImplAttributes & 0x0040; }

// method attributes helpers
static inline bool method_is_static(System_Reflection_MethodInfo method) { return method->Attributes & 0x0010; }
static inline bool method_is_final(System_Reflection_MethodInfo method) { return method->Attributes & 0x0020; }
static inline bool method_is_virtual(System_Reflection_MethodInfo method) { return method->Attributes & 0x0040; }
static inline bool method_is_hide_by_sig(System_Reflection_MethodInfo method) { return method->Attributes & 0x0080; }
static inline bool method_is_new_slot(System_Reflection_MethodInfo method) { return method->Attributes & 0x0100; }
static inline bool method_is_strict(System_Reflection_MethodInfo method) { return method->Attributes & 0x0200; }
static inline bool method_is_abstract(System_Reflection_MethodInfo method) { return method->Attributes & 0x0400; }
static inline bool method_is_special_name(System_Reflection_MethodInfo method) { return method->Attributes & 0x0800; }
static inline bool method_is_pinvoke_impl(System_Reflection_MethodInfo method) { return method->Attributes & 0x2000; }
static inline bool method_is_rt_special_name(System_Reflection_MethodInfo method) { return method->Attributes & 0x1000; }

/**
 * Print the method name as<name>(<parameters>) <name>(<parameters>)
 */
void method_print_name(System_Reflection_MethodInfo method, strbuilder_t* builder);

/**
 * Print the full method name as [<assembly>]<namespace>.<class>[+<nested>]::<name>(<parameters>)
 */
void method_print_full_name(System_Reflection_MethodInfo method, strbuilder_t* builder);

/**
 * Checks that the given method signature (as a MethodInfo) is the same as the method, this includes
 * name checking and parameter checking
 */
bool method_compare_name_and_sig(System_Reflection_MethodInfo method, System_Reflection_MethodInfo signature);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Reflection_PropertyInfo {
    struct System_Reflection_MemberInfo;
    uint16_t Attributes;
    System_Reflection_MethodInfo SetMethod;
    System_Reflection_MethodInfo GetMethod;
    System_Type PropertyType;
} *System_Reflection_PropertyInfo;

DEFINE_ARRAY(System_Reflection_PropertyInfo);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Exception *System_Exception;

struct System_Exception {
    struct System_Object;
    System_String Message;
    System_Exception InnerException;
};

typedef System_Exception System_ArithmeticException;
typedef System_Exception System_DivideByZeroException;
typedef System_Exception System_ExecutionEngineException;
typedef System_Exception System_IndexOutOfRangeException;
typedef System_Exception System_NullReferenceException;
typedef System_Exception System_InvalidCastException;
typedef System_Exception System_OutOfMemoryException;
typedef System_Exception System_OverflowException;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct TinyDotNet_Reflection_InterfaceImpl {
    struct System_Object;
    System_Type InterfaceType;
    int VTableOffset;
} *TinyDotNet_Reflection_InterfaceImpl;
DEFINE_ARRAY(TinyDotNet_Reflection_InterfaceImpl);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct TinyDotNet_Reflection_MethodImpl {
    struct System_Object;
    System_Reflection_MethodInfo Body;
    System_Reflection_MethodInfo Declaration;
} *TinyDotNet_Reflection_MethodImpl;
DEFINE_ARRAY(TinyDotNet_Reflection_MethodImpl);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct System_Delegate {
    struct System_Object;
    void* Fnptr;
    System_Object Target;
} *System_Delegate;

typedef struct System_MulticastDelegate *System_MulticastDelegate;

struct System_MulticastDelegate {
    struct System_Delegate;
    System_MulticastDelegate Next;
};

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct TinyDotNet_Reflection_MemberReference {
    struct System_Object;
    System_String Name;
    token_t Class;
    System_Byte_Array Signature;
} *TinyDotNet_Reflection_MemberReference;
DEFINE_ARRAY(TinyDotNet_Reflection_MemberReference);

typedef struct TinyDotNet_Reflection_MethodSpec {
    struct System_Object;
    System_Reflection_MethodInfo Method;
    System_Byte_Array Instantiation;
} *TinyDotNet_Reflection_MethodSpec;
DEFINE_ARRAY(TinyDotNet_Reflection_MethodSpec);

struct System_Reflection_Assembly {
    struct System_Object;

    // Information about the assembly
    System_String Name;
    uint16_t MajorVersion;
    uint16_t MinorVersion;
    uint16_t BuildNumber;
    uint16_t RevisionNumber;

    // the module and entry point of this assembly
    System_Reflection_Module Module;
    System_Reflection_MethodInfo EntryPoint;

    // types defined inside the binary
    System_Type_Array DefinedTypes;
    System_Reflection_MethodInfo_Array DefinedMethods;
    System_Reflection_FieldInfo_Array DefinedFields;
    System_Reflection_PropertyInfo_Array DefinedProperties;
    System_Byte_Array_Array DefinedTypeSpecs;
    TinyDotNet_Reflection_MemberReference_Array DefinedMemberRefs;
    TinyDotNet_Reflection_MethodSpec_Array DefinedMethodSpecs;

    // types imported from other assemblies, for easy lookup whenever needed
    System_Type_Array ImportedTypes;

    // we have two entries, one for GC tracking (the array)
    // and one for internally looking up the string entries
    struct {
        int key;
        System_String value;
    }* UserStringsTable;

    // for quickly searching attributes
    struct {
        // where to search the attribute on
        System_Object key;

        // the instances of attributes on this object
        System_Object* value;
    }* CustomAttributeMap;
};

/**
 * Get a type by its token
 *
 * @remark
 * Could fail if type requires type/method parameters
 */
err_t assembly_get_type_by_token(System_Reflection_Assembly assembly, token_t token, System_Type_Array typeArgs, System_Type_Array methodArgs, System_Type* out_type);

/**
 * Get a method by a token
 */
err_t assembly_get_method_by_token(System_Reflection_Assembly assembly, token_t token, System_Type_Array typeArgs, System_Type_Array methodArgs, System_Reflection_MethodInfo* out_method);

/**
 * Get a field by a token
 */
err_t assembly_get_field_by_token(System_Reflection_Assembly assembly, token_t token, System_Type_Array typeArgs, System_Type_Array methodArgs, System_Reflection_FieldInfo* out_field);

/**
 * Get a type by its name and namespace
 */
System_Type assembly_get_type_by_name(System_Reflection_Assembly assembly, const char* name, const char* namespace);

/**
 * Get a string by a token
 */
System_String assembly_get_string_by_token(System_Reflection_Assembly assembly, token_t token);

/**
 * Dump the assembly to the kernel output
 */
void assembly_dump(System_Reflection_Assembly assembly);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// TODO: should we maybe have this more customized for our needs
//       so for example differentiate Object and interface, and have
//       two float types (32 and 64)
typedef enum stack_type {
    STACK_TYPE_O,
    STACK_TYPE_INT32,
    STACK_TYPE_INT64,
    STACK_TYPE_INTPTR,
    STACK_TYPE_VALUE_TYPE,
    STACK_TYPE_FLOAT,
    STACK_TYPE_REF,
} stack_type_t;

typedef struct System_RuntimeTypeHandle {
    System_Type Type;
} System_RuntimeTypeHandle;

struct System_Type {
    // basic type information
    struct System_Reflection_MemberInfo;
    System_Reflection_Assembly Assembly;
    System_Type BaseType;
    System_String Namespace;
    System_Reflection_FieldInfo_Array Fields;
    System_Reflection_MethodInfo_Array Methods;
    System_Reflection_PropertyInfo_Array Properties;
    System_Type ElementType;
    uint32_t Attributes;
    token_t MetadataToken;
    bool IsArray;
    bool IsByRef;
    bool IsBoxed;
    bool IsPointer;

    // for type parameter (not instantiated)
    System_Type GenericTypeDefinition;
    int GenericTypeAttributes;
    int GenericParameterPosition;

    // when instantiated this is the actual type arguments
    // when not instantiated, its the generic arguments
    System_Type_Array GenericArguments;

    // for delegate types
    System_Reflection_MethodInfo DelegateSignature;

    // from the class-layout stuff
    int32_t ClassSize;
    int16_t PackingSize;

    // internal stuff related to offsets, vtables and so on
    int* ManagedPointersOffsets;
    bool IsSetup;
    bool IsSetupFinished;
    bool IsFilled;
    bool IsValueType;
    System_Reflection_MethodInfo Finalize;
    int ManagedSize;
    int ManagedAlignment;
    int StackSize;
    int StackAlignment;
    stack_type_t StackType;
    System_Reflection_MethodInfo StaticCtor;

    System_Reflection_MethodInfo_Array VirtualMethods;
    void** VTable;
    int VTableSize;

    TinyDotNet_Reflection_InterfaceImpl_Array InterfaceImpls;
    TinyDotNet_Reflection_MethodImpl_Array MethodImpls;
    MIR_item_t MirType;

    // getting instances from this type
    System_Type ArrayType;
    System_Type ByRefType;
    System_Type BoxedType;
    System_Type PointerType;
    System_Type UnboxedType;
    System_Type NextGenericInstance;

    // used to connected nested types
    System_Type NextNestedType;

    // used as the link to the first nested
    // type of this type
    System_Type NestedTypes;
};

static inline bool generic_argument_is_reference_type(System_Type type) { return type->GenericTypeAttributes & 0x0004; }
static inline bool generic_argument_is_not_nullable_value_type(System_Type type) { return type->GenericTypeAttributes & 0x0008; }
static inline bool generic_argument_is_default_constructor(System_Type type) { return type->GenericTypeAttributes & 0x0010; }

static inline stack_type_t type_get_stack_type(System_Type type) { return type == NULL ? STACK_TYPE_O : type->StackType; }
static inline bool type_is_generic_type(System_Type type) { return type->GenericArguments != NULL; }
static inline bool type_is_generic_definition(System_Type type) { return type_is_generic_type(type) && type->GenericTypeDefinition == NULL; }

bool type_is_generic_parameter(System_Type type);

void type_dump(System_Type type);

typedef enum type_visibility {
    TYPE_NOT_PUBLIC,
    TYPE_PUBLIC,
    TYPE_NESTED_PUBLIC,
    TYPE_NESTED_PRIVATE,
    TYPE_NESTED_FAMILY,
    TYPE_NESTED_ASSEMBLY,
    TYPE_NESTED_FAMILY_AND_ASSEMBLY,
    TYPE_NESTED_FAMILY_OR_ASSEMBLY,
} type_visibility_t;

typedef enum type_layout {
    TYPE_AUTO_LAYOUT = 0,
    TYPE_SEQUENTIAL_LAYOUT = 1,
    TYPE_EXPLICIT_LAYOUT = 2,
} type_layout_t;

static inline type_visibility_t type_visibility(System_Type type) { return type->Attributes & 0b111; }
static inline type_layout_t type_layout(System_Type type) { return (type->Attributes >> 3) & 0b11; }
static inline bool type_is_abstract(System_Type type) { return type->Attributes & 0x00000080; }
static inline bool type_is_sealed(System_Type type) { return type->Attributes & 0x00000100; }
static inline bool type_is_interface(System_Type type) { return type != NULL && type->Attributes & 0x00000020; }

const char* type_visibility_str(type_visibility_t visibility);

/**
 * Get the array type for the given type
 *
 * @param type  [IN] The type
 */
System_Type get_array_type(System_Type type);

/**
 * Get the by-ref type for the given type
 *
 * @param type  [IN] The type
 */
System_Type get_by_ref_type(System_Type type);

System_Type get_boxed_type(System_Type type);

System_Type get_pointer_type(System_Type type);

/**
 * Print the type name as <namespace>.<class>[+<nested>]
 */
void type_print_name(System_Type Type, strbuilder_t* builder);

/**
 * Print the full name as [assembly]<namespace>.<class>[+<nested>]
 */
void type_print_full_name(System_Type Type, strbuilder_t* builder);

/**
 * Get a field by its name
 *
 * @param type      [IN] The declaring type
 * @param name      [IN] The name
 */
System_Reflection_FieldInfo type_get_field(System_Type type, System_String name);

/**
 * Searched for a method that is the same as the signature (including name) in the given type
 */
System_Reflection_MethodInfo type_find_method_in_type(System_Type type, System_Reflection_MethodInfo signature);

/**
 * Iterate all the methods of the type with the same name, starting at the given index
 */
System_Reflection_MethodInfo type_iterate_methods(System_Type type, System_String name, int* index);

/**
 * Just like the type_iterate_methods, but takes in a c-string instead of a normal string
 */
System_Reflection_MethodInfo type_iterate_methods_cstr(System_Type type, const char* name, int* index);

/**
 * Get the implementation of the given interface method
 */
System_Reflection_MethodInfo type_get_interface_method_impl(System_Type targetType, System_Reflection_MethodInfo targetMethod);

/**
 * Get the interface implementation of the given type
 */
TinyDotNet_Reflection_InterfaceImpl type_get_interface_impl(System_Type targetType, System_Type interfaceType);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Implements the finalization on the System.Reflection.Assembly type, which is needed because of how
 * we have unmanaged structures
 */
System_Exception assembly_finalizer(System_Reflection_Assembly assembly);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

extern System_Type tSystem_Void;
extern System_Type tSystem_Enum;
extern System_Type tSystem_Exception;
extern System_Type tSystem_ValueType;
extern System_Type tSystem_Object;
extern System_Type tSystem_Type;
extern System_Type tSystem_Array;
extern System_Type tSystem_String;
extern System_Type tSystem_Boolean;
extern System_Type tSystem_Char;
extern System_Type tSystem_SByte;
extern System_Type tSystem_Byte;
extern System_Type tSystem_Int16;
extern System_Type tSystem_UInt16;
extern System_Type tSystem_Int32;
extern System_Type tSystem_UInt32;
extern System_Type tSystem_Int64;
extern System_Type tSystem_UInt64;
extern System_Type tSystem_Single;
extern System_Type tSystem_Double;
extern System_Type tSystem_IntPtr;
extern System_Type tSystem_UIntPtr;
extern System_Type tSystem_Delegate;
extern System_Type tSystem_MulticastDelegate;
extern System_Type tSystem_Reflection_Module;
extern System_Type tSystem_Reflection_Assembly;
extern System_Type tSystem_Reflection_FieldInfo;
extern System_Type tSystem_Reflection_MemberInfo;
extern System_Type tSystem_Reflection_ParameterInfo;
extern System_Type tSystem_Reflection_PropertyInfo;
extern System_Type tSystem_Reflection_LocalVariableInfo;
extern System_Type tSystem_Reflection_ExceptionHandlingClause;
extern System_Type tSystem_Reflection_MethodBase;
extern System_Type tSystem_Reflection_MethodBody;
extern System_Type tSystem_Reflection_MethodInfo;
extern System_Type tSystem_ArithmeticException;
extern System_Type tSystem_DivideByZeroException;
extern System_Type tSystem_ExecutionEngineException;
extern System_Type tSystem_IndexOutOfRangeException;
extern System_Type tSystem_NullReferenceException;
extern System_Type tSystem_InvalidCastException;
extern System_Type tSystem_OutOfMemoryException;
extern System_Type tSystem_OverflowException;
extern System_Type tSystem_RuntimeTypeHandle;
extern System_Type tSystem_Nullable;
extern System_Type tSystem_ReadOnlySpan;
extern System_Type tSystem_Span;

extern System_Type tTinyDotNet_Reflection_InterfaceImpl;
extern System_Type tTinyDotNet_Reflection_MemberReference;
extern System_Type tTinyDotNet_Reflection_MethodImpl;
extern System_Type tTinyDotNet_Reflection_MethodSpec;

extern System_Type tSystem_Runtime_CompilerServices_Unsafe;
extern System_Type tSystem_Runtime_CompilerServices_RuntimeHelpers;
extern System_Type tSystem_Runtime_CompilerServices_IsVolatile;
extern System_Type tSystem_Runtime_InteropServices_InAttribute;
extern System_Type tSystem_ThreadStaticAttribute;

static inline bool type_is_enum(System_Type type) { return type != NULL && !type->IsByRef && type->BaseType == tSystem_Enum; }
static inline bool type_is_object_ref(System_Type type) { return type == NULL || type_get_stack_type(type) == STACK_TYPE_O; }
static inline bool type_is_value_type(System_Type type) { return type != NULL && (type->BaseType == tSystem_ValueType || type->BaseType == tSystem_Enum); }
bool type_is_integer(System_Type type);

System_Type type_get_underlying_type(System_Type T);
System_Type type_get_verification_type(System_Type T);
System_Type type_get_intermediate_type(System_Type T);
bool type_is_array_element_compatible_with(System_Type T, System_Type U);
bool type_is_compatible_with(System_Type T, System_Type U);
bool type_is_verifier_assignable_to(System_Type Q, System_Type R);

bool isinstance(System_Object object, System_Type type);

bool check_field_accessibility(System_Reflection_MethodInfo from, System_Reflection_FieldInfo to);
bool check_method_accessibility(System_Reflection_MethodInfo from, System_Reflection_MethodInfo to);
bool check_type_visibility(System_Reflection_MethodInfo from, System_Type to);

System_Type get_this_type(System_Reflection_MethodInfo signature);

/**
 * Create a new generic type with the given generic arguments
 */
err_t type_make_generic(System_Type type, System_Type_Array arguments, System_Type* out_type);

/**
 * Expand the given instance of a generic type
 *
 * requires the definition to be initialized
 */
err_t type_expand_generic(System_Type type);

/**
 * Updates the interface impl to the given one, this is needed because interface impls are created
 * during a later stage than the inital type loading, which could cause a generic type to be created
 * before an interface impl was done, but it still depends on interface impls, so we just udpate it
 * again at a later stage during type init (during runtime it should be fine)
 */
err_t type_expand_interface_impls(System_Type type, TinyDotNet_Reflection_InterfaceImpl_Array interfaceImpls);

/**
 * Same as the interface impls
 */
err_t type_expand_method_impls(System_Type type, TinyDotNet_Reflection_MethodImpl_Array impls);

/**
 * Make a new generic method with the given generic arguments
 */
err_t method_make_generic(System_Reflection_MethodInfo method, System_Type_Array arguments, System_Reflection_MethodInfo* out_method);

/**
 * Expand the given type as a generic type from the arguments, while ignoring generic arguments from the ignore list
 */
err_t expand_type(System_Type type, System_Type_Array arguments, System_Type_Array ignore_arguments, System_Type* out_type);
