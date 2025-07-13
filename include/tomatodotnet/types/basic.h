#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#include "tomatodotnet/except.h"

typedef uint8_t Byte;
typedef uint16_t UInt16;
typedef uint32_t UInt32;
typedef uint64_t UInt64;
typedef uintptr_t UIntPtr;

typedef int8_t SByte;
typedef int16_t Int16;
typedef int32_t Int32;
typedef int64_t Int64;
typedef intptr_t IntPtr;

typedef float Single;
typedef double Double;

typedef bool Boolean;
typedef uint16_t Char;

typedef struct RuntimeTypeInfo* RuntimeTypeInfo;

typedef struct {} ValueType;
typedef struct {} Enum;
typedef struct {} Void;

typedef struct ObjectVTable {
    // The full runtime type of the type
    RuntimeTypeInfo Type;

    // The hierarchy encoding, used for checking
    // quickly is an instance of this type
    uint64_t TypeHierarchy;

    // the type's interface product, used for
    // checking quickly if implementing some type
    uint64_t InterfaceProduct;

    // the actual functions come now
    void* Functions[];
} ObjectVTable;

typedef struct Object {
    ObjectVTable* VTable;
    uint32_t GcColor : 2;
    uint32_t : 30;
    uint8_t Mutex;
    uint8_t CondVar;
    uint16_t MutexThreadId;
}* Object;
_Static_assert(sizeof(struct Object) == 16, "Object size too big");

typedef struct Interface {
    Object Instance;
    void** VTable;
} Interface;

typedef struct Delegate {
    Object Instance;
    void* Function;
} Delegate;

typedef struct String {
    struct Object;
    int Length;
    int _padding;
    Char Chars[];
}* String;

typedef struct Array {
    struct Object;
    int Length;
    int SubLength;
}* Array;

typedef struct Span {
    void* Reference;
    int Length;
} Span;

#define DEFINE_ARRAY(Type) \
    typedef struct Type##_Array { \
        struct Array; \
        Type Elements[]; \
    }* Type##_Array;

typedef struct Guid {
    uint8_t Data[16];
} Guid;

DEFINE_ARRAY(Byte);
DEFINE_ARRAY(Char);
DEFINE_ARRAY(RuntimeTypeInfo);
DEFINE_ARRAY(Object);

typedef struct Exception {
    struct Object;
    String Message;
    struct Exception* InnerException;
}* Exception;

/**
 * We depend on this in the runtime
 */
_Static_assert(offsetof(struct Char_Array, Elements) == offsetof(struct String, Chars), "");

tdn_err_t tdn_create_string_from_cstr(const char* cstr, String* out_str);

tdn_err_t tdn_append_cstr_to_string(String str, const char* cstr, String* out_str);

bool tdn_compare_string_to_cstr(String str, const char* cstr);
bool tdn_compare_string(String a, String b);