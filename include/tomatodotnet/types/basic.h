#pragma once

#include <stdint.h>
#include <stdbool.h>
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
    uint32_t VTable;
    uint8_t MonitorLock;
    uint8_t MonitorCondVar;
    uint8_t GCFlags;
    uint8_t _reserved;
}* Object;
_Static_assert(sizeof(struct Object) == 8, "Object size too big");

typedef struct Interface {
    Object Instance;
    void** VTable;
} Interface;

static inline ObjectVTable* object_get_vtable(Object object) { return (void*)(uintptr_t)object->VTable; }

typedef struct String {
    struct Object;
    int Length;
    Char Chars[];
}* String;

typedef struct Array {
    struct Object;
    int Length;
}* Array;

#define DEFINE_ARRAY(Type) \
    typedef struct Type##_Array { \
        struct Array; \
        Type Elements[]; \
    }* Type##_Array;

typedef struct Guid {
    uint8_t Data[16];
} Guid;

DEFINE_ARRAY(Byte);
DEFINE_ARRAY(RuntimeTypeInfo);

tdn_err_t tdn_create_string_from_cstr(const char* cstr, String* out_str);

tdn_err_t tdn_append_cstr_to_string(String str, const char* cstr, String* out_str);

bool tdn_compare_string_to_cstr(String str, const char* cstr);
bool tdn_compare_string(String a, String b);