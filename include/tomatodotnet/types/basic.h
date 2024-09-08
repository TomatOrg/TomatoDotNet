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

typedef struct Object {
    uint32_t VTable;
    uint32_t TypeId;
    uint8_t MonitorLock;
    uint8_t MonitorCondVar;
    uint16_t _reserved1;
    uint32_t _reserved2;
}* Object;
_Static_assert(sizeof(struct Object) == 8 * 2, "Object size too big");

typedef struct String {
    struct Object;
    int Length;
    uint8_t _padding[4];
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