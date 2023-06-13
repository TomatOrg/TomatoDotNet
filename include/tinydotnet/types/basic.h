#pragma once

#include <stdint.h>
#include <stdbool.h>
#include "tinydotnet/except.h"

typedef uint8_t System_Byte;
typedef uint16_t System_UInt16;
typedef uint32_t System_UInt32;
typedef uint64_t System_UInt64;
typedef uintptr_t System_UIntPtr;

typedef int8_t System_SByte;
typedef int16_t System_Int16;
typedef int32_t System_Int32;
typedef int64_t System_Int64;
typedef intptr_t System_IntPtr;

typedef bool System_Boolean;
typedef uint16_t System_Char;

typedef struct System_Type* System_Type;

typedef struct System_Object {
    uint32_t VTable;
    uint32_t ITable;
    uint8_t MonitorLock;
    uint8_t MonitorCondVar;
    uint16_t MonitorOwnerThreadId;
    uint32_t MonitorDepth : 24;
    uint32_t GcFlags : 8;
    System_Type ObjectType;
    struct System_Object* next;
}* System_Object;
//_Static_assert(sizeof(struct System_Object) <= 8 * 3, "Object size too big");

typedef struct System_String {
    struct System_Object;
    int Length;
    uint8_t _padding[4];
    System_Char Chars[];
}* System_String;

typedef struct System_Array {
    struct System_Object;
    int Length;
    uint8_t _padding[12];
}* System_Array;

#define DEFINE_ARRAY(Type) \
    typedef struct __attribute__((packed)) Type##_Array { \
        struct System_Array; \
        Type Elements[]; \
    }* Type##_Array;

typedef struct System_ValueType {} System_ValueType;
typedef struct System_Enum {} System_Enum;

typedef struct System_Guid {
    uint8_t Data[16];
} System_Guid;

typedef struct System_Version {
    struct System_Object;
    int Major;
    int Minor;
    int Build;
    int Revision;
}* System_Version;

DEFINE_ARRAY(System_Byte);
DEFINE_ARRAY(System_Type);

tdn_err_t tdn_create_string_from_cstr(const char* cstr, System_String* out_str);

tdn_err_t tdn_append_cstr_to_string(System_String str, const char* cstr, System_String* out_str);

bool tdn_compare_string_to_cstr(System_String str, const char* cstr);
