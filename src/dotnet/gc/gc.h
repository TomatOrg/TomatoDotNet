#pragma once

#include <stddef.h>
#include "../types.h"

void gc_free_all();

void* gc_new(System_Type type, size_t size);

void* gc_raw_alloc(size_t size);

#define GC_NEW(type) \
    ({ \
        type ___ptr = gc_new(t##type, sizeof(struct type)); \
        CHECK_ERROR(___ptr != NULL, TDN_ERROR_OUT_OF_MEMORY); \
        ___ptr; \
    })

// TODO: optimize for zero length
#define GC_NEW_ARRAY(type, count) \
    ({ \
        size_t ___array_length = (count); \
        System_Type ___array_type = NULL; \
        CHECK_AND_RETHROW(tdn_get_array_type(t##type, &___array_type)); \
        type##_Array ___ptr = gc_new(___array_type, sizeof(struct System_Array) + sizeof(struct type) * (___array_length)); \
        CHECK_ERROR(___ptr != NULL, TDN_ERROR_OUT_OF_MEMORY); \
        ___ptr->Length = ___array_length; \
        ___ptr; \
    })

#define GC_NEW_BYTE_ARRAY(count) \
    ({ \
        size_t ___array_length = (count); \
        System_Type ___array_type = NULL; \
        CHECK_AND_RETHROW(tdn_get_array_type(tSystem_Byte, &___array_type)); \
        System_Byte_Array ___ptr = gc_new(___array_type, sizeof(struct System_Array) + (___array_length)); \
        CHECK_ERROR(___ptr != NULL, TDN_ERROR_OUT_OF_MEMORY); \
        ___ptr->Length = ___array_length; \
        ___ptr; \
    })

