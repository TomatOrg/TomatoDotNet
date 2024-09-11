#include "jit_helpers.h"

#include <dotnet/loader.h>
#include <dotnet/gc/gc.h>
#include <util/except.h>
#include <util/string.h>

void jit_bzero(void* ptr, size_t size) {
    memset(ptr, 0, size);
}

void jit_memcpy(void* dst, void* src, size_t size) {
    memcpy(dst, src, size);
}

void* jit_gc_new(RuntimeTypeInfo type) {
    if (tdn_type_is_valuetype(type)) {
        // we want a boxed type, add the object size
        return gc_new(type, ALIGN_UP(sizeof(struct Object), type->HeapAlignment) + type->HeapSize);
    } else {
        return gc_new(type, type->HeapSize);
    }
}

void* jit_gc_newarr(RuntimeTypeInfo type, uint64_t element_count) {
    ASSERT(type->IsArray);

    // calculate the size of the array, don't forget to account for the alignment
    // of individual items
    RuntimeTypeInfo element_type = type->ElementType;
    size_t size = ALIGN_UP(sizeof(struct Array), element_type->StackAlignment);
    size += element_count * element_type->StackSize;

    return gc_new(type, size);
}

void jit_gc_memcpy() { ASSERT(!"jit_gc_memcpy"); }

void jit_gc_bzero() { ASSERT(!"jit_gc_bzero"); }

void jit_throw_invalid_cast_exception() { ASSERT(!"jit_throw_invalid_cast_exception"); }

void jit_throw_index_out_of_range_exception() { ASSERT(!"jit_throw_index_out_of_range_exception"); }

void jit_throw_overflow_exception() { ASSERT(!"jit_throw_overflow_exception"); }

void jit_throw_null_reference_exception() { ASSERT(!"jit_throw_null_reference_exception"); }

void jit_throw() { ASSERT(!"jit_throw"); }

void jit_rethrow() { ASSERT(!"jit_rethrow"); }

void jit_get_exception() { ASSERT(!"jit_get_exception"); }
