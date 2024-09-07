#include "jit_helpers.h"

#include <dotnet/loader.h>
#include <dotnet/gc/gc.h>
#include <util/except.h>

void jit_bzero() { ASSERT(!"jit_bzero"); }

void jit_memcpy() { ASSERT(!"jit_memcpy"); }

void* jit_gc_new(int type_id, uint64_t size) {
    RuntimeTypeInfo type = tdn_get_type_by_id(type_id);
    if (type == NULL) {
        return NULL;
    }
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
