#pragma once

#include <stddef.h>
#include <stdint.h>
#include <tomatodotnet/types/type.h>

void jit_bzero(void* ptr, size_t size);
void jit_memcpy(void* dst, void* src, size_t size);

// TODO: maybe pass in the VTable instead? its a smaller value
void* jit_gc_new(RuntimeTypeInfo type);
void* jit_gc_newarr(RuntimeTypeInfo type, size_t element_count);

void jit_gc_memcpy();
void jit_gc_bzero();
void jit_throw_invalid_cast_exception();
void jit_throw_index_out_of_range_exception();
void jit_throw_overflow_exception();
void jit_throw_null_reference_exception();
void jit_throw();
void jit_rethrow();
void jit_get_exception();
