#pragma once

#include <stddef.h>
#include <stdint.h>
#include <spidir/module.h>
#include <tomatodotnet/types/type.h>

void jit_bzero(void* ptr, size_t size);
void jit_memcpy(void* dst, void* src, size_t size);

// TODO: maybe pass in the VTable instead? its a smaller value
void* jit_gc_new(RuntimeTypeInfo type);
void* jit_gc_newarr(RuntimeTypeInfo type, size_t element_count);
void* jit_gc_newstr(uint32_t element_count);

void jit_gc_memcpy();
void jit_gc_bzero();

void jit_throw(Object exception, uint32_t pc);

void jit_throw_invalid_cast_exception();
void jit_throw_index_out_of_range_exception();
void jit_throw_overflow_exception();
void jit_throw_null_reference_exception();
void jit_rethrow();
void jit_get_exception();

int jit_leading_zero_count_32(uint32_t value);
int jit_leading_zero_count_64(uint64_t value);

void jit_print_str(String str);
void jit_print_int(int value);
void jit_print_ptr(void* value);

extern spidir_function_t g_jit_print_str;
extern spidir_function_t g_jit_print_int;
extern spidir_function_t g_jit_print_ptr;

/**
* The helpers as jit functions
*/
extern spidir_function_t g_jit_bzero;
extern spidir_function_t g_jit_memcpy;

extern spidir_function_t g_jit_leading_zero_count_32;
extern spidir_function_t g_jit_leading_zero_count_64;
