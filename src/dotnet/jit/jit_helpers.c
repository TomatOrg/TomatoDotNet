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
    size_t array_object_size = ALIGN_UP(sizeof(struct Array), element_type->StackAlignment);

    // and now add it all, use overflow checks to make sure we don't allocate too big items
    size_t size = element_count;
    if (__builtin_mul_overflow(size, element_type->StackSize, &size)) return NULL;
    if (__builtin_add_overflow(size, array_object_size, &size)) return NULL;
    return gc_new(type, size);
}

void* jit_gc_newstr(uint32_t element_count) {
    // use overflow checks to make sure we don't allocate too big items
    size_t size = element_count;
    if (__builtin_mul_overflow(size, sizeof(Char), &size)) return NULL;
    if (__builtin_add_overflow(size, sizeof(struct String), &size)) return NULL;
    return gc_new(tString, size);
}

void jit_throw(Object exception, uint32_t pc) {
    ERROR("Exception of type %T thrown (IL PC %d)", object_get_vtable(exception)->Type, pc);
    ASSERT(!"jit_throw");
}

void jit_gc_memcpy() { ASSERT(!"jit_gc_memcpy"); }

void jit_gc_bzero() { ASSERT(!"jit_gc_bzero"); }

void jit_throw_invalid_cast_exception() { ASSERT(!"jit_throw_invalid_cast_exception"); }

void jit_throw_index_out_of_range_exception() { ASSERT(!"jit_throw_index_out_of_range_exception"); }

void jit_throw_overflow_exception() { ASSERT(!"jit_throw_overflow_exception"); }

void jit_throw_null_reference_exception() { ASSERT(!"jit_throw_null_reference_exception"); }

void jit_rethrow() { ASSERT(!"jit_rethrow"); }

void jit_get_exception() { ASSERT(!"jit_get_exception"); }

int jit_leading_zero_count_32(uint32_t value) {
    if (value == 0) return 32;
    return __builtin_clz(value);
}

int jit_leading_zero_count_64(uint64_t value) {
    if (value == 0) return 64;
    return __builtin_clzll(value);
}

void jit_print_str(String str) {
    TRACE("%U", str);
}

void jit_print_int(int value) {
    TRACE("%d", value);
}

void jit_print_ptr(void* value) {
    TRACE("%p", value);
}
