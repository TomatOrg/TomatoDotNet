#pragma once

#include "assembly.h"

#include <stdbool.h>

// forward decl
struct method;
struct type;

typedef struct field {
    const char* name;
    struct type* type;

    uint8_t is_static : 1;
    uint8_t is_literal : 1;
    uint8_t is_init_only : 1;
    // TODO: default?
    // TODO: RVA?

    // the offset in memory/on the stack
    size_t offset;
} field_t;

typedef enum type_mod {
    TYPE_NORMAL,
    TYPE_PTR,
    TYPE_BY_REF,
    TYPE_ARRAY
} type_mod_t;

typedef enum stack_type {
    STACK_TYPE_INT32,
    STACK_TYPE_INT64,
    STACK_TYPE_F,
    STACK_TYPE_NATIVE_INT,
    STACK_TYPE_T,
} stack_type_t;

typedef struct type {
    // the assembly this type is related to
    assembly_t* assembly;
    token_t token;

    // The base type of this type
    struct type* extends;

    uint8_t is_interface : 1;
    uint8_t is_value_type : 1;

    uint8_t resolved_value_type : 1;
    uint8_t resolved_size : 1;

    // the size of the element on the stack or inside another class
    stack_type_t stack_type;
    size_t stack_size;
    size_t stack_alignment;

    // the size of the element in memory
    size_t memory_size;
    size_t memory_alignment;

    // the namespace and name of this type
    const char* namespace;
    const char* name;

    // the methods of this type
    size_t methods_count;
    struct method* methods;

    // the fields of this type
    size_t fields_count;
    field_t* fields;

    // is this type representing an array
    type_mod_t mod;

    // the type of the array (if this is an array)
    struct type* element_type;

    // the array type for this type
    struct type* array_type;
    struct type* ptr_type;
    struct type* by_ref_type;
} type_t;

/**
 * Returns true if this is a value type
 *
 * @remark
 * This will return false for native types like floats, ints and alike
 */
static inline bool type_is_valuetype(type_t* type) {
    return type->stack_type == STACK_TYPE_T && type->is_value_type;
}

static inline bool type_is_reference_type(type_t* type) {
    return type->stack_type == STACK_TYPE_T && !type->is_value_type;
}

// These are common built in types
extern type_t* g_void;
extern type_t* g_boolean;
extern type_t* g_char;
extern type_t* g_sbyte;
extern type_t* g_byte;
extern type_t* g_int16;
extern type_t* g_uint16;
extern type_t* g_int32;
extern type_t* g_uint32;
extern type_t* g_int64;
extern type_t* g_uint64;
extern type_t* g_float;
extern type_t* g_double;
extern type_t* g_string;
extern type_t* g_intptr;
extern type_t* g_uintptr;
extern type_t* g_object;
extern type_t* g_array;
extern type_t* g_value_type;

/**
 * Get the array type for the given element type
 *
 * @param type      [IN] The element type
 */
type_t* get_array_type(type_t* type);

/**
 * Get the pointer type for the given element type
 *
 * @param type      [IN] The element type
 */
type_t* get_ptr_type(type_t* type);

/**
 * Get the by-ref type for the given element type
 *
 * @param type      [IN] The element type
 */
type_t* get_by_ref_type(type_t* type);

/**
 * Print the type information nicely
 *
 * @param type      [IN] The type to print
 */
void type_print(type_t* type);

/**
 * Write the type name into a buffer
 *
 * @param type          [IN] The type to write
 * @param buffer        [IN] The buffer to write to
 * @param buffer_size   [IN] The size of the buffer
 */
size_t type_write_name(type_t* type, char* buffer, size_t buffer_size);
