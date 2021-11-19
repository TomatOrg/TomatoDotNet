#include "type.h"

#include <util/stb_ds.h>

#include "assembly_internal.h"
#include "metadata_spec.h"
#include "signature.h"

type_t* g_void;
type_t* g_boolean;
type_t* g_char;
type_t* g_sbyte;
type_t* g_byte;
type_t* g_int16;
type_t* g_uint16;
type_t* g_int32;
type_t* g_uint32;
type_t* g_int64;
type_t* g_uint64;
type_t* g_float;
type_t* g_double;
type_t* g_string;
type_t* g_intptr;
type_t* g_uintptr;
type_t* g_object;
type_t* g_array;
type_t* g_value_type;

type_t* get_array_type(type_t* type) {
    // fast path if it exists
    if (type->array_type != NULL) {
        return type->array_type;
    }

    // initialize a new array type
    type_t* new_type = malloc(sizeof(type_t));
    memcpy(new_type, g_array, sizeof(type_t));

    new_type->token = (token_t) { .packed = 0 };
    new_type->mod = TYPE_ARRAY;
    new_type->element_type = type;

    // TODO: generate interfaces

    type->array_type = new_type;

    return new_type;
}

type_t* get_ptr_type(type_t* type) {
    // fast path if it exists
    if (type->ptr_type != NULL) {
        return type->ptr_type;
    }

    // initialize a new array type
    type_t* new_type = malloc(sizeof(type_t));
    memcpy(new_type, g_uintptr, sizeof(type_t));

    new_type->token = (token_t) { .packed = 0 };
    new_type->mod = TYPE_PTR;
    new_type->element_type = type;

    // TODO: generate interfaces

    type->ptr_type = new_type;

    return new_type;
}

type_t* get_by_ref_type(type_t* type) {
    // fast path if it exists
    if (type->by_ref_type != NULL) {
        return type->by_ref_type;
    }

    // initialize a new array type
    type_t* new_type = malloc(sizeof(type_t));
    memcpy(new_type, g_intptr, sizeof(type_t));

    new_type->token = (token_t) { .packed = 0 };
    new_type->mod = TYPE_BY_REF;
    new_type->element_type = type;

    // TODO: generate interfaces

    type->by_ref_type = new_type;

    return new_type;
}

void type_print(type_t* type) {
    char buffer[256];
    type_write_name(type, buffer, sizeof(buffer));
    printf("%s", buffer);
}

size_t type_write_name(type_t* type, char* buffer, size_t buffer_size) {
    size_t printed_size = 0;
    switch (type->mod) {
        case TYPE_NORMAL: {
            if (type == g_void) {
                printed_size += snprintf(buffer, buffer_size, "void");
            } else if (type == g_boolean) {
                printed_size += snprintf(buffer, buffer_size, "bool");
            } else if (type == g_char) {
                printed_size += snprintf(buffer, buffer_size, "char");
            } else if (type == g_sbyte) {
                printed_size += snprintf(buffer, buffer_size, "sbyte");
            } else if (type == g_byte) {
                printed_size += snprintf(buffer, buffer_size, "byte");
            } else if (type == g_int16) {
                printed_size += snprintf(buffer, buffer_size, "short");
            } else if (type == g_uint16) {
                printed_size += snprintf(buffer, buffer_size, "ushort");
            } else if (type == g_int32) {
                printed_size += snprintf(buffer, buffer_size, "int");
            } else if (type == g_uint32) {
                printed_size += snprintf(buffer, buffer_size, "uint");
            } else if (type == g_int64) {
                printed_size += snprintf(buffer, buffer_size, "long");
            } else if (type == g_uint64) {
                printed_size += snprintf(buffer, buffer_size, "long");
            } else if (type == g_intptr) {
                printed_size += snprintf(buffer, buffer_size, "nint");
            } else if (type == g_uintptr) {
                printed_size += snprintf(buffer, buffer_size, "nuint");
            } else if (type == g_boolean) {
                printed_size += snprintf(buffer, buffer_size, "float");
            } else if (type == g_double) {
                printed_size += snprintf(buffer, buffer_size, "double");
            } else if (type == g_string) {
                printed_size += snprintf(buffer, buffer_size, "string");
            } else {
                if (type->assembly != g_corelib_assembly) {
                    printed_size += snprintf(buffer, buffer_size, "[%s]", type->assembly->name);
                    buffer += printed_size;
                    buffer_size -= printed_size;
                }
                type_t* enclosing = type->enclosing;
                while (enclosing != NULL) {
                    if (strlen(enclosing->namespace) == 0) {
                        printed_size += snprintf(buffer, buffer_size, "%s.", enclosing->name);
                        buffer += printed_size;
                        buffer_size -= printed_size;
                    } else {
                        printed_size += snprintf(buffer, buffer_size, "%s.%s.", enclosing->namespace, enclosing->name);
                        buffer += printed_size;
                        buffer_size -= printed_size;
                    }
                    enclosing = enclosing->enclosing;
                }

                if (strlen(type->namespace) == 0) {
                    printed_size += snprintf(buffer, buffer_size, "%s", type->name);
                } else {
                    printed_size += snprintf(buffer, buffer_size, "%s.%s", type->namespace, type->name);
                }
            }
        } break;

        case TYPE_ARRAY: {
            printed_size += type_write_name(type->element_type, buffer, buffer_size);
            buffer += printed_size;
            buffer_size -= printed_size;
            printed_size += snprintf(buffer, buffer_size, "[]");
        } break;

        case TYPE_BY_REF: {
            printed_size += snprintf(buffer, buffer_size, "ref ");
            buffer += printed_size;
            buffer_size -= printed_size;
            printed_size += type_write_name(type->element_type, buffer, buffer_size);
        } break;

        case TYPE_PTR: {
            printed_size += type_write_name(type->element_type, buffer, buffer_size);
            buffer += printed_size;
            buffer_size -= printed_size;
            printed_size += snprintf(buffer, buffer_size, "*");
        } break;

        default: printed_size += snprintf(buffer, buffer_size, "<invalid>");
    }

    return printed_size;
}
