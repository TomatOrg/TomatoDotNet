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

    new_type->mod = TYPE_BY_REF;
    new_type->element_type = type;

    // TODO: generate interfaces

    type->by_ref_type = new_type;

    return new_type;
}

void type_print(type_t* type) {
    switch (type->mod) {
        case TYPE_NORMAL: {
            if (type == g_void) {
                printf("void");
            } else if (type == g_boolean) {
                printf("bool");
            } else if (type == g_char) {
                printf("char");
            } else if (type == g_sbyte) {
                printf("sbyte");
            } else if (type == g_byte) {
                printf("byte");
            } else if (type == g_int16) {
                printf("short");
            } else if (type == g_uint16) {
                printf("ushort");
            } else if (type == g_int32) {
                printf("int");
            } else if (type == g_uint32) {
                printf("uint");
            } else if (type == g_int64) {
                printf("long");
            } else if (type == g_uint64) {
                printf("ulong");
            } else if (type == g_boolean) {
                printf("float");
            } else if (type == g_double) {
                printf("double");
            } else if (type == g_string) {
                printf("string");
            } else {
                if (type->assembly != g_corelib_assembly) {
                    printf("[%s]", type->assembly->name);
                }
                printf("%s.%s", type->namespace, type->name);
            }
        } break;

        case TYPE_ARRAY: {
            type_print(type->element_type);
            printf("[]");
        } break;

        case TYPE_BY_REF: {
            printf("ref ");
            type_print(type->element_type);
        } break;

        case TYPE_PTR: {
            type_print(type->element_type);
            printf("*");
        } break;

        default: printf("<invalid>");
    }
}

int type_output(char* buffer, size_t buffer_size, type_t* type) {
    int added = 0;

    switch (type->mod) {
        case TYPE_NORMAL: {
            if (type == g_void) {
                added += snprintf(buffer, buffer_size, "void");
            } else if (type == g_boolean) {
                added += snprintf(buffer, buffer_size, "bool");
            } else if (type == g_char) {
                added += snprintf(buffer, buffer_size, "char");
            } else if (type == g_sbyte) {
                added += snprintf(buffer, buffer_size, "sbyte");
            } else if (type == g_byte) {
                added += snprintf(buffer, buffer_size, "byte");
            } else if (type == g_int16) {
                added += snprintf(buffer, buffer_size, "short");
            } else if (type == g_uint16) {
                added += snprintf(buffer, buffer_size, "ushort");
            } else if (type == g_int32) {
                added += snprintf(buffer, buffer_size, "int");
            } else if (type == g_uint32) {
                added += snprintf(buffer, buffer_size, "uint");
            } else if (type == g_int64) {
                added += snprintf(buffer, buffer_size, "long");
            } else if (type == g_uint64) {
                added += snprintf(buffer, buffer_size, "ulong");
            } else if (type == g_boolean) {
                added += snprintf(buffer, buffer_size, "float");
            } else if (type == g_double) {
                added += snprintf(buffer, buffer_size, "double");
            } else if (type == g_string) {
                added += snprintf(buffer, buffer_size, "string");
            } else {
                if (type->assembly != g_corelib_assembly) {
                    added += snprintf(buffer, buffer_size, "[%s]", type->assembly->name);
                }
                added += snprintf(buffer, buffer_size, "%s.%s", type->namespace, type->name);
            }
        } break;

        case TYPE_ARRAY: {
            added = type_output(buffer, buffer_size, type->element_type);
            buffer += added;
            buffer_size -= added;
            strncat(buffer, "[]", buffer_size);
            added += 2;
        } break;

        case TYPE_BY_REF: {
            strncat(buffer, "ref ", buffer_size);
            buffer += 4;
            buffer_size -= 4;
            added += 4;
            added += type_output(buffer, buffer_size, type->element_type);
        } break;

        case TYPE_PTR: {
            added = type_output(buffer, buffer_size, type->element_type);
            buffer += added;
            buffer_size -= added;
            strncat(buffer, "*", buffer_size);
            added += 1;
        } break;

        default: return snprintf(buffer, buffer_size, "<unknown>");
    }

    return added;
}
