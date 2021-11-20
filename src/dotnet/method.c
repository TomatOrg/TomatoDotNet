#include "method.h"

#include "assembly_internal.h"
#include "type.h"

#include <string.h>


#define APPEND_FORMAT(buffer, buffer_size, fmt, ...) \
    do { \
        int __printed = snprintf(buffer, buffer_size, fmt, ## __VA_ARGS__); \
        CHECK(__printed < buffer_size); \
        buffer_size -= __printed; \
        buffer += __printed; \
    } while(0)

err_t method_write_signature(method_t* method, char* mangled_name, size_t buffer_size) {
    err_t err = NO_ERROR;

    int printed = type_write_name(method->return_type, mangled_name, buffer_size);
    CHECK(printed < buffer_size);
    buffer_size -= printed;
    mangled_name += printed;

    APPEND_FORMAT(mangled_name, buffer_size, "[%s]", method->assembly->name);

    type_t* type = method->parent;
    type_t* enclosing = type->enclosing;
    while (enclosing != NULL) {
        if (strlen(enclosing->namespace) > 0) {
            APPEND_FORMAT(mangled_name, buffer_size, "%s.", enclosing->namespace);
        }
        APPEND_FORMAT(mangled_name, buffer_size, "%s.", enclosing->name);
        enclosing = enclosing->enclosing;
    }

    if (strlen(type->namespace) > 0) {
        APPEND_FORMAT(mangled_name, buffer_size, "%s.", type->namespace);
    }

    APPEND_FORMAT(mangled_name, buffer_size, "%s::%s(", type->name, method->name);

    for (int i = 0; i < method->parameter_count; i++) {
        printed = type_write_name(method->parameters[i].type, mangled_name, buffer_size);
        CHECK(printed < buffer_size);
        buffer_size -= printed;
        mangled_name += printed;
        if (i != method->parameter_count - 1) {
            APPEND_FORMAT(mangled_name, buffer_size, ",");
        }
    }

    APPEND_FORMAT(mangled_name, buffer_size, ")");

cleanup:
    return err;
}
