#include "gc.h"

#include <stdlib.h>

typedef struct gc_header {
    // is this object marked
    uint8_t marked;

    // the type of this object
    type_t* type;

    // the memory of the object
    uint8_t memory[0];
} gc_header_t;

static gc_header_t* get_header(void* ptr) {
    return (gc_header_t*)((uintptr_t)ptr - sizeof(gc_header_t));
}

static void mark_object(void* ptr);

static void mark_members(type_t* type, uint8_t* memory) {
    // mark the object this object extends
    if (type->extends != NULL) {
        mark_members(type->extends, memory);
    }

    // mark the members of our object
    for (int i = 0; i < type->fields_count; i++) {
        field_t* field = &type->fields[i];
        if (field->is_static) continue;

        if (field->type->is_value_type) {
            // This is a value type, iterate its members to mark any references
            mark_members(type->fields[i].type, memory + field->offset);
        } else {
            // This is a reference type, mark it
            mark_object(*(void**)(memory + field->offset));
        }
    }
}

static void mark_object(void* ptr) {
    // mark the header and its members
    gc_header_t* header = get_header(ptr);
    header->marked = 1;
    mark_members(header->type, header->memory);
}

void* gc_alloc(type_t* type) {
    return malloc(type->memory_size);
}
