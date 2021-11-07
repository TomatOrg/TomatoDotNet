#include "gc.h"

#include <stdlib.h>

typedef struct gc_header {
} gc_header_t;

void* gc_alloc(type_t* type) {
    return malloc(type->memory_size);
}
