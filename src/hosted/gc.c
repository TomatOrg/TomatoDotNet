#include "types.h"

System_Object heap_find_fast(void *ptr) {
    return NULL;
}

void* gc_new(System_Type type, size_t size) {
    // allocate the object
    System_Object o = malloc(size);
    memset(o, 0, size); // this is necessary now, idk why
    o->color = 0;

    // set the object type
    if (type != NULL) {
        ASSERT(type->VTable != NULL);
        o->vtable = type->VTable;
    }

    // if there is no finalize then always suppress the finalizer
    if (type != NULL) {
        o->suppress_finalizer = type->Finalize == NULL;
    }

    return o;
}

static void write_field(void* o, size_t offset, void* new) {
    *(void**)((uintptr_t)o + offset) = new;
}

void gc_update(void* o, size_t offset, void* new) {
    write_field(o, offset, new);
}

void gc_add_root() {}

void gc_update_ref(void* ptr, void* new) {
    //System_Object object = heap_find((uintptr_t)ptr);
    //if (object != NULL) {
    //gc_update(object, (uintptr_t)ptr - (uintptr_t)object, new);
    //} else {
    *((void**)ptr) = new;
    //}
}
