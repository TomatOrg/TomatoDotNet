#include "gc.h"
#include "tinydotnet/host.h"
#include "util/except.h"

static System_Object m_gc_first;

void gc_free_all() {
    System_Object obj = m_gc_first;
    while (obj != NULL) {
        System_Object next = obj->next;
        tdn_host_free(obj);
        obj = next;
    }
}

void* gc_new(System_Type type, size_t size) {
    if (type == NULL) {
        ERROR("Tried to allocate object with null type?");
    }

    System_Object object = tdn_host_mallocz(size);
    object->ObjectType = type;
    object->next = m_gc_first;
    m_gc_first = object;
    return object;
}

void* gc_raw_alloc(size_t size) {
    System_Object object = tdn_host_mallocz(size);
    object->next = m_gc_first;
    m_gc_first = object;
    return object;
}
