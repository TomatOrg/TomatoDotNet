#include "gc.h"
#include "tomatodotnet/host.h"
#include "util/except.h"
#include "dotnet/metadata/metadata.h"
#include "util/stb_ds.h"

static Object m_gc_first;

void gc_free_all() {
    Object obj = m_gc_first;
    while (obj != NULL) {
        Object next = obj->next;

        if (obj->ObjectType == tRuntimeAssembly) {
            RuntimeAssembly assembly = (RuntimeAssembly)obj;
            assembly->Metadata->file.close_handle(assembly->Metadata->file.handle);
            hmfree(assembly->StringTable);
            dotnet_free_file(assembly->Metadata);
            tdn_host_free(assembly->Metadata);
        } else if (obj->ObjectType == tRuntimeTypeInfo) {
            RuntimeTypeInfo type = (RuntimeTypeInfo)obj;
            hmfree(type->GenericTypeInstances);
        }

        tdn_host_free(obj);
        obj = next;
    }
}

void* gc_new(RuntimeTypeInfo type, size_t size) {
    if (type == NULL) {
        ERROR("Tried to allocate object with null type?");
    }

    Object object = tdn_host_mallocz(size);
    if (object == NULL) {
        return NULL;
    }

    object->ObjectType = type;
    object->next = m_gc_first;
    m_gc_first = object;
    return object;
}

void* gc_raw_alloc(size_t size) {
    Object object = tdn_host_mallocz(size);
    if (object == NULL) {
        return NULL;
    }

    object->next = m_gc_first;
    m_gc_first = object;
    return object;
}

static void** m_gc_roots = NULL;

void gc_register_root(void* ptr) {
    arrpush(m_gc_roots, ptr);
}
