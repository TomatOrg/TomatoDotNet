#include "gc.h"

#include <stdalign.h>
#include <util/string.h>

#include "tomatodotnet/host.h"
#include "util/except.h"
#include "dotnet/metadata/metadata.h"
#include "util/stb_ds.h"

void* gc_new(RuntimeTypeInfo type, size_t size) {
    if (type == NULL) {
        ASSERT(!"Tried to allocate object with null type?");
    } else {

        // if this is a generic type or a generic type parameter then we
        // can't create an instance of this object
        if (type->IsGenericParameter) {
            ASSERT(!"Tried to create a generic type parameter");
            return NULL;
        }

        if (type->GenericTypeDefinition == type) {
            ASSERT(!"Tried to create a generic type definition");
            return NULL;
        }

        if (type->Attributes.Abstract) {
            ASSERT(!"Tried to create an abstract class");
            return NULL;
        }

        if (type->JitVTable == NULL) {
            ASSERT(!"Tried to create a type without a vtable");
            return NULL;
        }
    }

    Object object = tdn_host_gc_alloc(size, type->HeapAlignment);
    if (object == NULL) {
        return NULL;
    }

    // set the vtable
    object->VTable = type->JitVTable;

    return object;
}

void* gc_raw_alloc(size_t size) {
    Object object = tdn_host_gc_alloc(size, alignof(size_t));
    if (object == NULL) {
        return NULL;
    }
    return object;
}

void gc_register_root(void* ptr) {
    tdn_host_gc_register_root(ptr);
}
