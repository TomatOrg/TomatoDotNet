#include "gc.h"

#include <stdalign.h>
#include <util/string.h>

#include "tomatodotnet/host.h"
#include "util/except.h"
#include "dotnet/metadata/metadata.h"
#include "util/stb_ds.h"

#include <sanitizer/asan_interface.h>

#include "size_class.h"
#include "tomatodotnet/tdn.h"

void tdn_init_gc(uintptr_t base_address) {
    gc_size_class_init();
}

__attribute__((noinline))
static void* gc_slow_alloc_small(size_t size, uint32_t size_class) {
    return tdn_host_gc_alloc(size, MIN(size, 16));
}

static inline void* gc_fast_alloc(size_t size, size_t align) {
    // If the size is too large its not a fast-path anymore.
    int size_class = gc_get_size_class(size, align);
    if (UNLIKELY(size_class < 0)) {
        ASSERT(!"gc_slow_alloc_large");
    }

    // Fast path for allocating small size memory.
    // TODO: cpu cache allocation
    void* ret = NULL;
    if (UNLIKELY(ret == NULL)) {
        ret = gc_slow_alloc_small(size, size_class);
    }

    return ret;
}

void* tdn_gc_new(RuntimeTypeInfo type, size_t size) {
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

    // allocate the object itself
    Object object = gc_fast_alloc(size, type->HeapAlignment);
    if (UNLIKELY(object == NULL)) {
        return NULL;
    }

    // set the vtable
    object->VTable = type->JitVTable;

    return object;
}
