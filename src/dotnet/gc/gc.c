#include "gc.h"
#include "tomatodotnet/host.h"
#include "util/except.h"
#include "dotnet/metadata/metadata.h"
#include "util/stb_ds.h"

// static Object m_gc_first;
//
// void gc_free_all() {
//     Object obj = m_gc_first;
//     while (obj != NULL) {
//         Object next = obj->next;
//
//         if (obj->ObjectType == tRuntimeAssembly) {
//             RuntimeAssembly assembly = (RuntimeAssembly)obj;
//             if (assembly->Metadata != NULL) {
//                 assembly->Metadata->file.close_handle(assembly->Metadata->file.handle);
//                 dotnet_free_file(assembly->Metadata);
//                 tdn_host_free(assembly->Metadata);
//             }
//             hmfree(assembly->StringTable);
//         } else if (obj->ObjectType == tRuntimeTypeInfo) {
//             RuntimeTypeInfo type = (RuntimeTypeInfo)obj;
//             hmfree(type->GenericTypeInstances);
//         }
//
//         tdn_host_free(obj);
//         obj = next;
//     }
// }

void* gc_new(RuntimeTypeInfo type, size_t size) {
    if (type == NULL) {
        ERROR("Tried to allocate object with null type?");
    } else {
        // if this is a generic type or a generic type parameter then we
        // can't create an instance of this object
        if (type->IsGenericParameter) {
            ERROR("Tried to create a generic type parameter");
            return NULL;
        }

        if (type->GenericTypeDefinition == type) {
            ERROR("Tried to create a generic type definition");
            return NULL;
        }

        if (type->Attributes.Abstract) {
            ERROR("Tried to create an abstract class");
            return NULL;
        }
    }

    Object object = tdn_host_gc_alloc(size);
    if (object == NULL) {
        return NULL;
    }

    object->ObjectType = type;
    return object;
}

void* gc_raw_alloc(size_t size) {
    Object object = tdn_host_gc_alloc(size);
    if (object == NULL) {
        return NULL;
    }
    return object;
}

void gc_register_root(void* ptr) {
    tdn_host_gc_register_root(ptr);
}
