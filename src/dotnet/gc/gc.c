#include "dotnet/types.h"
#include "dotnet/metadata/metadata.h"
#include "tomatodotnet/types/type.h"
#include "util/except.h"
#include "tomatodotnet/util/stb_ds.h"


void tdn_gc_trace_children(Object obj) {
    RuntimeTypeInfo type = obj->VTable->Type;

    // scan the type instance itself, to ensure we don't delete it before
    // we handle all of the children
    tdn_host_gc_trace_object(obj, (Object)type);

    //
    // some runtime objects have native structures that we need to properly scan
    // this is easier than having the native heap also be scanned
    //
    if (type == tRuntimeAssembly) {
        RuntimeAssembly assembly = (RuntimeAssembly)obj;
        for (int i = 0; i < hmlen(assembly->StringTable); i++) {
            tdn_host_gc_trace_object(obj, (Object)assembly->StringTable[i].value);
        }

    } else if (type == tRuntimeMethodInfo) {
        RuntimeMethodInfo method = (RuntimeMethodInfo)obj;
        for (int i = 0; i < hmlen(method->GenericMethodInstances); i++) {
            tdn_host_gc_trace_object(obj, (Object)method->GenericMethodInstances[i].value);
        }

    } else if (type == tRuntimeTypeInfo) {
        RuntimeTypeInfo typ = (RuntimeTypeInfo)obj;
        for (int i = 0; i < hmlen(typ->GenericTypeInstances); i++) {
            tdn_host_gc_trace_object(obj, (Object)typ->GenericTypeInstances[i].value);
        }

        for (int i = 0; i < hmlen(typ->InterfaceImpls); i++) {
            tdn_host_gc_trace_object(obj, (Object)typ->InterfaceImpls[i].key);
            if ((Object)typ->InterfaceImpls[i].next != NULL) {
                tdn_host_gc_trace_object(obj, (Object)typ->InterfaceImpls[i].next);
            }
        }
    }

    //
    // and now actually scan it
    //
    if (type->IsArray) {
        type = type->ElementType;
        if (tdn_type_is_valuetype(type)) {
            if (arrlen(type->ManagedPointers) != 0) {
                ASSERT(!"TODO: array with managed structs");
            }
        } else {
            // array with objects
            Object_Array arr = (Object_Array)obj;
            for (int i = 0; i < arr->Length; i++) {
                Object child = arr->Elements[i];
                if (child != NULL) {
                    tdn_host_gc_trace_object(obj, child);
                }
            }
        }
    } else {
        // get the start of the object, if its a boxed type then we
        // need to ignore the fields
        void* obj_ptr = obj;
        if (tdn_type_is_valuetype(type)) {
            obj_ptr += tdn_get_boxed_value_offset(type);
        }

        // go over all the managed pointers
        int len = arrlen(type->ManagedPointers);
        for (int i = 0; i < len; i++) {
            // get the child object
            Object child = *(Object*)(obj_ptr + type->ManagedPointers[i]);
            if (child != NULL) {
                tdn_host_gc_trace_object(obj, child);
            }
        }
    }
}

void tdn_gc_free(Object obj) {
    RuntimeTypeInfo type = obj->VTable->Type;

    if (type == tRuntimeTypeInfo && (RuntimeTypeInfo)obj != type) {
        RuntimeTypeInfo value = (RuntimeTypeInfo)obj;
        hmfree(value->InterfaceImpls);
        hmfree(value->GenericTypeInstances);
        arrfree(value->ManagedPointers);
        tdn_host_free(value->JitVTable);

    } else if (type == tRuntimeAssembly) {
        RuntimeAssembly value = (RuntimeAssembly)obj;
        hmfree(value->StringTable);
        dotnet_free_file(value->Metadata);
        tdn_host_free(value->Metadata);

    } else if (type == tRuntimeMethodInfo) {
        RuntimeMethodInfo value = (RuntimeMethodInfo)obj;
        hmfree(value->GenericMethodInstances);

    } else if (type == tRuntimeFieldInfo) {
        RuntimeFieldInfo value = (RuntimeFieldInfo)obj;

        if (value->JitFieldPtr != NULL) {
            // Unregister gc roots
            if (tdn_type_is_valuetype(value->FieldType)) {
                // register the children of the struct
                for (int i = 0; i < arrlen(value->FieldType->ManagedPointers); i++) {
                    tdn_host_gc_unregister_root(value->JitFieldPtr + value->FieldType->ManagedPointers[i]);
                }
            } else {
                // a pointer to an object, register it directly
                tdn_host_gc_unregister_root(value->JitFieldPtr);
            }

            // free the memory
            if (!value->HasRVA) {
                tdn_host_free(value->JitFieldPtr);
            }
        }
    }
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
    return tdn_host_gc_alloc(type->JitVTable, ALIGN_UP(size, type->HeapAlignment), type->HeapAlignment);
}
