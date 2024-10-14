#include <stddef.h>
#include "loader.h"
#include "tomatodotnet/except.h"
#include "tomatodotnet/types/reflection.h"
#include "dotnet/metadata/pe.h"
#include "util/except.h"
#include "util/string.h"
#include "dotnet/metadata/metadata.h"
#include "dotnet/gc/gc.h"
#include "dotnet/metadata/sig.h"
#include "util/stb_ds.h"
#include "tomatodotnet/jit/jit.h"
#include <tomatodotnet/types/type.h>
#include <util/prime.h>

#include "jit/jit.h"

typedef struct memory_file_handle {
     void* buffer;
     size_t buffer_size;
} memory_file_handle_t;

static tdn_err_t memory_file_read(void* _handle, size_t offset, size_t size, void* buffer) {
    tdn_err_t err = TDN_NO_ERROR;
    memory_file_handle_t* handle = _handle;

    CHECK(handle->buffer_size >= size);
    CHECK(handle->buffer_size - size >= offset);
    memcpy(buffer, handle->buffer + offset, size);

cleanup:
    return err;
}

static void memory_file_close(void* _handle) {
    memory_file_handle_t* handle = _handle;
    tdn_host_free(handle->buffer);
    tdn_host_free(handle);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct init_type {
    const char* namespace;
    const char* name;
    size_t stack_size;
    size_t stack_alignment;
    size_t heap_size;
    size_t heap_alignment;
    RuntimeTypeInfo* dest;
    bool is_unmanaged;
} init_type_t;

typedef struct load_type {
    const char* namespace;
    const char* name;
    RuntimeTypeInfo* dest;
    size_t vtable_size;
} load_type_t;

#define INIT_VALUE_TYPE(namespace, name, is_unmanaged) \
    { \
        #namespace, \
        #name, \
        sizeof(name), \
        _Alignof(name), \
        sizeof(name), \
        _Alignof(name), \
        &t##name, \
        is_unmanaged \
    }

#define INIT_HEAP_TYPE(namespace, name) \
    { \
        #namespace, \
        #name, \
        sizeof(name), \
        _Alignof(name), \
        sizeof(struct name), \
        _Alignof(struct name), \
        &t##name \
    }

/**
 * Types that are initialized with some static information, required
 * for some of the recursive by nature (System.Int32 has int)
 */
static init_type_t m_init_types[] = {
    INIT_VALUE_TYPE(System, ValueType, true),
    INIT_VALUE_TYPE(System, Enum, true),
    INIT_VALUE_TYPE(System, Boolean, true),
    INIT_VALUE_TYPE(System, Char, true),
    INIT_VALUE_TYPE(System, SByte, true),
    INIT_VALUE_TYPE(System, Int16, true),
    INIT_VALUE_TYPE(System, Int32, true),
    INIT_VALUE_TYPE(System, Int64, true),
    INIT_VALUE_TYPE(System, IntPtr, true),
    INIT_VALUE_TYPE(System, Byte, true),
    INIT_VALUE_TYPE(System, UInt16, true),
    INIT_VALUE_TYPE(System, UInt32, true),
    INIT_VALUE_TYPE(System, UInt64, true),
    INIT_VALUE_TYPE(System, UIntPtr, true),
    INIT_VALUE_TYPE(System, Void, true),
};
static int m_inited_types = 0;

#define LOAD_TYPE(namespace, name) \
    { \
        #namespace, \
        #name, \
        &t##name, \
        4 \
    }

#define LOAD_TYPE_VTABLE(namespace, name, vtable) \
    { \
        #namespace, \
        #name, \
        &t##name, \
        vtable \
    }

/**
 * Types to load so the runtime can access them
 */
static load_type_t m_load_types[] = {
    LOAD_TYPE(System, Object),
    LOAD_TYPE(System, Array),
    LOAD_TYPE(System, String),
    LOAD_TYPE_VTABLE(System.Reflection, MethodBase, 5),
    LOAD_TYPE(System.Reflection, RuntimeAssembly),
    LOAD_TYPE(System.Reflection, RuntimeModule),
    LOAD_TYPE_VTABLE(System.Reflection, RuntimeFieldInfo, 5),
    LOAD_TYPE(System.Reflection, RuntimeMethodBody),
    LOAD_TYPE_VTABLE(System.Reflection, RuntimeMethodInfo, 5),
    LOAD_TYPE(System.Reflection, RuntimeConstructorInfo),
    LOAD_TYPE(System.Reflection, RuntimeLocalVariableInfo),
    LOAD_TYPE_VTABLE(System.Reflection, RuntimeTypeInfo, 5),
    LOAD_TYPE(System.Reflection, ParameterInfo),
    LOAD_TYPE(System.Reflection, RuntimeExceptionHandlingClause),
    LOAD_TYPE(System.Runtime.CompilerServices, IsReadOnlyAttribute),
    LOAD_TYPE(System.Runtime.CompilerServices, IsVolatile),
    LOAD_TYPE(System.Runtime.CompilerServices, Unsafe),
    LOAD_TYPE(System.Runtime.InteropServices, MemoryMarshal),
    LOAD_TYPE(System.Runtime.InteropServices, InAttribute),
    LOAD_TYPE(System, RuntimeTypeHandle),
    { "System", "Nullable`1", &tNullable, 4 },
};
static int m_loaded_types = 0;

/**
 * Contains the Core assembly, where the most basic types are stored
 */
static RuntimeAssembly mCoreAssembly = NULL;

static tdn_err_t create_vtable(RuntimeTypeInfo type, int count) {
    tdn_err_t err = TDN_NO_ERROR;

    // create the jit vtable
    CHECK(type->JitVTable == NULL);
    type->JitVTable = tdn_host_mallocz_low(sizeof(ObjectVTable) + count * sizeof(void*));
    CHECK_ERROR(type->JitVTable != NULL, TDN_ERROR_OUT_OF_MEMORY);

    // set the type in the vtable
    type->JitVTable->Type = type;

    // for verification later
    type->VTableSize = count;

cleanup:
    return err;
}

/**
 * Searches for both init-types and load-types for the given type, if it is found then it will be created
 * and returned, otherwise a null is returned out
 */
static tdn_err_t corelib_create_type(metadata_type_def_t* type_def, RuntimeTypeInfo* out_type) {
    tdn_err_t err = TDN_NO_ERROR;

    // we need to now fill the type array, while also initializing
    // all of the other needed types
    for (int i = 0; i < ARRAY_LENGTH(m_init_types); i++) {
        init_type_t* init_type = &m_init_types[i];
        if (
            strcmp(init_type->namespace, type_def->type_namespace) == 0 &&
            strcmp(init_type->name, type_def->type_name) == 0
        ) {
            RuntimeTypeInfo type = *init_type->dest ? *init_type->dest : GC_NEW(RuntimeTypeInfo);
            type->StackSize = init_type->stack_size;
            type->StackAlignment = init_type->stack_alignment;
            type->HeapSize = init_type->heap_size;
            type->HeapAlignment = init_type->heap_alignment;
            type->FillingHeapSize = 1;
            type->EndFillingHeapSize = 1;
            type->FillingStackSize = 1;
            type->EndFillingStackSize = 1;
            type->QueuedTypeInit = 1;
            type->IsUnmanaged = init_type->is_unmanaged;
            *init_type->dest = type;
            *out_type = type;
            m_inited_types++;
            goto cleanup;
        }
    }

    // another special case, we just need the type, nothing else
    for (int i = 0; i < ARRAY_LENGTH(m_load_types); i++) {
        load_type_t* load_type = &m_load_types[i];
        if (
            strcmp(load_type->namespace, type_def->type_namespace) == 0 &&
            strcmp(load_type->name, type_def->type_name) == 0
        ) {
            RuntimeTypeInfo type = *load_type->dest ? *load_type->dest : GC_NEW(RuntimeTypeInfo);
            if (type->JitVTable == NULL) {
                CHECK_AND_RETHROW(create_vtable(type, load_type->vtable_size));
            }

            type->QueuedTypeInit = 1;
            *load_type->dest = type;
            *out_type = type;
            m_loaded_types++;
            goto cleanup;
        }
    }

    *out_type = NULL;

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Type size setup
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Sort the arrays of fields by their alignment, using quick-sort
 */
static void sort_fields_by_alignment(RuntimeFieldInfo_Array arr, int low, int high) {
    if (low > high || low < 0) {
        return;
    }

    // perform the partition
    size_t pivot = arr->Elements[high]->FieldType->StackAlignment;
    int pi = low - 1;
    for (int j = low; j <= high - 1; j++) {
        if (arr->Elements[j]->FieldType->StackAlignment >= pivot) {
            pi++;
            RuntimeFieldInfo temp = arr->Elements[pi];
            arr->Elements[pi] = arr->Elements[j];
            arr->Elements[j] = temp;
        }
    }

    pi++;
    RuntimeFieldInfo temp = arr->Elements[pi];
    arr->Elements[pi] = arr->Elements[high];
    arr->Elements[high] = temp;

    // recursively sort it
    sort_fields_by_alignment(arr, low, pi - 1);
    sort_fields_by_alignment(arr, pi + 1, high);
}

static tdn_err_t fill_heap_size(RuntimeTypeInfo type);

/**
 * Fills the stack size and alignment of the object, should be called whenever the object
 * is accessed on the stack or as a field in another class
 */
static tdn_err_t fill_stack_size(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    if (type->EndFillingStackSize) {
        goto cleanup;
    }

    CHECK(!type->FillingStackSize);
    type->FillingStackSize = 1;

    // don't allow byref structs in unverified assemblies
    // otherwise we can have a very bad time
    if (type->IsByRefStruct) {
        CHECK(type->Module->Assembly->AllowUnsafe);
    }

    // value types have the same size as the heap size, while
    // reference types always have the size of a pointer
    if (tdn_type_is_valuetype(type)) {
        CHECK_AND_RETHROW(fill_heap_size(type));
        type->StackSize = type->HeapSize;
        type->StackAlignment = type->HeapAlignment;
    } else if (type->Attributes.Interface) {
        type->StackAlignment = _Alignof(Interface);
        type->StackSize = sizeof(Interface);
    } else {
        type->StackAlignment = _Alignof(Object);
        type->StackSize = sizeof(Object);
    }

    CHECK(type->StackSize <= SIZE_1MB);

    type->EndFillingStackSize = 1;

cleanup:
    return err;
}

/**
 * Fills the heap size of the object, should be called when creating the type
 * or this is a base type of another heap size calculation
 */
static tdn_err_t fill_heap_size(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // if we have the heap alignment already
    // we are def done
    if (type->EndFillingHeapSize) {
        goto cleanup;
    }

    CHECK(!type->FillingHeapSize);
    type->FillingHeapSize = 1;

    // if this is not a value type then we need to have the stack size of the
    // object at this point, otherwise it will be set after we are done in here
    if (!tdn_type_is_valuetype(type)) {
        CHECK_AND_RETHROW(fill_stack_size(type));
    }

    // get all the instance fields so we can give them an offset
    // start by counting then fill
    int count = 0;
    for (int i = 0; i < type->DeclaredFields->Length; i++) {
        if (!type->DeclaredFields->Elements[i]->Attributes.Static) {

            // if the type is readonly make sure all the fields are also readonly
            // (aka InitOnlt)
            if (type->IsReadOnly) {
                CHECK(type->DeclaredFields->Elements[i]->Attributes.InitOnly);
            }

            count++;
        }
    }

    // interface can't have instance fields
    if (type->Attributes.Interface) {
        // put invalid values so it will never be taken into account
        type->HeapAlignment = UINT32_MAX;
        type->HeapSize = UINT32_MAX;

        CHECK(count == 0);
        goto skip_field_allocation;
    }

    RuntimeFieldInfo_Array fields = GC_NEW_ARRAY(RuntimeFieldInfo, count);
    count = 0;
    for (int i = 0; i < type->DeclaredFields->Length; i++) {
        if (!type->DeclaredFields->Elements[i]->Attributes.Static) {
            fields->Elements[count++] = type->DeclaredFields->Elements[i];
        }
    }

    // based on the layout setup the fields
    if (type->Attributes.Layout == TDN_TYPE_LAYOUT_EXPLICIT) {
        CHECK_FAIL();
        // TODO: add support for explicit layout
    } else {
        // start by calculating the BaseType
        size_t current_size = 0;

        // we will treat the base type as essentially an invisible field
        // but because we extend it we need to take the heap size of it
        if (type->BaseType != NULL) {
            CHECK_AND_RETHROW(fill_heap_size(type->BaseType));
            current_size = type->BaseType->HeapSize;
        }

        // get the fields by the order we want them in the struct,
        // we are going to find the largest alignment on the way, just
        // to figure the alignment that we need to do
        if (type->Attributes.Layout == TDN_TYPE_LAYOUT_AUTO) {
            // in auto-layout we are going to sort by the alignment first
            sort_fields_by_alignment(fields, 0, fields->Length - 1);
        }

        // find the largest field for alignment calculation,
        // also fills their size
        bool is_managed = false;
        size_t largest_alignment = 0;
        for (int i = 0; i < fields->Length; i++) {
            RuntimeFieldInfo field = fields->Elements[i];
            RuntimeTypeInfo field_type = field->FieldType;
            if (field->Attributes.Static) {
                continue;
            }
            CHECK_AND_RETHROW(fill_stack_size(field_type));
            if (tdn_type_is_referencetype(field_type) || !field_type->IsUnmanaged) {
                is_managed = true;
            }

            // make sure we don't include a byref struct inside
            // of another byref struct
            if (type->IsByRefStruct && field_type->IsByRef) {
                CHECK(!field_type->ElementType->IsByRefStruct);
            }

            // make sure a byref is only inside of a byref struct
            if (field_type->IsByRef) {
                CHECK(type->IsByRefStruct);
            }

            largest_alignment = MAX(largest_alignment, fields->Elements[i]->FieldType->StackAlignment);
        }
        type->IsUnmanaged = !is_managed;

        // align the alignment nicely, and then align the base of our own data
        size_t alignment = MIN(largest_alignment, type->Packing ? type->Packing : _Alignof(size_t));
        if (alignment > 64) alignment = 128;
        else if (alignment > 32) alignment = 64;
        else if (alignment > 16) alignment = 32;
        else if (alignment > 8) alignment = 16;
        else if (alignment > 4) alignment = 8;
        else if (alignment > 2) alignment = 4;
        else if (alignment == 0) alignment = 1;
        current_size = ALIGN_UP(current_size, alignment);

        // now go over the fields and calculate the correct offset for them
        for (int i = 0; i < fields->Length; i++) {
            RuntimeFieldInfo field = fields->Elements[i];
            RuntimeTypeInfo field_type = field->FieldType;
            if (field->Attributes.Static) {
                continue;
            }

            // align up the size
            size_t field_alignment = MIN(alignment, field_type->StackSize);
            current_size = ALIGN_UP(current_size, field_alignment);

            // fill the field
            field->FieldOffset = current_size;

            // take the size up
            current_size += field_type->StackSize;
        }

        // and align the total struct properly, so it will always
        // have the alignment that we want it to have
        current_size = ALIGN_UP(current_size, alignment);

        // there are size limits, valuetype must be less
        // than 1mb (per spec) and other types must be
        // less than 2GB (GC limit)
        if (tdn_type_is_valuetype(type)) {
            CHECK(current_size <= SIZE_1MB);
        } else {
            CHECK(current_size <= SIZE_2GB);
        }

        // and now fill it in, if the heap size is already filled
        // from the class layout then use it right now
        type->HeapAlignment = alignment;
        if (type->HeapSize != 0) {
            CHECK(type->HeapSize >= current_size);
        } else {
            type->HeapSize = current_size;
        }
    }

    CHECK(type->HeapSize <= SIZE_2GB);

    // and now that we are done, if this is a value type
    // then also fill the stack size
    if (tdn_type_is_valuetype(type) && !type->FillingStackSize) {
        CHECK_AND_RETHROW(fill_stack_size(type));
    }

skip_field_allocation:

    type->EndFillingHeapSize = 1;

cleanup:
    return err;
}

static RuntimeMethodInfo find_overriden_method(RuntimeTypeInfo type, RuntimeMethodInfo method) {
    while (type != NULL) {
        // Use normal inheritance (I.8.10.4)
        for (int i = 0; i < type->DeclaredMethods->Length; i++) {
            RuntimeMethodInfo info = type->DeclaredMethods->Elements[i];

            // not virtual, continue
            if (!info->Attributes.Virtual)
                continue;

            // match the name
            if (!tdn_compare_string(info->Name, method->Name))
                continue;

            // check the return type
            if (info->ReturnParameter->ParameterType != method->ReturnParameter->ParameterType)
                continue;

            // Check parameter count matches
            if (info->Parameters->Length != method->Parameters->Length)
                continue;

            // check the parameters
            bool signatureMatch = true;
            for (int j = 0; j < info->Parameters->Length; j++) {
                ParameterInfo paramA = info->Parameters->Elements[j];
                ParameterInfo paramB = method->Parameters->Elements[j];
                if (paramA->ParameterType != paramB->ParameterType) {
                    signatureMatch = false;
                    break;
                }
            }
            if (!signatureMatch)
                continue;

            // set the offset
            return info;
        }

        // get the parent for next iteration
        type = type->BaseType;
    }

    return NULL;
}

static tdn_err_t fill_virtual_methods(RuntimeTypeInfo info) {
    tdn_err_t err = TDN_NO_ERROR;

    // finished already
    if (info->EndFillingVtable) {
        goto cleanup;
    }

    // prevent recursion
    CHECK(!info->FillingVtable);
    info->FillingVtable = true;

    // fill the vtable of the base class
    if (info->BaseType != NULL) {
        CHECK_AND_RETHROW(fill_virtual_methods(info->BaseType));
    }

    // allocate all interface offsets for the type
    RuntimeAssembly assembly = info->Module->Assembly;
    dotnet_file_t* metadata = assembly->Metadata;

    // the current offset of the vtable
    int vtable_offset = info->BaseType ? (info->BaseType->VTable ? info->BaseType->VTable->Length : 0) : 0;

    // ignore anything not of this type
    uint64_t interface_product = 1;
    for (int i = 0; i < metadata->interface_impls_count; i++) {
        metadata_interface_impl_t* impl = &metadata->interface_impls[i];
        if (impl->class.token != info->MetadataToken) {
            continue;
        }

        // get the interface type
        RuntimeTypeInfo interface;
        CHECK_AND_RETHROW(tdn_assembly_lookup_type(assembly, impl->interface.token, info->GenericArguments, NULL, &interface));

        // check if the parent already implemented this interface
        int parent_offset = -1;
        if (info->BaseType != NULL) {
            int idx = hmgeti(info->BaseType->InterfaceImpls, interface);
            if (idx >= 0) {
                parent_offset = info->BaseType->InterfaceImpls[idx].value;
            }
        }

        // TODO: maybe also go over all the interfaces that this interface exports and try
        //       to allocate area for them

        // if no offset was already allocated, allocate a new one
        if (parent_offset == -1) {
            CHECK_AND_RETHROW(fill_virtual_methods(interface));
            parent_offset = vtable_offset;
            vtable_offset += interface->VTable->Length;
        }

        // and now insert it
        hmput(info->InterfaceImpls, interface, parent_offset);

        // calculate the product and make sure it doesn't overflow
        CHECK(!__builtin_mul_overflow(interface_product, interface->TypeId, &interface_product));
    }

    // go over all the virtual methods and allocate vtable slots to all of them,
    for (int i = 0; i < info->DeclaredMethods->Length; i++) {
        RuntimeMethodInfo method = info->DeclaredMethods->Elements[i];

        // fixup readonly if the parent is readonly on the way
        if (info->IsReadOnly && !method->Attributes.Static) {
            method->IsReadOnly = true;
        }

        if (!method->Attributes.Virtual) {
            continue;
        }

        // if we already have an offset for
        // this one then we can ignore it
        if (method->VTableOffset >= 0) {
            continue;
        }

        // default to allocating a new slot
        if (!method->Attributes.VtableNewSlot) {
            // might override something else
            RuntimeMethodInfo parent = find_overriden_method(info->BaseType, method);
            CHECK(parent != NULL);

            CHECK(!parent->Attributes.Final);
            if (parent->Attributes.Strict) {
                // TODO: check accessibility
            }
            CHECK(parent->VTableOffset >= 0);
            method->VTableOffset = parent->VTableOffset;
        } else {
            method->VTableOffset = VTABLE_ALLOCATE_SLOT;
        }
    }

    // get the offset from the child
    for (int i = 0; i < info->DeclaredMethods->Length; i++) {
        RuntimeMethodInfo method = info->DeclaredMethods->Elements[i];
        if (!method->Attributes.Virtual) continue;

        if (method->VTableOffset == VTABLE_ALLOCATE_SLOT) {
            method->VTableOffset = vtable_offset++;
        } else {
            CHECK(method->VTableOffset >= 0, "%d", method->VTableOffset);
        }
    }

    // now allocate the new vtable
    info->VTable = GC_NEW_ARRAY(RuntimeMethodInfo, vtable_offset);

    // allocate the native vtable
    if (info->JitVTable == NULL) {
        CHECK_AND_RETHROW(create_vtable(info, vtable_offset));
    } else {
        CHECK(vtable_offset == info->VTableSize, "Got invalid VTABLE size %d/%d - %T", vtable_offset, info->VTableSize, info);
    }

    // set the type id information
    info->JitVTable->InterfaceProduct = interface_product;

    // copy entries from the parent
    if (info->BaseType != NULL) {
        // TODO: replace with gc_memcpy
        for (int vi = 0; vi < info->BaseType->VTable->Length; vi++) {
            info->VTable->Elements[vi] = info->BaseType->VTable->Elements[vi];
        }
    }

    // find all the implementations of the interfaces
    for (int i = 0; i < hmlen(info->InterfaceImpls); i++) {
        interface_impl_t* interface = &info->InterfaceImpls[i];

        for (int vi = 0; vi < interface->key->VTable->Length; vi++) {
            RuntimeMethodInfo base = interface->key->VTable->Elements[vi];
            RuntimeMethodInfo impl;

            // fallback to normal rules
            if (info->Attributes.Interface) {
                // for interface just store the original one
                impl = base;

            } else {
                // TODO: check for explicit implementations

                // fallback to normal override rules
                impl = find_overriden_method(info, base);

                // if the class is abstract and there is no implementation
                // then just
                if (info->Attributes.Abstract && impl == NULL) {
                    impl = base;
                }
            }
            CHECK(impl != NULL);

            info->VTable->Elements[interface->value + base->VTableOffset] = impl;
        }
    }

    // and now fill the normal slots as well
    for (int i = 0; i < info->DeclaredMethods->Length; i++) {
        RuntimeMethodInfo method = info->DeclaredMethods->Elements[i];
        if (method->Attributes.Virtual) {
            info->VTable->Elements[method->VTableOffset] = method;
        }
    }

    // make sure everything was implemented correctly
    for (int i = 0; i < info->VTable->Length; i++) {
        RuntimeMethodInfo method = info->VTable->Elements[i];
        CHECK(method != NULL, "vtable slot %d in %T not filled", i, info);

        if (!info->Attributes.Abstract) {
            CHECK(!method->Attributes.Abstract, "%T::%U used by %T",
                method->DeclaringType, method->Name, info);
        }
    }

    // we done
    info->EndFillingVtable = true;

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Method Body parsing
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define CorILMethod_TinyFormat 0x2
#define CorILMethod_FatFormat 0x3
#define CorILMethod_MoreSects 0x8
#define CorILMethod_InitLocals 0x10

typedef struct coril_method_fat {
    uint16_t flags : 12;
    uint16_t size : 4;
    uint16_t max_stack;
    uint32_t code_size;
    uint32_t local_var_sig_tok;
} PACKED coril_method_fat_t;

#define CorILMethod_Sect_EHTable 0x1
#define CorILMethod_Sect_OptILTable 0x2
#define CorILMethod_Sect_FatFormat 0x40
#define CorILMethod_Sect_MoreSects 0x80

typedef struct coril_method_sect_small {
    uint8_t kind;
    uint8_t data_size;
    uint16_t _reserved;
} PACKED coril_method_sect_small_t;

typedef struct coril_method_sect_fat {
    uint32_t kind : 8;
    uint32_t data_size : 24;
} PACKED coril_method_sect_fat_t;

typedef struct coril_exception_clause_small {
    uint16_t flags;
    uint16_t try_offset;
    uint8_t try_length;
    uint16_t handler_offset;
    uint8_t handler_length;
    union {
        uint32_t class_token;
        uint32_t filter_offset;
    };
} PACKED coril_exception_clause_small_t;

typedef struct coril_exception_clause_fat {
    uint32_t flags;
    uint32_t try_offset;
    uint32_t try_length;
    uint32_t handler_offset;
    uint32_t handler_length;
    union {
        uint32_t class_token;
        uint32_t filter_offset;
    };
} PACKED coril_exception_clause_fat_t;

static tdn_err_t tdn_parse_method_exception_handling_clauses(
    RuntimeMethodBase method_base,
    bool fat, void* data, size_t size,
    size_t code_size
) {
    tdn_err_t err = TDN_NO_ERROR;

    // figure the count before hand
    size_t count = size / (fat ? sizeof(coril_exception_clause_fat_t) : sizeof(coril_exception_clause_small_t));

    RuntimeExceptionHandlingClause_Array clauses = GC_NEW_ARRAY(RuntimeExceptionHandlingClause, count);
    method_base->MethodBody->ExceptionHandlingClauses = clauses;

    // iterate all the clauses
    size_t i = 0;
    while (size != 0) {
        // get the clause, expand from a small one if needed
        coril_exception_clause_fat_t clause;
        if (!fat) {
            CHECK(size >= sizeof(coril_exception_clause_small_t));
            coril_exception_clause_small_t header = *(coril_exception_clause_small_t*)data;
            clause.flags = header.flags;
            clause.try_offset = header.try_offset;
            clause.try_length = header.try_length;
            clause.handler_offset = header.handler_offset;
            clause.handler_length = header.handler_length;
            clause.class_token = header.class_token;
            data += sizeof(coril_exception_clause_small_t);
            size -= sizeof(coril_exception_clause_small_t);
        } else {
            CHECK(size >= sizeof(coril_exception_clause_fat_t));
            clause = *(coril_exception_clause_fat_t*)data;
            data += sizeof(coril_exception_clause_fat_t);
            size -= sizeof(coril_exception_clause_fat_t);
        }

        // make sure it is only a valid set of flags
        CHECK(
            clause.flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION ||
            clause.flags == COR_ILEXCEPTION_CLAUSE_FINALLY ||
            clause.flags == COR_ILEXCEPTION_CLAUSE_FAULT
        );

        // TODO: clause.flags == COR_ILEXCEPTION_CLAUSE_FILTER

        // verify the offsets
        CHECK(clause.handler_length >= 0);
        CHECK(clause.handler_offset >= 0);
        CHECK(clause.try_length >= 0);
        CHECK(clause.try_offset >= 0);
        CHECK(clause.handler_length >= 0);

        CHECK(clause.handler_offset < code_size);
        CHECK((int64_t)clause.handler_offset + clause.handler_length <= code_size);

        CHECK(clause.try_offset < code_size);
        CHECK((int64_t)clause.try_offset + clause.try_length <= code_size);

        // TODO: check no overlap
        // TODO: check no overlap with any other entry already parsed

        if (clause.flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
            CHECK(clause.filter_offset >= 0);
            CHECK(clause.filter_offset < code_size);

            // filter must come before the length
            CHECK(clause.filter_offset < clause.handler_offset);
        }

        // our jit mandates the handler comes before the try so we will always know everything only
        // after going through the try region, this is only important for finally regions
        // NOTE: this is not written in the spec
        if (clause.flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
            CHECK(clause.try_offset < clause.handler_offset);
        }

        // and now create it and fill it
        RuntimeExceptionHandlingClause c = GC_NEW(RuntimeExceptionHandlingClause);
        c->Flags = clause.flags;
        c->TryOffset = clause.try_offset;
        c->TryLength = clause.try_length;
        c->HandlerOffset = clause.handler_offset;
        c->HandlerLength = clause.handler_length;
        if (clause.flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
            CHECK_AND_RETHROW(tdn_assembly_lookup_type(
                    method_base->Module->Assembly,
                    clause.class_token,
                    method_base->DeclaringType->GenericArguments,
                    method_base->GenericArguments,
                    &c->CatchType));
        } else if (clause.flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
            c->FilterOffset = clause.filter_offset;
        }
        clauses->Elements[i++] = c;
    }

cleanup:
    return err;
}

tdn_err_t tdn_parser_method_body(
    RuntimeAssembly assembly,
    metadata_method_def_t* method_def,
    RuntimeMethodBase methodBase
) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeMethodBody body = GC_NEW(RuntimeMethodBody);
    methodBase->MethodBody = body;

    // get the tiny header for the start
    uint8_t* start = pe_image_address(&assembly->Metadata->file, method_def->rva);
    uint8_t* end = pe_image_address(&assembly->Metadata->file, method_def->rva + 1);
    CHECK(start != NULL && end != NULL);

    // parse the header
    size_t code_size = 0;
    size_t header_size = 1;
    uint8_t flags = *start & 0b11;
    if (flags == CorILMethod_TinyFormat) {
        // this is indeed tiny header, get the size
        code_size = *start >> 2;

        // set the default attributes
        body->MaxStackSize = 8;
        body->InitLocals = true;

    } else if (flags == CorILMethod_FatFormat) {
        // this is a big header, recalculate the end
        end = pe_image_address(&assembly->Metadata->file, method_def->rva + sizeof(coril_method_fat_t));
        CHECK(end != NULL);

        // take the new header size and check it
        coril_method_fat_t* fat = (coril_method_fat_t*)start;
        CHECK(fat->size == sizeof(coril_method_fat_t) / sizeof(uint32_t));
        header_size = sizeof(coril_method_fat_t);

        // update the flags with the fat ones
        flags = fat->flags;

        // get the main attributes
        body->MaxStackSize = fat->max_stack;
        body->InitLocals = fat->flags & CorILMethod_InitLocals;

        // parse the local variables
        body->LocalSignatureMetadataToken = (int)fat->local_var_sig_tok;
        if (fat->local_var_sig_tok != 0) {
            token_t token = { .token = body->LocalSignatureMetadataToken };
            CHECK(token.table == METADATA_STAND_ALONE_SIG);
            CHECK(token.index != 0 && token.index <= assembly->Metadata->stand_alone_sigs_count);
            metadata_stand_alone_sig_t* sig = &assembly->Metadata->stand_alone_sigs[token.index - 1];
            CHECK_AND_RETHROW(sig_parse_local_var_sig(
                    sig->signature,
                    assembly,
                    methodBase->DeclaringType->GenericArguments, methodBase->GenericArguments,
                    body));
        }

        code_size = fat->code_size;
    } else {
        CHECK_FAIL();
    }

    // now we have the code itself
    uint8_t* code_start = end;
    uint8_t* code_end = pe_image_address(&assembly->Metadata->file, method_def->rva + header_size + code_size);
    CHECK(code_end != NULL);
    CHECK(code_size <= INT32_MAX);

    // set the code
    body->ILSize = (int)code_size;
    body->IL = code_start;

    // there are more sections, process them
    if (flags & CorILMethod_MoreSects) {
        // make sure we can access the header
        uint32_t data_offset = method_def->rva + header_size + ALIGN_UP(code_size, sizeof(uint32_t));
        uint8_t* sect_start = pe_image_address(&assembly->Metadata->file, data_offset);
        CHECK(sect_start != NULL);
        CHECK(pe_image_address(&assembly->Metadata->file, data_offset + sizeof(uint32_t)) != NULL);

        // take one byte, we are going to make sure this is exception handling table
        // and that there are no more sections after it
        uint8_t kind = *sect_start;
        CHECK(kind & CorILMethod_Sect_EHTable);
        CHECK((kind & CorILMethod_Sect_MoreSects) == 0);

        // if not the fat format then extend the size
        size_t size;
        if (kind & CorILMethod_Sect_FatFormat) {
            coril_method_sect_fat_t* header = (coril_method_sect_fat_t*)sect_start;
            size = header->data_size;
        } else {
            coril_method_sect_small_t* header = (coril_method_sect_small_t*)sect_start;
            size = header->data_size;
        }

        // check the size beyond the header
        CHECK(pe_image_address(&assembly->Metadata->file, data_offset + size) != NULL);
        end += sizeof(uint32_t);

        // handle the extra data correctly
        CHECK_AND_RETHROW(tdn_parse_method_exception_handling_clauses(
                methodBase, kind & CorILMethod_Sect_FatFormat,
                sect_start + sizeof(uint32_t), size - sizeof(uint32_t), code_size));
    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Type filling
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct type_queue {
    RuntimeTypeInfo* types;
} type_queue_t;

static type_queue_t* m_type_queues = NULL;

static void push_type_queue() {
    arrpush(m_type_queues, (type_queue_t){});
}

/**
 * Used to generate primes for interfaces
 */
static prime_generator_t m_interface_prime_generator;

static tdn_err_t fill_type_id(RuntimeTypeInfo info) {
    tdn_err_t err = TDN_NO_ERROR;

    if (info->Attributes.Interface) {
        info->TypeId = prime_generate(&m_interface_prime_generator);
    } else {
        // TODO: this
    }

cleanup:
    return err;
}

tdn_err_t tdn_type_init(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    if (arrlen(m_type_queues) == 0) {
        // no delay, just calculate it
        CHECK_AND_RETHROW(fill_stack_size(type));
        CHECK_AND_RETHROW(fill_heap_size(type));
        CHECK_AND_RETHROW(fill_type_id(type));
        CHECK_AND_RETHROW(fill_virtual_methods(type));
    } else {
        // delayed for later
        arrpush(arrlast(m_type_queues).types, type);
    }

cleanup:
    return err;
}

static tdn_err_t drain_type_queue() {
    tdn_err_t err = TDN_NO_ERROR;
    type_queue_t queue = {0};

    CHECK(arrlen(m_type_queues) > 0);
    queue = arrpop(m_type_queues);

    // and init them all
    while (arrlen(queue.types) != 0) {
        RuntimeTypeInfo type = arrpop(queue.types);
        CHECK_AND_RETHROW(fill_stack_size(type));
        CHECK_AND_RETHROW(fill_heap_size(type));
        CHECK_AND_RETHROW(fill_type_id(type));
        CHECK_AND_RETHROW(fill_virtual_methods(type));
    }

cleanup:
    arrfree(queue.types);

    if (arrlen(m_type_queues) == 0) {
        arrfree(m_type_queues);
    }

    return err;
}

static void pop_type_queue() {
    if (arrlen(m_type_queues) > 0) {
        type_queue_t queue = arrpop(m_type_queues);
        arrfree(queue.types);

        if (arrlen(m_type_queues) == 0) {
            arrfree(m_type_queues);
        }
    } else {
        ERROR("Tried to pop queue but there was no queue to popup");
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Bootstrap of the type system
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static struct {
    const char* key;
    RuntimeAssembly value;
}* m_loaded_assemblies = NULL;

static tdn_err_t corelib_bootstrap() {
    tdn_err_t err = TDN_NO_ERROR;

    // start by initializing the System.Type type first
    tRuntimeTypeInfo = gc_raw_alloc(sizeof(struct RuntimeTypeInfo));
    CHECK_ERROR(tRuntimeTypeInfo != NULL, TDN_ERROR_OUT_OF_MEMORY);

    // make sure to set its type id properly
    CHECK_AND_RETHROW(create_vtable(tRuntimeTypeInfo, 5));
    tRuntimeTypeInfo->Object.VTable = (uint32_t)(uintptr_t)tRuntimeTypeInfo->JitVTable;

    // hard-code types we require for proper bootstrap
    tArray = GC_NEW(RuntimeTypeInfo); // for creating a Type[]
    tString = GC_NEW(RuntimeTypeInfo); // for creating a string
    tRuntimeAssembly = GC_NEW(RuntimeTypeInfo); // for creating the main assembly
    tRuntimeModule = GC_NEW(RuntimeTypeInfo); // for creating the main module

    // hard code to the correct amount of entries
    CHECK_AND_RETHROW(create_vtable(tArray, 4));
    CHECK_AND_RETHROW(create_vtable(tString, 4));
    CHECK_AND_RETHROW(create_vtable(tRuntimeAssembly, 4));
    CHECK_AND_RETHROW(create_vtable(tRuntimeModule, 4));

    // setup the basic type so GC_NEW_ARRAY can work
    CHECK_AND_RETHROW(tdn_create_string_from_cstr("RuntimeTypeInfo", &tRuntimeTypeInfo->Name));
    CHECK_AND_RETHROW(tdn_create_string_from_cstr("System.Reflection", &tRuntimeTypeInfo->Namespace));

cleanup:
    return err;
}

static tdn_err_t corelib_bootstrap_types(RuntimeAssembly assembly) {
    tdn_err_t err = TDN_NO_ERROR;

    //
    // finish setting up the type
    //
    tRuntimeTypeInfo->Module = assembly->Module;
    assembly->TypeDefs = GC_NEW_ARRAY(RuntimeTypeInfo, assembly->Metadata->type_defs_count);

    //
    // start by setting up the corelib types
    //

    for (int i = 0; i < assembly->TypeDefs->Length; i++) {
        metadata_type_def_t* type_def = &assembly->Metadata->type_defs[i];
        CHECK_AND_RETHROW(corelib_create_type(type_def, &assembly->TypeDefs->Elements[i]));
    }

    // make sure we loaded all the required types
    bool loaded_everything = true;
    if (m_loaded_types != ARRAY_LENGTH(m_load_types)) {
        ERROR("Failed to load some types:");

        for (int i = 0; i < ARRAY_LENGTH(m_load_types); i++) {
            if (*m_load_types[i].dest == NULL) {
                ERROR("\t- %s.%s", m_load_types[i].namespace, m_load_types[i].name);
            }
        }
        loaded_everything = false;
    }

    if (m_inited_types != ARRAY_LENGTH(m_init_types)) {
        if (loaded_everything)
            ERROR("Failed to load some types:");

        for (int i = 0; i < ARRAY_LENGTH(m_init_types); i++) {
            if (*m_init_types[i].dest == NULL) {
                ERROR("\t- %s.%s", m_init_types[i].namespace, m_init_types[i].name);
            }
        }
        loaded_everything = false;
    }

    CHECK(loaded_everything);

cleanup:
    return err;
}

static tdn_err_t corelib_jit_types(RuntimeAssembly assembly) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_AND_RETHROW(tdn_jit_init());

    //
    // jit the vtables of all the types that we have
    //

    CHECK_AND_RETHROW(tdn_jit_type(tArray));
    CHECK_AND_RETHROW(tdn_jit_type(tString));

    CHECK_AND_RETHROW(tdn_jit_type(tRuntimeAssembly));
    CHECK_AND_RETHROW(tdn_jit_type(tRuntimeModule));
    CHECK_AND_RETHROW(tdn_jit_type(tRuntimeFieldInfo));
    CHECK_AND_RETHROW(tdn_jit_type(tRuntimeMethodBody));
    CHECK_AND_RETHROW(tdn_jit_type(tRuntimeMethodInfo));
    CHECK_AND_RETHROW(tdn_jit_type(tRuntimeConstructorInfo));
    CHECK_AND_RETHROW(tdn_jit_type(tRuntimeLocalVariableInfo));
    CHECK_AND_RETHROW(tdn_jit_type(tRuntimeTypeInfo));
    CHECK_AND_RETHROW(tdn_jit_type(tParameterInfo));
    CHECK_AND_RETHROW(tdn_jit_type(tRuntimeExceptionHandlingClause));

cleanup:
    return err;
}

static tdn_err_t load_assembly(dotnet_file_t* file, RuntimeAssembly* out_assembly);

static tdn_err_t assembly_load_assembly_refs(RuntimeAssembly assembly) {
    tdn_err_t err = TDN_NO_ERROR;
    tdn_file_t current_file = NULL;

    // and now create it
    assembly->AssemblyRefs = (RuntimeAssembly_Array)GC_NEW_ARRAY(RuntimeAssembly, assembly->Metadata->assembly_refs_count);
    for (int i = 0; i < assembly->Metadata->assembly_refs_count; i++) {
        metadata_assembly_ref_t* assembly_ref = &assembly->Metadata->assembly_refs[i];

        const char* name = assembly_ref->name;

        // TODO: how to handle this correctly
        if (strcmp(name, "System.Runtime") == 0) {
            name = "System.Private.CoreLib";
        }

        // get from the hashmap of known assemblies
        // TODO: if not found call a callback to find and load the assembly
        int idx = shgeti(m_loaded_assemblies, name);
        CHECK(idx >= 0, "Failed to get assembly `%s` - not found", name);

        // TODO: maybe this should have an array of major versions we know about
        RuntimeAssembly new_assembly = m_loaded_assemblies[i].value;
        CHECK(new_assembly != NULL, "Failed to get assembly `%s` - recursive dependency", name);

        // TODO: validate the minor version

        assembly->AssemblyRefs->Elements[i] = new_assembly;
    }

cleanup:
    if (current_file != NULL) {
        tdn_host_close_file(current_file);
    }

    return err;
}

static tdn_err_t assembly_load_type_refs(RuntimeAssembly assembly) {
    tdn_err_t err = TDN_NO_ERROR;

    assembly->TypeRefs = (RuntimeTypeInfo_Array)GC_NEW_ARRAY(RuntimeTypeInfo, assembly->Metadata->type_refs_count);
    for (int i = 0; i < assembly->Metadata->type_refs_count; i++) {
        metadata_type_ref_t* type_ref = &assembly->Metadata->type_refs[i];
        RuntimeTypeInfo wanted_type = NULL;

        // search for the type wherever needed
        token_t resolution_scope = type_ref->resolution_scope;
        switch (resolution_scope.table) {

            // find the type from an assembly
            case METADATA_ASSEMBLY_REF: {
                CHECK(resolution_scope.index != 0 && resolution_scope.index <= assembly->AssemblyRefs->Length);
                RuntimeAssembly scope = assembly->AssemblyRefs->Elements[resolution_scope.index - 1];

                for (int j = 0; j < scope->TypeDefs->Length; j++) {
                    RuntimeTypeInfo type = scope->TypeDefs->Elements[j];
                    if (
                        (
                            (type->Namespace == NULL && type_ref->type_namespace[0] == '\0') ||
                            tdn_compare_string_to_cstr(type->Namespace, type_ref->type_namespace)
                        ) &&
                        tdn_compare_string_to_cstr(type->Name, type_ref->type_name)
                    ) {
                        wanted_type = type;
                        break;
                    }
                }
            } break;

            // find type from another type
            case METADATA_TYPE_REF: {
                // get the parent type
                CHECK(resolution_scope.index <= i);
                RuntimeTypeInfo type = assembly->TypeRefs->Elements[resolution_scope.index - 1];

                // try to search under it
                RuntimeTypeInfo nested = type->DeclaredNestedTypes;
                while (nested != NULL) {
                    if (
                        (
                            (nested->Namespace == NULL && type_ref->type_namespace[0] == '\0') ||
                            tdn_compare_string_to_cstr(nested->Namespace, type_ref->type_namespace)
                        ) &&
                        tdn_compare_string_to_cstr(nested->Name, type_ref->type_name)
                    ) {
                        wanted_type = nested;
                        break;
                    }
                    nested = nested->NextNestedType;
                }
            } break;

            default:
                CHECK_FAIL("%02x (%s.%s)", resolution_scope.table, type_ref->type_namespace, type_ref->type_name);
        }

        CHECK(wanted_type != NULL, "Couldn't resolve type %s.%s!", type_ref->type_namespace, type_ref->type_name);
        assembly->TypeRefs->Elements[i] = wanted_type;
    }

cleanup:
    return err;
}

static tdn_err_t assembly_load_methods(RuntimeAssembly assembly) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeModule module = assembly->Module;

    assembly->MethodDefs = (RuntimeMethodBase_Array)GC_NEW_ARRAY(MethodBase, assembly->Metadata->method_defs_count);
    for (int i = 0; i < assembly->Metadata->method_defs_count; i++) {
        metadata_method_def_t* method_def = &assembly->Metadata->method_defs[i];
        MethodAttributes attributes = { .Attributes = method_def->flags };
        MethodImplAttributes impl_attributes = { .Attributes = method_def->impl_flags };
        bool is_ctor = false;

        // TODO: maybe parse the signature in here anyways, we would need to push a queue frame
        //       before hand so we won't try to init stuff out of order

        // make sure the entire entry is valid, not including checks
        // that will be done at a later stage
        CHECK(!(attributes.Static && attributes.Final));
        CHECK(!(attributes.Static && attributes.Virtual));
        CHECK(!(attributes.Static && attributes.VtableNewSlot));
        CHECK(!(attributes.Final && attributes.Abstract));
        CHECK(!(attributes.Abstract && attributes.PinvokeImpl));
        if (attributes.Abstract) CHECK(attributes.Virtual);
        if (attributes.RTSpecialName) CHECK(attributes.SpecialName);
        if (attributes.Final || attributes.VtableNewSlot || attributes.Strict) CHECK(attributes.Virtual);
        if (attributes.PinvokeImpl) CHECK(!attributes.Virtual);
        if (!attributes.Abstract) CHECK(method_def->rva != 0 || attributes.PinvokeImpl || impl_attributes.CodeType == TDN_METHOD_IMPL_CODE_TYPE_RUNTIME);
        if (method_def->rva == 0) CHECK(attributes.Abstract || impl_attributes.CodeType == TDN_METHOD_IMPL_CODE_TYPE_RUNTIME || attributes.PinvokeImpl);
        if (method_def->rva != 0) CHECK(!attributes.Abstract && (impl_attributes.CodeType == TDN_METHOD_IMPL_CODE_TYPE_IL || impl_attributes.CodeType == TDN_METHOD_IMPL_CODE_TYPE_RUNTIME));
        if (attributes.PinvokeImpl) CHECK(method_def->rva == 0);
        if (attributes.RTSpecialName) {
            CHECK(strcmp(method_def->name, ".ctor") == 0 || strcmp(method_def->name, ".cctor") == 0);
            is_ctor = true;
        } else {
            CHECK(strcmp(method_def->name, ".ctor") != 0 && strcmp(method_def->name, ".cctor") != 0);
        }
        if (strcmp(method_def->name, ".ctor") == 0) {
            CHECK(!attributes.Static);
            CHECK(!attributes.Abstract);
            CHECK(!attributes.Virtual);
        } else if (strcmp(method_def->name, ".cctor") == 0) {
            CHECK(attributes.Static);
            CHECK(!attributes.Virtual);
            CHECK(!attributes.Abstract);
        }

        // now that we are sure this looks correct, continue and setup the
        RuntimeMethodBase base = NULL;
        if (is_ctor) {
            base = (RuntimeMethodBase)GC_NEW(RuntimeConstructorInfo);
        } else {
            base = (RuntimeMethodBase)GC_NEW(RuntimeMethodInfo);
        }
        base->MetadataToken = ((token_t){ .table = METADATA_METHOD_DEF, .index = i + 1 }).token;
        base->Attributes = attributes;
        base->MethodImplFlags = impl_attributes;
        base->Module = module;
        base->VTableOffset = VTABLE_INVALID;
        CHECK_AND_RETHROW(tdn_create_string_from_cstr(method_def->name, &base->Name));
        CHECK(base->Name != NULL);

        // save it
        assembly->MethodDefs->Elements[i] = base;
    }

cleanup:
    return err;
}

static tdn_err_t assembly_load_fields(RuntimeAssembly assembly) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeModule module = assembly->Module;

    assembly->Fields = GC_NEW_ARRAY(RuntimeFieldInfo, assembly->Metadata->fields_count);
    for (int i = 0; i < assembly->Metadata->fields_count; i++) {
        metadata_field_t* field = &assembly->Metadata->fields[i];
        FieldAttributes attributes = { .Attributes = field->flags };

        // top level validations
        CHECK(attributes.Literal + attributes.InitOnly <= 1);
        if (attributes.Literal) CHECK(attributes.Static);
        if (attributes.RTSpecialName) CHECK(attributes.SpecialName);

        // create and save the type
        RuntimeFieldInfo field_info = GC_NEW(RuntimeFieldInfo);
        CHECK_AND_RETHROW(tdn_create_string_from_cstr(field->name, &field_info->Name));
        CHECK(field_info->Name != NULL);
        field_info->Attributes = attributes;
        field_info->Module = module;
        field_info->MetadataToken = ((token_t){ .table = METADATA_FIELD, .index = i + 1 }).token;
        assembly->Fields->Elements[i] = field_info;
    }

cleanup:
    return err;
}

static bool is_module_type(RuntimeTypeInfo type) {
    return tdn_compare_string_to_cstr(type->Name, "<Module>") && (type->Namespace == NULL || type->Namespace->Length == 0);
}

static tdn_err_t connect_members_to_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;
    token_t token = { .token = type->MetadataToken };
    RuntimeAssembly assembly = type->Module->Assembly;
    metadata_type_def_t* type_def = &assembly->Metadata->type_defs[token.index - 1];
    bool is_module = is_module_type(type);

    // fill in the base type first, should only do so if its not System.Object, not an
    // interface, and not the <Module> class
    if (type != tObject && !type->Attributes.Interface && !is_module) {
        // resolve it
        CHECK_AND_RETHROW(tdn_assembly_lookup_type(
                assembly, type_def->extends.token, type->GenericArguments, NULL, &type->BaseType));

        CHECK(!type->BaseType->Attributes.Sealed);
    } else {
        CHECK(type_def->extends.index == 0);
    }

    // make sure the array is built correctly
    if (token.index != 1) {
        CHECK(type_def[-1].field_list.index <= type_def->field_list.index);
        CHECK(type_def[-1].method_list.index <= type_def->method_list.index);
    }

    // get the fields and methods count for later access
    size_t fields_count = (token.index == assembly->Metadata->type_defs_count ?
                           assembly->Metadata->fields_count :
                           type_def[1].field_list.index - 1) - (type_def->field_list.index - 1);
    size_t methods_count = (token.index == assembly->Metadata->type_defs_count ?
                            assembly->Metadata->method_defs_count :
                            type_def[1].method_list.index - 1) - (type_def->method_list.index - 1);

    // validate we have valid lists if we have a non-zero methods count
    if (methods_count != 0) {
        CHECK(type_def->method_list.index != 0);
        CHECK(type_def->method_list.index - 1 + methods_count <= assembly->Metadata->method_defs_count);
    }

    if (fields_count != 0) {
        CHECK(type_def->field_list.index != 0);
        CHECK(type_def->field_list.index - 1 + fields_count <= assembly->Metadata->fields_count);
    }

    // validations
    if (type == tValueType) CHECK(type->BaseType == tObject);
    if (type->Attributes.Interface) {
        CHECK(type->Attributes.Abstract);
        CHECK(!type->Attributes.Sealed);
    }

    if (type->BaseType == tEnum) {
        CHECK(type->Attributes.Sealed);
        CHECK(methods_count == 0);
    } else if (type->BaseType == tValueType && type != tEnum) {
        CHECK(type->Attributes.Sealed);
    }

    int variance = 0;
    if (tdn_is_generic_type_definition(type)) {
        variance = type->GenericParameterAttributes.Variance;
        if (
            variance == TDN_GENERIC_PARAM_VARIANCE_COVARIANT ||
            variance == TDN_GENERIC_PARAM_VARIANCE_CONTRAVARIANT
        ) {
            // TODO: also delegate types
            CHECK(type->Attributes.Interface);
        }
    }

    // initialize all the fields, we just need the stack size from them for now
    type->DeclaredFields = GC_NEW_ARRAY(RuntimeFieldInfo, fields_count);
    for (int i = 0; i < fields_count; i++) {
        metadata_field_t* field = &assembly->Metadata->fields[type_def->field_list.index - 1 + i];
        RuntimeFieldInfo fieldInfo = assembly->Fields->Elements[type_def->field_list.index - 1 + i];

        fieldInfo->DeclaringType = type;
        CHECK_AND_RETHROW(sig_parse_field(field->signature, fieldInfo));

        // validations
        if (type->Attributes.Interface) CHECK(fieldInfo->Attributes.Static);
        if (is_module) {
            CHECK(fieldInfo->Attributes.Static);
            CHECK(
                fieldInfo->Attributes.FieldAccess == TDN_FIELD_ACCESS_PRIVATE_SCOPE ||
                fieldInfo->Attributes.FieldAccess == TDN_FIELD_ACCESS_PRIVATE ||
                fieldInfo->Attributes.FieldAccess == TDN_FIELD_ACCESS_PUBLIC
            );
        }

        // check specifics for enums
        if (type->BaseType == tEnum) {
            if (fieldInfo->Attributes.RTSpecialName) {
                CHECK(strcmp(field->name, "value__") == 0);

                CHECK(type->EnumUnderlyingType == NULL);
                type->EnumUnderlyingType = fieldInfo->FieldType;
                CHECK( // TODO: is nint and nuint valid for enum?
                    fieldInfo->FieldType == tSByte ||
                    fieldInfo->FieldType == tInt16 ||
                    fieldInfo->FieldType == tInt32 ||
                    fieldInfo->FieldType == tInt64 ||
                    fieldInfo->FieldType == tByte ||
                    fieldInfo->FieldType == tUInt16 ||
                    fieldInfo->FieldType == tUInt32 ||
                    fieldInfo->FieldType == tUInt64
                );
            } else {
                CHECK(fieldInfo->Attributes.Static);
                CHECK(fieldInfo->Attributes.Literal);
                CHECK(fieldInfo->FieldType == type);
            }
        }

        type->DeclaredFields->Elements[i] = fieldInfo;
    }

    if (type->BaseType == tEnum) {
        CHECK(type->EnumUnderlyingType != NULL);
    }

    //
    // setup the methods
    //

    // count the ctors, we already verified the attributes properly
    int ctors = 0;
    int methods = 0;
    for (int i = 0; i < methods_count; i++) {
        metadata_method_def_t* method_def = &assembly->Metadata->method_defs[type_def->method_list.index - 1 + i];
        MethodAttributes attributes = { .Attributes = method_def->flags };
        if (attributes.RTSpecialName) {
            ctors++;
        } else {
            methods++;
        }
    }

    // now we can allocate and init all of them
    type->DeclaredConstructors = GC_NEW_ARRAY(RuntimeConstructorInfo, ctors);
    type->DeclaredMethods = GC_NEW_ARRAY(RuntimeMethodInfo, methods);
    ctors = 0;
    methods = 0;
    bool found_static_ctor = false;
    for (int i = 0; i < methods_count; i++) {
        int idx = type_def->method_list.index + i;
        metadata_method_def_t* method_def = &assembly->Metadata->method_defs[idx - 1];
        RuntimeMethodBase base = assembly->MethodDefs->Elements[idx - 1];
        MethodAttributes attributes = { .Attributes = method_def->flags };

        // get the correct version
        if (attributes.RTSpecialName) {
            type->DeclaredConstructors->Elements[ctors++] = (RuntimeConstructorInfo)base;
        } else {
            type->DeclaredMethods->Elements[methods++] = (RuntimeMethodInfo)base;
        }

        // validations
        if (base->Attributes.Abstract) CHECK(type->Attributes.Abstract);
        if (type->Attributes.Interface) CHECK(type->Attributes.Abstract); // TODO: can an interface have a static method?
        if (is_module) {
            CHECK(base->Attributes.Static);
            CHECK(!base->Attributes.Abstract);
            CHECK(!base->Attributes.Virtual);
            CHECK(
                base->Attributes.MemberAccess == TDN_METHOD_ACCESS_PRIVATE_SCOPE ||
                base->Attributes.MemberAccess == TDN_METHOD_ACCESS_PRIVATE ||
                base->Attributes.MemberAccess == TDN_METHOD_ACCESS_PUBLIC
            );
        }

        // setup most of the type
        base->DeclaringType = type;

        // parse the body
        if (method_def->rva != 0) {
            CHECK_AND_RETHROW(tdn_parser_method_body(assembly, method_def, base));
        }

        // and finally get the signature
        method_signature_t signature = {};
        CHECK_AND_RETHROW(sig_parse_method_def(
                method_def->signature, assembly,
                type->GenericArguments, base->GenericArguments,
                false,
                &signature));
        base->Parameters = signature.parameters;
        base->ReturnParameter = signature.return_parameter;

        // get parameter information from the params table
        size_t params_count = (idx == assembly->Metadata->method_defs_count ?
                               assembly->Metadata->params_count :
                               method_def[1].param_list.index - 1) - (method_def->param_list.index - 1);
        if (params_count != 0) {
            CHECK(method_def->param_list.index != 0);
            CHECK(method_def->param_list.index - 1 + params_count <= assembly->Metadata->params_count);
        }
        CHECK(params_count <= base->Parameters->Length + 1); // TODO: shouldn't this be equals??
        for (int pi = 0; pi < params_count; pi++) {
            metadata_param_t* param = &assembly->Metadata->params[method_def->param_list.index - 1 + pi];
            CHECK(param->sequence < base->Parameters->Length + 1);
            ParameterInfo info = param->sequence == 0 ? base->ReturnParameter : base->Parameters->Elements[param->sequence - 1];
            info->Attributes = (ParameterAttributes){ .Attributes = param->flags };
            CHECK_AND_RETHROW(tdn_create_string_from_cstr(param->name, &info->Name));

            // also store in the global array
            assembly->Params->Elements[method_def->param_list.index - 1 + pi] = info;
        }

        // validate the signatures of ctor and cctor
        if (base->Attributes.RTSpecialName) {
            if (strcmp(method_def->name, ".ctor") == 0) {
                CHECK(!type->Attributes.Interface);
                CHECK(!is_module);
                CHECK(base->ReturnParameter->ParameterType == tVoid);
            } else if (strcmp(method_def->name, ".cctor") == 0) {
                CHECK(!found_static_ctor);
                found_static_ctor = true;
                CHECK(base->ReturnParameter->ParameterType == tVoid);
                CHECK(base->Parameters->Length == 0);
            } else {
                CHECK_FAIL();
            }
        }

        // variance compliance, this is needed so we can trust transformations of the type later on
        if (variance == TDN_GENERIC_PARAM_VARIANCE_COVARIANT) {
            // should not have the parameter in the inputs
            for (int j = 0; j < base->Parameters->Length; j++) {
                CHECK(!tdn_type_contains_generic_parameters(base->Parameters->Elements[i]->ParameterType));
            }
        } else if (variance == TDN_GENERIC_PARAM_VARIANCE_CONTRAVARIANT) {
            // should not be in parameters to inherited interface or
            CHECK(!tdn_type_contains_generic_parameters(base->ReturnParameter->ParameterType));

            // TODO: check for interface impls
        }
    }

cleanup:
    return err;
}

static tdn_err_t assembly_load_generics(RuntimeAssembly assembly) {
    tdn_err_t err = TDN_NO_ERROR;

    void* last_object = NULL;
    int last_table = 0;
    int count = 0;
    for (int i = 0; i < assembly->Metadata->generic_params_count; i++) {
        metadata_generic_param_t* generic_param = &assembly->Metadata->generic_params[i];
        token_t owner = generic_param->owner;

        // resolve it
        void* current_object = NULL;
        if (owner.table == METADATA_TYPE_DEF) {
            CHECK(owner.index != 0 && owner.index <= assembly->TypeDefs->Length);
            current_object = assembly->TypeDefs->Elements[owner.index - 1];
        } else if (owner.table == METADATA_METHOD_DEF) {
            CHECK(owner.index != 0 && owner.index <= assembly->MethodDefs->Length);
            current_object = assembly->MethodDefs->Elements[owner.index - 1];
        } else {
            CHECK_FAIL();
        }

        // and set it up properly
        if (last_object != NULL && current_object != last_object) {
            RuntimeTypeInfo_Array arr = GC_NEW_ARRAY(RuntimeTypeInfo, count);
            if (last_table == METADATA_TYPE_DEF) {
                CHECK(((RuntimeTypeInfo)last_object)->GenericArguments == NULL);
                ((RuntimeTypeInfo)last_object)->GenericArguments = arr;
                ((RuntimeTypeInfo)last_object)->GenericTypeDefinition = last_object;
            } else {
                // only valid on methods, not on ctors
                CHECK(((RuntimeMethodInfo)last_object)->GenericArguments == NULL);
                CHECK(object_get_vtable(last_object)->Type == tRuntimeMethodInfo);
                ((RuntimeMethodInfo)last_object)->GenericArguments = arr;
                ((RuntimeMethodInfo)last_object)->GenericMethodDefinition = last_object;
            }

            // reset it
            count = 0;
            last_object = NULL;
        }

        // increment it
        count++;
        last_object = current_object;
        last_table = generic_param->owner.table;
    }

    if (count != 0) {
        RuntimeTypeInfo_Array arr = GC_NEW_ARRAY(RuntimeTypeInfo, count);
        if (last_table == METADATA_TYPE_DEF) {
            CHECK(((RuntimeTypeInfo)last_object)->GenericArguments == NULL);
            ((RuntimeTypeInfo)last_object)->GenericArguments = arr;
            ((RuntimeTypeInfo)last_object)->GenericTypeDefinition = last_object;
        } else {
            // only valid on methods, not on ctors
            CHECK(((RuntimeMethodInfo)last_object)->GenericArguments == NULL);
            CHECK(object_get_vtable(last_object)->Type == tRuntimeMethodInfo,
                "%T != %T", object_get_vtable(last_object)->Type, tRuntimeMethodInfo);
            ((RuntimeMethodInfo)last_object)->GenericArguments = arr;
            ((RuntimeMethodInfo)last_object)->GenericMethodDefinition = last_object;
        }
    }

    // now fill it properly
    assembly->GenericParams = GC_NEW_ARRAY(RuntimeTypeInfo, assembly->Metadata->generic_params_count);
    for (int i = 0; i < assembly->Metadata->generic_params_count; i++) {
        metadata_generic_param_t* generic_param = &assembly->Metadata->generic_params[i];
        token_t owner = generic_param->owner;

        RuntimeTypeInfo param = GC_NEW(RuntimeTypeInfo);
        param->GenericParameterPosition = generic_param->number;
        param->GenericParameterAttributes = (GenericParameterAttributes){ .value = generic_param->flags };
        param->IsGenericParameter = 1;
        param->Attributes.Visibility = TDN_TYPE_VISIBILITY_PUBLIC;
        if (param->GenericParameterAttributes.SpecialConstraint == TDN_GENERIC_PARAM_CONSTRAINT_NON_NULLABLE_VALUE_TYPE) {
            param->BaseType = tValueType;
        } else {
            param->BaseType = tObject;
        }
        CHECK_AND_RETHROW(tdn_create_string_from_cstr(generic_param->name, &param->Name));

        // store it
        assembly->GenericParams->Elements[i] = param;

        // resolve it
        if (owner.table == METADATA_TYPE_DEF) {
            CHECK(owner.index != 0 && owner.index <= assembly->TypeDefs->Length);
            param->IsGenericTypeParameter = 1;
            param->DeclaringType = assembly->TypeDefs->Elements[owner.index - 1];
            CHECK(param->DeclaringType->GenericArguments->Length > generic_param->number);
            param->DeclaringType->GenericArguments->Elements[generic_param->number] = param;
        } else if (owner.table == METADATA_METHOD_DEF) {
            CHECK(owner.index != 0 && owner.index <= assembly->MethodDefs->Length);
            param->IsGenericMethodParameter = 1;
            param->DeclaringMethod = assembly->MethodDefs->Elements[owner.index - 1];
            CHECK(param->DeclaringMethod->GenericArguments->Length > generic_param->number);
            param->DeclaringMethod->GenericArguments->Elements[generic_param->number] = param;
        } else {
            CHECK_FAIL();
        }
    }

cleanup:
    return err;
}

static tdn_err_t assembly_load_generic_constraints(RuntimeAssembly assembly) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeTypeInfo last_object = NULL;
    int count = 0;
    for (int i = 0; i < assembly->Metadata->generic_param_constraints_count; i++) {
        metadata_generic_param_constraint_t* generic_param = &assembly->Metadata->generic_param_constraints[i];
        token_t owner = generic_param->owner;
        CHECK(owner.table == METADATA_GENERIC_PARAM);
        CHECK(owner.index != 0 && owner.index <= assembly->GenericParams->Length);
        RuntimeTypeInfo current_object = assembly->GenericParams->Elements[owner.index - 1];

        // and set it up properly
        if (last_object != NULL && current_object != last_object) {
            CHECK(last_object->GenericParameterConstraints == NULL);
            last_object->GenericParameterConstraints = GC_NEW_ARRAY(RuntimeTypeInfo, count);
            count = 0;
            last_object = NULL;
        }

        // increment it
        count++;
        last_object = current_object;
    }

    if (count != 0) {
        CHECK(last_object->GenericParameterConstraints == NULL);
        last_object->GenericParameterConstraints = GC_NEW_ARRAY(RuntimeTypeInfo, count);
    }

    // now fill it properly
    int offset = 0;
    count = 0;
    for (int i = 0; i < assembly->Metadata->generic_param_constraints_count; i++) {
        metadata_generic_param_constraint_t* generic_param = &assembly->Metadata->generic_param_constraints[i];
        RuntimeTypeInfo current_object = assembly->GenericParams->Elements[generic_param->owner.index - 1];

        // move to the next object if needed, updating the offset from the start we are at
        if (last_object != NULL && current_object != last_object) {
            offset += count;
            count = 0;
            last_object = current_object;
        }

        // get the constraint
        RuntimeTypeInfo constraint = NULL;
        CHECK_AND_RETHROW(tdn_assembly_lookup_type(assembly, generic_param->constraint.token, current_object->GenericArguments, NULL, &constraint));

        // store it
        current_object->GenericParameterConstraints->Elements[i - offset] = constraint;

        // and we are done
        count++;
        last_object = current_object;
    }

cleanup:
    return err;
}

static tdn_err_t assembly_connect_misc(RuntimeAssembly assembly) {
    tdn_err_t err = TDN_NO_ERROR;

    // connect nested classes
    for (int i = 0; i < assembly->Metadata->nested_classes_count; i++) {
        metadata_nested_class_t* nest = &assembly->Metadata->nested_classes[i];
        RuntimeTypeInfo nested, enclosing;
        CHECK_AND_RETHROW(tdn_assembly_lookup_type(assembly, nest->enclosing_class.token, NULL, NULL, &enclosing));
        CHECK_AND_RETHROW(tdn_assembly_lookup_type(assembly, nest->nested_class.token, NULL, NULL, &nested));
        nested->DeclaringType = enclosing;

        // append to the singly linked list of nested types
        nested->NextNestedType = enclosing->DeclaredNestedTypes;
        enclosing->DeclaredNestedTypes = nested;
    }

    // connect class layout
    for (int i = 0; i < assembly->Metadata->class_layout_count; i++) {
        metadata_class_layout_t* layout = &assembly->Metadata->class_layout[i];
        CHECK(layout->parent.index != 0 && layout->parent.index <= assembly->TypeDefs->Length);
        RuntimeTypeInfo type = assembly->TypeDefs->Elements[layout->parent.index - 1];

        // validate
        CHECK(!type->Attributes.Interface);
        CHECK(type->Attributes.Layout == TDN_TYPE_LAYOUT_EXPLICIT || type->Attributes.Layout == TDN_TYPE_LAYOUT_SEQUENTIAL);
        if (type->BaseType == tValueType || type->BaseType == tEnum) CHECK(layout->class_size <= SIZE_1MB);
        if (type->Attributes.Layout == TDN_TYPE_LAYOUT_EXPLICIT) CHECK(layout->packing_size == 0);
        else {
            CHECK(
                layout->packing_size == 0 ||
                layout->packing_size == 1 ||
                layout->packing_size == 2 ||
                layout->packing_size == 4 ||
                layout->packing_size == 8 ||
                layout->packing_size == 16 ||
                layout->packing_size == 32 ||
                layout->packing_size == 64 ||
                layout->packing_size == 128
            );
        }

        // save it for later
        type->Packing = layout->packing_size;
        type->HeapSize = layout->class_size;
    }

    // connect jit related custom attributes
    for (int i = 0; i < assembly->Metadata->custom_attribute_count; i++) {
        metadata_custom_attribute_t* attr = &assembly->Metadata->custom_attributes[i];
        RuntimeMethodBase method;
        CHECK_AND_RETHROW(tdn_assembly_lookup_method(assembly, attr->type.token, NULL, NULL, &method));
        CHECK(object_get_vtable(&method->Object)->Type == tRuntimeConstructorInfo);
        RuntimeTypeInfo type = method->DeclaringType;

        if (type == tIsReadOnlyAttribute) {
            switch (attr->parent.table) {
                case METADATA_TYPE_DEF: {
                    RuntimeTypeInfo parent_type;
                    CHECK_AND_RETHROW(tdn_assembly_lookup_type(assembly, attr->parent.token, NULL, NULL, &parent_type));
                    parent_type->IsReadOnly = true;
                } break;

                case METADATA_METHOD_DEF: {
                    RuntimeMethodBase parent_type;
                    CHECK_AND_RETHROW(tdn_assembly_lookup_method(assembly, attr->parent.token, NULL, NULL, &parent_type));
                    parent_type->IsReadOnly = true;
                } break;

                case METADATA_PARAM: {
                    CHECK(attr->parent.index != 0 && attr->parent.index <= assembly->Params->Length);
                    ParameterInfo parent_type = assembly->Params->Elements[attr->parent.index - 1];
                    parent_type->IsReadonly = true;
                } break;

                default:
                    WARN("Found IsReadOnlyAttribute on unknown token %02x", attr->parent.table);
            }
        }
    }

cleanup:
    return err;
}

static tdn_err_t load_assembly(dotnet_file_t* file, RuntimeAssembly* out_assembly) {
    tdn_err_t err = TDN_NO_ERROR;
    bool pushed_type_queue = false;
    RuntimeAssembly assembly = NULL;

    // now we can actually load up the PE and dotnet metadata
    CHECK_AND_RETHROW(pe_load_image(&file->file));
    CHECK_AND_RETHROW(dotnet_load_file(file));

    // add the assembly to the lookup now
    if (file->assemblies_count != 0) {
        CHECK(file->assemblies_count == 1);

        if (m_loaded_assemblies == NULL) {
            sh_new_strdup(m_loaded_assemblies);
        }

        TRACE("Loading assembly `%s`", file->assemblies[0].name);
        shput(m_loaded_assemblies, file->assemblies[0].name, NULL);
    }


    // if we are loading the main assembly then bootstrap now
    if (mCoreAssembly == NULL) {
        CHECK_AND_RETHROW(corelib_bootstrap());
    }

    // now we need to create the assembly, if we are at boostrap we need
    // to do something a bit more special
    assembly = GC_NEW(RuntimeAssembly);
    assembly->Metadata = file;

    // special case for core assembly
    if (mCoreAssembly == NULL) {
        assembly->AllowUnsafe = 1;
        assembly->AllowExternalExports = 1;
    }

    // setup the basic type
    RuntimeModule module = GC_NEW(RuntimeModule);
    module->Assembly = assembly;
    assembly->Module = module;

    // must have only one module
    CHECK(file->modules_count == 1);

    // start
    if (mCoreAssembly == NULL) {
        CHECK_AND_RETHROW(corelib_bootstrap_types(assembly));
    } else {
        // this is the normal initialization path
        assembly->TypeDefs = GC_NEW_ARRAY(RuntimeTypeInfo, file->type_defs_count);
    }

    //
    // top level setup of types
    //
    for (int i = 0; i < assembly->TypeDefs->Length; i++) {
        metadata_type_def_t* type_def = &assembly->Metadata->type_defs[i];
        RuntimeTypeInfo type = NULL;
        if (assembly->TypeDefs->Elements[i] != NULL) {
            CHECK(mCoreAssembly == NULL);
            type = assembly->TypeDefs->Elements[i];
        } else {
            type = GC_NEW(RuntimeTypeInfo);
            assembly->TypeDefs->Elements[i] = type;
        }

        type->MetadataToken = ((token_t){ .table = METADATA_TYPE_DEF, .index = i + 1 }).token;
        type->Module = module;
        CHECK_AND_RETHROW(tdn_create_string_from_cstr(type_def->type_name, &type->Name));
        CHECK_AND_RETHROW(tdn_create_string_from_cstr(type_def->type_namespace, &type->Namespace));
        CHECK(type->Name != NULL);
        type->Attributes.Value = (int)type_def->flags;
    }

    // Load all the external references
    CHECK_AND_RETHROW(assembly_load_assembly_refs(assembly));
    CHECK_AND_RETHROW(assembly_load_type_refs(assembly));

    // load all the methods and fields
    CHECK_AND_RETHROW(assembly_load_methods(assembly));
    CHECK_AND_RETHROW(assembly_load_fields(assembly));

    // load all the generics type information
    CHECK_AND_RETHROW(assembly_load_generics(assembly));
    CHECK_AND_RETHROW(assembly_load_generic_constraints(assembly));

    assembly->Params = GC_NEW_ARRAY(ParameterInfo, assembly->Metadata->params_count);

    push_type_queue();
    pushed_type_queue = true;

    // and now connect the types with the members
    for (int i = 0; i < assembly->TypeDefs->Length; i++) {
        RuntimeTypeInfo type = assembly->TypeDefs->Elements[i];
        CHECK_AND_RETHROW(connect_members_to_type(type));
    }

    pushed_type_queue = false;
    CHECK_AND_RETHROW(drain_type_queue());

    // calculate the size of all the basic types
    for (int i = 0; i < assembly->TypeDefs->Length; i++) {
        RuntimeTypeInfo type = assembly->TypeDefs->Elements[i];
        CHECK_AND_RETHROW(tdn_type_init(type));
    }

    // connect all the misc classes
    CHECK_AND_RETHROW(assembly_connect_misc(assembly));

    // finish up with bootstrapping if this is the corelib
    if (mCoreAssembly == NULL) {
        // jit all the types required
        // for the runtime to work
        CHECK_AND_RETHROW(corelib_jit_types(assembly));

        mCoreAssembly = assembly;
    }

    // resolve the entry point
    if (file->entry_point_token != 0) {
        CHECK_AND_RETHROW(tdn_assembly_lookup_method(assembly, file->entry_point_token, NULL, NULL, &assembly->EntryPoint));
    }

    // commit it now
    if (file->assemblies_count != 0) {
        shput(m_loaded_assemblies, file->assemblies[0].name, assembly);
    }

    // we are success
    if (out_assembly != NULL) {
        *out_assembly = assembly;
    }

cleanup:
    if (pushed_type_queue) {
        pop_type_queue();
    }

    if (IS_ERROR(err) && assembly != NULL) {
        // we don't own the metadata in
        // the case we failed so remove it
        assembly->Metadata = NULL;
    }

    return err;
}

tdn_err_t tdn_load_assembly_from_memory(const void* buffer, size_t buffer_size, RuntimeAssembly* out_assembly) {
    tdn_err_t err = TDN_NO_ERROR;
    void* tmp_buffer = NULL;
    memory_file_handle_t* handle = NULL;
    dotnet_file_t* dotnet = NULL;

    // Setup the file handle and the dotnet file so we can parse them
    // we will also copy the buffer to our own memory
    // TODO: we might need to make this managed or something
    handle = tdn_host_mallocz(sizeof(memory_file_handle_t));
    CHECK_ERROR(handle != NULL, TDN_ERROR_OUT_OF_MEMORY);
    handle->buffer_size = buffer_size;
    tmp_buffer = tdn_host_mallocz(buffer_size);
    CHECK_ERROR(tmp_buffer != NULL, TDN_ERROR_OUT_OF_MEMORY);
    memcpy(tmp_buffer, buffer, buffer_size);
    handle->buffer = tmp_buffer;

    // setup the dotnet file itself
    dotnet = tdn_host_mallocz(sizeof(dotnet_file_t));
    CHECK_ERROR(dotnet != NULL, TDN_ERROR_OUT_OF_MEMORY);
    dotnet->file.handle = handle;
    dotnet->file.read_file = memory_file_read;
    dotnet->file.close_handle = memory_file_close;

    // call common code
    CHECK_AND_RETHROW(load_assembly(dotnet, out_assembly));

cleanup:
    // if we got an error free all the
    // native allocations
    if (IS_ERROR(err)) {
        dotnet_free_file(dotnet);
        tdn_host_free(dotnet);
        tdn_host_free(tmp_buffer);
        tdn_host_free(handle);
    }

    return err;
}
