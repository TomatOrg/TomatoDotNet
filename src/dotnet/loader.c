#include <stddef.h>
#include "loader.h"
#include "tinydotnet/except.h"
#include "tinydotnet/types/reflection.h"
#include "dotnet/metadata/pe.h"
#include "util/except.h"
#include "util/string.h"
#include "dotnet/metadata/metadata.h"
#include "dotnet/gc/gc.h"
#include "dotnet/metadata/sig.h"
#include "util/stb_ds.h"
#include "tinydotnet/jit/jit.h"
#include <tinydotnet/types/type.h>

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
} init_type_t;

typedef struct load_type {
    const char* namespace;
    const char* name;
    RuntimeTypeInfo* dest;
} load_type_t;

#define INIT_VALUE_TYPE(namespace, name) \
    { \
        #namespace, \
        #name, \
        sizeof(name), \
        _Alignof(name), \
        sizeof(name), \
        _Alignof(name), \
        &t##name \
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
    INIT_VALUE_TYPE(System, ValueType),
    INIT_VALUE_TYPE(System, Enum),
    INIT_VALUE_TYPE(System, Boolean),
    INIT_VALUE_TYPE(System, Char),
    INIT_VALUE_TYPE(System, SByte),
    INIT_VALUE_TYPE(System, Int16),
    INIT_VALUE_TYPE(System, Int32),
    INIT_VALUE_TYPE(System, Int64),
    INIT_VALUE_TYPE(System, IntPtr),
    INIT_VALUE_TYPE(System, Byte),
    INIT_VALUE_TYPE(System, UInt16),
    INIT_VALUE_TYPE(System, UInt32),
    INIT_VALUE_TYPE(System, UInt64),
    INIT_VALUE_TYPE(System, UIntPtr),
    INIT_VALUE_TYPE(System, Void),
};
static int m_inited_types = 0;

#define LOAD_TYPE(namespace, name) \
    { \
        #namespace, \
        #name, \
        &t##name \
    }
/**
 * Types to load so the runtime can access them
 */
static load_type_t m_load_types[] = {
    LOAD_TYPE(System, Object),
    LOAD_TYPE(System, Array),
    LOAD_TYPE(System, String),
    LOAD_TYPE(System.Reflection, MethodBase),
    LOAD_TYPE(System.Reflection, RuntimeAssembly),
    LOAD_TYPE(System.Reflection, RuntimeModule),
    LOAD_TYPE(System.Reflection, RuntimeFieldInfo),
    LOAD_TYPE(System.Reflection, RuntimeMethodBody),
    LOAD_TYPE(System.Reflection, RuntimeMethodInfo),
    LOAD_TYPE(System.Reflection, RuntimeConstructorInfo),
    LOAD_TYPE(System.Reflection, RuntimeLocalVariableInfo),
    LOAD_TYPE(System.Reflection, RuntimeTypeInfo),
    LOAD_TYPE(System.Reflection, ParameterInfo),
};
static int m_loaded_types = 0;

/**
 * Contains the Core assembly, where the most basic types are stored
 */
static RuntimeAssembly mCoreAssembly = NULL;

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
            RuntimeTypeInfo type = *init_type->dest ?: GC_NEW(RuntimeTypeInfo);
            type->StackSize = init_type->stack_size;
            type->StackAlignment = init_type->stack_alignment;
            type->HeapSize = init_type->heap_size;
            type->HeapAlignment = init_type->heap_alignment;
            type->FillingHeapSize = 1;
            type->EndFillingHeapSize = 1;
            type->FillingStackSize = 1;
            type->EndFillingStackSize = 1;
            type->QueuedTypeInit = 1;
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
            RuntimeTypeInfo type = *load_type->dest ?: GC_NEW(RuntimeTypeInfo);
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

    // value types have the same size as the heap size, while
    // reference types always have the size of a pointer
    if (tdn_type_is_valuetype(type)) {
        CHECK_AND_RETHROW(fill_heap_size(type));
        type->StackSize = type->HeapSize;
        type->StackAlignment = type->HeapAlignment;
    } else {
        type->StackAlignment = _Alignof(Object);
        type->StackSize = sizeof(Object);
    }

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
            count++;
        }
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
        size_t largest_alignment = 0;
        for (int i = 0; i < fields->Length; i++) {
            RuntimeFieldInfo field = fields->Elements[i];
            RuntimeTypeInfo field_type = field->FieldType;
            if (field->Attributes.Static) {
                continue;
            }
            CHECK_AND_RETHROW(fill_stack_size(field_type));
            largest_alignment = MAX(largest_alignment, fields->Elements[i]->FieldType->StackAlignment);
        }

        // align the alignment nicely, and then align the base of our own data
        size_t alignment = MIN(largest_alignment, type->Packing ?: _Alignof(size_t));
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

    // and now that we are done, if this is a value type
    // then also fill the stack size
    if (tdn_type_is_valuetype(type) && !type->FillingStackSize) {
        CHECK_AND_RETHROW(fill_stack_size(type));
    }

    type->EndFillingHeapSize = 1;

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
} coril_method_fat_t;

tdn_err_t tdn_parser_method_body(
    RuntimeAssembly assembly,
    metadata_method_def_t* method_def,
    RuntimeMethodBase methodBase
) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeMethodBody body = GC_NEW(RuntimeMethodBody);

    // get the tiny header for the start
    uint8_t* start = pe_image_address(&assembly->Metadata->file, method_def->rva);
    void* end = pe_image_address(&assembly->Metadata->file, method_def->rva + 1);
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
        end = pe_image_address(&assembly->Metadata->file, method_def->rva + 12);
        CHECK(end != NULL);

        coril_method_fat_t* fat = (coril_method_fat_t*)start;
        CHECK(fat->size >= 3);
        header_size = fat->size * 4;

        // get the main attributes
        body->MaxStackSize = fat->max_stack;
        body->InitLocals = fat->flags & CorILMethod_InitLocals;

        // for now verify no more sections
        CHECK((fat->flags & CorILMethod_MoreSects) == 0);

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

    // get the code end and start
    // TODO: more sections
    uint8_t* code_start = end;
    uint8_t* code_end = pe_image_address(&assembly->Metadata->file, method_def->rva + header_size + code_size);
    CHECK(code_end != NULL);
    CHECK(code_size <= INT32_MAX);

    // copy the code
    body->ILSize = (int)code_size;
    body->IL = code_start;

    // we are done
    methodBase->MethodBody = body;

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

tdn_err_t tdn_size_init(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    if (arrlen(m_type_queues) == 0) {
        // no delay, just calculate it
        CHECK_AND_RETHROW(fill_stack_size(type));
        CHECK_AND_RETHROW(fill_heap_size(type));
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

static tdn_err_t corelib_bootstrap() {
    tdn_err_t err = TDN_NO_ERROR;

    // start by initializing the System.Type type first
    tRuntimeTypeInfo = gc_raw_alloc(sizeof(struct RuntimeTypeInfo));
    CHECK_ERROR(tRuntimeTypeInfo != NULL, TDN_ERROR_OUT_OF_MEMORY);

    // hard-code types we require for proper bootstrap
    tArray = GC_NEW(RuntimeTypeInfo); // for creating a Type[]
    tString = GC_NEW(RuntimeTypeInfo); // for creating a string
    tRuntimeAssembly = GC_NEW(RuntimeTypeInfo); // for creating the main assembly
    tRuntimeModule = GC_NEW(RuntimeTypeInfo); // for creating the main module

    // setup the basic type so GC_NEW_ARRAY can work
    CHECK_AND_RETHROW(tdn_create_string_from_cstr("RuntimeTimeInfo", &tRuntimeTypeInfo->Name));
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
        CHECK_AND_RETHROW(tdn_create_string_from_cstr(method_def->name, &base->Name));

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
        metadata_method_def_t* method_def = &assembly->Metadata->method_defs[type_def->method_list.index - 1 + i];
        RuntimeMethodBase base = assembly->MethodDefs->Elements[type_def->method_list.index - 1 + i];
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
                &signature));
        base->Parameters = signature.parameters;
        base->ReturnParameter = signature.return_parameter;

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
                CHECK(((RuntimeTypeInfo)last_object)->GenericArguments == NULL);
                CHECK(((Object)last_object)->ObjectType == tRuntimeMethodInfo);
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
            CHECK(((RuntimeTypeInfo)last_object)->GenericArguments == NULL);
            CHECK(((Object)last_object)->ObjectType == tRuntimeMethodInfo);
            ((RuntimeMethodInfo)last_object)->GenericArguments = arr;
            ((RuntimeMethodInfo)last_object)->GenericMethodDefinition = last_object;
        }
    }

    // now fill it properly
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

        // resolve it
        if (owner.table == METADATA_TYPE_DEF) {
            CHECK(owner.index != 0 && owner.index <= assembly->TypeDefs->Length);
            param->DeclaringType = assembly->TypeDefs->Elements[owner.index - 1];
            param->DeclaringType->GenericArguments->Elements[generic_param->number] = param;
        } else if (owner.table == METADATA_METHOD_DEF) {
            CHECK(owner.index != 0 && owner.index <= assembly->MethodDefs->Length);
            param->DeclaringMethod = assembly->MethodDefs->Elements[owner.index - 1];
            param->DeclaringMethod->GenericArguments->Elements[generic_param->number] = param;
        } else {
            CHECK_FAIL();
        }
    }

cleanup:
    return err;
}

static tdn_err_t assembly_connect_misc(RuntimeAssembly assembly) {
    tdn_err_t err = TDN_NO_ERROR;

    // connect nested classes
    // TODO: this

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

cleanup:
    return err;
}

static tdn_err_t load_assembly(dotnet_file_t* file, RuntimeAssembly* out_assembly) {
    tdn_err_t err = TDN_NO_ERROR;
    bool pushed_type_queue = false;

    // now we can actually load up the PE and dotnet metadata
    CHECK_AND_RETHROW(pe_load_image(&file->file));
    CHECK_AND_RETHROW(dotnet_load_file(file));

    // if we are loading the main assembly then bootstrap now
    if (mCoreAssembly == NULL) {
        CHECK_AND_RETHROW(corelib_bootstrap());
    }

    // now we need to create the assembly, if we are at boostrap we need
    // to do something a bit more special
    RuntimeAssembly assembly = GC_NEW(RuntimeAssembly);
    assembly->Metadata = file;

    // setup the basic type
    RuntimeModule module = GC_NEW(RuntimeModule);
    module->Assembly = assembly;
    assembly->Module = module;

    // start
    if (mCoreAssembly == NULL) {
        CHECK_AND_RETHROW(corelib_bootstrap_types(assembly));
    } else {
        // this is the normal initialization path
        assembly->TypeDefs = GC_NEW_ARRAY(RuntimeTypeInfo, file->type_defs_count);
        for (int i = 0; i < file->type_defs_count; i++) {
            assembly->TypeDefs->Elements[i] = GC_NEW(RuntimeTypeInfo);
        }
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
        type->Attributes.Value = (int)type_def->flags;
    }

    // load all the methods and fields
    CHECK_AND_RETHROW(assembly_load_methods(assembly));
    CHECK_AND_RETHROW(assembly_load_fields(assembly));

    // load all the generics type information
    CHECK_AND_RETHROW(assembly_load_generics(assembly));

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
        CHECK_AND_RETHROW(tdn_size_init(type));
    }

    // connect all the misc classes
    CHECK_AND_RETHROW(assembly_connect_misc(assembly));

    // finish up with bootstrapping if this is the corelib
    if (mCoreAssembly == NULL) {
        mCoreAssembly = assembly;
    }

    // we are success
    *out_assembly = assembly;

cleanup:
    if (pushed_type_queue) {
        pop_type_queue();
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
    CHECK(handle != NULL);
    handle->buffer_size = buffer_size;
    tmp_buffer = tdn_host_mallocz(buffer_size);
    memcpy(tmp_buffer, buffer, buffer_size);
    handle->buffer = tmp_buffer;

    // setup the dotnet file itself
    dotnet = tdn_host_mallocz(sizeof(dotnet_file_t));
    CHECK(dotnet != NULL);
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
