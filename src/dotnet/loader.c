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
     const void* buffer;
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

//STATIC_ASSERT(offsetof(dotnet_file_t, modules_count) == (offsetof(dotnet_file_t, tables) + METADATA_MODULE * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, type_refs_count) == (offsetof(dotnet_file_t, tables) + METADATA_TYPE_REF * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, type_defs_count) == (offsetof(dotnet_file_t, tables) + METADATA_TYPE_DEF * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, fields_count) == (offsetof(dotnet_file_t, tables) + METADATA_FIELD * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, method_defs_count) == (offsetof(dotnet_file_t, tables) + METADATA_METHOD_DEF * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, params_count) == (offsetof(dotnet_file_t, tables) + METADATA_PARAM * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, interface_impls_count) == (offsetof(dotnet_file_t, tables) + METADATA_INTERFACE_IMPL * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, member_refs_count) == (offsetof(dotnet_file_t, tables) + METADATA_MEMBER_REF * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, constants_count) == (offsetof(dotnet_file_t, tables) + METADATA_CONSTANT * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, custom_attribute_count) == (offsetof(dotnet_file_t, tables) + METADATA_CUSTOM_ATTRIBUTE * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, decl_security_count) == (offsetof(dotnet_file_t, tables) + METADATA_DECL_SECURITY * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, class_layout_count) == (offsetof(dotnet_file_t, tables) + METADATA_CLASS_LAYOUT * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, field_layout_count) == (offsetof(dotnet_file_t, tables) + METADATA_FIELD_LAYOUT * sizeof(metadata_table_info_t)));
//STATIC_ASSERT(offsetof(dotnet_file_t, stand_alone_sigs_count) == (offsetof(dotnet_file_t, tables) + METADATA_STAND_ALONE_SIG * sizeof(metadata_table_info_t)));

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct init_type {
    const char* namespace;
    const char* name;
    size_t stack_size;
    size_t stack_alignment;
    size_t heap_size;
    size_t heap_alignment;
    System_Type* dest;
} init_type_t;

typedef struct load_type {
    const char* namespace;
    const char* name;
    System_Type* dest;
} load_type_t;

#define INIT_VALUE_TYPE(namespace, name) \
    { \
        #namespace, \
        #name, \
        sizeof(namespace##_##name), \
        _Alignof(namespace##_##name), \
        sizeof(namespace##_##name), \
        _Alignof(namespace##_##name), \
        &t##namespace##_##name \
    }

#define INIT_HEAP_TYPE(namespace, name) \
    { \
        #namespace, \
        #name, \
        sizeof(namespace##_##name), \
        _Alignof(namespace##_##name), \
        sizeof(struct namespace##_##name), \
        _Alignof(struct namespace##_##name), \
        &t##namespace##_##name \
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
};
static int m_inited_types = 0;

#define LOAD_TYPE(namespace_str, namespace, name) \
    { \
        #namespace_str, \
        #name, \
        &t##namespace##_##name \
    }
/**
 * Types to load so the runtime can access them
 */
static load_type_t m_load_types[] = {
    LOAD_TYPE(System, System, Object),
    LOAD_TYPE(System, System, Array),
    LOAD_TYPE(System, System, String),
    LOAD_TYPE(System, System, Void),
    LOAD_TYPE(System, System, Type),
    LOAD_TYPE(System.Reflection, System_Reflection, Assembly),
    LOAD_TYPE(System.Reflection, System_Reflection, Module),
    LOAD_TYPE(System.Reflection, System_Reflection, FieldInfo),
    LOAD_TYPE(System.Reflection, System_Reflection, MethodBody),
    LOAD_TYPE(System.Reflection, System_Reflection, MethodInfo),
    LOAD_TYPE(System.Reflection, System_Reflection, ParameterInfo),
    LOAD_TYPE(System.Reflection, System_Reflection, MemberInfo),
    LOAD_TYPE(TinyDotNet.Reflection, TinyDotNet_Reflection, MemberReference),
    LOAD_TYPE(TinyDotNet.Reflection, TinyDotNet_Reflection, TypeSpecification),
    { "", "<Module>", &tModule }, // special case
};
static int m_loaded_types = 0;

static System_Reflection_Assembly mCoreAssembly = NULL;

static tdn_err_t corelib_create_type(dotnet_file_t* file, metadata_type_def_t* type_def, System_Type* out_type) {
    tdn_err_t err = TDN_NO_ERROR;

    // we need to now fill the type array, while also initializing
    // all of the other needed types
    for (int i = 0; i < ARRAY_LENGTH(m_init_types); i++) {
        init_type_t* init_type = &m_init_types[i];
        if (
            strcmp(init_type->namespace, type_def->type_namespace) == 0 &&
            strcmp(init_type->name, type_def->type_name) == 0
        ) {
            System_Type type = *init_type->dest ?: GC_NEW(System_Type);
            type->StackSize = init_type->stack_size;
            type->StackAlignment = init_type->stack_alignment;
            type->HeapSize = init_type->heap_size;
            type->HeapAlignment = init_type->heap_alignment;
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
            System_Type type = *load_type->dest ?: GC_NEW(System_Type);
            *load_type->dest = type;
            *out_type = type;
            m_loaded_types++;
            goto cleanup;
        }
    }

    *out_type = GC_NEW(System_Type);

cleanup:
    return err;
}

static void sort_fields_by_alignment(System_Reflection_FieldInfo_Array arr, int low, int high) {
    if (low > high || low < 0) {
        return;
    }

    // perform the partition
    size_t pivot = arr->Elements[high]->FieldType->StackAlignment;
    int pi = low - 1;
    for (int j = low; j <= high - 1; j++) {
        if (arr->Elements[j]->FieldType->StackAlignment <= pivot) {
            pi++;
            System_Reflection_FieldInfo temp = arr->Elements[pi];
            arr->Elements[pi] = arr->Elements[j];
            arr->Elements[j] = temp;
        }
    }

    pi++;
    System_Reflection_FieldInfo temp = arr->Elements[pi];
    arr->Elements[pi] = arr->Elements[high];
    arr->Elements[high] = temp;

    // recursively sort it
    sort_fields_by_alignment(arr, low, pi - 1);
    sort_fields_by_alignment(arr, pi + 1, high);
}

tdn_err_t tdn_fill_size(System_Type type) {
    tdn_err_t err = TDN_NO_ERROR;

    // if we have the heap alignment already
    // we are def done
    if (type->HeapAlignment != 0) {
        goto cleanup;
    }

    // if we started filling the size, but we have the stack alignment
    // we are done as well, otherwise we need to start searching for
    // the alignment
    if (type->StartedFillingSize && type->StackAlignment != 0) {
        goto cleanup;
    }

    CHECK(!type->StartedFillingSize);
    type->StartedFillingSize = true;

    // set the initial size, allows for recursion to end quickly for the case of StackAlignment
    if (!tdn_type_is_valuetype(type)) {
        type->StackSize = sizeof(System_Object);
        type->StackAlignment = _Alignof(System_Object);
    }

    if (type->Attributes.Layout == TDN_TYPE_LAYOUT_EXPLICIT) {
        CHECK_FAIL();
        // TODO: add support for explicit layout
    } else {
        // start by calculating the BaseType
        size_t current_size = 0;

        // we will treat the base type as essentially an invisible field
        if (type->BaseType != NULL) {
            if (type->BaseType->HeapAlignment == 0) {
                CHECK_AND_RETHROW(tdn_fill_size(type->BaseType));
            }
            current_size = type->BaseType->HeapSize;
        }

        // get the fields by the order we want them in the struct,
        // we are going to find the largest alignment on the way, just
        // to figure the alignment that we need to do
        System_Reflection_FieldInfo_Array fields = type->Fields;
        if (type->Attributes.Layout == TDN_TYPE_LAYOUT_AUTO) {
            // in auto-layout we are going to sort by the alignment first
            sort_fields_by_alignment(fields, 0, fields->Length - 1);
        }

        // find the largest field for alignment calculation,
        // also fills their size
        size_t largest_field = 0;
        for (int i = 0; i < fields->Length; i++) {
            System_Reflection_FieldInfo field = fields->Elements[i];
            System_Type field_type = field->FieldType;
            if (field->Attributes.Static) {
                continue;
            }

            if (field_type->StackAlignment == 0) {
                CHECK_AND_RETHROW(tdn_fill_size(field_type));
            }

            largest_field = MAX(largest_field, fields->Elements[i]->FieldType->StackSize);
        }

        // align the alignment nicely, and then align the base of our own data
        size_t alignment = MIN(largest_field, type->Packing ?: _Alignof(size_t));
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
            System_Reflection_FieldInfo field = fields->Elements[i];
            System_Type field_type = field->FieldType;
            if (field->Attributes.Static) {
                continue;
            }

            // align up the size
            size_t field_alignment = MIN(alignment, field_type->StackSize);
            current_size = ALIGN_UP(current_size, field_alignment);

            // fill the field
            CHECK(!field->FilledOffset);
            field->FieldOffset = current_size;
            field->FilledOffset = true;

            // take the size up
            current_size += field_type->StackSize;
        }

        // and align the total struct properly, so it will always
        // have the alignment that we want it to have
        current_size = ALIGN_UP(current_size, alignment);

        // and now fill it in, if the heap size is already filled
        // from the class layout then use it right now
        type->HeapAlignment = alignment;
        if (type->HeapSize != 0) {
            CHECK(type->HeapSize >= current_size);
        } else {
            type->HeapSize = current_size;
        }

        // if its a value type also fill that
        if (tdn_type_is_valuetype(type)) {
            type->StackAlignment = type->HeapAlignment;
            type->StackSize = type->HeapSize;
        }

        // we done!
        type->FinishedFillingSize = true;
    }

cleanup:
    return err;
}

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

static tdn_err_t tdn_parser_method_body(pe_file_t* file, metadata_method_def_t* method_def, System_Reflection_MethodInfo methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    System_Reflection_MethodBody body = GC_NEW(System_Reflection_MethodBody);

    // get the tiny header for the start
    uint8_t* start = pe_image_address(file, method_def->rva);
    void* end = pe_image_address(file, method_def->rva + 1);
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
        end = pe_image_address(file, method_def->rva + 12);
        CHECK(end != NULL);

        coril_method_fat_t* fat = (coril_method_fat_t*)start;
        CHECK(fat->size >= 3);
        header_size = fat->size * 4;

        // get the main attributes
        body->MaxStackSize = fat->max_stack;
        body->InitLocals = fat->flags & CorILMethod_InitLocals;

        CHECK((fat->flags & CorILMethod_MoreSects) == 0);

        code_size = fat->code_size;
    } else {
        CHECK_FAIL();
    }

    // get the code end and start
    // TODO: more sections
    uint8_t* code_start = end;
    uint8_t* code_end = pe_image_address(file, method_def->rva + header_size + code_size);
    CHECK(code_end != NULL);

    // copy the code
    body->IL = GC_NEW_BYTE_ARRAY(code_size);
    memcpy(body->IL->Elements, code_start, code_size);

    // we are done
    methodInfo->MethodBody = body;

cleanup:
    return err;
}

static bool is_valid_enum_underlying_type(System_Type type) {
    return type == tSystem_Byte || type == tSystem_SByte ||
            type == tSystem_UInt16 || type == tSystem_Int16 ||
            type == tSystem_UInt32 || type == tSystem_Int32 ||
            type == tSystem_UInt64 || type == tSystem_Int64 ||
            type == tSystem_UIntPtr || type == tSystem_IntPtr;
}

static tdn_err_t tdn_fill_assembly(dotnet_file_t* file, System_Reflection_Assembly assembly) {
    tdn_err_t err = TDN_NO_ERROR;

    System_Reflection_Module module = GC_NEW(System_Reflection_Module);
    module->Assembly = assembly;

    if (mCoreAssembly == NULL) {
        // make sure we can make a type array
        tSystem_Type->Assembly = assembly;
        CHECK_AND_RETHROW(tdn_create_string_from_cstr("Type", &tSystem_Type->Name));
        CHECK_AND_RETHROW(tdn_create_string_from_cstr("System", &tSystem_Type->Namespace));
    }

    // create the defined types array
    assembly->Module = module;
    assembly->TypeDefs = GC_NEW_ARRAY(System_Type, file->type_defs_count);

    //
    // Fill the defined types array with initial data
    //
    for (int i = 0; i < assembly->TypeDefs->Length; i++) {
        metadata_type_def_t* type_def = &file->type_defs[i];

        // allocate the type
        System_Type type = NULL;
        if (mCoreAssembly == NULL) {
            // special case if loading the first assembly, we need to
            // check if its a predefined one that we already initialized
            CHECK_AND_RETHROW(corelib_create_type(file, type_def, &type));
        } else {
            type = GC_NEW(System_Type);
        }

        // MemberInfo
        type->Assembly = assembly;
        type->Module = module;
        type->MetadataToken = (token_t){ .table = METADATA_TYPE_DEF, .index = i + 1 }.token;

        // Type
        type->Attributes.Value = type_def->flags;

        // System.Type already has a name because it is needed to even create
        // the Type array needed for the rest of the resolving
        if (type != tSystem_Type) {
            CHECK_AND_RETHROW(tdn_create_string_from_cstr(type_def->type_name, &type->Name));
            if (type_def->type_namespace[0] != '\0') {
                CHECK_AND_RETHROW(tdn_create_string_from_cstr(type_def->type_namespace, &type->Namespace));
            }
        } else {
            CHECK(type->Name != NULL && type->Namespace != NULL);
        }

        // store it
        assembly->TypeDefs->Elements[i] = type;
    }

    // make sure we found all the basic types so we can continue with our loading
    if (mCoreAssembly == NULL) {
        CHECK(m_loaded_types == ARRAY_LENGTH(m_load_types));
        CHECK(m_inited_types == ARRAY_LENGTH(m_init_types));
    }

    //
    // Fill the TypeSpec
    //

    for (int i = 0; i < file->type_specs_count; i++) {
        metadata_type_spec_t* type_spec = &file->type_specs[i];

    }

    //
    // setup the nested classes, just resolve it and store the links
    //
    for (int i = 0; i < file->nested_classes_count; i++) {
        metadata_nested_class_t* nested_def = &file->nested_classes[i];
        System_Type enclosing;
        System_Type nested;
        CHECK_AND_RETHROW(tdn_assembly_lookup_type(assembly, nested_def->enclosing_class.token, NULL, NULL, &enclosing));
        CHECK_AND_RETHROW(tdn_assembly_lookup_type(assembly, nested_def->nested_class.token, NULL, NULL, &nested));
        nested->DeclaringType = nested->DeclaringType;
    }

    // TODO: anything else we need to resolve all the base types
    //       once we get to it

    //------------------------------------------------------------------------------------------------------------------

    // allocate the tables for quick lookup
    assembly->MethodDefs = GC_NEW_ARRAY(System_Reflection_MethodInfo, file->method_defs_count);
    assembly->Fields = GC_NEW_ARRAY(System_Reflection_FieldInfo, file->fields_count);

    //
    // finish the lookup of types
    //
    for (int i = 0; i < assembly->TypeDefs->Length; i++) {
        metadata_type_def_t* type_def = &file->type_defs[i];
        System_Type type = assembly->TypeDefs->Elements[i];

        // make sure the array is built correctly
        if (i != 0) {
            CHECK(type_def[-1].field_list.index <= type_def->field_list.index);
            CHECK(type_def[-1].method_list.index <= type_def->method_list.index);
        }

        // get the fields and methods count for later access
        size_t fields_count = ((i + 1) == file->type_defs_count ?
                               file->fields_count :
                               type_def[1].field_list.index - 1) - (type_def->field_list.index - 1);
        size_t methods_count = ((i + 1) == file->type_defs_count ?
                               file->method_defs_count :
                               type_def[1].method_list.index - 1) - (type_def->method_list.index - 1);

        // get the base type, will be verified in a later pass
        if (
            type != tSystem_Object &&
            type != tModule &&
            !type->Attributes.Interface
        ) {
            CHECK_AND_RETHROW(tdn_assembly_lookup_type(assembly, type_def->extends.token, NULL, NULL, &type->BaseType));
        }

        if (fields_count > 0) {
            CHECK(!type->Attributes.Interface);
        }

        // create the arrays nicely
        type->Fields = GC_NEW_ARRAY(System_Reflection_FieldInfo, fields_count);
        type->Methods = GC_NEW_ARRAY(System_Reflection_MethodInfo, methods_count);
    }

    //------------------------------------------------------------------------------------------------------------------

    //
    // Get all the generic information for all the methods and types
    // and fields so we can properly expand them on the next pass
    //

    // start by counting how many entries each has
    void* lastType = NULL;
    int lastTable = 0;
    int count = 0;
    for (int i = 0; i < file->generic_params_count; i++) {
        metadata_generic_param_t* generic_param = &file->generic_params[i];

        // resolve it
        void* currentType = NULL;
        if (generic_param->owner.table == METADATA_TYPE_DEF) {
            CHECK_AND_RETHROW(tdn_assembly_lookup_type(assembly, generic_param->owner.token, NULL, NULL, (void*)&currentType));
        } else if (generic_param->owner.table == METADATA_METHOD_DEF) {
            CHECK_AND_RETHROW(tdn_assembly_lookup_method(assembly, generic_param->owner.token, NULL, NULL, (void*)&currentType));
        } else {
            CHECK_FAIL();
        }

        // we got a new entry to the list
        if (lastType != NULL && currentType != lastType) {
            System_Type_Array arr = GC_NEW_ARRAY(System_Type, count);
            if (lastTable == METADATA_TYPE_DEF) {
                ((System_Type)lastType)->GenericArguments = arr;
                ((System_Type)lastType)->GenericTypeDefinition = lastType;
            } else {
                ((System_Reflection_MethodInfo)lastType)->GenericArguments = arr;
                ((System_Reflection_MethodInfo)lastType)->GenericMethodDefinition = lastType;
            }

            // reset it
            count = 0;
            lastType = NULL;
        }

        // make sure it is in sequence
        CHECK(generic_param->number == count);

        // increment it
        count++;
        lastType = currentType;
        lastTable = generic_param->owner.table;
    }

    // fill in the last one
    if (count != 0) {
        System_Type_Array arr = GC_NEW_ARRAY(System_Type, count);
        if (lastTable == METADATA_TYPE_DEF) {
            ((System_Type)lastType)->GenericArguments = arr;
            ((System_Type)lastType)->GenericTypeDefinition = lastType;
        } else {
            ((System_Reflection_MethodInfo)lastType)->GenericArguments = arr;
            ((System_Reflection_MethodInfo)lastType)->GenericMethodDefinition = lastType;
        }
    }

    // now fill it in properly
    for (int i = 0; i < file->generic_params_count; i++) {
        metadata_generic_param_t* generic_param = &file->generic_params[i];
        System_Type type;
        CHECK_AND_RETHROW(tdn_assembly_lookup_type(assembly, generic_param->owner.token, NULL, NULL, &type));

        // verify it makes sense
        System_Reflection_GenericParameterAttributes attrs = { .value = generic_param->flags };
        if (attrs.Variance != 0) {
            // TODO: delegate
            CHECK(type->Attributes.Interface);
        }

        // create the generic argument type
        // set the name, arguments, base type, size is filled but zero (meaning it does
        // not have a size)
        System_Type genericParam = GC_NEW(System_Type);
        CHECK_AND_RETHROW(tdn_create_string_from_cstr(generic_param->name, &genericParam->Name));
        genericParam->GenericParameterPosition = generic_param->number;
        genericParam->GenericParameterAttributes = attrs;
        genericParam->DeclaringType = type;
        genericParam->BaseType = attrs.SpecialConstraint == TDN_GENERIC_PARAM_CONSTRAINT_NON_NULLABLE_VALUE_TYPE ?
                                    tSystem_ValueType : tSystem_Object;
        genericParam->Attributes.Visibility = TDN_TYPE_VISIBILITY_PUBLIC;
        genericParam->StartedFillingSize = 1;
        genericParam->FinishedFillingSize = 1;

        // store it
        type->GenericArguments->Elements[generic_param->number] = genericParam;
    }

    //------------------------------------------------------------------------------------------------------------------
    //
    // Get misc tables
    //

    // MemberRefs
    assembly->MemberRefs = GC_NEW_ARRAY(TinyDotNet_Reflection_MemberReference, file->member_refs_count);
    for (int i = 0; i < file->member_refs_count; i++) {
        metadata_member_ref_t* member_ref = &file->member_refs[i];
        TinyDotNet_Reflection_MemberReference* ref = &assembly->MemberRefs->Elements[i];
        CHECK_AND_RETHROW(tdn_create_string_from_cstr(member_ref->name, &ref->Name));
        ref->ParentToken = member_ref->class.token;
        ref->Signature = GC_NEW_BYTE_ARRAY(member_ref->signature.size);
        memcpy(ref->Signature->Elements, member_ref->signature.data, member_ref->signature.size);
    }

    // TypeSpecs
    assembly->TypeSpecs = GC_NEW_ARRAY(TinyDotNet_Reflection_TypeSpecification, file->type_specs_count);
    for (int i = 0; i < file->type_specs_count; i++) {
        metadata_type_spec_t* type_spec = &file->type_specs[i];
        TinyDotNet_Reflection_TypeSpecification* spec = &assembly->TypeSpecs->Elements[i];
        spec->Signature = GC_NEW_BYTE_ARRAY(type_spec->signature.size);
        memcpy(spec->Signature->Elements, type_spec->signature.data, type_spec->signature.size);
    }

    //------------------------------------------------------------------------------------------------------------------

    //
    // now fill all the Fields and Methods
    //
    for (int i = 0; i < assembly->TypeDefs->Length; i++) {
        metadata_type_def_t* type_def = &file->type_defs[i];
        System_Type type = assembly->TypeDefs->Elements[i];

        bool is_module = strcmp(type_def->type_name, "<Module>") == 0;
        if (is_module) {
            CHECK(type->Methods->Length == 0);
            CHECK(type->Fields->Length == 0);
        }

        //
        // Base type
        //
        // TODO: better way to detect the <Module> class
        if (type != tSystem_Object && !is_module && !type->Attributes.Interface) {
            CHECK(type->BaseType != NULL);

            if (type == tSystem_ValueType) {
                CHECK(type->BaseType == tSystem_Object);
            }

            if (type->BaseType == tSystem_Enum) {
                CHECK(type->Attributes.Sealed);
                CHECK(type->Methods->Length == 0);
            } else if (type->BaseType == tSystem_ValueType && type != tSystem_Enum) {
                CHECK(type->Attributes.Sealed);
            }

            if (type->Attributes.Interface) {
                CHECK(type->Attributes.Abstract);
                CHECK(!type->Attributes.Sealed);
            }

//            // TODO: enum?
//            CHECK(type->BaseType != tSystem_ValueType);
            CHECK(!type->BaseType->Attributes.Interface);
            CHECK(!type->BaseType->Attributes.Sealed);

            // TODO: detect inheritance loops
        } else {
            CHECK(type->BaseType == NULL);
        }

        //
        // start with fields
        //
        bool found_enum_field = false;
        for (int fi = 0; fi < type->Fields->Length; fi++) {
            int idx = type_def->field_list.index + fi - 1;
            metadata_field_t* field = &file->fields[idx];
            System_Reflection_FieldInfo field_info = GC_NEW(System_Reflection_FieldInfo);
            type->Fields->Elements[fi] = field_info;

            // MemberInfo
            field_info->DeclaringType = type;
            field_info->Module = module;
            CHECK_AND_RETHROW(tdn_create_string_from_cstr(field->name, &field_info->Name));
            field_info->MetadataToken = (token_t){ .table = METADATA_FIELD, .index = idx + 1 }.token;
            assembly->Fields->Elements[idx] = field_info;

            // FieldInfo
            field_info->Attributes.Attributes = field->flags;

            // parse the signature
            CHECK_AND_RETHROW(sig_parse_field(field->signature, field_info));

            //
            // Validate the entries are valid according to the spec
            //

            CHECK(!(field_info->Attributes.Literal && field_info->Attributes.InitOnly));
            if (field_info->Attributes.RTSpecialName) CHECK(field_info->Attributes.SpecialName);
            // TODO: Module scope checks
            // TODO: Duplicate checking

            if (type->Attributes.Interface) {
                CHECK(field_info->Attributes.Static);
            }

            if (type->BaseType == tSystem_Enum) {
                if (field_info->Attributes.Static) {
                    CHECK(field_info->Attributes.Literal);
                    CHECK(field_info->FieldType == type);
                } else {
                    CHECK(!found_enum_field);
                    found_enum_field = true;
                    CHECK(field_info->Attributes.RTSpecialName);
                    CHECK(strcmp(field->name, "value__") == 0);
                    if (type->EnumUnderlyingType == NULL) {
                        // first field we see, just set this is as the type
                        CHECK(is_valid_enum_underlying_type(field_info->FieldType));
                        type->EnumUnderlyingType = field_info->FieldType;
                    } else {
                        CHECK(field_info->FieldType == type->EnumUnderlyingType);
                    }
                }
            }
        }

        //
        // and now the methods
        //
        for (int mi = 0; mi < type->Methods->Length; mi++) {
            int idx = type_def->method_list.index + mi - 1;
            metadata_method_def_t* method_def = &file->method_defs[idx];
            System_Reflection_MethodInfo method_info = GC_NEW(System_Reflection_MethodInfo);
            type->Methods->Elements[mi] = method_info;

            // MemberInfo
            method_info->DeclaringType = type;
            method_info->Module = module;
            CHECK_AND_RETHROW(tdn_create_string_from_cstr(method_def->name, &method_info->Name));
            method_info->MetadataToken = (token_t) { .table = METADATA_METHOD_DEF, .index = idx + 1 }.token;
            assembly->MethodDefs->Elements[idx] = method_info;

            // MethodBase
            method_info->ImplementationFlags.Attributes = method_def->impl_flags;
            method_info->Attributes.Attributes = method_def->flags;

            // MethodBody
            if (method_def->rva != 0) {
                CHECK_AND_RETHROW(tdn_parser_method_body(&file->file, method_def, method_info));
            }

            // parse the signature
            method_signature_t signature = {0};
            CHECK_AND_RETHROW(sig_parse_method_def(method_def->signature, assembly, type->GenericArguments, NULL, &signature));
            if (method_info->GenericArguments == NULL) {
                CHECK(signature.generic_param_count == 0);
                CHECK(signature.generic_param_count == 0);
            } else {
                CHECK(method_info->GenericArguments->Length == signature.generic_param_count);
            }
            method_info->ReturnParameter = signature.return_parameter;
            method_info->Parameters = signature.parameters;

            // now fill all the parameter information that we have in addition
            // to the signature
            size_t fields_count = ((idx + 1) == file->method_defs_count ?
                                   file->params_count :
                                   method_def[1].param_list.index - 1) - (method_def->param_list.index - 1);
            CHECK(fields_count <= method_info->Parameters->Length + 1);

            for (int pi = 0; pi < fields_count; pi++) {
                int pidx = method_def->param_list.index + pi - 1;
                metadata_param_t* param = &file->params[pidx];

                // get the info
                System_Reflection_ParameterInfo parameter_info = NULL;
                if (param->sequence == 0) {
                    parameter_info = method_info->ReturnParameter;
                } else {
                    CHECK(param->sequence - 1 < method_info->Parameters->Length);
                    parameter_info = method_info->Parameters->Elements[param->sequence - 1];
                }

                // fill it nicely
                parameter_info->MetadataToken = (token_t){ .table = METADATA_PARAM, .index = pidx + 1 }.token;
                parameter_info->Attributes.Attributes = param->flags;
                CHECK_AND_RETHROW(tdn_create_string_from_cstr(param->name, &parameter_info->Name));
            }

            //
            // Validate the entry according to the spec
            //

            // TODO: duplicate checking

            CHECK(!(method_info->Attributes.Static && method_info->Attributes.Final));
            CHECK(!(method_info->Attributes.Static && method_info->Attributes.Virtual));
            CHECK(!(method_info->Attributes.Static && method_info->Attributes.VtableNewSlot));
            CHECK(!(method_info->Attributes.Final && method_info->Attributes.Abstract));
            CHECK(!(method_info->Attributes.Abstract && method_info->Attributes.PinvokeImpl));
            CHECK(!(method_info->Attributes.MemberAccess == TDN_METHOD_ACCESS_PRIVATE_SCOPE && method_info->Attributes.SpecialName));
            CHECK(!(method_info->Attributes.MemberAccess == TDN_METHOD_ACCESS_PRIVATE_SCOPE && method_info->Attributes.RTSpecialName));

            if (type == tSystem_ValueType || type->BaseType == tSystem_ValueType || type->BaseType == tSystem_Enum) {
                CHECK(!method_info->ImplementationFlags.Synchronized);
            }

            if (method_info->Attributes.Final || method_info->Attributes.VtableNewSlot) {
                // TODO: strict?
                CHECK(method_info->Attributes.Virtual);
            }

            if (!method_info->Attributes.Abstract) {
                int match = 0;
                if (method_def->rva != 0) match++;
                if (method_info->Attributes.PinvokeImpl) match++;
                if (method_info->ImplementationFlags.CodeType == TDN_METHOD_IMPL_CODE_TYPE_RUNTIME) match++;
                CHECK(match == 1);
            }

            if (method_def->rva == 0) {
                CHECK(
                    method_info->Attributes.Abstract ||
                    method_info->ImplementationFlags.CodeType == TDN_METHOD_IMPL_CODE_TYPE_RUNTIME ||
                    method_info->Attributes.PinvokeImpl
                );
            } else {
                CHECK(!method_info->Attributes.Abstract);
                CHECK(
                    method_info->ImplementationFlags.CodeType == TDN_METHOD_IMPL_CODE_TYPE_NATIVE ||
                    method_info->ImplementationFlags.CodeType == TDN_METHOD_IMPL_CODE_TYPE_IL ||
                    method_info->ImplementationFlags.CodeType == TDN_METHOD_IMPL_CODE_TYPE_RUNTIME
                );
                // TODO: points to the CIL stream
            }

            if (method_info->Attributes.RTSpecialName) {
                if (strcmp(method_def->name, ".ctor") == 0) {
                    CHECK(method_info->ReturnParameter->ParameterType == tSystem_Void);
                    CHECK(!method_info->Attributes.Static);
                    CHECK(!method_info->Attributes.Abstract);
                    CHECK(!method_info->Attributes.Virtual);
                    CHECK(!type->Attributes.Interface);
                } else if (strcmp(method_def->name, ".cctor") == 0) {
                    CHECK(method_info->ReturnParameter->ParameterType == tSystem_Void);
                    CHECK(method_info->Parameters->Length == 0);
                    CHECK(method_info->Attributes.Static);
                    CHECK(!method_info->Attributes.Abstract);
                    CHECK(!method_info->Attributes.Virtual);
                } else {
                    CHECK_FAIL();
                }
            } else {
                CHECK(
                    strcmp(method_def->name, ".ctor") != 0 &&
                    strcmp(method_def->name, ".cctor") != 0
                );
            }

            if (method_info->Attributes.Abstract) {
                CHECK(method_info->Attributes.Virtual);
            }

            if (method_info->Attributes.RTSpecialName) {
                CHECK(method_info->Attributes.SpecialName);
            }

            if (type->Attributes.Interface) {
                CHECK(method_info->Attributes.Abstract);
            }
        }
    }

    //
    // final cleanups for anything that is less interesting
    //

    // fill module
    CHECK(file->modules_count == 1);
    module->MetadataToken = (token_t){ .table = METADATA_MODULE, .index = 1 }.token;
    module->ModuleVersionId = *file->modules->mvid;
    CHECK_AND_RETHROW(tdn_create_string_from_cstr(file->modules->name, &module->Name));

cleanup:
    if (IS_ERROR(err)) {
        for (int i = 0; i < ARRAY_LENGTH(m_init_types); i++) {
            if (*m_init_types[i].dest == NULL) {
                TRACE("Missing init type %s.%s", m_init_types[i].namespace, m_init_types[i].name);
            }
        }
        for (int i = 0; i < ARRAY_LENGTH(m_load_types); i++) {
            if (*m_load_types[i].dest == NULL) {
                TRACE("Missing load type %s.%s", m_load_types[i].namespace, m_load_types[i].name);
            }
        }
    }

    return err;
}

static tdn_err_t load_assembly(dotnet_file_t* file, System_Reflection_Assembly* out_assembly) {
    tdn_err_t err = TDN_NO_ERROR;

    if (mCoreAssembly == NULL) {
        // start by initializing the System_Type type first
        tSystem_Type = gc_raw_alloc(sizeof(struct System_Type));
        CHECK_ERROR(tSystem_Type != NULL, TDN_ERROR_OUT_OF_MEMORY);

        // hard-code types we require for proper boostrap
        tSystem_Array = GC_NEW(System_Type); // for creating a Type[] and a Module[]
        tSystem_String = GC_NEW(System_Type); // for creating a string
        tSystem_Reflection_Assembly = GC_NEW(System_Type); // for creating the main assembly
        tSystem_Reflection_Module = GC_NEW(System_Type); // for creating the main module
    }

    // create the assembly
    System_Reflection_Assembly assembly = GC_NEW(System_Reflection_Assembly);

    // fill the assembly with all the required type information
    CHECK_AND_RETHROW(tdn_fill_assembly(file, assembly));

    // we are done, we just initialized the core assembly, so
    // set it as the core, also pre-jit all the builtin types
    if (mCoreAssembly == NULL) {
        mCoreAssembly = assembly;
        CHECK_AND_RETHROW(tdn_jit_type(tSystem_String));
    }

    *out_assembly = assembly;

cleanup:
    return err;
}

tdn_err_t tdn_load_assembly_from_memory(const void* buffer, size_t buffer_size, System_Reflection_Assembly* assembly) {
    tdn_err_t err = TDN_NO_ERROR;

    // start by actually setting up the dotnet file, which will be used to continue loading
    memory_file_handle_t handle = {
        .buffer_size = buffer_size,
        .buffer = buffer
    };
    dotnet_file_t dotnet = {
        .file = {
            .handle = &handle,
            .read_file = memory_file_read
        }
    };
    CHECK_AND_RETHROW(pe_load_image(&dotnet.file));
    CHECK_AND_RETHROW(dotnet_load_file(&dotnet));

    // actually load it
    CHECK_AND_RETHROW(load_assembly(&dotnet, assembly));

cleanup:
    dotnet_free_file(&dotnet);

    // free the dotnet file
    return err;
}
