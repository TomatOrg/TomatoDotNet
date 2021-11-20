#include "assembly.h"

#include "assembly_internal.h"
#include "metadata.h"
#include "pe_spec.h"
#include "signature.h"

#include <util/stb_ds.h>
#include <stdalign.h>

#include <string.h>
#include <stdlib.h>


#define READ32(addr) (*((uint32_t*)(addr)))

typedef struct pe_context {
    // the binary itself
    uint8_t* blob;
    size_t blob_size;

    // the sections
    pe_section_header_t* section_headers;
    size_t section_header_count;

    // the assembly we are loading
    assembly_t* assembly;

    // temporary metadata
    pe_cli_header_t* cli_header;
    metadata_t metadata;
} parsed_pe_t;

/**
 * Get the RVA data as a new heap allocated buffer
 */
static void* get_rva_data(pe_directory_t directory, parsed_pe_t* ctx) {
    for (int i = 0; i < ctx->section_header_count; i++) {
        pe_section_header_t* header = &ctx->section_headers[i];

        // make sure it is in the section
        if (directory.rva < header->virtual_address) continue;
        if (directory.rva + directory.size > header->virtual_address + header->virtual_size) continue;

        // it is, allocate the data for it
        void* ptr = malloc(directory.size);
        if (ptr == NULL) return NULL;

        // get the raw values
        size_t offset = (directory.rva - header->virtual_address);
        size_t raw_offset = header->pointer_to_raw_data + offset;
        size_t raw_size = header->size_of_raw_data - offset;

        // calc the amount to copy and copy it
        size_t size_to_copy = directory.size;
        if (size_to_copy > raw_size) {
            size_to_copy = raw_size;
        }
        memcpy(ptr, ctx->blob + raw_offset, size_to_copy);

        // return the data itself, after allocation
        return ptr;
    }
    return NULL;
}

/**
 * Resolve the RVA as a pointer
 */
static void* get_rva_ptr(pe_directory_t* directory, parsed_pe_t* ctx) {
    for (int i = 0; i < ctx->section_header_count; i++) {
        pe_section_header_t* header = &ctx->section_headers[i];

        if (header->virtual_address <= directory->rva && directory->rva < header->virtual_address + header->virtual_size) {
            size_t offset = (directory->rva - header->virtual_address);
            size_t raw_offset = header->pointer_to_raw_data + offset;
            size_t raw_size = header->size_of_raw_data - offset;

            // set the size we have left and return the pointer
            directory->size = raw_size;
            return ctx->blob + raw_offset;
        }
    }
    return NULL;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Does all of the PE parsing the get the sections and to get the cli header
 */
static err_t parse_pe(parsed_pe_t* ctx) {
    err_t err = NO_ERROR;
    pe_cli_header_t* cli_header = NULL;

    // Get the lfanew and verify it
    CHECK(0x3c < ctx->blob_size);
    uint32_t lfanew = READ32(ctx->blob + 0x3c);
    uint32_t sections_offset = lfanew + 4 + sizeof(pe_file_header_t) + sizeof(pe_optional_header_t);
    CHECK(sections_offset < ctx->blob_size);

    // check the signature
    CHECK(ctx->blob[lfanew + 0] == 'P');
    CHECK(ctx->blob[lfanew + 1] == 'E');
    CHECK(ctx->blob[lfanew + 2] == '\0');
    CHECK(ctx->blob[lfanew + 3] == '\0');

    // get the pe header and verify it
    pe_file_header_t* file_header = (pe_file_header_t*)&ctx->blob[lfanew + 4];
    CHECK(file_header->machine == 0x14c);
    CHECK(file_header->optional_header_size == sizeof(pe_optional_header_t));
    CHECK(!(file_header->characteristics & IMAGE_FILE_RELOCS_STRIPPED));
    CHECK(file_header->characteristics & IMAGE_FILE_EXECUTABLE_IMAGE);

    // check the optional header, we ignore os, user and subsys versions for now
    pe_optional_header_t* optional_header = (pe_optional_header_t*)&ctx->blob[lfanew + 4 + sizeof(pe_file_header_t)];
    CHECK(optional_header->magic == 0x10B);
    CHECK(optional_header->image_base % 0x10000 == 0);
    CHECK(optional_header->section_alignment > optional_header->file_alignment);
    CHECK(optional_header->file_alignment == 0x200);
    CHECK(optional_header->file_checksum == 0);
    CHECK(optional_header->stack_reserve_size == SIZE_1MB);
    CHECK(optional_header->stack_commit_size == SIZE_4KB);
    CHECK(optional_header->heap_reserve_size == SIZE_1MB);
    CHECK(optional_header->heap_commit_size == SIZE_4KB);
    CHECK(optional_header->loader_flags == 0);
    CHECK(optional_header->number_of_data_directories == 0x10);

    // Verify the sections size and get the section headers
    CHECK(sections_offset + file_header->number_of_sections * sizeof(pe_section_header_t) < ctx->blob_size);
    ctx->section_header_count = file_header->number_of_sections;
    ctx->section_headers = (pe_section_header_t*)&ctx->blob[sections_offset];

    // verify section headers are all within the binary
    for (int i = 0; i < ctx->section_header_count; i++) {
        CHECK(ctx->section_headers[i].size_of_raw_data % optional_header->file_alignment == 0);
        CHECK(ctx->section_headers[i].pointer_to_raw_data % optional_header->file_alignment == 0);
        CHECK(ctx->section_headers[i].pointer_to_raw_data + ctx->section_headers[i].size_of_raw_data <= ctx->blob_size);
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Parse the CLI header
    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    // get and verify the cli header
    size_t cli_header_size = optional_header->cli_header.size;
    CHECK(cli_header_size >= sizeof(pe_cli_header_t));
    cli_header = get_rva_data(optional_header->cli_header, ctx);
    CHECK(cli_header != NULL);
    CHECK(cli_header->cb == sizeof(pe_cli_header_t));
    CHECK(cli_header->major_runtime_version == 2);
    CHECK(cli_header->minor_runtime_version == 5); // standard says 0, this is actually 5 :shrug:
    CHECK(cli_header->flags & COMIMAGE_FLAGS_ILONLY);
    CHECK(!(cli_header->flags & COMIMAGE_FLAGS_32BITREQUIRED));
    CHECK(!(cli_header->flags & COMIMAGE_FLAGS_NATIVE_ENTRYPOINT));
    CHECK(!(cli_header->flags & COMIMAGE_FLAGS_TRACKDEBUGDATA));

    // set it
    ctx->cli_header = cli_header;

cleanup:
    if (IS_ERROR(err)) {
        SAFE_FREE(cli_header);
    }

    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Just gets the metadata root from the cli header and
 * parses all of the metadata into the parsed pe context
 */
static err_t decode_metadata(parsed_pe_t* ctx) {
    err_t err = NO_ERROR;
    void* metadata_root = NULL;

    // get the metadata
    metadata_root = get_rva_data(ctx->cli_header->metadata, ctx);
    CHECK_ERROR(metadata_root != NULL, ERROR_NOT_FOUND);

    // parse it
    CHECK_AND_RETHROW(metadata_parse(ctx->assembly, metadata_root, ctx->cli_header->metadata.size, &ctx->metadata));

cleanup:
    SAFE_FREE(metadata_root);
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * This should be called on the main assembly before the create_all_types is called, it is used for
 * finding all of the base built-in types that need to be found for any type signature.
 */
static err_t init_types(assembly_t* assembly, metadata_t* metadata) {
    err_t err = NO_ERROR;

    metadata_type_def_t* type_defs = metadata->tables[METADATA_TYPE_DEF].table;
    for (int i = 0; i < assembly->types_count; i++) {
        metadata_type_def_t* type_def = &type_defs[i];
        type_t* type = &assembly->types[i];

        // must be in the system namespace
        if (strcmp(type_def->type_namespace, "System") != 0) {
            continue;
        }

        // now figure if this is a type we care about
        if (g_void == NULL && strcmp(type_def->type_name, "Void") == 0) {
            g_void = type;
        } else if (g_boolean == NULL && strcmp(type_def->type_name, "Boolean") == 0) {
            g_boolean = type;
            g_boolean->stack_type = STACK_TYPE_INT32;
            g_boolean->stack_size = sizeof(unsigned char);
            g_boolean->stack_alignment = alignof(unsigned char);
            g_boolean->is_value_type = 1;
            g_boolean->resolved_size = 1;
            g_boolean->resolved_value_type = 1;
        } else if (g_char == NULL && strcmp(type_def->type_name, "Char") == 0) {
            g_char = type;
            g_char->stack_type = STACK_TYPE_INT32;
            g_char->stack_size = sizeof(unsigned short);
            g_char->stack_alignment = alignof(unsigned short);
            g_char->is_value_type = 1;
            g_char->resolved_size = 1;
            g_char->resolved_value_type = 1;
        } else if (g_sbyte == NULL && strcmp(type_def->type_name, "SByte") == 0) {
            g_sbyte = type;
            g_sbyte->stack_type = STACK_TYPE_INT32;
            g_sbyte->stack_size = sizeof(int8_t);
            g_sbyte->stack_alignment = alignof(int8_t);
            g_sbyte->is_value_type = 1;
            g_sbyte->resolved_size = 1;
            g_sbyte->resolved_value_type = 1;
        } else if (g_byte == NULL && strcmp(type_def->type_name, "Byte") == 0) {
            g_byte = type;
            g_byte->stack_type = STACK_TYPE_INT32;
            g_byte->stack_size = sizeof(uint8_t);
            g_byte->stack_alignment = alignof(uint8_t);
            g_byte->is_value_type = 1;
            g_byte->resolved_size = 1;
            g_byte->resolved_value_type = 1;
        } else if (g_int16 == NULL && strcmp(type_def->type_name, "Int16") == 0) {
            g_int16 = type;
            g_int16->stack_type = STACK_TYPE_INT32;
            g_int16->stack_size = sizeof(int16_t);
            g_int16->stack_alignment = alignof(int16_t);
            g_int16->is_value_type = 1;
            g_int16->resolved_size = 1;
            g_int16->resolved_value_type = 1;
        } else if (g_uint16 == NULL && strcmp(type_def->type_name, "UInt16") == 0) {
            g_uint16 = type;
            g_uint16->stack_type = STACK_TYPE_INT32;
            g_uint16->stack_size = sizeof(uint16_t);
            g_uint16->stack_alignment = alignof(uint16_t);
            g_uint16->is_value_type = 1;
            g_uint16->resolved_size = 1;
            g_uint16->resolved_value_type = 1;
        } else if (g_int32 == NULL && strcmp(type_def->type_name, "Int32") == 0) {
            g_int32 = type;
            g_int32->stack_type = STACK_TYPE_INT32;
            g_int32->stack_size = sizeof(int32_t);
            g_int32->stack_alignment = alignof(int32_t);
            g_int32->is_value_type = 1;
            g_int32->resolved_size = 1;
            g_int32->resolved_value_type = 1;
        } else if (g_uint32 == NULL && strcmp(type_def->type_name, "UInt32") == 0) {
            g_uint32 = type;
            g_uint32->stack_type = STACK_TYPE_INT32;
            g_uint32->stack_size = sizeof(uint32_t);
            g_uint32->stack_alignment = alignof(uint32_t);
            g_uint32->is_value_type = 1;
            g_uint32->resolved_size = 1;
            g_uint32->resolved_value_type = 1;
        } else if (g_int64 == NULL && strcmp(type_def->type_name, "Int64") == 0) {
            g_int64 = type;
            g_int64->stack_type = STACK_TYPE_INT64;
            g_int64->stack_size = sizeof(int64_t);
            g_int64->stack_alignment = alignof(int64_t);
            g_int64->is_value_type = 1;
            g_int64->resolved_size = 1;
            g_int64->resolved_value_type = 1;
        } else if (g_uint64 == NULL && strcmp(type_def->type_name, "UInt64") == 0) {
            g_uint64 = type;
            g_uint64->stack_type = STACK_TYPE_INT64;
            g_uint64->stack_size = sizeof(uint64_t);
            g_uint64->stack_alignment = alignof(uint64_t);
            g_uint64->is_value_type = 1;
            g_uint64->resolved_size = 1;
            g_uint64->resolved_value_type = 1;
        } else if (g_float == NULL && strcmp(type_def->type_name, "Float") == 0) {
            g_float = type;
            g_float->stack_type = STACK_TYPE_F;
            g_float->stack_size = sizeof(float);
            g_float->stack_alignment = alignof(float);
            g_float->is_value_type = 1;
            g_float->resolved_size = 1;
            g_float->resolved_value_type = 1;
        } else if (g_double == NULL && strcmp(type_def->type_name, "Double") == 0) {
            g_double = type;
            g_double->stack_type = STACK_TYPE_F;
            g_double->stack_size = sizeof(double);
            g_double->stack_alignment = alignof(double);
            g_double->is_value_type = 1;
            g_double->resolved_size = 1;
            g_double->resolved_value_type = 1;
        } else if (g_string == NULL && strcmp(type_def->type_name, "String") == 0) {
            g_string = type;
            g_string->stack_type = STACK_TYPE_T;
            g_string->stack_size = sizeof(void*);
            g_string->stack_alignment = alignof(void*);
            g_string->is_value_type = 0;
            g_string->resolved_value_type = 1;
        } else if (g_intptr == NULL && strcmp(type_def->type_name, "IntPtr") == 0) {
            g_intptr = type;
            g_intptr->stack_type = STACK_TYPE_NATIVE_INT;
            g_intptr->stack_size = sizeof(intptr_t);
            g_intptr->stack_alignment = sizeof(intptr_t);
            g_intptr->is_value_type = 1;
            g_intptr->resolved_size = 1;
            g_intptr->resolved_value_type = 1;
        } else if (g_uintptr == NULL && strcmp(type_def->type_name, "UIntPtr") == 0) {
            g_uintptr = type;
            g_uintptr->stack_type = STACK_TYPE_NATIVE_INT;
            g_uintptr->stack_size = sizeof(uintptr_t);
            g_uintptr->stack_alignment = alignof(uintptr_t);
            g_uintptr->is_value_type = 1;
            g_uintptr->resolved_size = 1;
            g_uintptr->resolved_value_type = 1;
        } else if (g_object == NULL && strcmp(type_def->type_name, "Object") == 0) {
            g_object = type;
            g_object->stack_type = STACK_TYPE_T;
            g_object->stack_size = sizeof(void*);
            g_object->stack_alignment = alignof(void*);
            g_object->memory_size = 0;
            g_object->memory_alignment = 0;
            g_object->is_value_type = 0;
            g_object->resolved_value_type = 1;
            g_object->resolved_size = 1;
        } else if (g_array == NULL && strcmp(type_def->type_name, "Array") == 0) {
            g_array = type;
            g_array->is_value_type = 0;
            g_array->resolved_value_type = 1;
        } else if (g_value_type == NULL && strcmp(type_def->type_name, "ValueType") == 0) {
            g_value_type = type;
            g_value_type->stack_type = STACK_TYPE_T;
            g_value_type->stack_size = 0;
            g_value_type->stack_alignment = 0;
            g_value_type->is_value_type = 1;
            g_value_type->resolved_size = 1;
            g_value_type->resolved_value_type = 1;
        }
    }

    // check we have all the base types
    CHECK(g_void != NULL);
    CHECK(g_boolean != NULL);
    CHECK(g_char != NULL);
    CHECK(g_sbyte != NULL);
    CHECK(g_byte != NULL);
    CHECK(g_int16 != NULL);
    CHECK(g_uint16 != NULL);
    CHECK(g_int32 != NULL);
    CHECK(g_uint32 != NULL);
    CHECK(g_int64 != NULL);
    CHECK(g_uint64 != NULL);
//    CHECK(g_float != NULL);
//    CHECK(g_double != NULL);
//    CHECK(g_string != NULL);
    CHECK(g_intptr != NULL);
    CHECK(g_uintptr != NULL);
    CHECK(g_object != NULL);
    CHECK(g_array != NULL);
    CHECK(g_value_type != NULL);

cleanup:
    return err;
}

typedef struct fat_method_header {
    uint16_t flags : 12;
    uint16_t size : 4;
    uint16_t max_stack;
    uint32_t code_size;
    token_t local_var_sig_tok;
} fat_method_header_t;

static err_t create_all_types(assembly_t* assembly, metadata_t* metadata, parsed_pe_t* pe) {
    err_t err = NO_ERROR;

    // the tables
    metadata_type_def_t* type_defs = metadata->tables[METADATA_TYPE_DEF].table;
    metadata_type_ref_t* type_refs = metadata->tables[METADATA_TYPE_REF].table;
    metadata_method_def_t* method_defs = metadata->tables[METADATA_METHOD_DEF].table;
    metadata_field_t* md_fields = metadata->tables[METADATA_FIELD].table;
    metadata_param_t* params = metadata->tables[METADATA_PARAM].table;
    metadata_stand_alone_sig_t* sigs = metadata->tables[METADATA_STAND_ALONE_SIG].table;;

    // iterate all the types
    for (int i = 0; i < assembly->types_count; i++) {
        metadata_type_def_t* type_def = &type_defs[i];
        type_t* type = &assembly->types[i];

        // setup the type info
        type->assembly = assembly;
        type->token = (token_t) { .table = METADATA_TYPE_DEF, .index = i + 1 };
        type->name = type_def->type_name;
        type->namespace = type_def->type_namespace;

        type->is_interface = type_def->flags & 0x20 ? 1 : 0;

        // get the parent type, if any
        token_t extends = type_def->extends;
        if (extends.index != 0) {
            switch (extends.table) {

                // we got an internal type, get it
                case METADATA_TYPE_DEF: {
                    CHECK (extends.index - 1 < assembly->types_count);
                    type->extends = &assembly->types[extends.index - 1];
                } break;

                default: CHECK_FAIL();
            }
        }

        // get the methods, for this we need the start and last index
        int start_index = type_def->method_list.index - 1;
        int end_index = -1;
        if (i == assembly->types_count - 1) {
            // the end is the amount of rows since this is the last item
            end_index = assembly->methods_count;
        } else {
            // the end is the start of the next object
            end_index = type_defs[i + 1].method_list.index - 1;
        }
        CHECK(start_index <= assembly->methods_count);
        CHECK(end_index <= assembly->methods_count);
        CHECK(start_index <= end_index);

        // now set the count of methods and the pointer, will also
        // set the parent reference on the way
        type->methods_count = end_index - start_index;
        if (type->methods_count > 0) {
            type->methods = &assembly->methods[start_index];
            for (int j = 0; j < type->methods_count; j++) {
                metadata_method_def_t* method_def = &method_defs[start_index + j];
                method_t* method = &type->methods[j];

                method->assembly = assembly;
                method->name = method_def->name;
                method->parent = type;
                method->token = (token_t) { .table = METADATA_TYPE_DEF, .index = start_index + j + 1 };

                // setup cil data
                pe_directory_t directory = { .rva = method_def->rva };
                uint8_t* cil = get_rva_ptr(&directory, pe);
                CHECK(cil != NULL);
                if ((cil[0] & 0x3) == 0x2) {
                    // Tiny format
                    method->max_stack_depth = 8;
                    method->cil_size = cil[0] >> 2;
                    directory.rva += 1;
                } else if ((cil[0] & 0x3) == 0x3) {
                    // Fat format
                    fat_method_header_t* fat_header = (fat_method_header_t*)cil;
                    method->max_stack_depth = fat_header->code_size;
                    method->cil_size = fat_header->code_size;
                    directory.rva += fat_header->size * 4;

                    if (fat_header->local_var_sig_tok.packed != 0) {
                        CHECK(fat_header->local_var_sig_tok.table == METADATA_STAND_ALONE_SIG);
                        CHECK(1 <= fat_header->local_var_sig_tok.index && fat_header->local_var_sig_tok.index <= metadata->tables[METADATA_STAND_ALONE_SIG].rows);

                        sig_t sig = { .entry = sigs[fat_header->local_var_sig_tok.index - 1].signature };
                        CHECK_AND_RETHROW(sig_parse_method_locals(&sig, method));
                    }
                } else {
                    CHECK_FAIL();
                }
                directory.size = method->cil_size;
                method->cil = get_rva_data(directory, pe);
                CHECK(method->cil != NULL);

                // flags
                method->is_static = method_def->flags & 0x10 ? 1 : 0;
                method->is_final = method_def->flags & 0x20 ? 1 : 0;
                method->is_virtual = method_def->flags & 0x40 ? 1 : 0;
                method->is_abstract = method_def->flags & 0x400 ? 1 : 0;
                CHECK(!(method->is_static && method->is_final));
                CHECK(!(method->is_static && method->is_virtual));
                CHECK(!(method->is_final && method->is_abstract));
                if (method->is_abstract) CHECK(method->is_virtual);

                // parse the method signature
                sig_t sig = { .entry = method_def->signature };
                CHECK_AND_RETHROW(sig_parse_method(&sig, method));
            }
        }

        // get the md_fields list
        // get the methods, for this we need the start and last index
        start_index = type_def->field_list.index - 1;
        end_index = -1;
        if (i == assembly->types_count - 1) {
            // the end is the amount of rows since this is the last item
            end_index = metadata->tables[METADATA_FIELD].rows;
        } else {
            // the end is the start of the next object
            end_index = type_defs[i + 1].field_list.index - 1;
        }
        CHECK(start_index <= metadata->tables[METADATA_FIELD].rows);
        CHECK(end_index <= metadata->tables[METADATA_FIELD].rows);
        CHECK(start_index <= end_index);

        // setup all the md_fields in this type
        type->fields_count = end_index - start_index;
        if (type->fields_count > 0) {
            type->fields = &assembly->fields[start_index];
            for (int j = 0; j < type->fields_count; j++) {
                metadata_field_t* md_field = &md_fields[start_index + j];
                type->fields[j].name = md_field->name;
                type->fields[j].parent = type;
                type->fields[j].is_static = md_field->flags & 0x10 ? 1 : 0;
                type->fields[j].is_init_only = md_field->flags & 0x20 ? 1 : 0;
                type->fields[j].is_literal = md_field->flags & 0x40 ? 1 : 0;

                // parse the field type
                sig_t sig = { .entry = md_field->signature };
                CHECK_AND_RETHROW(sig_parse_field(&sig, assembly, &type->fields[j]));
            }
        }
    }

cleanup:
    return err;
}

static void resolve_value_type(type_t* type) {
    // already set the value type info
    if (type->resolved_value_type) {
        return;
    }

    // if we extend from a value type then this is a value type
    if (type->extends != NULL) {
        resolve_value_type(type->extends);
        type->is_value_type = type->extends->is_value_type;
    } else {
        type->is_value_type = 0;
    }

    // resolved it
    type->resolved_value_type = 1;
}

static void resolve_size(type_t* type);

static size_t sum_field_sizes(type_t* type, size_t* alignment) {
    size_t size = 0;

    if (type->extends != NULL) {
        resolve_size(type->extends);
        size = type->extends->memory_size;
    }

    // TODO: vtables

    for (int i = 0; i < type->fields_count; i++) {
        field_t* field = &type->fields[i];
        if (field->is_static) continue;
        resolve_size(field->type);

        // align the size, set the offset, and increment it
        size = ALIGN_UP(size, field->type->stack_alignment);
        field->offset = size;
        size += field->type->stack_size;

        // the alignment of everything must be the largest needed alignment
        if (*alignment < field->type->stack_alignment) {
            *alignment = field->type->stack_alignment;
        }
    }

    // return the size
    return size;
}

// TODO: track cyclic dependencies
static void resolve_size(type_t* type) {
    if (type->resolved_size) {
        return;
    }

    // everything which is user defined is T
    type->stack_type = STACK_TYPE_T;

    // first resolve the stack size because the element can't include itself
    // on the stack if it is a value type
    if (!type->resolved_size) {
        if (type->is_value_type) {
            // the stack size is the sum of all the fields aligned to the alignment
            // of the largest field
            type->stack_size = sum_field_sizes(type, &type->stack_alignment);
            type->stack_size = ALIGN_UP(type->stack_size, type->stack_alignment);
        } else {
            // for non-value-types the stack size is the size of an object on the stack
            // which should be just the size of a pointer
            type->stack_size = g_object->stack_size;
            type->stack_alignment = g_object->stack_alignment;
        }

        // we can now safely calculate the non-stack size
        type->resolved_size = 1;
    }

    // resolve the memory size
    type->memory_size = 0;

    if (type->is_value_type) {
        type->memory_size = type->stack_size;
        type->memory_alignment = type->stack_alignment;
    } else {
        // we are going to set the memory size in here because classes can have
        // cyclic values (because they are stored as pointers)
        type->memory_size = sum_field_sizes(type, &type->memory_alignment);
        type->memory_size = ALIGN_UP(type->memory_size, type->memory_alignment);
    }
}

static void setup_sizes_and_offsets(assembly_t* assembly) {
    for (int i = 0; i < assembly->types_count; i++) {
        type_t* type = &assembly->types[i];

        if (type->is_interface) {
            // this is more complicated...
            ERROR("TODO: is interface");
        } else {
            // resolve the value type and size of the type
            resolve_value_type(type);
            resolve_size(type);
        }
    }
}

static void add_this_parameter(assembly_t* assembly) {
    // properly add the `this` as a parameter so we can have
    // easier time with code generation
    for (int i = 0; i < assembly->types_count; i++) {
        type_t* type = &assembly->types[i];

        for (int j = 0; j < type->methods_count; j++) {
            method_t* method = &type->methods[j];

            if (!method->is_static) {
                // this is a non-static method, meaning it has a this, make space for it in the parameters
                method->parameters = realloc(method->parameters, (method->parameter_count + 1) * sizeof(param_t));
                memmove(method->parameters + 1, method->parameters, method->parameter_count * sizeof(param_t));
                method->parameter_count++;

                // figure its type, value types are always passed by ref to this
                method->parameters[0].name = "this";
                if (type->is_value_type) {
                    method->parameters[0].type = get_by_ref_type(type);
                } else {
                    method->parameters[0].type = type;
                }
            }
        }
    }
}

static err_t setup_nested_types(assembly_t* assembly, metadata_t* metadata) {
    err_t err = NO_ERROR;

    metadata_nested_class_t* nested_class = metadata->tables[METADATA_NESTED_CLASS].table;
    for (int i = 0; i < metadata->tables[METADATA_NESTED_CLASS].rows; i++) {
        type_t* nested = assembly_get_type_by_token(assembly, nested_class->nested_class);
        type_t* enclosing = assembly_get_type_by_token(assembly, nested_class->enclosing_class);
        CHECK(nested != NULL);
        CHECK(enclosing != NULL);
        nested->enclosing = enclosing;
    }

cleanup:
    return err;
}

assembly_t* g_corelib_assembly = NULL;

err_t load_assembly_from_blob(uint8_t* blob, size_t blob_size, assembly_t** assembly) {
    err_t err = NO_ERROR;
    parsed_pe_t ctx = { 0 };

    // allocate the new assembly
    assembly_t* new_assembly = calloc(1, sizeof(assembly_t));
    CHECK_ERROR(new_assembly != NULL, ERROR_OUT_OF_RESOURCES);

    // parse the PE binary
    ctx = (parsed_pe_t) {
        .blob = blob,
        .blob_size = blob_size,
        .assembly = new_assembly,
    };
    CHECK_AND_RETHROW(parse_pe(&ctx));

    // now parse the metadata
    CHECK_AND_RETHROW(decode_metadata(&ctx));

    // set the base arrays for methods and types
    new_assembly->types_count = ctx.metadata.tables[METADATA_TYPE_DEF].rows;
    new_assembly->types = calloc(new_assembly->types_count, sizeof(type_t));
    CHECK(new_assembly->types != NULL);

    new_assembly->methods_count = ctx.metadata.tables[METADATA_METHOD_DEF].rows;
    new_assembly->methods = calloc(new_assembly->methods_count, sizeof(method_t));
    CHECK(new_assembly->methods != NULL);

    new_assembly->fields_count = ctx.metadata.tables[METADATA_FIELD].rows;
    new_assembly->fields = calloc(new_assembly->fields_count, sizeof(field_t));
    CHECK(new_assembly->fields != NULL);

    // if the kernel assembly is NULL at this point we are loading the kernel
    // assembly, so setup all the base types
    if (g_corelib_assembly == NULL) {
        g_corelib_assembly = new_assembly;
        CHECK_AND_RETHROW(init_types(new_assembly, &ctx.metadata));
    }

    // setup all of the type info from the metadata
    CHECK_AND_RETHROW(create_all_types(new_assembly, &ctx.metadata, &ctx));
    CHECK_AND_RETHROW(setup_nested_types(new_assembly, &ctx.metadata));

    // Setup any extra info that is needed once all the types are loaded
    setup_sizes_and_offsets(new_assembly);
    add_this_parameter(new_assembly);

    // setup the name
    metadata_module_t* module = ctx.metadata.tables[METADATA_MODULE].table;
    new_assembly->name = module->name;

    // set the output
    *assembly = new_assembly;

cleanup:

    // Free everything we don't need anymore
    SAFE_FREE(ctx.cli_header);
    free_metadata(&ctx.metadata);

    // if we got an error also free the assembly...
    if (IS_ERROR(err)) {
        free_assembly(new_assembly);
    }

    return err;
}

err_t load_assembly(const char* file, assembly_t** assembly) {
    err_t err = NO_ERROR;
    void* ptr = NULL;
    FILE* f = NULL;

    f = fopen(file, "r");
    CHECK(f != NULL);

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    ptr = malloc(size);
    CHECK_ERROR(ptr != NULL, ERROR_OUT_OF_RESOURCES);

    CHECK(fread(ptr, size, 1, f) == 1);

    CHECK_AND_RETHROW(load_assembly_from_blob(ptr, size, assembly));

cleanup:
    if (f != NULL) {
        fclose(f);
    }
    SAFE_FREE(ptr);
    return err;
}

method_t* assembly_get_method_by_token(assembly_t* assembly, token_t token) {
    switch (token.table) {
        case METADATA_METHOD_DEF: {
            if (token.index < 1 || token.index > assembly->methods_count) {
                return NULL;
            }
            return &assembly->methods[token.index - 1];
        }

        // TODO: method ref

        default: return NULL;
    }
}

struct type* assembly_get_type_by_token(assembly_t* assembly, token_t token) {
    switch (token.table) {
        case METADATA_TYPE_DEF: {
            if (token.index < 1 || token.index > assembly->types_count) {
                return NULL;
            }
            return &assembly->types[token.index - 1];
        }

        // TODO: type ref

        default: return NULL;
    }
}

type_t* assembly_get_type_by_name(assembly_t* assembly, const char* namespace, const char* name) {
    for (int i = 0; i < assembly->types_count; i++) {
        type_t* type = &assembly->types[i];
        if (strcmp(namespace, type->namespace) != 0) continue;
        if (strcmp(name, type->name) != 0) continue;
        return type;
    }
    return NULL;
}

void free_assembly(assembly_t* assembly) {
    if (assembly != NULL) {
        // TODO: destroy all the dynamic types

        // destroy the types and methods
        SAFE_FREE(assembly->types);
        SAFE_FREE(assembly->methods);

        // destroy all the heaps
        SAFE_FREE(assembly->strings);
        SAFE_FREE(assembly->us);
        SAFE_FREE(assembly->blob);
        SAFE_FREE(assembly->guids);

        // free the assembly itself
        SAFE_FREE(assembly);
    }
}
