#include "metadata.h"
#include "util/except.h"
#include "util/string.h"
#include "sig.h"

// II.25.2.2.1
#define IMAGE_FILE_RELOCS_STRIPPED 0x0001
#define IMAGE_FILE_EXECUTABLE_IMAGE 0x0002
#define IMAGE_FILE_32BIT_MACHINE 0x0100
#define IMAGE_FILE_DLL 0x2000

#define METADATA_ROOT_SIGNATURE 0x424A5342

typedef struct metadata_root_before_version {
    uint32_t Signature;
    uint16_t MajorVersion;
    uint16_t MinorVersion;
    uint32_t Reserved;
    uint32_t Length;
} PACKED metadata_root_before_version_t;

typedef struct metadata_root_after_version {
    uint16_t Flags;
    uint16_t Streams;
} PACKED metadata_root_after_version_t;

typedef struct metadata_stream_header {
    uint32_t offset;
    uint32_t size;
    char name[];
} PACKED metadata_stream_header_t;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Metadata table expansion
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct metadata_tables_stream_header {
    uint32_t _reserved;
    uint8_t MajorVersion;
    uint8_t MinorVersion;
    uint8_t HeapSizes;
    uint8_t _reserved2;
    uint64_t Valid;
    uint64_t Sorted;
} PACKED metadata_tables_stream_header_t;

typedef enum coded_index {
    TYPE_DEF_OR_REF,
    HAS_CONSTANT,
    HAS_CUSTOM_ATTRIBUTE,
    HAS_FIELD_MARSHAL,
    HAS_DECL_SECURITY,
    MEMBER_REF_PARENT,
    HAS_SEMANTICS,
    METHOD_DEF_OR_REF,
    MEMBER_FORWARDED,
    IMPLEMENTATION,
    CUSTOM_ATTRIBUTE_TYPE,
    RESOLUTION_SCOPE,
    TYPE_OR_METHOD_DEF,
    CODED_INDEX_MAX,
} coded_index_t;

typedef struct metadata_loader_context {
    dotnet_file_t* file;
    metadata_table_info_t tables[64];
    bool coded_index_sizes[CODED_INDEX_MAX];
    uint8_t string_index_big;
    uint8_t guid_index_big;
    uint8_t blog_index_big;
} metadata_loader_context_t;

typedef enum metadata_loader_op {
    GET_TABLE_START,
    GET_TABLE_END = 63,

    GET_UINT16,
    GET_UINT32,
    GET_STRING,
    GET_GUID,
    GET_BLOB,

    GET_CODED_INDEX_START,
    GET_TYPE_DEF_OR_REF = GET_CODED_INDEX_START + TYPE_DEF_OR_REF,
    GET_HAS_CONSTANT = GET_CODED_INDEX_START + HAS_CONSTANT,
    GET_HAS_CUSTOM_ATTRIBUTE = GET_CODED_INDEX_START + HAS_CUSTOM_ATTRIBUTE,
    GET_HAS_FIELD_MARSHAL = GET_CODED_INDEX_START + HAS_FIELD_MARSHAL,
    GET_HAS_DECL_SECURITY = GET_CODED_INDEX_START + HAS_DECL_SECURITY,
    GET_MEMBER_REF_PARENT = GET_CODED_INDEX_START + MEMBER_REF_PARENT,
    GET_HAS_SEMANTICS = GET_CODED_INDEX_START + HAS_SEMANTICS,
    GET_METHOD_DEF_OR_REF = GET_CODED_INDEX_START + METHOD_DEF_OR_REF,
    GET_MEMBER_FORWARDED = GET_CODED_INDEX_START + MEMBER_FORWARDED,
    GET_IMPLEMENTATION = GET_CODED_INDEX_START + IMPLEMENTATION,
    GET_CUSTOM_ATTRIBUTE_TYPE = GET_CODED_INDEX_START + CUSTOM_ATTRIBUTE_TYPE,
    GET_RESOLUTION_SCOPE = GET_CODED_INDEX_START + RESOLUTION_SCOPE,
    GET_TYPE_OR_METHOD_DEF = GET_CODED_INDEX_START + TYPE_OR_METHOD_DEF,
    GET_CODED_INDEX_END = GET_TYPE_OR_METHOD_DEF,

    DONE,
} metadata_loader_op_t;

static uint8_t* m_coded_index_tables[] = {
    [TYPE_DEF_OR_REF] = (uint8_t[]){ 2, 3,
                                     METADATA_TYPE_DEF,
                                     METADATA_TYPE_REF,
                                     METADATA_TYPE_SPEC },
    [HAS_CONSTANT] = (uint8_t[]){ 2, 3,
                                  METADATA_FIELD,
                                  METADATA_PARAM,
                                  METADATA_PROPERTY },
    [HAS_CUSTOM_ATTRIBUTE] = (uint8_t[]){ 5, 22,
                                          METADATA_METHOD_DEF,
                                          METADATA_FIELD,
                                          METADATA_TYPE_REF,
                                          METADATA_TYPE_DEF,
                                          METADATA_PARAM,
                                          METADATA_INTERFACE_IMPL,
                                          METADATA_MEMBER_REF,
                                          METADATA_MODULE,
                                          -1,
                                          METADATA_PROPERTY,
                                          METADATA_EVENT,
                                          METADATA_STAND_ALONE_SIG,
                                          METADATA_MODULE_REF,
                                          METADATA_TYPE_SPEC,
                                          METADATA_ASSEMBLY,
                                          METADATA_ASSEMBLY_REF,
                                          METADATA_FILE,
                                          METADATA_EXPORTED_TYPE,
                                          METADATA_MANIFEST_RESOURCE,
                                          METADATA_GENERIC_PARAM,
                                          METADATA_GENERIC_PARAM_CONSTRAINT,
                                          METADATA_METHOD_SPEC},
    [HAS_FIELD_MARSHAL] = (uint8_t[]){ 1, 2,
                                       METADATA_FIELD,
                                       METADATA_PARAM},
    [HAS_DECL_SECURITY] = (uint8_t[]){ 2, 3,
                                       METADATA_TYPE_DEF,
                                       METADATA_METHOD_DEF,
                                       METADATA_ASSEMBLY},
    [MEMBER_REF_PARENT] = (uint8_t[]){ 3, 5,
                                       METADATA_TYPE_DEF,
                                       METADATA_TYPE_REF,
                                       METADATA_MODULE_REF,
                                       METADATA_METHOD_DEF,
                                       METADATA_TYPE_SPEC},
    [HAS_SEMANTICS] = (uint8_t[]){ 1, 2,
                                   METADATA_EVENT,
                                   METADATA_PROPERTY},
    [METHOD_DEF_OR_REF] = (uint8_t[]){ 1, 2,
                                       METADATA_METHOD_DEF,
                                       METADATA_MEMBER_REF},
    [MEMBER_FORWARDED] = (uint8_t[]){ 1, 2,
                                      METADATA_FIELD,
                                      METADATA_METHOD_DEF},
    [IMPLEMENTATION] = (uint8_t[]){ 2, 3,
                                    METADATA_FILE,
                                    METADATA_ASSEMBLY_REF,
                                    METADATA_EXPORTED_TYPE},
    [CUSTOM_ATTRIBUTE_TYPE] = (uint8_t[]){ 3, 5,
                                           -1,
                                           -1,
                                           METADATA_METHOD_DEF,
                                           METADATA_MEMBER_REF,
                                           -1},
    [RESOLUTION_SCOPE] = (uint8_t[]){ 2, 4,
                                      METADATA_MODULE,
                                      METADATA_MODULE_REF,
                                      METADATA_ASSEMBLY_REF,
                                      METADATA_TYPE_REF},
    [TYPE_OR_METHOD_DEF] = (uint8_t[]){ 1, 2,
                                        METADATA_TYPE_DEF,
                                        METADATA_METHOD_DEF},

};

static metadata_loader_op_t* m_table_ops[64] = {
        [METADATA_MODULE] = (metadata_loader_op_t[]){
            GET_UINT16,
            GET_STRING,
            GET_GUID,
            GET_GUID,
            GET_GUID,
            DONE },
        [METADATA_TYPE_REF] = (metadata_loader_op_t[]){
            GET_RESOLUTION_SCOPE,
            GET_STRING,
            GET_STRING,
            DONE },
        [METADATA_TYPE_DEF] = (metadata_loader_op_t[]){
            GET_UINT32,
            GET_STRING,
            GET_STRING,
            GET_TYPE_DEF_OR_REF,
            METADATA_FIELD,
            METADATA_METHOD_DEF,
            DONE },
        [METADATA_FIELD] = (metadata_loader_op_t[]){
            GET_UINT16,
            GET_STRING,
            GET_BLOB,
            DONE },
        [METADATA_METHOD_DEF] = (metadata_loader_op_t[]){
            GET_UINT32,
            GET_UINT16,
            GET_UINT16,
            GET_STRING,
            GET_BLOB,
            METADATA_PARAM,
            DONE },
        [METADATA_PARAM] = (metadata_loader_op_t[]){
            GET_UINT16,
            GET_UINT16,
            GET_STRING,
            DONE },
        [METADATA_INTERFACE_IMPL] = (metadata_loader_op_t[]){
            METADATA_TYPE_DEF,
            GET_TYPE_DEF_OR_REF,
            DONE },
        [METADATA_MEMBER_REF] = (metadata_loader_op_t[]){
            GET_MEMBER_REF_PARENT,
            GET_STRING,
            GET_BLOB,
            DONE },
        [METADATA_CONSTANT] = (metadata_loader_op_t[]) {
            GET_UINT16,
            GET_HAS_CONSTANT,
            GET_BLOB,
            DONE },
        [METADATA_CUSTOM_ATTRIBUTE] = (metadata_loader_op_t[]){
            GET_HAS_CUSTOM_ATTRIBUTE,
            GET_CUSTOM_ATTRIBUTE_TYPE,
            GET_BLOB,
            DONE },
        [METADATA_DECL_SECURITY] = (metadata_loader_op_t[]){
            GET_UINT16,
            GET_HAS_DECL_SECURITY,
            GET_BLOB,
            DONE },
        [METADATA_CLASS_LAYOUT] = (metadata_loader_op_t[]){
            GET_UINT16,
            GET_UINT32,
            METADATA_TYPE_DEF,
            DONE },
        [METADATA_FIELD_LAYOUT] = (metadata_loader_op_t[]){
            GET_UINT32,
            METADATA_FIELD,
            DONE },
        [METADATA_STAND_ALONE_SIG] = (metadata_loader_op_t[]){
            GET_BLOB,
            DONE },
        [METADATA_EVENT_MAP] = (metadata_loader_op_t[]){
            METADATA_TYPE_DEF,
            METADATA_EVENT,
            DONE },
        [METADATA_EVENT] = (metadata_loader_op_t[]){
            GET_UINT16,
            GET_STRING,
            GET_TYPE_DEF_OR_REF,
            DONE },
        [METADATA_PROPERTY_MAP] = (metadata_loader_op_t[]){
            METADATA_TYPE_DEF,
            METADATA_PROPERTY,
            DONE },
        [METADATA_PROPERTY] = (metadata_loader_op_t[]){
            GET_UINT16,
            GET_STRING,
            GET_BLOB,
            DONE },
        [METADATA_METHOD_SEMANTICS] = (metadata_loader_op_t[]){
            GET_UINT16,
            METADATA_METHOD_DEF,
            GET_HAS_SEMANTICS,
            DONE },
        [METADATA_METHOD_IMPL] = (metadata_loader_op_t[]){
            METADATA_TYPE_DEF,
            GET_METHOD_DEF_OR_REF,
            GET_METHOD_DEF_OR_REF,
            DONE },
        [METADATA_MODULE_REF] = (metadata_loader_op_t[]){
            GET_STRING,
            DONE },
        [METADATA_TYPE_SPEC] = (metadata_loader_op_t[]){
            GET_BLOB,
            DONE },
        [METADATA_FIELD_RVA] = (metadata_loader_op_t[]){
            GET_UINT32,
            METADATA_FIELD,
            DONE },
        [METADATA_ASSEMBLY] = (metadata_loader_op_t[]){
            GET_UINT32,
            GET_UINT16,
            GET_UINT16,
            GET_UINT16,
            GET_UINT16,
            GET_UINT32,
            GET_BLOB,
            GET_STRING,
            GET_STRING,
            DONE },
        [METADATA_ASSEMBLY_REF] = (metadata_loader_op_t[]){
            GET_UINT16,
            GET_UINT16,
            GET_UINT16,
            GET_UINT16,
            GET_UINT32,
            GET_BLOB,
            GET_STRING,
            GET_STRING,
            GET_BLOB,
            DONE },
        [METADATA_ASSEMBLY_REF_OS] = (metadata_loader_op_t[]){
            GET_UINT32,
            GET_UINT32,
            GET_UINT32,
            METADATA_ASSEMBLY_REF,
            DONE },
        [METADATA_FILE] = (metadata_loader_op_t[]){
            GET_UINT32,
            GET_STRING,
            GET_BLOB,
            DONE },
        [METADATA_EXPORTED_TYPE] = (metadata_loader_op_t[]){
            GET_UINT32,
            GET_UINT32,
            GET_STRING,
            GET_STRING,
            GET_IMPLEMENTATION,
            DONE },
        [METADATA_MANIFEST_RESOURCE] = (metadata_loader_op_t[]){
            GET_UINT32,
            GET_UINT32,
            GET_STRING,
            GET_IMPLEMENTATION,
            DONE },
        [METADATA_NESTED_CLASS] = (metadata_loader_op_t[]){
            METADATA_TYPE_DEF,
            METADATA_TYPE_DEF,
            DONE },
        [METADATA_GENERIC_PARAM] = (metadata_loader_op_t[]){
            GET_UINT16,
            GET_UINT16,
            GET_TYPE_OR_METHOD_DEF,
            GET_STRING,
            DONE },
        [METADATA_METHOD_SPEC] = (metadata_loader_op_t[]){
            GET_METHOD_DEF_OR_REF,
            GET_BLOB,
            DONE },
        [METADATA_GENERIC_PARAM_CONSTRAINT] = (metadata_loader_op_t[]){
            METADATA_GENERIC_PARAM,
            GET_TYPE_DEF_OR_REF,
            DONE },
};

static size_t is_coded_index_size_big(metadata_loader_context_t* context, int index) {
    uint32_t max_row_count = 0;
    uint8_t* coded_index = m_coded_index_tables[index];
    for (int i = 0; i < coded_index[1]; i++) {
        uint8_t table_idx = coded_index[2 + i];
        if (table_idx == (uint8_t)-1) continue;
        metadata_table_info_t* table = &context->tables[table_idx];
        if (max_row_count < table->row_count) {
            max_row_count = table->row_count;
        }
    }
    int value_bit_count = 16 - coded_index[0];
    return (max_row_count >= (1 << value_bit_count));
}

static size_t is_table_index_big(metadata_loader_context_t* context, int table) {
    return (context->tables[table].row_count > UINT16_MAX);
}

static tdn_err_t expand_metadata_table(metadata_loader_context_t* context, int table, void* stream, size_t stream_size, size_t* row_size) {
    tdn_err_t err = TDN_NO_ERROR;
    void* entries = NULL;

    metadata_loader_op_t* ops = m_table_ops[table];
    CHECK(ops != NULL);

    // calculate both in file and in memory sizes of the table
    size_t in_file = 0;
    size_t in_memory = 0;
    for (int i = 0; ops[i] != DONE; i++) {
        switch (ops[i]) {
            case GET_TABLE_START ... GET_TABLE_END:
                in_file += is_table_index_big(context, ops[i]) ? 4 : 2;
                in_memory += sizeof(token_t);
                break;

            case GET_CODED_INDEX_START ... GET_CODED_INDEX_END:
                in_file += context->coded_index_sizes[ops[i] - GET_CODED_INDEX_START] ? 4 : 2;
                in_memory += sizeof(token_t);
                break;

            case GET_UINT16: in_file += 2; in_memory += sizeof(uint16_t); break;
            case GET_UINT32: in_file += 4; in_memory += sizeof(uint32_t); break;
            case GET_STRING: in_file += context->string_index_big ? 4 : 2; in_memory += sizeof(const char*); break;
            case GET_BLOB: in_file += context->blog_index_big ? 4 : 2; in_memory += sizeof(blob_entry_t); break;
            case GET_GUID: in_file += context->guid_index_big ? 4 : 2; in_memory += sizeof(Guid*); break;

            default: return -1;
        }
    }

    size_t row_count = context->tables[table].row_count;

    // validate the stream is big enough
    CHECK(stream_size >= in_file * row_count);

    // allocate space for expanded part
    entries = tdn_host_mallocz(in_memory * context->tables[table].row_count);
    CHECK_ERROR(entries != NULL, TDN_ERROR_OUT_OF_MEMORY);

#define GET_INDEX(IS_BIG) \
    ({ \
        uint32_t ___value = 0; \
        if (IS_BIG) { \
            CHECK(stream_size >= 4); \
            ___value = *(uint32_t*)stream; \
            stream_size -= 4; \
            stream += 4; \
        } else { \
            CHECK(stream_size >= 2); \
            ___value = *(uint16_t*)stream; \
            stream_size -= 2; \
            stream += 2; \
        } \
        ___value; \
    })

    // expand it all
    void* entry = entries;
    void* entry_current = entry;
    void* stream_current = stream;
    for (size_t e = 0; e < context->tables[table].row_count; e++) {
        for (int i = 0; ops[i] != DONE; i++) {
            switch (ops[i]) {
                case GET_TABLE_START ... GET_TABLE_END: {
                    uint32_t index = GET_INDEX(is_table_index_big(context, ops[i]));
                    token_t* new_token = entry;
                    entry += sizeof(token_t);
                    new_token->table = ops[i];
                    new_token->index = index;

                    // validate the index is valid
                    CHECK(index <= context->tables[ops[i]].row_count + 1);
                } break;

                case GET_CODED_INDEX_START ... GET_CODED_INDEX_END: {
                    coded_index_t index = ops[i] - GET_CODED_INDEX_START;
                    uint32_t token = GET_INDEX(context->coded_index_sizes[index]);
                    uint8_t* coded_index = m_coded_index_tables[index];

                    token_t* new_token = entry;
                    entry += sizeof(token_t);

                    // get the ci table, and validate it is a good one
                    int bit_count = coded_index[0];
                    uint32_t token_table_mask = (1 << bit_count) - 1;
                    uint8_t ci_table = token & token_table_mask;
                    CHECK(ci_table < coded_index[1]);

                    // now resolve to the real table and validate it is inside of it
                    ci_table = coded_index[2 + ci_table];
                    CHECK(ci_table < 64);
                    new_token->table = ci_table;
                    new_token->index = token >> bit_count;
                    CHECK(new_token->index <= context->tables[ci_table].row_count + 1);
                } break;

                case GET_UINT16: {
                    uint16_t* new_value = entry;
                    entry += 2;

                    CHECK(stream_size >= 2);
                    *new_value = *(uint16_t*)stream;
                    stream_size -= 2;
                    stream += 2;
                } break;

                case GET_UINT32: {
                    uint32_t* new_value = entry;
                    entry += 4;

                    CHECK(stream_size >= 4);
                    *new_value = *(uint32_t*)stream;
                    stream_size -= 4;
                    stream += 4;
                } break;

                case GET_STRING: {
                    const char** new_value = entry;
                    entry += sizeof(const char*);
                    uint32_t index = GET_INDEX(context->string_index_big);

                    CHECK(index < context->file->strings_size);
                    *new_value = context->file->strings + index;
                } break;

                case GET_BLOB: {
                    blob_entry_t* new_value = entry;
                    entry += sizeof(blob_entry_t);
                    uint32_t index = GET_INDEX(context->blog_index_big);
                    CHECK(index < context->file->blob_size);

                    // parse the compressed length
                    uint32_t blob_size = 0;
                    blob_entry_t new_blob = {
                        .data = context->file->blob + index,
                        .size = context->file->blob_size - index,
                    };
                    CHECK_AND_RETHROW(sig_parse_compressed_int(&new_blob, &blob_size));

                    // validate and set the length
                    CHECK(blob_size <= context->file->blob_size - index);
                    new_blob.size = blob_size;

                    *new_value = new_blob;
                }; break;

                case GET_GUID: {
                    Guid** new_value = entry;
                    entry += sizeof(Guid*);
                    uint32_t index = GET_INDEX(context->guid_index_big);
                    if (index == 0) {
                        *new_value = NULL;
                    } else {
                        index--;
                        CHECK(index < context->file->guids_count);
                        *new_value = &context->file->guids[index];
                    }
                } break;

                default: return -1;
            }
        }

        // TODO: remove in non-debug, just makes sure
        //       we are reading this correctly
        entry_current += in_memory;
        CHECK(entry_current == entry);
        stream_current += in_file;
        CHECK(stream_current == stream);
    }

    // export the table
    context->tables[table].entries = entries;

    // output the row size
    *row_size = in_file * row_count;

cleanup:
    if (IS_ERROR(err)) {
        tdn_host_free(entries);
    }

    return err;
}

static tdn_err_t dotnet_parse_metadata_tables(dotnet_file_t* file, void* stream, size_t size) {
    tdn_err_t err = TDN_NO_ERROR;
    metadata_loader_context_t context = {};

    CHECK(size >= sizeof(metadata_tables_stream_header_t));
    metadata_tables_stream_header_t* header = stream;
    size -= sizeof(metadata_tables_stream_header_t);
    stream += sizeof(metadata_tables_stream_header_t);

    // start to setup the data we need for loading
    context.file = file;
    context.string_index_big = (header->HeapSizes & 0x01);
    context.guid_index_big = (header->HeapSizes & 0x02);
    context.blog_index_big = (header->HeapSizes & 0x04);

    // setup the row count for each of the valid ones
    for (int i = 0; i < 64; i++) {
        if (header->Valid & (1ull << i)) {
            CHECK(size >= 4);
            context.tables[i].row_count = *(uint32_t*)stream;
            CHECK(context.tables[i].row_count <= (1 << 24));
            size -= 4;
            stream += 4;
        }
    }

    // calculate the coded index sizes
    for (int i = 0; i < CODED_INDEX_MAX; i++) {
        context.coded_index_sizes[i] = is_coded_index_size_big(&context, i);
    }

    // actually expand the tables
    for (int i = 0; i < 64; i++) {
        if (header->Valid & (1ull << i)) {
            size_t row_size;
            CHECK_AND_RETHROW(expand_metadata_table(&context, i, stream, size, &row_size));
            CHECK(size >= row_size);
            size -= row_size;
            stream += row_size;
        }
    }

    memcpy(file->tables, context.tables, sizeof(context.tables));

cleanup:
    if (IS_ERROR(err)) {
        for (int i = 0; i < 64; i++) {
            if (context.tables[i].entries != NULL) {
                tdn_host_free(context.tables[i].entries);
                context.tables[i].entries = NULL;
            }
        }
    }

    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Dotnet initial metadata loader
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t dotnet_load_file(dotnet_file_t* file) {
    tdn_err_t err = TDN_NO_ERROR;

    // II.25.2.2.1
    uint16_t characteristics = file->file.header->FileHeader.Characteristics;
    CHECK((characteristics & IMAGE_FILE_RELOCS_STRIPPED) == 0);
    CHECK(characteristics & IMAGE_FILE_EXECUTABLE_IMAGE);
    CHECK((characteristics & IMAGE_FILE_32BIT_MACHINE) == 0);
//    file->dll = characteristics & IMAGE_FILE_DLL;

    // II.25.2.3.3 CLI Header
    IMAGE_DATA_DIRECTORY* com_descriptor = &file->file.header->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR];
    IMAGE_COR20_HEADER* com_header = pe_image_address(&file->file, com_descriptor->VirtualAddress);
    void* com_header_end = pe_image_address(&file->file, com_descriptor->VirtualAddress + com_descriptor->Size - 1);
    CHECK(com_header != NULL && com_header_end != NULL && (void*)com_header < com_header_end);
    CHECK(com_descriptor->Size >= sizeof(IMAGE_COR20_HEADER));
    CHECK(com_header->cb == sizeof(IMAGE_COR20_HEADER));

    // Check version
    // TODO: which minor do I need?
    CHECK(com_header->MajorRuntimeVersion == 2);
    CHECK(com_header->MinorRuntimeVersion >= 0);

    // II.25.3.3.1 Runtime flags
    CHECK(com_header->Flags & COMIMAGE_FLAGS_ILONLY);
    CHECK((com_header->Flags & COMIMAGE_FLAGS_32BITREQUIRED) == 0);
    CHECK((com_header->Flags & COMIMAGE_FLAGS_NATIVE_ENTRYPOINT) == 0);

    // II.24
    void* metadata_root = pe_image_address(&file->file, com_header->MetaData.VirtualAddress);
    void* metadata_root_end = pe_image_address(&file->file, com_header->MetaData.VirtualAddress + com_header->MetaData.Size - 1);
    CHECK(metadata_root != NULL && metadata_root_end != NULL && metadata_root < metadata_root_end);

    // II.24.2.1
    CHECK(com_header->MetaData.Size > sizeof(metadata_root_before_version_t));
    metadata_root_before_version_t* before = metadata_root;
    CHECK(before->Signature == METADATA_ROOT_SIGNATURE);
    const char* version = (const char*)(before + 1);
    CHECK(tdn_host_strnlen(version, 255) < 255);
    metadata_root_after_version_t* after = (metadata_root_after_version_t*)((uintptr_t)version + before->Length);
    CHECK(com_header->MetaData.Size >= (void*)(after + 1) - (void*)before);

    file->entry_point_token = com_header->EntryPointToken;

    // II.24.2.2
    void* md_stream_start = NULL;
    size_t md_stream_size = 0;

    metadata_stream_header_t* stream = (metadata_stream_header_t*)(after + 1);
    for (int i = 0; i < after->Streams; i++) {
        size_t len = tdn_host_strnlen(stream->name, 33);
        CHECK(len < 33);
        len += 1;

        void* start = pe_image_address(&file->file, com_header->MetaData.VirtualAddress + stream->offset);
        void* end = pe_image_address(&file->file, com_header->MetaData.VirtualAddress + stream->offset + stream->size - 1);
        CHECK(start != NULL && end != NULL && start <= end);
        CHECK((stream->size % 4) == 0);

        // check which one it can be
        if (strcmp(stream->name, "#Strings") == 0) {
            CHECK(stream->size <= 0x20000000); // Ecma-335-Augments.md#heap-sizes
            file->strings = start;
            file->strings_size = stream->size;
        } else if (strcmp(stream->name, "#US") == 0) {
            file->us = start;
            file->us_size = stream->size;
        } else if (strcmp(stream->name, "#Blob") == 0) {
            CHECK(stream->size <= 0x20000000); // Ecma-335-Augments.md#heap-sizes
            file->blob = start;
            file->blob_size = stream->size;
        } else if (strcmp(stream->name, "#GUID") == 0) {
            file->guids = start;
            file->guids_count = stream->size / 16;
        } else if (strcmp(stream->name, "#~") == 0) {
            md_stream_start = start;
            md_stream_size = stream->size;
        } else {
            WARN("metadata: unknown stream name `%s`", stream->name);
        }

        // next
        stream = (void*)(stream + 1) + ALIGN_UP(len, 4);
    }

    CHECK(md_stream_start != NULL);
    CHECK_AND_RETHROW(dotnet_parse_metadata_tables(file, md_stream_start, md_stream_size));

cleanup:
    return err;
}

void dotnet_free_file(dotnet_file_t* file) {
    if (file == NULL) return;

    for (size_t i = 0; i < ARRAY_LENGTH(file->tables); i++) {
        tdn_host_free(file->tables[i].entries);
        file->tables[i].entries = NULL;
    }

    file->us = NULL;
    file->strings = NULL;
    file->blob = NULL;
    file->guids = NULL;
    pe_free_image(&file->file);
}