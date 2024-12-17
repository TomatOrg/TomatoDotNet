#include "pe.h"

#include <util/alloc.h>

#include "util/except.h"

typedef struct pe_loader_context {
    tdn_err_t (*read_file)(void* file, size_t offset, size_t size, void* buffer);
    void* handle;
    uint32_t pe_header_offset;
    void* image_address;
    size_t image_size;
    size_t size_of_headers;
} pe_loader_context_t;

static tdn_err_t pe_get_header(pe_loader_context_t* context, IMAGE_NT_HEADERS32* header) {
    tdn_err_t err = TDN_NO_ERROR;

    // Read the dos header
    IMAGE_DOS_HEADER dos_header;
    CHECK_AND_RETHROW(context->read_file(context->handle, 0, sizeof(dos_header), &dos_header));

    // check if this is a dos header, and if so take the offset to the pe, otherwise
    // we will assume this is a raw pe without a dos header
    context->pe_header_offset = 0;
    if (dos_header.e_magic == IMAGE_DOS_SIGNATURE) {
        context->pe_header_offset = dos_header.e_lfanew;
    }

    // read the pe header
    CHECK_AND_RETHROW(context->read_file(context->handle, context->pe_header_offset, sizeof(*header), header));

    // make sure we understand the header
    CHECK(header->Signature == IMAGE_NT_SIGNATURE);
    // TODO: verify machine?
    CHECK(header->OptionalHeader.Magic == IMAGE_NT_OPTIONAL_HDR32_MAGIC);

    // check the size of the optional header
    uint32_t header_without_data_dir = sizeof(IMAGE_OPTIONAL_HEADER32) - sizeof(IMAGE_DATA_DIRECTORY) * IMAGE_NUMBEROF_DIRECTORY_ENTRIES;
    CHECK(((uint32_t)header->FileHeader.SizeOfOptionalHeader - header_without_data_dir == header->OptionalHeader.NumberOfRvaAndSizes * sizeof(IMAGE_DATA_DIRECTORY)));
    uint32_t section_header_offset = context->pe_header_offset + sizeof(uint32_t) + sizeof(IMAGE_FILE_HEADER) + header->FileHeader.SizeOfOptionalHeader;

    // check the file header number of section is valid
    CHECK(header->OptionalHeader.SizeOfImage > section_header_offset);
    CHECK((header->OptionalHeader.SizeOfImage - section_header_offset) / sizeof(IMAGE_SECTION_HEADER) > header->FileHeader.NumberOfSections);

    // check the optional header size of headers
    CHECK(header->OptionalHeader.SizeOfHeaders > section_header_offset);
    CHECK(header->OptionalHeader.SizeOfHeaders < header->OptionalHeader.SizeOfImage);
    CHECK((header->OptionalHeader.SizeOfHeaders - section_header_offset) / sizeof(IMAGE_SECTION_HEADER) >= header->FileHeader.NumberOfSections);

    // read the last byte to make sure the image has all the headers
    uint8_t data;
    CHECK_AND_RETHROW(context->read_file(context->handle, header->OptionalHeader.SizeOfHeaders - 1, 1, &data));

    context->image_size = header->OptionalHeader.SizeOfImage;
    context->size_of_headers = header->OptionalHeader.SizeOfHeaders;

    // check each section
    uint32_t number_of_sections = header->FileHeader.NumberOfSections;

    for (size_t i = 0; i < number_of_sections; i++) {
        IMAGE_SECTION_HEADER section_header;
        CHECK_AND_RETHROW(context->read_file(context->handle, section_header_offset, sizeof(section_header), &section_header));

        // if we have data make sure the data is in a correct offset
        // and make sure that we can read the iamge
        if (section_header.SizeOfRawData > 0) {
            CHECK(section_header.VirtualAddress >= context->size_of_headers);
            CHECK(section_header.PointerToRawData >= context->size_of_headers);

            // validate overflow
            CHECK((((uint32_t)~0) - section_header.PointerToRawData >= section_header.SizeOfRawData));
            CHECK_AND_RETHROW(context->read_file(context->handle, section_header.PointerToRawData + section_header.SizeOfRawData - 1, 1, &data));
        }

        // next
        section_header_offset += sizeof(IMAGE_SECTION_HEADER);
    }

cleanup:
    return err;
}

static tdn_err_t pe_get_image_info(pe_loader_context_t* context) {
    tdn_err_t err = TDN_NO_ERROR;

    IMAGE_NT_HEADERS32 header;
    CHECK_AND_RETHROW(pe_get_header(context, &header));

cleanup:
    return err;
}

//static bool pe_is_in_image(pe_loader_context_t* context, void* ptr, size_t size) {
//    if (ptr <= context->image_address) return false;
//    uintptr_t rva = (uintptr_t)ptr - (uintptr_t)context->image_address;
//    return rva + size <= context->image_size;
//}

static void* loader_image_address(pe_loader_context_t* context, uintptr_t address) {
    if (address >= context->image_size) {
        return NULL;
    }
    return context->image_address + address;
}

tdn_err_t pe_load_image(pe_file_t* pe_file) {
    tdn_err_t err = TDN_NO_ERROR;

    // do the initial loading
    pe_loader_context_t loader_context = {
        .handle = pe_file->handle,
        .read_file = pe_file->read_file,
        .image_address = NULL
    };
    CHECK_AND_RETHROW(pe_get_image_info(&loader_context));

    // allocate space for it
    loader_context.image_address = tdn_mallocz(loader_context.image_size);
    CHECK_ERROR(loader_context.image_address != NULL, TDN_ERROR_OUT_OF_MEMORY);

    // read the entire header into memory
    CHECK_AND_RETHROW(loader_context.read_file(loader_context.handle, 0, loader_context.size_of_headers, loader_context.image_address));
    IMAGE_NT_HEADERS32* header = loader_context.image_address + loader_context.pe_header_offset;
    IMAGE_SECTION_HEADER* first_section = loader_context.image_address +
                                            loader_context.pe_header_offset +
                                            sizeof(uint32_t) +
                                            sizeof(IMAGE_FILE_HEADER) +
                                            header->FileHeader.SizeOfOptionalHeader;
    size_t number_of_sections = header->FileHeader.NumberOfSections;

    // load each section of the image
    IMAGE_SECTION_HEADER* section = first_section;
    for (size_t i = 0; i < number_of_sections; i++) {
        // read the section
        size_t size = section->Misc.VirtualSize;
        if (size == 0 || size > section->SizeOfRawData) {
            size = section->SizeOfRawData;
        }

        // compute the address
        void* base = loader_image_address(&loader_context, section->VirtualAddress);
        void* end = loader_image_address(&loader_context, section->VirtualAddress + section->Misc.VirtualSize - 1);
        CHECK(base != NULL && end != NULL);
        if (section->SizeOfRawData > 0) {
            CHECK_AND_RETHROW(loader_context.read_file(loader_context.handle, section->PointerToRawData, size, base));
        }

        // the allocation starts out zeroed

        section++;
    }

    // set the final stuff
    pe_file->image = loader_context.image_address;
    pe_file->image_size = loader_context.image_size;
    pe_file->header = header;

cleanup:
    if (IS_ERROR(err)) {
        if (loader_context.image_address != NULL) {
            tdn_host_free(loader_context.image_address);
        }
    }

    return err;
}

void pe_free_image(pe_file_t* pe_file) {
    tdn_host_free(pe_file->image);
    pe_file->image = NULL;
}
