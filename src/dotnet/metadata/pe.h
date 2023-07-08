#pragma once

#include <stddef.h>
#include <stdint.h>
#include "tinydotnet/except.h"
#include "dotnet/metadata/metadata_tables.h"
#include "pe_image.h"

typedef struct pe_file {
    // used to abstract loading of files
    tdn_err_t (*read_file)(void* file, size_t offset, size_t size, void* buffer);
    void (*close_handle)(void* handle);
    void* handle;

    // the loaded image
    void* image;
    size_t image_size;
    IMAGE_NT_HEADERS32* header;
} pe_file_t;

static inline void* pe_image_address(pe_file_t* context, uintptr_t address) {
    if (address >= context->image_size) {
        return NULL;
    }
    return context->image + address;
}

tdn_err_t pe_load_image(pe_file_t* pe_file);

void pe_free_image(pe_file_t* pe_file);
