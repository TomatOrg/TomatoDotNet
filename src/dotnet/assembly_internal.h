#pragma once

#include "method.h"
#include "types.h"
#include "type.h"

#include <util/defs.h>

#include <stddef.h>
#include <stdint.h>

struct assembly {
    // the name of the assembly
    const char* name;

    // list of all the methods
    method_t* methods;
    size_t methods_count;

    // list of all the types
    type_t* types;
    size_t types_count;

    // static data, all allocated on the heap
    char* strings;
    size_t strings_size;
    char* us;
    size_t us_size;
    uint8_t* blob;
    size_t blob_size;
    guid_t* guids;
    size_t guids_count;
};
