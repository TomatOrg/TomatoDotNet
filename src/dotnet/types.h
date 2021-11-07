#pragma once

#include <util/defs.h>

#include <stdint.h>

typedef union token {
    struct {
        uint32_t index : 24;
        uint32_t table : 8;
    };
    uint32_t packed;
} PACKED token_t;

typedef struct guid {
    uint64_t low;
    uint64_t high;
} PACKED guid_t;
