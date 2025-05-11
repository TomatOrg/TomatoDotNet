#pragma once

#include <stdint.h>
#include <util/defs.h>

#define GC_PAGE_SHIFT               (13)
#define GC_MAX_SIZE                 (SIZE_256KB)
#define GC_MAX_CPU_CACHE_SIZE       (1572864) // 1.5 MB
#define GC_STEAL_AMOUNT             (1 << 16)
#define GC_NUM_CLASSES              (85)
#define GC_PAGE_SIZE                (1 << GC_PAGE_SHIFT)
#define GC_ALIGNMENT                (8)

#define GC_LARGE_SIZE               1024
#define GC_LARGE_SIZE_ALIGNMENT     128

void gc_size_class_init();

int gc_get_size_class(size_t size, size_t align);
