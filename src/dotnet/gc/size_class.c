#include "size_class.h"

#include "util/except.h"

#define GC_CLASS_ARRAY_SIZE (((GC_MAX_SIZE + 127 + (120 << 7)) >> 7) + 1)

/**
 * Performs a lookup from a size to a class index, dynamically generated
 */
static uint8_t m_class_array[GC_CLASS_ARRAY_SIZE] = {};

/**
 * A lookup from a class index, to a size
 */
static uint32_t m_class_to_size[] = {
        0,     8,    16,    32,    64,    80,    96,   112,   128,   144,   160,   176,   192,   208,   224,   240,   256,
      272,   288,   304,   320,   336,   352,   368,   384,   400,   416,   448,   480,   512,   576,   640,   704,   768,
      896,  1024,  1152,  1280,  1408,  1536,  1792,  2048,  2304,  2688,  2816,  3200,  3456,  3584,  4096,  4736,  5376,
     6144,  6528,  7168,  8192,  9472, 10240, 12288, 13568, 14336, 16384, 20480, 24576, 28672, 32768, 40960, 49152, 57344,
    65536, 73728, 81920, 90112, 98304,106496,114688,131072,139264,147456,155648,172032,188416,204800,221184,237568,262144,
};
STATIC_ASSERT(ARRAY_LENGTH(m_class_to_size) == GC_NUM_CLASSES);

static inline int get_class_index(size_t s) {
    if (s <= GC_LARGE_SIZE) {
        return (int)((s + 7) >> 3);
    } else if (s <= GC_MAX_SIZE) {
        return (int)((s + 127 + (120 << 7)) >> 7);
    } else {
        return -1;
    }
}

void gc_size_class_init(void) {
    uint32_t next_size = 0;
    for (uint32_t c = 1; c < GC_NUM_CLASSES; c++) {
        uint32_t max_size_in_class = m_class_to_size[c];

        for (uint32_t s = next_size; s <= max_size_in_class; s += GC_ALIGNMENT) {
            int idx = get_class_index(s);
            ASSERT(idx >= 0);
            m_class_array[idx] = c;
        }

        next_size = max_size_in_class + GC_ALIGNMENT;
        if (next_size > GC_MAX_SIZE) {
            break;
        }
    }
}

static inline size_t class_to_size(size_t size_class) {
    ASSERT(size_class < GC_NUM_CLASSES);
    return m_class_to_size[size_class];
}

int gc_get_size_class(size_t size, size_t align) {
    ASSERT(align != 0);

    if (UNLIKELY(align > GC_PAGE_SIZE)) {
        return -1;
    }

    int index = get_class_index(size);
    if (UNLIKELY(index < 0)) {
        return -1;
    }

    int size_class = m_class_array[index];
    if (UNLIKELY(class_to_size(size_class) & (align - 1))) {
        do {
            ++size_class;
        } while (UNLIKELY(class_to_size(size_class) & (align - 1)));
    }

    return size_class;
}