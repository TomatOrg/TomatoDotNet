
#include <stddef.h>
#include "tinydotnet/host.h"
#include "util/except.h"

void* spidir_platform_alloc(size_t size, size_t align) {
    if (align <= alignof(max_align_t)) {
        // Get a `realloc`-able pointer where possible
        return tdn_host_mallocz(size);
    } else {
        // TODO: lol
        __builtin_trap();
    }
}

void spidir_platform_free(void* ptr, size_t size, size_t align) {
    (void) size;
    (void) align;
    tdn_host_free(ptr);
}

void* spidir_platform_realloc(void* ptr, size_t old_size, size_t align,
                              size_t new_size) {
    if (align <= alignof(max_align_t)) {
        return tdn_host_realloc(ptr, new_size);
    } else {
        __builtin_trap();
    }
}

void spidir_platform_panic(const char* message, size_t message_len) {
    ERROR("spidir: %.*s", message_len, message);
    __builtin_trap();
}
