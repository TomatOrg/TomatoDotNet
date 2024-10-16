
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "tomatodotnet/host.h"

#include <sys/mman.h>

#include <util/except.h>

void tdn_host_log_trace(const char* format, ...) {
    printf("[*] ");
    va_list va;
    va_start(va, format);
    vprintf(format, va);
    va_end(va);
    printf("\n");
}

void tdn_host_log_warn(const char* format, ...) {
    printf("[!] ");
    va_list va;
    va_start(va, format);
    vprintf(format, va);
    va_end(va);
    printf("\n");
}

void tdn_host_log_error(const char* format, ...) {
    printf("[-] ");
    va_list va;
    va_start(va, format);
    vprintf(format, va);
    va_end(va);
    printf("\n");
}

void tdn_host_printf(const char* format, ...) {
    va_list va;
    va_start(va, format);
    vprintf(format, va);
    va_end(va);
    fflush(stdout);
}

void tdn_host_vprintf(const char* format, va_list args) {
    vprintf(format, args);
}

void* tdn_host_mallocz(size_t size) {
    void* ptr = malloc(size);
    if (ptr == NULL) return NULL;
    memset(ptr, 0, size);
    return ptr;
}

void* tdn_host_realloc(void* ptr, size_t size) {
    return realloc(ptr, size);
}

void tdn_host_free(void* ptr) {
    free(ptr);
}

static void* m_low_memory = NULL;

void* tdn_host_mallocz_low(size_t size) {
    if (m_low_memory == NULL) {
        m_low_memory = mmap((void*)BASE_2GB, SIZE_2GB, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE | MAP_FIXED, -1, 0);
        ASSERT(m_low_memory != MAP_FAILED);
        ASSERT((uintptr_t)m_low_memory == BASE_2GB);
    }
    void* ptr = m_low_memory;
    m_low_memory += ALIGN_UP(size, 8);
    return ptr;
}

void tdn_host_free_low(void* ptr) {

}

size_t tdn_host_strnlen(const char* string, size_t maxlen) {
    return strnlen(string, maxlen);
}

int tdn_host_resolve_assembly(const char* name, uint16_t revision, tdn_file_t* out_file) {
    FILE* file = fopen(name, "rb");
    if (file == NULL) {
        return errno;
    }
    *out_file = file;
    return 0;
}

int tdn_host_read_file(tdn_file_t file, size_t offset, size_t size, void* buffer) {
    if (fseek(file, (long)offset, SEEK_SET) != 0)
        return errno;
    if (fread(buffer, size, 1, file) != 1)
        return errno;
    return 0;
}

void tdn_host_close_file(tdn_file_t file) {
    fclose(file);
}

const char* tdn_host_error_to_string(int error) {
    return strerror(error);
}

void* tdn_host_gc_alloc(size_t size, size_t alignment) {
    return calloc(1, size);
}

void tdn_host_gc_register_root(void* root) {
    (void)root;
}

void tdn_host_gc_pin_object(void* object) {

}

void* tdn_host_map(size_t size) {
    void* ptr = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (ptr == MAP_FAILED) {
        return NULL;
    }
    return ptr;
}

void tdn_host_map_rx(void* ptr, size_t size) {
    mprotect(ptr, size, PROT_READ | PROT_EXEC);
}
