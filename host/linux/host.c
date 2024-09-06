
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "tomatodotnet/host.h"

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

void* tdn_host_gc_alloc(size_t size) {
    return calloc(1, size);
}

void tdn_host_gc_register_root(void* root) {
    (void)root;
}
