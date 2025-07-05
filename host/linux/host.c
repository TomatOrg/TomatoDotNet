
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include "tomatodotnet/host.h"

#include <linux/limits.h>
#include <sys/mman.h>

#include <util/except.h>
#include "tomatodotnet/util/stb_ds.h"

#include "tomatodotnet/tdn.h"

#if 1
    #define STDERR stderr
#else
    #define STDERR stdout
#endif

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
    fprintf(STDERR, "[-] ");
    va_list va;
    va_start(va, format);
    vfprintf(STDERR, format, va);
    va_end(va);
    fprintf(STDERR, "\n");
    fflush(STDERR);
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

void* tdn_host_mallocz(size_t size, size_t align) {
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

const char* g_assembly_search_path = NULL;

bool tdn_host_resolve_assembly(const char* name, uint16_t revision, tdn_file_t* out_file) {
    // attempt to search for the file
    char buffer[PATH_MAX] = {};
    strcat(buffer, g_assembly_search_path);
    if (g_assembly_search_path[strlen(buffer) - 1] != '/') {
        strcat(buffer, "/");
    }
    strcat(buffer, name);
    strcat(buffer, ".dll");

    FILE* file = fopen(buffer, "rb");
    if (file == NULL) {
        return false;
    }

    *out_file = file;
    return true;
}

tdn_err_t tdn_host_read_file(void* file, size_t offset, size_t size, void* buffer) {
    if (fseek(file, (long)offset, SEEK_SET) != 0)
        return -errno;
    if (fread(buffer, size, 1, file) != 1)
        return -errno;
    return TDN_NO_ERROR;
}

void tdn_host_close_file(void* file) {
    fclose(file);
}

const char* tdn_host_error_to_string(int error) {
    return strerror(error);
}

void* tdn_host_jit_alloc(size_t size) {
    void* ptr = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    if (ptr == MAP_FAILED) {
        return NULL;
    }
    return ptr;
}

void tdn_host_jit_set_exec(void* ptr, size_t size) {
    ASSERT(mprotect(ptr, size, PROT_READ | PROT_EXEC) == 0);
}

void tdn_host_jit_patch(void* dst, void* src, size_t size) {
    ASSERT(mprotect(dst, size, PROT_READ | PROT_WRITE | PROT_EXEC) == 0);
    memcpy(dst, src, size);
    ASSERT(mprotect(dst, size, PROT_READ | PROT_EXEC) == 0);
}

static int m_dump_spidir_count = 0;
static int m_dump_elf_count = 0;

void* tdn_host_jit_start_dump(tdn_jit_dump_type_t type) {
    char name[256];
    if (type == TDN_JIT_DUMP_SPIDIR) {
        snprintf(name, sizeof(name), "test.%d.spidir", m_dump_spidir_count++);
    } else {
        snprintf(name, sizeof(name), "test.%d.elf", m_dump_elf_count++);
    }
    return fopen(name, "w");
}

void tdn_host_jit_end_dump(void* ctx) {
    fclose(ctx);
}

spidir_dump_status_t tdn_host_jit_dump_callback(const char* data, size_t size, void* ctx) {
    fwrite(data, size, 1, ctx);
    return SPIDIR_DUMP_CONTINUE;
}
