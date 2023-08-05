#pragma once

#include <stddef.h>

// logging helpers
void tdn_host_log_trace(const char* format, ...);
void tdn_host_log_warn(const char* format, ...);
void tdn_host_log_error(const char* format, ...);

// raw logging
void tdn_host_printf(const char* format, ...);

// special string functions
size_t tdn_host_strnlen(const char* string, size_t maxlen);

// memory allocation
void* tdn_host_mallocz(size_t size);
void* tdn_host_realloc(void* ptr, size_t new_size);
void tdn_host_free(void* ptr);

// file management
typedef void* tdn_file_t;

/**
 * Resolve an assembly and open it as a file
 */
int tdn_resolve_assembly(const char* name, tdn_file_t* out_file);

/**
 * Read a file opened by tdn_resolve_assembly
 */
int tdn_read_file(tdn_file_t file, size_t offset, size_t size, void* buffer);

/**
 * Close a file returned by tdn_resolve_assembly
 */
int tdn_close_file(tdn_file_t file);

/**
 * Request the host to turn an error it returned into a
 * proper human readable string
 */
const char* tdn_host_error_to_string(int error);
