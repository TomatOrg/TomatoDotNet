#pragma once

#include <stddef.h>
#include <stdarg.h>
#include <stdint.h>
#include <spidir/module.h>

// logging helpers
void tdn_host_log_trace(const char* format, ...);
void tdn_host_log_warn(const char* format, ...);
void tdn_host_log_error(const char* format, ...);

// raw logging
void tdn_host_printf(const char* format, ...);
void tdn_host_vprintf(const char* format, va_list args);

// special string functions
size_t tdn_host_strnlen(const char* string, size_t maxlen);

// memory allocation
void* tdn_host_mallocz(size_t size);
void* tdn_host_realloc(void* ptr, size_t new_size);
void tdn_host_free(void* ptr);

// allocate memory that is located between 2gb and 4gb
void* tdn_host_mallocz_low(size_t size);
void tdn_host_free_low(void* ptr);

/**
 * Request a new mapping, the mapping starts as read-write
 */
void* tdn_host_map(size_t size);

/**
 * Turn a mapping into read-execute only region
 */
void tdn_host_map_rx(void* ptr, size_t size);

// gc operation
void* tdn_host_gc_alloc(size_t size, size_t alignment);
void tdn_host_gc_register_root(void* root);
void tdn_host_gc_pin_object(void* object);

// used for debugging the jit, will dump spidir modules
// using these functions
// TODO: ugly
void* tdn_host_jit_start_dump(void);
void tdn_host_jit_end_dump(void* ctx);
spidir_dump_status_t tdn_host_jit_dump_callback(const char* data, size_t size, void* ctx);

// file management
typedef void* tdn_file_t;

/**
 * Resolve an assembly and open it as a file, the major version of the assembly must match
 * the requested one, and the highest minor should be given for best result
 */
bool tdn_host_resolve_assembly(const char* name, uint16_t major_version, tdn_file_t* out_file);

/**
 * Read a file opened by tdn_host_resolve_assembly
 */
int tdn_host_read_file(tdn_file_t file, size_t offset, size_t size, void* buffer);

/**
 * Close a file returned by tdn_host_resolve_assembly
 */
void tdn_host_close_file(tdn_file_t file);

/**
 * Request the host to turn an error it returned into a
 * proper human readable string
 */
const char* tdn_host_error_to_string(int error);
