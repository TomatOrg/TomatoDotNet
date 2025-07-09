#pragma once

#include <stddef.h>
#include <stdarg.h>
#include <stdint.h>
#include <spidir/module.h>

#include "except.h"
#include "types/basic.h"

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
void* tdn_host_mallocz(size_t size, size_t align);
void* tdn_host_realloc(void* ptr, size_t new_size);
void tdn_host_free(void* ptr);

//----------------------------------------------------------------------------------------------------------------------
// Jit related allocations
//----------------------------------------------------------------------------------------------------------------------

/**
 * Request new memory where we will place the jitted code inside of
 *
 * At this point the region should be read-write
 */
void* tdn_host_jit_alloc(size_t size);

/**
 * Turn a jit allocation into rx, the region does not need to be writable anymore
 */
void tdn_host_jit_set_exec(void* ptr, size_t size);

/**
 * Perform a patch on the code, can be called after set_exec was set
 */
void tdn_host_jit_patch(void* dst, void* src, size_t size);

//----------------------------------------------------------------------------------------------------------------------
// GC allocation primitives
//----------------------------------------------------------------------------------------------------------------------

/**
 * Request memory meant for garbage collection
 *
 * Memory must be set to zero before you return
 * The vtable must be set as the vtable given to the function.
 *
 * It is allowed to return NULL
 */
Object tdn_host_gc_alloc(ObjectVTable* vtable, size_t size, size_t alignment);

/**
 * Called by the runtime to tell the host about the objects inside of another object
 * The parent is optional and is the object that contains the new object
 */
void tdn_host_gc_trace_object(Object parent, Object obj);

/**
 * Called to start a gc cycle, this should stop the world,
 * and call the scanning functions to start the mark and sweep.
 */
void tdn_host_gc_start(void);

/**
 * Register a new root for garbage collection
 */
void tdn_host_gc_register_root(void* root);

/**
 * Remove a root for garbage collection
 */
void tdn_host_gc_unregister_root(void* root);

//----------------------------------------------------------------------------------------------------------------------
// Jit spidir dump
//----------------------------------------------------------------------------------------------------------------------

typedef enum tdn_jit_dump_type {
    TDN_JIT_DUMP_SPIDIR,
    TDN_JIT_DUMP_SYMBOLS_ELF,
} tdn_jit_dump_type_t;

// used for debugging the jit, will dump spidir modules
// using these functions
void* tdn_host_jit_start_dump(tdn_jit_dump_type_t type);
void tdn_host_jit_end_dump(void* ctx);
spidir_dump_status_t tdn_host_jit_dump_callback(const char* data, size_t size, void* ctx);

//----------------------------------------------------------------------------------------------------------------------
// File access API
//----------------------------------------------------------------------------------------------------------------------

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
tdn_err_t tdn_host_read_file(tdn_file_t file, size_t offset, size_t size, void* buffer);

/**
 * Close a file returned by tdn_host_resolve_assembly
 */
void tdn_host_close_file(tdn_file_t file);

/**
 * Request the host to turn an error it returned into a
 * proper human readable string
 */
const char* tdn_host_error_to_string(int error);
