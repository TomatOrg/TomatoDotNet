#pragma once

#include <stddef.h>

#include "types/reflection.h"
#include "except.h"
#include "host.h"

typedef struct tdn_config {
    /**
     * Trace the opcodes while verifying the IL
     */
    bool jit_verify_trace;

    /**
     * Trace the opcodes while emitting the IL
     */
    bool jit_emit_trace;

    /**
     * The spidir log level to use
     */
    int jit_spidir_log_level;

    /**
     * Dump the spidir after finishing
     */
    bool jit_spidir_dump;

    /**
     * Should we run optimization
     */
    bool jit_optimize;

    /**
     * Should we allow inline
     */
    bool jit_inline;
} tdn_config_t;

/**
 * Get the Tomato.NET config, you are allowed to modify it at startup
 */
tdn_config_t* tdn_get_config(void);

/**
 * Load assembly from a memory buffer
 */
tdn_err_t tdn_load_assembly_from_memory(const void* buffer, size_t buffer_size, RuntimeAssembly* assembly);

/**
 * Load an assembly from a file handle
 */
tdn_err_t tdn_load_assembly_from_file(tdn_file_t file, RuntimeAssembly* assembly);

/**
 * Cleanup everything from the runtime's global state
 */
void tdn_cleanup(void);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Object allocation
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * The total size needs to be reserved for the heap
 */
#define TDN_GC_HEAP_SIZE        ((512ull * 1024ull * 1024ull * 1024ull) * 85ull)

/**
 * Initialize the jit, requires TDN_GC_HEAP_SIZE bytes from the given address, the memory
 * should be reserved and mapped on demand
 */
void tdn_init_gc(void* base_address);

/**
 * Trigger a garbage collection
 */
void tdn_gc_start(void);

/**
 * The raw gc allocation function, this will possibly
 * perform a collection
 */
void* tdn_gc_new(RuntimeTypeInfo type, size_t size);

/**
 * Scan a stack conservatively, this can be called from multiple threads
 * to perform a parallel scan
 */
void tdn_gc_scan_stack(void* start, size_t size);

/**
 * Start the root scanning, this can be called from multiple threads
 * to perform a parallel scan
 */
void tdn_gc_scan_roots(void);

/**
 * Perform the sweep, must be called after
 * all gc_scan_roots calls return.
 *
 * returns true if there are finalizers to run
 */
bool tdn_gc_sweep(void);

/**
 * Run finalizers, can be called from multiple threads
 */
void tdn_gc_run_finalizers(void);

/**
 * Allocate a new object
 */
#define TDN_GC_NEW(type) \
    ({ \
        type ___ptr = tdn_gc_new(t##type, sizeof(struct type)); \
        CHECK_ERROR(___ptr != NULL, TDN_ERROR_OUT_OF_MEMORY); \
        ___ptr; \
    })

/**
 * Allocate a new array of the given length
 */
#define TDN_GC_NEW_ARRAY(type, count) \
    ({ \
        size_t ___array_length = (count); \
        RuntimeTypeInfo ___array_type = NULL; \
        CHECK_AND_RETHROW(tdn_get_array_type(t##type, &___array_type)); \
        type##_Array ___ptr = tdn_gc_new(___array_type, ALIGN_UP(sizeof(struct Array), _Alignof(type)) + sizeof(type) * (___array_length)); \
        CHECK_ERROR(___ptr != NULL, TDN_ERROR_OUT_OF_MEMORY); \
        ___ptr->Length = ___array_length; \
        ___ptr; \
    })

/**
 * Allocate a new byte array
 */
#define TDN_GC_NEW_BYTE_ARRAY(count) \
    ({ \
        size_t ___array_length = (count); \
        RuntimeTypeInfo ___array_type = NULL; \
        CHECK_AND_RETHROW(tdn_get_array_type(tByte, &___array_type)); \
        Byte_Array ___ptr = tdn_gc_new(___array_type, sizeof(struct Array) + (___array_length)); \
        CHECK_ERROR(___ptr != NULL, TDN_ERROR_OUT_OF_MEMORY); \
        ___ptr->Length = ___array_length; \
        ___ptr; \
    })
