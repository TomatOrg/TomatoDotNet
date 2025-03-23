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
 * The raw gc allocation function, this will possibly
 * perform a collection
 */
void* tdn_gc_new(RuntimeTypeInfo type, size_t size);

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

