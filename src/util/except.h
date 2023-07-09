#pragma once

#include <tinydotnet/except.h>
#include "defs.h"

#define TRACE(fmt, ...) tdn_host_log_trace(fmt, ## __VA_ARGS__)
#define WARN(fmt, ...) tdn_host_log_warn(fmt, ## __VA_ARGS__)
#define ERROR(fmt, ...) tdn_host_log_error(fmt, ## __VA_ARGS__)

#define IS_ERROR(x) ((x) != TDN_NO_ERROR)

void dump_hex(const void* data, size_t size);

#define DEFAULT_ERROR TDN_ERROR_CHECK_FAILED
#define DEFAULT_LABEL cleanup

#define CHECK_ERROR_LABEL(expr, error, label, ...) \
    do { \
        if (!(expr)) { \
            err = error; \
            ERROR("Check `%s` failed with error `%s` at %s (%s:%d)", #expr, tdn_get_error_string(err), __FUNCTION__, __FILE__, __LINE__); \
            __builtin_debugtrap(); \
            goto label; \
        } \
    } while (0)

#define CHECK_ERROR(expr, error, ...)                   CHECK_ERROR_LABEL(expr, error, DEFAULT_LABEL, ## __VA_ARGS__)
#define CHECK_LABEL(expr, label, ...)                   CHECK_ERROR_LABEL(expr, DEFAULT_ERROR, label, ## __VA_ARGS__)
#define CHECK(expr, ...)                                CHECK_ERROR_LABEL(expr, DEFAULT_ERROR, DEFAULT_LABEL, ## __VA_ARGS__)

#define CHECK_FAIL_ERROR_LABEL(error, label, ...)       CHECK_ERROR_LABEL(false, error, label, ## __VA_ARGS__)
#define CHECK_FAIL_ERROR(error, ...)                    CHECK_ERROR_LABEL(false, error, DEFAULT_LABEL, ## __VA_ARGS__)
#define CHECK_FAIL_LABEL(label, ...)                    CHECK_ERROR_LABEL(false, DEFAULT_ERROR, label, ## __VA_ARGS__)
#define CHECK_FAIL(...)                                 CHECK_ERROR_LABEL(false, DEFAULT_ERROR, DEFAULT_LABEL, ## __VA_ARGS__)

#define CHECK_AND_RETHROW_LABEL(expr, label) \
    do { \
        err = expr; \
        if (IS_ERROR(err)) { \
            ERROR("\trethrown at %s (%s:%d)", __FUNCTION__, __FILE__, __LINE__); \
            goto label; \
        } \
    } while (0)

#define CHECK_AND_RETHROW(expr) CHECK_AND_RETHROW_LABEL(expr, DEFAULT_LABEL)