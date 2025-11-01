#include "native.h"
#include "util/except.h"

#define NAMESPACE   System.Diagnostics
#define CLASS       DebugProvider

static void DebugProvider_WriteCore(String message) {
    tdn_host_printf("%U", message);
}
NATIVE_FUNC(WriteCore, OBJ_REF);

static void DebugProvider_FailCore(String stackTrace, String message, String detailMessage, String errorSource) {
    ASSERT(!"Debug Assertion Failed", "%U - %U - %U", message, detailMessage, errorSource);
}
NATIVE_FUNC(FailCore, OBJ_REF, OBJ_REF, OBJ_REF, OBJ_REF);
