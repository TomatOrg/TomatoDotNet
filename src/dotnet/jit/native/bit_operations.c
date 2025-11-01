#include "native.h"

#define NAMESPACE   System.Numerics
#define CLASS       BitOperations

static uint32_t BitOperations_LeadingZeroCount_i32(uint32_t value) {
    return __builtin_clz(value | 1);
}
NATIVE_FUNC_OVERLOAD(LeadingZeroCount, i32, INT32);

static uint32_t BitOperations_LeadingZeroCount_i64(uint64_t value) {
    return __builtin_clzll(value | 1);;
}
NATIVE_FUNC_OVERLOAD(LeadingZeroCount, i64, INT64);
