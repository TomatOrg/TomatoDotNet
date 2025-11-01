#include "native.h"
#include <stdatomic.h>

#define NAMESPACE   System.Threading
#define CLASS       Interlocked

static uint32_t Interlocked_CompareExchange_int32(_Atomic(uint32_t)* location1, uint32_t value, uint32_t comparand) {
    atomic_compare_exchange_strong(location1, &comparand, value);
    return comparand;
}
NATIVE_FUNC_OVERLOAD(CompareExchange, int32, BY_REF, INT32, INT32);

static uint64_t Interlocked_CompareExchange_int64(_Atomic(uint64_t)* location1, uint64_t value, uint64_t comparand) {
    atomic_compare_exchange_strong(location1, &comparand, value);
    return comparand;
}
NATIVE_FUNC_OVERLOAD(CompareExchange, int64, BY_REF, INT64, INT64);

static Object Interlocked_CompareExchange_obj(_Atomic(Object)* location1, Object value, Object comparand) {
    // TODO: we might need some write barrier in here
    atomic_compare_exchange_strong(location1, &comparand, value);
    return comparand;
}
NATIVE_FUNC_OVERLOAD(CompareExchange, obj, BY_REF, OBJ_REF, OBJ_REF);

static float Interlocked_CompareExchange_f32(_Atomic(float)* location1, float value, float comparand) {
    atomic_compare_exchange_strong(location1, &comparand, value);
    return comparand;
}
NATIVE_FUNC_OVERLOAD(CompareExchange, f32, BY_REF, F32, F32);

static double Interlocked_CompareExchange_f64(_Atomic(double)* location1, double value, double comparand) {
    atomic_compare_exchange_strong(location1, &comparand, value);
    return comparand;
}
NATIVE_FUNC_OVERLOAD(CompareExchange, f64, BY_REF, F64, F64);
