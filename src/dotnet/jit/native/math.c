#include "native.h"

#define NAMESPACE   System
#define CLASS       Math

static double Math_Sqrt(double value) {
    return __builtin_sqrt(value);
}
NATIVE_FUNC(Sqrt, F64);

static double Math_Cos(double value) {
    return __builtin_cos(value);
}
NATIVE_FUNC(Cos, F64);

static double Math_Ceiling(double value) {
    return __builtin_ceil(value);
}
NATIVE_FUNC(Ceiling, F64);
