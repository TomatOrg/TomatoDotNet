#pragma once

#include "dotnet/jit/function.h"
#include "util/cpp_magic.h"

#include "tomatodotnet/types/reflection.h"

// helper to define the signature in a nicer way
#define NATIVE_FUNC_MAKE_SIG(x) JIT_KIND_##x
#define NATIVE_FUNC_SIG(...) \
    (const jit_stack_value_kind_t[]){ MAP(NATIVE_FUNC_MAKE_SIG, COMMA, ## __VA_ARGS__, UNKNOWN) }

#define _STR(a) #a
#define STR(a) _STR(a)

#define _MY_CAT(a, b) a ## b
#define MY_CAT(a, b) _MY_CAT(a, b)

#define NATIVE_FUNC(name, ...) \
    __attribute((section("native_function_descriptors"), used)) \
    const native_function_t MY_CAT(CLASS, _##name##_desc) = { \
        STR(NAMESPACE), STR(CLASS), #name, \
        MY_CAT(CLASS, _##name), \
        NATIVE_FUNC_SIG(__VA_ARGS__) \
    }

#define NATIVE_FUNC_OVERLOAD(name, suffix, ...) \
    __attribute((section("native_function_descriptors"), used)) \
    const native_function_t MY_CAT(CLASS, _##name##_##suffix##_desc) = { \
        STR(NAMESPACE), STR(CLASS), #name, \
        MY_CAT(CLASS, _##name##_##suffix), \
        NATIVE_FUNC_SIG(__VA_ARGS__) \
    }

#define NATIVE_FUNC_FOR(namespace, class, name, ...) \
    __attribute((section("native_function_descriptors"), used)) \
    const native_function_t class_##name##_desc = { \
        #namespace, #class, #name, \
        class##_##name, \
        NATIVE_FUNC_SIG(__VA_ARGS__) \
    }

typedef struct native_function {
    const char* namespace;
    const char* class;
    const char* name;
    void* function;
    const jit_stack_value_kind_t* args;
} native_function_t;

extern native_function_t __start_native_function_descriptors[];
extern native_function_t __stop_native_function_descriptors[];

void* jit_get_native_method(RuntimeMethodInfo info);
