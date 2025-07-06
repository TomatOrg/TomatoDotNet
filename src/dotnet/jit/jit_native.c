#include "jit_native.h"

#include "dotnet/loader.h"
#include "tomatodotnet/types/type.h"

typedef struct native_function {
    const char* namespace;
    const char* class;
    const char* name;
    void* function;
} native_function_t;

static double math_sqrt_double(double value) {
    return __builtin_sqrt(value);
}

static native_function_t m_native_functions[] = {
    { "System", "Math", "Sqrt", math_sqrt_double },
};

void* jit_get_native_method(RuntimeMethodInfo info) {
    // only go here for native implemented methods
    if (info->MethodImplFlags.CodeType != TDN_METHOD_IMPL_CODE_TYPE_NATIVE) {
        return NULL;
    }

    // only if its the corelib
    if (info->Module->Assembly != gCoreAssembly) {
        return NULL;
    }

    // search for a native method that implements this
    // TODO: type check of the arguments
    RuntimeTypeInfo type = info->DeclaringType;
    for (int i = 0; i < ARRAY_LENGTH(m_native_functions); i++) {
        if (!tdn_compare_string_to_cstr(type->Namespace, m_native_functions[i].namespace)) continue;
        if (!tdn_compare_string_to_cstr(type->Name, m_native_functions[i].class)) continue;
        if (!tdn_compare_string_to_cstr(info->Name, m_native_functions[i].name)) continue;
        return m_native_functions[i].function;
    }

    return NULL;
}
