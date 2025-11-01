#include "native.h"

#include "../function.h"
#include "../type.h"
#include "dotnet/loader.h"
#include "dotnet/types.h"
#include "tomatodotnet/tdn.h"
#include "tomatodotnet/types/type.h"

void* jit_get_native_method(RuntimeMethodInfo info) {
    // only go here for native implemented methods
    if (info->MethodImplFlags.CodeType != TDN_METHOD_IMPL_CODE_TYPE_NATIVE) {
        return NULL;
    }

    // only if its the corelib, or we are before the corelib was initialized
    if (gCoreAssembly != NULL && info->Module->Assembly != gCoreAssembly) {
        return NULL;
    }

    size_t native_functions_count = __stop_native_function_descriptors - __start_native_function_descriptors;

    // search for a native method that implements this
    // TODO: type check of the arguments
    RuntimeTypeInfo type = info->DeclaringType;
    for (int i = 0; i < native_functions_count; i++) {
        native_function_t* func = &__start_native_function_descriptors[i];
        const char* namespace = func->namespace;
        if (namespace != NULL && namespace[0] == '\0') namespace = NULL;
        if (!tdn_compare_string_to_cstr(type->Namespace, namespace)) continue;
        if (!tdn_compare_string_to_cstr(type->Name, func->class)) continue;
        if (!tdn_compare_string_to_cstr(info->Name, func->name)) continue;

        size_t arg_count = 0;
        for (jit_stack_value_kind_t arg = func->args[0]; arg != JIT_KIND_UNKNOWN; arg = func->args[++arg_count]) {
            if (info->Parameters->Length < arg_count) {
                break;
            }

            // compare the kind
            jit_stack_value_kind_t kind = jit_get_type_kind(info->Parameters->Elements[arg_count]->ParameterType);
            if (kind != func->args[arg_count]) {
                break;
            }
        }
        if (info->Parameters->Length != arg_count) {
            continue;
        }

        return func->function;
    }

    return NULL;
}
