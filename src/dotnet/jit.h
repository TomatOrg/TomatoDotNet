#pragma once

#include "assembly.h"
#include "method.h"

#include <mir-gen.h>
#include <mir.h>

typedef struct jit_instance {
    MIR_context_t context;
} jit_instance_t;

err_t jit_prepare_assembly(jit_instance_t* instance, assembly_t* assembly);

err_t jit_mangle_name(method_t* method, char* mangled_name, size_t buffer_size);
