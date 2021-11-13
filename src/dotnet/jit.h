#pragma once

#include "assembly.h"

#include <mir-gen.h>
#include <mir.h>

typedef struct jit_instance {
    MIR_context_t context;
} jit_instance_t;

err_t jit_prepare_assembly(jit_instance_t* instance, assembly_t* assembly);
