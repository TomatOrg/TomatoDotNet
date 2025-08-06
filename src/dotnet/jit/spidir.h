#pragma once

#include "spidir/module.h"
#include "tomatodotnet/types/reflection.h"

spidir_value_type_t* jit_get_spidir_arg_types(RuntimeMethodBase method);
spidir_value_type_t jit_get_spidir_ret_type(RuntimeMethodBase method);
spidir_value_type_t jit_get_spidir_type(RuntimeTypeInfo type);
