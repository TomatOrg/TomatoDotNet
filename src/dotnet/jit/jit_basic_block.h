#pragma once

#include <stdint.h>
#include <tomatodotnet/except.h>

#include "jit_internal.h"

tdn_err_t jit_find_basic_blocks(jit_method_t* method);
