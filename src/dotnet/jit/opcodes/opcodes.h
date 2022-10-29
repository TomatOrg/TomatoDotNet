#pragma once

#include "../jit_internal.h"
#include "util/except.h"

// call.c
err_t jit_emit_call(jit_method_context_t* ctx, opcode_t opcode);

// exception.c
err_t jit_emit_leave(jit_method_context_t* ctx);
err_t jit_emit_endfinally(jit_method_context_t* ctx);
err_t jit_emit_endfilter(jit_method_context_t* ctx);
err_t jit_emit_throw(jit_method_context_t* ctx);
err_t jit_emit_rethrow(jit_method_context_t* ctx);
