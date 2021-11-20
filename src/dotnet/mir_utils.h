#pragma once

#include <mir.h>

MIR_item_t mir_get_func(MIR_context_t ctx, const char* name);

MIR_item_t mir_get_forward(MIR_context_t ctx, const char* name);

MIR_item_t mir_get_proto(MIR_context_t ctx, const char* name);

MIR_item_t mir_get_import(MIR_context_t ctx, const char* name);

MIR_item_t mir_get_bss(MIR_context_t ctx, const char* name);
