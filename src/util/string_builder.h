#pragma once

#include "tinydotnet/types/basic.h"

typedef struct string_builder {
    char* chars;
} string_builder_t;

void string_builder_push_string(string_builder_t* builder, String str);
void string_builder_push_cstr(string_builder_t* builder, const char* str);
char* string_builder_build(string_builder_t* builder);
void string_builder_free(string_builder_t* builder);
