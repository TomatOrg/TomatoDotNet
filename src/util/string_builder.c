#include "string_builder.h"
#include "stb_ds.h"

void string_builder_push_string(string_builder_t* builder, String str) {
    for (size_t i = 0; i < str->Length; i++) {
        // TODO: utf16 to utf8 correctly
        arrpush(builder->chars, (char)str->Chars[i]);
    }
}

void string_builder_push_cstr(string_builder_t* builder, const char* str) {
    while (*str != '\0') {
        arrpush(builder->chars, *str);
        str++;
    }
}

char* string_builder_build(string_builder_t* builder) {
    arrpush(builder->chars, '\0');
    return builder->chars;
}

void string_builder_free(string_builder_t* builder) {
    arrfree(builder->chars);
}
