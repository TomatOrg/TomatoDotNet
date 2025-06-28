#include "string_builder.h"

#include "tomatodotnet/util/stb_ds.h"
#include "tomatodotnet/types/type.h"
#include "except.h"

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

void string_builder_push_uint(string_builder_t* builder, size_t a) {
    if (a <= 9) {
        string_builder_push_char(builder, a + '0');
    } else {
        ASSERT(!"TODO");
    }
}

void string_builder_push_char(string_builder_t* builder, char c) {
    arrpush(builder->chars, c);
}

char* string_builder_build(string_builder_t* builder) {
    arrpush(builder->chars, '\0');
    return builder->chars;
}

void string_builder_free(string_builder_t* builder) {
    arrfree(builder->chars);
}

void string_builder_push_type_signature(string_builder_t* builder, RuntimeTypeInfo type) {
    // push the declaring type if any
    if (type->DeclaringType != NULL) {
        string_builder_push_type_signature(builder, type->DeclaringType);
        string_builder_push_char(builder, '+');
    }

    // push the namespace if any
    if (type->Namespace != NULL) {
        string_builder_push_string(builder, type->Namespace);
        string_builder_push_char(builder, '.');
    }

    // push the type name
    string_builder_push_string(builder, type->Name);

    // push the generic signature if any
    if (type->GenericArguments != NULL) {
        string_builder_push_char(builder, '[');
        for (int i = 0; i < type->GenericArguments->Length; i++) {
            string_builder_push_type_signature(builder, type->GenericArguments->Elements[i]);
            if (i != type->GenericArguments->Length - 1) {
                string_builder_push_char(builder, ',');
            }
        }
        string_builder_push_char(builder, ']');
    }
}

void string_builder_push_method_signature(string_builder_t* builder, RuntimeMethodBase method, bool full) {
    // push the return type
    string_builder_push_type_signature(builder, method->ReturnParameter->ParameterType);
    string_builder_push_char(builder, ' ');

    // if we want a full signature push the declaring type of the method
    if (full) {
        string_builder_push_type_signature(builder, method->DeclaringType);
        string_builder_push_char(builder, ':');
        string_builder_push_char(builder, ':');
    }

    // push the method name
    string_builder_push_string(builder, method->Name);

    // push the generic signature if any
    if (method->GenericArguments != NULL) {
        string_builder_push_char(builder, '[');
        for (int i = 0; i < method->GenericArguments->Length; i++) {
            string_builder_push_type_signature(builder, method->GenericArguments->Elements[i]);
            if (i != method->GenericArguments->Length - 1) {
                string_builder_push_char(builder, ',');
            }
        }
        string_builder_push_char(builder, ']');
    }

    // push the parameter types
    string_builder_push_char(builder, '(');
    for (int i = 0; i < method->Parameters->Length; i++) {
        string_builder_push_type_signature(builder, method->Parameters->Elements[i]->ParameterType);
        if (i != method->Parameters->Length - 1) {
            string_builder_push_char(builder, ',');
            string_builder_push_char(builder, ' ');
        }
    }
    string_builder_push_char(builder, ')');
}

