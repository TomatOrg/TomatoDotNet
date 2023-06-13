
#include "tinydotnet/except.h"
#include "tinydotnet/types/basic.h"
#include "util/string.h"
#include "dotnet/gc/gc.h"
#include "util/except.h"

tdn_err_t tdn_create_string_from_cstr(const char* cstr, System_String* out_str) {
    tdn_err_t err = TDN_NO_ERROR;

    // count how many chars it has
    size_t len = strlen(cstr);
    CHECK(len <= INT32_MAX);

    // allocate it
    System_String new_str = gc_new(tSystem_String, sizeof(struct System_String) + len * 2);
    CHECK_ERROR(new_str, TDN_ERROR_OUT_OF_MEMORY);
    new_str->Length = (int)len;

    // TODO: do interning assuming that cstr come only from the binary and common
    //       stuff like namespace will arrive alot

    // and finally fill it
    for (int i = 0; i < len; i++) {
        new_str->Chars[i] = (System_Char)cstr[i];
    }

    // output it
    *out_str = new_str;

cleanup:
    return err;
}

tdn_err_t tdn_append_cstr_to_string(System_String str, const char* cstr, System_String* out_str) {
    tdn_err_t err = TDN_NO_ERROR;

    // count how many chars it has
    size_t len = strlen(cstr);
    CHECK(len <= INT32_MAX);
    CHECK(len + str->Length <= INT32_MAX);

    if (len == 0) {
        *out_str = str;
        goto cleanup;
    }

    // allocate it
    System_String new_str = gc_new(tSystem_String, sizeof(struct System_String) + (str->Length + len) * 2);
    CHECK_ERROR(new_str, TDN_ERROR_OUT_OF_MEMORY);
    new_str->Length = str->Length + (int)len;

    // copy the old data
    memcpy(new_str->Chars, str->Chars, str->Length * sizeof(System_Char));

    // and finally fill it
    for (int i = 0; i < len; i++) {
        new_str->Chars[str->Length + i] = (System_Char)cstr[i];
    }

    // output it
    *out_str = new_str;

cleanup:
    return err;
}

bool tdn_compare_string_to_cstr(System_String str, const char* cstr) {
    if (cstr == NULL && str == NULL) {
        return true;
    }

    if (cstr == NULL || str == NULL) {
        return false;
    }

    int len = strlen(cstr);
    if (str->Length != len) {
        return false;
    }

    for (int i = 0; i < len; i++) {
        if (str->Chars[i] != cstr[i]) {
            return false;
        }
    }

    return true;
}
