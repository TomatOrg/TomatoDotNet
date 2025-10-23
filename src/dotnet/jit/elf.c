#include "elf.h"

#include <stdalign.h>

#include "jit.h"
#include "helpers.h"
#include "type.h"
#include "dotnet/types.h"
#include "spidir/x64.h"
#include "tomatodotnet/types/type.h"
#include "tomatodotnet/util/stb_ds.h"
#include "util/defs.h"
#include "util/except.h"
#include "util/string.h"
#include "util/elf64.h"
#include "util/dwarf.h"
#include "util/string_builder.h"


typedef enum object_section {
    SECTION_NULL,
    SECTION_STRTAB,
    SECTION_SHSTRTAB,
    SECTION_TEXT,
    SECTION_RELA_TEXT,
    SECTION_DEBUG_INFO,
    SECTION_RELA_DEBUG_INFO,
    SECTION_DEBUG_ABBREV,
    SECTION_DEBUG_STR,
    SECTION_SYMTAB,
    SECTION_COUNT
} object_section_t;

typedef struct jit_dwarf_type_fixup {
    RuntimeTypeInfo key;
    int* value;
} jit_dwarf_type_fixups_t;

typedef struct jit_elf {
    // all the different sections that we will
    // eventually concat
    void* symtab;
    void* strtab;
    void* shstrtab;
    void* text;
    void* debug_info;
    void* debug_abbrev;
    void* debug_str;
    void* rela_text;

    // symbols defined in the elf
    struct {
        spidir_funcref_t key;
        int value;
    }* symbols;

    // location of namespaces in dwarf
    struct {
        const char* key;
        uint32_t value;
    }* namespaces;

    // queue of types to add dwarf types for
    RuntimeTypeInfo* type_queue;

    // mapping between a type and the dwarf offset
    struct {
        RuntimeTypeInfo key;
        int value;
    }* types;

    // the methods we have, for adding
    // more things like PC values
    struct {
        RuntimeMethodBase key;
        int value;
    }* methods;

    // offsets to fill in the type references
    // once the type is added to the stream
    jit_dwarf_type_fixups_t* type_fixups;
} jit_elf_t;

static jit_elf_t m_elf = {};

static char* strcpy(char* strDest, const char* strSrc) {
    char* temp = strDest;
    while ((*strDest++ = *strSrc++) != '\0');
    return temp;
}

static void mangle_push_string(string_builder_t* builder, String str) {
    int count = 0;
    for (count = 0; count < str->Length; count++) {
        if (str->Chars[count] == '`') {
            break;
        }
    }

    string_builder_push_uint(builder, count);
    for (int i = 0; i < count; i++) {
        string_builder_push_char(builder, str->Chars[i]);
    }
}

static void mangle_push_name(string_builder_t* builder, String str, bool thunk) {
    string_builder_push_uint(builder, str->Length + (thunk ? strlen("$thunk") : 0));
    string_builder_push_string(builder, str);
    if (thunk) {
        string_builder_push_cstr(builder, "$thunk");
    }
}

static void mangle_push_sub_string(string_builder_t* builder, String str, size_t offset, size_t count) {
    string_builder_push_uint(builder, count);
    for (size_t i = offset; i < offset + count; i++) {
        string_builder_push_char(builder, str->Chars[i]);
    }
}

static void mangle_push_namespace(string_builder_t* builder, String str) {
    int start = 0;
    for (int i = 0; i < str->Length; i++) {
        if (str->Chars[i] == '.') {
            mangle_push_sub_string(builder, str, start, i - start);
            start = i + 1;
        }
    }

    // push the last part
    mangle_push_sub_string(builder, str, start, str->Length - start);
}

static void mangle_push_type(string_builder_t* builder, RuntimeTypeInfo type);

static void mangle_push_class(string_builder_t* builder, RuntimeTypeInfo type) {
    string_builder_push_char(builder, 'N');
    if (type->Namespace != NULL) {
        mangle_push_namespace(builder, type->Namespace);
    }
    mangle_push_string(builder, type->Name);
    if (type->GenericArguments != NULL) {
        string_builder_push_char(builder, 'I');
        for (int i = 0; i < type->GenericArguments->Length; i++) {
            mangle_push_type(builder, type->GenericArguments->Elements[i]);
        }
        string_builder_push_char(builder, 'E');
    }
}

static void mangle_push_type(string_builder_t* builder, RuntimeTypeInfo type) {
    if (type == tVoid) string_builder_push_char(builder, 'v');
    else if (type == tChar) string_builder_push_cstr(builder, "Ds");
    else if (type == tSByte) string_builder_push_char(builder, 'a');
    else if (type == tByte) string_builder_push_char(builder, 'h');
    else if (type == tInt16) string_builder_push_char(builder, 's');
    else if (type == tUInt16) string_builder_push_char(builder, 't');
    else if (type == tInt32) string_builder_push_char(builder, 'i');
    else if (type == tUInt32) string_builder_push_char(builder, 'j');
    else if (type == tInt64) string_builder_push_char(builder, 'x');
    else if (type == tUInt64) string_builder_push_char(builder, 'y');
    else if (type == tIntPtr) string_builder_push_char(builder, 'l');
    else if (type == tUIntPtr) string_builder_push_char(builder, 'm');
    else if (type == tSingle) string_builder_push_char(builder, 'f');
    else if (type == tDouble) string_builder_push_char(builder, 'd');
    else if (type == tBoolean) string_builder_push_char(builder, 'b');
    else if (type->IsPointer) {
        string_builder_push_char(builder, 'P');
        mangle_push_type(builder, type->ElementType);
    } else if (type->IsByRef) {
        string_builder_push_char(builder, 'R');
        mangle_push_type(builder, type->ElementType);
    } else {
        if (tdn_type_is_gc_pointer(type)) {
            // classes are considered passed by pointer
            string_builder_push_char(builder, 'P');
        } else {
            // meanwhile structs are considered pass by ref
            string_builder_push_char(builder, 'R');
        }
        mangle_push_class(builder, type);
        string_builder_push_char(builder, 'E');
    }
}

static void mangle_method_name(string_builder_t* builder, RuntimeMethodBase method, bool thunk) {
    RuntimeTypeInfo type = method->DeclaringType;

    // the start of the encoding
    string_builder_push_cstr(builder, "_Z");

    if (type != NULL) {
        // encode the class of the method
        mangle_push_class(builder, type);
    }

    // push the method name
    if (tdn_compare_string_to_cstr(method->Name, ".ctor")) {
        string_builder_push_cstr(builder, "C1E");
    } else {
        mangle_push_name(builder, method->Name, thunk);

        // method generic arguments
        if (method->GenericArguments != NULL) {
            string_builder_push_char(builder, 'I');
            for (int i = 0; i < method->GenericArguments->Length; i++) {
                mangle_push_type(builder, method->GenericArguments->Elements[i]);
            }
            string_builder_push_char(builder, 'E');
        }

        string_builder_push_char(builder, 'E');
    }

    // Parameters
    if (method->Parameters->Length == 0) {
        string_builder_push_char(builder, 'v');
    } else {
        for (int i = 0; i < method->Parameters->Length; i++) {
            mangle_push_type(builder, method->Parameters->Elements[i]->ParameterType);
        }
    }
}

void string_builder_push_cpp_type_name(string_builder_t* builder, RuntimeTypeInfo type) {
    // cut after the `
    for (int i = 0; i < type->Name->Length; i++) {
        if (type->Name->Chars[i] == '`') {
            break;
        } else {
            string_builder_push_char(builder, type->Name->Chars[i]);
        }
    }

    // push the generic signature if any
    if (type->GenericArguments != NULL) {
        string_builder_push_char(builder, '<');
        for (int i = 0; i < type->GenericArguments->Length; i++) {
            RuntimeTypeInfo param = type->GenericArguments->Elements[i];

            // the namespce
            if (param->Namespace != NULL) {
                for (int j = 0; j < param->Namespace->Length; j++) {
                    if (param->Namespace->Chars[j] == '.') {
                        string_builder_push_cstr(builder, "::");
                    } else {
                        string_builder_push_char(builder, param->Namespace->Chars[j]);
                    }
                }
                string_builder_push_cstr(builder, "::");
            }

            // the type name
            string_builder_push_cpp_type_name(builder, param);
            if (i != type->GenericArguments->Length - 1) {
                string_builder_push_cstr(builder, ", ");
            }
        }
        string_builder_push_char(builder, '>');
    }
}


void string_builder_push_cpp_method_name(string_builder_t* builder, RuntimeMethodBase method) {
    // cut after the `
    for (int i = 0; i < method->Name->Length; i++) {
        if (method->Name->Chars[i] == '`') {
            break;
        } else {
            string_builder_push_char(builder, method->Name->Chars[i]);
        }
    }

    // push the generic signature if any
    if (method->GenericArguments != NULL) {
        string_builder_push_char(builder, '<');
        for (int i = 0; i < method->GenericArguments->Length; i++) {
            RuntimeTypeInfo param = method->GenericArguments->Elements[i];

            // the namespce
            if (param->Namespace != NULL) {
                for (int j = 0; j < param->Namespace->Length; j++) {
                    if (param->Namespace->Chars[j] == '.') {
                        string_builder_push_cstr(builder, "::");
                    } else {
                        string_builder_push_char(builder, param->Namespace->Chars[j]);
                    }
                }
                string_builder_push_cstr(builder, "::");
            }

            // the type name
            string_builder_push_cpp_type_name(builder, param);
            if (i != method->GenericArguments->Length - 1) {
                string_builder_push_cstr(builder, ", ");
            }
        }
        string_builder_push_char(builder, '>');
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Dwarf emitting API
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void dwarf_write_u8(void** data, uint8_t val) { *(typeof(val)*)arraddnptr(*data, sizeof(val)) = val; }
static void dwarf_write_u16(void** data, uint16_t val) { *(typeof(val)*)arraddnptr(*data, sizeof(val)) = val; }
static void dwarf_write_u32(void** data, uint32_t val) { *(typeof(val)*)arraddnptr(*data, sizeof(val)) = val; }
static void dwarf_write_u64(void** data, uint64_t val) { *(typeof(val)*)arraddnptr(*data, sizeof(val)) = val; }

static void dwarf_write_uleb(void** data, uint64_t val) {
    do {
        uint8_t b = val & 0x7F;
        val >>= 7;
        if (val != 0) b |= 0x80;
        dwarf_write_u8(data, b);
    } while (val != 0);
}

// static void dwarf_write_sleb(void** data, int64_t val) {
//     bool more = true;
//     while (more) {
//         uint8_t b = val & 0x7F;
//         val >>= 7;
//         if ((val == 0 && (b & 0x40) == 0) || (val == -1 && (b & 0x40))) {
//             more = false;
//         } else {
//             b |= 0x80;
//         }
//         dwarf_write_u8(data, b);
//     }
// }

static void dwarf_write_str(void** data, const char* str) {
    void* str_ptr = arraddnptr(*data, strlen(str) + 1);
    strcpy(str_ptr, str);
}

static void dwarf_write_string(void** data, String str) {
    char* str_ptr = arraddnptr(*data, str->Length + 1);
    for (int i = 0; i < str->Length; i++) {
        str_ptr[i] = str->Chars[i];
    }
    str_ptr[str->Length] = 0;
}

static void dwarf_write_abbrev(jit_elf_t* elf, uint32_t abbrev, uint32_t tag, bool has_children) {
    dwarf_write_uleb(&elf->debug_abbrev, abbrev);
    dwarf_write_uleb(&elf->debug_abbrev, tag);
    dwarf_write_u8(&elf->debug_abbrev, has_children ? DW_CHILDREN_yes : DW_CHILDREN_no);
}

static void dwarf_write_abbrev_attr(jit_elf_t* elf, uint32_t attr, uint32_t form) {
    dwarf_write_uleb(&elf->debug_abbrev, attr);
    dwarf_write_uleb(&elf->debug_abbrev, form);
}

static void dwarf_write_type_ref(jit_elf_t* elf, RuntimeTypeInfo type, bool as_pointer) {
    int32_t offset = as_pointer ? -6 : 0;

    int i = hmgeti(elf->types, type);
    if (i != -1) {
        // we have the type already, emit it and return
        dwarf_write_u32(&elf->debug_info, elf->types[i].value + offset);
        return;
    }

    // get the fixups for that type
    i = hmgeti(elf->type_fixups, type);
    if (i == -1) {
        // no fixups yet, so we also need to add it to the type queue
        arrpush(elf->type_queue, type);

        // and add to the fixups array
        hmput(elf->type_fixups, type, NULL);
        i = hmgeti(elf->type_fixups, type);
    }

    // add the offset and emit a dummy value until we get it
    arrpush(elf->type_fixups[i].value, arrlen(elf->debug_info));
    dwarf_write_u32(&elf->debug_info, offset);
}

static void dwarf_write_member_type(jit_elf_t* elf, RuntimeTypeInfo type) {
    dwarf_write_type_ref(elf, type, tdn_type_is_gc_pointer(type));
}

static void dwarf_write_param_type(jit_elf_t* elf, RuntimeTypeInfo type) {
    if (
        type == tBoolean || type == tChar ||
        type == tSByte || type == tByte ||
        type == tInt16 || type == tUInt16 ||
        type == tInt32 || type == tUInt32 ||
        type == tInt64 || type == tUInt64 ||
        type == tIntPtr || type == tUIntPtr ||
        type == tDouble || type == tSingle ||
        type->BaseType == tEnum || type->IsPointer ||
        type->IsByRef
    ) {
        dwarf_write_type_ref(elf, type, false);
    } else {
        dwarf_write_type_ref(elf, type, true);
    }
}

static void dwarf_write_base_type(jit_elf_t* elf, RuntimeTypeInfo type) {
    dwarf_write_type_ref(elf, type, false);
}

typedef enum tdn_abbrev {
    TDN_ABBREV_COMPILE_UNIT = 1,
    TDN_ABBREV_NAMESPACE,
    TDN_ABBREV_NAMESPACE_EXTENSION,
    TDN_ABBREV_CLASS,
    TDN_ABBREV_FIELD,
    TDN_ABBREV_BASE_TYPE,
    TDN_ABBREV_PRIMITIVE,
    TDN_ABBREV_GENERIC_ARG,
    TDN_ABBREV_POINTER,
    TDN_ABBREV_METHOD,
    TDN_ABBREV_STATIC_METHOD,
    TDN_ABBREV_METHOD_WITH_RETURN,
    TDN_ABBREV_STATIC_METHOD_WITH_RETURN,
    TDN_ABBREV_PARAM,
    TDN_ABBREV_METHOD_LOCATION,
    TDN_ABBREV_STATIC_METHOD_LOCATION,
    TDN_ABBREV_PARAM_LOCATION,
    TDN_ABBREV_ARRAY,
} tdn_abbrev_t;

static void dwarf_start_abbrev(jit_elf_t* elf) {
    // TDN_ABBREV_COMPILE_UNIT
    dwarf_write_abbrev(elf, TDN_ABBREV_COMPILE_UNIT, DW_TAG_compile_unit, true);
    dwarf_write_abbrev_attr(elf, DW_AT_producer, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_NAMESPACE
    dwarf_write_abbrev(elf, TDN_ABBREV_NAMESPACE, DW_TAG_namespace, true);
    dwarf_write_abbrev_attr(elf, DW_AT_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_NAMESPACE_EXTENSION
    dwarf_write_abbrev(elf, TDN_ABBREV_NAMESPACE_EXTENSION, DW_TAG_namespace, true);
    dwarf_write_abbrev_attr(elf, DW_AT_extension, DW_FORM_ref_udata);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_CLASS
    dwarf_write_abbrev(elf, TDN_ABBREV_CLASS, DW_TAG_class_type, true);
    dwarf_write_abbrev_attr(elf, DW_AT_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_byte_size, DW_FORM_udata);
    dwarf_write_abbrev_attr(elf, DW_AT_calling_convention, DW_FORM_data1);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_FIELD
    dwarf_write_abbrev(elf, TDN_ABBREV_FIELD, DW_TAG_member, false);
    dwarf_write_abbrev_attr(elf, DW_AT_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_type, DW_FORM_ref4);
    dwarf_write_abbrev_attr(elf, DW_AT_data_member_location, DW_FORM_udata);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_BASE_TYPE
    dwarf_write_abbrev(elf, TDN_ABBREV_BASE_TYPE, DW_TAG_inheritance, false);
    dwarf_write_abbrev_attr(elf, DW_AT_type, DW_FORM_ref4);
    dwarf_write_abbrev_attr(elf, DW_AT_data_member_location, DW_FORM_data1);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_PRIMITIVE
    dwarf_write_abbrev(elf, TDN_ABBREV_PRIMITIVE, DW_TAG_base_type, true);
    dwarf_write_abbrev_attr(elf, DW_AT_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_encoding, DW_FORM_data1);
    dwarf_write_abbrev_attr(elf, DW_AT_byte_size, DW_FORM_udata);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_POINTER
    dwarf_write_abbrev(elf, TDN_ABBREV_POINTER, DW_TAG_pointer_type, true);
    dwarf_write_abbrev_attr(elf, DW_AT_type, DW_FORM_ref4);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_GENERIC_ARG
    dwarf_write_abbrev(elf, TDN_ABBREV_GENERIC_ARG, DW_TAG_template_type_parameter, false);
    dwarf_write_abbrev_attr(elf, DW_AT_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_type, DW_FORM_ref4);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_METHOD
    dwarf_write_abbrev(elf, TDN_ABBREV_METHOD, DW_TAG_subprogram, true);
    dwarf_write_abbrev_attr(elf, DW_AT_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_linkage_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_object_pointer, DW_FORM_ref4);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_STATIC_METHOD
    dwarf_write_abbrev(elf, TDN_ABBREV_STATIC_METHOD, DW_TAG_subprogram, true);
    dwarf_write_abbrev_attr(elf, DW_AT_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_linkage_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_METHOD_WITH_RETURN
    dwarf_write_abbrev(elf, TDN_ABBREV_METHOD_WITH_RETURN, DW_TAG_subprogram, true);
    dwarf_write_abbrev_attr(elf, DW_AT_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_linkage_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_type, DW_FORM_ref4);
    dwarf_write_abbrev_attr(elf, DW_AT_object_pointer, DW_FORM_ref4);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_STATIC_METHOD_WITH_RETURN
    dwarf_write_abbrev(elf, TDN_ABBREV_STATIC_METHOD_WITH_RETURN, DW_TAG_subprogram, true);
    dwarf_write_abbrev_attr(elf, DW_AT_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_linkage_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_type, DW_FORM_ref4);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_PARAM
    dwarf_write_abbrev(elf, TDN_ABBREV_PARAM, DW_TAG_formal_parameter, false);
    dwarf_write_abbrev_attr(elf, DW_AT_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_artificial, DW_FORM_data1);
    dwarf_write_abbrev_attr(elf, DW_AT_type, DW_FORM_ref4);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_METHOD_LOCATION
    dwarf_write_abbrev(elf, TDN_ABBREV_METHOD_LOCATION, DW_TAG_subprogram, true);
    dwarf_write_abbrev_attr(elf, DW_AT_specification, DW_FORM_ref_udata);
    dwarf_write_abbrev_attr(elf, DW_AT_low_pc, DW_FORM_addr);
    dwarf_write_abbrev_attr(elf, DW_AT_high_pc, DW_FORM_addr);
    dwarf_write_abbrev_attr(elf, DW_AT_object_pointer, DW_FORM_ref4);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_METHOD_LOCATION
    dwarf_write_abbrev(elf, TDN_ABBREV_STATIC_METHOD_LOCATION, DW_TAG_subprogram, true);
    dwarf_write_abbrev_attr(elf, DW_AT_specification, DW_FORM_ref_udata);
    dwarf_write_abbrev_attr(elf, DW_AT_low_pc, DW_FORM_addr);
    dwarf_write_abbrev_attr(elf, DW_AT_high_pc, DW_FORM_addr);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_PARAM_LOCATION
    dwarf_write_abbrev(elf, TDN_ABBREV_PARAM_LOCATION, DW_TAG_formal_parameter, false);
    dwarf_write_abbrev_attr(elf, DW_AT_name, DW_FORM_string);
    dwarf_write_abbrev_attr(elf, DW_AT_artificial, DW_FORM_data1);
    dwarf_write_abbrev_attr(elf, DW_AT_type, DW_FORM_ref4);
    dwarf_write_abbrev_attr(elf, DW_AT_location, DW_FORM_exprloc);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // TDN_ABBREV_PARAM_LOCATION
    dwarf_write_abbrev(elf, TDN_ABBREV_ARRAY, DW_TAG_array_type, false);
    dwarf_write_abbrev_attr(elf, DW_AT_type, DW_FORM_ref4);
    dwarf_write_abbrev_attr(elf, 0, 0);

    // end of abbrev table
    dwarf_write_uleb(&elf->debug_abbrev, 0);
}

static void dwarf_start_info(jit_elf_t* elf) {
    sh_new_strdup(elf->namespaces);

    // setup the abbrev table
    dwarf_start_abbrev(elf);

    // Compilation unit header
    dwarf_write_u32(&elf->debug_info, 0);               // length, reserve as zero for now
    dwarf_write_u16(&elf->debug_info, 5);               // version
    dwarf_write_u8(&elf->debug_info, DW_UT_compile);    // unit_type
    dwarf_write_u8(&elf->debug_info, 8);                // address_size
    dwarf_write_u32(&elf->debug_info, 0xCAFEBABE);               // debug_abbrev_offset

    dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_COMPILE_UNIT);
    dwarf_write_str(&elf->debug_info, "Tomato.Net");
}

static void dwarf_finish_info(jit_elf_t* elf) {
    // end of compilation unit children
    dwarf_write_uleb(&elf->debug_info, 0);

    // set the length for the debug info
    *((uint32_t*)elf->debug_info) = arrlen(elf->debug_info) - sizeof(uint32_t);
}

static char* string_to_cstr(String str) {
    char* s = tdn_host_mallocz(str->Length + 1, 1);
    for (int i = 0; i < str->Length; i++) {
        s[i] = str->Chars[i];
    }
    return s;
}

static char *strchr(const char *s, int c) {
    for (; *s; s++)
        if (*s == (char)c)
            return (char*)s;
    return c == 0 ? (char*)s : 0;
}

static int dwarf_push_namespace_internal(jit_elf_t* elf, char* nm) {
    // we will push ourselves
    int count = 1;

    int i = shgeti(elf->namespaces, nm);
    if (i != -1) {
        // already exists, use extension
        dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_NAMESPACE_EXTENSION);
        dwarf_write_uleb(&elf->debug_info, elf->namespaces[i].value);

    } else {
        // check if we have more namespaces above us
        char* last_namespace = strchr(nm, '.');
        if (last_namespace != NULL) {
            *last_namespace = '\0';
            last_namespace++;
            count += dwarf_push_namespace_internal(elf, nm);
        } else {
            last_namespace = nm;
        }

        // push our namespace
        dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_NAMESPACE);
        dwarf_write_str(&elf->debug_info, last_namespace);
    }

    return count;
}

static int dwarf_push_namespace(jit_elf_t* elf, String str) {
    if (str == NULL) {
        return 0;
    }

    char* nm = string_to_cstr(str);
    int idx = dwarf_push_namespace_internal(elf, nm);
    tdn_host_free(nm);
    return idx;
}

static void dwarf_pop_namespace(jit_elf_t* elf, int count) {
    // end all of the children of all the namespaces we created
    for (int i = 0; i < count; i++) {
        dwarf_write_uleb(&elf->debug_info, 0);
    }
}

#define DWARF_REG_RAX   DW_OP_reg0
#define DWARF_REG_RDX   DW_OP_reg1
#define DWARF_REG_RCX   DW_OP_reg2
#define DWARF_REG_RBX   DW_OP_reg3
#define DWARF_REG_RSI   DW_OP_reg4
#define DWARF_REG_RDI   DW_OP_reg5
#define DWARF_REG_RBP   DW_OP_reg6
#define DWARF_REG_RSP   DW_OP_reg7
#define DWARF_REG_R8   DW_OP_reg8
#define DWARF_REG_R9   DW_OP_reg9
#define DWARF_REG_R10   DW_OP_reg10
#define DWARF_REG_R11   DW_OP_reg11
#define DWARF_REG_R12   DW_OP_reg12
#define DWARF_REG_R13   DW_OP_reg13
#define DWARF_REG_R14   DW_OP_reg14
#define DWARF_REG_R15   DW_OP_reg15

#define DWARF_REG_XMM0  DW_OP_reg17
#define DWARF_REG_XMM1  DW_OP_reg18
#define DWARF_REG_XMM2  DW_OP_reg19
#define DWARF_REG_XMM3  DW_OP_reg20
#define DWARF_REG_XMM4  DW_OP_reg21
#define DWARF_REG_XMM5  DW_OP_reg22
#define DWARF_REG_XMM6  DW_OP_reg23
#define DWARF_REG_XMM7  DW_OP_reg24
#define DWARF_REG_XMM8  DW_OP_reg25
#define DWARF_REG_XMM9  DW_OP_reg26
#define DWARF_REG_XMM10  DW_OP_reg27
#define DWARF_REG_XMM11  DW_OP_reg28
#define DWARF_REG_XMM12  DW_OP_reg29
#define DWARF_REG_XMM13  DW_OP_reg30
#define DWARF_REG_XMM14  DW_OP_reg31
#define DWARF_REG_XMM15  DW_OP_reg32

static bool method_has_real_return(RuntimeMethodBase method) {
    RuntimeTypeInfo return_type = method->ReturnParameter->ParameterType;
    return return_type != tVoid && !tdn_is_struct_like(return_type);
}

static void dwarf_write_param_location(jit_elf_t* elf, RuntimeTypeInfo type, int* i_offset, int* f_offset) {
    uint8_t* location = NULL;
    if (type == tSingle || type == tDouble) {
        if (*f_offset < 6) {
            uint8_t locations[] = {
                DWARF_REG_XMM0,
                DWARF_REG_XMM1,
                DWARF_REG_XMM2,
                DWARF_REG_XMM3,
                DWARF_REG_XMM4,
                DWARF_REG_XMM5,
                DWARF_REG_XMM6,
                DWARF_REG_XMM7,
            };
            arrpush(location, locations[*f_offset]);
        } else {
            arrpush(location, DW_OP_fbreg);
            arrpush(location, DW_OP_plus_uconst);
            int s_offset = MIN(*i_offset - 6, 0) * 8;
            s_offset += MIN(*f_offset - 5, 0) * 8;
            dwarf_write_uleb((void**)&location, s_offset);
        }
        (*f_offset)++;
    } else {
        if (*i_offset < 6) {
            uint8_t locations[] = {
                DWARF_REG_RDI,
                DWARF_REG_RSI,
                DWARF_REG_RDX,
                DWARF_REG_RCX,
                DWARF_REG_R8,
                DWARF_REG_R9,
            };
            arrpush(location, locations[*i_offset]);
        } else {
            arrpush(location, DW_OP_fbreg);
            arrpush(location, DW_OP_plus_uconst);
            int s_offset = MIN(*i_offset - 5, 0) * 8;
            s_offset += MIN(*f_offset - 6, 0) * 8;
            dwarf_write_uleb((void**)&location, s_offset);
        }
        (*i_offset)++;
    }

    dwarf_write_uleb(&elf->debug_info, arrlen(location));
    void* data = arraddnptr(elf->debug_info, arrlen(location));
    memcpy(data, location, arrlen(location));
    arrfree(location);
}

static void dwarf_write_method_params(jit_elf_t* elf, RuntimeMethodBase method, bool location, bool thunk) {
    int i_offset = 0, f_offset = 0;

    // the `this` parameter
    if (!method->Attributes.Static || thunk) {
        // complete the pointer to the object pointer
        dwarf_write_u32(&elf->debug_info, arrlen(elf->debug_info) + 4);

        // and emit the object pointer
        dwarf_write_uleb(&elf->debug_info, location ? TDN_ABBREV_PARAM_LOCATION : TDN_ABBREV_PARAM);
        dwarf_write_str(&elf->debug_info, "this");
        dwarf_write_u8(&elf->debug_info, true);
        if (thunk) {
            dwarf_write_param_type(elf, tObject);
        } else {
            dwarf_write_param_type(elf, method->DeclaringType);
        }

        // always in the same location
        if (location) {
            dwarf_write_param_location(elf, method->DeclaringType, &i_offset, &f_offset);
        }
    }

    for (int j = 0; j < method->Parameters->Length; j++) {
        ParameterInfo param = method->Parameters->Elements[j];
        dwarf_write_uleb(&elf->debug_info, location ? TDN_ABBREV_PARAM_LOCATION : TDN_ABBREV_PARAM);
        if (param->Name == NULL) {
            dwarf_write_str(&elf->debug_info, "");
        } else {
            dwarf_write_string(&elf->debug_info, param->Name);
        }
        dwarf_write_u8(&elf->debug_info, false);
        dwarf_write_param_type(elf, param->ParameterType);

        if (location) {
            dwarf_write_param_location(elf, param->ParameterType, &i_offset, &f_offset);
        }
    }

    // the implicit return type
    if (!method_has_real_return(method) && method->ReturnParameter->ParameterType != tVoid) {
        dwarf_write_uleb(&elf->debug_info, location ? TDN_ABBREV_PARAM_LOCATION : TDN_ABBREV_PARAM);
        dwarf_write_str(&elf->debug_info, "$result");
        dwarf_write_u8(&elf->debug_info, true);
        dwarf_write_param_type(elf, method->ReturnParameter->ParameterType);

        if (location) {
            dwarf_write_param_location(elf, method->ReturnParameter->ParameterType, &i_offset, &f_offset);
        }
    }
}

static void dwarf_write_method(jit_elf_t* elf, RuntimeMethodBase method, bool thunk) {
    uint8_t tag;
    if (method->Attributes.Static && !thunk) {
        if (method_has_real_return(method)) {
            tag = TDN_ABBREV_STATIC_METHOD_WITH_RETURN;
        } else {
            tag = TDN_ABBREV_STATIC_METHOD;
        }
    } else {
        if (method_has_real_return(method)) {
            tag = TDN_ABBREV_METHOD_WITH_RETURN;
        } else {
            tag = TDN_ABBREV_METHOD;
        }
    }
    hmput(m_elf.methods, (RuntimeMethodBase)method, arrlen(elf->debug_info));
    dwarf_write_uleb(&elf->debug_info, tag);

    // the name
    string_builder_t builder = {};
    string_builder_push_cpp_method_name(&builder, method);
    dwarf_write_str(&elf->debug_info, string_builder_build(&builder));
    string_builder_free(&builder);

    // the linkage name
    mangle_method_name(&builder, (RuntimeMethodBase)method, false);
    dwarf_write_str(&elf->debug_info, string_builder_build(&builder));
    string_builder_free(&builder);

    // the return type
    if (method_has_real_return(method)) {
        dwarf_write_member_type(elf, method->ReturnParameter->ParameterType);
    }

    // the values
    dwarf_write_method_params(elf, method, false, thunk);

    // we are done with its children
    dwarf_write_uleb(&elf->debug_info, 0);
}

static void dwarf_add_single_type(jit_elf_t* elf, RuntimeTypeInfo type) {
    // if already exists don't add it again
    int idx = hmgeti(elf->types, type);
    if (idx != -1) {
        return;
    }

    // if this is an array type, emit the array type itself
    uint32_t array_type_offset = arrlen(elf->debug_info);
    if (type->IsArray) {
        dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_ARRAY);
        dwarf_write_member_type(elf, type->ElementType);
    }

    // push the namespace references
    int ns_count = dwarf_push_namespace(elf, type->Namespace);

    // if this is a by-ref type, then we are going to first emit a pointer
    // type so we will reference it instead of referencing the normal type
    // as a "struct"
    int before = arrlen(elf->debug_info);
    dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_POINTER);
    dwarf_write_u32(&elf->debug_info, arrlen(elf->debug_info) + 5);
    dwarf_write_uleb(&elf->debug_info, 0);
    int after = arrlen(elf->debug_info);
    ASSERT(after - before == 6);

    // add the reference
    hmput(elf->types, type, arrlen(elf->debug_info));

    // perform the existing fixups and remove it from the fixups list
    jit_dwarf_type_fixups_t* fixups = hmgetp_null(elf->type_fixups, type);
    if (fixups != NULL) {
        for (int i = 0; i < arrlen(fixups->value); i++) {
            *((uint32_t*)(elf->debug_info + fixups->value[i])) += arrlen(elf->debug_info);
        }
        arrfree(fixups->value);
        hmdel(elf->type_fixups, type);
    }

    // emit the type properly
    if (
        type == tSByte || type == tInt16 ||
        type == tInt32 || type == tInt64 ||
        type == tIntPtr
    ) {
        dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_PRIMITIVE);
        dwarf_write_string(&elf->debug_info, type->Name);
        dwarf_write_u8(&elf->debug_info, DW_ATE_signed);
        dwarf_write_uleb(&elf->debug_info, type->HeapSize);
    } else if (
        type == tByte || type == tUInt16 ||
        type == tUInt32 || type == tUInt64 ||
        type == tUIntPtr
    ) {
        dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_PRIMITIVE);
        dwarf_write_string(&elf->debug_info, type->Name);
        dwarf_write_u8(&elf->debug_info, DW_ATE_unsigned);
        dwarf_write_uleb(&elf->debug_info, type->HeapSize);

    } else if (type == tBoolean) {
        dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_PRIMITIVE);
        dwarf_write_string(&elf->debug_info, type->Name);
        dwarf_write_u8(&elf->debug_info, DW_ATE_boolean);
        dwarf_write_uleb(&elf->debug_info, type->HeapSize);

    } else if (type == tChar) {
        dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_PRIMITIVE);
        dwarf_write_string(&elf->debug_info, type->Name);
        dwarf_write_u8(&elf->debug_info, DW_ATE_UTF);
        dwarf_write_uleb(&elf->debug_info, type->HeapSize);

    } else if (type == tSingle || type == tDouble) {
        dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_PRIMITIVE);
        dwarf_write_string(&elf->debug_info, type->Name);
        dwarf_write_u8(&elf->debug_info, DW_ATE_float);
        dwarf_write_uleb(&elf->debug_info, type->HeapSize);

    } else if (type->IsPointer || type->IsByRef) {
        dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_POINTER);
        dwarf_write_member_type(elf, type->ElementType);

    } else {
        if (type->Attributes.Sealed && type->Attributes.Abstract) {
            // static class, treat as namespace
            dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_NAMESPACE);
            dwarf_write_string(&elf->debug_info, type->Name);
        } else {
            // non static class
            dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_CLASS);
            string_builder_t builder = {};
            string_builder_push_cpp_type_name(&builder, type);
            dwarf_write_str(&elf->debug_info, string_builder_build(&builder));
            string_builder_free(&builder);
            dwarf_write_uleb(&elf->debug_info, type->HeapSize);
            dwarf_write_u8(&elf->debug_info, tdn_type_is_gc_pointer(type) ? DW_CC_pass_by_reference : DW_CC_pass_by_value);

            if (type->BaseType != NULL && type != tValueType) {
                dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_BASE_TYPE);
                dwarf_write_base_type(elf, type->BaseType);
                dwarf_write_u8(&elf->debug_info, 0);
            }

            // add the generic parameters, in order
            if (type->GenericArguments != NULL) {
                for (int i = 0; i < type->GenericArguments->Length; i++) {
                    dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_GENERIC_ARG);
                    dwarf_write_string(&elf->debug_info, type->GenericTypeDefinition->GenericArguments->Elements[i]->Name);
                    dwarf_write_member_type(elf, type->GenericArguments->Elements[i]);
                }
            }

            // add all the fields
            if (type->DeclaredFields != NULL) {
                for (int i = 0; i < type->DeclaredFields->Length; i++) {
                    RuntimeFieldInfo field = type->DeclaredFields->Elements[i];
                    if (field->Attributes.Static) continue;

                    dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_FIELD);
                    dwarf_write_string(&elf->debug_info, field->Name);
                    dwarf_write_member_type(elf, field->FieldType);
                    dwarf_write_uleb(&elf->debug_info, field->FieldOffset);
                }
            }

            // For arrays also add the elements pointer
            if (type->IsArray) {
                dwarf_write_uleb(&elf->debug_info, TDN_ABBREV_FIELD);
                dwarf_write_str(&elf->debug_info, "_elements");
                dwarf_write_u32(&elf->debug_info, array_type_offset);
                dwarf_write_uleb(&elf->debug_info, tdn_get_array_elements_offset(type->ElementType));
            }
        }

        if (type->DeclaredConstructors != NULL) {
            for (int i = 0; i < type->DeclaredConstructors->Length; i++) {
                dwarf_write_method(elf, (RuntimeMethodBase)type->DeclaredConstructors->Elements[i], false);
            }
        }

        if (type->DeclaredMethods != NULL) {
            for (int i = 0; i < type->DeclaredMethods->Length; i++) {
                dwarf_write_method(elf, (RuntimeMethodBase)type->DeclaredMethods->Elements[i], false);
            }
        }
    }

    // no children for now
    dwarf_write_uleb(&elf->debug_info, 0);

    // finish with it
    dwarf_pop_namespace(elf, ns_count);
}

static void dwarf_add_type(jit_elf_t* elf, RuntimeTypeInfo type) {
    arrpush(elf->type_queue, type);

    while (arrlen(elf->type_queue) != 0) {
        dwarf_add_single_type(elf, arrpop(elf->type_queue));
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Elf emitting API
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum predefined_symbols {
    SYMBOL_NULL,
    SYMBOL_DEBUG_ABBREV_SECTION,
    SYMBOL_TEXT_SECTION,
    SYMBOL_COUNT,
} predefined_symbols_t;

void jit_elf_start_emit() {
    // NULL symbol
    {
        Elf64_Sym* sym = arraddnptr(m_elf.symtab, sizeof(*sym));
        memset(sym, 0, sizeof(*sym));
        sym->st_name = arraddnindex(m_elf.strtab, strlen("")+1);
        sym->st_value = 0;
        sym->st_size = 0;
        sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_NOTYPE);
        sym->st_other = STV_DEFAULT;
        sym->st_shndx = 0;
        strcpy(m_elf.strtab + sym->st_name, "");
    }

    // .debug_abbrev symbol, for relocations
    {
        Elf64_Sym* sym = arraddnptr(m_elf.symtab, sizeof(*sym));
        memset(sym, 0, sizeof(*sym));
        sym->st_name = arraddnindex(m_elf.strtab, strlen(".debug_abbrev")+1);
        sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
        sym->st_other = STV_DEFAULT;
        sym->st_shndx = SECTION_DEBUG_ABBREV;
        strcpy(m_elf.strtab + sym->st_name, ".debug_abbrev");
    }

    // .text symbol, for relocations
    {
        Elf64_Sym* sym = arraddnptr(m_elf.symtab, sizeof(*sym));
        memset(sym, 0, sizeof(*sym));
        sym->st_name = arraddnindex(m_elf.strtab, strlen(".text")+1);
        sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
        sym->st_other = STV_DEFAULT;
        sym->st_shndx = SECTION_TEXT;
        strcpy(m_elf.strtab + sym->st_name, ".text");
    }

    // setup the debug info and abbrev sections
    dwarf_start_info(&m_elf);
}

static int create_or_update_symbol(spidir_funcref_t func) {
    // setup the symbol for this entry
    Elf64_Sym* sym = NULL;
    int idx = hmgeti(m_elf.symbols, func);
    int symbol_index;
    if (idx == -1) {
        // does not exist yet, create the new symbol
        sym = arraddnptr(m_elf.symtab, sizeof(*sym));
        memset(sym, 0, sizeof(*sym));

        RuntimeMethodBase target = jit_get_method_from_function(func);
        bool thunk = false;
        if (target == NULL) {
            target = jit_get_thunk_method(func);
            if (target != NULL) {
                thunk = true;
            }
        }

        // create the name
        string_builder_t builder = {};
        if (target == NULL) {
            // runtime helper
            const char* name = jit_get_helper_name(func);
            ASSERT(name != NULL);
            string_builder_push_cstr(&builder, name);
        } else {
            // real object
            mangle_method_name(&builder, target, thunk);
        }
        const char* name = string_builder_build(&builder);
        int name_offset = arraddnindex(m_elf.strtab, strlen(name)+1);
        strcpy(m_elf.strtab + name_offset, name);
        string_builder_free(&builder);

        sym->st_name = name_offset;

        symbol_index = hmlen(m_elf.symbols) + SYMBOL_COUNT;
        hmput(m_elf.symbols, func, symbol_index);

        // initialize as an external symbol for now
        sym->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC);
        sym->st_other = STV_DEFAULT;
        sym->st_shndx = SHN_UNDEF;
    } else {
        // get the existing symbol
        symbol_index = m_elf.symbols[idx].value;
        sym = &((Elf64_Sym*)m_elf.symtab)[symbol_index];
    }

    return symbol_index;
}

void jit_elf_add_entry(jit_codegen_entry_t* entry) {
    // add 16 bytes for suffix and align the text section
    // to 16 bytes as well
    int padding_start = arraddnindex(m_elf.text, 16);
    arrsetlen(m_elf.text, ALIGN_UP(arrlen(m_elf.text), 16));
    memset(m_elf.text + padding_start, 0xCC, arrlen(m_elf.text) - padding_start);

    // copy the code
    size_t code_size = spidir_codegen_blob_get_code_size(entry->blob);
    int code_offset = arraddnindex(m_elf.text, code_size);
    memcpy(m_elf.text + code_offset, spidir_codegen_blob_get_code(entry->blob), code_size);

    // copy the constpool, if there is one
    size_t constpool_size = spidir_codegen_blob_get_constpool_size(entry->blob);
    size_t constpool_offset = -1;
    if (constpool_size != 0) {
        arrsetlen(m_elf.text, ALIGN_UP(arrlen(m_elf.text), spidir_codegen_blob_get_constpool_align(entry->blob)));
        constpool_offset = arraddnindex(m_elf.text, constpool_size);
        memcpy(m_elf.text + constpool_offset, spidir_codegen_blob_get_constpool(entry->blob), constpool_size);
    }

    // finish setting up the symbol
    int symbol_index = create_or_update_symbol(spidir_funcref_make_internal(entry->key));
    Elf64_Sym* sym = &((Elf64_Sym*)m_elf.symtab)[symbol_index];
    sym->st_value = code_offset;
    sym->st_size = code_size;
    sym->st_shndx = SECTION_TEXT;

    // now emit the relocations
    const spidir_codegen_reloc_t* relocs = spidir_codegen_blob_get_relocs(entry->blob);
    for (int i = 0; i < spidir_codegen_blob_get_reloc_count(entry->blob); i++) {
        const spidir_codegen_reloc_t* reloc = &relocs[i];

        Elf64_Rela* rela = arraddnptr(m_elf.rela_text, sizeof(*rela));
        memset(rela, 0, sizeof(*rela));
        rela->r_offset = code_offset + reloc->offset;
        rela->r_addend = reloc->addend;

        Elf64_Xword sym = 0;
        RuntimeMethodBase target = NULL;
        switch (reloc->target_kind) {
            case SPIDIR_RELOC_TARGET_INTERNAL_FUNCTION: {
                spidir_funcref_t funcref = spidir_funcref_make_internal(reloc->target.internal);
                sym = create_or_update_symbol(funcref);
                target = jit_get_method_from_function(funcref);
            } break;

            case SPIDIR_RELOC_TARGET_EXTERNAL_FUNCTION: {
                spidir_funcref_t funcref = spidir_funcref_make_external(reloc->target.external);
                sym = create_or_update_symbol(funcref);
                target = jit_get_method_from_function(funcref);
            } break;

            case SPIDIR_RELOC_TARGET_CONSTPOOL: {
                sym = SYMBOL_TEXT_SECTION;
            } break;
        }

        // if we have a target then add its debug information as well
        if (target != NULL) {
            dwarf_add_type(&m_elf, target->DeclaringType);
        }

        switch (reloc->kind) {
            case SPIDIR_RELOC_X64_PC32: {
                rela->r_info = ELF64_R_INFO(sym, R_X86_64_PC32);
            } break;

            case SPIDIR_RELOC_X64_ABS64: {
                rela->r_info = ELF64_R_INFO(sym, R_X86_64_64);
            } break;

            default:
                ASSERT(!"Invalid relocation kind");
        }
    }

    //
    // Now add the debug entries
    //
    if (entry->method != NULL) {
        // add the full type decl first, if not specified yet
        dwarf_add_type(&m_elf, entry->method->DeclaringType);

        // add an entry that adds more data to the method specification
        // for its code location
        dwarf_write_uleb(&m_elf.debug_info, (entry->method->Attributes.Static && !entry->thunk) ?
            TDN_ABBREV_STATIC_METHOD_LOCATION : TDN_ABBREV_METHOD_LOCATION);
        dwarf_write_uleb(&m_elf.debug_info, hmget(m_elf.methods, entry->method));
        dwarf_write_u64(&m_elf.debug_info, code_offset);
        dwarf_write_u64(&m_elf.debug_info, code_offset + code_size);
        dwarf_write_method_params(&m_elf, entry->method, true, entry->thunk);
        dwarf_write_uleb(&m_elf.debug_info, 0);
    }
}

void jit_elf_emit() {
    // finish with all the debug information
    dwarf_finish_info(&m_elf);

    // reserve section names
    int null_str_offset = arraddnindex(m_elf.shstrtab, strlen("")+1);
    strcpy(m_elf.shstrtab + null_str_offset, "");
    int text_str_offset = arraddnindex(m_elf.shstrtab, strlen(".text")+1);
    strcpy(m_elf.shstrtab + text_str_offset, ".text");
    int rela_text_str_offset = arraddnindex(m_elf.shstrtab, strlen(".rela.text")+1);
    strcpy(m_elf.shstrtab + rela_text_str_offset, ".rela.text");

    int debug_info_str_offset = arraddnindex(m_elf.shstrtab, strlen(".debug_info")+1);
    strcpy(m_elf.shstrtab + debug_info_str_offset, ".debug_info");
    int rela_debug_info_str_offset = arraddnindex(m_elf.shstrtab, strlen(".rela.debug_info")+1);
    strcpy(m_elf.shstrtab + rela_debug_info_str_offset, ".rela.debug_info");

    int debug_abbrev_str_offset = arraddnindex(m_elf.shstrtab, strlen(".debug_abbrev")+1);
    strcpy(m_elf.shstrtab + debug_abbrev_str_offset, ".debug_abbrev");
    int debug_str_str_offset = arraddnindex(m_elf.shstrtab, strlen(".debug_str")+1);
    strcpy(m_elf.shstrtab + debug_str_str_offset, ".debug_str");

    int symtab_str_offset = arraddnindex(m_elf.shstrtab, strlen(".symtab")+1);
    strcpy(m_elf.shstrtab + symtab_str_offset, ".symtab");
    int strtab_str_offset = arraddnindex(m_elf.shstrtab, strlen(".strtab")+1);
    strcpy(m_elf.shstrtab + strtab_str_offset, ".strtab");
    int shstrtab_str_offset = arraddnindex(m_elf.shstrtab, strlen(".shstrtab")+1);
    strcpy(m_elf.shstrtab + shstrtab_str_offset, ".shstrtab");

    // calculate the elf size
    size_t elf_size = sizeof(Elf64_Ehdr);

    size_t text_offset = elf_size;
    elf_size += arrlen(m_elf.text);

    size_t rela_text_offset = elf_size;
    elf_size += arrlen(m_elf.rela_text);

    size_t debug_info_offset = elf_size;
    elf_size += arrlen(m_elf.debug_info);

    size_t rela_debug_info_offset = elf_size;
    size_t rela_debug_info_size = sizeof(Elf64_Rela);
    elf_size += rela_debug_info_size;

    size_t debug_abbrev_offset = elf_size;
    elf_size += arrlen(m_elf.debug_abbrev);

    size_t debug_str_offset = elf_size;
    elf_size += arrlen(m_elf.debug_str);

    size_t symtab_offset = elf_size;
    elf_size += arrlen(m_elf.symtab);

    size_t strtab_offset = elf_size;
    elf_size += arrlen(m_elf.strtab);

    size_t shstrtab_offset = elf_size;
    elf_size += arrlen(m_elf.shstrtab);

    size_t sections_offset = elf_size;
    elf_size += sizeof(Elf64_Shdr) * SECTION_COUNT;

    // allocate it all in one
    void* elf_data = tdn_host_mallocz(elf_size, 16);
    ASSERT(elf_data != NULL);

    memcpy(elf_data + text_offset, m_elf.text, arrlen(m_elf.text));
    size_t text_size = arrlen(m_elf.text);
    arrfree(m_elf.text);

    memcpy(elf_data + rela_text_offset, m_elf.rela_text, arrlen(m_elf.rela_text));
    size_t rela_text_size = arrlen(m_elf.rela_text);
    arrfree(m_elf.rela_text);

    memcpy(elf_data + debug_info_offset, m_elf.debug_info, arrlen(m_elf.debug_info));
    size_t debug_info_size = arrlen(m_elf.debug_info);
    arrfree(m_elf.debug_info);

    Elf64_Rela* rela_debug_info = elf_data + rela_debug_info_offset;
    rela_debug_info[0].r_offset = 0x8;
    rela_debug_info[0].r_addend = 0;
    rela_debug_info[0].r_info = ELF64_R_INFO((Elf64_Xword)SYMBOL_DEBUG_ABBREV_SECTION, R_X86_64_32);

    memcpy(elf_data + debug_abbrev_offset, m_elf.debug_abbrev, arrlen(m_elf.debug_abbrev));
    size_t debug_abbrev_size = arrlen(m_elf.debug_abbrev);
    arrfree(m_elf.debug_abbrev);

    memcpy(elf_data + debug_str_offset, m_elf.debug_str, arrlen(m_elf.debug_str));
    size_t debug_str_size = arrlen(m_elf.debug_str);
    arrfree(m_elf.debug_str);

    // copy all of the actual data into the elf
    memcpy(elf_data + symtab_offset, m_elf.symtab, arrlen(m_elf.symtab));
    size_t symtab_size = arrlen(m_elf.symtab);
    arrfree(m_elf.symtab);

    memcpy(elf_data + strtab_offset, m_elf.strtab, arrlen(m_elf.strtab));
    size_t strtab_size = arrlen(m_elf.strtab);
    arrfree(m_elf.strtab);

    memcpy(elf_data + shstrtab_offset, m_elf.shstrtab, arrlen(m_elf.shstrtab));
    size_t shstrtab_size = arrlen(m_elf.shstrtab);
    arrfree(m_elf.shstrtab);

    // append all of the sections
    Elf64_Shdr* sections = elf_data + sections_offset;

    // get some pointers now that we finished pushing all of the data
    Elf64_Ehdr* ehdr = elf_data;
    ehdr->e_ident[EI_MAG0] = ELFMAG0;
    ehdr->e_ident[EI_MAG1] = ELFMAG1;
    ehdr->e_ident[EI_MAG2] = ELFMAG2;
    ehdr->e_ident[EI_MAG3] = ELFMAG3;
    ehdr->e_ident[EI_CLASS] = ELFCLASS64;
    ehdr->e_ident[EI_DATA] = ELFDATA2LSB;
    ehdr->e_ident[EI_VERSION] = EV_CURRENT;
    ehdr->e_ident[EI_OSABI] = ELFOSABI_NONE;
    ehdr->e_ident[EI_ABIVERSION] = 0;
    ehdr->e_type = ET_REL;
    ehdr->e_machine = EM_X86_64;
    ehdr->e_version = EV_CURRENT;
    ehdr->e_phoff = 0;
    ehdr->e_phentsize = 0;
    ehdr->e_phnum = 0;
    ehdr->e_shoff = sections_offset;
    ehdr->e_shentsize = sizeof(Elf64_Shdr);
    ehdr->e_shnum = SECTION_COUNT;
    ehdr->e_ehsize = sizeof(*ehdr);
    ehdr->e_shstrndx = SECTION_SHSTRTAB;

    // NULL section
    {
        Elf64_Shdr* shdr = &sections[SECTION_NULL];
        shdr->sh_name = null_str_offset;
        shdr->sh_type = SHT_NULL;
    }

    // .strtab
    {
        Elf64_Shdr* shdr = &sections[SECTION_STRTAB];
        shdr->sh_name = strtab_str_offset;
        shdr->sh_type = SHT_STRTAB;
        shdr->sh_offset = strtab_offset;
        shdr->sh_size = strtab_size;
        shdr->sh_addralign = 1;
    }

    // .shstrtab
    {
        Elf64_Shdr* shdr = &sections[SECTION_SHSTRTAB];
        shdr->sh_name = shstrtab_str_offset;
        shdr->sh_type = SHT_STRTAB;
        shdr->sh_offset = shstrtab_offset;
        shdr->sh_size = shstrtab_size;
        shdr->sh_addralign = 1;
    }

    // .text
    {
        Elf64_Shdr* shdr = &sections[SECTION_TEXT];
        shdr->sh_name = text_str_offset;
        shdr->sh_type = SHT_PROGBITS;
        shdr->sh_flags = SHF_ALLOC | SHF_EXECINSTR;
        shdr->sh_offset = text_offset;
        shdr->sh_size = text_size;
        shdr->sh_addralign = 16;
    }

    // .rela.text
    {
        Elf64_Shdr* shdr = &sections[SECTION_RELA_TEXT];
        shdr->sh_name = rela_text_str_offset;
        shdr->sh_type = SHT_RELA;
        shdr->sh_flags = SHF_INFO_LINK;
        shdr->sh_offset = rela_text_offset;
        shdr->sh_size = rela_text_size;
        shdr->sh_info = SECTION_TEXT;
        shdr->sh_link = SECTION_SYMTAB;
        shdr->sh_entsize = sizeof(Elf64_Rela);
        shdr->sh_addralign = 8;
    }

    // .debug_info
    {
        Elf64_Shdr* shdr = &sections[SECTION_DEBUG_INFO];
        shdr->sh_name = debug_info_str_offset;
        shdr->sh_type = SHT_PROGBITS;
        shdr->sh_offset = debug_info_offset;
        shdr->sh_size = debug_info_size;
        shdr->sh_addralign = 1;
    }

    // .rela.debug_info
    {
        Elf64_Shdr* shdr = &sections[SECTION_RELA_DEBUG_INFO];
        shdr->sh_name = rela_debug_info_str_offset;
        shdr->sh_type = SHT_RELA;
        shdr->sh_flags = SHF_INFO_LINK;
        shdr->sh_offset = rela_debug_info_offset;
        shdr->sh_size = rela_debug_info_size;
        shdr->sh_info = SECTION_DEBUG_INFO;
        shdr->sh_link = SECTION_SYMTAB;
        shdr->sh_entsize = sizeof(Elf64_Rela);
        shdr->sh_addralign = 8;
    }

    // .debug_abbrev
    {
        Elf64_Shdr* shdr = &sections[SECTION_DEBUG_ABBREV];
        shdr->sh_name = debug_abbrev_str_offset;
        shdr->sh_type = SHT_PROGBITS;
        shdr->sh_offset = debug_abbrev_offset;
        shdr->sh_size = debug_abbrev_size;
        shdr->sh_addralign = 1;
    }

    // .debug_str
    {
        Elf64_Shdr* shdr = &sections[SECTION_DEBUG_STR];
        shdr->sh_name = debug_str_str_offset;
        shdr->sh_type = SHT_PROGBITS;
        shdr->sh_offset = debug_str_offset;
        shdr->sh_size = debug_str_size;
        shdr->sh_addralign = 1;
    }

    // .symtab
    {
        Elf64_Shdr* shdr = &sections[SECTION_SYMTAB];
        shdr->sh_name = symtab_str_offset;
        shdr->sh_type = SHT_SYMTAB;
        shdr->sh_flags = SHF_ALLOC;
        shdr->sh_offset = symtab_offset;
        shdr->sh_size = symtab_size;
        shdr->sh_link = SECTION_STRTAB;
        shdr->sh_info = symtab_size / sizeof(Elf64_Sym);
        shdr->sh_addralign = 8;
        shdr->sh_entsize = sizeof(Elf64_Sym);
    }

    void* ctx = tdn_host_jit_start_dump(TDN_JIT_DUMP_ELF);
    tdn_host_jit_dump_callback(elf_data, elf_size, ctx);
    tdn_host_jit_end_dump(ctx);
    tdn_host_free(elf_data);
    shfree(m_elf.namespaces);
    hmfree(m_elf.symbols);
    hmfree(m_elf.type_fixups);
    hmfree(m_elf.types);
    hmfree(m_elf.methods);
    arrfree(m_elf.type_queue);
    memset(&m_elf, 0, sizeof(m_elf));
}
