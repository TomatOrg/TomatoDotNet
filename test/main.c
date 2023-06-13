
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "tinydotnet/except.h"
#include "util/except.h"
#include "dotnet/gc/gc.h"
#include "dotnet/loader.h"
#include "tinydotnet/disasm.h"

#include <tinydotnet/tdn.h>
#include <printf.h>

static int string_output(FILE* stream, const struct printf_info* info, const void* const args[]) {
    int len = 0;

    System_String str = *(System_String*)args[0];

    if (str == NULL) {
        return fprintf(stream, "<NULL>");
    }

    for (int i = 0; i < str->Length; i++) {
        if (putc(str->Chars[i], stream) == EOF) {
            return len;
        }
        len++;
    }

    return len;
}

static int string_arginf_sz(const struct printf_info* info, size_t n, int* argtypes, int* size) {
    if (n > 0) {
        argtypes[0] = PA_POINTER;
    }
    return 1;
}

static void output_type_name(System_Type type, bool short_name) {
    if (short_name) {
        if (type == tSystem_Void) { printf("void"); return; }
        if (type == tSystem_Object) { printf("object"); return; }
        if (type == tSystem_SByte) { printf("sbyte"); return; }
        if (type == tSystem_Int16) { printf("short"); return; }
        if (type == tSystem_Int32) { printf("int"); return; }
        if (type == tSystem_Int64) { printf("long"); return; }
        if (type == tSystem_Byte) { printf("byte"); return; }
        if (type == tSystem_UInt16) { printf("ushort"); return; }
        if (type == tSystem_UInt32) { printf("uint"); return; }
        if (type == tSystem_UInt64) { printf("ulong"); return; }
        if (type == tSystem_String) { printf("string"); return; }
        if (type == tSystem_Boolean) { printf("bool"); return; }
//    if (type == tSystem_Char) { printf("char"); return; }
    }

    if (type->DeclaringType != NULL) {
        output_type_name(type->DeclaringType, false);
        printf("+");
    }

    if (type->Namespace != NULL) {
        printf("%U.", type->Namespace);
    }

    printf("%U", type->Name);

    if (type->GenericArguments != NULL) {
        printf("[");
        for (int i = 0; i < type->GenericArguments->Length; i++) {
            if (i != 0) {
                printf(", ");
            }
            output_type_name(type->GenericArguments->Elements[i], short_name);
        }
        printf("]");
    }
}

static tdn_err_t dump_type(System_Type type) {
    tdn_err_t err = TDN_NO_ERROR;

    static const char* visibility_str[] = {
            [TDN_TYPE_VISIBILITY_NOT_PUBLIC] = " ",
            [TDN_TYPE_VISIBILITY_PUBLIC] = "public ",
            [TDN_TYPE_VISIBILITY_NESTED_PUBLIC] = "public ",
            [TDN_TYPE_VISIBILITY_NESTED_PRIVATE] = "private ",
            [TDN_TYPE_VISIBILITY_NESTED_FAMILY] = "protected ",
            [TDN_TYPE_VISIBILITY_NESTED_ASSEMBLY] = "internal ",
            [TDN_TYPE_VISIBILITY_NESTED_FAMILY_AND_ASSEMBLY] = "private protected ",
            [TDN_TYPE_VISIBILITY_NESTED_FAMILY_OR_ASSEMBLY] = "protected internal ",
    };

    TRACE("");
    printf("[*] %s%s%s%s ",
           visibility_str[type->Attributes.Visibility],
           type->Attributes.Sealed ? "sealed " : "",
           !type->Attributes.Interface && type->Attributes.Abstract ? "abstract " : "",
           type->Attributes.Interface ? "interface" : "class");
    output_type_name(type, false);

    if (type->GenericArguments != NULL) {
        printf("<");
        for (int j = 0; j < type->GenericArguments->Length; j++) {
            if (j != 0) {
                printf(", ");
            }
            printf("%U", type->GenericArguments->Elements[j]->Name);
        }
        printf(">");
    }

    if (type->BaseType != NULL) {
        printf(" : ");
        output_type_name(type->BaseType, true);
    }

    if (!tdn_type_is_generic(type)) {
        tdn_fill_size(type);
    }

    printf(" { // sizeof == 0x%x\n", type->HeapSize);

    for (int j = 0; j < type->Fields->Length; j++) {
        System_Reflection_FieldInfo fieldInfo = type->Fields->Elements[j];
        static const char* visibility_str[] = {
                [TDN_FIELD_ACCESS_PRIVATE_SCOPE] = "<private scope>",
                [TDN_FIELD_ACCESS_PRIVATE] = "private",
                [TDN_FIELD_ACCESS_FAMILY_AND_ASSEMBLY] = "private protected",
                [TDN_FIELD_ACCESS_ASSEMBLY] = "internal",
                [TDN_FIELD_ACCESS_FAMILY] = "protected",
                [TDN_FIELD_ACCESS_FAMILY_OR_ASSEMBLY] = "protected internal",
                [TDN_FIELD_ACCESS_PUBLIC] = "public",
        };
        printf("[*] \t%s %s",
               visibility_str[fieldInfo->Attributes.FieldAccess],
               fieldInfo->Attributes.Static ? "static " : "");
        output_type_name(fieldInfo->FieldType, true);
        printf(" %U;", fieldInfo->Name);

        if (!fieldInfo->Attributes.Static) {
            printf(" // 0x%x", fieldInfo->FieldOffset);
        }

        printf("\n");
    }

    if (type->Fields->Length != 0 && type->Methods->Length != 0) {
        TRACE("");
    }

    for (int j = 0; j < type->Methods->Length; j++) {
        System_Reflection_MethodInfo methodInfo = type->Methods->Elements[j];
        static const char* visibility_str[] = {
                [TDN_METHOD_ACCESS_PRIVATE_SCOPE] = "<private scope>",
                [TDN_METHOD_ACCESS_PRIVATE] = "private",
                [TDN_METHOD_ACCESS_FAMILY_AND_ASSEMBLY] = "private protected",
                [TDN_METHOD_ACCESS_ASSEMBLY] = "internal",
                [TDN_METHOD_ACCESS_FAMILY] = "protected",
                [TDN_METHOD_ACCESS_FAMILY_OR_ASSEMBLY] = "protected internal",
                [TDN_METHOD_ACCESS_PUBLIC] = "public",
        };
        printf("[*] \t%s %s",
               visibility_str[methodInfo->Attributes.MemberAccess],
               methodInfo->Attributes.Static ? "static " : "");
        output_type_name(methodInfo->ReturnParameter->ParameterType, true);
        printf(" %U(", methodInfo->Name);
        for (int p = 0; p < methodInfo->Parameters->Length; p++) {
            if (p != 0) {
                printf(", ");
            }

            System_Reflection_ParameterInfo parameter = methodInfo->Parameters->Elements[p];
            printf("%s%s",
                   parameter->Attributes.In ? "in " : "",
                   parameter->Attributes.Out ? "out " : "");
            output_type_name(parameter->ParameterType, true);
            if (parameter->Name != NULL) {
                printf(" %U", parameter->Name);
            }
        }
        printf(")");

        if (methodInfo->MethodBody != NULL) {
            printf(" {\n");
            CHECK_AND_RETHROW(tdn_disasm_il(methodInfo));
            TRACE("\t}");
            TRACE("");
        } else {
            printf("\n");
        }
    }

    TRACE("}");

cleanup:
    return err;
}

int main() {
    tdn_err_t err = TDN_NO_ERROR;
    FILE* file = NULL;
    void* image = NULL;

    register_printf_specifier('U', string_output, string_arginf_sz);

    // load the test image
    file = fopen("/home/tomato/RiderProjects/TdnCoreLib/System.Private.CoreLib/bin/Debug/net7.0/System.Private.CoreLib.dll", "rb");
    CHECK_ERROR(file != NULL, -errno);
    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);
    image = malloc(size);
    CHECK_ERROR(image != NULL, -errno);
    CHECK_ERROR(fread(image, size, 1, file) == 1, -errno);
    fclose(file);
    file = NULL;

    // load the corelib
    System_Reflection_Assembly assembly;
    CHECK_AND_RETHROW(tdn_load_assembly_from_memory(image, size, &assembly));

    System_Type type = tdn_assembly_lookup_type_by_cstr(assembly, "System", "Test");
    CHECK(type != NULL);
    CHECK_AND_RETHROW(dump_type(type));

cleanup:
    free(image);
    if (file != NULL) fclose(file);
    gc_free_all();

    return (err != TDN_NO_ERROR) ? EXIT_FAILURE : EXIT_SUCCESS;
}
