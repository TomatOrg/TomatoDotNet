
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "tinydotnet/except.h"
#include "util/except.h"
#include "dotnet/gc/gc.h"
#include "dotnet/loader.h"
#include "tinydotnet/disasm.h"
#include "tinydotnet/jit/jit.h"
#include "dotnet/jit/jit_internal.h"

#include <tinydotnet/tdn.h>
#include <printf.h>

static int string_output(FILE* stream, const struct printf_info* info, const void* const args[]) {
    int len = 0;

    String str = *(String*)args[0];

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

static void output_type_name(RuntimeTypeInfo type, bool short_name) {
    if (short_name) {
        if (type == tVoid) { printf("void"); return; }
        if (type == tObject) { printf("object"); return; }
        if (type == tSByte) { printf("sbyte"); return; }
        if (type == tInt16) { printf("short"); return; }
        if (type == tInt32) { printf("int"); return; }
        if (type == tInt64) { printf("long"); return; }
        if (type == tByte) { printf("byte"); return; }
        if (type == tUInt16) { printf("ushort"); return; }
        if (type == tUInt32) { printf("uint"); return; }
        if (type == tUInt64) { printf("ulong"); return; }
        if (type == tString) { printf("string"); return; }
        if (type == tBoolean) { printf("bool"); return; }
        if (type == tChar) { printf("char"); return; }
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
        printf("<");
        for (int i = 0; i < type->GenericArguments->Length; i++) {
            if (i != 0) {
                printf(", ");
            }
            output_type_name(type->GenericArguments->Elements[i], short_name);
        }
        printf(">");
    }
}

static tdn_err_t dump_type(RuntimeTypeInfo type) {
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

    if (type->BaseType != NULL) {
        printf(" : ");
        output_type_name(type->BaseType, true);
    }

    printf(" { // sizeof == 0x%x\n", type->HeapSize);

    for (int j = 0; j < type->DeclaredFields->Length; j++) {
        RuntimeFieldInfo fieldInfo = type->DeclaredFields->Elements[j];
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

    if (type->DeclaredFields->Length != 0 && type->DeclaredMethods->Length != 0) {
        TRACE("");
    }

    for (int j = 0; j < type->DeclaredConstructors->Length + type->DeclaredMethods->Length; j++) {
        RuntimeMethodBase method = NULL;
        if (j >= type->DeclaredConstructors->Length) {
            method = (RuntimeMethodBase) type->DeclaredMethods->Elements[j - type->DeclaredConstructors->Length];
        } else {
            method = (RuntimeMethodBase) type->DeclaredConstructors->Elements[j];
        }
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
               visibility_str[method->Attributes.MemberAccess],
               method->Attributes.Static ? "static " : "");
        output_type_name(method->ReturnParameter->ParameterType, true);
        printf(" %U(", method->Name);
        for (int p = 0; p < method->Parameters->Length; p++) {
            if (p != 0) {
                printf(", ");
            }

            ParameterInfo parameter = method->Parameters->Elements[p];
            printf("%s%s",
                   parameter->Attributes.In ? "in " : "",
                   parameter->Attributes.Out ? "out " : "");
            output_type_name(parameter->ParameterType, true);
            if (parameter->Name != NULL) {
                printf(" %U", parameter->Name);
            }
        }
        printf(")");

        if (method->MethodBody != NULL) {
            printf(" {\n");
//            CHECK_AND_RETHROW(tdn_disasm_il(method, ));
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
    file = fopen("TdnCoreLib/System.Private.CoreLib/bin/Release/net7.0/System.Private.CoreLib.dll", "rb");
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
    RuntimeAssembly assembly;
    CHECK_AND_RETHROW(tdn_load_assembly_from_memory(image, size, &assembly));

    RuntimeTypeInfo type;
    CHECK_AND_RETHROW(tdn_assembly_lookup_type_by_cstr(assembly, "System", "Test", &type));
    CHECK(type != NULL);

    CHECK_AND_RETHROW(tdn_jit_method((RuntimeMethodBase)type->DeclaredMethods->Elements[0]));

//    type = type->DeclaredMethods->Elements[0]->Parameters->Elements[0]->ParameterType;
//    CHECK_AND_RETHROW(dump_type(type));



cleanup:
    free(image);
    if (file != NULL) fclose(file);
    gc_free_all();

    return (err != TDN_NO_ERROR) ? EXIT_FAILURE : EXIT_SUCCESS;
}
