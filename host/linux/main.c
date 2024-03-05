
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "tomatodotnet/except.h"
#include "util/except.h"
#include "dotnet/gc/gc.h"
#include "dotnet/loader.h"
#include "tomatodotnet/disasm.h"
#include "tomatodotnet/jit/jit.h"
#include "dotnet/jit/jit_internal.h"

#include "tomatodotnet/tdn.h"
#include <printf.h>
#include <time.h>

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

static void output_type_name(FILE* f, RuntimeTypeInfo type, bool short_name) {
    if (short_name) {
        if (type == tVoid) { fprintf(f, "void"); return; }
        if (type == tObject) { fprintf(f, "object"); return; }
        if (type == tSByte) { fprintf(f, "sbyte"); return; }
        if (type == tInt16) { fprintf(f, "short"); return; }
        if (type == tInt32) { fprintf(f, "int"); return; }
        if (type == tInt64) { fprintf(f, "long"); return; }
        if (type == tByte) { fprintf(f, "byte"); return; }
        if (type == tUInt16) { fprintf(f, "ushort"); return; }
        if (type == tUInt32) { fprintf(f, "uint"); return; }
        if (type == tUInt64) { fprintf(f, "ulong"); return; }
        if (type == tString) { fprintf(f, "string"); return; }
        if (type == tBoolean) { fprintf(f, "bool"); return; }
        if (type == tChar) { fprintf(f, "char"); return; }
    }

    if (type->IsGenericParameter) {
        fprintf(f, "%U", type->Name);
        return;
    }

    if (type->DeclaringType != NULL) {
        output_type_name(f, type->DeclaringType, false);
        fprintf(f, "+");
    }

    if (type->Namespace != NULL) {
        fprintf(f, "%U.", type->Namespace);
    }

    fprintf(f, "%U", type->Name);

    if (type->GenericArguments != NULL) {
        fprintf(f, "<");
        for (int i = 0; i < type->GenericArguments->Length; i++) {
            if (i != 0) {
                fprintf(f, ", ");
            }
            output_type_name(f, type->GenericArguments->Elements[i], short_name);
        }
        fprintf(f, ">");
    }
}

static int type_output(FILE* stream, const struct printf_info* info, const void* const args[]) {
    int len = 0;

    RuntimeTypeInfo type = *(RuntimeTypeInfo*)args[0];

    if (type == NULL) {
        return fprintf(stream, "<NULL>");
    }

    output_type_name(stream, type, true);

    return len;
}

static int type_arginf_sz(const struct printf_info* info, size_t n, int* argtypes, int* size) {
    if (n > 0) {
        argtypes[0] = PA_POINTER;
    }
    return 1;
}

static tdn_err_t dump_type(RuntimeTypeInfo type, const char* method_to_dump) {
    tdn_err_t err = TDN_NO_ERROR;

    static const char* type_visibility_str[] = {
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
           type_visibility_str[type->Attributes.Visibility],
           type->Attributes.Sealed ? "sealed " : "",
           !type->Attributes.Interface && type->Attributes.Abstract ? "abstract " : "",
           type->Attributes.Interface ? "interface" : "class");
    output_type_name(stdout, type, false);

    if (type->BaseType != NULL) {
        printf(" : ");
        output_type_name(stdout, type->BaseType, true);
    }

    printf(" { // sizeof == 0x%x\n", type->HeapSize);

    for (int j = 0; j < type->DeclaredFields->Length; j++) {
        RuntimeFieldInfo fieldInfo = type->DeclaredFields->Elements[j];
        static const char* field_visibility_str[] = {
            [TDN_FIELD_ACCESS_PRIVATE_SCOPE] = "<private scope>",
            [TDN_FIELD_ACCESS_PRIVATE] = "private",
            [TDN_FIELD_ACCESS_FAMILY_AND_ASSEMBLY] = "private protected",
            [TDN_FIELD_ACCESS_ASSEMBLY] = "internal",
            [TDN_FIELD_ACCESS_FAMILY] = "protected",
            [TDN_FIELD_ACCESS_FAMILY_OR_ASSEMBLY] = "protected internal",
            [TDN_FIELD_ACCESS_PUBLIC] = "public",
        };
        printf("[*] \t%s %s",
               field_visibility_str[fieldInfo->Attributes.FieldAccess],
               fieldInfo->Attributes.Static ? "static " : "");
        output_type_name(stdout, fieldInfo->FieldType, true);
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

        if (method_to_dump != NULL && !tdn_compare_string_to_cstr(method->Name, method_to_dump)) {
            continue;
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
        output_type_name(stdout, method->ReturnParameter->ParameterType, true);
        printf(" %U(", method->Name);
        for (int p = 0; p < method->Parameters->Length; p++) {
            if (p != 0) {
                printf(", ");
            }

            ParameterInfo parameter = method->Parameters->Elements[p];
            printf("%s%s",
                   parameter->Attributes.In ? "in " : "",
                   parameter->Attributes.Out ? "out " : "");
            output_type_name(stdout, parameter->ParameterType, true);
            if (parameter->Name != NULL) {
                printf(" %U", parameter->Name);
            }
        }
        printf(")");

        if (method->MethodBody != NULL) {
            printf(" {\n");
//            CHECK_AND_RETHROW(tdn_disasm_inst(method, ));
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

static tdn_err_t load_assembly_from_path(const char* filename, RuntimeAssembly* assembly) {
    tdn_err_t err = TDN_NO_ERROR;
    FILE* file = NULL;
    void* image = NULL;

    // load the test image
    file = fopen(filename, "rb");
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
    CHECK_AND_RETHROW(tdn_load_assembly_from_memory(image, size, assembly));

cleanup:
    free(image);
    if (file != NULL) fclose(file);

    return err;
}

int main() {
    tdn_err_t err = TDN_NO_ERROR;
    register_printf_specifier('U', string_output, string_arginf_sz);
    register_printf_specifier('T', type_output, type_arginf_sz);

    // load the corelib, must come first
    RuntimeAssembly corelib = NULL;
    CHECK_AND_RETHROW(load_assembly_from_path("TdnCoreLib/System.Private.CoreLib/bin/Release/net7.0/System.Private.CoreLib.dll", &corelib));

    RuntimeAssembly tests = NULL;
    CHECK_AND_RETHROW(load_assembly_from_path("TdnCoreLib/Tests/bin/Release/net7.0/Tests.dll", &tests));

//    RuntimeTypeInfo Bool_And_Op;
//    CHECK_AND_RETHROW(tdn_assembly_lookup_type_by_cstr(tests, "Tests", "Bool_And_Op", &Bool_And_Op));
//    dump_type(Bool_And_Op, "Sub_Funclet_153");

    clock_t t;
    t = clock();
    tdn_jit_method(tests->EntryPoint);
    t = clock() - t;
    double time_taken = ((double)t)/CLOCKS_PER_SEC; // in seconds
    TRACE("Full jit took %f seconds", time_taken);

    int (*entry_point)() = tdn_jit_get_method_address(tests->EntryPoint);
    int tests_output = entry_point();
    TRACE("Tests = %d", tests_output);
    ASSERT(tests_output == 0);

cleanup:
    gc_free_all();

    return (err != TDN_NO_ERROR) ? EXIT_FAILURE : EXIT_SUCCESS;
}
