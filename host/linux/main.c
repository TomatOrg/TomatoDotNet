
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "tomatodotnet/except.h"
#include "util/except.h"
#include "dotnet/loader.h"
#include "tomatodotnet/disasm.h"
#include "tomatodotnet/jit/jit.h"
#include <libgen.h>
#include "tomatodotnet/tdn.h"
#include <printf.h>
#include <string.h>
#include <time.h>
#include <dotnet/metadata/metadata.h>
#include "tomatodotnet/util/stb_ds.h"
#include <getopt.h>
#include <spidir/log.h>
#include <sys/mman.h>

#include "dotnet/types.h"
#include "dotnet/verifier/verifier.h"
#include "gc/mem_tree.h"
#include "util/string_builder.h"

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
    if (type->IsGenericTypeParameter) {
        fprintf(stream, " (of ");
        output_type_name(stream, type->DeclaringType, false);
        fprintf(stream, ")");
    } else if (type->IsGenericMethodParameter) {
        fprintf(stream, " (of ");
        output_type_name(stream, type->DeclaringMethod->DeclaringType, false);
        fprintf(stream, "::");
        fprintf(stream, "%U", type->DeclaringMethod->Name);
        fprintf(stream, ")");
    }

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
            [TDN_ACCESS_PRIVATE_SCOPE] = "<private scope>",
            [TDN_ACCESS_PRIVATE] = "private",
            [TDN_ACCESS_FAMILY_AND_ASSEMBLY] = "private protected",
            [TDN_ACCESS_ASSEMBLY] = "internal",
            [TDN_ACCESS_FAMILY] = "protected",
            [TDN_ACCESS_FAMILY_OR_ASSEMBLY] = "protected internal",
            [TDN_ACCESS_PUBLIC] = "public",
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
            [TDN_ACCESS_PRIVATE_SCOPE] = "<private scope>",
            [TDN_ACCESS_PRIVATE] = "private",
            [TDN_ACCESS_FAMILY_AND_ASSEMBLY] = "private protected",
            [TDN_ACCESS_ASSEMBLY] = "internal",
            [TDN_ACCESS_FAMILY] = "protected",
            [TDN_ACCESS_FAMILY_OR_ASSEMBLY] = "protected internal",
            [TDN_ACCESS_PUBLIC] = "public",
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

static void console_write_line(String str) {
    TRACE("%U", str);
}

extern const char* g_assembly_search_path;

static bool tdn_string_ends(String str, const char* pattern) {
    size_t len = strlen(pattern);
    for (int j = 0; j < len; j++) {
        if (str->Chars[(str->Length - len) + j] != pattern[j]) {
            return false;
        }
    }
    return true;
}

static bool tdn_string_contains(String str, const char* pattern) {
    size_t pattern_len = strlen(pattern);
    if (pattern_len == 0) return true;
    if (pattern_len > str->Length) return false;

    for (size_t i = 0; i <= str->Length - pattern_len; i++) {
        bool match = true;
        for (size_t j = 0; j < pattern_len; j++) {
            if (str->Chars[i + j] != pattern[j]) {
                match = false;
                break;
            }
        }

        if (match) {
            return true;
        }
    }

    return false;
}

static tdn_err_t tdn_run_ilverify_test(RuntimeAssembly assembly) {
    tdn_err_t err = TDN_NO_ERROR;

    for (int i = 0; i < assembly->TypeDefs->Length; i++) {
        RuntimeTypeInfo type = assembly->TypeDefs->Elements[i];
        if (tdn_is_generic_type_definition(type)) {
            continue;
        }

        for (int j = 0; j < type->DeclaredMethods->Length; j++) {
            RuntimeMethodBase method = (RuntimeMethodBase)type->DeclaredMethods->Elements[j];
            if (method->GenericMethodDefinition == (RuntimeMethodInfo)method) {
                continue;
            }

            // there are special cases where we need to choose another method
            RuntimeMethodBase test_method = method;
            if (tdn_string_contains(method->Name, "special.")) {
                if (tdn_string_contains(method->Name, "..ctor")) {
                    for (int i = 0; i < method->DeclaringType->DeclaredConstructors->Length; i++) {
                        RuntimeConstructorInfo ctor = method->DeclaringType->DeclaredConstructors->Elements[i];
                        // TODO: check exact signature or something
                        if (ctor->Parameters->Length == method->Parameters->Length) {
                            test_method = (RuntimeMethodBase)ctor;
                            break;
                        }
                    }
                } else {
                    CHECK_FAIL("%T::%U", method->DeclaringType, method->Name);
                }
            }

            if (tdn_string_ends(method->Name, "_Valid")) {
                ERROR("TESTING %T::%U", method->DeclaringType, method->Name);
                CHECK_AND_RETHROW(tdn_jit_method(test_method));

            } else if (tdn_string_contains(method->Name, "_Invalid_")) {
                ERROR("TESTING %T::%U", method->DeclaringType, method->Name);

                // we expect to get an error from this, if we didn't
                // then we failed the test
                tdn_err_t jit_err = verifier_verify_method(test_method);
                CHECK(IS_ERROR(jit_err), "Should have failed");

                // check that we got the correct condition
                if (tdn_string_contains(method->Name, "_ExpectedNumericType")) CHECK(jit_err == TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE);
                else if (tdn_string_contains(method->Name, "_StackUnexpected")) CHECK(jit_err == TDN_ERROR_VERIFIER_STACK_UNEXPECTED);
                else if (tdn_string_contains(method->Name, "_BranchOutOfTry")) CHECK(jit_err == TDN_ERROR_VERIFIER_BRANCH_OUT_OF_TRY);
                else if (tdn_string_contains(method->Name, "_BranchIntoTry")) CHECK(jit_err == TDN_ERROR_VERIFIER_BRANCH_INTO_TRY);
                else if (tdn_string_contains(method->Name, "_BranchOutOfFinally")) CHECK(jit_err == TDN_ERROR_VERIFIER_BRANCH_OUT_OF_FINALLY);
                else if (tdn_string_contains(method->Name, "_FallthroughException")) CHECK(jit_err == TDN_ERROR_VERIFIER_FALLTHROUGH_EXCEPTION);
                else if (tdn_string_contains(method->Name, "_BadJumpTarget")) CHECK(jit_err == TDN_ERROR_VERIFIER_BAD_JUMP_TARGET);
                else if (tdn_string_contains(method->Name, "_ThisMismatch")) CHECK(jit_err == TDN_ERROR_VERIFIER_THIS_MISMATCH);
                else if (tdn_string_contains(method->Name, "_CtorSig.CtorExpected")) CHECK(jit_err == TDN_ERROR_VERIFIER_CTOR_EXPECTED || jit_err == TDN_ERROR_VERIFIER_CTOR_SIG);
                else if (tdn_string_contains(method->Name, "_CtorExpected")) CHECK(jit_err == TDN_ERROR_VERIFIER_CTOR_EXPECTED);
                else if (tdn_string_contains(method->Name, "_CtorSig")) CHECK(jit_err == TDN_ERROR_VERIFIER_CTOR_SIG);
                else if (tdn_string_contains(method->Name, "_ReturnVoid")) CHECK(jit_err == TDN_ERROR_VERIFIER_RETURN_VOID);
                else if (tdn_string_contains(method->Name, "_ReturnMissing")) CHECK(jit_err == TDN_ERROR_VERIFIER_RETURN_MISSING);
                else if (tdn_string_contains(method->Name, "_ReturnEmpty")) CHECK(jit_err == TDN_ERROR_VERIFIER_RETURN_EMPTY);
                else if (tdn_string_contains(method->Name, "_ReturnFromTry")) CHECK(jit_err == TDN_ERROR_VERIFIER_RETURN_FROM_TRY);
                else if (tdn_string_contains(method->Name, "_ReturnFromHandler")) CHECK(jit_err == TDN_ERROR_VERIFIER_RETURN_FROM_HANDLER);
                else if (tdn_string_contains(method->Name, "_ReturnPtrToStack")) CHECK(jit_err == TDN_ERROR_VERIFIER_RETURN_PTR_TO_STACK);
                else if (tdn_string_contains(method->Name, "_MethodFallthrough")) CHECK(jit_err == TDN_ERROR_VERIFIER_METHOD_FALLTHROUGH);
                else if (tdn_string_contains(method->Name, "_ThisUninitReturn")) CHECK(jit_err == TDN_ERROR_VERIFIER_THIS_UNINIT_RETURN);
                else CHECK_FAIL("Invalid error condition");

            } else {
                TRACE("IGNORING %T::%U", method->DeclaringType, method->Name);
            }
        }
    }

cleanup:
    return err;
}

static void type_clear_fields(RuntimeTypeInfo type) {
    for (int i = 0; i < type->DeclaredFields->Length; i++) {
        RuntimeFieldInfo field = type->DeclaredFields->Elements[i];
        if (field->JitFieldPtr != NULL && !field->HasRVA) {
            memset(field->JitFieldPtr, 0, field->FieldType->StackSize);
        }
    }

    for (int i = 0; i < hmlen(type->GenericTypeInstances); i++) {
        type_clear_fields(type->GenericTypeInstances[i].value);
    }
}

static void assembly_clear_roots(RuntimeAssembly assembly) {
    for (int i = 0; i < assembly->TypeDefs->Length; i++) {
        type_clear_fields(assembly->TypeDefs->Elements[i]);
    }
}

extern void* g_gc_bottom_of_stack;

int main(int argc, char* argv[]) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeAssembly corelib = NULL;
    RuntimeAssembly run = NULL;
    int result = 0;

    g_gc_bottom_of_stack = __builtin_frame_address(0);

    register_printf_specifier('U', string_output, string_arginf_sz);
    register_printf_specifier('T', type_output, type_arginf_sz);

    int jit_emit_verbose = 0;
    int jit_type_verbose = 0;
    int jit_verify_verbose = 0;
    int jit_dump = 0;
    int jit_dump_elf = 0;
    int jit_dont_optimize = 0;
    int jit_dont_inline = 0;
    int il_verify_test = 0;
    struct option options[] = {
        {"search-path", required_argument, 0, 's'},
        {"jit-emit-verbose", no_argument, &jit_emit_verbose, 1},
        {"jit-type-verbose", no_argument, &jit_type_verbose, 1},
        {"jit-verify-verbose", no_argument, &jit_verify_verbose, 1},
        {"jit-dump", no_argument, &jit_dump, 1},
        {"jit-dump-elf", no_argument, &jit_dump_elf, 1},
        {"jit-dont-optimize", no_argument, &jit_dont_optimize, 1},
        {"jit-dont-inline", no_argument, &jit_dont_inline, 1},
        {"ilverify-test", no_argument, &il_verify_test, 1},
        {0, 0, 0, 0}
    };

    int opt, option_index;
    while ((opt = getopt_long(argc, argv, "s:", options, &option_index)) != -1) {
        switch (opt) {
            case 0: break;

            case 's': {
                g_assembly_search_path = strdup(optarg);
                CHECK(g_assembly_search_path != NULL);
            } break;

            case '?': {
                err = TDN_ERROR_CHECK_FAILED;
                goto cleanup;
            } break;

            default:
                printf("?? getopt returned character code 0%o ??\n", opt);
                break;

        }
    }

    // Remaining non-option arguments (should be the DLL to run)
    CHECK(optind < argc, "Expected <dll to run> after options");

    // set the global options
    tdn_config_t* config = tdn_get_config();
    if (jit_dont_inline) config->jit_inline = false;
    if (jit_dont_optimize) config->jit_optimize = false;
    if (jit_dump) config->jit_spidir_dump = true;
    if (jit_emit_verbose) config->jit_emit_trace = true;
    if (jit_type_verbose) config->jit_type_trace = true;
    if (jit_verify_verbose) config->jit_verify_trace = true;
    if (jit_dump_elf) config->jit_elf_dump = true;

    // auto-resolve the search path from the file to run
    if (g_assembly_search_path == NULL) {
        g_assembly_search_path = dirname(strdup(argv[optind]));
        CHECK(g_assembly_search_path != NULL);
    }

    tdn_file_t corelib_file = NULL;
    CHECK(tdn_host_resolve_assembly("System.Private.CoreLib", 1, &corelib_file),
        "Failed to resolve System.Private.CoreLib");

    // load the corelib
    CHECK_AND_RETHROW(tdn_load_assembly_from_file(corelib_file, &corelib));

    // now load the assembly we want to run
    CHECK_AND_RETHROW(load_assembly_from_path(argv[optind], &run));

    if (il_verify_test) {
        CHECK_AND_RETHROW(tdn_run_ilverify_test(run));
    } else {
        // and now jit it and let it run
        clock_t t;
        t = clock();
        CHECK(run->EntryPoint != NULL, "Not an executable assembly");
        CHECK_AND_RETHROW(tdn_jit_method(run->EntryPoint));
        t = clock() - t;
        double time_taken = ((double)t)/CLOCKS_PER_SEC; // in seconds
        TRACE("Jit took %f seconds", time_taken);

        CHECK(run->EntryPoint->MethodPtr != NULL);
        int (*entry_point)() = run->EntryPoint->MethodPtr;
        result = entry_point();
    }

cleanup:
    tdn_cleanup();

    // collect everything, we should have
    // nothing allocated after this
    if (corelib != NULL) assembly_clear_roots(corelib);
    if (run != NULL) assembly_clear_roots(run);
    tdn_host_gc_start();

    // to ensure that everything gets cleaned properly
    // we will destroty the tree
    mem_tree_destroy();

    return (err != TDN_NO_ERROR) ? EXIT_FAILURE : result;
}
