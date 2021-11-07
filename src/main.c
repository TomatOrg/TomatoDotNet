
#include <dotnet/assembly.h>
#include <dotnet/type.h>
#include <dotnet/method.h>
#include <string.h>

int main() {
    err_t err = NO_ERROR;

    assembly_t* assembly = NULL;
    CHECK_AND_RETHROW(load_assembly("/home/tomato/projects/tinydotnet/Corelib/Corelib/bin/Release/net5.0/Corelib.dll", &assembly));

    type_t* type = assembly_get_type_by_name(assembly, "Corelib", "Program");
    CHECK(type != NULL);

    method_t* method = NULL;
    for (int i = 0; i < type->methods_count; i++) {
        if (strcmp(type->methods[i].name, "Main") == 0) {
            method = &type->methods[i];
        }
    }
    CHECK(method != NULL);

cleanup:
    return IS_ERROR(err) ? EXIT_FAILURE : EXIT_SUCCESS;
}
