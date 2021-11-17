
#include <dotnet/assembly.h>
#include <dotnet/type.h>
#include <dotnet/method.h>
#include <string.h>
#include <dotnet/jit.h>
#include <dotnet/thread.h>

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

    // create the domain and load our assembly
    app_domain_t* app_domain = new_app_domain();
    app_domain_load_assembly(app_domain, assembly);

    // create a thread from main and run it
    int return_value = 0;
    thread_t* thread = NULL;
    CHECK_AND_RETHROW(new_thread(app_domain, method, &thread));
    CHECK_AND_RETHROW(thread_exec(thread, &return_value, NULL));
    TRACE("Return value: %d", return_value);

cleanup:
    return IS_ERROR(err) ? EXIT_FAILURE : EXIT_SUCCESS;
}
