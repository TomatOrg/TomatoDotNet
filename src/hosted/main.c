
#include <dotnet/jit/jit.h>
#include <dotnet/gc/gc.h>
#include <dotnet/loader.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include "time/tsc.h"

void *corelib_file, *kernel_file;
size_t corelib_file_size, kernel_file_size;

int get_cpu_count() { return 8; }

void load_file(const char* name, void** file, uint64_t* size) {
    struct stat s;
    int fd = open(name, O_RDONLY);
    fstat(fd, &s);
    *size = s.st_size;
    *file = mmap(0, *size, PROT_READ, MAP_PRIVATE, fd, 0);
}

int main() {
    load_file("Pentagon/Corelib/bin/Release/net6.0/Corelib.dll", &corelib_file, &corelib_file_size);
    load_file("Pentagon/Pentagon/bin/Release/net6.0/Pentagon.dll", &kernel_file, &kernel_file_size);
    //init_gc();
    init_jit();

    // load the corelib
    uint64_t start = microtime();
    loader_load_corelib(corelib_file, corelib_file_size);
    printf("corelib loading took %lums\n", (microtime() - start) / 1000);

    start = microtime();
    System_Reflection_Assembly kernel_asm = NULL;
    loader_load_assembly(kernel_file, kernel_file_size, &kernel_asm);
    printf("kernel loading took %dms\n", (microtime() - start) / 1000);

    method_result_t(*entry_point)() = kernel_asm->EntryPoint->MirFunc->addr;
    method_result_t result = entry_point();
    //CHECK(result.exception == NULL, "Got exception: \"%U\"", result.exception->Message);
    printf("Kernel output: %d\n", result.value);
}

