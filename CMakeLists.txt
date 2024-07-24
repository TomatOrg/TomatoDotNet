cmake_minimum_required(VERSION 3.5)
project(tomatodotnet C)

set(CMAKE_C_STANDARD 23)

set(CMAKE_C_COMPILER afl-clang-fast)

file(GLOB_RECURSE SOURCES ./src/*.c ./src/*.h)
file(GLOB_RECURSE HOST_SOURCES ./host/linux/*.c ./host/linux/*.h)
file(GLOB_RECURSE INCLUDE_HEADERS include/tomatodotnet/*.h)

# TODO: something much much much better than this shit
list(FILTER SOURCES EXCLUDE REGEX "\/jit\/")

add_compile_options(-Wall -Werror)
add_compile_options(-Wno-unused-variable -Wno-constant-logical-operand -Wno-unused-parameter -Wno-unused-label -Wno-unused-but-set-variable -Wno-unused-function -Wno-format-invalid-specifier)
add_compile_options(-fms-extensions -Wno-microsoft)
add_compile_options(-O0 -g)

############################################################################

#add_compile_options(-D__JIT_SPIDIR__)
#set(SPIDIR_ENABLE_LOGGING 1)
#add_subdirectory(libs/spidir/c-api)
#list (APPEND SOURCES
#    ./src/dotnet/jit/jit.c
#    ./src/dotnet/jit/jit_internal.c
#    ./src/dotnet/jit/spidir/jit.c
#    ./src/dotnet/jit/spidir/platform.c
#)
#link_libraries(spidir)

############################################################################

#add_compile_options(-D__JIT_MIR__)
#add_subdirectory(libs/mir)
#list (APPEND SOURCES
#        src/dotnet/jit/jit.c
#        src/dotnet/jit/jit_internal.c
#        src/dotnet/jit/mir/jit.c
#)
#link_libraries(mir_static)

############################################################################

add_compile_options(-D__JIT_LLVM__)
find_package(LLVM REQUIRED CONFIG)
llvm_map_components_to_libnames(llvm_libs Core OrcJIT Support nativecodegen)
list (APPEND SOURCES
    src/dotnet/jit/jit.c
    src/dotnet/jit/jit_internal.c
    src/dotnet/jit/llvm/jit.c
)
add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})
link_libraries(${llvm_libs} -lstdc++)

############################################################################

include_directories(libs)
include_directories(include)
include_directories(src)

add_executable(tomatodotnet ${SOURCES} ${HOST_SOURCES} ${TEST_SOURCES} ${INCLUDE_HEADERS})
