cmake_minimum_required(VERSION 3.5)
project(tomatodotnet C)

set(CMAKE_C_STANDARD 23)

file(GLOB_RECURSE SOURCES ./src/*.c ./src/*.h)
file(GLOB_RECURSE HOST_SOURCES ./host/linux/*.c ./host/linux/*.h)
file(GLOB_RECURSE INCLUDE_HEADERS include/tomatodotnet/*.h)

# TODO: something much much much better than this shit
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/./src/dotnet/jit/mir/jit.c")
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/./src/dotnet/jit/firm/jit.c")
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/./src/dotnet/jit/spidir/jit.c")
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/./src/dotnet/jit/spidir/platform.c")
list(REMOVE_ITEM SOURCES "${CMAKE_SOURCE_DIR}/./src/dotnet/jit/spidir/spidir_debug.h")

add_compile_options(-Wall -Werror)
add_compile_options(-Wno-unused-variable -Wno-constant-logical-operand -Wno-unused-parameter -Wno-unused-label -Wno-unused-but-set-variable -Wno-unused-function -Wno-format-invalid-specifier)
add_compile_options(-fms-extensions -Wno-microsoft)
add_compile_options(-fsanitize=address)

add_link_options(-fsanitize=address)

#add_compile_options(-D__JIT_SPIDIR__)
#set(SPIDIR_ENABLE_LOGGING 1)
#add_subdirectory(libs/spidir/c-api)
#list (APPEND SOURCES
#    ./src/dotnet/jit/spidir/jit.c
#    ./src/dotnet/jit/spidir/platform.c
#)
#link_libraries(spidir)

add_compile_options(-D__JIT_FIRM__)
add_subdirectory(libs/libfirm)
include_directories(libs/libfirm/include ${libfirm_BINARY_DIR}/gen/include/libfirm)
list (APPEND SOURCES
    ./src/dotnet/jit/firm/jit.c
)
link_libraries(firm)

#add_compile_options(-D__JIT_MIR__)
#list (APPEND SOURCES
#    ./src/dotnet/jit/mir/jit.c
#    ./libs/mir/mir.c
#    ./libs/mir/mir.h
#)
#include_directories(libs)

include_directories(include)
include_directories(src)

add_executable(tomatodotnet ${SOURCES} ${HOST_SOURCES} ${TEST_SOURCES} ${INCLUDE_HEADERS})
