cmake_minimum_required(VERSION 3.25)
project(tomatodotnet C)

set(CMAKE_C_STANDARD 23)

file(GLOB_RECURSE SOURCES ./src/*.c ./src/*.h)
file(GLOB_RECURSE HOST_SOURCES ./host/linux/*.c ./host/linux/*.h)
file(GLOB_RECURSE INCLUDE_HEADERS include/tomatodotnet/*.h)

set(CMAKE_C_COMPILER clang-16)

add_compile_options(-Wall -Werror)
add_compile_options(-Wno-unused-variable -Wno-constant-logical-operand -Wno-unused-parameter -Wno-unused-label -Wno-unused-but-set-variable -Wno-unused-function -Wno-format-invalid-specifier)
add_compile_options(-fms-extensions -Wno-microsoft)
add_compile_options(-fsanitize=address)
add_compile_options(-D__JIT_SPIDIR__)

add_link_options(-fsanitize=address)

set(SPIDIR_ENABLE_LOGGING 1)
add_subdirectory(libs/spidir/c-api)

include_directories(include)
include_directories(src)

add_executable(tomatodotnet ${SOURCES} ${HOST_SOURCES} ${TEST_SOURCES} ${INCLUDE_HEADERS})
target_link_libraries(tomatodotnet spidir)
