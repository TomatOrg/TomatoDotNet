cmake_minimum_required(VERSION 3.20)
project(tinydotnet C)

set(CMAKE_C_STANDARD 11)

set(CMAKE_C_COMPILER clang)

set(CMAKE_C_FLAGS "")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Werror -std=gnu11")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-unused-label")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-address-of-packed-member")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-psabi")

IF(CMAKE_BUILD_TYPE MATCHES DEBUG)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O0 -g")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=undefined")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-sanitize=alignment")
ENDIF(CMAKE_BUILD_TYPE MATCHES DEBUG)

IF(CMAKE_BUILD_TYPE MATCHES RELEASE)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O3 -g0 -flto")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DNDEBUG")
ENDIF(CMAKE_BUILD_TYPE MATCHES RELEASE)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fshort-wchar")
include_directories(src/dotnet)
include_directories(lib)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fms-extensions -Wno-microsoft-anon-tag")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-unused-command-line-argument")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-format-invalid-specifier -Wno-format-extra-args")

file(GLOB_RECURSE DOTNET_SOURCES src/dotnet/*.c src/dotnet/*.h)
file(GLOB_RECURSE HOSTED_SOURCES src/hosted/*.c src/hosted/*.h)
include_directories(src/hosted)
include_directories(src)

########################################################################################################################
# Printf
########################################################################################################################

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DDPRINTF_NTOA_BUFFER_SIZE=64")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DPRINTF_DISABLE_SUPPORT_EXPONENTIAL")

########################################################################################################################
# Unicode converter
########################################################################################################################

set(UNICODE_SOURCES
    lib/utf8-utf16-converter/converter/src/converter.c
)
include_directories(lib/utf8-utf16-converter/converter/include)

########################################################################################################################
# MIR JIT compiler
########################################################################################################################

set(MIR_SOURCES
    lib/mir/mir.c
    lib/mir/mir-gen.c
)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DMIR_NO_SCAN")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DMIR_PARALLEL_GEN")

add_executable(tinydotnet ${HOSTED_SOURCES} ${DOTNET_SOURCES} ${UNICODE_SOURCES} ${MIR_SOURCES})
