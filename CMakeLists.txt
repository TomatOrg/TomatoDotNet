cmake_minimum_required(VERSION 3.20)
project(tinydotnet C)

set(CMAKE_C_STANDARD 11)

set(CMAKE_C_FLAGS " -g -Wformat -Werror -Wno-address-of-packed-member")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -flto")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D__FILENAME__='\"$(subst ${CMAKE_SOURCE_DIR}/,,$(abspath $<))\"'")

include_directories(src)

file(GLOB_RECURSE SOURCES tinydotnet/*.c tinydotnet/*.h)

file(GLOB_RECURSE ENTRY_SOURCES src/*.c src/*.h)

include_directories(mir)

set(MIR_SOURCES
    mir/mir.c
    mir/mir-gen.c
)

add_executable(tinydotnet ${SOURCES} ${ENTRY_SOURCES} ${MIR_SOURCES})
