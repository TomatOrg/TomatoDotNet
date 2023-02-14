#pragma once

#include "types.h"

typedef struct exception_frame {
    // the previous frame
    struct exception_frame* previous;

    // this is not documented nicely but setjmp/longjmp
    // need 5 entries
    void* continuation[5];
} exception_frame_t;

/**
 * Gets the currently thrown exception
 */
System_Exception exception_get();

/**
 * Clear the currently thrown exception, also pops
 * the current frame on the way, should be called
 * when you are done handling an exception
 */
void exception_clear();

/**
 * Throw a new exception
 */
 _Noreturn void exception_throw(System_Exception exception);

/**
 * Re-throw the current exception
 */
void exception_rethrow();

/**
 * Set the current exception frame
 */
int exception_set_frame(exception_frame_t* current_frame);

/**
 * Pop the exception frame stack, so we can jump
 * into the next handler
 */
exception_frame_t* exception_pop_frame();
