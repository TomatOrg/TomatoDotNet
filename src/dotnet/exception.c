#include "exception.h"

#include "util/except.h"
#include "thread/thread.h"
#include "mem/mem.h"

#define CURRENT_EXCEPTION ((System_Exception __seg_fs*)offsetof(thread_control_block_t, current_exception))

static THREAD_LOCAL exception_frame_t* m_exception_stack = NULL;

static THREAD_LOCAL int m_exception_stack_depth = 0;

System_Exception exception_get() {
    return *CURRENT_EXCEPTION;
}

bool exception_has_handler() {
    return m_exception_stack != NULL;
}

void exception_clear() {
    exception_pop_frame();
    *CURRENT_EXCEPTION = NULL;
}

void exception_throw(System_Exception exception) {
    exception_frame_t* frame = exception_pop_frame();
    *CURRENT_EXCEPTION = exception;
    __builtin_longjmp(frame->continuation, 1);
}

void exception_rethrow() {
    exception_frame_t* frame = exception_pop_frame();

    // if we had an exception then rethrow it, otherwise just
    // return and continue the normal handling
    if (*CURRENT_EXCEPTION) {
        __builtin_longjmp(frame->continuation, 1);
    }
}

int exception_set_frame(exception_frame_t* current_frame) {
    ASSERT(STACK_POOL_START <= (uintptr_t)current_frame && (uintptr_t)current_frame < STACK_POOL_END);
    memset(current_frame, 0, sizeof(*current_frame));
    current_frame->previous = m_exception_stack;
    m_exception_stack = current_frame;
    m_exception_stack_depth++;
    return __builtin_setjmp(current_frame->continuation);
}

exception_frame_t* exception_pop_frame() {
    m_exception_stack_depth--;
    exception_frame_t* frame = m_exception_stack;
    m_exception_stack = frame->previous;
    return frame;
}
