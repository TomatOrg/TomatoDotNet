#pragma once

#include "thread.h"

#include <stdbool.h>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Get the current running thread
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Put a thread into a ready state
 *
 * @param thread    [IN]
 */
void scheduler_ready_thread(thread_t* thread);

typedef struct suspend_state {
    thread_t* thread;
    bool stopped;
    bool dead;
} suspend_state_t;

/**
 * suspends the thread at a safe point and returns the
 * state of the suspended thread. The caller gets read access
 * to the thread until it calls resume.
 *
 * @param thread    [IN] The thread to suspend
 */
suspend_state_t scheduler_suspend_thread(thread_t* thread);

/**
 * Resumes a thread that was previously suspended.
 *
 * @param state     [IN] The state of the thread to resume
 */
void scheduler_resume_thread(suspend_state_t status);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Preemption stuff
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Disable preemption, nestable
 */
void scheduler_preempt_disable(void);

/**
 * Enable preemption, nestable
 */
void scheduler_preempt_enable(void);

/**
 * Returns true if preemption is enabled
 */
bool scheduler_is_preemption(void);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Get the current running thread
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Get the currently running thread on the current CPU
 */
thread_t* get_current_thread();

/**
 * Request the scheduler to yield from our thread, passing our time-slice to the caller,
 * putting us at the CPU's local run-queue
 */
void scheduler_yield();