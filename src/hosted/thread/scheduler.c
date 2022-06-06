#define _GNU_SOURCE
#include "scheduler.h"
#include <stdarg.h>
#include <stdio.h>
#include <pthread.h>
#include <signal.h>
#include <sys/syscall.h>
#include <sys/user.h>
#include <stdlib.h>
#include <util/stb_ds.h>

void scheduler_preempt_disable() {}
void scheduler_preempt_enable() {}
void scheduler_ready_thread() {} // TODO: repurpose the same waitgroup here

suspend_state_t scheduler_suspend_thread(thread_t* thread) {
    if (thread->dead) {
        return (suspend_state_t){ .thread = thread, .stopped = true, .dead = true };
    }

    // musl doesn't support pthread_sigqueue (yet), so i need to do it manually
    union sigval val = { .sival_ptr = thread };
    siginfo_t si = {
        .si_signo = SIGUSR1,
	    .si_code = SI_QUEUE,
	    .si_value = val,
	    .si_uid = thread->uid,
	    .si_pid = thread->pid,
    };

    // the wait group is needed because otherwise this function would return
    // with the registers possibly not saved yet
    wait_group_add(&thread->wg, 1);
    syscall(SYS_rt_tgsigqueueinfo, thread->pid, thread->tid, SIGUSR1, &si);
    wait_group_wait(&thread->wg);
    
    return (suspend_state_t){ .thread = thread, .stopped = true };
}

void scheduler_resume_thread(suspend_state_t state) {
    pthread_kill(state.thread->pthread, SIGUSR2);
}