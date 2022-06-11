#define _GNU_SOURCE
#include "thread.h"
#include <stdarg.h>
#include <stdio.h>
#include <pthread.h>
#include <signal.h>
#include <sys/syscall.h>
#include <sys/user.h>
#include <stdlib.h>
#include <util/stb_ds.h>
#include <sync/mutex.h>

// all threads list, TODO: threads aren't removed yet
thread_t** g_all_threads = NULL;

mutex_t m_all_threads_lock = { 0 };
void lock_all_threads() {
    mutex_lock(&m_all_threads_lock);
}
void unlock_all_threads() {
    mutex_unlock(&m_all_threads_lock);
}

// cursed thread pausing hack;
// sending SIGUSR1 is going to pause the thread and any
// other signal (for example SIGUSR2 which is defined to be empty)
// is gonna return from the pause()
static void sigusr1_handler(int signum, siginfo_t* info, void* arg) {
    ucontext_t* context = arg;
    greg_t* gregs = context->uc_mcontext.gregs;
    thread_t* thread = info->si_value.sival_ptr;
    thread_save_state_t* regs = &thread->save_state;
    regs->rsp = gregs[REG_RSP];
    regs->r15 = gregs[REG_R15];
    regs->r14 = gregs[REG_R14];
    regs->r13 = gregs[REG_R13];
    regs->r12 = gregs[REG_R12];
    regs->r11 = gregs[REG_R11];
    regs->r10 = gregs[REG_R10];
    regs->r9 = gregs[REG_R9];
    regs->r8 = gregs[REG_R8];
    regs->rbp = gregs[REG_RBP];
    regs->rdi = gregs[REG_RDI];
    regs->rsi = gregs[REG_RSI];
    regs->rdx = gregs[REG_RDX];
    regs->rcx = gregs[REG_RCX];
    regs->rbx = gregs[REG_RBX];
    regs->rax = gregs[REG_RAX];
    wait_group_done(&thread->wg);
    // the actual pause, this waits for a SIGUSR2, which just resumes execution
    pause();
}
static void sigusr2_handler(int signum) {}



static __thread thread_t *current_thread;
thread_t* get_current_thread() {
    return current_thread;
}

static void add_to_all_threads(thread_t* thread) {
    // set the default gc thread data, updated by the gc whenever it iterates the
    // thread list and does stuff
    thread->tcb->gc_data = m_default_gc_thread_data;
    arrpush(g_all_threads, thread);
}

// i need to get the thread id and install signal handlers
// which i cannot do from the parent thread
void* thread_entrypoint(void* arg) {
    thread_t* thread = arg;
    thread_entry_t entry = (thread_entry_t)thread->entry;
    
    current_thread = thread; // this is thread local

    // pthread cannot return any of those
    // needed when pthread functions aren't enough
    thread->uid = getpid();
    thread->pid = getpid();
    thread->tid = syscall(SYS_gettid);
    
    // register SIGUSR1 as sigaction handler
    // this allows to pass a void* context and the ucontext data
    // former is used for thread struct, latter for save state
    // SIGUSR1 is used for the cursed process suspend code
    struct sigaction sa = {
        .sa_sigaction = &sigusr1_handler,
        .sa_flags = SA_SIGINFO
    };
    sigaction(SIGUSR1, &sa, NULL);
    signal(SIGUSR2, sigusr2_handler); // SIGUSR2 is used to resume, as it's just empty


    // enter the thread
    entry(thread->ctx);
    
    // user function returned, the thread is dead
    // TODO: remove the thread from g_all_threads
    thread->dead = true;
    free(thread->tcb);
    
    // wait for stack base calculation and g_all_threads
    wait_group_wait(&thread->wg);

    return NULL;
}

thread_t* create_thread(thread_entry_t entry, void* ctx, const char* fmt, ...) {
    thread_t* thread = malloc(sizeof(thread_t));
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(thread->name, sizeof(thread->name), fmt, ap);
    va_end(ap);
    thread->entry = (void*)entry;
    thread->ctx = ctx;
    thread->tcb = malloc(sizeof(thread_control_block_t)); // TODO: alignment
    thread->tcb->tcb = thread->tcb;
    thread->dead = false;
    thread->wg = (wait_group_t)INIT_WAIT_GROUP();

    pthread_create(&thread->pthread, NULL, thread_entrypoint, thread);
    
    wait_group_add(&thread->wg, 1);
    
    // get the stack base and size
    pthread_attr_t attrs;
    pthread_getattr_np(thread->pthread, &attrs);
    void* stack_ptr;
    size_t stack_size;
    pthread_attr_getstack(&attrs, &stack_ptr, &stack_size);
    thread->stack_top = (size_t)stack_ptr;

    // add to g_all_threads
    add_to_all_threads(thread);
    
    wait_group_done(&thread->wg);

    return thread;
}