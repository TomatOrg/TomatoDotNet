#pragma once

#include "spinlock.h"

#include <thread/thread.h>

typedef struct semaphore {
    // TODO: this
} semaphore_t;

void semaphore_acquire(semaphore_t* semaphore, bool lifo);

void semaphore_release(semaphore_t* semaphore, bool handoff);
