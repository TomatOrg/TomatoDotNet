#pragma once

#include "spinlock.h"

#include <semaphore.h>

typedef sem_t semaphore_t;
// this is quite ugly, thanks posix!
#define INIT_SEMAPHORE() \
  ({ \
    sem_t sem; \
    sem_init(&sem); \
    sem; \
  })

void semaphore_acquire(semaphore_t* semaphore, bool lifo);

void semaphore_release(semaphore_t* semaphore, bool handoff);
