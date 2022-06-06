#pragma once

#include <stdbool.h>

#include <pthread.h>

typedef pthread_mutex_t spinlock_t;

#define INIT_SPINLOCK() PTHREAD_MUTEX_INITIALIZER

void spinlock_lock(spinlock_t* spinlock);

bool spinlock_try_lock(spinlock_t* spinlock);

void spinlock_unlock(spinlock_t* spinlock);

bool spinlock_is_locked(spinlock_t* spinlock);
