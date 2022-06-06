#pragma once

#include "semaphore.h"

#include <stdint.h>

#include <pthread.h>

typedef pthread_mutex_t mutex_t;

#define INIT_MUTEX() PTHREAD_MUTEX_INITIALIZER

void mutex_lock(mutex_t* mutex);

bool mutex_try_lock(mutex_t* mutex);

void mutex_unlock(mutex_t* mutex);
