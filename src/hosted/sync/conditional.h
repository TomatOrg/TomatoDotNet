#pragma once

#include "mutex.h"

#include <pthread.h>

typedef pthread_cond_t conditional_t;

#define INIT_CONDITIONAL() PTHREAD_COND_INITIALIZER

void conditional_wait(conditional_t* conditional, mutex_t* mutex);

void conditional_signal(conditional_t* conditional);

void conditional_broadcast(conditional_t* conditional);
