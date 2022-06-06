#pragma once

#include "mutex.h"

typedef struct rwmutex {
    // TODO: this
} rwmutex_t;

void rwmutex_rlock(rwmutex_t* rw);

void rwmutex_runlock(rwmutex_t* rw);

void rwmutex_lock(rwmutex_t* rw);

void rwmutex_unlock(rwmutex_t* rw);
