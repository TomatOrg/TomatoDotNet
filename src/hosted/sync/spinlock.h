#pragma once

#include <stdbool.h>

typedef struct spinlock {
    // TODO: this
} spinlock_t;

#define INIT_SPINLOCK() ((spinlock_t){  })

void spinlock_lock(spinlock_t* spinlock);

bool spinlock_try_lock(spinlock_t* spinlock);

void spinlock_unlock(spinlock_t* spinlock);

bool spinlock_is_locked(spinlock_t* spinlock);
