#include "spinlock.h"

void spinlock_lock(spinlock_t* spinlock) {
    pthread_mutex_lock(spinlock);
}

void spinlock_unlock(spinlock_t* spinlock) {
    pthread_mutex_unlock(spinlock);
}
