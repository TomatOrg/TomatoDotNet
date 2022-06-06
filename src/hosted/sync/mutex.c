#include "mutex.h"

void mutex_lock(mutex_t* mutex) {
    pthread_mutex_lock(mutex);
}

void mutex_unlock(mutex_t* mutex) {
    pthread_mutex_unlock(mutex);
}
