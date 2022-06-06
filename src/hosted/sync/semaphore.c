#include "semaphore.h"

void semaphore_acquire(semaphore_t* semaphore, bool lifo) {
    sem_wait(semaphore);
}
void semaphore_release(semaphore_t* semaphore, bool handoff) {
    sem_post(semaphore);
}
