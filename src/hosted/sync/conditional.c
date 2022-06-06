#include "conditional.h"

void conditional_signal(conditional_t* cond) {
    pthread_cond_signal(cond);
}

void conditional_wait(conditional_t* conditional, mutex_t* mutex) {
    pthread_cond_wait(conditional, mutex);
}

void conditional_broadcast(conditional_t* conditional) {
    pthread_cond_broadcast(conditional);
}