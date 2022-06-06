#pragma once

#include "mutex.h"

typedef struct conditional {
    // TODO: this
} conditional_t;

void conditional_wait(conditional_t* conditional, mutex_t* mutex);

void conditional_signal(conditional_t* conditional);

void conditional_broadcast(conditional_t* conditional);
