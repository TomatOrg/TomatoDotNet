#pragma once

#include <stdint.h>

#include <hosted/sync/semaphore.h>

typedef struct wait_group {
    _Atomic(uint64_t) state;
    semaphore_t sema;
} wait_group_t;

#define INIT_WAIT_GROUP() { 0 }

void wait_group_add(wait_group_t* wg, int delta);

void wait_group_done(wait_group_t* wg);

void wait_group_wait(wait_group_t* wg);
