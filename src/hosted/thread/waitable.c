#include "waitable.h"
#include "scheduler.h"
#include "time/tsc.h"
#include "util/fastrand.h"

#include <mem/malloc.h>

waitable_t* create_waitable(size_t size) {
    return NULL;
}

waitable_t* put_waitable(waitable_t* waitable) {
    return NULL;
}

void release_waitable(waitable_t* waitable) {
}

bool waitable_send(waitable_t* w, bool block) {
    return true;
}

waitable_result_t waitable_wait(waitable_t* w, bool block) {
    return WAITABLE_SUCCESS;
}

selected_waitable_t waitable_select(waitable_t** waitables, int send_count, int wait_count, bool block) {
    return (selected_waitable_t){};
}

waitable_t* after(int64_t microseconds) {
    return NULL;
}

void waitable_close(waitable_t* w) {
}