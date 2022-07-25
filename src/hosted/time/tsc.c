#include <sys/time.h>
#include <stdint.h>
#include <stddef.h>

uint64_t microtime() {
    struct timeval t;
    gettimeofday(&t, NULL);
    return ((uint64_t)t.tv_sec * 1000000) + t.tv_usec;
}

uint64_t get_tsc_freq() {
    return 1; // 1 tick per microsecond
}

uint64_t get_tsc() {
    return microtime();
}
