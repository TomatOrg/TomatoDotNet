#include <sys/time.h>
#include <stdint.h>
#include <stddef.h>

uint64_t microtime() {
    struct timeval t;
    gettimeofday(&t, NULL);
    return ((uint64_t)t.tv_sec * 1000000) + t.tv_usec;
}

