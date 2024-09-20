#pragma once

#include <stdint.h>

typedef struct prime_generator {
    int index;
} prime_generator_t;

uint64_t prime_generate(prime_generator_t *generator);
