#pragma once

#include <util/except.h>

/**
 * Get a timer in microseconds, it has no defined start date, but will
 * always grow upward in microseconds
 */
uint64_t microtime();

/**
 * Gets the TSC frequency
 */
uint64_t get_tsc_freq();

/**
 * Get the current TSC
 */
uint64_t get_tsc();
