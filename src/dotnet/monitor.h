#pragma once

#include <stdint.h>
#include <stdbool.h>
#include "util/except.h"

void free_monitor(void* object);

err_t monitor_enter(void* object);

err_t monitor_is_entered(void* object, bool* taken);

err_t monitor_exit(void* object);

err_t monitor_pulse(void* object);

err_t monitor_pulse_all(void* object);

err_t monitor_wait(void* object);
