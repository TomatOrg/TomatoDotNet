#pragma once

#include <tomatodotnet/jit/jit.h>

typedef void (*jit_cctor_t)(void);

/**
 * Initialize anything that needs to be initialized
 */
tdn_err_t tdn_jit_init();

/**
* Queue cctor to run right after jitting is done
*/
void jit_queue_cctor(jit_cctor_t type);
