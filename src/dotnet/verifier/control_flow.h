#pragma once

#include "internal.h"

/**
 * General purpose control flow propagation
 */
tdn_err_t verifier_propagate_control_flow(function_t* function, block_t* from, block_t* target, bool is_fallthrough);

/**
 * Propagation of the `this` state without stack related operations
 */
tdn_err_t verifier_propagate_this_state(function_t* function, block_t* from, block_t* target);

/**
 * Checks needed specifically when using the leave instruction
 */
tdn_err_t verifier_is_valid_leave_target(function_t *function, block_t *src_blk, block_t *target_blk);
