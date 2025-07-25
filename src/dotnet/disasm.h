#pragma once

#define VarPop  (-1)
#define Pop0    0
#define Pop1    1
#define PopRef  Pop1
#define PopI    Pop1
#define PopI8   Pop1
#define PopR4   Pop1
#define PopR8   Pop1

#define VarPush     (-1)
#define Push0       0
#define Push1       1
#define PushRef     Push1
#define PushI       Push1
#define PushI8      Push1
#define PushR4      Push1
#define PushR8      Push1
#include <stddef.h>

typedef struct il_stack_behavior {
    int pop;
    int push;
} il_stack_behavior_t;

/**
 * Mapping between opcode to stack behaviour
 */
extern const il_stack_behavior_t g_il_stack_behavior[];
extern const size_t g_il_stack_behavior_count;
