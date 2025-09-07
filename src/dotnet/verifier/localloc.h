#pragma once

#include <stddef.h>

#include "tomatodotnet/disasm.h"
#include "tomatodotnet/except.h"

typedef enum localloc_verifier_state {
    LOCALLOC_VERIFIER_STATE__NONE,
    LOCALLOC_VERIFIER_STATE__FOUND_LDC_I4,
    LOCALLOC_VERIFIER_STATE__FOUND_CONV_U,
    LOCALLOC_VERIFIER_STATE__FOUND_SIZEOF,
    LOCALLOC_VERIFIER_STATE__FOUND_MUL_OVF_UN,
    LOCALLOC_VERIFIER_STATE__FOUND_LOCALLOC,
    LOCALLOC_VERIFIER_STATE__FOUND_LDC_I4_LEN,
} localloc_verifier_state_t;

typedef struct localloc_verifier {
    localloc_verifier_state_t state;
    int size;
} localloc_verifier_t;

/**
 * check the localloc state machine, the patterns we are matching are:
 *      ldc.i4 <total size>
 *      conv.u
 *      localloc
 *
 * Or (when there are generics):
 *      ldc.i4 <len>
 *      conv.u
 *      sizeof !!T
 *      mul.ovf.un
 *      localloc
 *
 * in both cases we will set a constant value for localloc
 * in the case of safe assemblies we will also verify that
 * it continues with:
 *      ldc.i4 <len>
 *      newobj Span<T>(void*,int)
 *
 * we will explicitly check that the len * sizeof(T) matches
 * the allocated size
 */
tdn_err_t localloc_verifier_check(localloc_verifier_t* verifier, tdn_il_inst_t* inst, bool allow_unsafe);
