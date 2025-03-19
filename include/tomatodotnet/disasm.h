#pragma once

#include <stdint.h>
#include <stddef.h>

#include "types/reflection.h"


// TODO: maybe have some prefix or something
typedef enum tdn_il_operand {
    TDN_IL_NO_OPERAND,
    TDN_IL_VARIABLE,
    TDN_IL_INT8,
    TDN_IL_INT32,
    TDN_IL_INT64,
    TDN_IL_FLOAT32,
    TDN_IL_FLOAT64,
    TDN_IL_BRANCH_TARGET,
    TDN_IL_METHOD,
    TDN_IL_FIELD,
    TDN_IL_TYPE,
    TDN_IL_STRING,
    TDN_IL_SWITCH,
    // TODO: some others
} tdn_il_operand_t;

typedef enum tdn_il_control_flow {
    TDN_IL_CF_FIRST,
    TDN_IL_CF_NEXT,
    TDN_IL_CF_BREAK,
    TDN_IL_CF_CALL,
    TDN_IL_CF_RETURN,
    TDN_IL_CF_BRANCH,
    TDN_IL_CF_COND_BRANCH,
    TDN_IL_CF_THROW,
    TDN_IL_CF_META,
} tdn_il_control_flow_t;

typedef enum tdn_il_opcode {
#define OPDEF_REAL_OPCODES_ONLY
#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) c = (((REFPRE - (s1)) << 8) + (s2)),
#include "tomatodotnet/opcode.def"
#undef OPDEF
#undef OPDEF_REAL_OPCODES_ONLY
} tdn_il_opcode_t;

typedef enum tdn_il_prefix {
    TDN_IL_PREFIX_VOLATILE = 1 << 0,
} tdn_il_prefix_t;

typedef struct tdn_il_inst {
    // the opcode we have
    tdn_il_opcode_t opcode;

    // the operand type
    tdn_il_operand_t operand_type;
    union {
        uint16_t variable;
        int8_t int8;
        int32_t int32;
        int64_t int64;
        uint32_t uint8;
        uint32_t uint32;
        uint64_t uint64;
        float float32;
        double float64;
        uint32_t branch_target;
        RuntimeMethodBase method;
        RuntimeFieldInfo field;
        RuntimeTypeInfo type;
        String string;
    } operand;
    int operand_token;

    // the length of the current instruction
    size_t length;

    // the operand control flow
    tdn_il_control_flow_t control_flow;

    // the prefixes for the instruction
    tdn_il_prefix_t prefixes;

    // the pc of the opcode
    uint32_t pc;
} tdn_il_inst_t;

const char* tdn_get_opcode_name(tdn_il_opcode_t opcode);

tdn_err_t tdn_disasm_inst(RuntimeMethodBase method, uint32_t pc, tdn_il_inst_t* inst);

/**
 * Convert to a more generic version for easier processing
 */
void tdn_normalize_inst(tdn_il_inst_t* inst);

//
// Print helpers
//
int tdn_disasm_print_start(RuntimeMethodBody body, uint32_t pc, tdn_il_inst_t inst, int indent);
int tdn_disasm_print_end(RuntimeMethodBody body, uint32_t pc, int indent);
