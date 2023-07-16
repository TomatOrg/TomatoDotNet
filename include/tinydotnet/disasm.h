#pragma once

#include <stdint.h>
#include <stddef.h>

#include "types/reflection.h"


// TODO: maybe have some prefix or something
typedef enum tdn_il_operand {
    TDN_IL_NO_OPERAND,
    TDN_IL_VARIABLE,
    TDN_IL_INT32,
    TDN_IL_INT64,
    TDN_IL_FLOAT32,
    TDN_IL_FLOAT64,
    TDN_IL_BRANCH_TARGET,
    TDN_IL_METHOD,
    TDN_IL_FIELD,
    TDN_IL_TYPE,
    TDN_IL_STRING,
    // TODO: some others
} tdn_il_operand_t;

typedef enum tdn_il_control_flow {
    TDN_IL_NEXT,
    TDN_IL_BREAK,
    TDN_IL_CALL,
    TDN_IL_RETURN,
    TDN_IL_BRANCH,
    TDN_IL_COND_BRANCH,
    TDN_IL_THROW,
} tdn_il_control_flow_t;

typedef enum tdn_il_opcode {
#define OPDEF_REAL_OPCODES_ONLY
#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) c = (((0xff - (s1)) << 16) + (s2)),
#include "tinydotnet/opcode.def"
#undef OPDEF
#undef OPDEF_REAL_OPCODES_ONLY
} tdn_il_opcode_t;

typedef struct tdn_il_inst {
    // the opcode we have
    tdn_il_opcode_t opcode;

    // the operand type
    tdn_il_operand_t operand_type;
    union {
        uint16_t variable;
        int32_t int32;
        int64_t int64;
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

    // the operand control flow
    tdn_il_control_flow_t control_flow;
} tdn_il_inst_t;

tdn_err_t tdn_disasm_inst(tdn_il_inst_t* );
