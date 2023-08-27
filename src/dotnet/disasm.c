

#include "tinydotnet/disasm.h"
#include "tinydotnet/types/type.h"
#include "util/except.h"
#include "util/string.h"

typedef enum opcode_operand {
    InlineBrTarget,
    InlineField,
    InlineI,
    InlineI8,
    InlineMethod,
    InlineNone,
    InlineR,
    InlineSig,
    InlineString,
    InlineSwitch,
    InlineTok,
    InlineType,
    InlineVar,
    ShortInlineBrTarget,
    ShortInlineI,
    ShortInlineR,
    ShortInlineVar,
} opcode_operand_t;

typedef struct il_opcode {
    const char* name;
    opcode_operand_t operand_type;
    tdn_il_control_flow_t control_flow;
} il_opcode_t;

/**
 * Metadata for all the opcodes, used for decoding, this is turned
 * into another struct which is a bit more useful
 */
static il_opcode_t m_il_opcodes[] = {
#define OPDEF_REAL_OPCODES_ONLY
#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) [c] = { .name = s, .operand_type = args, .control_flow = TDN_IL_##ctrl },
#include "tinydotnet/opcode.def"
#undef OPDEF
#undef OPDEF_REAL_OPCODES_ONLY
};

#define FETCH(type) \
    ({ \
        CHECK(body->ILSize - pc >= sizeof(type)); \
        type __value = *(type*)&body->IL[pc]; \
        pc += sizeof(type); \
        __value; \
    })

const char* tdn_get_opcode_name(tdn_il_opcode_t opcode) {
    if (opcode < ARRAY_LENGTH(m_il_opcodes)) {
        return m_il_opcodes[opcode].name;
    } else {
        return NULL;
    }
}

tdn_err_t tdn_disasm_inst(RuntimeMethodBase method, uint32_t pc, tdn_il_inst_t* inst) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeAssembly assembly = method->Module->Assembly;
    RuntimeMethodBody body = method->MethodBody;

    uint32_t start_pc = pc;
    memset(inst, 0, sizeof(*inst));

    // fetch and build the opcode and resolve its metadata, making sure
    // it is a valid opcode
    tdn_il_opcode_t opcode = FETCH(uint8_t);
    if (opcode >= RESERVED_PREFIX_START) {
        opcode = (REFPRE - opcode) << 8;
        opcode |= FETCH(uint8_t);
    }
    CHECK(opcode < ARRAY_LENGTH(m_il_opcodes));
    il_opcode_t* op = &m_il_opcodes[opcode];
    CHECK(op->name != NULL);
    CHECK(strcmp(op->name, "unused") != 0);

    // fill it in
    inst->opcode = opcode;
    inst->control_flow = op->control_flow;

    // and now parse the operand
    switch (op->operand_type) {
        case InlineBrTarget: {
            inst->operand_type = TDN_IL_BRANCH_TARGET;
            int32_t offset = FETCH(int32_t);
            inst->operand.branch_target = pc + offset;
            CHECK(inst->operand.branch_target < body->ILSize);
        } break;

        case InlineField: {
            inst->operand_type = TDN_IL_FIELD;
            CHECK_AND_RETHROW(tdn_assembly_lookup_field(assembly, FETCH(int32_t), &inst->operand.field));
        } break;

        case InlineI: {
            inst->operand_type = TDN_IL_INT32;
            inst->operand.uint32 = FETCH(uint32_t);
        } break;

        case InlineI8: {
            inst->operand_type = TDN_IL_INT64;
            inst->operand.uint64 = FETCH(uint64_t);
        } break;

        case InlineMethod: {
            inst->operand_type = TDN_IL_METHOD;
            CHECK_AND_RETHROW(tdn_assembly_lookup_method(
                    assembly, FETCH(int32_t),
                    method->DeclaringType->GenericArguments, method->GenericArguments,
                    &inst->operand.method));
        } break;

        case InlineNone: {
            inst->operand_type = TDN_IL_NO_OPERAND;
        } break;

        case InlineR: {
            inst->operand_type = TDN_IL_FLOAT64;
            inst->operand.float64 = FETCH(double);
        } break;

        case InlineSig:
            CHECK_FAIL("TODO: InlineSig");

        case InlineString:
            CHECK_FAIL("TODO: InlineString");

        case InlineSwitch:
            CHECK_FAIL("TODO: InlineSwitch");

        case InlineTok:
            CHECK_FAIL("TODO: InlineTok");

        case InlineType: {
            inst->operand_type = TDN_IL_TYPE;
            CHECK_AND_RETHROW(tdn_assembly_lookup_type(
                    assembly, FETCH(int32_t),
                    method->DeclaringType->GenericArguments, method->GenericArguments,
                    &inst->operand.type));
        } break;

        case InlineVar: {
            inst->operand_type = TDN_IL_VARIABLE;
            inst->operand.variable = FETCH(uint16_t);
        } break;

        case ShortInlineBrTarget: {
            inst->operand_type = TDN_IL_BRANCH_TARGET;
            int8_t offset = FETCH(int8_t);
            inst->operand.branch_target = pc + offset;
            CHECK(inst->operand.branch_target < body->ILSize);
        } break;

        case ShortInlineI: {
            inst->operand_type = TDN_IL_INT8;
            inst->operand.uint8 = FETCH(uint8_t);
        } break;

        case ShortInlineR: {
            inst->operand_type = TDN_IL_FLOAT32;
            inst->operand.float32 = FETCH(float);
        } break;

        case ShortInlineVar: {
            inst->operand_type = TDN_IL_VARIABLE;
            inst->operand.variable = FETCH(uint8_t);
        } break;

        default:
            CHECK_FAIL();
    }

    // add the length
    inst->length = pc - start_pc;

cleanup:
    return err;
}

void tdn_normalize_inst(tdn_il_inst_t* inst) {
    switch (inst->opcode) {
        case CEE_LDARG_0: inst->opcode = CEE_LDARG; inst->operand_type = TDN_IL_VARIABLE; inst->operand.variable = 0; break;
        case CEE_LDARG_1: inst->opcode = CEE_LDARG; inst->operand_type = TDN_IL_VARIABLE; inst->operand.variable = 1; break;
        case CEE_LDARG_2: inst->opcode = CEE_LDARG; inst->operand_type = TDN_IL_VARIABLE; inst->operand.variable = 2; break;
        case CEE_LDARG_3: inst->opcode = CEE_LDARG; inst->operand_type = TDN_IL_VARIABLE; inst->operand.variable = 3; break;
        case CEE_LDLOC_0: inst->opcode = CEE_LDLOC; inst->operand_type = TDN_IL_VARIABLE; inst->operand.variable = 0; break;
        case CEE_LDLOC_1: inst->opcode = CEE_LDLOC; inst->operand_type = TDN_IL_VARIABLE; inst->operand.variable = 1; break;
        case CEE_LDLOC_2: inst->opcode = CEE_LDLOC; inst->operand_type = TDN_IL_VARIABLE; inst->operand.variable = 2; break;
        case CEE_LDLOC_3: inst->opcode = CEE_LDLOC; inst->operand_type = TDN_IL_VARIABLE; inst->operand.variable = 3; break;
        case CEE_STLOC_0: inst->opcode = CEE_STLOC; inst->operand_type = TDN_IL_VARIABLE; inst->operand.variable = 0; break;
        case CEE_STLOC_1: inst->opcode = CEE_STLOC; inst->operand_type = TDN_IL_VARIABLE; inst->operand.variable = 1; break;
        case CEE_STLOC_2: inst->opcode = CEE_STLOC; inst->operand_type = TDN_IL_VARIABLE; inst->operand.variable = 2; break;
        case CEE_STLOC_3: inst->opcode = CEE_STLOC; inst->operand_type = TDN_IL_VARIABLE; inst->operand.variable = 3; break;
        case CEE_LDARG_S: inst->opcode = CEE_LDARG; break;
        case CEE_LDARGA_S: inst->opcode = CEE_LDARGA; break;
        case CEE_STARG_S: inst->opcode = CEE_STARG; break;
        case CEE_LDLOC_S: inst->opcode = CEE_LDLOC; break;
        case CEE_LDLOCA_S: inst->opcode = CEE_LDLOCA; break;
        case CEE_STLOC_S: inst->opcode = CEE_STLOC; break;
        case CEE_LDC_I4_M1: inst->opcode = CEE_LDC_I4; inst->operand_type = TDN_IL_INT32; inst->operand.int32 = -1; break;
        case CEE_LDC_I4_0: inst->opcode = CEE_LDC_I4; inst->operand_type = TDN_IL_INT32; inst->operand.int32 = 0; break;
        case CEE_LDC_I4_1: inst->opcode = CEE_LDC_I4; inst->operand_type = TDN_IL_INT32; inst->operand.int32 = 1; break;
        case CEE_LDC_I4_2: inst->opcode = CEE_LDC_I4; inst->operand_type = TDN_IL_INT32; inst->operand.int32 = 2; break;
        case CEE_LDC_I4_3: inst->opcode = CEE_LDC_I4; inst->operand_type = TDN_IL_INT32; inst->operand.int32 = 3; break;
        case CEE_LDC_I4_4: inst->opcode = CEE_LDC_I4; inst->operand_type = TDN_IL_INT32; inst->operand.int32 = 4; break;
        case CEE_LDC_I4_5: inst->opcode = CEE_LDC_I4; inst->operand_type = TDN_IL_INT32; inst->operand.int32 = 5; break;
        case CEE_LDC_I4_6: inst->opcode = CEE_LDC_I4; inst->operand_type = TDN_IL_INT32; inst->operand.int32 = 6; break;
        case CEE_LDC_I4_7: inst->opcode = CEE_LDC_I4; inst->operand_type = TDN_IL_INT32; inst->operand.int32 = 7; break;
        case CEE_LDC_I4_8: inst->opcode = CEE_LDC_I4; inst->operand_type = TDN_IL_INT32; inst->operand.int32 = 8; break;
        case CEE_LDC_I4_S: inst->opcode = CEE_LDC_I4; inst->operand_type = TDN_IL_INT32; inst->operand.int32 = inst->operand.int8; break;
        case CEE_BR_S: inst->opcode = CEE_BR; break;
        case CEE_BRFALSE_S: inst->opcode = CEE_BRFALSE; break;
        case CEE_BRTRUE_S: inst->opcode = CEE_BRTRUE; break;
        case CEE_BEQ_S: inst->opcode = CEE_BEQ; break;
        case CEE_BGE_S: inst->opcode = CEE_BGE; break;
        case CEE_BGT_S: inst->opcode = CEE_BGT; break;
        case CEE_BLE_S: inst->opcode = CEE_BLE; break;
        case CEE_BLT_S: inst->opcode = CEE_BLT; break;
        case CEE_BNE_UN_S: inst->opcode = CEE_BNE_UN; break;
        case CEE_BGE_UN_S: inst->opcode = CEE_BGE_UN; break;
        case CEE_BGT_UN_S: inst->opcode = CEE_BGT_UN; break;
        case CEE_BLE_UN_S: inst->opcode = CEE_BLE_UN; break;
        case CEE_BLT_UN_S: inst->opcode = CEE_BLT_UN; break;
        case CEE_LDELEM_I1: inst->opcode = CEE_LDELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tSByte; break;
        case CEE_LDELEM_I2: inst->opcode = CEE_LDELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tInt16; break;
        case CEE_LDELEM_I4: inst->opcode = CEE_LDELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tInt32; break;
        case CEE_LDELEM_I8: inst->opcode = CEE_LDELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tInt64; break;
        case CEE_LDELEM_U1: inst->opcode = CEE_LDELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tByte; break;
        case CEE_LDELEM_U2: inst->opcode = CEE_LDELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tUInt16; break;
        case CEE_LDELEM_U4: inst->opcode = CEE_LDELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tUInt32; break;
        case CEE_LDELEM_R4: inst->opcode = CEE_LDELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = NULL; break;
        case CEE_LDELEM_R8: inst->opcode = CEE_LDELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = NULL; break;
        case CEE_LDELEM_I: inst->opcode = CEE_LDELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tIntPtr; break;
        case CEE_LEAVE_S: inst->opcode = CEE_LEAVE; break;
        default: break;
    }
}
