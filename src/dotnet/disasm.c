

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
