

#include "tomatodotnet/disasm.h"

#include <util/string_builder.h>

#include "gc/gc.h"
#include "metadata/metadata.h"
#include "metadata/metadata_tables.h"
#include "metadata/sig.h"
#include "tomatodotnet/types/type.h"
#include "util/except.h"
#include "util/stb_ds.h"
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
#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) [c] = { .name = s, .operand_type = args, .control_flow = TDN_IL_CF_##ctrl },
#include "tomatodotnet/opcode.def"
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
            inst->operand_token = FETCH(int32_t);
            CHECK_AND_RETHROW(tdn_assembly_lookup_field(
                    assembly, inst->operand_token,
                    method->DeclaringType->GenericArguments, method->GenericArguments,
                    &inst->operand.field));
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
            inst->operand_token = FETCH(int32_t);
            CHECK_AND_RETHROW(tdn_assembly_lookup_method(
                    assembly, inst->operand_token,
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

        case InlineString: {
            token_t user_string_token = FETCH(token_t);
            CHECK(user_string_token.table == 0x70);
            inst->operand_type = TDN_IL_STRING;
            CHECK(user_string_token.index < assembly->Metadata->us_size);

            // get the string from the token
            String string = hmget(assembly->StringTable, user_string_token.index);
            if (string == NULL) {
                // not found, create a new string and store it

                // get the blob
                uint32_t blob_size = 0;
                blob_entry_t blob = {
                    .data = assembly->Metadata->us + user_string_token.index,
                    .size = assembly->Metadata->us_size - user_string_token.index
                };
                CHECK_AND_RETHROW(sig_parse_compressed_int(&blob, &blob_size));

                // validate and set the length
                CHECK(blob_size <= assembly->Metadata->blob_size - user_string_token.index);

                // must be uneven
                CHECK(blob_size % 2 == 1);
                size_t char_count = (blob_size - 1) / 2;

                // create it
                string = gc_new(tString, sizeof(struct String) + char_count * sizeof(Char));
                CHECK_ERROR(string != NULL, TDN_ERROR_OUT_OF_MEMORY);

                // copy the bytes
                // TODO: validate this is a valid utf16 string
                string->Length = (int)char_count;
                memcpy(string->Chars, blob.data, char_count * sizeof(Char));

                // and store it now for future uses
                hmput(assembly->StringTable, user_string_token.index, string);
            }

            inst->operand.string = string;
        } break;

        case InlineSwitch:
            CHECK_FAIL("TODO: InlineSwitch");

        case InlineTok:
            CHECK_FAIL("TODO: InlineTok");

        case InlineType: {
            inst->operand_type = TDN_IL_TYPE;
            inst->operand_token = FETCH(int32_t);
            CHECK_AND_RETHROW(tdn_assembly_lookup_type(
                    assembly, inst->operand_token,
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
        case CEE_STELEM_I1: inst->opcode = CEE_STELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tSByte; break;
        case CEE_STELEM_I2: inst->opcode = CEE_STELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tInt16; break;
        case CEE_STELEM_I4: inst->opcode = CEE_STELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tInt32; break;
        case CEE_STELEM_I8: inst->opcode = CEE_STELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tInt64; break;
//        case CEE_STELEM_R4: inst->opcode = CEE_STELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = NULL; break;
//        case CEE_STELEM_R8: inst->opcode = CEE_STELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = NULL; break;
        case CEE_STELEM_I: inst->opcode = CEE_STELEM; inst->operand_type = TDN_IL_TYPE; inst->operand.type = tIntPtr; break;

        case CEE_LDIND_I1: inst->operand_type = TDN_IL_TYPE; inst->operand.type = tSByte; break;
        case CEE_LDIND_I2: inst->operand_type = TDN_IL_TYPE; inst->operand.type = tInt16; break;
        case CEE_LDIND_I4: inst->operand_type = TDN_IL_TYPE; inst->operand.type = tInt32; break;
        case CEE_LDIND_I8: inst->operand_type = TDN_IL_TYPE; inst->operand.type = tInt64; break;
        case CEE_LDIND_U1: inst->operand_type = TDN_IL_TYPE; inst->operand.type = tByte; break;
        case CEE_LDIND_U2: inst->operand_type = TDN_IL_TYPE; inst->operand.type = tUInt16; break;
        case CEE_LDIND_U4: inst->operand_type = TDN_IL_TYPE; inst->operand.type = tUInt32; break;

        case CEE_STIND_I1: inst->operand_type = TDN_IL_TYPE; inst->operand.type = tSByte; break;
        case CEE_STIND_I2: inst->operand_type = TDN_IL_TYPE; inst->operand.type = tInt16; break;
        case CEE_STIND_I4: inst->operand_type = TDN_IL_TYPE; inst->operand.type = tInt32; break;
        case CEE_STIND_I8: inst->operand_type = TDN_IL_TYPE; inst->operand.type = tInt64; break;

        case CEE_LEAVE_S: inst->opcode = CEE_LEAVE; break;
        default: break;
    }
}

int tdn_disasm_print_start(RuntimeMethodBody body, uint32_t pc, tdn_il_inst_t inst, int indent) {
    for (int i = 0; body->ExceptionHandlingClauses != NULL && i < body->ExceptionHandlingClauses->Length; i++) {
        RuntimeExceptionHandlingClause c = body->ExceptionHandlingClauses->Elements[i];
        if (c->TryOffset == pc) {
            tdn_host_printf("[*] \t\t\t%*s.try\n", indent, "");
            tdn_host_printf("[*] \t\t\t%*s{\n", indent, "");
            indent += 4;

        } else if (c->HandlerOffset == pc) {
            if (c->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
                string_builder_t temp_builder = {};
                string_builder_push_type_signature(&temp_builder, c->CatchType);
                tdn_host_printf("[*] \t\t\t%*scatch %s\n", indent, "", string_builder_build(&temp_builder));
                string_builder_free(&temp_builder);
            } else if (c->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
                tdn_host_printf("[*] \t\t\t%*sfinally\n", indent, "");
            } else {
                ASSERT(!"TODO");
            }
            tdn_host_printf("[*] \t\t\t%*s{\n", indent, "");
            indent += 4;
        }
    }

    // check if we are getting nested

    tdn_host_printf("[*] \t\t\t%*sIL_%04x: %s", indent, "", pc, tdn_get_opcode_name(inst.opcode));
    switch (inst.operand_type) {
        case TDN_IL_BRANCH_TARGET: tdn_host_printf(" IL_%04x", inst.operand.branch_target); break;
        case TDN_IL_NO_OPERAND: break;
        case TDN_IL_VARIABLE: tdn_host_printf(" %d", inst.operand.variable); break;
        case TDN_IL_INT8: tdn_host_printf(" %d", inst.operand.int8); break;
        case TDN_IL_INT32: tdn_host_printf(" %d", inst.operand.int32); break;
        case TDN_IL_INT64: tdn_host_printf(" %lld", (long long int)inst.operand.int64); break;
        case TDN_IL_FLOAT32: tdn_host_printf(" %f", inst.operand.float32); break;
        case TDN_IL_FLOAT64: tdn_host_printf(" %f", inst.operand.float64); break;

        case TDN_IL_METHOD: {
            string_builder_t tmp_builder = {};
            string_builder_push_method_signature(&tmp_builder, inst.operand.method, true);
            tdn_host_printf(" %s", string_builder_build(&tmp_builder));
            string_builder_free(&tmp_builder);
        } break;

        case TDN_IL_FIELD: {
            string_builder_t tmp_builder = {};
            string_builder_push_type_signature(&tmp_builder, inst.operand.field->DeclaringType);
            tdn_host_printf(" %s::%U", string_builder_build(&tmp_builder), inst.operand.field->Name);
            string_builder_free(&tmp_builder);
        } break;

        case TDN_IL_TYPE: {
            string_builder_t tmp_builder = {};
            string_builder_push_type_signature(&tmp_builder, inst.operand.type);
            tdn_host_printf(" %s", string_builder_build(&tmp_builder));
            string_builder_free(&tmp_builder);
        } break;

        case TDN_IL_STRING: tdn_host_printf(" %U", inst.operand.string); break;
        case TDN_IL_SWITCH: ASSERT(!"TODO: switch");
    }
    tdn_host_printf("\n");

    return indent;
}

int tdn_disasm_print_end(RuntimeMethodBody body, uint32_t pc, int indent) {
    for (int i = 0; body->ExceptionHandlingClauses != NULL && i < body->ExceptionHandlingClauses->Length; i++) {
        RuntimeExceptionHandlingClause c = body->ExceptionHandlingClauses->Elements[i];
        if (c->TryOffset + c->TryLength == pc) {
            indent -= 4;
            tdn_host_printf("[*] \t\t\t%*s} // end .try - %04x\n", indent, "");
        } else if (c->HandlerOffset + c->HandlerLength == pc) {
            indent -= 4;
            tdn_host_printf("[*] \t\t\t%*s} // end handler\n", indent, "");
        }
    }

    return indent;
}
