#include "disasm.h"

typedef enum opcode_operand {
    InlineNone,
    InlineVar,
    ShortInlineVar,
    InlineI,
    ShortInlineI,
    InlineI8,
    InlineR,
    ShortInlineR,
    InlineBrTarget,
    ShortInlineBrTarget,
    InlineMethod,
    InlineField,
    InlineType,
    InlineString,
    InlineSig,
    InlineTok,
    InlineSwitch,
} opcode_operand_t;

typedef struct disasm_opcode {
    const char* name;
    opcode_operand_t operand;
    uint8_t length;
    uint8_t byte1;
    uint8_t byte2;
} disasm_opcode_t;

static disasm_opcode_t m_opcodes[] = {
#define OPDEF_REAL_OPCODES_ONLY
#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) (disasm_opcode_t){ s, args, l, s1, s2 },
#include "tinydotnet/opcode.def"

#undef OPDEF
#undef OPDEF_REAL_OPCODES_ONLY
};

#define FETCH_I1() \
    ({ \
        CHECK(left >= 1); \
        left -= 1; \
        int8_t ___value = *(int8_t*)opcodes; \
        opcodes += 1; \
        ___value; \
    })

#define FETCH_U1() \
    ({ \
        CHECK(left >= 1); \
        left -= 1; \
        uint8_t ___value = *(uint8_t*)opcodes; \
        opcodes += 1; \
        ___value; \
    })

#define FETCH_U2() \
    ({ \
        CHECK(left >= 2); \
        left -= 2; \
        uint16_t ___value = *(uint16_t*)opcodes; \
        opcodes += 2; \
        ___value; \
    })

#define FETCH_U4() \
    ({ \
        CHECK(left >= 4); \
        left -= 4; \
        uint32_t ___value = *(uint32_t*)opcodes; \
        opcodes += 4; \
        ___value; \
    })


static void output_type_name(RuntimeTypeInfo type, bool short_name) {
    if (short_name) {
        if (type == tVoid) { tdn_host_printf("void"); return; }
        if (type == tObject) { tdn_host_printf("object"); return; }
        if (type == tSByte) { tdn_host_printf("sbyte"); return; }
        if (type == tInt16) { tdn_host_printf("short"); return; }
        if (type == tInt32) { tdn_host_printf("int"); return; }
        if (type == tInt64) { tdn_host_printf("long"); return; }
        if (type == tByte) { tdn_host_printf("byte"); return; }
        if (type == tUInt16) { tdn_host_printf("ushort"); return; }
        if (type == tUInt32) { tdn_host_printf("uint"); return; }
        if (type == tUInt64) { tdn_host_printf("ulong"); return; }
        if (type == tString) { tdn_host_printf("string"); return; }
        if (type == tBoolean) { tdn_host_printf("bool"); return; }
        if (type == tChar) { tdn_host_printf("char"); return; }
    }

    if (type->DeclaringType != NULL) {
        output_type_name(type->DeclaringType, false);
        tdn_host_printf("+");
    }

    if (type->Namespace != NULL) {
//        tdn_host_printf("[%U]", type->Module->Name);
        tdn_host_printf("%U.", type->Namespace);
    }

    tdn_host_printf("%U", type->Name);

    if (type->GenericArguments != NULL) {
        tdn_host_printf("<");
        for (int i = 0; i < type->GenericArguments->Length; i++) {
            if (i != 0) {
                tdn_host_printf(", ");
            }
            output_type_name(type->GenericArguments->Elements[i], short_name);
        }
        tdn_host_printf(">");
    }
}

tdn_err_t tdn_disasm_il(RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeMethodBody body = method->MethodBody;
    RuntimeAssembly assembly = method->Module->Assembly;

    TRACE("\t\t.maxstacksize %d", body->MaxStackSize);

    if (body->LocalVariables != NULL) {
        TRACE("\t\t.locals %s(", body->InitLocals ? "init " : "");
        for (int i = 0; i < body->LocalVariables->Length; i++) {
            tdn_host_printf("[*] \t\t\t[%d] ", i);
            output_type_name(body->LocalVariables->Elements[i]->LocalType, true);
            tdn_host_printf("\n");
        }
        TRACE("\t\t)");
    }

    size_t left = body->ILSize;
    uint8_t* opcodes = body->IL;
    while (left != 0) {
        uint32_t current_ip = opcodes - body->IL;

        // get the opcode as an index
        left--;
        uint16_t opcode = *opcodes++;
        if (opcode == STP1) {
            // if we have a prefix then add it
            CHECK(left > 0);
            left--;
            opcode = 0x100 + *opcodes++;
        } else {
            // if this is anything other make sure it is not
            // an unknown prefix
            CHECK(opcode < RESERVED_PREFIX_START);
        }
        disasm_opcode_t* op = &m_opcodes[opcode];

        // print it
        tdn_host_printf("[*] \t\tIL_%04x: %s", current_ip, op->name);

        switch (op->operand) {
            case InlineNone: break;
            case InlineVar: tdn_host_printf(" %d", FETCH_U2()); break;
            case ShortInlineVar: tdn_host_printf(" %d", FETCH_U1()); break;
            case InlineI: tdn_host_printf(" %d", FETCH_U4()); break;
            case ShortInlineI: tdn_host_printf(" %d", FETCH_U1()); break;
            case InlineI8: CHECK_FAIL("TODO: InlineI8");
            case InlineR: CHECK_FAIL("TODO: InlineR");
            case ShortInlineR: CHECK_FAIL("TODO: ShortInlineR");
            case InlineBrTarget: CHECK_FAIL("TODO: InlineBrTarget");

            case ShortInlineBrTarget: {
                uint32_t target = (uint32_t)FETCH_I1();
                target += opcodes - body->IL;
                tdn_host_printf(" IL_%04x", target);
            } break;

            case InlineMethod: {
                RuntimeMethodBase m = NULL;
                CHECK_AND_RETHROW(tdn_assembly_lookup_method(
                        assembly, FETCH_U4(),
                        method->DeclaringType->GenericArguments, method->GenericArguments,
                        &m));

                if (!m->Attributes.Static) {
                    tdn_host_printf(" instance");
                }
                tdn_host_printf(" ");
                output_type_name(m->ReturnParameter->ParameterType, true);
                tdn_host_printf(" ");
                output_type_name(m->DeclaringType, true);
                tdn_host_printf("::%U(", m->Name);
                for (int i = 0; i < m->Parameters->Length; i++) {
                    if (i != 0) tdn_host_printf(", ");
                    ParameterInfo p = m->Parameters->Elements[i];
                    output_type_name(p->ParameterType, true);
                }
                tdn_host_printf(")");
            } break;

            case InlineField: {
                RuntimeFieldInfo field;
                CHECK_AND_RETHROW(tdn_assembly_lookup_field(assembly, FETCH_U4(), &field));

                tdn_host_printf(" ");
                output_type_name(field->FieldType, true);
                tdn_host_printf(" ");
                output_type_name(field->DeclaringType, true);
                tdn_host_printf("::%U", field->Name);
            } break;

            case InlineType: {
                RuntimeTypeInfo type = NULL;
                CHECK_AND_RETHROW(tdn_assembly_lookup_type(
                        assembly, FETCH_U4(),
                        method->DeclaringType->GenericArguments,
                        NULL,
//                        method->GenericArguments,
                        &type));

                tdn_host_printf(" ");
                output_type_name(type, true);
            } break;

            case InlineString: tdn_host_printf(" __string(%d)", FETCH_U4()); break;
            
            case InlineSig: CHECK_FAIL("TODO: InlineSig");
            case InlineTok: CHECK_FAIL("TODO: InlineTok");
            case InlineSwitch: CHECK_FAIL("TODO: InlineSwitch");
        }

        // new line at the end
        tdn_host_printf("\n");
    }

cleanup:
    return err;
}
