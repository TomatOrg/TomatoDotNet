#pragma once

#include <stdint.h>

 = {
#define OPDEF_REAL_OPCODES_ONLY
#define OPDEF(c,s,pop,push,args,type,l,s1,s2,ctrl) (disasm_opcode_t){ s, args, l, s1, s2 },
#include "tinydotnet/opcode.def"
#undef OPDEF
#undef OPDEF_REAL_OPCODES_ONLY
};
