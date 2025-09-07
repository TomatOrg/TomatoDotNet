#include "gdb.h"

#include <stdalign.h>

#include "helpers.h"
#include "dotnet/types.h"
#include "tomatodotnet/types/type.h"
#include "tomatodotnet/util/stb_ds.h"
#include "util/defs.h"
#include "util/except.h"
#include "util/string.h"
#include "util/elf64.h"
#include "util/string_builder.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// From GDB docs:
//  https://sourceware.org/gdb/current/onlinedocs/gdb.html/Declarations.html#Declarations
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum
{
    JIT_NOACTION = 0,
    JIT_REGISTER_FN,
    JIT_UNREGISTER_FN
  } jit_actions_t;

struct jit_code_entry
{
    struct jit_code_entry *next_entry;
    struct jit_code_entry *prev_entry;
    const char *symfile_addr;
    uint64_t symfile_size;
};

struct jit_descriptor
{
    uint32_t version;
    /* This type should be jit_actions_t, but we use uint32_t
       to be explicit about the bitwidth.  */
    uint32_t action_flag;
    struct jit_code_entry *relevant_entry;
    struct jit_code_entry *first_entry;
};

/* GDB puts a breakpoint in this function.  */
void __attribute__((noinline)) __jit_debug_register_code() { };

/* Make sure to specify the version statically, because the
   debugger may check the version before we can set it.  */
struct jit_descriptor __jit_debug_descriptor = { 1, 0, 0, 0 };

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit interface usage
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum object_section {
    SECTION_NULL,
    SECTION_TEXT,
    SECTION_SYMTAB,
    SECTION_STRTAB,
    SECTION_SHSTRTAB,
    SECTION_COUNT
} object_section_t;

static void jit_gdb_register_elf(void *elf_data, size_t elf_size) {
    struct jit_code_entry* entry = tdn_host_mallocz(sizeof(*entry), 16);
    ASSERT(entry != NULL);

    entry->symfile_addr = elf_data;
    entry->symfile_size = elf_size;

    // Insert at head of linked list
    entry->next_entry = __jit_debug_descriptor.first_entry;
    if (entry->next_entry)
        entry->next_entry->prev_entry = entry;

    __jit_debug_descriptor.first_entry = entry;
    __jit_debug_descriptor.relevant_entry = entry;
    __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;

    __jit_debug_register_code();
}

static char* strcpy(char* strDest, const char* strSrc) {
    char* temp = strDest;
    while ((*strDest++ = *strSrc++) != '\0');
    return temp;
}

void jit_gdb_register(
    jit_codegen_entry_t* funcs,
    void* code, size_t code_size
) {
    //
    // Start by preparing all of the data in all of the sections
    //
    void* symtab = NULL;
    void* strtab = NULL;
    void* shstrtab = NULL;

    // NULL symbol
    {
        Elf64_Sym* sym = arraddnptr(symtab, sizeof(*sym));
        memset(sym, 0, sizeof(*sym));
        sym->st_name = arraddnindex(strtab, strlen("")+1);
        sym->st_value = 0;
        sym->st_size = 0;
        sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_NOTYPE);
        sym->st_other = STV_DEFAULT;
        sym->st_shndx = 0;
        strcpy(strtab + sym->st_name, "");
    }

    // now append all the symbols
    for (int i = 0; i < hmlen(funcs); i++) {
        // setup the name of the symbol
        string_builder_t builder = {};
        string_builder_push_method_signature(&builder, funcs[i].method, true);
        char* name = string_builder_build(&builder);
        int name_offset = arraddnindex(strtab, strlen(name) + 1);
        strcpy(strtab + name_offset, name);
        string_builder_free(&builder);

        // actually append the symbol
        Elf64_Sym* sym = arraddnptr(symtab, sizeof(*sym));
        memset(sym, 0, sizeof(*sym));
        sym->st_name = name_offset;
        sym->st_value = (funcs[i].thunk ? funcs[i].method->ThunkPtr : funcs[i].method->MethodPtr) - code;
        sym->st_size = (funcs[i].thunk ? funcs[i].method->ThunkSize : funcs[i].method->MethodSize);
        sym->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC);
        sym->st_other = STV_DEFAULT;
        sym->st_shndx = SECTION_TEXT;
    }

    // reserve section names
    int null_str_offset = arraddnindex(shstrtab, strlen("")+1);
    strcpy(shstrtab + null_str_offset, "");
    int text_str_offset = arraddnindex(shstrtab, strlen(".text")+1);
    strcpy(shstrtab + text_str_offset, ".text");
    int symtab_str_offset = arraddnindex(shstrtab, strlen(".symtab")+1);
    strcpy(shstrtab + symtab_str_offset, ".symtab");
    int strtab_str_offset = arraddnindex(shstrtab, strlen(".strtab")+1);
    strcpy(shstrtab + strtab_str_offset, ".strtab");
    int shstrtab_str_offset = arraddnindex(shstrtab, strlen(".shstrtab")+1);
    strcpy(shstrtab + shstrtab_str_offset, ".shstrtab");

    //
    // Now finalize the entire elf
    //

    // calculate the elf size
    size_t elf_size = sizeof(Elf64_Ehdr);

    size_t symtab_offset = elf_size;
    elf_size += arrlen(symtab);

    size_t strtab_offset = elf_size;
    elf_size += arrlen(strtab);

    size_t shstrtab_offset = elf_size;
    elf_size += arrlen(shstrtab);

    size_t sections_offset = elf_size;
    elf_size += sizeof(Elf64_Shdr) * SECTION_COUNT;

    // allocate it all in one
    void* elf_data = tdn_host_mallocz(elf_size, 16);
    ASSERT(elf_data != NULL);

    // copy all of the actual data into the elf
    memcpy(elf_data + symtab_offset, symtab, arrlen(symtab));
    size_t symtab_size = arrlen(symtab);
    arrfree(symtab);

    memcpy(elf_data + strtab_offset, strtab, arrlen(strtab));
    size_t strtab_size = arrlen(strtab);
    arrfree(strtab);

    memcpy(elf_data + shstrtab_offset, shstrtab, arrlen(shstrtab));
    size_t shstrtab_size = arrlen(shstrtab);
    arrfree(shstrtab);

    // append all of the sections
    Elf64_Shdr* sections = elf_data + sections_offset;

    // get some pointers now that we finished pushing all of the data
    Elf64_Ehdr* ehdr = elf_data;
    ehdr->e_ident[EI_MAG0] = ELFMAG0;
    ehdr->e_ident[EI_MAG1] = ELFMAG1;
    ehdr->e_ident[EI_MAG2] = ELFMAG2;
    ehdr->e_ident[EI_MAG3] = ELFMAG3;
    ehdr->e_ident[EI_CLASS] = ELFCLASS64;
    ehdr->e_ident[EI_DATA] = ELFDATA2LSB;
    ehdr->e_ident[EI_VERSION] = EV_CURRENT;
    ehdr->e_ident[EI_OSABI] = ELFOSABI_NONE;
    ehdr->e_ident[EI_ABIVERSION] = 0;
    ehdr->e_type = ET_REL;
    ehdr->e_machine = EM_X86_64;
    ehdr->e_version = EV_CURRENT;
    ehdr->e_phoff = 0;
    ehdr->e_phentsize = 0;
    ehdr->e_phnum = 0;
    ehdr->e_shoff = sections_offset;
    ehdr->e_shentsize = sizeof(Elf64_Shdr);
    ehdr->e_shnum = SECTION_COUNT;
    ehdr->e_ehsize = sizeof(*ehdr);
    ehdr->e_shstrndx = SECTION_SHSTRTAB;

    // NULL section
    {
        Elf64_Shdr* shdr = &sections[SECTION_NULL];
        shdr->sh_name = null_str_offset;
        shdr->sh_type = SHT_NULL;
    }

    // .text
    {
        Elf64_Shdr* shdr = &sections[SECTION_TEXT];
        shdr->sh_name = text_str_offset;
        shdr->sh_type = SHT_PROGBITS;
        shdr->sh_flags = SHF_ALLOC | SHF_EXECINSTR;
        shdr->sh_addr = (uint64_t)code;
        shdr->sh_size = code_size;
        shdr->sh_addralign = 1 << 0;
    }

    // .symtab
    {
        Elf64_Shdr* shdr = &sections[SECTION_SYMTAB];
        shdr->sh_name = symtab_str_offset;
        shdr->sh_type = SHT_SYMTAB;
        shdr->sh_flags = SHF_ALLOC;
        shdr->sh_addr = (uint64_t)elf_data + symtab_offset;
        shdr->sh_offset = symtab_offset;
        shdr->sh_size = symtab_size;
        shdr->sh_link = SECTION_STRTAB;
        shdr->sh_info = symtab_size / sizeof(Elf64_Sym);
        shdr->sh_addralign = 1 << 0;
        shdr->sh_entsize = sizeof(Elf64_Sym);
    }

    // .strtab
    {
        Elf64_Shdr* shdr = &sections[SECTION_STRTAB];
        shdr->sh_name = strtab_str_offset;
        shdr->sh_type = SHT_STRTAB;
        shdr->sh_flags = SHF_ALLOC | SHF_STRINGS;
        shdr->sh_addr = (uint64_t)elf_data + strtab_offset;
        shdr->sh_offset = strtab_offset;
        shdr->sh_size = strtab_size;
        shdr->sh_addralign = 1 << 0;
        shdr->sh_entsize = 1;
    }

    // .shstrtab
    {
        Elf64_Shdr* shdr = &sections[SECTION_SHSTRTAB];
        shdr->sh_name = shstrtab_str_offset;
        shdr->sh_type = SHT_STRTAB;
        shdr->sh_flags = SHF_ALLOC | SHF_STRINGS;
        shdr->sh_addr = (uint64_t)elf_data + shstrtab_offset;
        shdr->sh_offset = shstrtab_offset;
        shdr->sh_size = shstrtab_size;
        shdr->sh_addralign = 1 << 0;
        shdr->sh_entsize = 1;
    }

    jit_gdb_register_elf(elf_data, elf_size);
}
