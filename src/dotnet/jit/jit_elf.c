#include "jit_elf.h"

#include <stdalign.h>

#include "jit.h"
#include "jit_helpers.h"
#include "dotnet/types.h"
#include "spidir/x64.h"
#include "tomatodotnet/types/type.h"
#include "tomatodotnet/util/stb_ds.h"
#include "util/defs.h"
#include "util/except.h"
#include "util/string.h"
#include "util/elf64.h"
#include "util/string_builder.h"


static char* strcpy(char* strDest, const char* strSrc) {
    char* temp = strDest;
    while ((*strDest++ = *strSrc++) != '\0');
    return temp;
}

static void mangle_push_string(string_builder_t* builder, String str) {
    string_builder_push_uint(builder, str->Length);
    string_builder_push_string(builder, str);
}

static void mangle_push_name(string_builder_t* builder, String str, bool thunk) {
    string_builder_push_uint(builder, str->Length + (thunk ? strlen("$thunk") : 0));
    string_builder_push_string(builder, str);
    if (thunk) {
        string_builder_push_cstr(builder, "$thunk");
    }
}

static void mangle_push_sub_string(string_builder_t* builder, String str, size_t offset, size_t count) {
    string_builder_push_uint(builder, count);
    for (size_t i = offset; i < offset + count; i++) {
        string_builder_push_char(builder, str->Chars[i]);
    }
}

static void mangle_push_namespace(string_builder_t* builder, String str) {
    int start = 0;
    for (int i = 0; i < str->Length; i++) {
        if (str->Chars[i] == '.') {
            mangle_push_sub_string(builder, str, start, i - start);
            start = i + 1;
        }
    }

    // push the last part
    mangle_push_sub_string(builder, str, start, str->Length - start);
}

static void mangle_push_type(string_builder_t* builder, RuntimeTypeInfo type);

static void mangle_push_class(string_builder_t* builder, RuntimeTypeInfo type) {
    string_builder_push_char(builder, 'N');
    if (type->Namespace != NULL) {
        mangle_push_namespace(builder, type->Namespace);
    }
    mangle_push_string(builder, type->Name);
    if (type->GenericArguments != NULL) {
        string_builder_push_char(builder, 'I');
        for (int i = 0; i < type->GenericArguments->Length; i++) {
            mangle_push_type(builder, type->GenericArguments->Elements[i]);
        }
        string_builder_push_char(builder, 'E');
    }
}

static void mangle_push_type(string_builder_t* builder, RuntimeTypeInfo type) {
    if (type == tVoid) string_builder_push_char(builder, 'v');
    else if (type == tChar) string_builder_push_cstr(builder, "Ds");
    else if (type == tSByte) string_builder_push_char(builder, 'a');
    else if (type == tByte) string_builder_push_char(builder, 'h');
    else if (type == tInt16) string_builder_push_char(builder, 's');
    else if (type == tUInt16) string_builder_push_char(builder, 't');
    else if (type == tInt32) string_builder_push_char(builder, 'i');
    else if (type == tUInt32) string_builder_push_char(builder, 'j');
    else if (type == tInt64) string_builder_push_char(builder, 'x');
    else if (type == tUInt64) string_builder_push_char(builder, 'y');
    else if (type == tIntPtr) string_builder_push_char(builder, 'l');
    else if (type == tUIntPtr) string_builder_push_char(builder, 'm');
    else if (type == tSingle) string_builder_push_char(builder, 'f');
    else if (type == tDouble) string_builder_push_char(builder, 'd');
    else if (type == tBoolean) string_builder_push_char(builder, 'b');
    else if (type->IsPointer) {
        string_builder_push_char(builder, 'P');
        mangle_push_type(builder, type->ElementType);
    } else if (type->IsByRef) {
        string_builder_push_char(builder, 'R');
        mangle_push_type(builder, type->ElementType);
    } else {
        if (tdn_type_is_referencetype(type)) {
            // classes are considered passed by pointer
            string_builder_push_char(builder, 'P');
        } else {
            // meanwhile structs are considered pass by ref
            string_builder_push_char(builder, 'R');
        }
        mangle_push_class(builder, type);
        string_builder_push_char(builder, 'E');
    }
}

static void mangle_method_name(string_builder_t* builder, RuntimeMethodBase method, bool thunk) {
    RuntimeTypeInfo type = method->DeclaringType;

    // the start of the encoding
    string_builder_push_cstr(builder, "_Z");

    // encode the class of the method
    mangle_push_class(builder, type);

    // push the method name
    mangle_push_name(builder, method->Name, thunk);
    string_builder_push_char(builder, 'E');

    // Parameters
    if (method->Parameters->Length == 0) {
        string_builder_push_char(builder, 'v');
    } else {
        for (int i = 0; i < method->Parameters->Length; i++) {
            mangle_push_type(builder, method->Parameters->Elements[i]->ParameterType);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Elf emitting API
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef enum object_section {
    SECTION_NULL,
    SECTION_STRTAB,
    SECTION_SHSTRTAB,
    SECTION_TEXT,
    SECTION_RELA_TEXT,
    SECTION_RODATA,
    SECTION_SYMTAB,
    SECTION_COUNT
} object_section_t;

typedef struct jit_elf {
    void* symtab;
    void* strtab;
    void* shstrtab;

    void* text;

    void* rela_text;

    void* rodata;
    size_t rodata_align;

    struct {
        spidir_funcref_t key;
        int value;
    }* symbols;

    int symbol_count;
} jit_elf_t;

static jit_elf_t m_elf = {};

void jit_elf_start_emit() {
    // NULL symbol
    {
        Elf64_Sym* sym = arraddnptr(m_elf.symtab, sizeof(*sym));
        memset(sym, 0, sizeof(*sym));
        sym->st_name = arraddnindex(m_elf.strtab, strlen("")+1);
        sym->st_value = 0;
        sym->st_size = 0;
        sym->st_info = ELF64_ST_INFO(STB_LOCAL, STT_NOTYPE);
        sym->st_other = STV_DEFAULT;
        sym->st_shndx = 0;
        strcpy(m_elf.strtab + sym->st_name, "");
        m_elf.symbol_count++;
    }
}

static int create_or_update_symbol(spidir_funcref_t func) {
    // setup the symbol for this entry
    Elf64_Sym* sym = NULL;
    int idx = hmgeti(m_elf.symbols, func);
    int symbol_index;
    if (idx == -1) {
        // does not exist yet, create the new symbol
        sym = arraddnptr(m_elf.symtab, sizeof(*sym));
        memset(sym, 0, sizeof(*sym));

        RuntimeMethodBase target = jit_get_method_from_function(func);
        bool thunk = false;
        if (target == NULL) {
            target = jit_get_thunk_method(func);
            if (target != NULL) {
                thunk = true;
            }
        }

        // create the name
        string_builder_t builder = {};
        if (target == NULL) {
            // runtime helper
            const char* name = jit_get_helper_name(func);
            ASSERT(name != NULL);
            string_builder_push_cstr(&builder, name);
        } else {
            // real object
            mangle_method_name(&builder, target, thunk);
        }
        const char* name = string_builder_build(&builder);
        int name_offset = arraddnindex(m_elf.strtab, strlen(name)+1);
        strcpy(m_elf.strtab + name_offset, name);
        string_builder_free(&builder);

        sym->st_name = name_offset;

        symbol_index = m_elf.symbol_count++;
        hmput(m_elf.symbols, func, symbol_index);
    } else {
        // get the existing symbol
        symbol_index = m_elf.symbols[idx].value;
        sym = &((Elf64_Sym*)m_elf.symtab)[symbol_index];
    }

    // initialize as an external symbol for now
    sym->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_FUNC);
    sym->st_other = STV_DEFAULT;
    sym->st_shndx = SHN_UNDEF;

    return symbol_index;
}

void jit_elf_add_entry(jit_codegen_entry_t* entry) {
    // align data section
    size_t alignment = (arrlen(m_elf.text) % 16);
    if (alignment != 0) {
        alignment = 16 - alignment;
        void* ptr = arraddnptr(m_elf.text, alignment);
        memset(ptr, 0xCC, alignment);
    }

    // copy the code
    size_t code_size = spidir_codegen_blob_get_code_size(entry->blob);
    int code_offset = arraddnindex(m_elf.text, code_size);
    memcpy(m_elf.text + code_offset, spidir_codegen_blob_get_code(entry->blob), code_size);

    // // copy the constant pool
    // // TODO: constpool alignment
    // size_t costpool_size = spidir_codegen_blob_get_constpool_size(entry->blob);
    // int constpool_offset = arraddnindex(m_elf.rodata, costpool_size);
    // memcpy(m_elf.rodata + constpool_offset, spidir_codegen_blob_get_constpool(entry->blob), costpool_size);

    // finish setting up the symbol
    int symbol_index = create_or_update_symbol(spidir_funcref_make_internal(entry->key));
    Elf64_Sym* sym = &((Elf64_Sym*)m_elf.symtab)[symbol_index];
    sym->st_value = code_offset;
    sym->st_size = code_size;
    sym->st_shndx = SECTION_TEXT;

    // now emit the relocations
    const spidir_codegen_reloc_t* relocs = spidir_codegen_blob_get_relocs(entry->blob);
    for (int i = 0; i < spidir_codegen_blob_get_reloc_count(entry->blob); i++) {
        const spidir_codegen_reloc_t* reloc = &relocs[i];

        Elf64_Rela* rela = arraddnptr(m_elf.rela_text, sizeof(*rela));
        memset(rela, 0, sizeof(*rela));
        rela->r_offset = code_offset + reloc->offset;
        rela->r_addend = reloc->addend;


        Elf64_Xword sym = 0;
        switch (reloc->target_kind) {
            case SPIDIR_RELOC_TARGET_INTERNAL_FUNCTION: {
                sym = create_or_update_symbol(spidir_funcref_make_internal(reloc->target.internal));
            } break;

            case SPIDIR_RELOC_TARGET_EXTERNAL_FUNCTION: {
                sym = create_or_update_symbol(spidir_funcref_make_external(reloc->target.external));
            } break;

            case SPIDIR_RELOC_TARGET_CONSTPOOL: {
                // how do we reference the constpool section?? do we need to have
                // a symbol for this to work???
            } break;
        }

        switch (reloc->kind) {
            case SPIDIR_RELOC_X64_PC32: {
                rela->r_info = ELF64_R_INFO(sym, 2);
            } break;

            case SPIDIR_RELOC_X64_ABS64: {
                rela->r_info = ELF64_R_INFO(sym, 1);
            } break;

            default:
                ASSERT(!"Invalid relocation kind");
        }
    }
}

void jit_elf_emit() {
    // reserve section names
    int null_str_offset = arraddnindex(m_elf.shstrtab, strlen("")+1);
    strcpy(m_elf.shstrtab + null_str_offset, "");
    int text_str_offset = arraddnindex(m_elf.shstrtab, strlen(".text")+1);
    strcpy(m_elf.shstrtab + text_str_offset, ".text");
    int rela_text_str_offset = arraddnindex(m_elf.shstrtab, strlen(".rela.text")+1);
    strcpy(m_elf.shstrtab + rela_text_str_offset, ".rela.text");
    int rodata_str_offset = arraddnindex(m_elf.shstrtab, strlen(".rodata")+1);
    strcpy(m_elf.shstrtab + rodata_str_offset, ".rodata");
    int symtab_str_offset = arraddnindex(m_elf.shstrtab, strlen(".symtab")+1);
    strcpy(m_elf.shstrtab + symtab_str_offset, ".symtab");
    int strtab_str_offset = arraddnindex(m_elf.shstrtab, strlen(".strtab")+1);
    strcpy(m_elf.shstrtab + strtab_str_offset, ".strtab");
    int shstrtab_str_offset = arraddnindex(m_elf.shstrtab, strlen(".shstrtab")+1);
    strcpy(m_elf.shstrtab + shstrtab_str_offset, ".shstrtab");


    // calculate the elf size
    size_t elf_size = sizeof(Elf64_Ehdr);

    size_t text_offset = elf_size;
    elf_size += arrlen(m_elf.text);

    size_t rela_text_offset = elf_size;
    elf_size += arrlen(m_elf.rela_text);

    size_t rodata_offset = elf_size;
    elf_size += arrlen(m_elf.rodata);

    size_t symtab_offset = elf_size;
    elf_size += arrlen(m_elf.symtab);

    size_t strtab_offset = elf_size;
    elf_size += arrlen(m_elf.strtab);

    size_t shstrtab_offset = elf_size;
    elf_size += arrlen(m_elf.shstrtab);

    size_t sections_offset = elf_size;
    elf_size += sizeof(Elf64_Shdr) * SECTION_COUNT;

    // allocate it all in one
    void* elf_data = tdn_host_mallocz(elf_size, 16);
    ASSERT(elf_data != NULL);

    memcpy(elf_data + text_offset, m_elf.text, arrlen(m_elf.text));
    size_t text_size = arrlen(m_elf.text);
    arrfree(m_elf.text);

    memcpy(elf_data + rela_text_offset, m_elf.rela_text, arrlen(m_elf.rela_text));
    size_t rela_text_size = arrlen(m_elf.rela_text);
    arrfree(m_elf.rela_text);

    memcpy(elf_data + rodata_offset, m_elf.rodata, arrlen(m_elf.rodata));
    size_t rodata_size = arrlen(m_elf.rodata);
    arrfree(m_elf.rodata);

    // copy all of the actual data into the elf
    memcpy(elf_data + symtab_offset, m_elf.symtab, arrlen(m_elf.symtab));
    size_t symtab_size = arrlen(m_elf.symtab);
    arrfree(m_elf.symtab);

    memcpy(elf_data + strtab_offset, m_elf.strtab, arrlen(m_elf.strtab));
    size_t strtab_size = arrlen(m_elf.strtab);
    arrfree(m_elf.strtab);

    memcpy(elf_data + shstrtab_offset, m_elf.shstrtab, arrlen(m_elf.shstrtab));
    size_t shstrtab_size = arrlen(m_elf.shstrtab);
    arrfree(m_elf.shstrtab);

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

    // .strtab
    {
        Elf64_Shdr* shdr = &sections[SECTION_STRTAB];
        shdr->sh_name = strtab_str_offset;
        shdr->sh_type = SHT_STRTAB;
        shdr->sh_offset = strtab_offset;
        shdr->sh_size = strtab_size;
        shdr->sh_addralign = 1;
    }

    // .shstrtab
    {
        Elf64_Shdr* shdr = &sections[SECTION_SHSTRTAB];
        shdr->sh_name = shstrtab_str_offset;
        shdr->sh_type = SHT_STRTAB;
        shdr->sh_offset = shstrtab_offset;
        shdr->sh_size = shstrtab_size;
        shdr->sh_addralign = 1;
    }

    // .text
    {
        Elf64_Shdr* shdr = &sections[SECTION_TEXT];
        shdr->sh_name = text_str_offset;
        shdr->sh_type = SHT_PROGBITS;
        shdr->sh_flags = SHF_ALLOC | SHF_EXECINSTR;
        shdr->sh_offset = text_offset;
        shdr->sh_size = text_size;
        shdr->sh_addralign = 16;
    }

    // .rela.text
    {
        Elf64_Shdr* shdr = &sections[SECTION_RELA_TEXT];
        shdr->sh_name = rela_text_str_offset;
        shdr->sh_type = SHT_RELA;
        shdr->sh_flags = SHF_INFO_LINK;
        shdr->sh_offset = rela_text_offset;
        shdr->sh_size = rela_text_size;
        shdr->sh_info = SECTION_TEXT;
        shdr->sh_link = SECTION_SYMTAB;
        shdr->sh_entsize = sizeof(Elf64_Rela);
        shdr->sh_addralign = 8;
    }

    // .rodata
    {
        Elf64_Shdr* shdr = &sections[SECTION_RODATA];
        shdr->sh_name = rodata_str_offset;
        shdr->sh_type = SHT_PROGBITS;
        shdr->sh_flags = SHF_ALLOC;
        shdr->sh_offset = rodata_offset;
        shdr->sh_size = rodata_size;
        shdr->sh_addralign = m_elf.rodata_align;
    }

    // .symtab
    {
        Elf64_Shdr* shdr = &sections[SECTION_SYMTAB];
        shdr->sh_name = symtab_str_offset;
        shdr->sh_type = SHT_SYMTAB;
        shdr->sh_flags = SHF_ALLOC;
        shdr->sh_offset = symtab_offset;
        shdr->sh_size = symtab_size;
        shdr->sh_link = SECTION_STRTAB;
        shdr->sh_info = symtab_size / sizeof(Elf64_Sym);
        shdr->sh_addralign = 8;
        shdr->sh_entsize = sizeof(Elf64_Sym);
    }

    void* ctx = tdn_host_jit_start_dump(TDN_JIT_DUMP_ELF);
    tdn_host_jit_dump_callback(elf_data, elf_size, ctx);
    tdn_host_jit_end_dump(ctx);
    tdn_host_free(elf_data);
    hmfree(m_elf.symbols);
    memset(&m_elf, 0, sizeof(m_elf));
}
