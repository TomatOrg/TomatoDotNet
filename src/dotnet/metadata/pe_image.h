#pragma once

#include "util/defs.h"

#define IMAGE_DOS_SIGNATURE 0x5A4D

typedef struct IMAGE_DOS_HEADER {
    uint16_t e_magic; // Magic number
    uint16_t e_cblp; // uint8_ts on last page of file
    uint16_t e_cp; // Pages in file
    uint16_t e_crlc; // Relocations
    uint16_t e_cparhdr; // Size of header in paragraphs
    uint16_t e_minalloc; // Minimum extra paragraphs needed
    uint16_t e_maxalloc; // Maximum extra paragraphs needed
    uint16_t e_ss; // Initial (relative) SS value
    uint16_t e_sp; // Initial SP value
    uint16_t e_csum; // Checksum
    uint16_t e_ip; // Initial IP value
    uint16_t e_cs; // Initial (relative) CS value
    uint16_t e_lfarlc; // Metadata address of relocation table
    uint16_t e_ovno; // Overlay number
    uint64_t e_res; // Reserved words
    uint16_t e_oemid; // OEM identifier (for e_oeminfo)
    uint16_t e_oeminfo; // OEM information; e_oemid specific
    uint64_t e_res2_0; // Reserved words
    uint64_t e_res2_1; // Reserved words
    uint32_t e_res2_2; // Reserved words
    uint32_t e_lfanew; // Metadata address of new exe header
} PACKED IMAGE_DOS_HEADER;

typedef struct IMAGE_FILE_HEADER {
    uint16_t Machine;
    uint16_t NumberOfSections;
    uint32_t TimeDateStamp;
    uint32_t PointerToSymbolTable;
    uint32_t NumberOfSymbols;
    uint16_t SizeOfOptionalHeader;
    uint16_t Characteristics;
} PACKED IMAGE_FILE_HEADER;

#define IMAGE_DIRECTORY_ENTRY_EXPORT 0
#define IMAGE_DIRECTORY_ENTRY_IMPORT 1
#define IMAGE_DIRECTORY_ENTRY_RESOURCE 2
#define IMAGE_DIRECTORY_ENTRY_EXCEPTION 3
#define IMAGE_DIRECTORY_ENTRY_SECURITY 4
#define IMAGE_DIRECTORY_ENTRY_BASERELOC 5
#define IMAGE_DIRECTORY_ENTRY_DEBUG 6
#define IMAGE_DIRECTORY_ENTRY_COPYRIGHT 7
#define IMAGE_DIRECTORY_ENTRY_GLOBALPTR 8
#define IMAGE_DIRECTORY_ENTRY_TLS 9
#define IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG 10
#define IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT 11
#define IMAGE_DIRECTORY_ENTRY_IAT 12
#define IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT 13
#define IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR 14

typedef struct IMAGE_DATA_DIRECTORY {
    uint32_t VirtualAddress;
    uint32_t Size;
} PACKED IMAGE_DATA_DIRECTORY;

#define IMAGE_NUMBEROF_DIRECTORY_ENTRIES 16
#define IMAGE_NT_OPTIONAL_HDR32_MAGIC 0x010B

typedef struct IMAGE_OPTIONAL_HEADER32 {
    uint16_t Magic;
    uint8_t MajorLinkerVersion;
    uint8_t MinorLinkerVersion;
    uint32_t SizeOfCode;
    uint32_t SizeOfInitializedData;
    uint32_t SizeOfUninitializedData;
    uint32_t AddressOfEntryPoint;
    uint32_t BaseOfCode;
    uint32_t BaseOfData;
    uint32_t ImageBase;
    uint32_t SectionAlignment;
    uint32_t FileAlignment;
    uint16_t MajorOperatingSystemVersion;
    uint16_t MinorOperatingSystemVersion;
    uint16_t MajorImageVersion;
    uint16_t MinorImageVersion;
    uint16_t MajorSubsystemVersion;
    uint16_t MinorSubsystemVersion;
    uint32_t Win32VersionValue;
    uint32_t SizeOfImage;
    uint32_t SizeOfHeaders;
    uint32_t CheckSum;
    uint16_t Subsystem;
    uint16_t DllCharacteristics;
    uint32_t SizeOfStackReserve;
    uint32_t SizeOfStackCommit;
    uint32_t SizeOfHeapReserve;
    uint32_t SizeOfHeapCommit;
    uint32_t LoaderFlags;
    uint32_t NumberOfRvaAndSizes;
    IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
} PACKED IMAGE_OPTIONAL_HEADER32;

#define IMAGE_NT_SIGNATURE 0x00004550

typedef struct IMAGE_NT_HEADERS32 {
    uint32_t Signature;
    IMAGE_FILE_HEADER FileHeader;
    IMAGE_OPTIONAL_HEADER32 OptionalHeader;
} PACKED IMAGE_NT_HEADERS32;

#define COMIMAGE_FLAGS_ILONLY 0x00000001
#define COMIMAGE_FLAGS_32BITREQUIRED 0x00000002
#define COMIMAGE_FLAGS_STRONGNAMESIGNED 0x00000008
#define COMIMAGE_FLAGS_NATIVE_ENTRYPOINT 0x00000010
#define COMIMAGE_FLAGS_TRACKDEBUGDATA 0x00010000

typedef struct IMAGE_COR20_HEADER {
    uint32_t cb;
    uint16_t MajorRuntimeVersion;
    uint16_t MinorRuntimeVersion;
    IMAGE_DATA_DIRECTORY MetaData;
    uint32_t Flags;
    uint32_t EntryPointToken;
    IMAGE_DATA_DIRECTORY Resources;
    IMAGE_DATA_DIRECTORY StrongNameSignature;
    IMAGE_DATA_DIRECTORY CodeManagerTable;
    IMAGE_DATA_DIRECTORY VTableFixups;
    IMAGE_DATA_DIRECTORY ExportAddressTableJumps;
    IMAGE_DATA_DIRECTORY ManagedNativeHeader;
} PACKED IMAGE_COR20_HEADER;

#define IMAGE_SIZEOF_SHORT_NAME 8

typedef struct IMAGE_SECTION_HEADER {
    char Name[IMAGE_SIZEOF_SHORT_NAME];
    union {
        uint32_t PhysicalAddress;
        uint32_t VirtualSize;
    } Misc;
    uint32_t VirtualAddress;
    uint32_t SizeOfRawData;
    uint32_t PointerToRawData;
    uint32_t PointerToRelocations;
    uint32_t PointerToLinenumbers;
    uint16_t NumberOfRelocations;
    uint16_t NumberOfLinenumbers;
    uint32_t Characteristics;
} PACKED IMAGE_SECTION_HEADER;
