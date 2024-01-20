#pragma once

#include "pe.h"
#include "tomatodotnet/types/type.h"

typedef struct metadata_table_info {
    uint32_t row_count;
    void* entries;
} metadata_table_info_t;

typedef struct dotnet_file {
    // the PE file of this dotnet file
    pe_file_t file;

    // the metadata tables of the file
    union {
        metadata_table_info_t tables[64];
        struct {
            // 0x00
            uint32_t modules_count;
            metadata_module_t* modules;
            // 0x01
            uint32_t type_refs_count;
            metadata_type_ref_t* type_refs;
            // 0x02
            uint32_t type_defs_count;
            metadata_type_def_t* type_defs;
            // 0x03
            metadata_table_info_t _reserved1_0x03;
            // 0x04
            uint32_t fields_count;
            metadata_field_t* fields;
            // 0x05
            metadata_table_info_t _reserved_0x05;
            // 0x06
            uint32_t method_defs_count;
            metadata_method_def_t* method_defs;
            // 0x07
            metadata_table_info_t _reserved_0x07;
            // 0x08
            uint32_t params_count;
            metadata_param_t* params;
            // 0x09
            uint32_t interface_impls_count;
            metadata_interface_impl_t* interface_impls;
            // 0x0A
            uint32_t member_refs_count;
            metadata_member_ref_t* member_refs;
            // 0x0B
            uint32_t constants_count;
            metadata_constant_t* constants;
            // 0x0C
            uint32_t custom_attribute_count;
            metadata_custom_attribute_t* custom_attributes;
            // 0x0D
            metadata_table_info_t _reserved4;
            // 0x0E
            uint32_t decl_security_count;
            metadata_decl_security_t* decl_security;
            // 0x0F
            uint32_t class_layout_count;
            metadata_class_layout_t* class_layout;
            // 0x10
            uint32_t field_layout_count;
            metadata_field_layout_t* field_layout;
            // 0x11
            uint32_t stand_alone_sigs_count;
            metadata_stand_alone_sig_t* stand_alone_sigs;
            // 0x12
            uint32_t event_maps_count;
            metadata_event_map_t* event_maps;
            // 0x13
            metadata_table_info_t _reserved_0x13;
            // 0x14
            uint32_t events_count;
            metadata_event_t events;
            // 0x15
            uint32_t property_maps_count;
            metadata_property_map_t* property_maps;
            // 0x16
            metadata_table_info_t _reserved_0x16;
            // 0x17
            uint32_t properties_count;
            metadata_property_t* properties;
            // 0x18
            uint32_t method_semantics_count;
            metadata_method_semantics_t* method_semantics;
            // 0x19
            uint32_t method_impls_count;
            metadata_method_impl_t* method_impls;
            // 0x1A
            uint32_t module_refs_count;
            metadata_module_ref_t* module_refs;
            // 0x1B
            uint32_t type_specs_count;
            metadata_type_spec_t* type_specs;
            // 0x1C
            metadata_table_info_t _reserved_0x1C;
            // 0x1D
            uint32_t field_rvas_count;
            metadata_field_rva_t* field_rvas;
            // 0x1E
            metadata_table_info_t _reserved_0x1E;
            // 0x1F
            metadata_table_info_t _reserved_0x1F;
            // 0x20
            uint32_t assemblies_count;
            metadata_assembly_t* assemblies;
            // 0x21
            metadata_table_info_t _reserved_0x21;
            // 0x22
            metadata_table_info_t _reserved_0x22;
            // 0x23
            uint32_t assembly_refs_count;
            metadata_assembly_ref_t* assembly_refs;
            // 0x24
            metadata_table_info_t _reserved_0x24;
            // 0x25
            metadata_table_info_t _reserved_0x25;
            // 0x26
            metadata_table_info_t _reserved_0x26;
            // 0x27
            metadata_table_info_t _reserved_0x27;
            // 0x28
            metadata_table_info_t _reserved_0x28;
            // 0x29
            uint32_t nested_classes_count;
            metadata_nested_class_t* nested_classes;
            // 0x2A
            uint32_t generic_params_count;
            metadata_generic_param_t* generic_params;
            // 0x2B
            uint32_t method_specs_count;
            metadata_method_spec_t* method_specs;
            // 0x2C
            uint32_t generic_param_constraints_count;
            metadata_generic_param_constraint_t* generic_param_constraints;
        };
    };

    const char* strings;
    size_t strings_size;

    const uint8_t* us;
    size_t us_size;

    const uint8_t* blob;
    size_t blob_size;

    Guid* guids;
    size_t guids_count;

    int entry_point_token;
} dotnet_file_t;

tdn_err_t dotnet_load_file(dotnet_file_t* file);

void dotnet_free_file(dotnet_file_t* file);
