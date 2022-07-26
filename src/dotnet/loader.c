
#include "metadata/sig_spec.h"
#include "metadata/sig.h"
#include "jit/jit.h"
#include "gc/gc.h"

#include <util/stb_ds.h>

#include "time/tsc.h"
#include "exception.h"
#include "loader.h"
#include "encoding.h"
#include "opcodes.h"
#include "monitor.h"
#include "thread/scheduler.h"

#include <stdalign.h>
#include <string.h>
#include <stdlib.h>

System_Reflection_Assembly g_corelib = NULL;

// TODO: we really need a bunch of constants for the flags for better readability

static err_t validate_token(token_t* token, metadata_t* metadata, bool allow_null) {
    err_t err = NO_ERROR;

    CHECK(token->table <= ARRAY_LEN(metadata->tables));
    CHECK(metadata->tables[token->table].table != NULL);
    CHECK(token->index <= metadata->tables[token->table].rows);

    if (!allow_null) {
        CHECK(token->index != 0);
    }

cleanup:
    return err;
}

static err_t validate_metadata_assembly(pe_file_t* ctx, metadata_t* metadata) {
    err_t err = NO_ERROR;

    CHECK(metadata->tables[METADATA_ASSEMBLY].rows == 1);

    metadata_assembly_t* assembly = metadata->tables[METADATA_ASSEMBLY].table;
    CHECK(assembly->name[0] != '\0');

cleanup:
    return err;
}

static err_t validate_metadata_module(pe_file_t* ctx, metadata_t* metadata) {
    err_t err = NO_ERROR;

    CHECK(metadata->tables[METADATA_MODULE].rows == 1);

    metadata_module_t* module = metadata->tables[METADATA_MODULE].table;
    CHECK(module->name[0] != '\0');

cleanup:
    return err;
}

/**
 * Any simple validations are made in here before the CIL is even parsed
 */
static err_t validate_metadata(pe_file_t* ctx, metadata_t* metadata) {
    err_t err = NO_ERROR;

    CHECK_AND_RETHROW(validate_metadata_assembly(ctx, metadata));

cleanup:
    return err;
}

static err_t decode_metadata(pe_file_t* ctx, metadata_t* metadata) {
    err_t err = NO_ERROR;
    void* metadata_root = NULL;

    // get the metadata
    metadata_root = pe_get_rva_data(ctx, ctx->cli_header->metadata);
    CHECK_ERROR(metadata_root != NULL, ERROR_NOT_FOUND);

    // parse it
    CHECK_AND_RETHROW(metadata_parse(ctx, metadata_root, ctx->cli_header->metadata.size, metadata));

    // do basic validations
    CHECK_AND_RETHROW(validate_metadata(ctx, metadata));

cleanup:
    // we no longer need this
    free(metadata_root);
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// All the basic type setup
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static err_t parse_method_cil(System_Reflection_MethodInfo method, blob_entry_t sig, pe_file_t* file, metadata_t* metadata) {
    err_t err = NO_ERROR;

    System_Reflection_MethodBody body = UNSAFE_GC_NEW(tSystem_Reflection_MethodBody);

    // set it
    GC_UPDATE(method, MethodBody, body);

    // get the signature table
    metadata_stand_alone_sig_t* standalone_sigs = metadata->tables[METADATA_STAND_ALONE_SIG].table;
    int standalone_sigs_count = metadata->tables[METADATA_STAND_ALONE_SIG].rows;

    // get the header type
    CHECK(sig.size > 0);
    uint8_t header_type = sig.data[0];

    if ((header_type & 0b11) == CorILMethod_FatFormat) {
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // fat format header
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // fetch the header in its full
        method_fat_format_t* header = (method_fat_format_t*) (sig.data);
        CHECK(sizeof(method_fat_format_t) <= sig.size);
        CHECK(header->size * 4 <= sig.size);

        // skip the rest of the header
        sig.data += header->size * 4;
        sig.size -= header->size * 4;

        // set the init locals flag
        body->InitLocals = header->flags & CorILMethod_InitLocals;

        // variables
        if (header->local_var_sig_tok.token != 0) {
            CHECK(header->local_var_sig_tok.table == METADATA_STAND_ALONE_SIG);
            CHECK(header->local_var_sig_tok.index > 0);
            CHECK(header->local_var_sig_tok.index <= standalone_sigs_count);
            blob_entry_t signature = standalone_sigs[header->local_var_sig_tok.index - 1].signature;
            CHECK_AND_RETHROW(parse_local_var_sig(signature, method, file, metadata));
        } else {
            // empty array for ease of use
            GC_UPDATE(body, LocalVariables, GC_NEW_ARRAY(tSystem_Reflection_LocalVariableInfo, 0));
        }

        // copy some info
        body->MaxStackSize = header->max_stack;

        // copy the il
        CHECK(header->code_size <= sig.size);
        GC_UPDATE(body, Il, GC_NEW_ARRAY(tSystem_Byte, header->code_size));
        memcpy(body->Il->Data, sig.data, body->Il->Length);
        sig.size -= header->code_size;
        sig.data += header->code_size;

        // process method sections
        bool more_sect = header->flags & CorILMethod_MoreSects;
        while (more_sect) {
            // align the data so we can handle the next section
            size_t diff = sig.size - ALIGN_DOWN(sig.size, 4);
            CHECK(diff <= sig.size);
            sig.size -= diff;
            sig.data += diff;

            // get the flags of the section
            CHECK(2 <= sig.size);
            uint8_t flags = sig.data[0];

            // get the section header
            size_t section_size = 0;
            if (flags & CorILMethod_Sect_FatFormat) {
                CHECK(4 <= sig.size);
                method_section_fat_t* section = (method_section_fat_t*) sig.data;
                sig.data += sizeof(method_section_fat_t);
                sig.size -= sizeof(method_section_fat_t);
                section_size = section->size;
            } else {
                method_section_tiny_t* section = (method_section_tiny_t*)sig.data;
                sig.data += sizeof(method_section_tiny_t);
                sig.size -= sizeof(method_section_tiny_t);
                section_size = section->size;
            }

            // verify we have the whole section, so we don't need to worry about it later on
            CHECK(section_size <= sig.size);

            // check the type
            uint8_t kind = flags & CorILMethod_Sect_KindMask;
            switch (kind) {
                case CorILMethod_Sect_EHTable: {
                    CHECK(body->ExceptionHandlingClauses == NULL);
                    size_t count = 0;
                    if (flags & CorILMethod_Sect_FatFormat) {
                        // fat exception table
                        count = (section_size - 4) / 24;
                    } else {
                        // non-fat exception table
                        // skip 2 reserved bytes
                        CHECK(2 <= sig.size);
                        sig.size -= 2;
                        sig.data += 2;
                        count = (section_size - 4) / 12;
                    }

                    // allocate it
                    GC_UPDATE(body, ExceptionHandlingClauses, GC_NEW_ARRAY(tSystem_Reflection_ExceptionHandlingClause, count));

                    // parse it
                    for (int i = 0; i < count; i++) {
                        System_Reflection_ExceptionHandlingClause clause = UNSAFE_GC_NEW(tSystem_Reflection_ExceptionHandlingClause);
                        GC_UPDATE_ARRAY(body->ExceptionHandlingClauses, i, clause);

                        if (flags & CorILMethod_Sect_FatFormat) {
                            // fat clause
                            method_fat_exception_clause_t* ec = (method_fat_exception_clause_t*)sig.data;
                            sig.size -= sizeof(method_section_fat_t);
                            sig.data += sizeof(method_section_fat_t);
                            if (ec->flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
                                System_Type type;
                                CHECK_AND_RETHROW(assembly_get_type_by_token(method->Module->Assembly, ec->class_token,
                                                                             method->DeclaringType->GenericArguments, method->GenericArguments, &type));
                                GC_UPDATE(clause, CatchType, type);
                                CHECK_AND_RETHROW(loader_setup_type(file, metadata, type));
                            } else if (ec->flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
                                clause->FilterOffset = ec->filter_offset;
                            }
                            clause->Flags = ec->flags;
                            clause->HandlerLength = ec->handler_length;
                            clause->HandlerOffset = ec->handler_offset;
                            clause->TryLength = ec->try_length;
                            clause->TryOffset = ec->try_offset;
                        } else {
                            // small clause
                            method_exception_clause_t* ec = (method_exception_clause_t*)sig.data;
                            sig.size -= sizeof(method_exception_clause_t);
                            sig.data += sizeof(method_exception_clause_t);
                            if (ec->flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
                                System_Type type;
                                CHECK_AND_RETHROW(assembly_get_type_by_token(method->Module->Assembly, ec->class_token,
                                                                             method->DeclaringType->GenericArguments, method->GenericArguments, &type));
                                GC_UPDATE(clause, CatchType, type);
                                CHECK_AND_RETHROW(loader_setup_type(file, metadata, type));
                            } else if (ec->flags == COR_ILEXCEPTION_CLAUSE_FILTER) {
                                clause->FilterOffset = ec->filter_offset;
                            }
                            clause->Flags = ec->flags;
                            clause->HandlerLength = ec->handler_length;
                            clause->HandlerOffset = ec->handler_offset;
                            clause->TryLength = ec->try_length;
                            clause->TryOffset = ec->try_offset;
                        }

                        // check the type
                        // TODO: is this a bit field or not? I can't figure out
                        CHECK(
                            clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION ||
                            clause->Flags == COR_ILEXCEPTION_CLAUSE_FILTER ||
                            clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY ||
                            clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT
                        );

                        // check offsets
                        CHECK(clause->HandlerOffset < header->code_size);
                        CHECK(clause->HandlerOffset + clause->HandlerLength < header->code_size);
                        CHECK(clause->TryOffset < header->code_size);
                        CHECK(clause->TryOffset + clause->TryLength < header->code_size);

                        // TODO: check for overlaps

                        // make sure handler comes after try
                        CHECK(clause->TryOffset < clause->HandlerOffset);
                    }
                } break;

                default: {
                    CHECK_FAIL("Invalid section kind: %x", kind);
                } break;
            }

            // check for more sections
            more_sect = flags & CorILMethod_Sect_MoreSects;
        }

        // an empty arrays if there are no exceptions
        if (body->ExceptionHandlingClauses == NULL) {
            GC_UPDATE(body, ExceptionHandlingClauses, GC_NEW_ARRAY(tSystem_Reflection_ExceptionHandlingClause, 0));
        }
    } else if ((header_type & 0b11) == CorILMethod_TinyFormat) {
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // tiny format header
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // the size is known to be good since it is a single byte
        method_tiny_format_t* header = (method_tiny_format_t*) (sig.data);

        // skip the rest of the header
        sig.size--;
        sig.data++;

        // no local variables
        GC_UPDATE(body, LocalVariables, GC_NEW_ARRAY(tSystem_Reflection_LocalVariableInfo, 0));

        // no exceptions
        GC_UPDATE(body, ExceptionHandlingClauses, GC_NEW_ARRAY(tSystem_Reflection_ExceptionHandlingClause, 0));

        // set the default options
        body->MaxStackSize = 8;

        // copy the il
        CHECK(header->size <= sig.size);
        GC_UPDATE(body, Il, GC_NEW_ARRAY(tSystem_Byte, header->size));
        memcpy(body->Il->Data, sig.data, body->Il->Length);
    } else {
        CHECK_FAIL("Invalid method format");
    }

cleanup:
    return err;
}

static err_t set_class_layout(System_Reflection_Assembly assembly, metadata_t* metadata) {
    err_t err = NO_ERROR;

    metadata_class_layout_t* class_layouts = metadata->tables[METADATA_CLASS_LAYOUT].table;
    for (int i = 0; i < metadata->tables[METADATA_CLASS_LAYOUT].rows; i++) {
        metadata_class_layout_t* class_layout = &class_layouts[i];
        System_Type type;
        CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, class_layout->parent, NULL, NULL, &type));
        CHECK(type != NULL);

        // layout must be seq or explicit to have a row in this class
        type_layout_t layout = type_layout(type);
        CHECK(layout == TYPE_SEQUENTIAL_LAYOUT || layout == TYPE_EXPLICIT_LAYOUT);

        // set the class size, checking it will be done later on
        type->ClassSize = class_layout->class_size;

        // can only have packing size on seq layout
        if (class_layout->packing_size != 0) {
            CHECK(layout == TYPE_SEQUENTIAL_LAYOUT);
            type->PackingSize = class_layout->packing_size;

            // make sure it is valid
            switch (type->PackingSize) {
                case 0:
                case 1:
                case 2:
                case 4:
                case 8:
                case 16:
                case 32:
                case 64:
                case 128:
                    break;
                default:
                    CHECK_FAIL();
            }
        }
    }

cleanup:
    return err;
}

static err_t loader_load_type_refs(System_Reflection_Assembly assembly, metadata_t* metadata) {
    err_t err = NO_ERROR;


    metadata_assembly_ref_t* assembly_refs = metadata->tables[METADATA_ASSEMBLY_REF].table;
    size_t assembly_refs_count = metadata->tables[METADATA_ASSEMBLY_REF].rows;

    //
    // resolve all external types which are assembly ref, this makes it easier
    // for resolving other stuff later on
    //

    metadata_type_ref_t* type_refs = metadata->tables[METADATA_TYPE_REF].table;
    size_t type_refs_count = metadata->tables[METADATA_TYPE_REF].rows;

    GC_UPDATE(assembly, ImportedTypes, GC_NEW_ARRAY(tSystem_Type, type_refs_count));

    for (int i = 0; i < type_refs_count; i++) {
        metadata_type_ref_t* type_ref = &type_refs[i];
        if (type_ref->resolution_scope.table != METADATA_ASSEMBLY_REF) continue;

        // get the ref
        CHECK(0 < type_ref->resolution_scope.index && type_ref->resolution_scope.index <= assembly_refs_count);
        metadata_assembly_ref_t* assembly_ref = &assembly_refs[type_ref->resolution_scope.index - 1];

        // resolve the assembly
        System_Reflection_Assembly refed = NULL;
        if (string_equals_cstr(g_corelib->Name, assembly_ref->name)) {
            refed = g_corelib;
        } else {
            // TODO: properly load anything which is not loaded
            CHECK_FAIL();
        }
        CHECK(refed != NULL);

        // Validate the version we have loaded
        //  - majors:   must match, indicate a breaking change in the API
        //  - minor:    must be higher, indicates a compatible change in the API
        //  - build:    must be higher, indicates a bugfix/improvement
        //  - revision: ignored, mostly used as a unique id for the build so the
        //              exact release source can be found
        CHECK(refed->MajorVersion == assembly_ref->major_version);
        CHECK(refed->MinorVersion >= assembly_ref->minor_version);
        CHECK(refed->BuildNumber >= assembly_ref->build_number);

        // get the type
        System_Type refed_type = assembly_get_type_by_name(refed, type_ref->type_name, type_ref->type_namespace);
        CHECK(refed_type != NULL, "Missing type `%s.%s` in assembly `%s`", type_ref->type_namespace, type_ref->type_name, assembly_ref->name);

        // store it
        GC_UPDATE_ARRAY(assembly->ImportedTypes, i, refed_type);
    }

    //
    // now continue to other types
    //

    for (int i = 0; i < type_refs_count; i++) {
        metadata_type_ref_t* type_ref = &type_refs[i];
        System_Type resolved_type = NULL;

        switch (type_ref->resolution_scope.table) {
            // already handled
            case METADATA_ASSEMBLY_REF: continue;

            case METADATA_TYPE_REF: {
                // get the parent type (hopefully already got resolved)
                CHECK(type_ref->resolution_scope.index > 0 && type_ref->resolution_scope.index - 1 < assembly->ImportedTypes->Length);
                System_Type type = assembly->ImportedTypes->Data[type_ref->resolution_scope.index - 1];
                CHECK(type != NULL);

                // go over all the nested types and search for the correct one
                System_Type nested = type->NestedTypes;
                while (nested != NULL) {
                    if (string_equals_cstr(nested->Name, type_ref->type_name)) {
                        resolved_type = nested;
                        break;
                    }
                    nested = nested->NextNestedType;
                }
                CHECK(resolved_type != NULL);
            } break;

            default:
                CHECK_FAIL("Invalid resolution scope %02x", type_ref->resolution_scope.table);
        }

        GC_UPDATE_ARRAY(assembly->ImportedTypes, i, resolved_type);
    }

    // validate we got all the types before we continue to members
    for (int i = 0; i < assembly->ImportedTypes->Length; i++) {
        CHECK(assembly->ImportedTypes->Data[i] != NULL);
    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Setup type information, before feeling everything
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

err_t loader_setup_type(pe_file_t* file, metadata_t* metadata, System_Type type) {
    err_t err = NO_ERROR;
    metadata_type_def_t* type_def = &((metadata_type_def_t*)metadata->tables[METADATA_TYPE_DEF].table)[type->MetadataToken.index - 1];
    System_Reflection_Assembly assembly = type->Assembly;

    // entry guard
    if (type->IsSetup) {
        goto cleanup;
    }
    type->IsSetup = true;

    // get the base type
    System_Type base_type;
    CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, type_def->extends, type->GenericArguments, NULL, &base_type));
    GC_UPDATE(type, BaseType, base_type);

    // setup the field
    for (int fi = 0; fi < type->Fields->Length; fi++) {
        int index = type_def->field_list.index + fi - 1;
        metadata_field_t* field = metadata_get_field(metadata, index);
        System_Reflection_FieldInfo fieldInfo = type->Fields->Data[fi];
        CHECK_AND_RETHROW(parse_field_sig(field->signature, fieldInfo, file, metadata));
    }

    // setup the methods
    for (int mi = 0; mi < type->Methods->Length; mi++) {
        int index = type_def->method_list.index + mi - 1;
        metadata_method_def_t* method_def = metadata_get_method_def(metadata, index);
        System_Reflection_MethodInfo methodInfo = type->Methods->Data[mi];

        if (method_def->rva != 0) {
            // get the rva
            pe_directory_t directory = {
                .rva = method_def->rva,
            };
            const void* rva_base = pe_get_rva_ptr(file, &directory);
            CHECK(rva_base != NULL);

            // parse the method info
            CHECK_AND_RETHROW(parse_method_cil(methodInfo, (blob_entry_t){
                .size = directory.size,
                .data= rva_base
            }, file, metadata));
        }

        CHECK_AND_RETHROW(parse_method_def_sig(method_def->signature, methodInfo, file, metadata));
    }

    // now we finished the type setup
    type->IsSetupFinished = true;

    // expand all the type instances that
    // were already created from this type
    for (System_Type instance = type->NextGenericInstance; instance != NULL; instance = instance->NextGenericInstance) {
        CHECK_AND_RETHROW(type_expand_generic(instance));
    }

cleanup:
    return err;
}

static err_t setup_type_info(pe_file_t* file, metadata_t* metadata, System_Reflection_Assembly assembly) {
    err_t err = NO_ERROR;

    // TODO: these are not going to be tracked by the GC properly, for now disable preemption
    //       but we will need to find something better so it won't DOS the system lol
    scheduler_preempt_disable();

    struct {
        System_Type key;
        System_Type* value;
    }* interfaces = NULL;

    struct {
        System_Type key;
        TinyDotNet_Reflection_MethodImpl* value;
    }* method_impls_table = NULL;

    struct {
        void* key;
        System_Type* value;
    }* owner_generic_params = NULL;

    //------------------------------------------------------------------------------------------------------------------
    // Do the base type init
    //------------------------------------------------------------------------------------------------------------------

    int types_count = metadata->tables[METADATA_TYPE_DEF].rows;
    metadata_type_def_t* type_defs = metadata->tables[METADATA_TYPE_DEF].table;

    int fields_count = metadata->tables[METADATA_FIELD].rows;
    int methods_count = metadata->tables[METADATA_METHOD_DEF].rows;

    for (int i = 0; i < types_count; i++) {
        metadata_type_def_t* type_def = &type_defs[i];
        System_Type type = assembly->DefinedTypes->Data[i];
        type->MetadataToken = (token_t){ .table = METADATA_TYPE_DEF, .index = i + 1 };

        // set the owners and flags
        GC_UPDATE(type, Assembly, assembly);
        GC_UPDATE(type, Module, assembly->Module);
        type->Attributes = type_def->flags;
        type->GenericParameterPosition = -1;

        CHECK(
            type_layout(type) == TYPE_SEQUENTIAL_LAYOUT ||
            type_layout(type) == TYPE_EXPLICIT_LAYOUT ||
            type_layout(type) == TYPE_AUTO_LAYOUT
        );

        // setup the name and base types
        GC_UPDATE(type, Name, new_string_from_utf8(type_def->type_name, strlen(type_def->type_name)));
        GC_UPDATE(type, Namespace, new_string_from_utf8(type_def->type_namespace, strlen(type_def->type_namespace)));
        CHECK(type->Name->Length > 0);
    }

    //
    // Setup all the basic type information, this includes
    // anything that is not relying on another type in the
    // list so we can do this in a linear fashion
    //

    for (int i = 0; i < types_count; i++) {
        metadata_type_def_t* type_def = &type_defs[i];
        System_Type type = assembly->DefinedTypes->Data[i];

        // init methods partially since these are needed for generic methods
        // setup methods
        int last_idx = (i + 1 == types_count) ?
                       methods_count :
                       type_def[1].method_list.index - 1;
        CHECK(last_idx <= methods_count);

        type->Methods = GC_NEW_ARRAY(tSystem_Reflection_MethodInfo, last_idx - type_def->method_list.index + 1);
        for (int mi = 0; mi < type->Methods->Length; mi++) {
            int index = type_def->method_list.index + mi - 1;
            metadata_method_def_t* method_def = metadata_get_method_def(metadata, index);
            System_Reflection_MethodInfo methodInfo = GC_NEW(tSystem_Reflection_MethodInfo);
            methodInfo->MethodIndex = mi;
            GC_UPDATE_ARRAY(type->Methods, mi, methodInfo);
            GC_UPDATE_ARRAY(assembly->DefinedMethods, index, methodInfo);

            GC_UPDATE(methodInfo, DeclaringType, type);
            GC_UPDATE(methodInfo, Module, type->Module);
            GC_UPDATE(methodInfo, Name, new_string_from_utf8(method_def->name, strlen(method_def->name)));
            methodInfo->Attributes = method_def->flags;
            methodInfo->ImplAttributes = method_def->impl_flags;
        }

        // setup fields
        last_idx = (i + 1 == types_count) ?
                       fields_count :
                       type_def[1].field_list.index - 1;
        CHECK(last_idx <= fields_count);

        type->Fields = GC_NEW_ARRAY(tSystem_Reflection_FieldInfo, last_idx - type_def->field_list.index + 1);
        for (int fi = 0; fi < type->Fields->Length; fi++) {
            size_t index = type_def->field_list.index + fi - 1;
            metadata_field_t* field = metadata_get_field(metadata, index);
            System_Reflection_FieldInfo fieldInfo = UNSAFE_GC_NEW(tSystem_Reflection_FieldInfo);
            GC_UPDATE_ARRAY(type->Fields, fi, fieldInfo);
            GC_UPDATE_ARRAY(assembly->DefinedFields, index, fieldInfo);

            GC_UPDATE(fieldInfo, DeclaringType, type);
            GC_UPDATE(fieldInfo, Module, type->Module);
            GC_UPDATE(fieldInfo, Name, new_string_from_utf8(field->name, strlen(field->name)));
            fieldInfo->Attributes = field->flags;
        }

    }

    //
    // load all the external type references
    //

    CHECK_AND_RETHROW(loader_load_type_refs(assembly, metadata));

    //------------------------------------------------------------------------------------------------------------------
    // Anything that we can't handle now just save it for later on
    //------------------------------------------------------------------------------------------------------------------

    //
    // Save aside everything from the type specs
    //

    int type_specs_count = metadata->tables[METADATA_TYPE_SPEC].rows;
    metadata_type_spec_t* type_specs = metadata->tables[METADATA_TYPE_SPEC].table;
    GC_UPDATE(assembly, DefinedTypeSpecs, GC_NEW_ARRAY(get_array_type(tSystem_Byte), type_specs_count));
    for (int i = 0; i < type_specs_count; i++) {
        metadata_type_spec_t* spec = &type_specs[i];
        System_Byte_Array blob = GC_NEW_ARRAY(tSystem_Byte, spec->signature.size);
        memcpy(blob->Data, spec->signature.data, spec->signature.size);
        GC_UPDATE_ARRAY(assembly->DefinedTypeSpecs, i, blob);
    }

    //
    // Save aside everything from the member refs
    //

    int member_refs_count = metadata->tables[METADATA_MEMBER_REF].rows;
    metadata_member_ref_t* member_refs = metadata->tables[METADATA_MEMBER_REF].table;
    GC_UPDATE(assembly, DefinedMemberRefs, GC_NEW_ARRAY(get_array_type(tSystem_Byte), member_refs_count));
    for (int i = 0; i < member_refs_count; i++) {
        metadata_member_ref_t* ref = &member_refs[i];
        TinyDotNet_Reflection_MemberReference member = UNSAFE_GC_NEW(tTinyDotNet_Reflection_MemberReference);
        GC_UPDATE(member, Name, new_string_from_utf8(ref->name, strlen(ref->name)));
        System_Byte_Array blob = GC_NEW_ARRAY(tSystem_Byte, ref->signature.size);
        memcpy(blob->Data, ref->signature.data, ref->signature.size);
        GC_UPDATE(member, Signature, blob);
        member->Class = ref->class;
        GC_UPDATE_ARRAY(assembly->DefinedMemberRefs, i, member);
    }

    //------------------------------------------------------------------------------------------------------------------
    // Setup generic type/method arguments
    //------------------------------------------------------------------------------------------------------------------

    int generic_params_count = metadata->tables[METADATA_GENERIC_PARAM].rows;
    metadata_generic_param_t* generic_params = metadata->tables[METADATA_GENERIC_PARAM].table;

    //
    // Start with accumulating the parameters
    //

    for (int i = 0; i < generic_params_count; i++) {
        metadata_generic_param_t* generic_param = &generic_params[i];

        void* owner = NULL;
        if (generic_param->owner.table == METADATA_TYPE_DEF) {
            CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, generic_param->owner, NULL, NULL, (void*)&owner));
        } else {
            CHECK_AND_RETHROW(assembly_get_method_by_token(assembly, generic_param->owner, NULL, NULL, (void*)&owner));
        }

        System_Type typeParam = UNSAFE_GC_NEW(tSystem_Type);
        typeParam->MetadataToken = (token_t) { .table = METADATA_GENERIC_PARAM, .index = i + 1 };
        GC_UPDATE(typeParam, Name, new_string_from_utf8(generic_param->name, strlen(generic_param->name)));
        typeParam->GenericParameterPosition = generic_param->number;
        typeParam->GenericTypeAttributes = generic_param->flags;

        int parami = hmgeti(owner_generic_params, owner);
        if (parami == -1) {
            CHECK(generic_param->number == 0);
            System_Type* arr = NULL;
                    arrpush(arr, typeParam);
            hmput(owner_generic_params, owner, arr);
        } else {
            CHECK(generic_param->number == arrlen(owner_generic_params[parami].value));
                    arrpush(owner_generic_params[parami].value, typeParam);
        }
    }

    //
    // And now that we accounted for all of them we can actually put them in the correct place
    //

    for (int i = 0; i < types_count; i++) {
        System_Type type = assembly->DefinedTypes->Data[i];

        // set the generics of the type
        int gi = hmgeti(owner_generic_params, type);
        if (gi != -1) {
            GC_UPDATE(type, GenericArguments, GC_NEW_ARRAY(tSystem_Type, arrlen(owner_generic_params[gi].value)));
            for (int j = 0; j < type->GenericArguments->Length; j++) {
                System_Type gtype = owner_generic_params[gi].value[j];
                CHECK(type->GenericArguments->Data[gtype->GenericParameterPosition] == NULL);
                type->GenericArguments->Data[gtype->GenericParameterPosition] = gtype;
            }
        }

        // set the generics for the methods
        for (int mi = 0; mi < type->Methods->Length; mi++) {
            System_Reflection_MethodInfo methodInfo = type->Methods->Data[mi];

            // now fill the method generic params
            int mgi = hmgeti(owner_generic_params, methodInfo);
            if (mgi != -1) {
                GC_UPDATE(methodInfo, GenericArguments, GC_NEW_ARRAY(tSystem_Type, arrlen(owner_generic_params[mgi].value)));
                for (int j = 0; j < methodInfo->GenericArguments->Length; j++) {
                    System_Type gtype = owner_generic_params[mgi].value[j];
                    CHECK(methodInfo->GenericArguments->Data[gtype->GenericParameterPosition] == NULL);
                    methodInfo->GenericArguments->Data[gtype->GenericParameterPosition] = gtype;
                }
            }
        }
    }

    //------------------------------------------------------------------------------------------------------------------
    // We are done with all the basics, now for the real fun which is parsing all the fields and methods, this
    // requires us to work in a recursive manner sadly, because there is no order made by the compiler
    //------------------------------------------------------------------------------------------------------------------

    for (int i = 0; i < types_count; i++) {
        System_Type type = assembly->DefinedTypes->Data[i];
        CHECK_AND_RETHROW(loader_setup_type(file, metadata, type));
    }

    //------------------------------------------------------------------------------------------------------------------
    // Now continue with anything that requires assembly_get_type_by_token, as we have all
    // the basic definitions (including generics)
    //------------------------------------------------------------------------------------------------------------------

    //
    // Save aside everything from the method spec
    //

    int method_spec_count = metadata->tables[METADATA_METHOD_SPEC].rows;
    metadata_method_spec_t* method_spec = metadata->tables[METADATA_METHOD_SPEC].table;
    GC_UPDATE(assembly, DefinedMethodSpecs, GC_NEW_ARRAY(get_array_type(tTinyDotNet_Reflection_MethodSpec), method_spec_count));
    for (int i = 0; i < method_spec_count; i++) {
        System_Reflection_MethodInfo method = NULL;
        metadata_method_spec_t* spec = &method_spec[i];
        TinyDotNet_Reflection_MethodSpec methodSpec = UNSAFE_GC_NEW(tTinyDotNet_Reflection_MethodSpec);
        CHECK_AND_RETHROW(assembly_get_method_by_token(assembly, spec->method, NULL, NULL, &method));
        System_Byte_Array blob = GC_NEW_ARRAY(tSystem_Byte, spec->instantiation.size);
        memcpy(blob->Data, spec->instantiation.data, spec->instantiation.size);
        GC_UPDATE(methodSpec, Instantiation, blob);
        GC_UPDATE(methodSpec, Method, method);
        GC_UPDATE_ARRAY(assembly->DefinedMethodSpecs, i, methodSpec);
    }

    //------------------------------------------------------------------------------------------------------------------
    // Take care of all the interface implementations
    //------------------------------------------------------------------------------------------------------------------

    metadata_interface_impl_t* interface_impls = metadata->tables[METADATA_INTERFACE_IMPL].table;
    int interface_impls_count = metadata->tables[METADATA_INTERFACE_IMPL].rows;

    // count interfaces for each type, use stack size as a temp while we do the counting
    for (int ii = 0; ii < interface_impls_count; ii++) {
        metadata_interface_impl_t* interface_impl = &interface_impls[ii];
        System_Type class;
        System_Type interface;
        CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, interface_impl->class, NULL, NULL, &class));
        CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, interface_impl->interface, class->GenericArguments, NULL, &interface));

        int i = hmgeti(interfaces, class);
        if (i == -1) {
            System_Type* arr = NULL;
            arrpush(arr, interface);
            hmput(interfaces, class, arr);
        } else {
            arrpush(interfaces[i].value, interface);
        }
    }

    // Allocate all the arrays nicely
    for (int i = 0; i < hmlen(interfaces); i++) {
        System_Type type = interfaces[i].key;
        GC_UPDATE(type, InterfaceImpls, GC_NEW_ARRAY(tTinyDotNet_Reflection_InterfaceImpl, arrlen(interfaces[i].value)));
        for (int j = 0; j < arrlen(interfaces[i].value); j++) {
            TinyDotNet_Reflection_InterfaceImpl interfaceImpl = UNSAFE_GC_NEW(tTinyDotNet_Reflection_InterfaceImpl);
            GC_UPDATE(interfaceImpl, InterfaceType, interfaces[i].value[j]);
            GC_UPDATE_ARRAY(type->InterfaceImpls, j, interfaceImpl);
        }

        // Update the created interfaces of the interface impls
        System_Type genericChild = type->NextGenericInstance;
        while (genericChild != NULL) {
            type_expand_interface_impls(genericChild, type->InterfaceImpls);
            genericChild = genericChild->NextGenericInstance;
        }
    }

    //------------------------------------------------------------------------------------------------------------------
    // Take care of all the method impls
    //------------------------------------------------------------------------------------------------------------------

    metadata_method_impl_t* method_impls = metadata->tables[METADATA_METHOD_IMPL].table;
    int method_impls_count = metadata->tables[METADATA_METHOD_IMPL].rows;

    for (int i = 0; i < method_impls_count; i++) {
        metadata_method_impl_t* method_impl = &method_impls[i];
        System_Type class;
        System_Reflection_MethodInfo body;
        System_Reflection_MethodInfo declaration;
        CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, method_impl->class, NULL, NULL, &class));
        CHECK_AND_RETHROW(assembly_get_method_by_token(assembly, method_impl->method_body, class->GenericArguments, NULL, &body));
        CHECK_AND_RETHROW(assembly_get_method_by_token(assembly, method_impl->method_declaration, class->GenericArguments, NULL, &declaration));

        // validate the declaration
        CHECK(body->DeclaringType == class);
        CHECK(method_is_virtual(body));
        CHECK(body->MethodBody != NULL);

        // validate the body
//        bool found = false;
//        System_Type base = class;
//        while (base != NULL) {
//            if (base == declaration->DeclaringType) {
//                found = true;
//                break;
//            }
//            base = base->BaseType;
//        }
//        CHECK(found);
        CHECK(!method_is_final(declaration));

        TinyDotNet_Reflection_MethodImpl methodImpl = UNSAFE_GC_NEW(tTinyDotNet_Reflection_MethodImpl);
        GC_UPDATE(methodImpl, Body, body);
        GC_UPDATE(methodImpl, Declaration, declaration);

        int j = hmgeti(method_impls_table, class);
        if (j == -1) {
            TinyDotNet_Reflection_MethodImpl* arr = NULL;
            arrpush(arr, methodImpl);
            hmput(method_impls_table, class, arr);
        } else {
            arrpush(method_impls_table[j].value, methodImpl);
        }
    }

    // Allocate all the arrays nicely
    for (int i = 0; i < hmlen(method_impls_table); i++) {
        System_Type type = method_impls_table[i].key;
        GC_UPDATE(type, MethodImpls, GC_NEW_ARRAY(tTinyDotNet_Reflection_MethodImpl, arrlen(method_impls_table[i].value)));
        for (int j = 0; j < arrlen(method_impls_table[i].value); j++) {
            GC_UPDATE_ARRAY(type->MethodImpls, j, method_impls_table[i].value[j]);
        }

        // Update the created interfaces of the interface impls
        System_Type genericChild = type->NextGenericInstance;
        while (genericChild != NULL) {
            type_expand_method_impls(genericChild, type->MethodImpls);
            genericChild = genericChild->NextGenericInstance;
        }
    }

    // init class layout
    CHECK_AND_RETHROW(set_class_layout(assembly, metadata));

cleanup:
    for (int i = 0; i < hmlen(interfaces); i++) {
        arrfree(interfaces[i].value);
    }
    hmfree(interfaces);

    for (int i = 0; i < hmlen(owner_generic_params); i++) {
        arrfree(owner_generic_params[i].value);
    }
    hmfree(owner_generic_params);

    for (int i = 0; i < hmlen(method_impls_table); i++) {
        arrfree(method_impls_table[i].value);
    }
    hmfree(method_impls_table);

    scheduler_preempt_enable();

    return err;
}

err_t loader_fill_method(System_Type type, System_Reflection_MethodInfo method) {
    err_t err = NO_ERROR;

    // don't initialize twice
    if (method->IsFilled) {
        goto cleanup;
    }
    method->IsFilled = true;

    // init return type
    if (method->ReturnType != NULL) {
        CHECK_AND_RETHROW(loader_fill_type(method->ReturnType));
    }

    // init all the other parameters
    for (int i = 1; i < method->Parameters->Length; i++) {
        System_Reflection_ParameterInfo parameterInfo = method->Parameters->Data[i];
        CHECK_AND_RETHROW(loader_fill_type(parameterInfo->ParameterType));
    }

cleanup:
    return err;
}

#if 0
    #define TRACE_FILL_TYPE(...) TRACE(__VA_ARGS__)
#else
    #define TRACE_FILL_TYPE(...)
#endif

static System_Reflection_MethodInfo find_virtually_overridden_method(System_Type type, System_Reflection_MethodInfo method) {
    while (type != NULL) {
        // search the method impl table
        if (type->MethodImpls != NULL) {
            for (int i = 0; i < type->MethodImpls->Length; i++) {
                if (type->MethodImpls->Data[i]->Declaration == method) {
                    return type->MethodImpls->Data[i]->Body;
                }
            }
        }

        // Use normal inheritance (I.8.10.4)
        for (int i = 0; i < type->Methods->Length; i++) {
            System_Reflection_MethodInfo info = type->Methods->Data[i];

            // not virtual, continue
            if (!method_is_virtual(info)) continue;

            // match the name
            if (!string_equals(info->Name, method->Name)) continue;

            // check the return type
            if (info->ReturnType != method->ReturnType) continue;

            // Check parameter count matches
            if (info->Parameters->Length != method->Parameters->Length) continue;

            // check the parameters
            bool signatureMatch = true;
            for (int j = 0; j < info->Parameters->Length; j++) {
                System_Reflection_ParameterInfo paramA = info->Parameters->Data[j];
                System_Reflection_ParameterInfo paramB = method->Parameters->Data[j];
                if (paramA->ParameterType != paramB->ParameterType) {
                    signatureMatch = false;
                    break;
                }
            }
            if (!signatureMatch) continue;

            // set the offset
            return info;
        }

        // get the parent for next iteration
        type = type->BaseType;
    }

    // not found
    return NULL;
}

/**
 * Checks if a value type without needing to initialize the full type
 */
static bool unprimed_is_value_type(System_Type type) {
    while (type != NULL) {
        if (type == tSystem_ValueType) return true;
        type = type->BaseType;
    }
    return false;
}

static err_t fill_vtable_for_interface(System_Type root, System_Type interface, int base_offset) {
    err_t err = NO_ERROR;

    CHECK(type_is_interface(interface));
    CHECK(base_offset + interface->Methods->Length <= root->VirtualMethods->Length);

    // now go over its interfaces
    int virtual_offset = 0;
    if (interface->InterfaceImpls != NULL) {
        for (int i = 0; i < interface->InterfaceImpls->Length; i++) {
            TinyDotNet_Reflection_InterfaceImpl impl = interface->InterfaceImpls->Data[i];

            // we are going relative to this vtable, so add our base to the offsets
            CHECK_AND_RETHROW(fill_vtable_for_interface(root, impl->InterfaceType, base_offset + impl->VTableOffset));

            // count how many interface methods we have to get to our actual offset
            virtual_offset += impl->InterfaceType->VirtualMethods->Length;
        }
    }

    // add the base offset to this, so we start from the base offset
    base_offset += virtual_offset;

    // fill the vtable for this impl
    for (int i = 0; i < interface->Methods->Length; i++) {
        System_Reflection_MethodInfo method = interface->Methods->Data[i];
        if (!type_is_interface(root)) {
            method = find_virtually_overridden_method(root, method);
            CHECK(method != NULL);
        }
        GC_UPDATE_ARRAY(root->VirtualMethods, base_offset + i, method);
    }

cleanup:
    return err;
}

err_t loader_fill_type(System_Type type) {
    err_t err = NO_ERROR;
    static int depth = 0;
    TRACE_FILL_TYPE("%*s%U.%U", depth * 4, "", type->Namespace, type->Name);
    depth++;

    bool locked = 0;

    // the type is already filled, ignore it (fast path)
    if (type->IsFilled) {
        goto cleanup;
    }

    // slow path, fill it
    CHECK_AND_RETHROW(monitor_enter(type));
    locked = true;

    // check again just in case
    if (type->IsFilled) {
        goto cleanup;
    }

    // we are going to fill the type now
    type->IsFilled = true;

    // initialize to an invalid value just in case
    type->VTableSize = -1;

    // special case, we should not have anything else in here that is
    // important specifically for ValueType class
    if (type == tSystem_ValueType) {
        type->IsValueType = true;
        type->StackType = STACK_TYPE_VALUE_TYPE;
    }

    int virtual_count = 0;

    int managedSize = 0;
    int managedSizePrev = 0;
    int managedAlignment = 1;

    // first check the parent
    if (type->BaseType != NULL) {
        if (type_is_interface(type)) {
            // interfaces must inherit from System.Object
            // TODO: is that correct?
            CHECK(type->BaseType == tSystem_Object);
        }

        // fill the type information of the parent
        CHECK_AND_RETHROW(loader_fill_type(type->BaseType));

        // validate that we don't inherit from a sealed type
        CHECK(!type_is_sealed(type->BaseType));

        // check we have a size
        if (type->BaseType->IsValueType) {
            // Can not inherit from value types, except for enum which is allowed
            CHECK(type->BaseType == tSystem_ValueType || type->BaseType == tSystem_Enum);

            // we are a value type too
            type->IsValueType = true;
            if (type->StackType == STACK_TYPE_O) {
                type->StackType = STACK_TYPE_VALUE_TYPE;
            }
        }

        // now check if it has virtual methods
        if (type->BaseType->VirtualMethods != NULL) {
            virtual_count = type->BaseType->VirtualMethods->Length;
        }

        // get the managed size, only needed for any type which is not the
        // value type which starts without the actual size of the object class
        if (type != tSystem_ValueType) {
            managedSize = type->BaseType->ManagedSize;
            managedSizePrev = managedSize;
            managedAlignment = type->BaseType->ManagedAlignment;
        }

        // copy the managed pointers offsets
        for (int i = 0; i < arrlen(type->BaseType->ManagedPointersOffsets); i++) {
            arrpush(type->ManagedPointersOffsets, type->BaseType->ManagedPointersOffsets[i]);
        }
    }

    // this is only needed for non-generic types
    if (!type_is_generic_definition(type) && !type_is_generic_parameter(type)) {
        // make sure this was primed already
        CHECK(type->Methods != NULL);
        CHECK(type->Fields != NULL);

        // first we need to take care of the virtual method table
        for (int i = 0; i < type->Methods->Length; i++) {
            System_Reflection_MethodInfo methodInfo = type->Methods->Data[i];

            if (method_is_virtual(methodInfo)) {
                if (method_is_new_slot(methodInfo)) {
                    // this is a newslot, always allocate a new slot
                    methodInfo->VTableOffset = -1;
                } else {
                    System_Reflection_MethodInfo overridden = find_virtually_overridden_method(type->BaseType, methodInfo);
                    if (overridden == NULL) {
                        // not a newslot but we did not find the implementation,
                        // just allocate a new slot for it instead
                        methodInfo->VTableOffset = -1;
                    } else {
                        // if strict do an accessibility check
                        if (method_is_strict(overridden)) {
                            CHECK(check_method_accessibility(methodInfo, overridden));
                        }

                        // should be a virtual method which can be overridden
                        CHECK(method_is_virtual(overridden));
                        CHECK(!method_is_final(overridden));
                        CHECK(overridden->VTableOffset >= 0);
                        methodInfo->VTableOffset = overridden->VTableOffset;
                    }
                }
            } else {
                int index = 0;
                System_Reflection_MethodInfo otherMethod = NULL;
                while ((otherMethod = type_iterate_methods(type, methodInfo->Name, &index)) != NULL) {
                    if (otherMethod == methodInfo) continue;
                    if (otherMethod->ReturnType != methodInfo->ReturnType) continue;
                    if (otherMethod->Parameters->Length != methodInfo->Parameters->Length) continue;
                    bool all_matching = true;
                    for (int j = 0; j < otherMethod->Parameters->Length; j++) {
                        if (otherMethod->Parameters->Data[j] != methodInfo->Parameters->Data[j]) {
                            all_matching = false;
                            break;
                        }
                    }
                    CHECK(!all_matching);
                }
            }

            // for interfaces all methods need to be abstract
            if (type_is_interface(type)) {
                CHECK(method_is_abstract(methodInfo));
            }
        }

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Virtual Method Table initial creation
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // first figure out everything to do with interface implementations
        if (type->InterfaceImpls != NULL) {
            // TODO: validate that we have all the interfaces the parent has

            // go over and expand all the interfaces needed
            for (int i = 0; i < type->InterfaceImpls->Length; i++) {
                TinyDotNet_Reflection_InterfaceImpl impl = type->InterfaceImpls->Data[i];
                System_Type interface = impl->InterfaceType;

                CHECK_AND_RETHROW(loader_fill_type(interface));
                CHECK(interface->VirtualMethods != NULL);

                // set the offset of this impl and increment our vtable count
                impl->VTableOffset = virtual_count;
                virtual_count += interface->VirtualMethods->Length;

                // not interface, setup the offsets of all the functions that
                // implement this interface
                if (!type_is_interface(type)) {
                    for (int mi = 0; mi < interface->VirtualMethods->Length; mi++) {
                        System_Reflection_MethodInfo method = interface->VirtualMethods->Data[mi];
                        System_Reflection_MethodInfo implementation = find_virtually_overridden_method(type, method);
                        CHECK(implementation != NULL);

                        if (implementation->VTableOffset == -1) {
                            // this is a newslot method, set its offset, its offset is the base of this
                            // interface + the offset in the virtual methods table of the offset
                            implementation->VTableOffset = impl->VTableOffset + mi;
                        }
                    }
                }
            }
        }

        // now that we have set all the methods that are part of interfaces, check how many
        // of these methods are not in interfaces, and set them up to be after all of the interfaces
        for (int i = 0; i < type->Methods->Length; i++) {
            System_Reflection_MethodInfo method = type->Methods->Data[i];
            if (!method_is_virtual(method)) continue;
            if (method->VTableOffset != -1) continue;

            // set its offset
            method->VTableOffset = virtual_count++;
        }

        // now we know the exact amount of virtual methods that we have for this type
        GC_UPDATE(type, VirtualMethods, GC_NEW_ARRAY(tSystem_Reflection_MethodInfo, virtual_count));
        if (virtual_count != 0) {
            type->VTable = malloc(sizeof(void*) * virtual_count);
            CHECK_ERROR(type->VTable != NULL, ERROR_OUT_OF_MEMORY);
        } else {
            type->VTable = NULL;
        }

        // copy the parent's vtable
        if (type->BaseType != NULL) {
            for (int i = 0; i < type->BaseType->VirtualMethods->Length; i++) {
                GC_UPDATE_ARRAY(type->VirtualMethods, i, type->BaseType->VirtualMethods->Data[i]);
            }
        }

        // now place all of our methods in the vtable
        for (int i = 0; i < type->Methods->Length; i++) {
            System_Reflection_MethodInfo method = type->Methods->Data[i];
            if (!method_is_virtual(method)) continue;
            GC_UPDATE_ARRAY(type->VirtualMethods, method->VTableOffset, method);
        }

        if (type->InterfaceImpls != NULL) {
            for (int i = 0; i < type->InterfaceImpls->Length; i++) {
                TinyDotNet_Reflection_InterfaceImpl impl = type->InterfaceImpls->Data[i];
                CHECK_AND_RETHROW(fill_vtable_for_interface(type, impl->InterfaceType, impl->VTableOffset));
            }
        }

        // just in case, make sure all the methods are set properly
        for (int i = 0; i < type->VirtualMethods->Length; i++) {
            CHECK(type->VirtualMethods->Data[i] != NULL);
        }

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // process all the non-static fields at this moment, we are going to calculate the size the
        // same way SysV does it
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // If its not a value-type and the stack-size is not present, then set it up now.
        // It needs to be done here as non-static fields in non-value types can point to
        // the containing type.
        if (type->StackSize == 0 && !type->IsValueType) {
            if (type_is_interface(type)) {
                type->StackSize = sizeof(void*) * 2;
                type->StackAlignment = alignof(void*);
            } else {
                type->StackSize = sizeof(void*);
                type->StackAlignment = alignof(void*);
            }
        }

        // TODO: for auto-layout we can optimize the layout
        //       to have the best packing possible

        // TODO: handle explicit layout, but check for overlapping
        //       so no reference type will have overlapping with
        //       another type
        CHECK(type_layout(type) != TYPE_EXPLICIT_LAYOUT);

        // the default max alignment is 16
        uint8_t max_alignment = type->PackingSize ?: 16;

        // for non-static we have two steps, first resolve all the stack sizes, for ref types
        // we are not going to init ourselves yet
        for (int i = 0; i < type->Fields->Length; i++) {
            System_Reflection_FieldInfo fieldInfo = type->Fields->Data[i];
            if (field_is_static(fieldInfo)) continue;

            // Fill it
            if (unprimed_is_value_type(fieldInfo->FieldType)) {
                CHECK_AND_RETHROW(loader_fill_type(fieldInfo->FieldType));
                CHECK(fieldInfo->FieldType->StackSize != 0);
            } else {
                if (type_is_interface(fieldInfo->FieldType)) {
                    fieldInfo->FieldType->StackSize = sizeof(void*) * 2;
                    fieldInfo->FieldType->StackAlignment = alignof(void*);
                } else {
                    fieldInfo->FieldType->StackSize = sizeof(void*);
                    fieldInfo->FieldType->StackAlignment = alignof(void*);
                }
            }

            // only static fields can be literal
            CHECK(!field_is_literal(fieldInfo));

            // align the size
            int16_t alignment = MIN(max_alignment, fieldInfo->FieldType->StackAlignment);
            managedSize = ALIGN_UP(managedSize, alignment);
            fieldInfo->MemoryOffset = managedSize;

            // now add the size
            managedSize += fieldInfo->FieldType->StackSize;
            CHECK(managedSize > managedSizePrev, "Type size overflow! %d -> %d", managedSizePrev, managedSize);
            managedSizePrev = managedSize;

            // pointer offsets for gc
            if (!fieldInfo->FieldType->IsValueType) {
                // this is a normal reference type, just add the offset to us
                arrpush(type->ManagedPointersOffsets, fieldInfo->MemoryOffset);
            } else {
                // for value types we are essentially embedding them in us, so we are
                // going to just copy all the offsets from them and add their base to
                // our offsets
                int* offsets = arraddnptr(type->ManagedPointersOffsets, arrlen(fieldInfo->FieldType->ManagedPointersOffsets));
                for (int j = 0; j < arrlen(fieldInfo->FieldType->ManagedPointersOffsets); j++, offsets++) {
                    int offset = fieldInfo->FieldType->ManagedPointersOffsets[j];
                    *offsets = (int)fieldInfo->MemoryOffset + offset;
                }
            }

            // set new type alignment
            managedAlignment = MAX(managedAlignment, fieldInfo->FieldType->StackAlignment);
            managedAlignment = MIN(max_alignment, managedAlignment);
        }

        // lastly align the whole size to the struct alignment
        managedSize = ALIGN_UP(managedSize, managedAlignment);
        CHECK(managedSize >= managedSizePrev, "Type size overflow! %d >= %d", managedSize, managedSizePrev);

        if (type->ManagedSize != 0) {
            // only builtin-types should have this, and it means
            // they should be sequential layout
            CHECK(type_layout(type) == TYPE_SEQUENTIAL_LAYOUT);

            // This has a C type equivalent, verify the sizes match
            CHECK(type->ManagedSize == managedSize && type->ManagedAlignment == managedAlignment,
                  "Size mismatch for type %U.%U (native: %d bytes (%d), dotnet: %d bytes (%d))",
                  type->Namespace, type->Name,
                  type->ManagedSize, type->ManagedAlignment,
                  managedSize, managedAlignment);
        }

        // set the correct class size, must not be smaller than
        // the size we got
        if (type->ClassSize != 0) {
            CHECK(type->ClassSize >= managedSize);
            managedSize = type->ClassSize;
        }

        type->ManagedSize = managedSize;
        type->ManagedAlignment = managedAlignment;

        // Sort the stack size, if it was a reference type we already set it, otherwise it
        // is a struct type
        if (type->StackSize == 0) {
            CHECK(type->IsValueType);
            type->StackSize = type->ManagedSize;
            type->StackAlignment = type->ManagedAlignment;

            // max size for stack type is 1MB
            CHECK(type->StackSize <= SIZE_1MB);
        }

        // now that we initialized the instance size of this, we can go over and initialize
        // all the fields, both static and non-static
        for (int i = 0; i < type->Fields->Length; i++) {
            System_Reflection_FieldInfo fieldInfo = type->Fields->Data[i];
            if (field_is_static(fieldInfo)) continue;

            // Fill it
            CHECK_AND_RETHROW(loader_fill_type(fieldInfo->FieldType));
        }

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Handle static fields
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // now that all the non-static fields are initialized we can have all our globals initialized properly
        // there is nothing much to do since most of the work is in the jit
        for (int i = 0; i < type->Fields->Length; i++) {
            System_Reflection_FieldInfo fieldInfo = type->Fields->Data[i];
            if (!field_is_static(fieldInfo)) continue;

            // Fill it
            CHECK_AND_RETHROW(loader_fill_type(fieldInfo->FieldType));
        }

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Now handle all the methods
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        for (int i = 0; i < type->Methods->Length; i++) {
            System_Reflection_MethodInfo methodInfo = type->Methods->Data[i];


            if (method_is_rt_special_name(methodInfo)) {
                CHECK(method_is_special_name(methodInfo));

                if (string_equals_cstr(methodInfo->Name, ".cctor")) {
                    CHECK(method_is_static(methodInfo));
                    CHECK(methodInfo->Parameters->Length == 0);
                    CHECK(methodInfo->ReturnType == NULL);
                    CHECK(type->StaticCtor == NULL);
                    type->StaticCtor = methodInfo;
                } else if (string_equals_cstr(methodInfo->Name, ".ctor")) {
                    CHECK(!method_is_static(methodInfo));
                    CHECK(methodInfo->ReturnType == NULL);
                }
            } else {
                // finalize is extra special
                if (string_equals_cstr(methodInfo->Name, "Finalize")) {
                    // for performance reason we are not going to have every method have a finalizer
                    // but instead we are going to have it virtually virtual
                    if (methodInfo->ReturnType == NULL && methodInfo->Parameters->Length == 0) {
                        CHECK(type->Finalize == NULL);
                        GC_UPDATE(type, Finalize, methodInfo);
                    }
                }

                // the rest should not be marked rtspecialname (we can have specialname
                // since Properties use it for example)
                CHECK(!method_is_rt_special_name(methodInfo));
            }
        }

        // figure out if a subclass of us has a finalizer
        if (type->Finalize == NULL) {
            System_Type base = type->BaseType;
            while (base != NULL) {
                if (base->Finalize != NULL) {
                    GC_UPDATE(type, Finalize, base->Finalize);
                    break;
                }
                base = base->BaseType;
            }
        }

        // Now fill all the method defs
        for (int i = 0; i < type->Methods->Length; i++) {
            System_Reflection_MethodInfo methodInfo = type->Methods->Data[i];
            if (methodInfo->GenericArguments != NULL) continue; // no need to fill generic methods
            CHECK_AND_RETHROW(loader_fill_method(type, methodInfo));
        }

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Setup interface size and such
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        if (type_is_interface(type)) {
            // make sure we have no fields
            CHECK(type->Fields->Length == 0);

            // all interfaces have a single managed pointer which
            // is the instance field, no need to create this per
            // type so create this once
            static int* interface_pointers = NULL;
            if (interface_pointers) {
                arrsetlen(interface_pointers, 1);
                interface_pointers[0] = sizeof(void*);
            }

            // we are going to treat a raw interface type as a
            // value type that has two pointers in it, for simplicity
            type->ManagedSize = sizeof(void*);
            type->ManagedAlignment = alignof(void*);
            type->ManagedPointersOffsets = interface_pointers;
        }

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Finally check special runtime types
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        if (type_is_enum(type)) {
            // for enum types we need to get the underlying type
            for (int i = 0; i < type->Fields->Length; i++) {
                System_Reflection_FieldInfo field = type->Fields->Data[i];
                if (!field_is_static(field)) {
                    // TODO: check specialname/rtspecialname
                    CHECK(string_equals_cstr(field->Name, "value__"));
                    CHECK(type_is_integer(field->FieldType));
                    type->ElementType = field->FieldType;
                } else {
                    CHECK(field_is_literal(field));
                }
            }
        }
    } else {
        // fill all the instances instead
        System_Type instance = type->NextGenericInstance;
        while (instance != NULL) {
            CHECK_AND_RETHROW(loader_fill_type(instance));
            instance = instance->NextGenericInstance;
        }
    }

    // set the namespace if this is a nested type
    if (type->DeclaringType != NULL) {
        System_Type rootType = type->DeclaringType;
        while (rootType->DeclaringType != NULL) {
            rootType = rootType->DeclaringType;
        }
        GC_UPDATE(type, Namespace, rootType->Namespace);
    }

cleanup:

    if (locked) {
        PANIC_ON(monitor_exit(type));
    }

    depth--;
    TRACE_FILL_TYPE("%*s%U.%U - %d, %d", depth * 4, "", type->Namespace, type->Name, type->ManagedSize, type->StackSize);
    return err;
}

static err_t parse_user_strings(System_Reflection_Assembly assembly, pe_file_t* file) {
    err_t err = NO_ERROR;

    int string_count = 0;

    // count how many strings we have
    blob_entry_t us = {
        .data = file->us,
        .size = file->us_size
    };
    while (us.size != 0) {
        // get the size
        uint32_t string_size;
        CHECK_AND_RETHROW(parse_compressed_integer(&us, &string_size));

        // we got another string
        string_count++;

        // skip this string entry
        CHECK(string_size <= us.size);
        us.size -= string_size;
        us.data += string_size;
    }

    assembly->UserStrings = GC_NEW_ARRAY(tSystem_String, string_count);

    // now create all the strings
    string_count = 0;
    us.data = file->us;
    us.size = file->us_size;
    while (us.size != 0) {
        int offset = file->us_size - us.size;

        // get the size
        uint32_t string_size;
        CHECK_AND_RETHROW(parse_compressed_integer(&us, &string_size));
        CHECK(string_size <= us.size);

        // create the string and store it
        System_String string = GC_NEW_STRING(string_size / 2);
        memcpy(string->Chars, us.data, (string_size / 2) * 2);

        // set the entries in the table and array
        GC_UPDATE_ARRAY(assembly->UserStrings, string_count, string);
        hmput(assembly->UserStringsTable, offset, string);

        // we got another string
        string_count++;

        // skip this string entry
        us.size -= string_size;
        us.data += string_size;
    }

cleanup:
    return err;
}

static err_t connect_nested_types(System_Reflection_Assembly assembly, metadata_t* metadata) {
    err_t err = NO_ERROR;

    metadata_nested_class_t* nested_classes = metadata->tables[METADATA_NESTED_CLASS].table;
    for (int i = 0; i < metadata->tables[METADATA_NESTED_CLASS].rows; i++) {
        metadata_nested_class_t* nested_class = &nested_classes[i];
        System_Type enclosing;
        System_Type nested;
        CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, nested_class->enclosing_class, NULL, NULL, &enclosing));
        CHECK_AND_RETHROW(assembly_get_type_by_token(assembly, nested_class->nested_class, NULL, NULL, &nested));
        CHECK(enclosing != NULL && nested != NULL);
        GC_UPDATE(nested, DeclaringType, enclosing);

        GC_UPDATE(nested, NextNestedType, enclosing->NestedTypes);
        GC_UPDATE(enclosing, NestedTypes, nested);
    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Type init
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct type_init {
    const char* namespace;
    const char* name;
    System_Type* global;
    int stack_size;
    int stack_alignment;
    int managed_size;
    int managed_alignment;
    int vtable_size;
    stack_type_t stack_type;
} type_init_t;

#define TYPE_INIT(_namespace, _name, code)                      \
    {                                                           \
        .namespace = (_namespace),                              \
        .name = (_name),                                        \
        .global = &t##code,                                     \
        .stack_size = sizeof(code),                             \
        .stack_alignment = alignof(code),                       \
        .managed_size = sizeof(struct code),                    \
        .managed_alignment = alignof(struct code),              \
        .stack_type = STACK_TYPE_O,                             \
    }

#define EXCEPTION_INIT(_namespace, _name, code)                 \
    {                                                           \
        .namespace = (_namespace),                              \
        .name = (_name),                                        \
        .global = &t##code,                                     \
        .stack_size = sizeof(code),                             \
        .stack_alignment = alignof(code),                       \
        .managed_size = sizeof(struct System_Exception),        \
        .managed_alignment = alignof(struct System_Exception),  \
        .stack_type = STACK_TYPE_O,                             \
    }

#define VALUE_TYPE_INIT(_namespace, _name, code, stype)         \
    {                                                           \
        .namespace = (_namespace),                              \
        .name = (_name),                                        \
        .global = &t##code,                                     \
        .stack_size = sizeof(code),                             \
        .stack_alignment = alignof(code),                       \
        .managed_size = sizeof(code),                           \
        .managed_alignment = alignof(code),                     \
        .stack_type = (stype)                                   \
    }

#define TYPE_LOOKUP(_namespace, _name, code)                    \
    {                                                           \
        .namespace = (_namespace),                              \
        .name = (_name),                                        \
        .global = &code,                                        \
        .stack_size = -1,                                       \
    }

static type_init_t m_type_init[] = {
    TYPE_INIT("System", "Exception", System_Exception),
    VALUE_TYPE_INIT("System", "Enum", System_Enum, STACK_TYPE_VALUE_TYPE),
    VALUE_TYPE_INIT("System", "ValueType", System_ValueType, STACK_TYPE_VALUE_TYPE),
    TYPE_INIT("System", "Object", System_Object),
    TYPE_INIT("System", "Type", System_Type),
    TYPE_INIT("System", "Array", System_Array),
    TYPE_INIT("System", "String", System_String),
    VALUE_TYPE_INIT("System", "Boolean", System_Boolean, STACK_TYPE_INT32),
    VALUE_TYPE_INIT("System", "Char", System_Char, STACK_TYPE_INT32),
    VALUE_TYPE_INIT("System", "SByte", System_SByte, STACK_TYPE_INT32),
    VALUE_TYPE_INIT("System", "Byte", System_Byte, STACK_TYPE_INT32),
    VALUE_TYPE_INIT("System", "Int16", System_Int16, STACK_TYPE_INT32),
    VALUE_TYPE_INIT("System", "UInt16", System_UInt16, STACK_TYPE_INT32),
    VALUE_TYPE_INIT("System", "Int32", System_Int32, STACK_TYPE_INT32),
    VALUE_TYPE_INIT("System", "UInt32", System_UInt32, STACK_TYPE_INT32),
    VALUE_TYPE_INIT("System", "Int64", System_Int64, STACK_TYPE_INT64),
    VALUE_TYPE_INIT("System", "UInt64", System_UInt64, STACK_TYPE_INT64),
    VALUE_TYPE_INIT("System", "Single", System_Single, STACK_TYPE_FLOAT),
    VALUE_TYPE_INIT("System", "Double", System_Double, STACK_TYPE_FLOAT),
    VALUE_TYPE_INIT("System", "IntPtr", System_IntPtr, STACK_TYPE_INTPTR),
    VALUE_TYPE_INIT("System", "UIntPtr", System_UIntPtr, STACK_TYPE_INTPTR),
    TYPE_INIT("System", "Delegate", System_Delegate),
    TYPE_INIT("System", "MulticastDelegate", System_MulticastDelegate),
    TYPE_INIT("System.Reflection", "Module", System_Reflection_Module),
    TYPE_INIT("System.Reflection", "Assembly", System_Reflection_Assembly),
    TYPE_INIT("System.Reflection", "FieldInfo", System_Reflection_FieldInfo),
    TYPE_INIT("System.Reflection", "MemberInfo", System_Reflection_MemberInfo),
    TYPE_INIT("System.Reflection", "ParameterInfo", System_Reflection_ParameterInfo),
    TYPE_INIT("System.Reflection", "LocalVariableInfo", System_Reflection_LocalVariableInfo),
    TYPE_INIT("System.Reflection", "ExceptionHandlingClause", System_Reflection_ExceptionHandlingClause),
    TYPE_INIT("System.Reflection", "MethodBase", System_Reflection_MethodBase),
    TYPE_INIT("System.Reflection", "MethodBody", System_Reflection_MethodBody),
    TYPE_INIT("System.Reflection", "MethodInfo", System_Reflection_MethodInfo),
    EXCEPTION_INIT("System", "ArithmeticException", System_ArithmeticException),
    EXCEPTION_INIT("System", "DivideByZeroException", System_DivideByZeroException),
    EXCEPTION_INIT("System", "ExecutionEngineException", System_ExecutionEngineException),
    EXCEPTION_INIT("System", "IndexOutOfRangeException", System_IndexOutOfRangeException),
    EXCEPTION_INIT("System", "NullReferenceException", System_NullReferenceException),
    EXCEPTION_INIT("System", "InvalidCastException", System_InvalidCastException),
    EXCEPTION_INIT("System", "OutOfMemoryException", System_OutOfMemoryException),
    EXCEPTION_INIT("System", "OverflowException", System_OverflowException),
    TYPE_INIT("TinyDotNet.Reflection", "InterfaceImpl", TinyDotNet_Reflection_InterfaceImpl),
    TYPE_INIT("TinyDotNet.Reflection", "MemberReference", TinyDotNet_Reflection_MemberReference),
    TYPE_INIT("TinyDotNet.Reflection", "MethodImpl", TinyDotNet_Reflection_MethodImpl),
    TYPE_INIT("TinyDotNet.Reflection", "MethodSpec", TinyDotNet_Reflection_MethodSpec),
    VALUE_TYPE_INIT("System", "RuntimeTypeHandle", System_RuntimeTypeHandle, STACK_TYPE_VALUE_TYPE),
    VALUE_TYPE_INIT("System", "Span`1", System_Span, STACK_TYPE_VALUE_TYPE),
    VALUE_TYPE_INIT("System", "Nullable`1", System_Nullable, STACK_TYPE_VALUE_TYPE),

    TYPE_LOOKUP("System.Runtime.CompilerServices", "Unsafe", tSystem_Runtime_CompilerServices_Unsafe),
    TYPE_LOOKUP("System.Runtime.CompilerServices", "IsVolatile", tSystem_Runtime_CompilerServices_IsVolatile),
};

static void init_type(metadata_type_def_t* type_def, System_Type type) {
    // check if this is a builtin type
    for (int i = 0; i < ARRAY_LEN(m_type_init); i++) {
        type_init_t* bt = &m_type_init[i];
        if (
            strcmp(type_def->type_namespace, bt->namespace) == 0 &&
            strcmp(type_def->type_name, bt->name) == 0
        ) {
            if (type->StackSize >= 0) {
                type->ManagedSize = bt->managed_size;
                type->StackSize = bt->stack_size;
                type->ManagedAlignment = bt->managed_alignment;
                type->StackAlignment = bt->stack_alignment;
                type->StackType = bt->stack_type;
            }
            *bt->global = type;
            break;
        }
    }
}

static err_t validate_have_init_types() {
    err_t err = NO_ERROR;

    bool missing = false;
    for (int i = 0; i < ARRAY_LEN(m_type_init); i++) {
        type_init_t* bt = &m_type_init[i];
        if (*bt->global == NULL) {
            TRACE("Missing `%s.%s`!", bt->namespace, bt->name);
            missing = true;
        }
    }
    CHECK(!missing);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// invoke all the cctors
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static err_t loader_setup_module(System_Reflection_Assembly assembly, metadata_t* metadata) {
    err_t err = NO_ERROR;

    // setup the assembly
    metadata_assembly_t* mt_assembly = metadata->tables[METADATA_ASSEMBLY].table;
    GC_UPDATE(assembly, Name, new_string_from_cstr(mt_assembly->name));
    assembly->MajorVersion = mt_assembly->major_version;
    assembly->MinorVersion = mt_assembly->minor_version;
    assembly->BuildNumber = mt_assembly->build_number;
    assembly->RevisionNumber = mt_assembly->revision_number;

    // create the module
    metadata_module_t* module = metadata->tables[METADATA_MODULE].table;
    GC_UPDATE(assembly, Module, UNSAFE_GC_NEW(tSystem_Reflection_Module));
    GC_UPDATE(assembly->Module, Name, new_string_from_cstr(module->name));
    GC_UPDATE(assembly->Module, Assembly, assembly);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// corelib is a bit different so load it as needed
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define RESET_TYPE(instance, __type) \
    do { \
        instance->type = (uintptr_t)__type; \
        instance->vtable = __type->VTable; \
    } while (0);


err_t loader_load_corelib(void* buffer, size_t buffer_size) {
    err_t err = NO_ERROR;
    metadata_t metadata = { 0 };

    uint64_t start = microtime();

    // Start by loading the PE file for the corelib
    pe_file_t file = {
        .file = buffer,
        .file_size = buffer_size
    };
    CHECK_AND_RETHROW(pe_parse(&file));

    // decode the dotnet metadata
    CHECK_AND_RETHROW(decode_metadata(&file, &metadata));

    // allocate the corelib on the kernel heap and not the object heap, just because
    // it is always going to be allocated anyways
    System_Reflection_Assembly assembly = gc_new(NULL, sizeof(struct System_Reflection_Assembly));
    CHECK(assembly != NULL);

    // setup the basic type system
    int types_count = metadata.tables[METADATA_TYPE_DEF].rows;
    metadata_type_def_t* type_defs = metadata.tables[METADATA_TYPE_DEF].table;

    int method_count = metadata.tables[METADATA_METHOD_DEF].rows;
    int field_count = metadata.tables[METADATA_FIELD].rows;

    // do first time allocation and init
    assembly->DefinedTypes = gc_new(NULL, sizeof(struct System_Array) + types_count * sizeof(System_Type));
    assembly->DefinedTypes->Length = types_count;
    for (int i = 0; i < types_count; i++) {
        metadata_type_def_t* type_def = &type_defs[i];
        assembly->DefinedTypes->Data[i] = gc_new(NULL, sizeof(struct System_Type));
        CHECK(assembly->DefinedTypes->Data[i] != NULL);
        init_type(type_def, assembly->DefinedTypes->Data[i]);
    }

    // validate we got all the base types we need for a proper runtime
    CHECK_AND_RETHROW(validate_have_init_types());

    // create the module
    CHECK_AND_RETHROW(loader_setup_module(assembly, &metadata));

    assembly->DefinedMethods = gc_new(NULL, sizeof(struct System_Array) + method_count * sizeof(System_Reflection_MethodInfo));
    assembly->DefinedMethods->Length = method_count;
    assembly->DefinedFields = gc_new(NULL, sizeof(struct System_Array) + field_count * sizeof(System_Reflection_FieldInfo));
    assembly->DefinedFields->Length = field_count;

    // we need the nested types before we finish up the setup type info
    CHECK_AND_RETHROW(connect_nested_types(assembly, &metadata));

    // do first time type init
    CHECK_AND_RETHROW(setup_type_info(&file, &metadata, assembly));

    // initialize all the runtime required types
    for (int i = 0; i < ARRAY_LEN(m_type_init); i++) {
        type_init_t* bt = &m_type_init[i];
        System_Type type = *bt->global;
        CHECK_AND_RETHROW(loader_fill_type(type));
    }

    //
    // now set all the page tables, because we are missing them at
    // this point of writing
    //

    RESET_TYPE(assembly, tSystem_Reflection_Assembly);
    RESET_TYPE(assembly->Module, tSystem_Reflection_Module);
    RESET_TYPE(assembly->DefinedTypes, get_array_type(tSystem_Type));
    RESET_TYPE(assembly->DefinedMethods, get_array_type(tSystem_Reflection_MethodInfo));
    RESET_TYPE(assembly->DefinedFields, get_array_type(tSystem_Reflection_FieldInfo));
    for (int i = 0; i < types_count; i++) {
        RESET_TYPE(assembly->DefinedTypes->Data[i], tSystem_Type);
    }

    // get the user strings for the runtime
    CHECK_AND_RETHROW(parse_user_strings(assembly, &file));

    // save this
    g_corelib = assembly;
    gc_add_root(&g_corelib);

cleanup:
    free_metadata(&metadata);
    free_pe_file(&file);

    if (!IS_ERROR(err)) {
        TRACE("loading assembly `%U` (v%d.%d.%d.%d) took %dms",
              assembly->Name,
              assembly->MajorVersion, assembly->MinorVersion, assembly->BuildNumber, assembly->RevisionNumber,
              (microtime() - start) / 1000);
    }

    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// this is the normal parsing and initialization
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

err_t loader_load_assembly(void* buffer, size_t buffer_size, System_Reflection_Assembly* out_assembly) {
    err_t err = NO_ERROR;
    metadata_t metadata = { 0 };
    uint64_t start = microtime();

    // Start by loading the PE file for the corelib
    pe_file_t file = {
        .file = buffer,
        .file_size = buffer_size
    };
    CHECK_AND_RETHROW(pe_parse(&file));

    // decode the dotnet metadata
    CHECK_AND_RETHROW(decode_metadata(&file, &metadata));

    // allocate the new assembly
    System_Reflection_Assembly assembly = UNSAFE_GC_NEW(tSystem_Reflection_Assembly);

    // load all the types and stuff
    int types_count = metadata.tables[METADATA_TYPE_DEF].rows;
    int method_count = metadata.tables[METADATA_METHOD_DEF].rows;
    int field_count = metadata.tables[METADATA_FIELD].rows;

    // create all the types
    GC_UPDATE(assembly, DefinedTypes, GC_NEW_ARRAY(tSystem_Type, types_count));
    for (int i = 0; i < types_count; i++) {
        GC_UPDATE_ARRAY(assembly->DefinedTypes, i, UNSAFE_GC_NEW(tSystem_Type));
    }

    CHECK_AND_RETHROW(loader_setup_module(assembly, &metadata));

    // create all the methods and fields
    GC_UPDATE(assembly, DefinedMethods, GC_NEW_ARRAY(tSystem_Reflection_MethodInfo, method_count));
    GC_UPDATE(assembly, DefinedFields, GC_NEW_ARRAY(tSystem_Reflection_FieldInfo, field_count));

    // we need the nested types before we finish up the setup type info
    CHECK_AND_RETHROW(connect_nested_types(assembly, &metadata));

    // do first time type init
    CHECK_AND_RETHROW(setup_type_info(&file, &metadata, assembly));

    // get the user strings for the runtime
    CHECK_AND_RETHROW(parse_user_strings(assembly, &file));

    // get the entry point
    System_Reflection_MethodInfo entryPoint = NULL;
    CHECK_AND_RETHROW(assembly_get_method_by_token(assembly, file.cli_header->entry_point_token, NULL, NULL, &entryPoint));
    GC_UPDATE(assembly, EntryPoint, entryPoint);

    // give out the assembly
    *out_assembly = assembly;

cleanup:
    free_metadata(&metadata);
    free_pe_file(&file);

    if (!IS_ERROR(err)) {
        TRACE("loading assembly `%U` (v%d.%d.%d.%d) took %dms",
              assembly->Name,
              assembly->MajorVersion, assembly->MinorVersion, assembly->BuildNumber, assembly->RevisionNumber,
              (microtime() - start) / 1000);
    }

    return err;
}
