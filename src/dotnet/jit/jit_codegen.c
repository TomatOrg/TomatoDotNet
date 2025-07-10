#include "jit_codegen.h"

#include <string.h>
#include "tomatodotnet/util/stb_ds.h"
#include <spidir/codegen.h>
#include <spidir/x64.h>
#include <tomatodotnet/types/type.h>
#include <util/except.h>

#include "jit.h"
#include "jit_gdb.h"
#include "jit_elf.h"
#include "jit_helpers.h"
#include "jit_type.h"

static spidir_codegen_machine_handle_t m_codegen_machine = NULL;

void jit_codegen_init(void) {
    m_codegen_machine = spidir_codegen_create_x64_machine();
}

static spidir_function_t* m_functions_to_jit = NULL;
static jit_codegen_entry_t* m_function_blobs = NULL;

void jit_codegen_queue(RuntimeMethodBase method, spidir_function_t function, bool thunk) {
    // if already in queue skip
    if (hmgeti(m_function_blobs, function) >= 0) {
        return;
    }

    // if already fully jitted no reason to queue it again for codegen,
    // including for the thunk case
    if (thunk) {
        if (method->ThunkPtr != NULL) {
            return;
        }
    } else {
        if (method->MethodPtr != NULL) {
            return;
        }
    }

    // queue the function
    arrpush(m_functions_to_jit, function);

    // add to the blobs list
    jit_codegen_entry_t entry = {
        .method = method,
        .key = function,
        .thunk = thunk
    };
    hmputs(m_function_blobs, entry);
}

static spidir_funcref_t get_reloc_target(const spidir_codegen_reloc_t* reloc) {
    if (reloc->target_kind == SPIDIR_RELOC_TARGET_INTERNAL_FUNCTION) {
        return spidir_funcref_make_internal(reloc->target.internal);
    } else if (reloc->target_kind == SPIDIR_RELOC_TARGET_EXTERNAL_FUNCTION) {
        return spidir_funcref_make_external(reloc->target.external);
    } else {
        ASSERT(!"Invalid relocation target kind");
    }
}

/**
 * Perform the actual relocation on the given blob
 */
static tdn_err_t jit_relocate_function(RuntimeMethodBase method, bool thunk, spidir_codegen_blob_handle_t blob) {
    tdn_err_t err = TDN_NO_ERROR;

    void* code = thunk ? method->ThunkPtr : method->MethodPtr;
    size_t code_size = thunk ? method->ThunkSize : method->MethodSize;

    // calculate the offset
    size_t constpool_offset = ALIGN_UP(code_size, spidir_codegen_blob_get_constpool_align(blob));
    void* constpool_ptr = code + constpool_offset;

    size_t reloc_count = spidir_codegen_blob_get_reloc_count(blob);
    const spidir_codegen_reloc_t* relocs = spidir_codegen_blob_get_relocs(blob);
    for (size_t j = 0; j < reloc_count; j++) {
        uint64_t P = (uint64_t)(code + relocs[j].offset);
        int64_t A = relocs[j].addend;

        // resolve the target, will either be a builtin or a
        // method pointer we jitted
        uint64_t F;
        switch (relocs[j].target_kind) {
            case SPIDIR_RELOC_TARGET_EXTERNAL_FUNCTION:
            case SPIDIR_RELOC_TARGET_INTERNAL_FUNCTION: {
                // get the generic funcref
                // TODO: can we make this look a bit better? maybe more optimized?
                spidir_funcref_t target_func = get_reloc_target(&relocs[j]);

                RuntimeMethodBase target = jit_get_method_from_function(target_func);
                bool thunk = false;
                if (target == NULL) {
                    target = jit_get_thunk_method(target_func);
                    if (target != NULL) {
                        thunk = true;
                    }
                }

                if (target == NULL) {
                    void* ptr = jit_get_helper_ptr(target_func);
                    CHECK(ptr != NULL, "Failed to get function reference to %d while relocating %T::%U",
                        relocs[j].target, method->DeclaringType, method->Name);
                    F = (uint64_t)ptr;
                } else {
                    if (thunk) {
                        CHECK(target->ThunkPtr != NULL);
                        F = (uint64_t)target->ThunkPtr;
                    } else {
                        CHECK(target->MethodPtr != NULL);
                        F = (uint64_t)target->MethodPtr;
                    }
                }
            } break;

            case SPIDIR_RELOC_TARGET_CONSTPOOL: {
                F = (uint64_t)constpool_ptr;
            } break;

            default: CHECK_FAIL();
        }

        // and now actually apply the relocation
        switch (relocs[j].kind) {
            case SPIDIR_RELOC_X64_PC32: {
                int64_t value = F + A - P;
                CHECK(INT32_MIN <= value && value <= INT32_MAX, "%p", value);
                int32_t pc32 = value;
                memcpy((void*)P, &pc32, sizeof(pc32));
            } break;

            case SPIDIR_RELOC_X64_ABS64: {
                uint64_t value = F + A;
                memcpy((void*)P, &value, sizeof(value));
            } break;

            default:
                CHECK_FAIL("Unknown relocation kind: %d", relocs[j].kind);
        }
    }

cleanup:
    return err;
}

tdn_err_t jit_codegen(spidir_module_handle_t module) {
    tdn_err_t err = TDN_NO_ERROR;

    // go over all the functions and generate code for them
    while (arrlen(m_functions_to_jit) != 0) {
        spidir_function_t function = arrpop(m_functions_to_jit);
        jit_codegen_entry_t* entry = hmgetp_null(m_function_blobs, function);
        CHECK(entry != NULL);

        // perform the codegen
        spidir_codegen_config_t config = {
            .verify_ir = true,
            .verify_regalloc = true,
        };
        spidir_codegen_status_t status = spidir_codegen_emit_function(m_codegen_machine, &config, module, function, &entry->blob);
        CHECK(status == SPIDIR_CODEGEN_OK, "Got codegen error %d", status);

        // go over the relocs and add the required functions to be jitted
        size_t reloc_count = spidir_codegen_blob_get_reloc_count(entry->blob);
        const spidir_codegen_reloc_t* relocs = spidir_codegen_blob_get_relocs(entry->blob);
        for (int i = 0; i < reloc_count; i++) {
            // ignore anything that is not a function call
            // to an internal function
            if (relocs[i].target_kind != SPIDIR_RELOC_TARGET_INTERNAL_FUNCTION) {
                continue;
            }

            spidir_funcref_t callee = spidir_funcref_make_internal(relocs[i].target.internal);
            RuntimeMethodBase callee_method = jit_get_method_from_function(callee);
            bool thunk = false;
            if (callee_method == NULL) {
                callee_method = jit_get_thunk_method(callee);
                if (callee_method != NULL) {
                    thunk = true;
                }
            }

            // queue it
            jit_codegen_queue(callee_method, spidir_funcref_get_internal(callee), thunk);
        }
    }

    // dump the entire elf
    jit_elf_start_emit();
    for (int i = 0; i < hmlen(m_function_blobs); i++) {
        jit_elf_add_entry(&m_function_blobs[i]);
    }
    jit_elf_emit();

    // now that we are done, calculate the total size we need
    // for all of the functions
    size_t total_size = 0;
    for (int i = 0; i < hmlen(m_function_blobs); i++) {
        jit_codegen_entry_t* entry = &m_function_blobs[i];

        // add 16 bytes for a prefix, and align
        // the whole function to 16 bytes
        total_size += 16;
        total_size = ALIGN_UP(total_size, 16);
        total_size += spidir_codegen_blob_get_code_size(entry->blob);

        // if we have a constant pool, allocate space for it as well
        size_t constpool_size = spidir_codegen_blob_get_constpool_size(entry->blob);
        if (constpool_size != 0) {
            total_size = ALIGN_UP(total_size, spidir_codegen_blob_get_constpool_align(entry->blob));
            total_size += constpool_size;
        }
    }

    if (total_size == 0) {
        goto cleanup;
    }

    // now we can map the area
    void* jit_area = tdn_host_jit_alloc(total_size);
    CHECK(jit_area != NULL);

    // now that we are done, calculate the total size we need
    // for all of the functions
    size_t offset = 0;
    for (int i = 0; i < hmlen(m_function_blobs); i++) {
        jit_codegen_entry_t* entry = &m_function_blobs[i];
        RuntimeMethodBase method = entry->method;

        // add 16 bytes for a prefix, and align
        // the whole function to 16 bytes
        offset += 16;
        offset = ALIGN_UP(offset, 16);

        // copy the code
        size_t code_size = spidir_codegen_blob_get_code_size(entry->blob);
        const void* code = spidir_codegen_blob_get_code(entry->blob);
        if (entry->thunk) {
            method->ThunkSize = code_size;
            method->ThunkPtr = jit_area + offset;
        } else {
            method->MethodSize = code_size;
            method->MethodPtr = jit_area + offset;
        }
        memcpy(jit_area + offset, code, code_size);

        // generate a value type virtual thunk
        if (method->Attributes.Virtual && tdn_type_is_valuetype(method->DeclaringType)) {
            // get the object header size
            size_t object_header_size = jit_get_boxed_value_offset(method->DeclaringType);
            CHECK(object_header_size <= 0x7F);

            // add rdi, $object_header_size
            CHECK(method->ThunkPtr == NULL);
            uint8_t opcode[4] = { 0x48, 0x83, 0xC7, object_header_size };
            method->ThunkPtr = jit_area + offset - sizeof(opcode);
            method->ThunkSize = sizeof(opcode);
            memcpy(method->ThunkPtr, opcode, method->ThunkSize);
        }

        offset += code_size;

        // if we have a constant pool, initialize it
        size_t constpool_size = spidir_codegen_blob_get_constpool_size(entry->blob);
        if (constpool_size != 0) {
            offset = ALIGN_UP(offset, spidir_codegen_blob_get_constpool_align(entry->blob));
            memcpy(jit_area + offset, spidir_codegen_blob_get_constpool(entry->blob), constpool_size);
            offset += constpool_size;
        }
    }

    // ensure we got to the same value
    CHECK(offset == total_size);

    // now perform the relocations
    for (int i = 0; i < hmlen(m_function_blobs); i++) {
        jit_codegen_entry_t* entry = &m_function_blobs[i];
        CHECK_AND_RETHROW(jit_relocate_function(entry->method, entry->thunk, entry->blob));
    }

    // turn the area into executable memory
    tdn_host_jit_set_exec(jit_area, total_size);

    // register for the debugger to see
    jit_gdb_register(m_function_blobs, jit_area, total_size);

cleanup:
    return err;
}

void jit_codgen_cleanup() {
    // properly free everything
    for (int i = 0; i < hmlen(m_function_blobs); i++) {
        if (m_function_blobs[i].blob != NULL) {
            spidir_codegen_blob_destroy(m_function_blobs[i].blob);
        }
    }
    hmfree(m_function_blobs);
    arrfree(m_functions_to_jit);
}
