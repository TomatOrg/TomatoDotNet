#include "jit_codegen.h"

#include <string.h>
#include <util/stb_ds.h>
#include <spidir/codegen.h>
#include <spidir/x64.h>
#include <tomatodotnet/types/type.h>
#include <util/except.h>

#include "jit.h"
#include "jit_helpers.h"
#include "jit_type.h"

static spidir_codegen_machine_handle_t m_codegen_machine = NULL;

void jit_codegen_init(void) {
    m_codegen_machine = spidir_codegen_create_x64_machine();
}

typedef struct jit_codegen_entry {
    spidir_function_t key;
    spidir_codegen_blob_handle_t blob;
    RuntimeMethodBase method;
} jit_codegen_entry_t;

static spidir_function_t* m_functions_to_jit = NULL;
static jit_codegen_entry_t* m_function_blobs = NULL;

void jit_codegen_queue(RuntimeMethodBase method, spidir_function_t function) {
    // if already in queue skip
    if (hmgeti(m_function_blobs, function) >= 0) {
        return;
    }

    // queue the function
    arrpush(m_functions_to_jit, function);

    // add to the blobs list
    jit_codegen_entry_t entry = {
        .method = method,
        .key = function
    };
    hmputs(m_function_blobs, entry);
}

/**
 * Perform the actual relocation on the given blob
 */
static tdn_err_t jit_relocate_function(void* method_ptr, spidir_codegen_blob_handle_t blob) {
    tdn_err_t err = TDN_NO_ERROR;

    size_t reloc_count = spidir_codegen_blob_get_reloc_count(blob);
    const spidir_codegen_reloc_t* relocs = spidir_codegen_blob_get_relocs(blob);
    for (size_t j = 0; j < reloc_count; j++) {
        uint64_t P = (uint64_t)(method_ptr + relocs[j].offset);
        int64_t A = relocs[j].addend;

        // resolve the target, will either be a builtin or a
        // method pointer we jitted
        uint64_t F;
        RuntimeMethodBase target = jit_get_method_from_function(relocs[j].target);
        if (target == NULL) {
            void* ptr = jit_get_helper_ptr(relocs[j].target);
            CHECK(ptr != NULL);
            F = (uint64_t)ptr;
        } else {
            CHECK(target->MethodPtr != NULL);
            F = (uint64_t)target->MethodPtr;
        }

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
            spidir_function_t callee = relocs[i].target;
            RuntimeMethodBase callee_method = jit_get_method_from_function(callee);
            if (callee_method != NULL) {
                jit_codegen_queue(callee_method, callee);
            }
        }
    }

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
        method->MethodSize = code_size;
        method->MethodPtr = jit_area + offset;
        memcpy(method->MethodPtr, code, method->MethodSize);

        // generate a value type virtual thunk
        if (method->Attributes.Virtual && tdn_type_is_valuetype(method->DeclaringType)) {
            // get the object header size
            size_t object_header_size = jit_get_boxed_value_offset(method->DeclaringType);
            CHECK(object_header_size <= 0x7F);

            // add rdi, $object_header_size
            uint8_t opcode[4] = { 0x48, 0x83, 0xC7, object_header_size };
            method->ThunkPtr = jit_area + offset - sizeof(opcode);
            method->ThunkSize = sizeof(opcode);
            memcpy(method->ThunkPtr, opcode, method->ThunkSize);
        }

        // TODO: static delegate thunk with 3 or less arguments

        offset += code_size;
    }

    // ensure we got to the same value
    CHECK(offset == total_size);

    // now perform the relocations
    for (int i = 0; i < hmlen(m_function_blobs); i++) {
        jit_codegen_entry_t* entry = &m_function_blobs[i];
        CHECK_AND_RETHROW(jit_relocate_function(entry->method->MethodPtr, entry->blob));
    }

    // turn the area into executable memory
    tdn_host_jit_set_exec(jit_area, total_size);

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
