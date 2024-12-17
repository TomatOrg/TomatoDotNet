#include "jit_emit.h"

#include <stdalign.h>
#include <dotnet/gc/gc.h>
#include <dotnet/metadata/metadata.h>
#include <util/except.h>
#include <util/stb_ds.h>

#include <spidir/module.h>
#include <spidir/codegen.h>
#include <spidir/x64.h>
#include <spidir/opt.h>

#include <tomatodotnet/disasm.h>
#include <tomatodotnet/types/type.h>
#include <util/alloc.h>
#include <util/string.h>
#include <util/string_builder.h>

#include "jit.h"
#include "jit_builtin.h"
#include "jit_helpers.h"
#include "jit_verify.h"

/**
 * The module used for jitting
 */
static spidir_module_handle_t m_jit_module = NULL;

spidir_function_t g_jit_bzero;
spidir_function_t g_jit_memcpy;

spidir_function_t g_jit_leading_zero_count_32;
spidir_function_t g_jit_leading_zero_count_64;

static spidir_function_t m_jit_gc_new;
static spidir_function_t m_jit_gc_newarr;
static spidir_function_t m_jit_gc_newstr;

static spidir_function_t m_jit_throw;

static spidir_function_t m_jit_throw_invalid_cast_exception;
static spidir_function_t m_jit_throw_index_out_of_range_exception;

static spidir_function_t m_jit_interface_downcast;

static struct {
    spidir_function_t key;
    void* value;
}* m_jit_helper_lookup = NULL;

spidir_function_t g_jit_print_str;
spidir_function_t g_jit_print_int;
spidir_function_t g_jit_print_ptr;

static void create_jit_debug_helpers() {
    g_jit_print_str = spidir_module_create_extern_function(m_jit_module,
        "jit_print_str",
        SPIDIR_TYPE_NONE,
        1,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_PTR
        }
    );
    hmput(m_jit_helper_lookup, g_jit_print_str, jit_print_str);

    g_jit_print_int = spidir_module_create_extern_function(m_jit_module,
        "jit_print_int",
        SPIDIR_TYPE_NONE,
        1,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_I32
        }
    );
    hmput(m_jit_helper_lookup, g_jit_print_int, jit_print_int);

    g_jit_print_ptr = spidir_module_create_extern_function(m_jit_module,
        "jit_print_ptr",
        SPIDIR_TYPE_NONE,
        1,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_PTR
        }
    );
    hmput(m_jit_helper_lookup, g_jit_print_ptr, jit_print_ptr);
}

static void create_jit_helpers() {
    create_jit_debug_helpers();

    g_jit_bzero = spidir_module_create_extern_function(m_jit_module,
        "jit_bzero",
        SPIDIR_TYPE_NONE,
        2,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_PTR,
            SPIDIR_TYPE_I64
        }
    );
    hmput(m_jit_helper_lookup, g_jit_bzero, jit_bzero);

    g_jit_memcpy = spidir_module_create_extern_function(m_jit_module,
        "jit_memcpy",
        SPIDIR_TYPE_NONE,
        3,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_PTR,
            SPIDIR_TYPE_PTR,
            SPIDIR_TYPE_I64
        }
    );
    hmput(m_jit_helper_lookup, g_jit_memcpy, jit_memcpy);

    m_jit_gc_new = spidir_module_create_extern_function(m_jit_module,
        "jit_gc_new",
        SPIDIR_TYPE_PTR,
        1,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_PTR
        }
    );
    hmput(m_jit_helper_lookup, m_jit_gc_new, jit_gc_new);

    m_jit_gc_newarr = spidir_module_create_extern_function(m_jit_module,
        "jit_gc_newarr",
        SPIDIR_TYPE_PTR,
        2,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_PTR,
            SPIDIR_TYPE_I64
        }
    );
    hmput(m_jit_helper_lookup, m_jit_gc_newarr, jit_gc_newarr);

    m_jit_gc_newstr = spidir_module_create_extern_function(m_jit_module,
        "jit_gc_newstr",
        SPIDIR_TYPE_PTR,
        1,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_I32
        }
    );
    hmput(m_jit_helper_lookup, m_jit_gc_newstr, jit_gc_newstr);

    m_jit_interface_downcast = spidir_module_create_extern_function(m_jit_module,
        "jit_interface_downcast",
        SPIDIR_TYPE_PTR,
        2,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_PTR,
            SPIDIR_TYPE_PTR
        }
    );
    hmput(m_jit_helper_lookup, m_jit_interface_downcast, jit_interface_downcast);

    m_jit_throw = spidir_module_create_extern_function(m_jit_module,
        "jit_throw",
        SPIDIR_TYPE_NONE,
        2,
        (spidir_value_type_t[]){
            SPIDIR_TYPE_PTR,
            SPIDIR_TYPE_I32,
        }
    );
    hmput(m_jit_helper_lookup, m_jit_throw, jit_throw);

    m_jit_throw_invalid_cast_exception = spidir_module_create_extern_function(m_jit_module,
        "jit_throw_invalid_cast_exception",
        SPIDIR_TYPE_NONE,
        0, NULL
    );
    hmput(m_jit_helper_lookup, m_jit_throw_invalid_cast_exception, jit_throw_invalid_cast_exception);

    m_jit_throw_index_out_of_range_exception = spidir_module_create_extern_function(m_jit_module,
        "jit_throw_index_out_of_range_exception",
        SPIDIR_TYPE_NONE,
        0, NULL
    );
    hmput(m_jit_helper_lookup, m_jit_throw_index_out_of_range_exception, jit_throw_index_out_of_range_exception);

    g_jit_leading_zero_count_32 = spidir_module_create_extern_function(m_jit_module,
        "jit_leading_zero_count_32",
        SPIDIR_TYPE_I32,
        1, (spidir_value_type_t[]){ SPIDIR_TYPE_I32 }
    );
    hmput(m_jit_helper_lookup, g_jit_leading_zero_count_32, jit_leading_zero_count_32);

    g_jit_leading_zero_count_64 = spidir_module_create_extern_function(m_jit_module,
        "jit_leading_zero_count_64",
        SPIDIR_TYPE_I32,
        1, (spidir_value_type_t[]){ SPIDIR_TYPE_I64 }
    );
    hmput(m_jit_helper_lookup, g_jit_leading_zero_count_64, jit_leading_zero_count_32);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The code emitting itself
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

typedef struct jit_emit_ctx {
    // the method we are working on
    jit_method_t* method;

    tdn_err_t err;
} jit_emit_ctx_t;

static spidir_value_type_t get_spidir_type(RuntimeTypeInfo info) {
    info = tdn_get_intermediate_type(info);
    if (info == tInt32) {
        return SPIDIR_TYPE_I32;
    } else if (info == tInt64 || info == tIntPtr) {
        return SPIDIR_TYPE_I64;
    } else {
        return SPIDIR_TYPE_PTR;
    }
}

static spidir_mem_size_t get_spidir_mem_size(RuntimeTypeInfo info) {
    switch (info->StackSize) {
        case 1: return SPIDIR_MEM_SIZE_1;
        case 2: return SPIDIR_MEM_SIZE_2;
        case 4: return SPIDIR_MEM_SIZE_4;
        case 8: return SPIDIR_MEM_SIZE_8;
        default: ASSERT(!"Invalid size");
    }
}

spidir_value_type_t jit_get_spidir_ret_type(RuntimeMethodBase method) {
    RuntimeTypeInfo type = tdn_get_intermediate_type(method->ReturnParameter->ParameterType);
    if (type == tInt32) {
        return SPIDIR_TYPE_I32;
    } else if (type == tInt64 || type == tIntPtr) {
        return SPIDIR_TYPE_I64;
    } else if (jit_is_struct_like(type)) {
        // things which act like a struct return by using reference
        return SPIDIR_TYPE_NONE;
    } else {
        ASSERT(tdn_type_is_referencetype(type) || type->IsByRef);
        return SPIDIR_TYPE_PTR;
    }
}

spidir_value_type_t* jit_get_spidir_arg_types(RuntimeMethodBase method) {
    spidir_value_type_t* types = NULL;

    // this pointer
    if (!method->Attributes.Static) {
        arrpush(types, SPIDIR_TYPE_PTR);
    }

    // and now all of the arguments
    for (int i = 0; i < method->Parameters->Length; i++) {
        spidir_value_type_t type = get_spidir_type(method->Parameters->Elements[i]->ParameterType);
        arrpush(types, type);
    }

    // implicit retval pointer
    RuntimeTypeInfo ret_type = method->ReturnParameter->ParameterType;
    if (
        ret_type != tVoid &&
        jit_is_struct_like(ret_type)
    ) {
        arrpush(types, SPIDIR_TYPE_PTR);
    }

    return types;
}


static jit_basic_block_t* get_basic_block(jit_method_t* method, long target_pc, long leave_target) {
    int bi = hmgeti(method->labels, target_pc);
    if (bi < 0) {
        return NULL;
    }
    jit_basic_block_t* block = method->labels[bi].value;

    // lookup based on the leave target
    if (leave_target >= 0) {
        jit_leave_block_key_t key = {
            .block = block,
            .leave_target = leave_target
        };

        int bi = hmgeti(method->leave_blocks, key);
        if (bi < 0) {
            return NULL;
        }
        block = method->leave_blocks[bi].value;
    }

    return block;
}

static void jit_queue_block(jit_method_t* method, spidir_builder_handle_t builder, jit_basic_block_t* block) {
    if (block->state <= JIT_BLOCK_VERIFIED) {
        block->state = JIT_BLOCK_PENDING_EMIT;

        // create the block
        block->block = spidir_builder_create_block(builder);

        // create the phis for this block
        if (block->needs_phi) {
            // switch to the target block
            spidir_block_t current;
            ASSERT(spidir_builder_cur_block(builder, &current));
            spidir_builder_set_block(builder, block->block);

            for (int i = 0; i < arrlen(block->stack); i++) {
                block->stack[i].value = spidir_builder_build_phi(
                    builder,
                    get_spidir_type(block->stack[i].type),
                    0, NULL,
                    &block->stack[i].phi
                );
            }

            // switch back
            spidir_builder_set_block(builder, current);
        }

        // queue it
        arrpush(method->block_queue, block);
    }
}

static long get_leave_target(uint32_t* leave_target_stack) {
    if (leave_target_stack == NULL) {
        return -1;
    }
    return arrlast(leave_target_stack);
}

static tdn_err_t emit_merge_basic_block(
    jit_method_t* method,
    spidir_builder_handle_t builder,
    uint32_t target_pc,
    jit_stack_value_t* stack,
    spidir_block_t* block,
    long leave_target
) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_basic_block_t* target = get_basic_block(method, target_pc, leave_target);
    CHECK(target != NULL);

    // queue the block, will also handle creating the phi if required
    jit_queue_block(method, builder, target);

    // add all the phi inputs
    // TODO: in theory we only need to do this on the entries that are at the top
    //       of the stack to the lowest this block goes
    if (target->needs_phi) {
        CHECK(arrlen(stack) == arrlen(target->stack));
        for (int i = 0; i < arrlen(stack); i++) {
            spidir_builder_add_phi_input(builder,
                target->stack[i].phi,
                stack[i].value);
        }
    } else {
        // copy all of the stack over
        for (int i = 0; i < arrlen(stack); i++) {
            target->stack[i].value = stack[i].value;
        }
    }

    // output the block if this entry
    *block = target->block;

cleanup:
    return err;
}

#define EVAL_STACK_PUSH(_type, _value, ...) \
    do { \
        CHECK(arrlen(stack) < body->MaxStackSize); \
        jit_stack_value_t __item = { .type = tdn_get_intermediate_type(_type), .value = _value, ## __VA_ARGS__ }; \
        arrpush(stack, __item); \
    } while (0)

#define EVAL_STACK_POP() \
    ({ \
        CHECK(arrlen(stack) > 0); \
        arrpop(stack); \
    })

#define SWAP(a, b) \
    do { \
        typeof(a) __tmp = a; \
        a = b; \
        b = __tmp; \
    } while(0)

#define GET_ARG_TYPE(_index) \
    ({ \
        typeof(_index) __index = _index; \
        RuntimeTypeInfo __arg_type; \
        if (this_type != NULL) { \
            if (__index == 0) { \
                __arg_type = this_type; \
            } else { \
                __index--; \
                CHECK(__index < method->Parameters->Length); \
                __arg_type = method->Parameters->Elements[__index]->ParameterType; \
            } \
        } else { \
            CHECK(__index < method->Parameters->Length); \
            __arg_type = method->Parameters->Elements[__index]->ParameterType; \
        } \
        __arg_type; \
    })

static void jit_register_roots(void* ptr, RuntimeTypeInfo type) {
    if (jit_is_struct(type)) {
        // need to go over all its fields
        for (int i = 0; i < type->DeclaredFields->Length; i++) {
            RuntimeFieldInfo field = type->DeclaredFields->Elements[i];
            if (!field->Attributes.Static) {
                jit_register_roots(ptr + field->FieldOffset, field->FieldType);
            }
        }

    } else if (tdn_type_is_referencetype(type)) {
        // this is a reference, either the
        // actual object or the interface
        // instance
        gc_register_root(ptr);

    }
}

static tdn_err_t jit_init_static_field(RuntimeFieldInfo field) {
    tdn_err_t err = TDN_NO_ERROR;

    if (field->JitFieldPtr != NULL) {
        goto cleanup;
    }

    RuntimeTypeInfo type = field->FieldType;

    // allocate it on the heap, that is the easiest way to do it
    field->JitFieldPtr = tdn_mallocz(type->StackSize);
    CHECK_ERROR(field->JitFieldPtr != NULL, TDN_ERROR_OUT_OF_MEMORY);

    // register all the roots to the gc
    jit_register_roots(field->JitFieldPtr, type);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Struct slots
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static spidir_value_t jit_get_struct_slot(spidir_builder_handle_t builder, RuntimeTypeInfo type) {
    // TODO: how should we handle this the best
    return spidir_builder_build_stackslot(builder, type->StackSize, type->StackAlignment);
}

static void jit_release_struct_slot(spidir_value_t value) {
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Emit helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void jit_emit_memcpy(spidir_builder_handle_t builder, spidir_value_t dst, spidir_value_t src, RuntimeTypeInfo type) {
    // TODO: replace with a jit builtin-memcpy
    // TODO: use gc_memcpy in case contains a refernce
    spidir_builder_build_call(builder,
        g_jit_memcpy,
        3,
        (spidir_value_t[]){
            dst,
            src,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, type->StackSize)
        }
    );
}

static void jit_emit_bzero(spidir_builder_handle_t builder, spidir_value_t dst, RuntimeTypeInfo type) {
    // TODO: replace with a jit builtin-bzero/memset
    // TODO: use gc_bzero in case contains a refernce
    spidir_builder_build_call(builder,
        g_jit_bzero,
        2,
        (spidir_value_t[]){
            dst,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, type->StackSize)
        }
    );
}

static size_t jit_get_interface_offset(RuntimeTypeInfo type, RuntimeTypeInfo iface) {
    int idx = hmgeti(type->InterfaceImpls, iface);
    if (idx < 0) {
        return -1;
    }
    return type->InterfaceImpls[idx].value;
}

static interface_impl_t* jit_find_variant_interface(RuntimeTypeInfo type, RuntimeTypeInfo iface) {
    for (int i = 0; i < hmlen(type->InterfaceImpls); i++) {
        if (type->InterfaceImpls[i].key->GenericTypeDefinition != iface->GenericTypeDefinition) continue;
        RuntimeTypeInfo T = type->InterfaceImpls[i].key;
        RuntimeTypeInfo U = iface;

        // check that the variance
        bool matched = true;
        RuntimeTypeInfo base = T->GenericTypeDefinition;
        if (base != NULL && T->GenericArguments != NULL) {
            for (int j = 0; j < T->GenericArguments->Length; j++) {
                RuntimeTypeInfo Ti = T->GenericArguments->Elements[j];
                RuntimeTypeInfo Ui = U->GenericArguments->Elements[j];
                RuntimeTypeInfo base_typ = base->GenericArguments->Elements[j];
                uint32_t var_i = base_typ->GenericParameterAttributes.Variance;

                // a. var_i = none (no variance) and Ti is identical to Ui
                if (var_i == 0) {
                    if (Ti != Ui) {
                        matched = false;
                        break;
                    }
                }

                // b. var_i = + (covariance), and T i is compatible-with Ui
                if (var_i == TDN_GENERIC_PARAM_VARIANCE_COVARIANT) {
                    if (!tdn_type_compatible_with(Ti, Ui)) {
                        matched = false;
                        break;
                    }
                }

                // c. var_i = - (contravariance), and Ui is compatible-with Ti
                if (var_i == TDN_GENERIC_PARAM_VARIANCE_CONTRAVARIANT) {
                    if (!tdn_type_compatible_with(Ui, Ti)) {
                        matched = false;
                        break;
                    }
                }
            }
        }

        // matches everything
        if (matched) {
            return &type->InterfaceImpls[i];
        }
    }

    return NULL;
}

/**
 * Checks if the interface we convert to requires a stub in order to function
 */
static bool jit_needs_variant_vtable_stub(RuntimeTypeInfo wanted_iface, RuntimeTypeInfo got_iface) {
    if (wanted_iface->GenericArguments == NULL) {
        return false;
    }

    bool might_need = false;
    for (int i = 0; i < wanted_iface->GenericArguments->Length; i++) {
        RuntimeTypeInfo a = wanted_iface->GenericArguments->Elements[i];
        RuntimeTypeInfo b = got_iface->GenericArguments->Elements[i];
        if (a->Attributes.Interface != b->Attributes.Interface) {
            might_need = true;
        }
    }

    if (might_need) {
        // TODO: check for exact methods
        return true;
    }

    return false;
}

static bool jit_convert_interface(
    spidir_builder_handle_t builder,
    spidir_value_t dest, spidir_value_t src,
    RuntimeTypeInfo dest_type, RuntimeTypeInfo src_type
) {
    ASSERT(jit_is_interface(dest_type));

    if (!jit_is_interface(src_type)) {
        // object -> interface

        // store the instance
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, src, dest);

        // load the vtable pointer
        spidir_value_t vtable = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64, src);
        vtable = spidir_builder_build_inttoptr(builder, vtable);

        // calculate the offset from the vtable and add it to it
        size_t interface_offset = jit_get_interface_offset(src_type, dest_type);
        if (interface_offset == -1) {
            interface_impl_t* impl = jit_find_variant_interface(src_type, dest_type);
            if (impl != NULL) {
                if (jit_needs_variant_vtable_stub(dest_type, impl->key)) {
                    // we need to build a stub table to thinner/fatten the interfaces
                    ASSERT(!"TODO: Interface <-> object variance support");
                } else {
                    // we don't need any special thunk, use the normal vtable
                    interface_offset = impl->value;
                }
            } else {
                // could not find the target at jit time, assume we need a
                // runtime cast also assume we already checked it is a valid cast
                vtable = spidir_builder_build_call(
                    builder,
                    m_jit_interface_downcast,
                    2,
                    (spidir_value_t[]){
                        src,
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uintptr_t)dest_type)
                    }
                );
            }
        }

        // if we have a normal interface use it
        if (interface_offset != -1) {
            interface_offset = interface_offset * sizeof(void*) + offsetof(ObjectVTable, Functions);
            vtable = spidir_builder_build_ptroff(builder, vtable,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, interface_offset));
        }

        // and store it
        dest = spidir_builder_build_ptroff(builder, dest,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable)));
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, vtable, dest);

    } else if (dest_type != src_type) {
        // upcasting interfaces, need to move the vtable pointer
        ASSERT(jit_is_interface(dest_type));
        ASSERT(jit_is_interface(src_type));

        // copy the instance
        spidir_value_t instance = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, src);
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, instance, dest);

        // load the vtable pointer
        spidir_value_t vtable = spidir_builder_build_ptroff(builder, src,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable)));
        vtable = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, src);

        // calculate the offset from the vtable and add it to it
        size_t interface_offset = jit_get_interface_offset(src_type, dest_type);
        if (interface_offset == -1) {
            interface_impl_t* impl = jit_find_variant_interface(src_type, dest_type);
            ASSERT(impl != NULL);

            if (jit_needs_variant_vtable_stub(dest_type, impl->key)) {
                // we need to build a stub table to thinner/fatten the interfaces
                ASSERT(!"TODO: Interface <-> object variance support");
            } else {
                // we don't need any special thunk, use the normal vtable
                interface_offset = impl->value;
            }
        }
        interface_offset = interface_offset * sizeof(void*);
        vtable = spidir_builder_build_ptroff(builder, vtable,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, interface_offset));

        // and store it
        dest = spidir_builder_build_ptroff(builder, dest,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable)));
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, vtable, dest);

    } else {
        // nothing was needed to be done
        // so we did not emit a store at all
        return false;
    }

    return true;
}

static void jit_emit_store(spidir_builder_handle_t builder, spidir_value_t dest, spidir_value_t value, RuntimeTypeInfo dest_type, RuntimeTypeInfo src_type) {
    // store something that is a struct
    if (jit_is_struct_like(dest_type)) {
        // attempt to convert the interface in-place, if there is no conversion
        // to be done perform the normal memcpy
        if (
            !jit_is_interface(dest_type) ||
            !jit_convert_interface(builder, dest, value, dest_type, src_type)
        ) {
            jit_emit_memcpy(builder, dest, value, dest_type);
        }

        // release the struct slot for further use
        jit_release_struct_slot(value);

    } else {
        if (jit_is_interface(src_type)) {
            // interface -> object
            // just need to load the instance field
            value = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, value);
        }

        // store something that is not a struct
        spidir_builder_build_store(
            builder,
            get_spidir_mem_size(dest_type),
            value,
            dest);
    }
}

static spidir_value_t jit_emit_load(spidir_builder_handle_t builder, spidir_value_t src, RuntimeTypeInfo src_type, RuntimeTypeInfo dest_type) {
    // TODO: we will need to add code to convert structs in here as well
    ASSERT(tdn_get_intermediate_type(src_type) == tdn_get_intermediate_type(dest_type));

    // store something that is a struct
    if (jit_is_struct_like(src_type)) {
        spidir_value_t new_struct = jit_get_struct_slot(builder, src_type);
        jit_emit_memcpy(builder, new_struct, src, src_type);
        return new_struct;

    } else {
        // store something that is not a struct
        // zero extend by default
        spidir_value_t value = spidir_builder_build_load(
            builder,
            get_spidir_mem_size(src_type),
            get_spidir_type(src_type),
            src);

        // sign extend if required
        if (src_type == tSByte || src_type == tInt16) {
            value = spidir_builder_build_sfill(builder, src_type->StackSize * 8, value);
        }

        return value;
    }

}

static void jit_emit_array_length_check(spidir_builder_handle_t builder, spidir_value_t array, spidir_value_t index) {
    // load the length
    spidir_value_t length_ptr = spidir_builder_build_ptroff(builder,
        array,
        spidir_builder_build_iconst(builder,
            SPIDIR_TYPE_I64, offsetof(struct Array, Length))
    );

    spidir_value_t length = spidir_builder_build_load(builder,
        SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64,
        length_ptr
    );

    // compare it to the element index
    spidir_value_t in_range_result = spidir_builder_build_icmp(builder,
        SPIDIR_ICMP_ULT, SPIDIR_TYPE_I32,
        index, length);

    // now emit the brcond on the check
    spidir_block_t in_range = spidir_builder_create_block(builder);
    spidir_block_t out_of_range = spidir_builder_create_block(builder);
    spidir_builder_build_brcond(builder, in_range_result, in_range, out_of_range);

    // go to the invalid path
    spidir_builder_set_block(builder, out_of_range);

    // throw IndexOutOfRangeException
    spidir_builder_build_call(
        builder,
        m_jit_throw_index_out_of_range_exception,
        0, NULL
    );
    spidir_builder_build_unreachable(builder);

    // go back to the valid path
    spidir_builder_set_block(builder, in_range);
}

static spidir_value_t jit_emit_array_offset(spidir_builder_handle_t builder, spidir_value_t array, spidir_value_t index, RuntimeTypeInfo element_type) {
    // get the offset from the first element
    spidir_value_t offset = spidir_builder_build_imul(builder,
        index,
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64,
            element_type->StackSize)
    );

    // add the offset to the first element
    offset = spidir_builder_build_iadd(
        builder,
        offset,
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64,
            ALIGN_UP(sizeof(struct Array), element_type->StackAlignment))
    );

    // and now add it to the array pointer
    return spidir_builder_build_ptroff(builder, array, offset);
}

static spidir_value_t jit_emit_type_check(spidir_builder_handle_t builder, spidir_value_t obj, bool obj_is_interface, RuntimeTypeInfo target) {
    spidir_value_t vtable = SPIDIR_VALUE_INVALID;

    // if the object is actually an interface then we are going
    // to load the object first
    if (obj_is_interface) {
        obj = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, obj);
    }

    // load the vtable pointer from the object
    vtable = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64, obj);
    vtable = spidir_builder_build_inttoptr(builder, vtable);

    // and now check
    if (jit_is_interface(target)) {
        // checking against an interface

        // load the interface product
        spidir_value_t product = spidir_builder_build_load(
            builder,
            SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_I64,
            spidir_builder_build_ptroff(
                builder,
                vtable,
                spidir_builder_build_iconst(
                    builder,
                    SPIDIR_TYPE_I64,
                    offsetof(ObjectVTable, InterfaceProduct)
                )
            )
        );

        // and now check that the prime is dividable by the interface product, if it is (and the reminder
        // is zero) then we know that the type implements the interface, otherwise it does not implement it
        //  (obj->product % target->prime) == 0
        return spidir_builder_build_icmp(
            builder,
            SPIDIR_ICMP_EQ,
            SPIDIR_TYPE_I32,
            spidir_builder_build_urem(
                builder,
                product,
                spidir_builder_build_iconst(
                    builder,
                    SPIDIR_TYPE_I64,
                    target->InterfacePrime
                )
            ),
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 0)
        );
    } else if (tdn_type_is_valuetype(target)) {
        // checking against a boxed value type, just compare the vtables
        // themselves
        return spidir_builder_build_icmp(
            builder,
            SPIDIR_ICMP_EQ,
            SPIDIR_TYPE_I32,
            vtable,
            spidir_builder_build_iconst(
                builder,
                SPIDIR_TYPE_PTR,
                (uintptr_t)target->JitVTable
            )
        );

    } else {
        // checking against a normal class

        // load the type hierarchy
        spidir_value_t hierarchy = spidir_builder_build_load(
            builder,
            SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_I64,
            spidir_builder_build_ptroff(
                builder,
                vtable,
                spidir_builder_build_iconst(
                    builder,
                    SPIDIR_TYPE_I64,
                    offsetof(ObjectVTable, TypeHierarchy)
                )
            )
        );

        // the expected hierarchy
        spidir_value_t expected_hierarchy = spidir_builder_build_iconst(
            builder,
            SPIDIR_TYPE_I64,
            target->JitVTable->TypeHierarchy
        );

        // build the mask based on the target type
        spidir_value_t type_mask = spidir_builder_build_iconst(
            builder,
            SPIDIR_TYPE_I64,
            (1ull << target->TypeMaskLength) - 1ull
        );

        // and now check that the bits at the start of the hierarchy are the same
        // as the target type
        //  (obj->type_hierarchy & target->type_mask) == target->type_hierarchy
        return spidir_builder_build_icmp(
            builder,
            SPIDIR_ICMP_EQ,
            SPIDIR_TYPE_I32,
            spidir_builder_build_and(
                builder,
                hierarchy,
                type_mask
            ),
            expected_hierarchy
        );
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Thunk generation
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static void jit_emit_static_delegate_thunk(spidir_builder_handle_t builder, void* _ctx) {
    jit_method_t* jmethod = _ctx;

    // setup the call
    spidir_block_t entry = spidir_builder_create_block(builder);
    spidir_builder_set_block(builder, entry);
    spidir_builder_set_entry_block(builder, entry);

    // load all the arguments
    spidir_value_t* args = NULL;
    for (int i = 0; i < jmethod->method->Parameters->Length; i++) {
        arrpush(args, spidir_builder_build_param_ref(builder, i + 1));
    }

    // perform the indirect call
    spidir_value_t result = spidir_builder_build_call(
        builder,
        jmethod->function,
        arrlen(args), args
    );

    // and return it
    spidir_builder_build_return(builder, result);

    arrfree(args);
}

static tdn_err_t jit_generate_static_delegate_thunk(spidir_module_handle_t module, jit_method_t* method) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_type_t* args = NULL;

    // build the name
    string_builder_t builder = {};
    string_builder_push_method_signature(&builder, method->method, true);
    string_builder_push_cstr(&builder, " [static-delegate-thunk]");
    const char* name = string_builder_build(&builder);

    // build the arg types, insert a dummy ptr to the first argument
    // to simulate the thiscall
    args = jit_get_spidir_arg_types(method->method);
    arrins(args, 0, SPIDIR_TYPE_PTR);

    // create the function
    method->thunk = spidir_module_create_function(
        module,
        name,
        jit_get_spidir_ret_type(method->method),
        arrlen(args), args
    );
    method->has_thunk = true;

    // build the function
    spidir_module_build_function(
        module,
        method->thunk,
        jit_emit_static_delegate_thunk,
        method
    );

    // register the thunk
    jit_method_register_thunk(method);

cleanup:
    string_builder_free(&builder);
    arrfree(args);

    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Emit basic block
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static tdn_err_t jit_emit_basic_block(spidir_builder_handle_t builder, jit_method_t* jmethod, jit_basic_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = jmethod->method;
    RuntimeMethodBody body = method->MethodBody;
    jit_stack_value_t* stack = NULL;
    RuntimeTypeInfo this_type = NULL;

    spidir_value_t* args = NULL;
    spidir_value_type_t* args_type = NULL;

    // figure the this type if this is a non-static method
    if (!method->Attributes.Static) {
        this_type = method->DeclaringType;
        if (tdn_type_is_valuetype(this_type)) {
            CHECK_AND_RETHROW(tdn_get_byref_type(this_type, &this_type));
        }
    }

    // copy the initial stack
    arrsetlen(stack, arrlen(block->stack));
    memcpy(stack, block->stack, arrlen(block->stack) * sizeof(*stack));

    // move to the block we are emitting
    spidir_builder_set_block(builder, block->block);

#ifdef JIT_VERBOSE_EMIT
    int indent = 0;
#endif

    block->state = JIT_BLOCK_FINISHED;

    RuntimeTypeInfo constrained_type = NULL;

    // get the pc
    tdn_il_inst_t inst = { .control_flow = TDN_IL_CF_FIRST };
    uint32_t pc = block->start;
    while (pc < block->end) {
        // get the instruction
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));

#ifdef JIT_VERBOSE_EMIT
        indent = tdn_disasm_print_start(body, pc, inst, indent);
#endif

        tdn_normalize_inst(&inst);
        uint32_t current_pc = pc;
        pc += inst.length;

        switch (inst.opcode) {

            case CEE_VOLATILE: {
                // nothing to do for now, eventually pass this to spidir for
                // the following memory access
            } break;

            case CEE_CONSTRAINED: {
                constrained_type = inst.operand.type;
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arguments
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STARG: {
                int index = inst.operand.variable;
                RuntimeTypeInfo arg_type = GET_ARG_TYPE(index);
                jit_stack_value_t value = EVAL_STACK_POP();

                // make sure the argument is spilled, this is easier than tracking
                // the value type across segments (and then we might as well do it
                // for locals as well)
                CHECK(jmethod->args[index].spill_required);
                spidir_value_t dest = jmethod->args[index].value;
                jit_emit_store(builder, dest, value.value, arg_type, value.type);
            }  break;

            case CEE_LDARG: {
                int index = inst.operand.variable;
                RuntimeTypeInfo arg_type = GET_ARG_TYPE(index);
                arg_type = tdn_get_intermediate_type(arg_type);

                spidir_value_t value = jmethod->args[index].value;

                if (jit_is_struct_like(arg_type)) {
                    // struct, need to copy it
                    spidir_value_t new_struct = jit_get_struct_slot(builder, arg_type);
                    jit_emit_memcpy(builder, new_struct, value, arg_type);
                    value = new_struct;

                } else if (jmethod->args[index].spill_required) {
                    // spilled, need to load it
                    value = spidir_builder_build_load(builder,
                        get_spidir_mem_size(arg_type),
                        get_spidir_type(arg_type),
                        value);

                }

                EVAL_STACK_PUSH(arg_type, value);
            }  break;

            case CEE_LDARGA: {
                int index = inst.operand.variable;
                RuntimeTypeInfo arg_type = GET_ARG_TYPE(index);

                arg_type = tdn_get_verification_type(arg_type);
                CHECK_AND_RETHROW(tdn_get_byref_type(arg_type, &arg_type));
                EVAL_STACK_PUSH(arg_type, jmethod->args[index].value);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Locals
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STLOC: {
                int index = inst.operand.variable;
                RuntimeLocalVariableInfo local = body->LocalVariables->Elements[index];

                // verify the type
                jit_stack_value_t value = EVAL_STACK_POP();
                jit_emit_store(builder, jmethod->locals[index].value, value.value, local->LocalType, value.type);
            } break;

            case CEE_LDLOC: {
                int index = inst.operand.variable;
                RuntimeLocalVariableInfo local = body->LocalVariables->Elements[index];
                spidir_value_t value = jit_emit_load(builder, jmethod->locals[index].value, local->LocalType, local->LocalType);
                RuntimeTypeInfo type = tdn_get_intermediate_type(local->LocalType);
                EVAL_STACK_PUSH(type, value);
            } break;

            case CEE_LDLOCA: {
                int index = inst.operand.variable;
                RuntimeLocalVariableInfo local = body->LocalVariables->Elements[index];

                RuntimeTypeInfo type = tdn_get_verification_type(local->LocalType);
                CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));

                EVAL_STACK_PUSH(type, jmethod->locals[index].value);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Fields
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_STFLD: {
                RuntimeFieldInfo field = inst.operand.field;
                jit_stack_value_t value = EVAL_STACK_POP();
                jit_stack_value_t obj = EVAL_STACK_POP();

                // get the pointer to the field
                spidir_value_t field_ptr;
                if (!field->Attributes.Static) {
                    field_ptr = spidir_builder_build_ptroff(builder, obj.value,
                    spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, field->FieldOffset));
                } else {
                    CHECK_AND_RETHROW(jit_init_static_field(field));
                    field_ptr = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)field->JitFieldPtr);
                }

                // and now emit the store
                jit_emit_store(builder, field_ptr, value.value, field->FieldType, value.type);
            } break;

            case CEE_LDFLD: {
                RuntimeFieldInfo field = inst.operand.field;
                jit_stack_value_t obj = EVAL_STACK_POP();

                // get the pointer to the field
                spidir_value_t field_ptr;
                if (!field->Attributes.Static) {
                    field_ptr = spidir_builder_build_ptroff(builder, obj.value,
                    spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, field->FieldOffset));
                } else {
                    CHECK_AND_RETHROW(jit_init_static_field(field));
                    field_ptr = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)field->JitFieldPtr);
                }

                // now perform the load
                spidir_value_t value = jit_emit_load(builder, field_ptr, field->FieldType, field->FieldType);

                // and push it
                RuntimeTypeInfo type = tdn_get_intermediate_type(field->FieldType);
                EVAL_STACK_PUSH(type, value);
            } break;

            case CEE_LDFLDA: {
                RuntimeFieldInfo field = inst.operand.field;
                jit_stack_value_t obj = EVAL_STACK_POP();

                // get the pointer to the field
                spidir_value_t field_ptr;
                if (!field->Attributes.Static) {
                    field_ptr = spidir_builder_build_ptroff(builder, obj.value,
                    spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, field->FieldOffset));
                } else {
                    CHECK_AND_RETHROW(jit_init_static_field(field));
                    field_ptr = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)field->JitFieldPtr);
                }

                RuntimeTypeInfo type = tdn_get_verification_type(field->FieldType);
                CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));
                EVAL_STACK_PUSH(type, field_ptr);
            } break;

            case CEE_STSFLD: {
                jit_stack_value_t value = EVAL_STACK_POP();

                RuntimeFieldInfo field = inst.operand.field;
                CHECK_AND_RETHROW(jit_init_static_field(field));
                spidir_value_t field_ptr = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)field->JitFieldPtr);

                jit_emit_store(builder, field_ptr, value.value, field->FieldType, value.type);
            } break;

            case CEE_LDSFLD: {
                // get the pointer to the field
                RuntimeFieldInfo field = inst.operand.field;
                CHECK_AND_RETHROW(jit_init_static_field(field));
                spidir_value_t field_ptr = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)field->JitFieldPtr);

                // now perform the load
                spidir_value_t value = jit_emit_load(builder, field_ptr, field->FieldType, field->FieldType);

                RuntimeTypeInfo type = tdn_get_intermediate_type(inst.operand.field->FieldType);
                EVAL_STACK_PUSH(type, value);
            } break;

            case CEE_LDSFLDA: {
                // get the pointer to the field
                RuntimeFieldInfo field = inst.operand.field;
                CHECK_AND_RETHROW(jit_init_static_field(field));
                spidir_value_t field_ptr = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)field->JitFieldPtr);

                RuntimeTypeInfo type = tdn_get_verification_type(inst.operand.field->FieldType);
                CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));
                EVAL_STACK_PUSH(type, field_ptr);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Stack manipulation
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDC_I4: {
                spidir_value_t value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, inst.operand.uint32);
                EVAL_STACK_PUSH(tInt32, value);
            } break;

            case CEE_LDC_I8: {
                spidir_value_t value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst.operand.int64);
                EVAL_STACK_PUSH(tInt64, value);
            } break;

            case CEE_LDNULL: {
                spidir_value_t value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0);
                EVAL_STACK_PUSH(tObject, value);
            } break;

            case CEE_LDSTR: {
                // pin the string so it will never get GCed
                tdn_host_gc_pin_object(inst.operand.string);

                spidir_value_t value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)inst.operand.string);
                EVAL_STACK_PUSH(tString, value);
            } break;

            case CEE_POP: {
                jit_stack_value_t value = EVAL_STACK_POP();
                jit_release_struct_slot(value.value);
            } break;

            case CEE_DUP: {
                jit_stack_value_t value = EVAL_STACK_POP();
                EVAL_STACK_PUSH(value.type, value.value, .attrs = value.attrs);

                if (jit_is_struct_like(value.type)) {
                    // if this is a struct like we need to actually create
                    // a copy of it, since otherwise it might get modified
                    spidir_value_t copy = jit_get_struct_slot(builder, value.type);
                    jit_emit_memcpy(builder, copy, value.value, value.type);
                    EVAL_STACK_PUSH(value.type, copy, .attrs = value.attrs);
                } else {
                    EVAL_STACK_PUSH(value.type, value.value, .attrs = value.attrs);
                }
            } break;

            case CEE_LDFTN: {
                // get the jit method
                jit_method_t* target_method = NULL;
                CHECK_AND_RETHROW(jit_get_or_create_method(inst.operand.method, &target_method));

                // we need to create a stub for static functions
                spidir_value_t addr = SPIDIR_VALUE_INVALID;
                if (inst.operand.method->Attributes.Static) {
                    CHECK_AND_RETHROW(jit_generate_static_delegate_thunk(spidir_builder_get_module(builder), target_method));
                    addr = spidir_builder_build_funcaddr(builder, target_method->thunk);

                } else {
                    addr = spidir_builder_build_funcaddr(builder, target_method->function);
                }

                // load the address and push it
                // TODO: for now we need the ptrtoint because of how the method ctor is defined
                //       as intptr, in the future we might want to replace this to make the jit
                //       be able to inline things properly
                EVAL_STACK_PUSH(tIntPtr, spidir_builder_build_ptrtoint(builder, addr));
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Method calling
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_NEWOBJ:
            case CEE_CALL:
            case CEE_CALLVIRT: {
                RuntimeMethodBase target = inst.operand.method;

                // get the target this type
                RuntimeTypeInfo target_this_type = NULL;
                if (!target->Attributes.Static) {
                    target_this_type = target->DeclaringType;
                    if (tdn_type_is_valuetype(target_this_type)) {
                        CHECK_AND_RETHROW(tdn_get_byref_type(target_this_type, &target_this_type));
                    }
                }

                // add all the args
                for (int i = target->Parameters->Length - 1; i >= 0; i--) {
                    RuntimeTypeInfo arg_type = target->Parameters->Elements[i]->ParameterType;
                    jit_stack_value_t arg = EVAL_STACK_POP();

                    // if this is an interface and the type of the arg is not the exact same
                    // as the type of the param then perform an interface convert on a new
                    // slot
                    spidir_value_t value = arg.value;
                    if (jit_is_interface(arg_type) && arg_type != arg.type) {
                        spidir_value_t new_slot;
                        if (!jit_is_interface(arg.type)) {
                            // not even an interface, will need to
                            // allocate a new slot for this
                            new_slot = jit_get_struct_slot(builder, arg_type);
                        } else {
                            // this is already an interface, because of the eval-stack rules
                            // this is already a new copy that we can do whatever we want with
                            new_slot = value;
                        }

                        // perform the interface convertion
                        CHECK(jit_convert_interface(builder, new_slot, value, arg_type, arg.type));
                        // TODO: release the struct after the call

                        // now use the new slot as the valeu
                        value = new_slot;
                    }

                    arrins(args, 0, value);
                }

                RuntimeTypeInfo obj_type = NULL;
                bool need_explicit_null_check = false;
                if (inst.opcode == CEE_NEWOBJ) {
                    // perform the allocation, in the case of a struct value
                    // just emit a stack slot and zero it
                    spidir_value_t obj;
                    if (jit_is_delegate(target_this_type)) {
                        obj = jit_get_struct_slot(builder, tMulticastDelegate);
                        jit_emit_bzero(builder, obj, tMulticastDelegate);

                    } else if (jit_is_struct(target->DeclaringType)) {
                        obj = jit_get_struct_slot(builder, target->DeclaringType);
                        jit_emit_bzero(builder, obj, target->DeclaringType);

                    } else if (target->DeclaringType == tString) {
                        // String class is special because when allocating it we need to figure
                        // the correct amount of memory required
                        spidir_value_t element_count = SPIDIR_VALUE_INVALID;
                        if (target->Parameters->Length == 1) {
                            if (target->Parameters->Elements[0]->ParameterType == tInt32) {
                                // takes in a length
                                element_count = args[0];
                            } else {
                                // takes in a ReadOnlySpan
                                // TODO: verify that or something
                                element_count = args[0];

                                // add the offset to the length, its after the ref
                                element_count = spidir_builder_build_ptroff(
                                    builder,
                                    element_count,
                                    spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, sizeof(void*))
                                );

                                // and now deref it to get the value
                                element_count = spidir_builder_build_load(builder,
                                    SPIDIR_MEM_SIZE_4,
                                    SPIDIR_TYPE_I32,
                                    element_count
                                );
                            }

                        } else {
                            CHECK_FAIL();
                        }

                        // and now allocate the string by using newstr
                        obj = spidir_builder_build_call(builder,
                            m_jit_gc_newstr,
                            1,
                            (spidir_value_t[]){
                                element_count
                            }
                        );
                    } else {
                        // call the gc to create the new object
                        obj = spidir_builder_build_call(builder,
                            m_jit_gc_new,
                            1,
                            (spidir_value_t[]){
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)target->DeclaringType)
                            }
                        );
                    }
                    arrins(args, 0, obj);

                } else if (target_this_type != NULL) {
                    jit_stack_value_t obj = EVAL_STACK_POP();
                    arrins(args, 0, obj.value);
                    obj_type = obj.type;

                    if (inst.opcode == CEE_CALLVIRT) {
                        if (!target->Attributes.Virtual) {
                            // method not virtual, just need the null check
                            inst.opcode = CEE_CALL;
                            need_explicit_null_check = true;

                        } else if (obj.attrs.known_type != NULL || constrained_type != NULL) {
                            RuntimeTypeInfo known_type = obj.attrs.known_type;
                            if (known_type == NULL) {
                                known_type = constrained_type;
                            }

                            // we have a known type, we can de-virt to it
                            target = (RuntimeMethodBase)known_type->VTable->Elements[target->VTableOffset];
                            inst.opcode = CEE_CALL;
                            need_explicit_null_check = true;

                            if (
                                tdn_type_is_referencetype(obj.type) &&
                                tdn_type_is_valuetype(known_type) &&
                                tdn_type_is_valuetype(target->DeclaringType)
                            ) {
                                // we have a value type, de-virtualize
                                args[0] = spidir_builder_build_ptroff(builder, args[0],
                                    spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, jit_get_boxed_value_offset(obj.attrs.known_type)));

                            } else if (jit_is_interface(obj.type)) {
                                // we have an interface, get the raw object instance
                                args[0] = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, args[0]);

                            }

                        } else if (target->Attributes.Final || target->DeclaringType->Attributes.Sealed) {
                            // the type is sealed / the method is final, we can de-virt it
                            target = (RuntimeMethodBase)target->DeclaringType->VTable->Elements[target->VTableOffset];
                            inst.opcode = CEE_CALL;
                            if (!jit_is_delegate(target->DeclaringType)) {
                                need_explicit_null_check = true;
                            }

                        } else if (jit_is_struct(target->DeclaringType)) {
                            // the type is a struct, attempt to de-virt it
                            RuntimeMethodBase possible_target = (RuntimeMethodBase)target->DeclaringType->VTable->Elements[target->VTableOffset];
                            CHECK(possible_target->DeclaringType == target->DeclaringType);
                            target = possible_target;
                            inst.opcode = CEE_CALL;
                            need_explicit_null_check = true;

                        }
                    }

                } else {
                    // special case for static-virtual, we need to search for the real implementation
                    // in the method impl metadata, we don't actually parse it outside, so we will
                    // just parse it inside
                    if (constrained_type != NULL) {
                        RuntimeAssembly assembly = constrained_type->Module->Assembly;
                        token_t token = {};
                        for (int i = 0; i < assembly->Metadata->method_impls_count; i++) {
                            metadata_method_impl_t* impl = &assembly->Metadata->method_impls[i];
                            if (impl->class.token != constrained_type->MetadataToken) {
                                continue;
                            }

                            RuntimeMethodBase decl;
                            CHECK_AND_RETHROW(tdn_assembly_lookup_method(
                                assembly,
                                impl->method_declaration.token,
                                target->DeclaringType->GenericArguments,
                                target->GenericArguments,
                                &decl
                            ));

                            if (target == decl) {
                                token = impl->method_body;
                                break;
                            }
                        }
                        CHECK(token.index != 0);

                        // found the token, now go over the functions in our
                        // constrained class and search for the correct method
                        // TODO: check ctors as well?
                        target = NULL;
                        for (int i = 0; i < constrained_type->DeclaredMethods->Length; i++) {
                            RuntimeMethodInfo maybe_target = constrained_type->DeclaredMethods->Elements[i];
                            if (maybe_target->MetadataToken == token.token) {
                                target = (RuntimeMethodBase)maybe_target;
                                break;
                            }
                        }
                        CHECK(target != NULL);
                    }
                }

                // handle the implicit return value, it is going to be
                // the last parameter
                spidir_value_t ret_val_ptr = SPIDIR_VALUE_INVALID;
                ParameterInfo ret_info = target->ReturnParameter;
                RuntimeTypeInfo ret_type = tdn_get_intermediate_type(ret_info->ParameterType);
                if (ret_type != tVoid && jit_is_struct_like(ret_type)) {
                    ret_val_ptr = jit_get_struct_slot(builder, ret_type);
                    arrpush(args, ret_val_ptr);
                }

                // now emit the call, if its a callvirt we need
                // to perform an indirect call
                spidir_value_t ret_value;
                if (inst.opcode == CEE_CALLVIRT) {
                    // resolve the call location
                    spidir_value_t func_addr = SPIDIR_VALUE_INVALID;
                    size_t base_offset = sizeof(void*) * target->VTableOffset;
                    if (target_this_type->Attributes.Interface) {
                        // load the vtable pointer
                        if (jit_is_interface(obj_type)) {
                            // from an interface fat pointer
                            func_addr = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable));
                            func_addr = spidir_builder_build_ptroff(builder, args[0], func_addr);
                            func_addr = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, func_addr);
                        } else {
                            // load the vtable pointer, the load will automatically zero extend the pointer
                            func_addr = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64, args[0]);
                            func_addr = spidir_builder_build_inttoptr(builder, func_addr);

                            // get the offset into the actual interface vtable
                            size_t interface_offset = jit_get_interface_offset(obj_type, target_this_type);
                            ASSERT(interface_offset != -1);
                            base_offset += offsetof(ObjectVTable, Functions) + interface_offset;
                        }

                        // lastly replace the this pointer with the real one
                        args[0] = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, args[0]);
                        ASSERT(offsetof(Interface, Instance) == 0);

                    } else {
                        if (jit_is_interface(obj_type)) {
                            // we are calling with an interface, take the actual reference from it
                            args[0] = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, args[0]);
                        }

                        // load the vtable pointer, the load will automatically zero extend the pointer
                        func_addr = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64, args[0]);
                        func_addr = spidir_builder_build_inttoptr(builder, func_addr);

                        // because in here we have the vtable header as well
                        // offset it right now
                        base_offset += offsetof(ObjectVTable, Functions);
                    }

                    // offset the vtable by the required offset
                    if (base_offset != 0) {
                        func_addr = spidir_builder_build_ptroff(builder, func_addr,
                            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, base_offset));
                    }

                    // now load the pointer of the function itself
                    func_addr = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, func_addr);

                    // now emit the call
                    args_type = jit_get_spidir_arg_types(target);
                    ret_value = spidir_builder_build_callind(
                        builder,
                        jit_get_spidir_ret_type(target),
                        arrlen(args),
                        args_type,
                        func_addr,
                        args
                    );
                } else {
                    // make sure to verify the target if not already verified, this could
                    // happen if we de-virtualized something in here
                    CHECK_AND_RETHROW(jit_verify_method(target));

                    // get the function
                    // TODO: replace with a version that doesn't create since we
                    //       already should handle it in the verifier
                    jit_method_t* target_method = NULL;
                    CHECK_AND_RETHROW(jit_get_or_create_method(target, &target_method));

                    // perform the null check on this if required
                    if (need_explicit_null_check) {
                        // just perform a deref, this will fail if we have a null pointer in there,
                        // use a size of 1 in case we have a struct ref in here
                        spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_1, SPIDIR_TYPE_I32, args[0]);
                    }

                    // and now we can perform the direct call
                    ret_value = spidir_builder_build_call(
                        builder,
                        target_method->function,
                        arrlen(args),
                        args
                    );
                }

                // finally we need to handle the return value
                if (inst.opcode == CEE_NEWOBJ) {
                    // we can remember the known type to be the one we just created, this will
                    // help us eliminate some indirect calls when we know the exact type that
                    // was created
                    EVAL_STACK_PUSH(method->DeclaringType, args[0], .attrs = { .known_type = target->DeclaringType });

                } else if (ret_type != tVoid) {
                    // push the pointer we created for this
                    if (jit_is_struct_like(ret_type)) {
                        ret_value = ret_val_ptr;
                    }

                    EVAL_STACK_PUSH(ret_type, ret_value);
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Array handlig
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_NEWARR: {
                jit_stack_value_t num_elems = EVAL_STACK_POP();

                // extend to 64bit if a 32bit integer,
                // we need to sign extend it
                spidir_value_t element_count = num_elems.value;
                if (num_elems.type == tInt32) {
                    element_count = spidir_builder_build_iext(builder, element_count);
                    element_count = spidir_builder_build_sfill(builder, 32, element_count);
                }

                RuntimeTypeInfo array_type;
                CHECK_AND_RETHROW(tdn_get_array_type(inst.operand.type, &array_type));

                // allocate it
                spidir_value_t array = spidir_builder_build_call(
                    builder,
                    m_jit_gc_newarr,
                    2,
                    (spidir_value_t[]){
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)array_type),
                        element_count
                    }
                );

                // set the length, the reason we do it in here
                // is to make sure that the jit sees the value
                spidir_value_t length_ptr = spidir_builder_build_ptroff(builder,
                    array,
                    spidir_builder_build_iconst(builder,
                        SPIDIR_TYPE_I64, offsetof(struct Array, Length))
                );
                spidir_builder_build_store(builder,
                    SPIDIR_MEM_SIZE_4,
                    element_count,
                    length_ptr);

                EVAL_STACK_PUSH(array_type, array);
            } break;

            case CEE_LDLEN: {
                jit_stack_value_t array = EVAL_STACK_POP();

                // load the length, a 4 byye -> 64bit zero extends by default
                spidir_value_t length_ptr = spidir_builder_build_ptroff(builder,
                    array.value,
                    spidir_builder_build_iconst(builder,
                        SPIDIR_TYPE_I64, offsetof(struct Array, Length))
                );

                spidir_value_t length = spidir_builder_build_load(builder,
                    SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I64,
                    length_ptr
                );

                EVAL_STACK_PUSH(tIntPtr, length);
            } break;

            case CEE_STELEM:
            case CEE_STELEM_REF: {
                jit_stack_value_t value = EVAL_STACK_POP();
                jit_stack_value_t index = EVAL_STACK_POP();
                jit_stack_value_t array = EVAL_STACK_POP();

                // sign extend the index
                spidir_value_t index_val = index.value;
                if (index.type == tInt32) {
                    index_val = spidir_builder_build_iext(builder, index_val);
                    index_val = spidir_builder_build_sfill(builder, 32, index_val);
                }

                // emit the length check
                jit_emit_array_length_check(builder, array.value, index_val);

                // get the pointer of the element
                spidir_value_t offset = jit_emit_array_offset(builder, array.value, index_val, array.type->ElementType);

                // and now store the value
                jit_emit_store(builder, offset, value.value, array.type->ElementType, value.type);
            } break;

            case CEE_LDELEM:
            case CEE_LDELEM_REF: {
                jit_stack_value_t index = EVAL_STACK_POP();
                jit_stack_value_t array = EVAL_STACK_POP();

                // sign extend the index
                spidir_value_t index_val = index.value;
                if (index.type == tInt32) {
                    index_val = spidir_builder_build_iext(builder, index_val);
                    index_val = spidir_builder_build_sfill(builder, 32, index_val);
                }

                // emit the length check
                jit_emit_array_length_check(builder, array.value, index_val);

                // get the pointer of the element
                spidir_value_t offset = jit_emit_array_offset(builder, array.value, index_val, array.type->ElementType);

                // now perform the load itself
                // TODO: should we use the inst.operand.type here?
                spidir_value_t value = jit_emit_load(builder, offset, array.type->ElementType, array.type->ElementType);

                EVAL_STACK_PUSH(tdn_get_intermediate_type(array.type->ElementType), value);
            } break;

            case CEE_LDELEMA: {
                jit_stack_value_t index = EVAL_STACK_POP();
                jit_stack_value_t array = EVAL_STACK_POP();

                // sign extend the index
                spidir_value_t index_val = index.value;
                if (index.type == tInt32) {
                    index_val = spidir_builder_build_iext(builder, index_val);
                    index_val = spidir_builder_build_sfill(builder, 32, index_val);
                }

                // emit the length check
                jit_emit_array_length_check(builder, array.value, index_val);

                // get the pointer of the element
                spidir_value_t offset = jit_emit_array_offset(builder, array.value, index_val, array.type->ElementType);

                // and push the tracked address
                RuntimeTypeInfo ref_type = tdn_get_verification_type(array.type->ElementType);
                CHECK_AND_RETHROW(tdn_get_byref_type(ref_type, &ref_type));
                EVAL_STACK_PUSH(ref_type, offset);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Indirect access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDIND_I1:
            case CEE_LDIND_I2:
            case CEE_LDIND_I4:
            case CEE_LDIND_I8:
            case CEE_LDIND_U1:
            case CEE_LDIND_U2:
            case CEE_LDIND_U4:
            case CEE_LDIND_I:
            case CEE_LDIND_REF:
            case CEE_LDOBJ: {
                jit_stack_value_t addr = EVAL_STACK_POP();

                // get the type we will have on the stack eventually
                RuntimeTypeInfo type = inst.operand.type;
                if (type == NULL) {
                    type = addr.type->ElementType;
                    type = tdn_get_verification_type(type);
                } else {
                    type = tdn_get_intermediate_type(type);
                }

                // perform the load
                spidir_value_t value = jit_emit_load(builder, addr.value, addr.type->ElementType, type);
                EVAL_STACK_PUSH(type, value);
            } break;

            case CEE_STIND_I1:
            case CEE_STIND_I2:
            case CEE_STIND_I4:
            case CEE_STIND_I8:
            case CEE_STIND_I:
            case CEE_STIND_REF:
            case CEE_STOBJ: {
                jit_stack_value_t val = EVAL_STACK_POP();
                jit_stack_value_t addr = EVAL_STACK_POP();
                jit_emit_store(builder, addr.value, val.value, addr.type->ElementType, val.type);
            } break;

            case CEE_INITOBJ: {
                jit_stack_value_t dest = EVAL_STACK_POP();
                jit_emit_bzero(builder, dest.value, dest.type);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Math
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_ADD:
            case CEE_SUB:
            case CEE_MUL:
            case CEE_DIV:
            case CEE_REM:
            case CEE_DIV_UN:
            case CEE_REM_UN:
            case CEE_AND:
            case CEE_XOR:
            case CEE_OR:
            case CEE_ADD_OVF:
            case CEE_ADD_OVF_UN:
            case CEE_SUB_OVF:
            case CEE_SUB_OVF_UN:
            case CEE_MUL_OVF:
            case CEE_MUL_OVF_UN: {
                jit_stack_value_t value2 = EVAL_STACK_POP();
                jit_stack_value_t value1 = EVAL_STACK_POP();
                bool is_unsigned = inst.opcode == CEE_DIV_UN || inst.opcode == CEE_REM_UN ||
                                    inst.opcode == CEE_ADD_OVF_UN || inst.opcode == CEE_SUB_OVF_UN ||
                                    inst.opcode == CEE_MUL_OVF_UN;

                spidir_value_t val1 = value1.value;
                spidir_value_t val2 = value2.value;

                RuntimeTypeInfo result;
                if (value1.type == tInt32) {
                    if (value2.type == tInt32) {
                        result = tInt32;
                    } else {
                        CHECK(value2.type == tIntPtr);
                        result = tIntPtr;

                        // extend to intptr
                        val1 = spidir_builder_build_iext(builder, val1);
                        if (is_unsigned) {
                            val1 = spidir_builder_build_and(builder, val1,
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 0xFFFFFFFF));
                        } else {
                            val1 = spidir_builder_build_sfill(builder, 32, val1);
                        }
                    }

                } else if (value1.type == tIntPtr) {
                    CHECK(value2.type == tInt32 || value2.type == tIntPtr);
                    result = tIntPtr;

                    if (value2.type == tInt32) {
                        // extend to intptr
                        val2 = spidir_builder_build_iext(builder, val2);
                        if (is_unsigned) {
                            val2 = spidir_builder_build_and(builder, val2,
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 0xFFFFFFFF));
                        } else {
                            val2 = spidir_builder_build_sfill(builder, 32, val2);
                        }
                    }

                } else if (value1.type == tInt64) {
                    CHECK(value2.type == tInt64);
                    result = tInt64;

                } else {
                    CHECK_FAIL();
                }

                spidir_value_t val;
                switch (inst.opcode) {
                    case CEE_ADD_OVF:
                    case CEE_ADD_OVF_UN:
                    case CEE_ADD: val = spidir_builder_build_iadd(builder, val1, val2); break;
                    case CEE_SUB_OVF:
                    case CEE_SUB_OVF_UN:
                    case CEE_SUB: val = spidir_builder_build_isub(builder, val1, val2); break;
                    case CEE_MUL_OVF:
                    case CEE_MUL_OVF_UN:
                    case CEE_MUL: val = spidir_builder_build_imul(builder, val1, val2); break;
                    case CEE_DIV: val = spidir_builder_build_sdiv(builder, val1, val2); break;
                    case CEE_REM: val = spidir_builder_build_srem(builder, val1, val2); break;
                    case CEE_DIV_UN: val = spidir_builder_build_udiv(builder, val1, val2); break;
                    case CEE_REM_UN: val = spidir_builder_build_urem(builder, val1, val2); break;
                    case CEE_AND: val = spidir_builder_build_and(builder, val1, val2); break;
                    case CEE_XOR: val = spidir_builder_build_xor(builder, val1, val2); break;
                    case CEE_OR: val = spidir_builder_build_or(builder, val1, val2); break;
                    default: CHECK_FAIL();
                }

                EVAL_STACK_PUSH(result, val);
            } break;

            case CEE_SHL:
            case CEE_SHR:
            case CEE_SHR_UN: {
                jit_stack_value_t shift_amount = EVAL_STACK_POP();
                jit_stack_value_t value = EVAL_STACK_POP();

                spidir_value_t result;
                switch (inst.opcode) {
                    case CEE_SHL: result = spidir_builder_build_shl(builder, value.value, shift_amount.value); break;
                    case CEE_SHR: result = spidir_builder_build_ashr(builder, value.value, shift_amount.value); break;
                    case CEE_SHR_UN: result = spidir_builder_build_lshr(builder, value.value, shift_amount.value); break;
                    default: CHECK_FAIL();
                }

                EVAL_STACK_PUSH(value.type, result);
            } break;

            case CEE_NOT:
            case CEE_NEG: {
                jit_stack_value_t value = EVAL_STACK_POP();
                bool is_64bit = value.type == tInt64 || value.type == tIntPtr;

                spidir_value_t result;
                if (inst.opcode == CEE_NOT) {
                    // emulate not by xoring with FFs
                    result = spidir_builder_build_xor(builder, value.value,
                        spidir_builder_build_iconst(builder,
                            is_64bit ? SPIDIR_TYPE_I64 : SPIDIR_TYPE_I32,
                            is_64bit ? UINT64_MAX : UINT32_MAX));
                } else {
                    // emulate neg by doing 0 - value
                    result = spidir_builder_build_isub(builder,
                        spidir_builder_build_iconst(builder,
                            is_64bit ? SPIDIR_TYPE_I64 : SPIDIR_TYPE_I32, 0),
                            value.value);
                }

                EVAL_STACK_PUSH(value.type, result);
            } break;

            case CEE_CEQ:
            case CEE_CGT:
            case CEE_CGT_UN:
            case CEE_CLT:
            case CEE_CLT_UN: {
                jit_stack_value_t value2 = EVAL_STACK_POP();
                jit_stack_value_t value1 = EVAL_STACK_POP();
                bool is_unsigned = inst.opcode == CEE_CGT_UN || inst.opcode == CEE_CLT_UN;

                spidir_value_t val1 = value1.value;
                spidir_value_t val2 = value2.value;

                // check if we need to extend either
                if (value1.type == tIntPtr) {
                    if (value2.type == tInt32) {
                        val2 = spidir_builder_build_iext(builder, val2);
                        if (is_unsigned) {
                            val2 = spidir_builder_build_and(builder, val2,
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, UINT32_MAX));
                        } else {
                            val2 = spidir_builder_build_sfill(builder, 32, val2);
                        }
                    }

                } else if (value1.type == tInt32) {
                    if (value2.type == tIntPtr) {
                        val1 = spidir_builder_build_iext(builder, val1);
                        if (is_unsigned) {
                            val1 = spidir_builder_build_and(builder, val1,
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, UINT32_MAX));
                        } else {
                            val1 = spidir_builder_build_sfill(builder, 32, val1);
                        }
                    }
                }

                // choose the kind, spidir doesn't have greater than variants
                // so we are going to swap whenever there is a need for one
                spidir_icmp_kind_t kind;
                switch (inst.opcode) {
                    case CEE_CEQ: kind = SPIDIR_ICMP_EQ; break;

                    case CEE_CGT: SWAP(val1, val2);
                    case CEE_CLT: kind = SPIDIR_ICMP_SLT; break;

                    case CEE_CGT_UN: SWAP(val1, val2);
                    case CEE_CLT_UN: kind = SPIDIR_ICMP_ULT; break;

                    default: CHECK_FAIL();
                }

                spidir_value_t value = spidir_builder_build_icmp(builder, kind, SPIDIR_TYPE_I32, val1, val2);
                EVAL_STACK_PUSH(tInt32, value);
            } break;

            case CEE_CONV_U:
            case CEE_CONV_I: {
                jit_stack_value_t value = EVAL_STACK_POP();

                spidir_value_t val = value.value;
                if (value.type == tInt32) {
                    val = spidir_builder_build_iext(builder, val);
                    if (inst.opcode == CEE_CONV_U) {
                        val = spidir_builder_build_and(builder, val,
                            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, UINT32_MAX));
                    } else {
                        val = spidir_builder_build_sfill(builder, 32, val);
                    }
                }

                EVAL_STACK_PUSH(tIntPtr, val);
            } break;

            case CEE_CONV_U8:
            case CEE_CONV_I8: {
                jit_stack_value_t value = EVAL_STACK_POP();

                spidir_value_t val = value.value;
                if (value.type == tInt32) {
                    val = spidir_builder_build_iext(builder, val);
                    if (inst.opcode == CEE_CONV_U8) {
                        val = spidir_builder_build_and(builder, val,
                            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, UINT32_MAX));
                    } else {
                        val = spidir_builder_build_sfill(builder, 32, val);
                    }
                }

                EVAL_STACK_PUSH(tInt64, val);
            } break;

            case CEE_CONV_U4:
            case CEE_CONV_I4:
            case CEE_CONV_U2:
            case CEE_CONV_I2:
            case CEE_CONV_I1:
            case CEE_CONV_U1: {
                jit_stack_value_t value = EVAL_STACK_POP();

                spidir_value_t val = value.value;
                if (value.type == tInt64 || value.type == tIntPtr) {
                    // just need to truncate it
                    val = spidir_builder_build_itrunc(builder, val);

                } else if (inst.opcode == CEE_CONV_U2) {
                    val = spidir_builder_build_and(builder, val,
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, UINT16_MAX));

                } else if (inst.opcode == CEE_CONV_U1) {
                    val = spidir_builder_build_and(builder, val,
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, UINT8_MAX));

                } else if (inst.opcode == CEE_CONV_I2) {
                    val = spidir_builder_build_sfill(builder, 16, val);

                } else if (inst.opcode == CEE_CONV_I1) {
                    val = spidir_builder_build_sfill(builder, 8, val);
                }

                EVAL_STACK_PUSH(tInt32, val);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Branching
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_BEQ:
            case CEE_BGE:
            case CEE_BGT:
            case CEE_BLE:
            case CEE_BLT:
            case CEE_BNE_UN:
            case CEE_BGE_UN:
            case CEE_BGT_UN:
            case CEE_BLE_UN:
            case CEE_BLT_UN: {
                jit_stack_value_t value2 = EVAL_STACK_POP();
                jit_stack_value_t value1 = EVAL_STACK_POP();
                bool is_unsigned = inst.opcode == CEE_BNE_UN || inst.opcode == CEE_BGE_UN ||
                                    inst.opcode == CEE_BGT_UN || inst.opcode == CEE_BLE_UN ||
                                    inst.opcode == CEE_BLT_UN;

                // start by emitting the conditional

                spidir_value_t val1 = value1.value;
                spidir_value_t val2 = value2.value;

                // check if we need to extend either
                if (value1.type == tIntPtr) {
                    if (value2.type == tInt32) {
                        val2 = spidir_builder_build_iext(builder, val2);
                        if (is_unsigned) {
                            val2 = spidir_builder_build_and(builder, val2,
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, UINT32_MAX));
                        } else {
                            val2 = spidir_builder_build_sfill(builder, 32, val2);
                        }
                    }

                } else if (value1.type == tInt32) {
                    if (value2.type == tIntPtr) {
                        val1 = spidir_builder_build_iext(builder, val1);
                        if (is_unsigned) {
                            val1 = spidir_builder_build_and(builder, val1,
                                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, UINT32_MAX));
                        } else {
                            val1 = spidir_builder_build_sfill(builder, 32, val1);
                        }
                    }
                }

                // choose the kind, spidir doesn't have greater than variants
                // so we are going to swap whenever there is a need for one
                spidir_icmp_kind_t kind;
                switch (inst.opcode) {
                    case CEE_BEQ: kind = SPIDIR_ICMP_EQ; break;
                    case CEE_BNE_UN: kind = SPIDIR_ICMP_NE; break;

                    case CEE_BGE: SWAP(val1, val2);
                    case CEE_BLE: kind = SPIDIR_ICMP_SLE; break;

                    case CEE_BGT: SWAP(val1, val2);
                    case CEE_BLT: kind = SPIDIR_ICMP_SLT; break;

                    case CEE_BGE_UN: SWAP(val1, val2);
                    case CEE_BLE_UN: kind = SPIDIR_ICMP_ULE; break;

                    case CEE_BGT_UN: SWAP(val1, val2);
                    case CEE_BLT_UN: kind = SPIDIR_ICMP_ULT; break;

                    default: CHECK_FAIL();
                }

                spidir_value_t value = spidir_builder_build_icmp(builder, kind, SPIDIR_TYPE_I32, val1, val2);

                // get the blocks of each option
                spidir_block_t true_block;
                CHECK_AND_RETHROW(emit_merge_basic_block(
                    jmethod, builder,
                    inst.operand.branch_target,
                    stack, &true_block,
                    get_leave_target(block->leave_target_stack)));

                spidir_block_t false_block;
                CHECK_AND_RETHROW(emit_merge_basic_block(
                    jmethod, builder,
                    pc,
                    stack, &false_block,
                    get_leave_target(block->leave_target_stack)));

                // and finally emit the actual brcond
                spidir_builder_build_brcond(builder, value, true_block, false_block);
            } break;

            case CEE_BRTRUE:
            case CEE_BRFALSE: {
                jit_stack_value_t value = EVAL_STACK_POP();

                // get the blocks of each option
                spidir_block_t true_block;
                CHECK_AND_RETHROW(emit_merge_basic_block(
                    jmethod, builder,
                    inst.operand.branch_target,
                    stack, &true_block,
                    get_leave_target(block->leave_target_stack)));

                spidir_block_t false_block;
                CHECK_AND_RETHROW(emit_merge_basic_block(
                    jmethod, builder,
                    pc,
                    stack, &false_block,
                    get_leave_target(block->leave_target_stack)));

                spidir_value_t cond = value.value;

                if (jit_is_delegate(value.type)) {
                    // load the method ptr, since its the thing that can be null
                    // when dealing with a delegate
                    cond = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
                        spidir_builder_build_ptroff(builder, cond,
                            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Delegate, Function))));
                }

                if (tdn_type_is_referencetype(value.type)) {
                    // can't pass ptr to brcond, turn into an int first
                    cond = spidir_builder_build_icmp(builder,
                        SPIDIR_ICMP_NE,
                        SPIDIR_TYPE_I32,
                        cond, spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0));
                }

                // use the same condition but swap args
                if (inst.opcode == CEE_BRFALSE) {
                    SWAP(true_block, false_block);
                }

                // and finally emit the actual brcond
                spidir_builder_build_brcond(builder, cond, true_block, false_block);
            } break;

            case CEE_BR: {
                spidir_block_t dest;
                CHECK_AND_RETHROW(emit_merge_basic_block(
                    jmethod, builder,
                    inst.operand.branch_target,
                    stack, &dest,
                    get_leave_target(block->leave_target_stack)));

                spidir_builder_build_branch(builder, dest);
            } break;

            case CEE_RET: {
                RuntimeTypeInfo type = method->ReturnParameter->ParameterType;

                if (type != tVoid) {
                    jit_stack_value_t ret_val = EVAL_STACK_POP();

                    if (jit_is_struct_like(ret_val.type)) {
                        // get the return pointer from the top of the stack implicitly
                        spidir_value_t ret_ptr = spidir_builder_build_param_ref(builder, arrlen(jmethod->locals));

                        // copy the value to it, in some cases we will perform an interface convertion to match
                        // the actual returned type
                        if (
                            !jit_is_interface(type) ||
                            !jit_convert_interface(builder, ret_ptr, ret_val.value, type, ret_val.type)
                        ) {
                            jit_emit_memcpy(builder, ret_ptr, ret_val.value, ret_val.type);
                        }

                        jit_release_struct_slot(ret_val.value);

                        spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
                    } else {
                        // just return it
                        spidir_builder_build_return(builder, ret_val.value);
                    }
                } else {
                    // nothing to return
                    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
                }

                CHECK(arrlen(stack) == 0);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Class casting
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_BOX: {
                jit_stack_value_t value = EVAL_STACK_POP();

                // TODO: how does box work with nullable

                // if this is areference type then there is nothing to do,
                // just keep it as is
                spidir_value_t res = value.value;
                if (!tdn_type_is_referencetype(inst.operand.type)) {
                    // allocate it
                    res = spidir_builder_build_call(
                        builder,
                        m_jit_gc_new,
                        1,
                        (spidir_value_t[]){
                            spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, (uint64_t)inst.operand.type)
                        }
                    );

                    // get the pointer to the data
                    spidir_value_t value_ptr = spidir_builder_build_ptroff(builder, res,
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, jit_get_boxed_value_offset(inst.operand.type)));

                    // store to it the value itself
                    jit_emit_store(builder, value_ptr, value.value, inst.operand.type, value.type);
                }

                // track it as an object
                EVAL_STACK_PUSH(tObject, res, .attrs = {.known_type = inst.operand.type});
            } break;

            case CEE_UNBOX_ANY: {
                jit_stack_value_t obj = EVAL_STACK_POP();

                // if this is already a reference type we have nothing to do
                spidir_value_t value = obj.value;
                if (!tdn_type_is_referencetype(inst.operand.type)) {
                    // if we don't know the exact types we
                    // need to emit a type check
                    if (obj.attrs.known_type != inst.operand.type) {
                        // the null check will just check the vtables, because we know
                        // that it must be a value type which doesn't have any inheritance

                        // load the vtable of the object
                        spidir_value_t runtime = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_4, SPIDIR_TYPE_I32, value);
                        spidir_value_t expected = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, (uint32_t)(uintptr_t)inst.operand.type->JitVTable);
                        spidir_value_t result = spidir_builder_build_icmp(builder, SPIDIR_ICMP_EQ, SPIDIR_TYPE_I32, runtime, expected);

                        // and emit the brcond
                        spidir_block_t same_type = spidir_builder_create_block(builder);
                        spidir_block_t not_same_type = spidir_builder_create_block(builder);
                        spidir_builder_build_brcond(builder, result, same_type, not_same_type);

                        // start with the invalid path
                        spidir_builder_set_block(builder, not_same_type);
                        spidir_builder_build_call(
                            builder,
                            m_jit_throw_invalid_cast_exception,
                            0, NULL
                        );
                        spidir_builder_build_unreachable(builder);

                        // continue on the same path
                        spidir_builder_set_block(builder, same_type);
                    }

                    // get the pointer to the data
                    spidir_value_t value_ptr = spidir_builder_build_ptroff(builder, obj.value,
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, jit_get_boxed_value_offset(inst.operand.type)));

                    // perform the load assuming its
                    value = jit_emit_load(builder, value_ptr, inst.operand.type, inst.operand.type);
                } else {
                    // TODO: perform castclass
                    CHECK_FAIL();
                }

                // and push it
                EVAL_STACK_PUSH(tdn_get_intermediate_type(inst.operand.type), value);
            } break;

            case CEE_CASTCLASS: {
                jit_stack_value_t obj = EVAL_STACK_POP();

                // create the continuation
                spidir_block_t cont = spidir_builder_create_block(builder);

                // TODO: perform a type check already, not emitting any check if its the same type
                //       or emit a single NULL if it is not the same

                bool def_isinst = false;
                bool def_not_isinst = false;

                spidir_value_t result = obj.value;
                if (!def_not_isinst && !def_isinst) {
                    // emit the type check itself
                    spidir_value_t isinst = jit_emit_type_check(
                        builder,
                        obj.value, jit_is_interface(obj.type),
                        inst.operand.type
                    );

                    spidir_block_t perform_isinst = spidir_builder_create_block(builder);

                    // check if not null, if not then go to perform inst, otherwise
                    // go to the continuation
                    spidir_value_t is_not_null = spidir_builder_build_icmp(
                        builder,
                        SPIDIR_ICMP_NE, SPIDIR_TYPE_I32,
                        obj.value,
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0)
                    );

                    // null check, if null then continue with null
                    spidir_builder_build_brcond(builder, is_not_null, perform_isinst, cont);

                    // we don't have a null, perform the isinst
                    spidir_builder_set_block(builder, perform_isinst);

                    spidir_block_t not_isinst = spidir_builder_create_block(builder);

                    // place a brcond, we go to the same location but we will
                    // have a phi with different results
                    spidir_builder_build_brcond(builder, isinst, cont, not_isinst);

                    // throw the cast class exception
                    spidir_builder_set_block(builder, not_isinst);

                    spidir_builder_build_call(
                        builder,
                        m_jit_throw_invalid_cast_exception,
                        0, NULL
                    );

                    // should not return from invalid cast exception
                    spidir_builder_build_unreachable(builder);

                    // finally place the continuation
                    spidir_builder_set_block(builder, cont);

                    // if the dest type is an interface, we need to actually convert it
                    if (jit_is_interface(inst.operand.type)) {
                        spidir_value_t iface = jit_get_struct_slot(builder, inst.operand.type);
                        ASSERT(jit_convert_interface(
                            builder,
                            iface, result,
                            inst.operand.type,
                            obj.type
                        ));
                        result = iface;
                    }

                } else if (def_isinst) {
                    // is def the instance
                    // nothing special to do, since we always push the value

                } else if (def_not_isinst) {
                    // is def not the same, just give null

                    // we throw the exception
                    // TODO: mark as unreachable with the rest
                    //       of the code, not possible right now
                    //       since we keep emitting and have no
                    //       way to stop emitting
                    spidir_builder_build_call(
                        builder,
                        m_jit_throw_invalid_cast_exception,
                        0, NULL
                    );

                } else {
                    CHECK_FAIL();
                }

                // push the result, for value types we use the known type for things
                if (tdn_type_is_valuetype(inst.operand.type)) {
                    EVAL_STACK_PUSH(tObject, result, .attrs = { .known_type = inst.operand.type });
                } else {
                    EVAL_STACK_PUSH(inst.operand.type, result);
                }
            } break;

            case CEE_ISINST: {
                jit_stack_value_t obj = EVAL_STACK_POP();

                // create the continuation
                spidir_block_t cont = spidir_builder_create_block(builder);

                // TODO: perform a type check already, not emitting any check if its the same type
                //       or emit a single NULL if it is not the same

                bool def_isinst = false;
                bool def_not_isinst = false;

                spidir_value_t result = SPIDIR_VALUE_INVALID;
                if (!def_not_isinst && !def_isinst) {
                    // emit the type check itself
                    spidir_value_t isinst = jit_emit_type_check(
                        builder,
                        obj.value, jit_is_interface(obj.type),
                        inst.operand.type
                    );

                    spidir_block_t perform_isinst = spidir_builder_create_block(builder);

                    // check if not null, if not then go to perform inst, otherwise
                    // go to the continuation
                    spidir_value_t is_not_null = spidir_builder_build_icmp(
                        builder,
                        SPIDIR_ICMP_NE, SPIDIR_TYPE_I32,
                        obj.value,
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0)
                    );

                    // null check, if null then continue with null
                    spidir_builder_build_brcond(builder, is_not_null, perform_isinst, cont);

                    // we don't have a null, perform the isinst
                    spidir_builder_set_block(builder, perform_isinst);

                    // place a brcond, we go to the same location but we will
                    // have a phi with different results
                    spidir_builder_build_brcond(builder, isinst, cont, cont);

                    // finally place the continuation
                    spidir_builder_set_block(builder, cont);

                    // build the phi, this will either be the pointer if the same
                    // or a NULL if not the same
                    spidir_value_t values[] = {
                        // the is_not_null check
                        // technically we can also use obj.value, but I think semantically
                        // its more correct to treat it as a null on its own
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0),

                        // the type check
                        obj.value,
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0)
                    };
                    result = spidir_builder_build_phi(
                        builder, SPIDIR_TYPE_PTR,
                        ARRAY_LENGTH(values), values,
                        NULL
                    );

                } else if (def_isinst) {
                    // is def the instance, just give the value
                    result = obj.value;

                } else if (def_not_isinst) {
                    // is def not the same, just give null
                    result = spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0);

                } else {
                    CHECK_FAIL();
                }

                // push the result, for value types we use the known type for things
                if (tdn_type_is_valuetype(inst.operand.type)) {
                    EVAL_STACK_PUSH(tObject, result, .attrs = { .known_type = inst.operand.type });
                } else {
                    EVAL_STACK_PUSH(inst.operand.type, result);
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Exception handling
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_THROW: {
                jit_stack_value_t obj = EVAL_STACK_POP();

                // just throw the object
                spidir_builder_build_call(builder,
                    m_jit_throw,
                    2,
                    (spidir_value_t[]){
                        obj.value,
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, current_pc)
                    }
                );

                // and we are unreachable after this
                spidir_builder_build_unreachable(builder);

                // and clear the stack
                arrsetlen(stack, 0);
            } break;

            case CEE_LEAVE: {
                // empty the stack
                arrsetlen(stack, 0);

                // check if there is any finally around us
                spidir_block_t target_block;
                RuntimeExceptionHandlingClause clause = jit_get_enclosing_try_clause(jmethod, current_pc, COR_ILEXCEPTION_CLAUSE_FINALLY, NULL);
                if (clause != NULL) {
                    CHECK_AND_RETHROW(emit_merge_basic_block(
                        jmethod, builder,
                        clause->HandlerOffset,
                        stack, &target_block,
                        inst.operand.branch_target));
                } else {
                    CHECK_AND_RETHROW(emit_merge_basic_block(
                        jmethod, builder,
                        inst.operand.branch_target,
                        stack, &target_block,
                        get_leave_target(block->leave_target_stack)));
                }

                // go to the target, will either be the leave destination
                // or the finally handler before it
                spidir_builder_build_branch(builder, target_block);
            } break;

            case CEE_ENDFINALLY: {
                // empty the stack
                arrsetlen(stack, 0);

                // TODO: verify the endfinally is actually inside a finally handler

                // check if there is any finally around us
                RuntimeExceptionHandlingClause clause = jit_get_enclosing_try_clause(jmethod, current_pc, COR_ILEXCEPTION_CLAUSE_FINALLY, NULL);
                spidir_block_t target_block;
                if (clause != NULL) {
                    // we do, merge with it, we need to go to the clause
                    // the leave target will actually stay the same for
                    // this case
                    CHECK_AND_RETHROW(emit_merge_basic_block(
                        jmethod, builder,
                        clause->HandlerOffset,
                        stack, &target_block,
                        get_leave_target(block->leave_target_stack)));

                } else {
                    // take the previous leave target, or -1 if non
                    long previous_leave_target = -1;
                    if (arrlen(block->leave_target_stack) >= 2) {
                        previous_leave_target = block->leave_target_stack[arrlen(block->leave_target_stack) - 2];
                    }

                    // we don't have any finally handlers, we can call the
                    // target directly, we use the same leave target that
                    // we have right now
                    CHECK_AND_RETHROW(emit_merge_basic_block(
                        jmethod, builder,
                        get_leave_target(block->leave_target_stack),
                        stack, &target_block,
                        previous_leave_target));
                }

                // go to the target, will either be the leave destination
                // or the finally handler before it
                spidir_builder_build_branch(builder, target_block);
            } break;
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Misc
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_SIZEOF: {
                spidir_value_t value = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, inst.operand.type->StackSize);
                EVAL_STACK_PUSH(tInt32, value);
            } break;

            case CEE_NOP: break;

            default: CHECK_FAIL("Unknown opcode `%s`", tdn_get_opcode_name(inst.opcode));
        }

        arrfree(args_type);
        arrfree(args);

#ifdef JIT_VERBOSE_EMIT
        indent = tdn_disasm_print_end(body, pc, indent);
#endif
    }

    // we have a fallthrough, handle it
    if (inst.control_flow == TDN_IL_CF_NEXT || inst.control_flow == TDN_IL_CF_CALL) {
        spidir_block_t new_block;
        CHECK_AND_RETHROW(emit_merge_basic_block(
            jmethod, builder,
            pc, stack, &new_block,
            get_leave_target(block->leave_target_stack)));

        spidir_builder_build_branch(builder, new_block);
    }

cleanup:
    arrfree(stack);
    arrfree(args_type);
    arrfree(args);

    return err;
}

static bool jit_prepare_args(spidir_builder_handle_t builder, jit_method_t* method) {
    bool modified_block = false;

    for (int i = 0; i < arrlen(method->args); i++) {
        jit_arg_t* arg = &method->args[i];

        if (arg->spill_required) {
            // create the stack slot
            arg->value = spidir_builder_build_stackslot(builder,
                arg->type->StackSize, arg->type->StackAlignment);

            // and store the value to it
            spidir_value_t value = spidir_builder_build_param_ref(builder, i);
            spidir_mem_size_t size;
            if (jit_is_struct_like(arg->type)) {
                size = SPIDIR_MEM_SIZE_8;
            } else {
                size = get_spidir_mem_size(arg->type);
            }
            spidir_builder_build_store(builder, size, value, arg->value);

            modified_block = true;
        } else {
            // just remember the param ref
            arg->value = spidir_builder_build_param_ref(builder, i);
        }
    }

    return modified_block;
}

static bool jit_prepare_locals(spidir_builder_handle_t builder, jit_method_t* method) {
    bool modified_block = false;

    for (int i = 0; i < arrlen(method->locals); i++) {
        jit_local_t* arg = &method->locals[i];

        arg->value = spidir_builder_build_stackslot(builder, arg->type->StackSize, arg->type->StackAlignment);
        if (jit_is_struct_like(arg->type)) {
            jit_emit_bzero(builder, arg->value, arg->type);
        } else {
            spidir_builder_build_store(builder,
                get_spidir_mem_size(arg->type),
                spidir_builder_build_iconst(builder, get_spidir_type(arg->type), 0),
                arg->value
            );
        }
    }

    return modified_block;
}

static void jit_emit_spidir_from_il(spidir_builder_handle_t builder, void* _ctx) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_emit_ctx_t* ctx = _ctx;
    jit_method_t* jmethod = ctx->method;

    // start from the first block
    jit_queue_block(jmethod, builder, jmethod->basic_blocks[0]);

    // set the entry block
    spidir_builder_set_entry_block(builder, jmethod->basic_blocks[0]->block);
    spidir_builder_set_block(builder, jmethod->basic_blocks[0]->block);

    bool modified_block = false;

    // prepare the args
    if (jit_prepare_args(builder, jmethod)) {
        modified_block = true;
    }

    // prepare te locals
    if (jit_prepare_locals(builder, jmethod)) {
        modified_block = true;
    }

    // did we modify the block? if so we need to allocate a new block
    // to be used for the main block
    if (modified_block) {
        // create a new block
        spidir_block_t new_block = spidir_builder_create_block(builder);
        spidir_builder_build_branch(builder, new_block);
        jmethod->basic_blocks[0]->block = new_block;
    }

    // and dispatch them all
    while (arrlen(jmethod->block_queue)) {
        jit_basic_block_t* block = arrpop(jmethod->block_queue);
        CHECK_AND_RETHROW(jit_emit_basic_block(builder, jmethod, block));
    }

cleanup:
    ctx->err = err;
}

static tdn_err_t jit_emit_method(jit_method_t* method) {
    tdn_err_t err = TDN_NO_ERROR;

#ifdef JIT_DEBUG_EMIT
    TRACE("%T::%U", method->method->DeclaringType, method->method->Name);
#endif

    if (method->method->MethodBody == NULL) {
        jit_builtin_context_t ctx = {
            .method = method->method,
            .err = TDN_NO_ERROR
        };
        spidir_module_build_function(
            m_jit_module,
            method->function,
            jit_emit_builtin,
            &ctx
        );
        CHECK_AND_RETHROW(ctx.err);
    } else {
        jit_emit_ctx_t ctx = {
            .method = method,
            .err = TDN_NO_ERROR
        };
        spidir_module_build_function(
            m_jit_module,
            method->function,
            jit_emit_spidir_from_il,
            &ctx
        );
        CHECK_AND_RETHROW(ctx.err);
    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Top level API management
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * The methods left to be emitted
 */
static jit_method_t** m_methods_to_emit = NULL;

/**
 * The types which need their vtables fixed
 */
static RuntimeTypeInfo* m_types_to_emit = NULL;

typedef struct jit_method_result {
    // offset within the map
    size_t offset;

    // the codegen blob
    spidir_codegen_blob_handle_t blob;
} jit_method_result_t;

typedef struct jit_method_results {
    jit_method_result_t function;
    jit_method_result_t thunk;
} jit_method_results_t;

/**
 * Blobs of the emitted methods
 */
static jit_method_results_t* m_method_jit_results = NULL;

/**
 * The mahcine used for jtiting
 */
static spidir_codegen_machine_handle_t m_spidir_machine = NULL;

tdn_err_t jit_init_emit() {
    tdn_err_t err = TDN_NO_ERROR;

    // create the backend used for jitting
    m_spidir_machine = spidir_codegen_create_x64_machine();

cleanup:
    return err;
}

static spidir_function_t create_spidir_function(RuntimeMethodBase method, bool external) {
    // build the name
    string_builder_t builder = {};
    string_builder_push_method_signature(&builder, method, true);
    const char* name = string_builder_build(&builder);

    // get the signature
    spidir_value_type_t ret_type = jit_get_spidir_ret_type(method);
    spidir_value_type_t* arg_types = jit_get_spidir_arg_types(method);

    spidir_function_t function;
    if (external) {
        function = spidir_module_create_extern_function(m_jit_module, name, ret_type, arrlen(arg_types), arg_types);
    } else {
        function = spidir_module_create_function(m_jit_module, name, ret_type, arrlen(arg_types), arg_types);
    }

    arrfree(arg_types);
    string_builder_free(&builder);

    return function;
}

void jit_queue_emit(jit_method_t* method) {
    // if required create a new spidir module
    if (m_jit_module == NULL) {
        // create the module
        m_jit_module = spidir_module_create();

        // create all the helpers we might need
        create_jit_helpers();
    }

    // emit it
    arrpush(m_methods_to_emit, method);

    // create the function itself
    method->function = create_spidir_function(method->method, false);
}

void jit_queue_emit_extern(jit_method_t* method) {
    // if required create a new spidir module
    if (m_jit_module == NULL) {
        m_jit_module = spidir_module_create();

    }

    // create the function itself
    method->function = create_spidir_function(method->method, true);
}

void jit_queue_emit_type(RuntimeTypeInfo type) {
    arrpush(m_types_to_emit, type);
}

static tdn_err_t jit_relocate_function(void* method_ptr, jit_method_result_t* blob, jit_method_t* method) {
    tdn_err_t err = TDN_NO_ERROR;

    size_t reloc_count = spidir_codegen_blob_get_reloc_count(blob->blob);
    const spidir_codegen_reloc_t* relocs = spidir_codegen_blob_get_relocs(blob->blob);
    for (size_t j = 0; j < reloc_count; j++) {
        uint64_t P = (uint64_t)(method_ptr + relocs[j].offset);
        int64_t A = relocs[j].addend;

        // resolve the target, will either be a builtin or a
        // method pointer we jitted
        uint64_t F;
        jit_method_t* target = jit_get_method_from_function(relocs[j].target);
        if (target == NULL) {
            void* ptr = hmget(m_jit_helper_lookup, relocs[j].target);
            CHECK(ptr != NULL);
            F = (uint64_t)ptr;
        } else {
            // check if we reference the function or the thunk
            if (target->function.id == relocs[j].target.id) {
                F = (uint64_t)(target->method->MethodPtr);
            } else {
                F = (uint64_t)(target->method->ThunkPtr);
            }
            CHECK(F != 0);
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

/**
 * If this is a virtual method from a value type, then we need a thunk for moving the this pointer
 * from the boxed object to the non-boxed offset
 */
static bool jit_needs_value_type_virtual_thunk(RuntimeMethodBase method) {
    return tdn_type_is_valuetype(method->DeclaringType) && !method->Attributes.Static && method->Attributes.Virtual;
}

static tdn_err_t jit_map_and_relocate(size_t map_size) {
    tdn_err_t err = TDN_NO_ERROR;

    // map it as read-write, initialize it as fully int3 just in case
    void* map = tdn_host_map(map_size);
    CHECK_ERROR(map != NULL, TDN_ERROR_OUT_OF_MEMORY);
    memset(map, 0xCC, map_size);

    // copy over all of the code and set the method pointers
    for (int i = 0; i < arrlen(m_method_jit_results); i++) {
        jit_method_results_t* results = &m_method_jit_results[i];
        jit_method_t* method = m_methods_to_emit[i];

        if (results->function.blob != NULL) {
            method->method->MethodPtr = map + results->function.offset;
            method->method->MethodSize = spidir_codegen_blob_get_code_size(results->function.blob);
            memcpy(
                method->method->MethodPtr,
                spidir_codegen_blob_get_code(results->function.blob),
                method->method->MethodSize
            );
        }

        if (results->thunk.blob != NULL) {
            method->method->ThunkPtr = map + results->thunk.offset;
            method->method->ThunkSize = spidir_codegen_blob_get_code_size(results->thunk.blob);
            memcpy(
                method->method->ThunkPtr,
                spidir_codegen_blob_get_code(results->thunk.blob),
                method->method->ThunkSize
            );
        }

        // check if we need to create a value type virtual thunk, this adjusts the this
        // pointer to not have the object header like the value type methods expect
        // we always make sure we have 16 bytes between functions so we can easily
        // put this before the function, keeping all the the code aligned to 16 bytes
        if (jit_needs_value_type_virtual_thunk(method->method)) {
            CHECK(method->method->ThunkPtr == NULL);

            size_t object_header_size = ALIGN_UP(sizeof(struct Object), method->method->DeclaringType->HeapAlignment);
            CHECK(object_header_size <= 0x7F);

            // add rdi, $object_header_size
            uint8_t opcode[4] = { 0x48, 0x83, 0xC7, object_header_size };

            // setup the size and copy the opcode
            method->method->ThunkSize = sizeof(opcode);
            method->method->ThunkPtr = method->method->MethodPtr - sizeof(opcode);
            memcpy(method->method->ThunkPtr, opcode, sizeof(opcode));
        }

        // check the static constructor
        RuntimeTypeInfo type = method->method->DeclaringType;
        if (type->TypeInitializer == (RuntimeConstructorInfo)method->method) {
            if (type->Attributes.BeforeFieldInit) {
                jit_queue_cctor(method->method->MethodPtr);
            } else {
                // TODO: need to generate a hook or something
                //       to call the initializer
                CHECK_FAIL();
            }
        }
    }

    // now we can apply the relocations, this can technically be done in parallel but I think
    // its cheap enough that its not worth it
    for (int i = 0; i < arrlen(m_method_jit_results); i++) {
        jit_method_results_t* results = &m_method_jit_results[i];
        jit_method_t* method = m_methods_to_emit[i];

        if (results->function.blob != NULL) {
            jit_relocate_function(method->method->MethodPtr, &results->function, method);
        }

        if (results->thunk.blob != NULL) {
            jit_relocate_function(method->method->ThunkPtr, &results->thunk, method);
        }
    }

    // now remap it as read-execute
    tdn_host_map_rx(map, map_size);

cleanup:
    return err;
}

tdn_err_t jit_emit(void) {
    tdn_err_t err = TDN_NO_ERROR;

    // if there were no methods that need to be jitted don't do anything
    if (arrlen(m_methods_to_emit) == 0) {
        goto cleanup;
    }

    // emit all the methods, do it inline since its simpler
    for (int i = 0; i < arrlen(m_methods_to_emit); i++) {
        jit_method_t* method = m_methods_to_emit[i];
        CHECK_AND_RETHROW(jit_emit_method(method));
    }

#ifdef JIT_DUMP_EMIT
    void* ctx = tdn_host_jit_start_dump();
    spidir_module_dump(m_jit_module, tdn_host_jit_dump_callback, ctx);
    tdn_host_jit_end_dump(ctx);
#endif

    //
    // Optimization time!
    //

    // perform all global optimizations
    spidir_opt_run(m_jit_module);


    // perform the codegen
    // TODO: do this in parallel in the future
    arrsetlen(m_method_jit_results, arrlen(m_methods_to_emit));
    memset(m_method_jit_results, 0, arrlen(m_method_jit_results) * sizeof(*m_method_jit_results));

    for (int i = 0; i < arrlen(m_methods_to_emit); i++) {
        jit_method_t* method = m_methods_to_emit[i];

        // if already was jitted before then don't jit it again
        if (method->method->MethodPtr == NULL) {
            spidir_codegen_config_t config = {
                .verify_ir = true,
                .verify_regalloc = true
            };
            spidir_codegen_status_t status = spidir_codegen_emit_function(
                m_spidir_machine, &config,
                m_jit_module,
                method->function,
                &m_method_jit_results[i].function.blob
            );
            CHECK(status == SPIDIR_CODEGEN_OK, "Failed to jit: %d", status);
        }

        if (method->has_thunk) {
            spidir_codegen_config_t config = {
                .verify_ir = true,
                .verify_regalloc = true
            };
            spidir_codegen_status_t status = spidir_codegen_emit_function(
                m_spidir_machine, &config,
                m_jit_module,
                method->thunk,
                &m_method_jit_results[i].thunk.blob
            );
            CHECK(status == SPIDIR_CODEGEN_OK, "Failed to jit: %d", status);
        }
    }

    // now that all the codegen is finished we can sum up the size
    // required and map it
    // we align methods to 16 bytes but also make sure that they always
    // have room of at least 16 bytes, this just makes sure that we have
    // place for any thunk needed to happen before the function
    size_t map_size = 0;
    for (int i = 0; i < arrlen(m_method_jit_results); i++) {
        jit_method_results_t* results = &m_method_jit_results[i];

        if (results->function.blob != NULL) {
            map_size += 16;
            map_size = ALIGN_UP(map_size, 16);
            results->function.offset = map_size;
            map_size += spidir_codegen_blob_get_code_size(results->function.blob);
        }

        if (results->thunk.blob != NULL) {
            map_size += 16;
            map_size = ALIGN_UP(map_size, 16);
            results->thunk.offset = map_size;
            map_size += spidir_codegen_blob_get_code_size(results->thunk.blob);
        }
    }

    if (map_size > 0) {
        CHECK_AND_RETHROW(jit_map_and_relocate(map_size));
    }

    // now fill up all the vtables
    for (int i = 0; i < arrlen(m_types_to_emit); i++) {
        RuntimeTypeInfo type = m_types_to_emit[i];

        for (int j = 0; j < type->VTable->Length; j++) {
            RuntimeMethodInfo method = type->VTable->Elements[j];

            // if we have a thunk then use it instead
            void* ptr = method->MethodPtr;
            if (method->ThunkPtr != NULL) {
                ptr = method->ThunkPtr;
            }
            CHECK(ptr != NULL);

            // save it in the jit vtable
            type->JitVTable->Functions[j] = ptr;
        }
    }

cleanup:
    // destroy all the blobs and free the results
    for (int i = 0; i < arrlen(m_method_jit_results); i++) {
        if (m_method_jit_results[i].function.blob != NULL) {
            spidir_codegen_blob_destroy(m_method_jit_results[i].function.blob);
        }
        if (m_method_jit_results[i].thunk.blob != NULL) {
            spidir_codegen_blob_destroy(m_method_jit_results[i].thunk.blob);
        }
    }

    arrfree(m_method_jit_results);
    arrfree(m_methods_to_emit);
    arrfree(m_types_to_emit);
    hmfree(m_jit_helper_lookup);

    // cleanup the module
    if (m_jit_module != NULL) {
        spidir_module_destroy(m_jit_module);
        m_jit_module = NULL;
    }

    return err;
}
