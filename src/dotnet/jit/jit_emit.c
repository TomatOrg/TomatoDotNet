#include "jit_emit.h"

#include <stdalign.h>
#include <tomatodotnet/disasm.h>
#include <util/except.h>
#include <util/stb_ds.h>
#include <util/string.h>
#include <util/string_builder.h>

#include <spidir/opt.h>

#include "jit_builtin.h"
#include "jit_helpers.h"
#include "jit_verify.h"

spidir_value_type_t jit_get_spidir_type(RuntimeTypeInfo type) {
    if (
        type == tBoolean || type == tChar ||
        type == tSByte || type == tByte ||
        type == tInt16 || type == tUInt16 ||
        type == tInt32 || type == tUInt32
    ) {
        return SPIDIR_TYPE_I32;
    } else if (
        type == tInt64 || type == tUInt64 ||
        type == tIntPtr  || type == tUIntPtr
    ) {
        return SPIDIR_TYPE_I64;
    } else {
        // anything else is a pointer, be structs, refs, pointers, arrays or whatever
        return SPIDIR_TYPE_PTR;
    }
}

spidir_value_type_t jit_get_spidir_ret_type(RuntimeMethodBase method) {
    RuntimeTypeInfo type = method->ReturnParameter->ParameterType;
    if (jit_is_struct_like(type)) {
        // things which act like a struct return by using an implicit reference
        return SPIDIR_TYPE_NONE;
    }

    return jit_get_spidir_type(type);
}

spidir_value_type_t* jit_get_spidir_arg_types(RuntimeMethodBase method) {
    spidir_value_type_t* types = NULL;

    // this pointer
    if (!method->Attributes.Static) {
        arrpush(types, SPIDIR_TYPE_PTR);
    }

    // and now all of the arguments
    for (int i = 0; i < method->Parameters->Length; i++) {
        spidir_value_type_t type = jit_get_spidir_type(method->Parameters->Elements[i]->ParameterType);
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


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Actual emit code
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//----------------------------------------------------------------------------------------------------------------------
// Buffer operations
//----------------------------------------------------------------------------------------------------------------------

static void jit_emit_bzero(spidir_builder_handle_t builder, spidir_value_t ptr, RuntimeTypeInfo type) {
    // get the correct function for the bzero
    spidir_function_t function;
    if (type->IsUnmanaged) {
        function = jit_helper_get(spidir_builder_get_module(builder), JIT_HELPER_BZERO);
    } else {
        function = jit_helper_get(spidir_builder_get_module(builder), JIT_HELPER_GC_BZERO);
    }

    // call it
    spidir_builder_build_call(builder,
        function,
        2, (spidir_value_t[]){
            ptr,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, type->StackSize)
        });
}

static void jit_emit_memcpy(spidir_builder_handle_t builder, spidir_value_t dest, spidir_value_t src, RuntimeTypeInfo type) {
    // get the correct function for the bzero
    spidir_function_t function;
    if (type->IsUnmanaged) {
        function = jit_helper_get(spidir_builder_get_module(builder), JIT_HELPER_MEMCPY);
    } else {
        function = jit_helper_get(spidir_builder_get_module(builder), JIT_HELPER_GC_MEMCPY);
    }

    // call it
    spidir_builder_build_call(builder,
        function,
        3, (spidir_value_t[]){
            dest, src,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, type->StackSize)
        });
}

//----------------------------------------------------------------------------------------------------------------------
// De-virt
//----------------------------------------------------------------------------------------------------------------------

/**
 * Get the offset of the interface inside the vtable of the given type
 * TODO: take variance into account
 */
static size_t jit_get_interface_offset(RuntimeTypeInfo type, RuntimeTypeInfo iface) {
    int idx = hmgeti(type->InterfaceImpls, iface);
    if (idx < 0) {
        return -1;
    }
    return type->InterfaceImpls[idx].value;
}

/**
 * Attempt to de-virtualize a method based on the value we got on the stack
 */
static RuntimeMethodBase jit_devirt_method(jit_value_t instance, RuntimeMethodBase method) {
    // not a virtual method, nothing to de-virt
    if (!method->Attributes.Virtual) {
        return method;
    }

    // choose either the knwon or the generic type
    RuntimeTypeInfo type = instance.attrs.known_type != NULL ? instance.attrs.known_type : instance.type;

    // perform a vtable lookup
    RuntimeMethodBase real_method;
    if (method->DeclaringType->Attributes.Interface) {
        size_t offset = jit_get_interface_offset(type, method->DeclaringType);
        ASSERT(offset != -1);
        // TODO: variance support
        real_method = (RuntimeMethodBase)type->VTable->Elements[offset + method->VTableOffset];
    } else {
        real_method = (RuntimeMethodBase)type->VTable->Elements[method->VTableOffset];
    }

    // if this is the final implementation, or we are a sealed
    // type then we can just return the real method
    // if the type is known to be exactly as said
    // then we are also going to handle it like so
    if (real_method->Attributes.Final || type->Attributes.Sealed || instance.attrs.exact_known_type) {
        return real_method;
    }

    return method;
}

//----------------------------------------------------------------------------------------------------------------------
// Inlining
//----------------------------------------------------------------------------------------------------------------------

// these constants are inspired by openjdk
#define INLINE_SMALL_CODE       1000    /* native function size */
#define MAX_INLINE_SIZE         35      /* il bytecode size */
#define MAX_TRIVIAL_SIZE        6       /* il bytecode size */
#define MAX_INLINE_LEVEL        15      /* the max nesting of inline we allow */

static bool jit_check_should_inline(jit_method_t* from, RuntimeMethodBase method) {
    if (method->MethodBody == NULL) {
        return false;
    }

    // inline is requested
    if (method->MethodImplFlags.AggressiveInlining) {
        return true;
    }

    // TODO: for now assume a method is cold, we might want some logic
    //       to check for hot methods

    // check if already compiled into a medium method
    if (method->MethodPtr != NULL && method->MethodSize > (INLINE_SMALL_CODE / 4)) {
        return false;
    }

    // method too big for inline
    if (method->MethodBody->ILSize > MAX_INLINE_SIZE) {
        return false;
    }

    return true;
}

static bool jit_check_should_not_inline(jit_method_t* from, RuntimeMethodBase method) {
    if (method->MethodBody == NULL) {
        return true;
    }

    // make sure we don't recursive too much
    if (from->inline_level + 1 >= MAX_INLINE_LEVEL) {
        return true;
    }

    // don't allow recursive inline
    if (from->method == method) {
        return true;
    }

    // request no inline by attribute
    if (method->MethodImplFlags.NoInlining) {
        return true;
    }

    // requested inline by attribute
    if (method->MethodImplFlags.AggressiveInlining) {
        return false;
    }

    // don't inline if already compiled into a big method
    if (method->MethodPtr != NULL && method->MethodSize > INLINE_SMALL_CODE) {
        return true;
    }

    // small methods should always get inlined
    if (method->MethodBody->ILSize <= MAX_TRIVIAL_SIZE) {
        return false;
    }

    // TODO: based on some heuristics or something?

    return false;
}

static bool jit_emit_should_inline(jit_method_t* from, RuntimeMethodBase method) {
#ifdef JIT_DISABLE_INLINE
    return false;
#else
    return !jit_check_should_not_inline(from, method) && jit_check_should_inline(from, method);
#endif
}

// forward decl
static tdn_err_t jit_emit_method(jit_method_t* method, spidir_value_t* args, spidir_builder_handle_t builder);

static tdn_err_t jit_perform_inline(
    spidir_builder_handle_t builder,
    jit_method_t* from, spidir_value_t* args,
    RuntimeMethodBase to_inline,
    spidir_value_t* retval
) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_method_t inlined_method = {
        .method = to_inline,
        .inline_level = from->inline_level + 1
    };

    TRACE("ATTEMPT ONE:");
    for (int i = 0; hmlen(inlined_method.labels); i++) {
        TRACE("%p", inlined_method.labels[i].value);
    }

    // prepare the method
    CHECK_AND_RETHROW(jit_verify_prepare_method(&inlined_method));

    // TODO: modify the method with the attributes that we have for it
    //       this should help with de-virt

    // finalize the verification with the new de-virt info
    CHECK_AND_RETHROW(jit_verify_method(&inlined_method));

    // prepare the return pad
    spidir_block_t current;
    CHECK(spidir_builder_cur_block(builder, &current));

    // create the return pad with a phi for setting the return value, we are going to
    // return the stackslot directly even when its a normal struct
    inlined_method.inline_return_block = spidir_builder_create_block(builder);
    spidir_builder_set_block(builder, inlined_method.inline_return_block);
    spidir_value_t return_value = spidir_builder_build_phi(
        builder,
        jit_get_spidir_type(to_inline->ReturnParameter->ParameterType),
        0, NULL, &inlined_method.inline_return_phi
    );
    spidir_builder_set_block(builder, current);

    // and now emit the inlined method
    CHECK_AND_RETHROW(jit_emit_method(&inlined_method, args, builder));

    // and now that we are done continue from the return pad
    spidir_builder_set_block(builder, inlined_method.inline_return_block);

    // we are done
    *retval = return_value;

cleanup:
    jit_destroy_method(&inlined_method);

    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Block merging
//----------------------------------------------------------------------------------------------------------------------

static jit_basic_block_t* jit_emit_get_basic_block(jit_method_t* method, long target_pc, long leave_target) {
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

static tdn_err_t jit_pass_value(jit_value_t* incoming, jit_value_t* target) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(arrlen(incoming) == arrlen(target));
    for (int i = 0; i < arrlen(incoming); i++) {
        // sanity that everything ended up as we expected
        CHECK(incoming[i].type == target[i].type);
        target[i].value = incoming[i].value;
    }

cleanup:
    return err;
}

static tdn_err_t jit_create_phis(spidir_builder_handle_t builder, jit_value_t* incoming, jit_value_t* target) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(arrlen(incoming) == arrlen(target));
    for (int i = 0; i < arrlen(incoming); i++) {
        // sanity that everything ended up as we expected
        CHECK(incoming[i].type == target[i].type);

        // and create the phi with the incoming value already
        if (!incoming->attrs.spilled) {
            target[i].value = spidir_builder_build_phi(builder,
                jit_get_spidir_type(incoming[i].type),
                1, &incoming[i].value,
                &target[i].phi
            );
        }
    }

cleanup:
    return err;
}

static tdn_err_t jit_merge_phis(spidir_builder_handle_t builder, jit_value_t* incoming, jit_value_t* target) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(arrlen(incoming) == arrlen(target));
    for (int i = 0; i < arrlen(incoming); i++) {
        // sanity that everything ended up as we expected
        CHECK(incoming[i].type == target[i].type);

        if (!incoming->attrs.spilled) {
            // and create the phi with the incoming value already
            spidir_builder_add_phi_input(builder, target[i].phi, incoming[i].value);
        }
    }

cleanup:
    return err;
}

static tdn_err_t jit_emit_merge_basic_block(
    jit_method_t* method,
    spidir_builder_handle_t builder,
    uint32_t target_pc, spidir_block_t* block,
    jit_value_t* stack, jit_value_t* locals, jit_value_t* args,
    long leave_target
) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_basic_block_t* target = jit_emit_get_basic_block(method, target_pc, leave_target);
    CHECK(target != NULL);
    CHECK(target->initialized);

    // push to the queue for emitting
    if (!target->in_queue) {
        arrpush(method->block_queue, target);
        target->in_queue = true;
    }

    // create the new block
    target->block = spidir_builder_create_block(builder);
    *block = target->block;

    // check if we need to generate and merge phis or not
    if (target->need_phis) {
        // will enter this code multiple times, prepare the phis and the new block
        if (!target->emitted) {
            // switch to the new block to create the phis
            spidir_block_t cur_block;
            bool has_current = spidir_builder_cur_block(builder, &cur_block);
            spidir_builder_set_block(builder, target->block);

            CHECK_AND_RETHROW(jit_create_phis(builder, stack, target->stack));
            CHECK_AND_RETHROW(jit_create_phis(builder, locals, target->locals));
            CHECK_AND_RETHROW(jit_create_phis(builder, args, target->args));

            // return to the current block
            if (has_current) {
                spidir_builder_set_block(builder, cur_block);
            }
        } else {
            // merge the phis
            CHECK_AND_RETHROW(jit_merge_phis(builder, stack, target->stack));
            CHECK_AND_RETHROW(jit_merge_phis(builder, locals, target->locals));
            CHECK_AND_RETHROW(jit_merge_phis(builder, args, target->args));
        }

    } else {
        // must only reach this once, so ensure that it is not yet initialized
        CHECK(!target->emitted);

        // for sanity perform merging, this is not required
        CHECK_AND_RETHROW(jit_pass_value(stack, target->stack));
        CHECK_AND_RETHROW(jit_pass_value(locals, target->locals));
        CHECK_AND_RETHROW(jit_pass_value(args, target->args));
    }

    // mark as initialized
    target->emitted = true;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Converting pointers
//----------------------------------------------------------------------------------------------------------------------

static void jit_object_to_interface(
    spidir_builder_handle_t builder,
    RuntimeTypeInfo from, spidir_value_t from_value,
    RuntimeTypeInfo to, spidir_value_t to_value
) {
    // fast path for explicit null value, just bzero
    if (from == NULL) {
        jit_emit_bzero(builder, to_value, to);
        return;
    }

    size_t offset = jit_get_interface_offset(from, to);
    ASSERT(offset != -1);

    // store the instance part, can be done unconditionally
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, from_value, to_value);

    // check if the object is null
    spidir_block_t non_null_path = spidir_builder_create_block(builder);
    spidir_block_t next = spidir_builder_create_block(builder);
    spidir_builder_build_brcond(builder, from_value, non_null_path, next);

    // its not null, load the vtable base
    spidir_builder_set_block(builder, non_null_path);
    spidir_value_t vtable_base = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, from_value);
    spidir_builder_build_branch(builder, next);

    // continuation
    spidir_builder_set_block(builder, next);

    // create a phi, we will either have the vtable base if we came from the non-null path
    // or we will have NULL vtable base otherwise
    vtable_base = spidir_builder_build_phi(builder, SPIDIR_TYPE_PTR, 2,
        (spidir_value_t[]){
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0),
            vtable_base,
        }, NULL);

    // add to it the offset to the interface part
    // NOTE: we are fine with adding the offset to null, it will never result in a valid
    //       pointer so it will fail the same way as a normal zeroed interface
    vtable_base = spidir_builder_build_ptroff(builder, vtable_base,
        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64,
            offsetof(ObjectVTable, Functions) + sizeof(void*) * offset));

    // store the vtable part of the vtable
    spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, vtable_base,
        spidir_builder_build_ptroff(builder, to_value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable))));
}

/**
 * Emit a store to memory, this performs all the copies
 * required for that to happen
 *
 * assumes the to_value is the pointer to where you want to store the to
 * and that the from_value is the stack value that was there before
 */
static void jit_emit_store(
    spidir_builder_handle_t builder,
    RuntimeTypeInfo from, spidir_value_t from_value,
    RuntimeTypeInfo to, spidir_value_t to_value
) {
    if (!jit_is_interface(from) && jit_is_interface(to)) {
        // converting from an object to an interface
        jit_object_to_interface(builder, from, from_value, to, to_value);

    } else if (jit_is_interface(from) && !jit_is_interface(to)) {
        // converting from an interface to an object
        STATIC_ASSERT(offsetof(Interface, Instance) == 0);
        spidir_value_t instance = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, from_value);
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, instance, to_value);

    } else if (jit_is_interface(from) && jit_is_interface(to) && from != to) {
        // lowering interface

        // get the offset of the sub interface from this interface
        size_t offset = jit_get_interface_offset(from, to);
        ASSERT(offset != -1);

        // store the instance
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, from_value, to_value);

        // load the current base
        // NOTE: if the instance is null, this still works, while we won't have a completely null
        //       vtable, it will still be under the 2gb mark so it will fail to do any deref from
        //       it
        spidir_value_t vtable_base = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
            spidir_builder_build_ptroff(builder, from_value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable))));

        // add to it the new base
        vtable_base = spidir_builder_build_ptroff(builder, vtable_base,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, sizeof(void*) * offset));

        // and now store the new one
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, vtable_base,
            spidir_builder_build_ptroff(builder, to_value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable))));

    } else if (jit_is_struct_like(from)) {
        // copying a struct-like, perform a memcpy
        jit_emit_memcpy(builder, to_value, from_value, from);

    } else {
        // a primitive value, just perform a normal store
        spidir_mem_size_t mem_size;
        switch (from->StackSize) {
            case 1: mem_size = SPIDIR_MEM_SIZE_1; break;
            case 2: mem_size = SPIDIR_MEM_SIZE_2; break;
            case 4: mem_size = SPIDIR_MEM_SIZE_4; break;
            case 8: mem_size = SPIDIR_MEM_SIZE_8; break;
            default: ASSERT(!"Invalid primitive value size");
        }
        spidir_builder_build_store(builder, mem_size, from_value, to_value);

    }
}

static spidir_value_t jit_emit_load(spidir_builder_handle_t builder, RuntimeTypeInfo from, spidir_value_t from_value) {
    if (jit_is_struct_like(from)) {
        // a struct like, allocate a stack slot and copy
        spidir_value_t value = spidir_builder_build_stackslot(builder, from->StackSize, from->StackAlignment);
        jit_emit_memcpy(builder, value, from_value, from);
        return value;

    } else {
        // a primitive value, just perform a normal load
        spidir_mem_size_t mem_size;
        switch (from->StackSize) {
            case 1: mem_size = SPIDIR_MEM_SIZE_1; break;
            case 2: mem_size = SPIDIR_MEM_SIZE_2; break;
            case 4: mem_size = SPIDIR_MEM_SIZE_4; break;
            case 8: mem_size = SPIDIR_MEM_SIZE_8; break;
            default: ASSERT(!"Invalid primitive value size");
        }
        return spidir_builder_build_load(builder, mem_size, jit_get_spidir_type(from), from_value);
    }
}

/**
 * Convert a pointer, this is done in-place of the from_value if possible
 * and might allocate a new stack-slot if required
 *
 * This is meant to be used when passing a value in the stack
 */
static spidir_value_t jit_convert_pointer(
    spidir_builder_handle_t builder,
    RuntimeTypeInfo from, spidir_value_t from_value,
    RuntimeTypeInfo to
) {
    // converting from an object to an interface
    if (!jit_is_interface(from) && jit_is_interface(to)) {
        spidir_value_t slot = spidir_builder_build_stackslot(builder, sizeof(Interface), alignof(Interface));
        jit_object_to_interface(builder, from, from_value, to, slot);
        return slot;
    }

    // converting from an interface to an object
    if (jit_is_interface(from) && !jit_is_interface(to)) {
        STATIC_ASSERT(offsetof(Interface, Instance) == 0);
        return spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, from_value);
    }

    // lowering interface, do it in place
    if (jit_is_interface(from) && jit_is_interface(to) && from != to) {
        // get the offset of the sub interface from this interface
        size_t offset = jit_get_interface_offset(from, to);
        ASSERT(offset != -1);

        // load the current base
        spidir_value_t vtable_base = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
            spidir_builder_build_ptroff(builder, from_value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable))));

        // add to it the new base
        vtable_base = spidir_builder_build_ptroff(builder, vtable_base,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, sizeof(void*) * offset));

        // and now store the new one
        spidir_builder_build_store(builder, SPIDIR_MEM_SIZE_8, vtable_base,
            spidir_builder_build_ptroff(builder, from_value,
                spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable))));

        return from_value;
    }

    // TODO: box a delegate type
    // NOTE: unboxing a delegate requires a cast, so that is handled automatically

    return from_value;
}

//----------------------------------------------------------------------------------------------------------------------
// Actual emitting
//----------------------------------------------------------------------------------------------------------------------

#define SWAP(a, b) \
    ({ \
        typeof(a) __tmp = a; \
        a = b; \
        b = __tmp; \
    })

#define EVAL_STACK_PUSH(_type, _value, ...) \
    do { \
        CHECK(arrlen(stack) < body->MaxStackSize); \
        jit_value_t __item = { .type = _type, .value = _value, ## __VA_ARGS__ }; \
        arrpush(stack, __item); \
    } while (0)

#define EVAL_STACK_POP() \
    ({ \
        CHECK(arrlen(stack) > 0); \
        arrpop(stack); \
    })

static long get_leave_target(uint32_t* leave_target_stack) {
    if (leave_target_stack == NULL) {
        return -1;
    }
    return arrlast(leave_target_stack);
}

static spidir_value_t jit_sign_extend_i32(spidir_builder_handle_t builder, spidir_value_t value, bool signext) {
    value = spidir_builder_build_iext(builder, value);
    if (signext) {
        value = spidir_builder_build_and(builder, value,
            spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, 0xFFFFFFFF));
    } else {
        value = spidir_builder_build_sfill(builder, 32, value);
    }
    return value;
}

static tdn_err_t jit_emit_basic_block(jit_method_t* jmethod, spidir_builder_handle_t builder, jit_basic_block_t* block) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = jmethod->method;
    RuntimeMethodBody body = method->MethodBody;

    // the context
    jit_value_t* stack = NULL;
    jit_value_t* locals = NULL;
    jit_value_t* args = NULL;
    spidir_value_t* spidir_args = NULL;
    RuntimeTypeInfo this_type = NULL;

    // figure the this type if this is a non-static method
    if (!method->Attributes.Static) {
        this_type = jmethod->args[0].type;
    }

    // ensure that the block we are seeing is already initialized
    // and copy over all the arrays
    CHECK(block->initialized);

    // set the spidir block to the current block
    spidir_builder_set_block(builder, block->block);

    // copy the locals state
    arrsetlen(stack, arrlen(block->stack));
    memcpy(stack, block->stack, arrlen(stack) * sizeof(*stack));
    arrsetlen(locals, arrlen(block->locals));
    memcpy(locals, block->locals, arrlen(locals) * sizeof(*locals));
    arrsetlen(args, arrlen(block->args));
    memcpy(args, block->args, arrlen(args) * sizeof(*args));

#ifdef JIT_VERBOSE_EMIT
    int indent = 0;
#endif

    // get the pc
    tdn_il_inst_t inst = { .control_flow = TDN_IL_CF_FIRST };
    uint32_t pc = block->start;
    while (pc < block->end) {
        // get the instruction
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));

#ifdef JIT_VERBOSE_VERIFY
        indent = tdn_disasm_print_start(body, pc, inst, indent);
#endif

        tdn_normalize_inst(&inst);
        uint32_t current_pc = pc;
        pc += inst.length;

        switch (inst.opcode) {

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arithmetic
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_CEQ:
            case CEE_CGT:
            case CEE_CGT_UN:
            case CEE_CLT:
            case CEE_CLT_UN: {
                jit_value_t value2 = EVAL_STACK_POP();
                jit_value_t value1 = EVAL_STACK_POP();

                spidir_value_t val1 = value1.value, val2 = value2.value;
                spidir_icmp_kind_t kind;
                switch (inst.opcode) {
                    case CEE_CEQ: kind = SPIDIR_ICMP_EQ; break;
                    case CEE_CGT: SWAP(val1, val2); kind = SPIDIR_ICMP_SLT; break;
                    case CEE_CGT_UN: SWAP(val1, val2); kind = SPIDIR_ICMP_ULT; break;
                    case CEE_CLT: kind = SPIDIR_ICMP_SLT; break;
                    case CEE_CLT_UN: kind = SPIDIR_ICMP_ULT; break;
                    default: CHECK_FAIL();
                }

                spidir_value_t result = spidir_builder_build_icmp(builder, kind, SPIDIR_TYPE_I32, val1, val2);
                EVAL_STACK_PUSH(tInt32, result);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Stack manipulation
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDNULL: {
                EVAL_STACK_PUSH(NULL, spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0));
            } break;

            case CEE_LDC_I4: {
                EVAL_STACK_PUSH(tInt32, spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, inst.operand.uint32));
            } break;

            case CEE_LDC_I8: {
                EVAL_STACK_PUSH(tInt64, spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, inst.operand.uint64));
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Arguments
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDARG: {
                CHECK(inst.operand.variable < arrlen(args));
                jit_value_t* arg = &args[inst.operand.variable];
                RuntimeTypeInfo type = jit_get_intermediate_type(arg->type);

                if (arg->attrs.spilled) {
                    EVAL_STACK_PUSH(type, jit_emit_load(builder, arg->type, arg->value), .attrs = arg->attrs);
                } else {
                    EVAL_STACK_PUSH(type, arg->value, .attrs = arg->attrs);
                }
            } break;

            case CEE_STARG: {
                CHECK(inst.operand.variable < arrlen(args));
                jit_value_t* arg = &args[inst.operand.variable];
                jit_value_t value = EVAL_STACK_POP();

                if (arg->attrs.spilled) {
                    jit_emit_store(builder, value.type, value.value, arg->type, arg->value);
                    arg->attrs = value.attrs;
                    arg->attrs.spilled = true;
                } else {
                    arg->value = value.value;
                    arg->attrs = value.attrs;
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Locals
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDLOC: {
                CHECK(inst.operand.variable < arrlen(locals));
                jit_value_t* arg = &locals[inst.operand.variable];
                RuntimeTypeInfo type = jit_get_intermediate_type(arg->type);

                if (arg->attrs.spilled) {
                    EVAL_STACK_PUSH(type, jit_emit_load(builder, arg->type, arg->value), .attrs = arg->attrs);
                } else {
                    EVAL_STACK_PUSH(type, arg->value, .attrs = arg->attrs);
                }
            } break;

            case CEE_STLOC: {
                CHECK(inst.operand.variable < arrlen(locals));
                jit_value_t* local = &locals[inst.operand.variable];
                jit_value_t value = EVAL_STACK_POP();

                if (local->attrs.spilled) {
                    jit_emit_store(builder, value.type, value.value, local->type, local->value);
                    local->attrs = value.attrs;
                    local->attrs.spilled = true;
                } else {
                    local->value = value.value;
                    local->attrs = value.attrs;
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Field access
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LDFLD: {
                jit_value_t value = EVAL_STACK_POP();
                RuntimeFieldInfo field = inst.operand.field;

                // get the pointe rto the field
                spidir_value_t field_ptr = SPIDIR_VALUE_INVALID;
                if (field->Attributes.Static) {
                    // TODO: use a global
                    CHECK_FAIL();
                } else {
                    field_ptr = spidir_builder_build_ptroff(builder, value.value,
                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, field->FieldOffset));
                }

                // and load it
                spidir_value_t field_value = jit_emit_load(builder, field->FieldType, field_ptr);
                RuntimeTypeInfo stack_type = jit_get_intermediate_type(field->FieldType);
                EVAL_STACK_PUSH(stack_type, field_value);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Calls
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_CALL:
            case CEE_CALLVIRT: {
                RuntimeMethodBase callee = inst.operand.method;
                RuntimeTypeInfo callee_this = NULL;

                if (!callee->Attributes.Static) {
                    callee_this = method->DeclaringType;
                    if (tdn_type_is_valuetype(callee_this)) {
                        CHECK_AND_RETHROW(tdn_get_byref_type(callee_this, &callee_this));
                    }
                }

                // check if we need an explicit null check
                bool explicit_null_check = false;
                if (inst.opcode == CEE_CALLVIRT && callee->Attributes.Static) {
                    explicit_null_check = true;
                }

                // add the parameters, we are also going to perform the pointer conversion as required
                // for the types
                for (int i = callee->Parameters->Length - 1; i >= 0; i--) {
                    jit_value_t value = EVAL_STACK_POP();
                    arrins(spidir_args, 0, jit_convert_pointer(builder,
                        value.type, value.value,
                        callee->Parameters->Elements[i]->ParameterType)
                    );
                }

                // add the this type
                spidir_value_t func_ptr = SPIDIR_VALUE_INVALID;
                if (callee_this != NULL) {
                    jit_value_t value = EVAL_STACK_POP();

                    if (jit_is_interface(value.type)) {
                        // we have an interface on the stack, get the instance from it
                        STATIC_ASSERT(offsetof(Interface, Instance) == 0);
                        spidir_value_t instance = spidir_builder_build_load(
                            builder,
                            SPIDIR_MEM_SIZE_8,
                            SPIDIR_TYPE_PTR,
                            value.value
                        );
                        arrins(spidir_args, 0, instance);
                    } else {
                        // normal object/reference
                        arrins(spidir_args, 0, value.value);
                    }

                    // now that we have the instance perform the de-virt, if we are
                    // successful then replace the callvirt with a normal call
                    if (inst.opcode == CEE_CALLVIRT) {
                        RuntimeMethodBase new_callee = jit_devirt_method(value, callee);
                        if (new_callee != callee) {
                            // needs an explicit null check now that we don't access the
                            // the vtable anymore
                            explicit_null_check = true;
                            callee = new_callee;
                            inst.opcode = CEE_CALL;
                        } else {
                            // not de-virtualized, resolve the function address

                            // load the vtable of the object on the stack
                            spidir_value_t vtable_base = SPIDIR_VALUE_INVALID;
                            if (jit_is_interface(value.type)) {
                                vtable_base = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
                                    spidir_builder_build_ptroff(builder, value.value,
                                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(Interface, VTable))));
                            } else {
                                vtable_base = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR, value.value);
                            }

                            // now figure the offset of the function in the interface
                            size_t base_offset = sizeof(void*) + callee->VTableOffset;
                            if (jit_is_interface(callee_this) && !jit_is_interface(value.type)) {
                                // calling an interface method on an object, adjust the base offset to
                                // represent the offset to the iface inside of the object's vtable
                                size_t iface_offset = jit_get_interface_offset(value.type, callee_this);
                                ASSERT(iface_offset != -1);
                                base_offset += iface_offset * sizeof(void*);
                                base_offset += offsetof(ObjectVTable, Functions);
                            }

                            // and now load the function pointer
                            func_ptr = spidir_builder_build_load(builder, SPIDIR_MEM_SIZE_8, SPIDIR_TYPE_PTR,
                                spidir_builder_build_ptroff(builder, vtable_base,
                                    spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, base_offset)));
                        }
                    }
                }

                // get the ret type
                RuntimeTypeInfo ret_type = callee->ReturnParameter->ParameterType;
                spidir_value_t return_value = SPIDIR_VALUE_INVALID;

                // if the return type is a struct like, then we need to allocate an implicit
                // argument for it
                if (ret_type != tVoid && jit_is_struct_like(ret_type)) {
                    return_value = spidir_builder_build_stackslot(builder, ret_type->StackSize, ret_type->StackAlignment);
                    arrpush(spidir_args, return_value);
                    EVAL_STACK_PUSH(ret_type, return_value);
                }

                if (inst.opcode == CEE_CALLVIRT) {
                    // perform an indirect call
                    spidir_value_type_t* arg_types = jit_get_spidir_arg_types(callee);
                    CHECK(arg_types != NULL);
                    return_value = spidir_builder_build_callind(builder,
                        jit_get_spidir_ret_type(callee),
                        arrlen(arg_types),
                        arg_types,
                        func_ptr,
                        spidir_args
                    );
                    arrfree(arg_types);

                } else {
                    // attempt to call as a builtin
                    jit_builtin_emitter_t emitter = jit_get_builtin_emitter(callee);
                    if (emitter != NULL) {
                        return_value = emitter(builder, callee, spidir_args);

                    } else if (jit_emit_should_inline(jmethod, callee)) {
                        // we should perform an inline of the new function
                        CHECK_AND_RETHROW(jit_perform_inline(
                            builder, jmethod,
                            spidir_args,
                            callee,
                            &return_value
                        ));

                        // this is kinda ugly but we need to actually replace the
                        // result on the top of the stack
                        if (ret_type != tVoid && jit_is_struct_like(ret_type)) {
                            arrlast(stack).value = return_value;
                        }

                    } else {
                        // nothing special to be done, just queue to emit it
                        CHECK_AND_RETHROW(jit_queue_emit_method(spidir_builder_get_module(builder), callee));

                        // get the jit function for it
                        jit_method_t* jit_callee = NULL;
                        CHECK_AND_RETHROW(jit_get_method(callee, &jit_callee));

                        // and now emit the call itself
                        return_value = spidir_builder_build_call(
                            builder,
                            jit_callee->function,
                            arrlen(spidir_args),
                            spidir_args
                        );
                    }
                }

                // handle primitive return types
                if (ret_type != tVoid && !jit_is_struct_like(ret_type)) {
                    EVAL_STACK_PUSH(ret_type, return_value);
                }
            } break;

            case CEE_RET: {
                RuntimeTypeInfo type = method->ReturnParameter->ParameterType;

                jit_value_t ret_value = { .value = SPIDIR_VALUE_INVALID };
                if (type != tVoid) {
                    ret_value = EVAL_STACK_POP();
                }

                if (jmethod->inline_level) {
                    // we are inside an inline, add output to the phi
                    // and branch to the continuation, this also works
                    // for when we return a struct-like, the stack local
                    // is just used
                    if (type != tVoid) {
                        spidir_builder_add_phi_input(builder, jmethod->inline_return_phi, ret_value.value);
                    }
                    spidir_builder_build_branch(builder, jmethod->inline_return_block);

                } else if (type != tVoid) {
                    if (jit_is_struct_like(type)) {
                        // this is a struct like, copy into the return reference
                        jit_emit_store(builder, ret_value.type, ret_value.value, type, jmethod->return_ref);
                        spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
                    } else {
                        // just a scalar, return it
                        spidir_builder_build_return(builder, ret_value.value);
                    }
                }

                CHECK(arrlen(stack) == 0);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Control flow
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_BR: {
                spidir_block_t new_block;
                CHECK_AND_RETHROW(jit_emit_merge_basic_block(
                    jmethod, builder,
                    inst.operand.branch_target, &new_block,
                    stack, locals, args,
                    get_leave_target(block->leave_target_stack)));
                spidir_builder_build_branch(builder, new_block);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Exceptions
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_THROW: {
                jit_value_t obj = EVAL_STACK_POP();

                // call the helper to throw
                // TODO: turn to control flow whenever possible?
                spidir_builder_build_call(builder,
                    jit_helper_get(spidir_builder_get_module(builder), JIT_HELPER_THROW),
                    1,
                    (spidir_value_t[]){ obj.value }
                );

                // can't be reached
                spidir_builder_build_unreachable(builder);

                // stack should be empty now
                arrsetlen(stack, 0);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Misc
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_NOP: {
            } break;

            default: CHECK_FAIL("Unknown opcode `%s`", tdn_get_opcode_name(inst.opcode));
        }

#ifdef JIT_VERBOSE_VERIFY
        indent = tdn_disasm_print_end(body, pc, indent);
#endif

        arrfree(spidir_args);
    }

    // we have a fallthrough
    if (inst.control_flow == TDN_IL_CF_NEXT || inst.control_flow == TDN_IL_CF_CALL) {
        spidir_block_t new_block;
        CHECK_AND_RETHROW(jit_emit_merge_basic_block(
            jmethod,
            builder,
            pc, &new_block,
            stack, locals, args,
            get_leave_target(block->leave_target_stack)));

        // branch into this block
        spidir_builder_build_branch(builder, new_block);
    }

    // last must be a valid instruction
    CHECK(
        inst.control_flow != TDN_IL_CF_FIRST &&
        inst.control_flow != TDN_IL_CF_META
    );

cleanup:
    arrfree(spidir_args);
    arrfree(stack);
    arrfree(locals);
    arrfree(args);

    return err;
}

static tdn_err_t jit_emit_prepare_method(jit_method_t* jmethod, spidir_value_t* args, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = jmethod->method;
    RuntimeMethodBody body = method->MethodBody;

    // setup the main entry block
    if (!jmethod->inline_level) {
        spidir_block_t entry_block = spidir_builder_create_block(builder);
        spidir_builder_set_block(builder, entry_block);
        spidir_builder_set_entry_block(builder, entry_block);
    }

    // if we have a this add it to the local
    int param_i = 0;
    if (!method->Attributes.Static) {
        if (jmethod->args[param_i].attrs.spilled) {
            // spilled, means we need to copy it to a local
            // and store it in there
            CHECK_FAIL();
        } else {
            if (args != NULL) {
                jmethod->args[param_i].value = args[param_i];
            } else {
                jmethod->args[param_i].value = spidir_builder_build_param_ref(builder, param_i);
            }
        }
        param_i++;
    }

    // now prepare the rest of the arguments
    for (int i = 0; i < method->Parameters->Length; i++) {
        jit_value_t* arg = &jmethod->args[i];

        spidir_value_t arg_value;
        if (args != NULL) {
            arg_value = args[param_i];
        } else {
            arg_value = spidir_builder_build_param_ref(builder, param_i);
        }

        if (arg->attrs.spilled) {
            // spilled, means we need to copy it to a stackslot and store it in there
            arg->value = spidir_builder_build_stackslot(builder, arg->type->StackSize, arg->type->StackAlignment);
            jit_emit_store(builder, arg->type, arg_value, arg->type, arg->value);
        } else {
            arg->value = arg_value;
        }

        param_i++;
    }

    if (args == NULL) {
        RuntimeTypeInfo ret_type = method->ReturnParameter->ParameterType;
        if (ret_type != tVoid && jit_is_struct_like(ret_type)) {
            jmethod->return_ref = spidir_builder_build_param_ref(builder, param_i);
            param_i++;
        }
    }

    // and now prepare the locals
    if (body->LocalVariables != NULL) {
        for (int i = 0; i < body->LocalVariables->Length; i++) {
            jit_value_t* local = &jmethod->locals[i];

            if (local->attrs.spilled || jit_is_struct_like(local->type)) {
                // the local needs to be stored in a stack slot
                local->value = spidir_builder_build_stackslot(builder, local->type->StackSize, local->type->StackAlignment);

                // if we need to initialize it emit a bzero
                if (local->attrs.needs_init) {
                    jit_emit_bzero(builder, local->value, local->type);
                }

            } else {
                // non-spilled local and not a struct, initialize to either
                // an invalid value or to zero
                if (local->attrs.needs_init) {
                    local->value = spidir_builder_build_iconst(builder, jit_get_spidir_type(local->type), 0);
                } else {
                    local->value = SPIDIR_VALUE_INVALID;
                }
            }
        }
    }

cleanup:
    return err;
}

static tdn_err_t jit_emit_method(jit_method_t* method, spidir_value_t* args, spidir_builder_handle_t builder) {
    tdn_err_t err = TDN_NO_ERROR;

#ifdef JIT_DEBUG_EMIT
    TRACE("EMIT: %T::%U", method->method->DeclaringType, method->method->Name);
#endif

    // prepare the method, setting up the initial locals, arguments and so on, the
    // args come from the spidir function itself
    CHECK_AND_RETHROW(jit_emit_prepare_method(method, args, builder));

    // "merge" with the first block, giving it the initial pc for everything
    spidir_block_t il_entry_block;
    CHECK_AND_RETHROW(jit_emit_merge_basic_block(
        method,
        builder,
        0, &il_entry_block,
        NULL, method->locals, method->args,
        -1)
    );

    // perform a branch from the entry block to this block
    spidir_builder_build_branch(builder, il_entry_block);

    while (arrlen(method->block_queue) > 0) {
        jit_basic_block_t* block = arrpop(method->block_queue);

#ifdef JIT_VERBOSE_EMIT
        TRACE("\tBlock (IL_%04x)", block->start);
#endif
        CHECK_AND_RETHROW(jit_emit_basic_block(method, builder, block));
    }

cleanup:
    arrfree(method->block_queue);

    return err;
}

typedef struct jit_emit_context {
    tdn_err_t err;
    jit_method_t* method;
} jit_emit_context_t;

static void jit_emit_il(spidir_builder_handle_t builder, void* _ctx) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_emit_context_t* ctx = _ctx;

    CHECK_AND_RETHROW(jit_emit_method(ctx->method, NULL, builder));

cleanup:
    ctx->err = err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Emit management code
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static jit_method_t** m_methods_to_emit = NULL;

tdn_err_t jit_init_emit() {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: stuff

cleanup:
    return err;
}

static spidir_function_t create_spidir_function(spidir_module_handle_t module, RuntimeMethodBase method, bool external) {
    // build the name
    string_builder_t builder = {};
    string_builder_push_method_signature(&builder, method, true);
    const char* name = string_builder_build(&builder);

    // get the signature
    spidir_value_type_t ret_type = jit_get_spidir_ret_type(method);
    spidir_value_type_t* arg_types = jit_get_spidir_arg_types(method);

    spidir_function_t function;
    if (external) {
        function = spidir_module_create_extern_function(module, name, ret_type, arrlen(arg_types), arg_types);
    } else {
        function = spidir_module_create_function(module, name, ret_type, arrlen(arg_types), arg_types);
    }

    arrfree(arg_types);
    string_builder_free(&builder);

    return function;
}


tdn_err_t jit_queue_emit_method(spidir_module_handle_t module, RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_method_t* jit_method = NULL;
    CHECK_AND_RETHROW(jit_get_method(method, &jit_method));

    // if the method was not emitted yet
    if (!jit_method->emitting) {
        if (method->MethodPtr != NULL) {
            // already emitted before, use the extern to it
            jit_method->function = create_spidir_function(module, method, true);

        } else {
            // normal/builtin function, create the method so it can be emitted
            jit_method->function = create_spidir_function(module, method, false);
            arrpush(m_methods_to_emit, jit_method);
        }

        jit_method->emitting = true;
    }

cleanup:
    return err;
}

static tdn_err_t jit_emit_all(spidir_module_handle_t module) {
    tdn_err_t err = TDN_NO_ERROR;

    while (arrlen(m_methods_to_emit) != 0) {
        jit_method_t* method = arrpop(m_methods_to_emit);

        // check if we need to verify it
        if (!method->verified && method->method->MethodBody != NULL) {
            CHECK_AND_RETHROW(jit_verify_prepare_method(method));
            CHECK_AND_RETHROW(jit_verify_method(method));
            method->verified = true;
        }

        // should not get something that is already jitted
        CHECK(method->method->MethodPtr == NULL);

        // and now we can do the real emitting
        if (method->method->MethodBody == NULL) {
            // builtin method, use the builtin emitter
            jit_builtin_context_t ctx = {
                .method = method->method,
                .err = TDN_NO_ERROR
            };
            spidir_module_build_function(module, method->function, jit_emit_builtin, &ctx);
            CHECK_AND_RETHROW(ctx.err);
        } else {
            // normal IL method
            jit_emit_context_t ctx = {
                .method = method,
                .err = TDN_NO_ERROR
            };
            spidir_module_build_function(module, method->function, jit_emit_il, &ctx);
            CHECK_AND_RETHROW(ctx.err);
        }
    }

cleanup:
    return err;
}

tdn_err_t jit_emit(spidir_module_handle_t module) {
    tdn_err_t err = TDN_NO_ERROR;

    // emit everything
    CHECK_AND_RETHROW(jit_emit_all(module));

    // perform optimizations
#ifndef JIT_DISABLE_OPTIMIZATIONS
    spidir_opt_run(module);
#endif

    // TODO: code gen and optimizations and stuff

#ifdef JIT_DUMP_EMIT
    void* ctx = tdn_host_jit_start_dump();
    spidir_module_dump(module, tdn_host_jit_dump_callback, ctx);
    tdn_host_jit_end_dump(ctx);
#endif

cleanup:
    return err;
}

void jit_emit_clean(void) {
    arrfree(m_methods_to_emit);
}
