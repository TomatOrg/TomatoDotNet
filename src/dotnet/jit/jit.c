#include "tomatodotnet/types/type.h"
#include "tomatodotnet/jit/jit.h"
#include "tomatodotnet/disasm.h"

#include "util/except.h"
#include "util/stb_ds.h"
#include "util/string_builder.h"
#include "jit_internal.h"

#include <stdbool.h>

#include "dotnet/gc/gc.h"
#include "util/string.h"
#include "dotnet/metadata/metadata_tables.h"

//
// If you want to output the instructions that we see as we see them
// useful for debugging
//
// #define JIT_IL_OUTPUT

//
// If the jit does not support SEH like
// exceptions then we will need to handle
// it with
//
// same
//
#ifdef __JIT_LLVM__
    #define __EXPLICIT_NULL_CHECK__
    #define __EXPLICIT_ZERO_CHECK__
#endif

/**
 * The jit module, DON'T USE FROM WITHIN THE BUILDER
 */
static jit_module_t m_jit_module;

/**
 * Stack of methods that we need to jit and are already
 * prepared
 */
static RuntimeMethodBase* m_methods_to_jit = NULL;

//
// builtin functions
//

// memory manipulation (mainly used for structs)
static jit_function_t m_builtin_bzero;
static jit_function_t m_builtin_memcpy;

// GC related
static jit_function_t m_builtin_gc_new;
static jit_function_t m_builtin_gc_bzero;
static jit_function_t m_builtin_gc_memcpy;

// throwing related
static jit_function_t m_builtin_exceptions[JIT_EXCEPTION_MAX];
static jit_function_t m_builtin_throw;

// exception control flow related
static jit_function_t m_builtin_rethrow;
static jit_function_t m_builtin_get_exception;

tdn_err_t tdn_jit_init() {
    tdn_err_t err = TDN_NO_ERROR;

    // create the dummy null type
    tNull = GC_NEW(RuntimeTypeInfo);
    CHECK_AND_RETHROW(tdn_create_string_from_cstr("<null>", &tNull->Name));

    CHECK_AND_RETHROW(jit_init());

    m_jit_module = jit_module_create();

    //
    // Create built-in types
    //
    m_builtin_bzero = jit_module_create_extern_function(m_jit_module,
            "bzero",
            JIT_TYPE_NONE,
            2, (jit_value_type_t[]){ JIT_TYPE_PTR, JIT_TYPE_I64 });

    m_builtin_memcpy = jit_module_create_extern_function(m_jit_module,
            "memcpy",
            JIT_TYPE_NONE,
            3, (jit_value_type_t[]){ JIT_TYPE_PTR, JIT_TYPE_PTR, JIT_TYPE_I64 });

    m_builtin_gc_new = jit_module_create_extern_function(m_jit_module,
            "gc_new",
            JIT_TYPE_PTR,
            2, (jit_value_type_t[]){ JIT_TYPE_PTR, JIT_TYPE_I64 });

    m_builtin_gc_memcpy = jit_module_create_extern_function(m_jit_module,
            "gc_memcpy",
            JIT_TYPE_NONE,
            3, (jit_value_type_t[]){ JIT_TYPE_PTR, JIT_TYPE_PTR, JIT_TYPE_PTR });

    m_builtin_gc_bzero = jit_module_create_extern_function(m_jit_module,
            "gc_bzero",
            JIT_TYPE_NONE,
            2, (jit_value_type_t[]){ JIT_TYPE_PTR, JIT_TYPE_PTR });

    m_builtin_exceptions[JIT_EXCEPTION_INVALID_CAST] = jit_module_create_extern_function(m_jit_module,
           "throw_invalid_cast_exception",
           JIT_TYPE_NONE,
           0, NULL);

    m_builtin_exceptions[JIT_EXCEPTION_INDEX_OUT_OF_RANGE] = jit_module_create_extern_function(m_jit_module,
            "throw_index_out_of_range_exception",
            JIT_TYPE_NONE,
            0, NULL);

    m_builtin_exceptions[JIT_EXCEPTION_OVERFLOW] = jit_module_create_extern_function(m_jit_module,
            "throw_overflow_exception",
            JIT_TYPE_NONE,
            0, NULL);

    m_builtin_exceptions[JIT_EXCEPTION_NULL_REFERENCE] = jit_module_create_extern_function(m_jit_module,
            "throw_null_reference_exception",
            JIT_TYPE_NONE,
            0, NULL);

    m_builtin_exceptions[JIT_EXCEPTION_DIVIDE_BY_ZERO] = jit_module_create_extern_function(m_jit_module,
             "throw_divide_by_zero_exception",
             JIT_TYPE_NONE,
             0, NULL);

    m_builtin_throw = jit_module_create_extern_function(m_jit_module,
                "throw",
                JIT_TYPE_NONE,
                1, (jit_value_type_t[]){ JIT_TYPE_PTR });

    m_builtin_rethrow = jit_module_create_extern_function(m_jit_module,
                "rethrow",
                JIT_TYPE_NONE,
                0, NULL);

    m_builtin_get_exception = jit_module_create_extern_function(m_jit_module,
                "get_exception",
                JIT_TYPE_PTR,
                0, NULL);

    // link the builtins module
    jit_link_module(m_jit_module);
    m_jit_module = NULL;

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// JIT helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static jit_value_type_t get_jit_argument_type(RuntimeTypeInfo type) {
    if (
        type == tSByte || type == tByte ||
        type == tInt16 || type == tUInt16 ||
        type == tInt32 || type == tUInt32 ||
        type == tBoolean
    ) {
        return JIT_TYPE_I32;
    } else if (type->BaseType == tEnum) {
        return get_jit_argument_type(type->EnumUnderlyingType);
    } else if (
        type == tInt64 || type == tUInt64 ||
        type == tIntPtr || type == tUIntPtr
    ) {
        return JIT_TYPE_I64;
    } else if (type == tVoid) {
        return JIT_TYPE_NONE;
    } else if (tdn_type_is_valuetype(type)) {
        // pass by-reference, copied by the caller
        return JIT_TYPE_PTR;
    } else {
        // this is def a pointer
        return JIT_TYPE_PTR;
    }
}


static jit_value_type_t get_jit_return_type(RuntimeTypeInfo type) {
    if (
        type == tSByte || type == tByte ||
        type == tInt16 || type == tUInt16 ||
        type == tInt32 || type == tUInt32 ||
        type == tBoolean
    ) {
        return JIT_TYPE_I32;
    } else if (type->BaseType == tEnum) {
        return get_jit_return_type(type->EnumUnderlyingType);
    } else if (
        type == tInt64 || type == tUInt64 ||
        type == tIntPtr || type == tUIntPtr
    ) {
        return JIT_TYPE_I64;
    } else if (type == tVoid) {
        return JIT_TYPE_NONE;
    } else if (tdn_type_is_valuetype(type)) {
        // argument is passed by reference
        return JIT_TYPE_NONE;
    } else {
        // this is def a pointer
        return JIT_TYPE_PTR;
    }
}

static jit_mem_size_t get_jit_mem_size(RuntimeTypeInfo info) {
    switch (info->StackSize) {
        case 1: return JIT_MEM_SIZE_1;
        case 2: return JIT_MEM_SIZE_2;
        case 4: return JIT_MEM_SIZE_4;
        case 8: return JIT_MEM_SIZE_8;
        default: ASSERT(!"get_jit_mem_size: invalid memory type");
    }
}

static jit_value_type_t get_jit_mem_type(RuntimeTypeInfo info) {
    info = tdn_get_intermediate_type(info);
    if (info == tInt32) {
        return JIT_TYPE_I32;
    } else if (info == tInt64 || info == tIntPtr) {
        return JIT_TYPE_I64;
    } else if (tdn_type_is_referencetype(info)) {
        return JIT_TYPE_PTR;
    } else {
        ASSERT(!"get_jit_mem_type: invalid memory type");
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit prepare helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static tdn_err_t jit_prepare_method(RuntimeMethodBase method, jit_module_t module);

static void register_struct_fields(RuntimeTypeInfo type, void* base) {
    // go over the struct fields and register them
    for (int i = 0; i < type->DeclaredFields->Length; i++) {
        RuntimeFieldInfo field = type->DeclaredFields->Elements[i];
        if (field->Attributes.Static) continue;

        if (jit_is_struct_type(type)) {
            // if its a struct recurse
            register_struct_fields(field->FieldType, base + field->FieldOffset);
        } else if (tdn_type_is_referencetype(type)) {
            // if its a reference register it right away
            gc_register_root(base + field->FieldOffset);
        }
    }
}

/**
 * Prepares the static part of a type, mostly to do with the static fields and a type
 * ctor if required
 *
 * TODO: maybe just jit it all so we can mark private methods as private since we know for sure no one else
 *       will need to handle them
 *
 * TODO: maybe I should instead allocate the thing on demand
 */
static tdn_err_t jit_prepare_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // check if already jitted
    if (type->JitStartedStatic) {
        goto cleanup;
    }
    type->JitStartedStatic = 1;

    // create the fields
    for (int i = 0; i < type->DeclaredFields->Length; i++) {
        RuntimeFieldInfo field = type->DeclaredFields->Elements[i];
        if (!field->Attributes.Static) continue;

        // TODO: handle field RVAs
        CHECK(!field->Attributes.HasFieldRVA);

        // allocate the field
        field->JitFieldPtr = tdn_host_mallocz(field->FieldType->StackSize);
        CHECK_ERROR(field->JitFieldPtr != NULL, TDN_ERROR_OUT_OF_MEMORY);

        // register gc fields
        if (jit_is_struct_type(type)) {
            register_struct_fields(field->FieldType, field->JitFieldPtr);
        } else if (tdn_type_is_referencetype(type)) {
            gc_register_root(field->JitFieldPtr);
        }
    }

    // TODO: register the type ctor

cleanup:
    return err;
}

/**
 * Prepares an instance of a type, this takes care of vtable and itable functions
 * that have to be generated to complete the object
 */
static tdn_err_t jit_prepare_type_instance(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // check if already jitted
    if (type->JitStartedInstance) {
        goto cleanup;
    }
    type->JitStartedInstance = 1;

    CHECK_AND_RETHROW(jit_prepare_type(type));

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Access checks
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static bool is_instance(RuntimeTypeInfo has, RuntimeTypeInfo want) {
    while (has != want) {
        if (has == tObject) {
            return false;
        }
        has = has->BaseType;
    }
    return true;
}

static bool check_type_access(RuntimeMethodBase method, RuntimeTypeInfo type, int token) {
    // can always access no matter what
    if (method->DeclaringType == type) {
        return true;
    }

    if (type->DeclaringType != NULL) {
        if (!check_type_access(method, type->DeclaringType, 0)) {
            return false;
        }
    }

    // top level stuff
    switch (type->Attributes.Visibility) {
        case TDN_TYPE_VISIBILITY_NOT_PUBLIC: {
            return method->Module == type->Module;
        } break;

        case TDN_TYPE_VISIBILITY_PUBLIC:
        case TDN_TYPE_VISIBILITY_NESTED_PUBLIC: {
            return true;
        } break;

        case TDN_TYPE_VISIBILITY_NESTED_PRIVATE: {
            return method->DeclaringType == type->DeclaringType;
        } break;

        case TDN_TYPE_VISIBILITY_NESTED_FAMILY: {
            return is_instance(method->DeclaringType, type->DeclaringType);
        } break;

        case TDN_TYPE_VISIBILITY_NESTED_ASSEMBLY: {
            return method->Module == type->Module;
        } break;

        case TDN_TYPE_VISIBILITY_NESTED_FAMILY_AND_ASSEMBLY: {
            return method->Module == type->Module && is_instance(method->DeclaringType, type->DeclaringType);
        } break;

        case TDN_TYPE_VISIBILITY_NESTED_FAMILY_OR_ASSEMBLY: {
            return method->Module == type->Module || is_instance(method->DeclaringType, type->DeclaringType);
        } break;

        default: {
            ASSERT(!"Invalid type access");
        } break;
    }
}

static bool check_field_access(RuntimeMethodBase method, RuntimeFieldInfo field, int token) {
    // check type accessibility first
    if (!check_type_access(method, field->DeclaringType, 0)) {
        ERROR("Failed to access type %T from %T:%U", field->DeclaringType, method->DeclaringType, method->Name);
        return false;
    }

    switch (field->Attributes.FieldAccess) {
        case TDN_FIELD_ACCESS_PRIVATE_SCOPE: {
            token_t tok = { .token = token };
            return method->Module == field->Module && tok.table == METADATA_FIELD;
        } break;

        case TDN_FIELD_ACCESS_PRIVATE: {
            return method->DeclaringType == field->DeclaringType;
        } break;

        case TDN_FIELD_ACCESS_FAMILY: {
            return is_instance(method->DeclaringType, field->DeclaringType);
        } break;

        case TDN_FIELD_ACCESS_ASSEMBLY: {
            return method->Module == field->Module;
        } break;

        case TDN_FIELD_ACCESS_FAMILY_AND_ASSEMBLY: {
            return method->Module == field->Module && is_instance(method->DeclaringType, field->DeclaringType);
        } break;

        case TDN_FIELD_ACCESS_FAMILY_OR_ASSEMBLY: {
            return method->Module == field->Module || is_instance(method->DeclaringType, field->DeclaringType);
        } break;

        case TDN_FIELD_ACCESS_PUBLIC: {
            return true;
        } break;

        default:
            ASSERT(!"Invalid field access");
    }
}

static bool check_method_access(RuntimeMethodBase method, RuntimeMethodBase target, int token) {
    // check type accessibility first
    if (!check_type_access(method, target->DeclaringType, 0)) {
        return false;
    }

    switch (target->Attributes.MemberAccess) {
        case TDN_METHOD_ACCESS_PRIVATE_SCOPE: {
            token_t tok = { .token = token };
            return method->Module == target->Module && tok.table == METADATA_METHOD_DEF;
        } break;

        case TDN_METHOD_ACCESS_PRIVATE: {
            return method->DeclaringType == target->DeclaringType;
        } break;

        case TDN_METHOD_ACCESS_FAMILY: {
            return is_instance(method->DeclaringType, target->DeclaringType);
        } break;

        case TDN_METHOD_ACCESS_ASSEMBLY: {
            return method->Module == target->Module;
        } break;

        case TDN_METHOD_ACCESS_FAMILY_AND_ASSEMBLY: {
            return method->Module == target->Module && is_instance(method->DeclaringType, target->DeclaringType);
        } break;

        case TDN_METHOD_ACCESS_FAMILY_OR_ASSEMBLY: {
            return method->Module == target->Module || is_instance(method->DeclaringType, target->DeclaringType);
        } break;

        case TDN_METHOD_ACCESS_PUBLIC: {
            return true;
        } break;

        default:
            ASSERT(!"Invalid method access");
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Jit helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define SWAP(a, b) \
    do { \
        typeof(a) __temp = a; \
        a = b; \
        b = __temp; \
    } while (0)

/**
 * Emit a memcpy with a known size
 */
static void jit_emit_memcpy(jit_builder_t builder, jit_value_t ptr1, jit_value_t ptr2, size_t size) {
    // TODO:
    jit_builder_build_call(builder, m_builtin_memcpy, 3,
                              (jit_value_t[]){
                                      ptr1, ptr2,
                                      jit_builder_build_iconst(builder, JIT_TYPE_I64, size)
                              });
}

static void jit_emit_gc_memcpy(jit_builder_t builder, RuntimeTypeInfo type, jit_value_t ptr1, jit_value_t ptr2) {
    // TODO: if struct is small enough inline it manually
    jit_builder_build_call(builder, m_builtin_gc_memcpy, 3,
                              (jit_value_t[]){
                                      jit_builder_build_iconst(builder, JIT_TYPE_PTR, (uint64_t)type),
                                      ptr1, ptr2,
                              });
}

/**
 * Resolve the parameter type, taking into account that for non-static arg0 is the this
 */
static tdn_err_t jit_resolve_parameter_type(RuntimeMethodBase method, int arg, RuntimeTypeInfo* type, ParameterAttributes* attributes) {
    tdn_err_t err = TDN_NO_ERROR;

    // resolve the argument type
    RuntimeTypeInfo arg_type = NULL;
    if (arg == 0 && !method->Attributes.Static) {
        // non-static method's first arg is the this
        if (tdn_type_is_valuetype(method->DeclaringType)) {
            CHECK_AND_RETHROW(tdn_get_byref_type(method->DeclaringType, &arg_type));
        } else {
            arg_type = method->DeclaringType;
        }

    } else {
        // this is not included in Parameters list
        uint16_t param_arg = arg - (method->Attributes.Static ? 0 : 1);

        // get the correct argument
        CHECK(param_arg < method->Parameters->Length);
        arg_type = method->Parameters->Elements[param_arg]->ParameterType;
        *attributes = method->Parameters->Elements[param_arg]->Attributes;
    }

    // return it
    *type = arg_type;

cleanup:
    return err;
}

/**
 * Create the labels of all the jump locations in the given region
 */
static tdn_err_t create_region_labels(jit_context_t* ctx, jit_region_t* region) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(region->labels == NULL);

    // get the index ranges
    int start_index = jit_get_label_location_index(ctx, region->pc_start, false);
    int end_index = jit_get_label_location_index(ctx, region->pc_end, false);
    arrsetcap(region->labels, end_index - start_index);

    // get all the labels
    for (int i = start_index; i < end_index; i++) {
        jit_label_location_t* location = &ctx->labels[i];
        if (location->created) {
            continue;
        }
        location->created = true;

        jit_label_t label = {
            .block = jit_builder_create_block(ctx->builder),
            .needs_phi = location->needs_phi,
            .address = location->pc,
        };
        arrpush(region->labels, label);
    }

    // setup the entry block, if we have a label at the start of it then use that, otherwise
    // just create a new one
    if (arrlen(region->labels) > 0 && region->labels[0].address == region->pc_start) {
        region->entry_block = region->labels[0].block;
    } else {
        region->entry_block = jit_builder_create_block(ctx->builder);
    }

cleanup:
    return err;
}

/**
 * Resolve the label of a
 *
 */
static tdn_err_t resolve_and_verify_branch_target(
    jit_context_t* ctx,
    jit_region_t* region,
    uint32_t target,
    jit_label_t** out_label
) {
    tdn_err_t err = TDN_NO_ERROR;

    // if we have a branch target make sure we have the target label
    jit_label_t* target_label = jit_get_label(region, target);
    CHECK(target_label != NULL);

    // stack consistency check
    if (target_label->snapshot.initialized) {
        // we have a snapshot, perform a merge as needed, only modify if we have not visited it yet
        CHECK_AND_RETHROW(eval_stack_merge(ctx->builder, &ctx->stack, target_label, !target_label->visited));
    } else {
        // otherwise create a snapshot of our stack
        CHECK_AND_RETHROW(eval_stack_snapshot(ctx->builder, &ctx->stack, target_label));
    }

    // give it back
    *out_label = target_label;

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Exception handlers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static jit_block_t jit_throw_builtin_exception(jit_context_t* ctx, jit_builder_t builder, jit_builtin_exception_t exception) {
    if ((ctx->exceptions & (1 << exception)) == 0) {
        jit_block_t prev_block;
        ASSERT(jit_builder_cur_block(builder, &prev_block));

        // create the new block
        ctx->exception_blocks[exception] = jit_builder_create_block(builder);
        ctx->exceptions |= (1 << exception);

        // emit the throw
        jit_builder_set_block(builder, ctx->exception_blocks[exception]);
        jit_builder_build_call(builder, m_builtin_exceptions[exception], 0, NULL);
        jit_builder_build_unreachable(builder);

        // and go back to the original thing
        jit_builder_set_block(builder, prev_block);
    }

    // TODO: add another input to the phi

    // return the builtin exception
    return ctx->exception_blocks[exception];
}

static void jit_emit_null_check(jit_context_t* ctx, jit_builder_t builder, jit_value_t obj) {
    // create the null check
    jit_block_t valid = jit_builder_create_block(builder);
    jit_value_t null = jit_builder_build_iconst(builder, JIT_TYPE_PTR, 0);
    jit_value_t cmp = jit_builder_build_icmp(builder,
                                             JIT_ICMP_NE, JIT_TYPE_I64,
                                             null, obj);

    // get the throw block
    jit_block_t invalid = jit_throw_builtin_exception(ctx, builder, JIT_EXCEPTION_NULL_REFERENCE);

    // either jump to the throw or continue
    jit_builder_build_brcond(builder, cmp, valid, invalid);
    jit_builder_set_block(builder, valid);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Intrinsics
//
// These are emitted instead of function calls, so we can assume type
// checking was already done on the caller
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static tdn_err_t handle_unsafe_intrinsics(
    jit_builder_t builder,
    RuntimeMethodBase target,
    RuntimeTypeInfo* types, jit_value_t* args,
    jit_value_t* out
) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(target->ReturnParameter->ParameterType != tVoid);
    CHECK(!tdn_type_is_valuetype(target->ReturnParameter->ParameterType));

    if (tdn_compare_string_to_cstr(target->Name, "As")) {
        CHECK(arrlen(args) == 1);
        *out = args[0];

    } else if (tdn_compare_string_to_cstr(target->Name, "AsRef")) {
        CHECK(arrlen(args) == 1);
        *out = args[0];

    } else if (tdn_compare_string_to_cstr(target->Name, "Add")) {
        CHECK(arrlen(args) == 2);

        jit_value_t value = args[0];

        if (types[1] == tInt32) {
            // expand into a 64bit integer
            value = jit_builder_build_iext(builder, value);
            value = jit_builder_build_sfill(builder, 32, value);
        } else {
            CHECK(types[1] == tIntPtr || types[1] == tUIntPtr);
        }

        // calculate an offset
        size_t element_size = target->GenericArguments->Elements[0]->StackSize;
        jit_value_t offset = jit_builder_build_imul(builder, args[1],
                                                    jit_builder_build_iconst(builder, JIT_TYPE_I64, element_size));

        // add it to the pointer
        *out = jit_builder_build_ptroff(builder, value, offset);

    } else if (tdn_compare_string_to_cstr(target->Name, "AddByteOffset")) {
        CHECK(arrlen(args) == 2);

        // calculate an offset
        size_t element_size = target->GenericArguments->Elements[0]->StackSize;
        jit_value_t offset = jit_builder_build_imul(builder, args[1],
                                                    jit_builder_build_iconst(builder, JIT_TYPE_I64, element_size));

        // add it to the pointer
        *out = jit_builder_build_ptroff(builder, args[0], offset);

    } else if (tdn_compare_string_to_cstr(target->Name, "AreSame")) {
        CHECK(arrlen(args) == 2);

        // NOTE: this is actually a safe instruction (we allow it in the bytecode
        //       of CEQ), so there is no harm in allowing it
        *out = jit_builder_build_icmp(builder, JIT_ICMP_EQ, JIT_TYPE_I32, args[0], args[1]);

    } else {
        CHECK_FAIL("Unknown method %T::%U", target->DeclaringType, target->Name);
    }

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The jit itself
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/**
 * Jit a single instruction, this is to separate control flow and exceptions logic from instruction specific
 * logic and make the code more readable
 */
static tdn_err_t jit_instruction(
    jit_context_t* ctx,
    jit_region_t* region,
    tdn_il_inst_t inst,
    uint32_t pc
) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeMethodBase method = ctx->method;
    jit_builder_t builder = ctx->builder;
    eval_stack_t* stack = &ctx->stack;

    RuntimeTypeInfo* call_args_types = NULL;
    jit_value_t* call_args_values = NULL;

    // handle prefix cleanu pfirst
    if (!ctx->last_was_prefix) {
        ctx->volatile_prefix = 0;
        ctx->constrained = NULL;
    }
    ctx->last_was_prefix = 0;

    //
    // the main instruction jitting
    // TODO: split this to multiple functions in different places
    //
    switch (inst.opcode) {
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Prefixes
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        case CEE_VOLATILE: {
            CHECK(!ctx->last_was_prefix);
            ctx->last_was_prefix = 1;
            ctx->volatile_prefix = 1;
        } break;

        case CEE_CONSTRAINED: {
            CHECK(!ctx->last_was_prefix);
            ctx->last_was_prefix = 1;
            ctx->constrained = inst.operand.type;
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Arguments
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // load an argument
        case CEE_LDARG: {
            uint16_t argi = inst.operand.variable;
            CHECK(argi < arrlen(ctx->args));
            jit_arg_t* arg = &ctx->args[argi];

            // get the argument we are loading
            RuntimeTypeInfo arg_type = tdn_get_intermediate_type(arg->type);

            if (arg->spilled) {
                // was spilled, this is a stack slot
                if (jit_is_struct_type(arg_type)) {
                    // use memcpy
                    jit_value_t location;
                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, arg_type, &location));
                    jit_emit_memcpy(builder, location, arg->value, arg_type->StackSize);
                } else {
                    // use a proper load
                    CHECK_AND_RETHROW(eval_stack_push(stack, arg_type,
                                                      jit_builder_build_load(builder,
                                                                                get_jit_mem_size(arg->type),
                                                                                get_jit_mem_type(arg->type),
                                                                                arg->value)));
                }
            } else {
                // was not spilled, this is a param-ref
                if (jit_is_struct_type(arg_type)) {
                    // passed by pointer, memcpy to the stack
                    jit_value_t location;
                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, arg_type, &location));
                    jit_emit_memcpy(builder, location, arg->value, arg_type->StackSize);
                } else {
                    // just push it
                    CHECK_AND_RETHROW(eval_stack_push(stack, arg_type, arg->value));
                }
            }

            // if this is a byref its non-local since it came
            // from the outside
            if (arg_type->IsByRef) {
                eval_stack_item_t* item = eval_stack_get_top(stack);
                item->is_nonlocal_ref = true;
                if (arg->attributes.In) {
                    item->is_writable = false;
                    item->is_readable = true;

                } else if (arg->attributes.Out) {
                    item->is_writable = true;
                    item->is_readable = false;

                } else {
                    item->is_writable = true;
                    item->is_readable = true;
                }
            }
        } break;

        case CEE_LDARGA: {
            uint16_t argi = inst.operand.variable;
            CHECK(argi < arrlen(ctx->args));
            jit_arg_t* arg = &ctx->args[argi];
            CHECK(arg->spilled);

            // can't take a reference to a byref parameter
            CHECK(!arg->type->IsByRef);

            // get the argument we are loading
            RuntimeTypeInfo arg_type;
            CHECK_AND_RETHROW(tdn_get_byref_type(arg->type, &arg_type));

            // push the stack slot to the stack
            CHECK_AND_RETHROW(eval_stack_push(stack, arg_type, arg->value));

            // this is readable and writable since its local
            eval_stack_item_t* item = eval_stack_get_top(stack);
            item->is_readable = true;
            item->is_writable = true;
        } break;

        case CEE_STARG: {
            uint16_t argi = inst.operand.variable;
            CHECK(argi < arrlen(ctx->args));
            jit_arg_t* arg = &ctx->args[argi];
            CHECK(arg->spilled);

            // can't modify a byref parameter
            CHECK(!arg->type->IsByRef);

            RuntimeTypeInfo value_type;
            jit_value_t value;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // check type
            CHECK(tdn_type_verifier_assignable_to(value_type, arg->type));

            // was spilled, this is a stack slot
            if (jit_is_struct_type(value_type)) {
                // use memcpy
                jit_emit_memcpy(builder, arg->value, value, value_type->StackSize);
            } else {
                // use a proper load
                jit_builder_build_store(builder,
                                           get_jit_mem_size(arg->type),
                                           value, arg->value);
            }
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Locals
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // load a local variable
        case CEE_LDLOC: {
            // verify the argument and get the stack type
            int var = inst.operand.variable;
            CHECK(var < arrlen(ctx->locals));

            RuntimeTypeInfo type = method->MethodBody->LocalVariables->Elements[var]->LocalType;
            RuntimeTypeInfo tracked_type = tdn_get_intermediate_type(type);

            if (jit_is_struct_type(type)) {
                // struct type, copy the stack slot to the eval stack
                jit_value_t loc;
                CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, type, &loc));
                jit_emit_memcpy(builder, loc, ctx->locals[var], type->StackSize);
            } else {
                // not a struct type, load it from the stack slot
                jit_value_t value = jit_builder_build_load(builder,
                                                                 get_jit_mem_size(type),
                                                                 get_jit_mem_type(type),
                                                                 ctx->locals[var]);
                if (type == tSByte) {
                    value = jit_builder_build_sfill(builder, 8, value);
                } else if (type == tInt16) {
                    value = jit_builder_build_sfill(builder, 16, value);
                }
                CHECK_AND_RETHROW(eval_stack_push(stack, tracked_type, value));
            }
        } break;

        // load the pointer to a local variable
        case CEE_LDLOCA: {
            int var = inst.operand.variable;
            CHECK(var < method->MethodBody->LocalVariables->Length);

            RuntimeTypeInfo type = method->MethodBody->LocalVariables->Elements[var]->LocalType;

            // can't get byref to byref
            CHECK(!type->IsByRef);

            type = tdn_get_verification_type(type);
            CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));

            CHECK_AND_RETHROW(eval_stack_push(stack, type, ctx->locals[var]));

            // reference to local variables is always readable and writable
            eval_stack_item_t* item = eval_stack_get_top(stack);
            item->is_writable = true;
            item->is_readable = true;
        } break;

        // store to a local variable
        case CEE_STLOC: {
            // verify the argument and get the stack type
            int var = inst.operand.variable;
            CHECK(var < method->MethodBody->LocalVariables->Length);

            eval_stack_item_t value;
            CHECK_AND_RETHROW(eval_stack_pop_item(stack, &value));

            // check the type
            RuntimeTypeInfo local_type = method->MethodBody->LocalVariables->Elements[var]->LocalType;
            CHECK(tdn_type_verifier_assignable_to(value.type, local_type));

            if (jit_is_struct_type(value.type)) {
                // struct type, copy the stack slot to the eval stack
                jit_emit_memcpy(builder, ctx->locals[var], value.value, value.type->StackSize);
            } else {
                // TODO: support readonly locals, for now we have no way to properly represent that
                if (value.type->IsByRef) {
                    CHECK(value.is_readable);
                    CHECK(value.is_writable);
                }

                // not a struct type, just store it
                jit_builder_build_store(builder,
                                           get_jit_mem_size(local_type),
                                           value.value,
                                           ctx->locals[var]);
            }
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Object related
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // load a field
        case CEE_LDFLD:
        case CEE_LDFLDA: {
            RuntimeFieldInfo field = inst.operand.field;
            bool ldflda = inst.opcode == CEE_LDFLDA;

            // pop the item
            jit_value_t obj;
            RuntimeTypeInfo obj_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &obj_type, &obj));

            // verify the field is contained within the given object
            if (obj_type->IsByRef) {
                CHECK(field->DeclaringType == obj_type->ElementType);
            } else {
                if (field->DeclaringType == tArray) {
                    CHECK(obj_type->IsArray);
                } else {
                    CHECK(field->DeclaringType == obj_type);
                }
            }

            // check this is either an object or a managed pointer
            CHECK(
                obj_type->IsByRef ||
                tdn_type_is_referencetype(obj_type) ||
                (jit_is_struct_type(obj_type) && !ldflda)
            );

            // validate we had a volatile prefix for volatile fields
            if (field->IsVolatile) {
                CHECK(ctx->volatile_prefix);
                ctx->volatile_prefix = 0;
            }

            // get the stack type of the field
            RuntimeTypeInfo field_type = inst.operand.field->FieldType;

            // figure the pointer to the field itself
            jit_value_t field_ptr;
            CHECK(!field->Attributes.Static); // fix if we ever get this

            // instance field
            if (field->FieldOffset == 0) {
                // field is at offset zero, just load it
                field_ptr = obj;
            } else {
                // build an offset to the field
                field_ptr = jit_builder_build_ptroff(builder, obj,
                                                        jit_builder_build_iconst(builder,
                                                                                    JIT_TYPE_I64,
                                                                                    field->FieldOffset));
            }

            if (ldflda) {
                // explicit null check since only loading the value
                jit_emit_null_check(ctx, builder, obj);

                // tracks as a managed pointer to the verification type
                RuntimeTypeInfo value_type = tdn_get_verification_type(field_type);
                CHECK_AND_RETHROW(tdn_get_byref_type(value_type, &value_type));

                // for reference to field we don't need the load
                CHECK_AND_RETHROW(eval_stack_push(stack, value_type, field_ptr));
            } else {
#ifdef __EXPLICIT_NULL_CHECK__
                jit_emit_null_check(ctx, builder, obj);
#endif

                // tracks as the intermediate type
                RuntimeTypeInfo value_type = tdn_get_intermediate_type(field_type);

                // perform the actual load
                if (jit_is_struct_type(field_type)) {
                    // we are copying a struct to the stack
                    jit_value_t value;
                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, value_type, &value));
                    jit_emit_memcpy(builder, value, field_ptr, field_type->StackSize);
                } else {
                    // we are copying a simpler value
                    jit_value_t value = jit_builder_build_load(builder,
                                                                     get_jit_mem_size(field_type),
                                                                     get_jit_mem_type(field_type),
                                                                     field_ptr);
                    if (field_type == tSByte) {
                        value = jit_builder_build_sfill(builder, 8, value);
                    } else if (field_type == tInt16) {
                        value = jit_builder_build_sfill(builder, 16, value);
                    }
                    CHECK_AND_RETHROW(eval_stack_push(stack, value_type, value));
                }
            }
        } break;

        // store to a field
        case CEE_STFLD: {
            RuntimeFieldInfo field = inst.operand.field;

            // pop the item
            jit_value_t obj, value;
            RuntimeTypeInfo obj_type, value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &obj_type, &obj));

            // verify the field is contained within the given object
            if (obj_type->IsByRef) {
                CHECK(field->DeclaringType == obj_type->ElementType);
            } else {
                CHECK(field->DeclaringType == obj_type);
            }

            // validate we had a volatile prefix for volatile fields
            if (field->IsVolatile) {
                CHECK(ctx->volatile_prefix);
                ctx->volatile_prefix = 0;
            }

            // check this is either an object or a managed pointer
            CHECK(
                obj_type->IsByRef ||
                tdn_type_is_referencetype(obj_type)
            );

            // get the stack type of the field
            RuntimeTypeInfo field_type = inst.operand.field->FieldType;
            CHECK(tdn_type_verifier_assignable_to(value_type, field_type));

            // figure the pointer to the field itself
            jit_value_t field_ptr;
            CHECK(!field->Attributes.Static); // fix if we ever get this

#ifdef __EXPLICIT_NULL_CHECK__
            jit_emit_null_check(ctx, builder, obj);
#endif

            // instance field
            if (field->FieldOffset == 0) {
                // field is at offset zero, just load it
                field_ptr = obj;
            } else {
                // build an offset to the field
                field_ptr = jit_builder_build_ptroff(builder, obj,
                                                        jit_builder_build_iconst(builder,
                                                                                    JIT_TYPE_I64,
                                                                                    field->FieldOffset));
            }

            // perform the actual store
            if (jit_is_struct_type(field_type)) {
                if (field_type->IsUnmanaged) {
                    jit_emit_memcpy(builder, field_ptr, value, field_type->StackSize);
                } else {
                    jit_emit_gc_memcpy(builder, field_type, field_ptr, value);
                }
            } else {
                // we are copying a simpler value
                // TODO: GC write barrier for reference types
                jit_builder_build_store(builder,
                                           get_jit_mem_size(field_type),
                                           value,
                                           field_ptr);
            }
        } break;

        //----------------------------------------------------------------------------------------------------------

        // load a static field
        case CEE_LDSFLD:
        case CEE_LDSFLDA: {
            RuntimeFieldInfo field = inst.operand.field;
            CHECK(field->Attributes.Static);
            bool ldsflda = inst.opcode == CEE_LDSFLDA;

            // get the stack type of the field
            RuntimeTypeInfo field_type = inst.operand.field->FieldType;

            // Make sure the type is initialized properly
            // TODO: lazy init static fields?
            CHECK_AND_RETHROW(jit_prepare_type(field_type));

            // validate we had a volatile prefix for volatile fields
            if (field->IsVolatile) {
                CHECK(ctx->volatile_prefix);
                ctx->volatile_prefix = 0;
            }

            // figure the pointer to the field itself
            CHECK(!inst.operand.field->Attributes.HasFieldRVA);
            jit_value_t field_ptr = jit_builder_build_iconst(builder, JIT_TYPE_PTR,
                                                             (uint64_t)field->JitFieldPtr);

            if (ldsflda) {
                // tracks as a managed pointer to the verification type
                RuntimeTypeInfo value_type = tdn_get_verification_type(field_type);
                CHECK_AND_RETHROW(tdn_get_byref_type(value_type, &value_type));

                // for reference to field we don't need the load
                CHECK_AND_RETHROW(eval_stack_push(stack, value_type, field_ptr));
            } else {
                // tracks as the intermediate type
                RuntimeTypeInfo value_type = tdn_get_intermediate_type(field_type);

                // perform the actual load
                if (jit_is_struct_type(field_type)) {
                    // we are copying a struct to the stack
                    jit_value_t value;
                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, value_type, &value));
                    jit_emit_memcpy(builder, value, field_ptr, field_type->StackSize);
                } else {
                    // we are copying a simpler value
                    jit_value_t value = jit_builder_build_load(builder,
                                                                     get_jit_mem_size(field_type),
                                                                     get_jit_mem_type(field_type),
                                                                     field_ptr);
                    if (field_type == tSByte) {
                        value = jit_builder_build_sfill(builder, 8, value);
                    } else if (field_type == tInt16) {
                        value = jit_builder_build_sfill(builder, 16, value);
                    }
                    CHECK_AND_RETHROW(eval_stack_push(stack, value_type, value));
                }
            }
        } break;

        // store to a static field
        case CEE_STSFLD: {
            RuntimeFieldInfo field = inst.operand.field;
            CHECK(field->Attributes.Static);

            // validate we had a volatile prefix for volatile fields
            if (field->IsVolatile) {
                CHECK(ctx->volatile_prefix);
                ctx->volatile_prefix = 0;
            }

            // pop the item
            jit_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // get the stack type of the field
            RuntimeTypeInfo field_type = inst.operand.field->FieldType;
            CHECK(tdn_type_verifier_assignable_to(value_type, field_type));

            // Make sure the type is initialized properly
            // TODO: lazy init static fields?
            CHECK_AND_RETHROW(jit_prepare_type(field_type));

            // figure the pointer to the field itself
            CHECK(!inst.operand.field->Attributes.HasFieldRVA);
            jit_value_t field_ptr = jit_builder_build_iconst(builder, JIT_TYPE_PTR,
                                                             (uint64_t)field->JitFieldPtr);

            // perform the actual store
            if (jit_is_struct_type(field_type)) {
                // we are copying a struct to the stack
                jit_emit_memcpy(builder, field_ptr, value, field_type->StackSize);
            } else {
                // we are copying a simpler value
                jit_builder_build_store(builder,
                                           get_jit_mem_size(field_type),
                                           value,
                                           field_ptr);
            }
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Array related
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // load the length of an array
        case CEE_LDLEN: {
            // pop the items
            jit_value_t array;
            RuntimeTypeInfo array_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &array_type, &array));

#ifdef __EXPLICIT_NULL_CHECK__
            jit_emit_null_check(ctx, builder, array);
#endif

            // push the length, the load automatically zero extends it
            jit_value_t length_offset = jit_builder_build_iconst(builder, JIT_TYPE_I64, offsetof(struct Array, Length));
            jit_value_t length_ptr = jit_builder_build_ptroff(builder, array, length_offset);
            jit_value_t length = jit_builder_build_load(builder, JIT_MEM_SIZE_4, JIT_TYPE_I64, length_ptr);
            CHECK_AND_RETHROW(eval_stack_push(stack, tIntPtr, length));
        } break;

        // load an element from an array
        case CEE_LDELEMA:
        case CEE_LDELEM:
        case CEE_LDELEM_REF: {
            // pop the items
            jit_value_t index, array;
            RuntimeTypeInfo index_type, array_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &index_type, &index));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &array_type, &array));

            // verify the element type
            CHECK(array_type->IsArray);
            RuntimeTypeInfo T = array_type->ElementType;
            RuntimeTypeInfo type = inst.operand.type;
            if (inst.opcode == CEE_LDELEM_REF) {
                // figure from the array, must be a pointer type
                CHECK(tdn_type_is_referencetype(T));
                type = T;
            } else {
                // make sure the wanted type matches the array type
                CHECK(tdn_type_array_element_compatible_with(array_type->ElementType, type));
            }

            // verify the index type
            CHECK(index_type == tInt32 || index_type == tIntPtr);

            // sign extend the index to a 64bit value
            if (index_type == tInt32) {
                index = jit_builder_build_iext(builder, index);
                index = jit_builder_build_sfill(builder, 32, index);
            }

#ifdef __EXPLICIT_NULL_CHECK__
            jit_emit_null_check(ctx, builder, array);
#endif

            jit_block_t length_is_valid = jit_builder_create_block(builder);

            // length check, this will also take care of the NULL check since if
            // we have a null value we will just fault in here
            jit_value_t length_offset = jit_builder_build_iconst(builder, JIT_TYPE_I64, offsetof(struct Array, Length));
            jit_value_t length_ptr = jit_builder_build_ptroff(builder, array, length_offset);
            jit_value_t length = jit_builder_build_load(builder, JIT_MEM_SIZE_4, JIT_TYPE_I64, length_ptr);

            // make sure the index < length
            jit_block_t throw_index_out_of_range = jit_throw_builtin_exception(ctx, builder, JIT_EXCEPTION_INDEX_OUT_OF_RANGE);
            jit_value_t length_check = jit_builder_build_icmp(builder, JIT_ICMP_SLT, JIT_TYPE_I64, index, length);
            jit_builder_build_brcond(builder, length_check, length_is_valid, throw_index_out_of_range);

            // perform the valid length branch, load the value from the array
            jit_builder_set_block(builder, length_is_valid);
            jit_value_t element_size = jit_builder_build_iconst(builder, JIT_TYPE_I64, type->StackSize);
            jit_value_t array_size = jit_builder_build_iconst(builder, JIT_TYPE_I64, tArray->HeapSize);
            jit_value_t byte_offset = jit_builder_build_imul(builder, element_size, index);
            jit_value_t abs_offset = jit_builder_build_iadd(builder, array_size, byte_offset);
            jit_value_t element_ptr = jit_builder_build_ptroff(builder, array, abs_offset);

            if (inst.opcode == CEE_LDELEMA) {
                RuntimeTypeInfo tracked = tdn_get_verification_type(type);
                CHECK_AND_RETHROW(tdn_get_byref_type(tracked, &tracked));

                // just need the address, we don't need an explicit null check
                // since we are going to get a fault on the length check if
                // the array is null
                CHECK_AND_RETHROW(eval_stack_push(stack, tracked, element_ptr));
            } else {
                RuntimeTypeInfo tracked = tdn_get_intermediate_type(type);

                // perform the actual load
                if (jit_is_struct_type(tracked)) {
                    // we are copying a struct to the stack
                    jit_value_t value;
                    CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, tracked, &value));
                    jit_emit_memcpy(builder, value, element_ptr, tracked->StackSize);
                } else {
                    // we are copying a simpler value
                    jit_value_t value = jit_builder_build_load(builder,
                                                                     get_jit_mem_size(type),
                                                                     get_jit_mem_type(type),
                                                                     element_ptr);
                    if (type == tSByte) {
                        value = jit_builder_build_sfill(builder, 8, value);
                    } else if (type == tInt16) {
                        value = jit_builder_build_sfill(builder, 16, value);
                    }
                    CHECK_AND_RETHROW(eval_stack_push(stack, tracked, value));
                }
            }
        } break;

        // load an element from an array
        case CEE_STELEM:
        case CEE_STELEM_REF: {
            // pop the items
            jit_value_t value, index, array;
            RuntimeTypeInfo value_type, index_type, array_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &index_type, &index));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &array_type, &array));

            // verify the array type
            CHECK(array_type->IsArray);
            RuntimeTypeInfo T = array_type->ElementType;
            if (inst.opcode == CEE_STELEM_REF) {
                // figure from the array, must be a pointer type
                CHECK(tdn_type_is_referencetype(T));
                CHECK(tdn_type_array_element_compatible_with(value_type, T));
            } else {
                // TODO: I am not sure about this but without the intermediate type it won't work
                // CHECK(tdn_type_array_element_compatible_with(value_type, inst.operand.type));
                CHECK(tdn_type_array_element_compatible_with(value_type, tdn_get_intermediate_type(inst.operand.type)));
                CHECK(tdn_type_array_element_compatible_with(inst.operand.type, T));
            }

            // verify the index type
            CHECK(index_type == tInt32 || index_type == tIntPtr);

            // sign extend the index to a 64bit value
            if (index_type == tInt32) {
                index = jit_builder_build_iext(builder, index);
                index = jit_builder_build_sfill(builder, 32, index);
            }

#ifdef __EXPLICIT_NULL_CHECK__
            jit_emit_null_check(ctx, builder, array);
#endif

            jit_block_t length_is_valid = jit_builder_create_block(builder);

            // length check, this will also take care of the NULL check since if
            // we have a null value we will just fault in here
            jit_value_t length_offset = jit_builder_build_iconst(builder, JIT_TYPE_I64, offsetof(struct Array, Length));
            jit_value_t length_ptr = jit_builder_build_ptroff(builder, array, length_offset);
            jit_value_t length = jit_builder_build_load(builder, JIT_MEM_SIZE_4, JIT_TYPE_I64, length_ptr);

            // make sure the index < length
            jit_block_t throw_inex_out_of_range = jit_throw_builtin_exception(ctx, builder, JIT_EXCEPTION_INDEX_OUT_OF_RANGE);
            jit_value_t length_check = jit_builder_build_icmp(builder, JIT_ICMP_SLT, JIT_TYPE_I64, index, length);
            jit_builder_build_brcond(builder, length_check, length_is_valid, throw_inex_out_of_range);

            // perform the valid length branch, load the value from the array
            jit_builder_set_block(builder, length_is_valid);
            jit_value_t element_size = jit_builder_build_iconst(builder, JIT_TYPE_I64, T->StackSize);
            jit_value_t array_size = jit_builder_build_iconst(builder, JIT_TYPE_I64, tArray->HeapSize);
            jit_value_t byte_offset = jit_builder_build_imul(builder, element_size, index);
            jit_value_t abs_offset = jit_builder_build_iadd(builder, array_size, byte_offset);
            jit_value_t element_ptr = jit_builder_build_ptroff(builder, array, abs_offset);

            // TODO: perform write barrier as needed

            // perform the actual store
            if (jit_is_struct_type(T)) {
                // we are copying a struct to the stack
                if (T->IsUnmanaged) {
                    jit_emit_memcpy(builder, element_ptr, value, T->StackSize);
                } else {
                    jit_emit_gc_memcpy(builder, T, element_ptr, value);
                }
            } else {
                // we are copying a simpler value
                jit_builder_build_store(builder,
                                           get_jit_mem_size(T),
                                           value,
                                           element_ptr);
            }
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Function calls
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////

        case CEE_NEWOBJ:
        case CEE_CALL:
        case CEE_CALLVIRT: {
            RuntimeMethodBase target = inst.operand.method;
            bool is_static = target->Attributes.Static;
            bool is_call = inst.opcode == CEE_CALL;
            bool is_callvirt = inst.opcode == CEE_CALLVIRT;
            bool is_newobj = inst.opcode == CEE_NEWOBJ;
            size_t object_size = 0;

            // verify we can call it
            if (is_newobj) {
                // newobj cannot call static or abstract methods
                CHECK(!target->Attributes.Abstract);
                CHECK(!target->Attributes.Static);

                // ctor must be a special name
                CHECK(target->Attributes.RTSpecialName);

                // TODO: for strings we need to calculate the string size
                object_size = target->DeclaringType->HeapSize;
            } else if (is_callvirt) {
                // callvirt can not call static methods
                CHECK(!target->Attributes.Static);
            } else {
                // call can not call abstract methods
                CHECK(!target->Attributes.Abstract);
            }

            // get all the arguments
            int call_args_count = target->Parameters->Length + (is_static ? 0 : 1);
            arrsetlen(call_args_types, call_args_count);
            arrsetlen(call_args_values, call_args_count);
            for (int i = call_args_count - 1; i >= 0; i--) {
                if (is_newobj && i == 0) {
                    // special case for the first argument for newobj, we need to allocate it, either
                    // on the stack if its a value type or on the heap if its a reference type
                    RuntimeTypeInfo target_this_type;
                    jit_value_t obj = JIT_VALUE_INVALID;
                    if (tdn_type_is_referencetype(target->DeclaringType)) {
                        // call gc_new to allocate it
                        target_this_type = target->DeclaringType;
                        obj = jit_builder_build_call(builder, m_builtin_gc_new, 2, (jit_value_t[]){
                            jit_builder_build_iconst(builder, JIT_TYPE_PTR, (uint64_t)target_this_type),
                            jit_builder_build_iconst(builder, JIT_TYPE_I64, object_size)
                        });
                        CHECK_AND_RETHROW(eval_stack_push(stack, target->DeclaringType, obj));
                    } else {
                        // allocate it on the stack
                        CHECK_AND_RETHROW(tdn_get_byref_type(target->DeclaringType, &target_this_type));
                        CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, target->DeclaringType, &obj));
                    }

                    call_args_types[i] = target_this_type;
                    call_args_values[i] = obj;
                    break;
                }

                // pop it
                eval_stack_item_t item;
                CHECK_AND_RETHROW(eval_stack_pop_item(stack, &item));
                call_args_types[i] = item.type;
                call_args_values[i] = item.value;

                // validate the stack type
                ParameterAttributes attributes = { .Attributes = 0 };
                RuntimeTypeInfo target_type;
                if (!is_static) {
                    if (i == 0) {
                        // if we are in a callvirt update the target
                        // to be from the instance type, this will
                        // help us to perform a de-virt in case the
                        // class is either sealed or the method is final
                        if (is_callvirt) {
                            // TODO: this
                        }

                        // figure the correct one for value types
                        if (tdn_type_is_valuetype(target->DeclaringType)) {
                            CHECK_AND_RETHROW(tdn_get_byref_type(target->DeclaringType, &target_type));
                        } else {
                            target_type = target->DeclaringType;
                        }

                        if (is_call) {
                            // TODO: verify that we don't bypass the override of a function
                            //       unless we are part of the class tree
                        } else if (
                            is_callvirt &&
                            (
                                !target->Attributes.Virtual || // method is not virtual
                                target->DeclaringType->Attributes.Sealed || // the type on the stack is sealed
                                (target->Attributes.Virtual && target->Attributes.Final) // the method is final
                            )
                        ) {
                            jit_emit_null_check(ctx, builder, call_args_values[0]);
                        }
                    } else {
                        target_type = target->Parameters->Elements[i - 1]->ParameterType;
                        attributes = target->Parameters->Elements[i - 1]->Attributes;
                    }
                } else {
                    target_type = target->Parameters->Elements[i]->ParameterType;
                    attributes = target->Parameters->Elements[i]->Attributes;
                }

                // verifications on byref restrictions
                if (target_type->IsByRef) {
                    if (attributes.In) {
                        // in - must be readable, doesn't need to be writable
                        CHECK(item.is_readable);
                    } else if (attributes.Out) {
                        // out - must be writable, doesn't need to be readable
                        CHECK(item.is_writable);
                    } else {
                        // normal ref, must be readable and writable
                        CHECK(item.is_readable);
                        CHECK(item.is_writable);
                    }
                }

                // check that we can do the assignment
                CHECK(tdn_type_verifier_assignable_to(call_args_types[i], target_type),
                      "%T verifier-assignable-to %T", call_args_types[i], target_type);
            }

            // TODO: indirect calls, de-virt

            // make sure that we can actually call the target
            CHECK_AND_RETHROW(jit_prepare_method(target, jit_builder_get_module(builder)));

            RuntimeTypeInfo ret_type = tdn_get_intermediate_type(target->ReturnParameter->ParameterType);

            // for struct instances allocate and push it beforehand
            if (ret_type != tVoid && jit_is_struct_type(ret_type)) {
                // need to allocate the space in the caller, insert it as the first argument, already pushed
                // to the stack
                CHECK(!is_newobj);
                jit_value_t ret_buffer;
                CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, ret_type, &ret_buffer));
                arrins(call_args_values, 0, ret_buffer);
            }

            // restrict access to certain classes only to assemblies that can run
            // unsafe code
            RuntimeAssembly assembly = method->Module->Assembly;
            RuntimeTypeInfo declaring_type = target->DeclaringType;
            if (!assembly->AllowUnsafe) {
                CHECK(declaring_type != tUnsafe);
                CHECK(declaring_type != tMemoryMarshal);
            }

            // and now emit it, also handling special cases on the fly
            jit_value_t value;
            if (target->MethodImplFlags.CodeType == TDN_METHOD_IMPL_CODE_TYPE_RUNTIME) {
                if (declaring_type == tUnsafe) {
                    CHECK_AND_RETHROW(handle_unsafe_intrinsics(builder, target, call_args_types, call_args_values, &value));
                } else {
                    CHECK_FAIL();
                }
            } else {
                // emit the actual call
                value = jit_builder_build_call(builder, jit_get_function_from_id(target->JitMethodId), arrlen(call_args_values), call_args_values);
            }

            // for primitive types push it now
            if (ret_type != tVoid && !jit_is_struct_type(ret_type)) {
                CHECK(!is_newobj);
                CHECK_AND_RETHROW(eval_stack_push(stack, ret_type, value));

                if (ret_type->IsByRef) {
                    eval_stack_item_t* item = eval_stack_get_top(stack);

                    // we got this as a return, its readable
                    item->is_readable = true;

                    // if this is not a readonly return then
                    // mark this as writable as well
                    if (!target->ReturnParameter->IsReadonly) {
                        item->is_writable = true;
                    }
                }
            }
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Control flow
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // unconditional branch
        case CEE_BR: {
            // get and validate the label
            jit_label_t* target_label = NULL;
            CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, inst.operand.branch_target, &target_label));

            // a branch, emit the branch
            jit_builder_build_branch(builder, target_label->block);
            region->has_block = false;

            // because we don't fall-through we clear the stack
            eval_stack_clear(stack);
        } break;

        // conditional branches
        case CEE_BRFALSE:
        case CEE_BRTRUE: {
            // pop the item
            jit_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // ECMA-335 doesn't say brtrue takes in anything but
            // O and native int, but I think its just an oversight
            CHECK(
                tdn_type_is_referencetype(value_type) ||
                value_type == tInt32 ||
                value_type == tInt64 ||
                value_type == tIntPtr
            );

            // reference types can't be put directly into a brcond, so
            // we are going to emit a compare first
            if (tdn_type_is_referencetype(value_type)) {
                value = jit_builder_build_icmp(builder, JIT_ICMP_NE, JIT_TYPE_I64, value,
                                               jit_builder_build_iconst(builder, JIT_TYPE_PTR, 0));
            }

            // get the jump locations
            jit_label_t* target_label = NULL;
            CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, inst.operand.branch_target, &target_label));

            jit_label_t* next_label = NULL;
            CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, pc + inst.length, &next_label));

            // choose the target to fit the brcond
            jit_block_t true_dest;
            jit_block_t false_dest;
            if (inst.opcode == CEE_BRTRUE) {
                // jump if non-zero
                true_dest = target_label->block;
                false_dest = next_label->block;
            } else {
                // jump if zero
                true_dest = next_label->block;
                false_dest = target_label->block;
            }

            // a branch, emit the branch
            jit_builder_build_brcond(builder, value, true_dest, false_dest);
        } break;

        // all the different compare and compare-and-branches
        // that we have
        case CEE_BEQ:
        case CEE_BGE:
        case CEE_BGT:
        case CEE_BLE:
        case CEE_BLT:
        case CEE_BNE_UN:
        case CEE_BGE_UN:
        case CEE_BGT_UN:
        case CEE_BLE_UN:
        case CEE_BLT_UN:
        case CEE_CEQ:
        case CEE_CGT:
        case CEE_CGT_UN:
        case CEE_CLT:
        case CEE_CLT_UN: {
            // pop the items
            jit_value_t value1, value2;
            RuntimeTypeInfo value1_type, value2_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value2_type, &value2));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value1_type, &value1));

            //
            // perform the binary comparison and branch operations check,
            // anything else can not be tested
            //
            if (value1_type == tInt32) {
                CHECK(value2_type == tInt32 || value2_type == tIntPtr);

            } else if (value1_type == tInt64) {
                CHECK(value2_type == tInt64);

            } else if (value1_type == tIntPtr) {
                CHECK(value2_type == tInt32 || value2_type == tIntPtr);

            } else if (value1_type->IsByRef) {
                // TODO: does this only apply to types
                //       of the same reference? I assume
                //       it does but we might need to change this
                CHECK(value2_type == value1_type);

            } else if (tdn_type_is_referencetype(value1_type) && tdn_type_is_referencetype(value2_type)) {
                CHECK(
                    inst.opcode == CEE_BEQ ||
                    inst.opcode == CEE_BNE_UN ||
                    inst.opcode == CEE_CEQ ||
                    inst.opcode == CEE_CGT_UN
                );

            } else {
                CHECK_FAIL();
            }

            // jit only has the one side, need to flip for the other side
            jit_icmp_kind_t kind;
            bool compare = false;
            switch (inst.opcode) {
                case CEE_CEQ: compare = true;
                case CEE_BEQ: kind = JIT_ICMP_EQ; break;
                case CEE_BGE: kind = JIT_ICMP_SLE; SWAP(value1, value2); break;
                case CEE_CGT: compare = true;
                case CEE_BGT: kind = JIT_ICMP_SLT; SWAP(value1, value2); break;
                case CEE_BLE: kind = JIT_ICMP_SLE; break;
                case CEE_CLT: compare = true;
                case CEE_BLT: kind = JIT_ICMP_SLT; break;
                case CEE_BNE_UN: kind = JIT_ICMP_NE; break;
                case CEE_BGE_UN: kind = JIT_ICMP_ULE; SWAP(value1, value2); break;
                case CEE_CGT_UN: compare = true;
                case CEE_BGT_UN: kind = JIT_ICMP_ULT; SWAP(value1, value2); break;
                case CEE_BLE_UN: kind = JIT_ICMP_ULE; break;
                case CEE_CLT_UN: compare = true;
                case CEE_BLT_UN: kind = JIT_ICMP_ULT; break;
                default: CHECK_FAIL();
            }

            // create the comparison
            jit_value_t cmp = jit_builder_build_icmp(builder,
                                                           kind,
                                                     compare ? JIT_TYPE_I32 : JIT_TYPE_I64,
                                                           value1, value2);

            // check if its a compare or not
            if (compare) {
                // a compare, just push the result as an int32
                eval_stack_push(stack, tInt32, cmp);
            } else {
                // get the jump locations
                jit_label_t* target_label = NULL;
                CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, inst.operand.branch_target, &target_label));

                jit_label_t* next_label = NULL;
                CHECK_AND_RETHROW(resolve_and_verify_branch_target(ctx, region, pc + inst.length, &next_label));

                // a branch, emit the branch
                jit_builder_build_brcond(builder, cmp, target_label->block, next_label->block);
            }
        } break;

        // return value from the function
        case CEE_RET: {
            // return must be from the root region
            CHECK(region->clause == NULL);

            RuntimeTypeInfo wanted_ret_type = method->ReturnParameter->ParameterType;
            if (wanted_ret_type == tVoid) {
                jit_builder_build_return(builder, JIT_VALUE_INVALID);
            } else {
                eval_stack_item_t ret;
                eval_stack_pop_item(stack, &ret);

                // validations for reference return types
                if (ret.type->IsByRef) {
                    // make sure this is a non-local reference, otherwise we
                    // are not allowed to return it
                    CHECK(ret.is_nonlocal_ref);

                    // must be a readable reference if we return it
                    CHECK(ret.is_readable);

                    // if the return is not readonly, make sure it is
                    // also a writable reference
                    if (!method->ReturnParameter->IsReadonly) {
                        CHECK(ret.is_writable);
                    }
                }

                // make sure the type is a valid return target
                CHECK(tdn_type_verifier_assignable_to(ret.type, wanted_ret_type));

                if (jit_is_struct_type(ret.type)) {
                    // returning a struct, need to use the implicit
                    // ret pointer
                    jit_emit_memcpy(builder,
                                    jit_builder_build_param_ref(builder, 0),
                                    ret.value,
                                    ret.type->StackSize);

                    // and return without a ret value
                    jit_builder_build_return(builder, JIT_VALUE_INVALID);
                } else {
                    // returning a normal pointer sized thing
                    jit_builder_build_return(builder, ret.value);
                }
            }

            // eval stack must be empty at this point
            CHECK(arrlen(stack->stack) == 0);
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Exception control flow
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        case CEE_LEAVE: {
            // start with emptying the eval stack
            eval_stack_clear(stack);

            // the target
            uint32_t branch_target = inst.operand.branch_target;

            // if we are in this path make sure we have a clause
            CHECK(region->clause != NULL);

            // and make sure it is not a fault/finally one
            if (region->is_handler) {
                CHECK(
                    region->clause->Flags != COR_ILEXCEPTION_CLAUSE_FINALLY &&
                    region->clause->Flags != COR_ILEXCEPTION_CLAUSE_FAULT
                );
            }
            // TODO: check not within a filter

            // this exits the block, lets figure to where
            jit_label_t* target_label = NULL;
            finally_handler_t* first_handle = NULL;
            finally_handler_t* last_handler = NULL;
            for (int i = arrlen(ctx->regions) - 1; i >= 0; i--) {
                jit_region_t* cur_region = ctx->regions[i];
                if (cur_region->is_handler) continue; // we must leave into another protected block

                if (cur_region->pc_start <= branch_target && branch_target < cur_region->pc_end) {
                    // found the target try-region
                    target_label = jit_get_label(cur_region, branch_target);
                    CHECK(target_label != NULL);
                    break;

                } else if (cur_region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
                    // we need to go through this finally
                    finally_handler_t* path = hmgetp_null(cur_region->finally_handlers->finally_paths, branch_target);
                    if (path == NULL) {
                        jit_region_t* new_region = tdn_host_mallocz(sizeof(jit_region_t));
                        CHECK(new_region != NULL);

                        // create the new region and set
                        // all the fields, including creating
                        // the labels for that run
                        new_region->clause = cur_region->clause;
                        new_region->clause_index = cur_region->clause_index;
                        new_region->pc_start = cur_region->clause->HandlerOffset;
                        new_region->pc_end = cur_region->clause->HandlerOffset + cur_region->clause->HandlerLength;
                        new_region->is_finally_path = true;
                        new_region->is_handler = true;
                        new_region->current_finally_path = INT32_MAX;
                        new_region->finally_handlers = cur_region->finally_handlers;
                        CHECK_AND_RETHROW(create_region_labels(ctx, new_region));

                        // create a new handler
                        finally_handler_t new_path = {
                            .region = new_region,
                            .key = branch_target
                        };
                        hmputs(cur_region->finally_handlers->finally_paths, new_path);
                        path = hmgetp_null(cur_region->finally_handlers->finally_paths, branch_target);
                        CHECK(path != NULL);
                    }

                    // if we already have another finally on the stack
                    // set its next block to the current block, also
                    // verify the consistency
                    if (last_handler != NULL) {
                        if (last_handler->region->has_next_block) {
                            CHECK(JIT_IS_SAME_BLOCK(last_handler->region->next_block, path->region->entry_block));
                        } else {
                            last_handler->region->next_block = path->region->entry_block;
                            last_handler->region->has_next_block = true;
                        }
                    }

                    last_handler = path;
                    if (first_handle == NULL) first_handle = path;
                }
            }

            // make sure we got a label at all
            CHECK(target_label != NULL);

            // update the last handler to jump into the target label
            if (last_handler != NULL) {
                if (last_handler->region->has_next_block) {
                    CHECK(JIT_IS_SAME_BLOCK(last_handler->region->next_block, target_label->block));
                } else {
                    last_handler->region->next_block = target_label->block;
                    last_handler->region->has_next_block = true;
                }
            }

            // if we have a first handle then get the label
            // from that region to jump to
            if (first_handle != NULL) {
                target_label = jit_get_label(first_handle->region, first_handle->region->pc_start);
                if (target_label == NULL) {
                    // no label, so just go into the entry and don't process the target label stuff
                    jit_builder_build_branch(builder, first_handle->region->entry_block);
                    region->has_block = false;
                    break;
                }
            }

            // create a branch into the handler, we know that, verify the stack
            // on the way
            if (target_label->snapshot.initialized) {
                CHECK(arrlen(target_label->snapshot.stack) == 0);
            } else {
                target_label->snapshot.initialized = true;
            }

            jit_builder_build_branch(builder, target_label->block);
            region->has_block = false;
        } break;

        // this needs to jump into the next block, for fault and finally's fault path
        // this simply goes into the next handler, for non-faulting finally path this
        // goes into
        case CEE_ENDFINALLY: {
            // must be inside fault or finally
            CHECK(
                region->is_handler &&
                (
                    region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY ||
                    region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT
                )
            );

            // empty the stack
            eval_stack_clear(stack);

            // figure where we need to go
            if (!region->is_finally_path) {
                // get the region of the next handler, skipping
                // the current one
                for (int i = arrlen(ctx->regions) - 2; i >= 0; i--) {
                    if (ctx->regions[i]->is_handler)
                        continue;

                    if (ctx->regions[i]->clause == NULL) {
                        // we got to the root region, meaning there is nothing
                        // else in the fault path to handle, so just rethrow the
                        // error
                        jit_builder_build_call(builder, m_builtin_rethrow, 0, NULL);
                        jit_builder_build_unreachable(builder);
                    } else {
                        jit_region_t* handler = &ctx->handler_regions[ctx->regions[i]->clause_index];
                        jit_builder_build_branch(builder, handler->entry_block);
                    }

                    region->has_block = false;
                    break;
                }
            } else {
                // we need to go to the next entry finally of this path
                CHECK(region->has_next_block);
                jit_builder_build_branch(builder, region->next_block);
                region->has_block = false;
            }
        } break;

        // throw an exception
        case CEE_THROW: {
            // pop the item
            jit_value_t object;
            RuntimeTypeInfo object_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &object_type, &object));

            // make sure this is an object
            CHECK(tdn_type_is_referencetype(object_type));

            // must not be null
            jit_emit_null_check(ctx, builder, object);

            // and throw it
            jit_builder_build_call(builder, m_builtin_throw, 1, &object);

            // and we can't actually reach here
            jit_builder_build_unreachable(builder);

            // because we don't fall-through we clear the stack
            eval_stack_clear(stack);
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Stack manipulation
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // Push an int32 to the stack
        case CEE_LDC_I4: {
            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32,
                                              jit_builder_build_iconst(builder,
                                                                          JIT_TYPE_I32, inst.operand.int32)));
        } break;

        // Push an int64
        case CEE_LDC_I8: {
            CHECK_AND_RETHROW(eval_stack_push(stack, tInt64,
                                              jit_builder_build_iconst(builder,
                                                                          JIT_TYPE_I64, inst.operand.uint64)));
        } break;

        // push a string
        case CEE_LDSTR: {
            CHECK_AND_RETHROW(eval_stack_push(stack, tString,
                                              jit_builder_build_iconst(builder,
                                                                          JIT_TYPE_PTR, (uintptr_t)inst.operand.string)));
        } break;

        // Push a null object
        case CEE_LDNULL: {
            CHECK_AND_RETHROW(eval_stack_push(stack, tNull,
                                              jit_builder_build_iconst(builder,
                                                                          JIT_TYPE_PTR, 0)));
        } break;

        // Pop a value and dup it
        case CEE_DUP: {
            // pop the value and ignore it
            RuntimeTypeInfo type;
            jit_value_t value;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &type, &value));

            // and now push it twice
            CHECK_AND_RETHROW(eval_stack_push(stack, type, value));
            CHECK_AND_RETHROW(eval_stack_push(stack, type, value));
        } break;

        // Pop a value and ignore it
        case CEE_POP: {
            // pop the value and ignore it
            CHECK_AND_RETHROW(eval_stack_pop(stack, NULL, NULL));
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Math related
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // shifts
        case CEE_SHL:
        case CEE_SHR:
        case CEE_SHR_UN: {
            // pop the items
            jit_value_t value, shift_amount;
            RuntimeTypeInfo value_type, shift_amount_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &shift_amount_type, &shift_amount));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // type check
            CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);
            CHECK(shift_amount_type == tInt32 || shift_amount_type == tIntPtr);

            // truncate to 32bit if needed
            if (shift_amount_type == tIntPtr) {
                shift_amount = jit_builder_build_itrunc(builder, shift_amount);
            }

            // perform the operation
            jit_value_t result_value;
            switch (inst.opcode) {
                case CEE_SHL: result_value = jit_builder_build_shl(builder, value, shift_amount); break;
                case CEE_SHR: result_value = jit_builder_build_ashr(builder, value, shift_amount); break;
                case CEE_SHR_UN: result_value = jit_builder_build_lshr(builder, value, shift_amount); break;
                default: CHECK_FAIL();
            }

            // push it to the stack
            CHECK_AND_RETHROW(eval_stack_push(stack, value_type, result_value));
        } break;

        // binary operations on either integers or floats
        case CEE_ADD:
        case CEE_SUB:
        case CEE_AND:
        case CEE_OR:
        case CEE_XOR:
        case CEE_MUL:
        case CEE_DIV:
        case CEE_DIV_UN:
        case CEE_REM:
        case CEE_REM_UN: {
            // pop the items
            jit_value_t value1, value2;
            RuntimeTypeInfo value1_type, value2_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value2_type, &value2));
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value1_type, &value1));

            // figure the type we are going to use
            RuntimeTypeInfo result = NULL;
            if (value1_type == tInt32) {
                if (value2_type == tInt32) {
                    result = tInt32;
                } else if (value2_type == tIntPtr) {
                    CHECK_FAIL(); // TODO: how to expand properly
                    result = tIntPtr;
                } else {
                    CHECK_FAIL();
                }
            } else if (value1_type == tInt64) {
                CHECK(value2_type == tInt64);
                result = tInt64;
            } else if (value1_type == tIntPtr) {
                if (value2_type == tInt32) {
                    CHECK_FAIL(); // TODO: how to expand properly
                    result = tIntPtr;
                } else if (value2_type == tIntPtr) {
                    result = tIntPtr;
                } else {
                    CHECK_FAIL();
                }
            } else {
                CHECK_FAIL();
            }

            // TODO: for floats make sure it is an instruction that can take floats

#ifdef __EXPLICIT_ZERO_CHECK__
            if (inst.opcode == CEE_DIV || inst.opcode == CEE_DIV_UN || inst.opcode == CEE_REM || inst.opcode == CEE_REM_UN) {

                jit_value_type_t typ;
                if (value2_type == tInt32) {
                    typ = JIT_TYPE_I32;
                } else {
                    typ = JIT_TYPE_I64;
                }

                jit_block_t valid = jit_builder_create_block(builder);
                jit_value_t zero = jit_builder_build_iconst(builder, typ, 0);
                jit_value_t cmp = jit_builder_build_icmp(builder,
                                                         JIT_ICMP_NE, JIT_TYPE_I64,
                                                         zero, value2);

                jit_block_t throw_divide_by_zero = jit_throw_builtin_exception(ctx, builder, JIT_EXCEPTION_DIVIDE_BY_ZERO);
                jit_builder_build_brcond(builder, cmp, valid, throw_divide_by_zero);

                // valid branch, continue forward
                jit_builder_set_block(builder, valid);
            }
#endif

            // create the operation
            jit_value_t result_value;
            switch (inst.opcode) {
                case CEE_ADD: result_value = jit_builder_build_iadd(builder, value1, value2); break;
                case CEE_SUB: result_value = jit_builder_build_isub(builder, value1, value2); break;
                case CEE_AND: result_value = jit_builder_build_and(builder, value1, value2); break;
                case CEE_OR: result_value = jit_builder_build_or(builder, value1, value2); break;
                case CEE_XOR: result_value = jit_builder_build_xor(builder, value1, value2); break;
                case CEE_MUL: result_value = jit_builder_build_imul(builder, value1, value2); break;
                case CEE_DIV: result_value = jit_builder_build_sdiv(builder, value1, value2); break;
                case CEE_DIV_UN: result_value = jit_builder_build_udiv(builder, value1, value2); break;
                case CEE_REM: result_value = jit_builder_build_srem(builder, value1, value2); break;
                case CEE_REM_UN: result_value = jit_builder_build_urem(builder, value1, value2); break;
                default: CHECK_FAIL();
            }

            // push it to the stack
            CHECK_AND_RETHROW(eval_stack_push(stack, result, result_value));
        } break;

        // bitwise not, emulate with value ^ ~0
        case CEE_NOT: {
            // pop the item
            jit_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // type check, also get the ones value for the width
            // for the xor operation
            jit_value_t ones;
            if (value_type == tInt32) {
                ones = jit_builder_build_iconst(builder, JIT_TYPE_I32, (uint32_t)~0u);
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                ones = jit_builder_build_iconst(builder, JIT_TYPE_I64, (uint64_t)~0ull);
            } else {
                CHECK_FAIL();
            }

            // create the operation
            jit_value_t result_value = jit_builder_build_xor(builder, value, ones);

            // push it to the stack
            CHECK_AND_RETHROW(eval_stack_push(stack, value_type, result_value));
        } break;

        // negation, emulate with 0 - value
        case CEE_NEG: {
            // pop the item
            jit_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // type check, also get the ones value for the width
            // for the xor operation
            jit_value_type_t type;
            if (value_type == tInt32) {
                type = JIT_TYPE_I32;
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                type = JIT_TYPE_I64;
            } else {
                CHECK_FAIL();
            }

            // TODO: floating point

            // create the operation
            jit_value_t zero = jit_builder_build_iconst(builder, type, 0);
            jit_value_t result_value = jit_builder_build_isub(builder, zero, value);

            // push it to the stack
            CHECK_AND_RETHROW(eval_stack_push(stack, value_type, result_value));
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Integer conversions
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        case CEE_CONV_I1:
        case CEE_CONV_U1: {
            jit_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // make sure it is an integer type
            CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);

            if (value_type != tInt32) {
                // truncate to a 32bit value
                value = jit_builder_build_itrunc(builder, value);
            }

            if (inst.opcode == CEE_CONV_U1) {
                value = jit_builder_build_and(builder, value,
                                                 jit_builder_build_iconst(builder,
                                                                             JIT_TYPE_I32,
                                                                             0xFF));
            } else {
                value = jit_builder_build_sfill(builder, 8, value);
            }

            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
        } break;


        case CEE_CONV_I2:
        case CEE_CONV_U2: {
            jit_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // make sure it is an integer type
            CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);

            if (value_type != tInt32) {
                // truncate to a 32bit value
                value = jit_builder_build_itrunc(builder, value);
            }

            if (inst.opcode == CEE_CONV_U2) {
                value = jit_builder_build_and(builder, value,
                                                 jit_builder_build_iconst(builder,
                                                                             JIT_TYPE_I32,
                                                                             0xFFFF));
            } else {
                value = jit_builder_build_sfill(builder, 16, value);
            }

            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
        } break;

        case CEE_CONV_I4:
        case CEE_CONV_U4: {
            jit_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            if (value_type == tInt32) {
                // nothing to do, push it again
                CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                // truncate to a 32bit value
                value = jit_builder_build_itrunc(builder, value);
                CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
            } else {
                // invalid type
                CHECK_FAIL();
            }
        } break;

        case CEE_CONV_I:
        case CEE_CONV_I8: {
            jit_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // choose based on the opcode
            RuntimeTypeInfo push_type = inst.opcode == CEE_CONV_I ? tIntPtr : tInt64;

            if (value_type == tInt32) {
                // comes from a 32bit integer, first extend to a 64bit integer
                // and then sign extend it
                value = jit_builder_build_iext(builder, value);
                value = jit_builder_build_sfill(builder, 32, value);
                CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                // nothing to do, push it again
                CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
            } else {
                // invalid type
                CHECK_FAIL();
            }
        } break;

            // we can also handle in here the overflow versions because
            // converting an unsigned i32/i64 to an unsigned i64 can never fail
        case CEE_CONV_U:
        case CEE_CONV_U8: {
            jit_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // choose based on the opcode
            RuntimeTypeInfo push_type = inst.opcode == CEE_CONV_U ? tIntPtr : tInt64;

            if (value_type == tInt32) {
                // comes from a 32bit integer, first extend to a 64bit integer
                // and then zero extend it
                value = jit_builder_build_iext(builder, value);
                value = jit_builder_build_and(builder, value,
                                                 jit_builder_build_iconst(builder,
                                                                             JIT_TYPE_I64,
                                                                             0xFFFFFFFF));
                CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                // nothing to do, push it again
                CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
            } else {
                // invalid type
                CHECK_FAIL();
            }
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Type conversions with overflow checking
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
        //
        // | to / from | uint32            | uint64                | int32                    | int64                    |
        // |-----------|-------------------|-----------------------|--------------------------|--------------------------|
        // | uint8     | value ult 0x100   | value ult 0x100       | value ult 0x100          | value ult 0x100          |
        // | uint16    | value ult 0x10000 | value ult 0x10000     | value ult 0x10000        | value ult 0x10000        |
        // | uint32    | nop               | value ult 0x100000000 | -1 slt value             | value ult 0x100000000    |
        // | uint64    | nop               | nop                   | -1 slt value             | -1 slt value             |
        // | int8      | value ult 0x80    | value ult 0x80        | sfill 8 and eq original  | sfill 8 and eq original  |
        // | int16     | value ult 0x8000  | value ult 0x8000      | sfill 16 and eq original | sfill 16 and eq original |
        // | int32     | -1 slt value      | -1 slt value          | nop                      | sfill 32 and eq original |
        // | int64     | nop               | -1 slt value          | nop                      | nop                      |
        //

        case CEE_CONV_OVF_U1:
        case CEE_CONV_OVF_I1:
        case CEE_CONV_OVF_U1_UN:
        case CEE_CONV_OVF_I1_UN:
        case CEE_CONV_OVF_U2:
        case CEE_CONV_OVF_I2:
        case CEE_CONV_OVF_U2_UN:
        case CEE_CONV_OVF_I2_UN:
        case CEE_CONV_OVF_U4:
        case CEE_CONV_OVF_I4:
        case CEE_CONV_OVF_U4_UN:
        case CEE_CONV_OVF_I4_UN: {
            jit_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // type check
            jit_value_type_t type;
            uint64_t minus_one;
            if (value_type == tInt32) {
                // special case of nop
                //      uint32 -> uint32
                //      int32 -> int32
                if (inst.opcode == CEE_CONV_OVF_U4_UN || inst.opcode == CEE_CONV_OVF_I4) {
                    CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
                    break;
                }

                type = JIT_TYPE_I32;
                minus_one = (uint32_t)-1;
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                type = JIT_TYPE_I64;
                minus_one = (uint64_t)-1;
            } else {
                CHECK_FAIL();
            }

            jit_value_t cond;
            if (
                (value_type == tInt32 && inst.opcode == CEE_CONV_OVF_U4) ||
                (inst.opcode == CEE_CONV_OVF_I4_UN)
            ) {
                // special case:
                //      int32 -> uint32
                //      uint32/uint64 -> int32
                // perform a signed positive check
                cond = jit_builder_build_icmp(builder, JIT_ICMP_SLT, JIT_TYPE_I64,
                                                 jit_builder_build_iconst(builder, type, minus_one),
                                                 value);
            } else if (
                inst.opcode == CEE_CONV_OVF_I1 ||
                inst.opcode == CEE_CONV_OVF_I2 ||
                inst.opcode == CEE_CONV_OVF_I4
            ) {
                // int32/int64 -> int8
                // int32/int64 -> int16
                // int64 -> int32

                // get the correct bit count
                uint64_t bit_width;
                if (inst.opcode == CEE_CONV_OVF_I1) {
                    bit_width = 8;
                } else if (inst.opcode == CEE_CONV_OVF_I2) {
                    bit_width = 16;
                } else if (inst.opcode == CEE_CONV_OVF_I4) {
                    bit_width = 32;
                } else {
                    CHECK_FAIL();
                }

                // sign extend and check they are still the same
                jit_value_t signed_value = jit_builder_build_sfill(builder, bit_width, value);
                cond = jit_builder_build_icmp(builder, JIT_ICMP_EQ, JIT_TYPE_I64,
                                                 value, signed_value);
            } else {
                // get the correct max value
                uint64_t max_value;
                if (inst.opcode == CEE_CONV_OVF_U1 || inst.opcode == CEE_CONV_OVF_U1_UN) {
                    max_value = 0x100;
                } else if (inst.opcode == CEE_CONV_OVF_U2 || inst.opcode == CEE_CONV_OVF_U2_UN) {
                    max_value = 0x10000;
                } else if (inst.opcode == CEE_CONV_OVF_U4 || inst.opcode == CEE_CONV_OVF_U4_UN) {
                    max_value = 0x100000000;
                } else if (inst.opcode == CEE_CONV_OVF_I1_UN) {
                    max_value = 0x80;
                } else if (inst.opcode == CEE_CONV_OVF_I2_UN) {
                    max_value = 0x8000;
                } else {
                    CHECK_FAIL();
                }

                // perform an unsigned less than check, this is fine for both the signed and unsigned
                // input because we are using twos complement
                cond = jit_builder_build_icmp(builder, JIT_ICMP_ULT, JIT_TYPE_I64, value,
                                                 jit_builder_build_iconst(builder, type,
                                                                             max_value));
            }

            // perform the branch
            jit_block_t valid = jit_builder_create_block(builder);
            jit_block_t invalid = jit_throw_builtin_exception(ctx, builder, JIT_EXCEPTION_OVERFLOW);
            jit_builder_build_brcond(builder, cond, valid, invalid);

            // valid path, push the new valid
            jit_builder_set_block(builder, valid);

            // truncate if came from a bigger value
            if (value_type != tInt32) {
                value = jit_builder_build_itrunc(builder, value);
            }
            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32, value));
        } break;

        case CEE_CONV_OVF_I8:
        case CEE_CONV_OVF_I:
        case CEE_CONV_OVF_U8_UN:
        case CEE_CONV_OVF_U_UN: {
            jit_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // choose the type to push
            RuntimeTypeInfo push_type;
            if (inst.opcode == CEE_CONV_OVF_U_UN || inst.opcode == CEE_CONV_OVF_I) {
                push_type = tIntPtr;
            } else {
                push_type = tInt64;
            }

            // type check
            CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);

            // if the input was 32bit zero/sign extend it,
            // based on the signed -> signed or the unsigned -> unsigned
            if (value_type == tInt32) {
                value = jit_builder_build_iext(builder, value);
                if (inst.opcode == CEE_CONV_OVF_I8 || inst.opcode == CEE_CONV_OVF_I) {
                    value = jit_builder_build_sfill(builder, 32, value);
                } else {
                    value = jit_builder_build_and(builder, value,
                                                     jit_builder_build_iconst(builder,
                                                                                 JIT_TYPE_I64, 0xFFFFFFFF));
                }
            }

            // just push the same as the wanted type
            CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
        } break;

        case CEE_CONV_OVF_I8_UN:
        case CEE_CONV_OVF_I_UN:
        case CEE_CONV_OVF_U8:
        case CEE_CONV_OVF_U: {
            jit_value_t value;
            RuntimeTypeInfo value_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &value_type, &value));

            // choose the type to push
            RuntimeTypeInfo push_type;
            if (inst.opcode == CEE_CONV_OVF_U || inst.opcode == CEE_CONV_OVF_I_UN) {
                push_type = tIntPtr;
            } else {
                push_type = tInt64;
            }

            // type check
            jit_value_type_t type;
            uint64_t minus_one;
            if (value_type == tInt32) {
                if (inst.opcode == CEE_CONV_OVF_I8_UN || inst.opcode == CEE_CONV_OVF_I_UN) {
                    // special case, uint32 -> int64 is always valid
                    goto skip_positive_check;
                }

                type = JIT_TYPE_I32;
                minus_one = (uint32_t)-1;
            } else if (value_type == tInt64 || value_type == tIntPtr) {
                type = JIT_TYPE_I64;
                minus_one = (uint64_t)-1;
            } else {
                CHECK_FAIL();
            }

            // make sure is positive by doing a sign check
            jit_value_t cond = jit_builder_build_icmp(builder, JIT_ICMP_SLT, JIT_TYPE_I64,
                                                            jit_builder_build_iconst(builder, type, minus_one),
                                                            value);

            // perform the branch
            jit_block_t valid = jit_builder_create_block(builder);
            jit_block_t invalid = jit_throw_builtin_exception(ctx, builder, JIT_EXCEPTION_OVERFLOW);
            jit_builder_build_brcond(builder, cond, valid, invalid);

            // valid path, push the new valid
            jit_builder_set_block(builder, valid);

        skip_positive_check:
            // if the input was 32bit,
            // in this case we only either have unsigned -> signed or signed -> unsigned, in both
            // cases we make sure that the value is positive, so we can just do a normal zero extension
            if (value_type == tInt32) {
                value = jit_builder_build_iext(builder, value);
                value = jit_builder_build_and(builder, value,
                                                 jit_builder_build_iconst(builder,
                                                                             JIT_TYPE_I64, 0xFFFFFFFF));
            }

            // just push the same as the wanted type
            CHECK_AND_RETHROW(eval_stack_push(stack, push_type, value));
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Allocation
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        case CEE_NEWARR: {
            jit_value_t num_elems;
            RuntimeTypeInfo num_elems_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &num_elems_type, &num_elems));

            // type check
            CHECK(num_elems_type == tInt32 || num_elems_type == tIntPtr);

            // extend to 64bit if needed
            if (num_elems_type == tInt32) {
                num_elems = jit_builder_build_iext(builder, num_elems);
                num_elems = jit_builder_build_sfill(builder, 32, num_elems);
            }

            // get the array type we are allocating
            RuntimeTypeInfo array_type = NULL;
            CHECK_AND_RETHROW(tdn_get_array_type(inst.operand.type, &array_type));

            // calculate the array size we will need
            jit_value_t array_size = jit_builder_build_imul(builder, num_elems,
                                                                  jit_builder_build_iconst(builder,
                                                                                              JIT_TYPE_I64,
                                                                                              inst.operand.type->StackSize));
            array_size = jit_builder_build_iadd(builder, array_size, jit_builder_build_iconst(builder,
                                                                                                    JIT_TYPE_I64,
                                                                                                    sizeof(struct Array)));

            // call the gc_new to allocate the new object
            jit_value_t array = jit_builder_build_call(builder, m_builtin_gc_new, 2, (jit_value_t[]){
                jit_builder_build_iconst(builder, JIT_TYPE_PTR, (uint64_t)array_type), array_size
            });

            // and finally set the length of the array
            jit_builder_build_store(builder, JIT_MEM_SIZE_4, num_elems,
                                       jit_builder_build_ptroff(builder, array,
                                                                   jit_builder_build_iconst(builder,
                                                                                               JIT_TYPE_I64,
                                                                                               offsetof(struct Array, Length))));

            // push the array pointer
            CHECK_AND_RETHROW(eval_stack_push(stack, array_type, array));
        } break;

        case CEE_BOX: {
            jit_value_t val;
            RuntimeTypeInfo val_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &val_type, &val));

            CHECK(tdn_type_verifier_assignable_to(val_type, inst.operand.type));

            if (tdn_type_is_referencetype(inst.operand.type)) {
                // for reference type there is nothing special todo, it stays as is
                CHECK_AND_RETHROW(eval_stack_push(stack, val_type, val));

            } else {
                // this is the struct path, it may have nullable and may not have nullable
                bool is_nullable = val_type->GenericTypeDefinition == tNullable;

                // if we have a Nullable<> then prepare the allocation and movement
                size_t has_value_offset = -1;
                size_t val_offset = -1;
                jit_block_t next;
                if (is_nullable) {
                    // get the needed offsets for the given type
                    RuntimeFieldInfo_Array fields = val_type->DeclaredFields;
                    for (int i = 0; i < fields->Length; i++) {
                        RuntimeFieldInfo field = fields->Elements[i];
                        if (tdn_compare_string_to_cstr(field->Name, "_hasValue")) {
                            has_value_offset = field->FieldOffset;
                        } else if (tdn_compare_string_to_cstr(field->Name, "_value")) {
                            val_offset = field->FieldOffset;
                        }
                    }
                    CHECK(has_value_offset != -1);
                    CHECK(val_offset != -1);

                    // the needed pointers
                    jit_value_t has_value_ptr = jit_builder_build_ptroff(builder, val,
                                                                               jit_builder_build_iconst(builder,
                                                                                                           JIT_TYPE_I64,
                                                                                                           has_value_offset));

                    jit_value_t value_ptr = jit_builder_build_ptroff(builder, val,
                                                                           jit_builder_build_iconst(builder,
                                                                                                       JIT_TYPE_I64,
                                                                                                       has_value_offset));

                    // check if we have a value
                    jit_value_t has_value = jit_builder_build_load(builder, JIT_MEM_SIZE_1, JIT_TYPE_I32,
                                                                         has_value_ptr);
                    has_value = jit_builder_build_icmp(builder,
                                                          JIT_ICMP_NE,
                                                          JIT_TYPE_I64,
                                                          has_value,
                                                          jit_builder_build_iconst(builder,
                                                                                      JIT_TYPE_I32,
                                                                                      0));

                    // we are using a phi to choose the correct
                    next = jit_builder_create_block(builder);
                    jit_block_t allocate = jit_builder_create_block(builder);
                    jit_builder_build_brcond(builder, has_value, allocate, next);

                    // perform the copy path
                    jit_builder_set_block(builder, allocate);

                    // if we have an integer type we need to read it, to make it consistent with
                    // whatever we will have in the non-nullable case
                    val_type = tdn_get_verification_type(val_type->GenericArguments->Elements[0]);
                    if (val_type == tByte) {
                        val = jit_builder_build_load(builder, JIT_MEM_SIZE_1, JIT_TYPE_I32, value_ptr);
                    } else if (val_type == tInt16) {
                        val = jit_builder_build_load(builder, JIT_MEM_SIZE_2, JIT_TYPE_I32, value_ptr);
                    } else if (val_type == tInt32) {
                        val = jit_builder_build_load(builder, JIT_MEM_SIZE_4, JIT_TYPE_I32, value_ptr);
                    } else if (val_type == tInt64) {
                        val = jit_builder_build_load(builder, JIT_MEM_SIZE_8, JIT_TYPE_I32, value_ptr);
                    } else {
                        // otherwise it is just the pointer since its a struct
                        val = value_ptr;
                    }
                }

                // allocate it
                jit_value_t args[2] = {
                    jit_builder_build_iconst(builder, JIT_TYPE_PTR, (uint64_t)inst.operand.type),
                    jit_builder_build_iconst(builder, JIT_TYPE_I64, sizeof(struct Object) + inst.operand.type->HeapSize),
                };
                jit_value_t obj = jit_builder_build_call(builder, m_builtin_gc_new, 2, args);
                jit_value_t obj_value = jit_builder_build_ptroff(builder, obj,
                                                                       jit_builder_build_iconst(builder,
                                                                                                   JIT_TYPE_I64,
                                                                                                   sizeof(struct Object)));

                // copy it, if its an integer copy properly, which will most likely truncate it
                if (val_type == tByte) {
                    jit_builder_build_store(builder, JIT_MEM_SIZE_1, val, obj_value);
                } else if (val_type == tInt16) {
                    jit_builder_build_store(builder, JIT_MEM_SIZE_2, val, obj_value);
                } else if (val_type == tInt32) {
                    jit_builder_build_store(builder, JIT_MEM_SIZE_4, val, obj_value);
                } else if (val_type == tInt64) {
                    jit_builder_build_store(builder, JIT_MEM_SIZE_8, val, obj_value);
                } else {
                    if (val_type->IsUnmanaged) {
                        jit_emit_memcpy(builder, obj_value, val, val_type->StackSize);
                    } else {
                        jit_emit_gc_memcpy(builder, val_type, obj_value, val);
                    }
                }

                // and finally now that it is properly allocated we can setup the next path
                // with the proper phi
                if (is_nullable) {
                    jit_builder_build_branch(builder, next);

                    // now setup the phi
                    //  first input is the allocation path
                    //  second input is the no allocation path
                    jit_builder_set_block(builder, next);
                    jit_value_t inputs[2] = {
                        jit_builder_build_iconst(builder, JIT_TYPE_PTR, 0),
                        obj,
                    };
                    obj = jit_builder_build_phi(builder, JIT_TYPE_PTR, 2, inputs, NULL);
                }

                // finally push the object, will either be a phi result or just the object
                CHECK_AND_RETHROW(eval_stack_push(stack, tObject, obj));
            }
        } break;

        case CEE_UNBOX:
        case CEE_UNBOX_ANY: {
            jit_value_t obj;
            RuntimeTypeInfo obj_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &obj_type, &obj));

            // TODO: Nullable support

            // must be a reference type
            CHECK(tdn_type_is_referencetype(obj_type));
            CHECK(tdn_type_verifier_assignable_to(inst.operand.type, obj_type));

            if (tdn_type_is_referencetype(inst.operand.type)) {
                // unbox must be a value type
                CHECK(inst.opcode != CEE_UNBOX);

                // TODO: implement the correct behaviour for reference types
                CHECK_FAIL();
            } else {
#ifdef __EXPLICIT_NULL_CHECK__
                jit_emit_null_check(ctx, builder, obj);
#endif

                // we can inline the check since the type has to be the exact value type we are trying to unpack
                // this will take care of the null check as well
                jit_value_t object_type_offset = jit_builder_build_iconst(builder, JIT_TYPE_I64, offsetof(struct Object, ObjectType));
                jit_value_t object_type_ptr = jit_builder_build_ptroff(builder, obj, object_type_offset);
                jit_value_t object_type = jit_builder_build_load(builder, JIT_MEM_SIZE_8, JIT_TYPE_PTR, object_type_ptr);

                jit_block_t valid = jit_builder_create_block(builder);
                jit_block_t invalid = jit_throw_builtin_exception(ctx, builder, JIT_EXCEPTION_INVALID_CAST);

                jit_value_t wanted_type = jit_builder_build_iconst(builder, JIT_TYPE_PTR, (uint64_t)inst.operand.type);
                jit_value_t is_wanted_type = jit_builder_build_icmp(builder, JIT_ICMP_EQ, JIT_TYPE_I64, object_type, wanted_type);
                jit_builder_build_brcond(builder, is_wanted_type, valid, invalid);

                // valid path, actually emit it
                jit_builder_set_block(builder, valid);

                // compute the address of the pointer
                jit_value_t object_size = jit_builder_build_iconst(builder, JIT_TYPE_I64, sizeof(struct Object));
                jit_value_t value_type_ptr = jit_builder_build_ptroff(builder, obj, object_size);

                if (inst.opcode == CEE_UNBOX) {
                    // for unbox just push the ref to it
                    RuntimeTypeInfo ref_type = NULL;
                    CHECK_AND_RETHROW(tdn_get_byref_type(inst.operand.type, &ref_type));
                    CHECK_AND_RETHROW(eval_stack_push(stack, ref_type, value_type_ptr));
                } else {
                    // for unbox.any deref it as well
                    if (jit_is_struct_type(obj_type)) {
                        jit_value_t location;
                        CHECK_AND_RETHROW(eval_stack_alloc(stack, builder, inst.operand.type, &location));
                        jit_emit_memcpy(builder, location, value_type_ptr, inst.operand.type->StackSize);
                    } else {
                        jit_value_t value = jit_builder_build_load(builder,
                                                                   get_jit_mem_size(inst.operand.type),
                                                                   get_jit_mem_type(inst.operand.type),
                                                                   value_type_ptr);
                        RuntimeTypeInfo tracked_type = tdn_get_intermediate_type(inst.operand.type);
                        CHECK_AND_RETHROW(eval_stack_push(stack, tracked_type, value));
                    }
                }
            }

        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Indirect access
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////

        case CEE_INITOBJ: {
            jit_value_t dest;
            RuntimeTypeInfo dest_type;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &dest_type, &dest));

            // must be a ByRef
            CHECK(dest_type->IsByRef);
            dest_type = dest_type->ElementType;

            // must be assignable to properly
            CHECK(tdn_type_assignable_to(inst.operand.type, dest_type),
                  "%T assignable-to %T", inst.operand.type, dest_type);

            // get the base type so we know how to best assign it
            dest_type = tdn_get_verification_type(dest_type);
            if (dest_type == tByte) {
                jit_builder_build_store(builder, JIT_MEM_SIZE_1,
                                           jit_builder_build_iconst(builder, JIT_TYPE_I32, 0), dest);

            } else if (dest_type == tInt16) {
                jit_builder_build_store(builder, JIT_MEM_SIZE_2,
                                           jit_builder_build_iconst(builder, JIT_TYPE_I32, 0), dest);

            } else if (dest_type == tInt32) {
                jit_builder_build_store(builder, JIT_MEM_SIZE_4,
                                           jit_builder_build_iconst(builder, JIT_TYPE_I32, 0), dest);

            } else if (dest_type == tInt64) {
                jit_builder_build_store(builder, JIT_MEM_SIZE_8,
                                           jit_builder_build_iconst(builder, JIT_TYPE_I64, 0), dest);

            } else if (tdn_type_is_referencetype(dest_type) || dest_type == tIntPtr) {
                jit_builder_build_store(builder, JIT_MEM_SIZE_8,
                                           jit_builder_build_iconst(builder, JIT_TYPE_PTR, 0), dest);

            } else if (dest_type->IsUnmanaged) {
                jit_builder_build_call(builder, m_builtin_bzero, 2,
                                          (jit_value_t[]){
                                              dest,
                                              jit_builder_build_iconst(builder, JIT_TYPE_I64, dest_type->StackSize)
                                          });

            } else {
                jit_builder_build_call(builder, m_builtin_gc_bzero, 2,
                                          (jit_value_t[]){
                                                  jit_builder_build_iconst(builder, JIT_TYPE_PTR, (uint64_t)dest_type),
                                                  dest,
                                          });
            }
        } break;

        case CEE_LDIND_I1:
        case CEE_LDIND_I2:
        case CEE_LDIND_I4:
        case CEE_LDIND_I8:
        case CEE_LDIND_U1:
        case CEE_LDIND_U2:
        case CEE_LDIND_U4: {
            eval_stack_item_t addr;
            CHECK_AND_RETHROW(eval_stack_pop_item(stack, &addr));

            // must be a ByRef
            CHECK(addr.type->IsByRef);
            CHECK(addr.is_readable);
            RuntimeTypeInfo addr_type = addr.type->ElementType;

            // make sure the type is verified
            CHECK(tdn_type_assignable_to(addr_type, inst.operand.type));
            RuntimeTypeInfo stack_type = tdn_get_intermediate_type(addr_type);

            jit_value_type_t mem_type = get_jit_mem_type(addr_type);
            jit_mem_size_t mem_size = get_jit_mem_size(addr_type);
            jit_value_t result = jit_builder_build_load(builder, mem_size, mem_type, addr.value);

            // sign extend as needed
            if (addr_type == tSByte) {
                result = jit_builder_build_sfill(builder, 8, result);
            } else if (addr_type == tInt16) {
                result = jit_builder_build_sfill(builder, 16, result);
            }

            CHECK_AND_RETHROW(eval_stack_push(stack, stack_type, result));
        } break;

        case CEE_STIND_I1:
        case CEE_STIND_I2:
        case CEE_STIND_I4:
        case CEE_STIND_I8: {
            jit_value_t val;
            RuntimeTypeInfo val_type;
            eval_stack_item_t addr;
            CHECK_AND_RETHROW(eval_stack_pop(stack, &val_type, &val));
            CHECK_AND_RETHROW(eval_stack_pop_item(stack, &addr));

            // must be a ByRef
            CHECK(addr.type->IsByRef);
            CHECK(addr.is_writable);
            RuntimeTypeInfo addr_type = addr.type->ElementType;

            // make sure the type is verified
            CHECK(tdn_type_verifier_assignable_to(val_type, addr_type));

            jit_mem_size_t mem_size = get_jit_mem_size(addr_type);
            jit_builder_build_store(builder, mem_size, val, addr.value);
        } break;

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        // Misc operations
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

        // push the size in bytes of the given type
        case CEE_SIZEOF: {
            CHECK_AND_RETHROW(eval_stack_push(stack, tInt32,
                                              jit_builder_build_iconst(builder, JIT_TYPE_I32,
                                                                          inst.operand.type->StackSize)));
        } break;

        case CEE_NOP: {
            // do nothing
        } break;

        default:
            CHECK_FAIL();
    }

    // if we did not have the prefix this time make sure that we handled
    // all the prefixes that we had
    if (!ctx->last_was_prefix) {
        CHECK(!ctx->volatile_prefix);
        CHECK(ctx->constrained == NULL);
    }

cleanup:
    arrfree(call_args_types);
    arrfree(call_args_values);

    return err;
}

typedef struct jit_builder_ctx {
    RuntimeMethodBase method;
    tdn_err_t err;
} jit_builder_ctx_t;

static void jit_method_callback(jit_builder_t builder, void* _ctx) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_builder_ctx_t* builder_ctx = _ctx;
    RuntimeMethodBase method = builder_ctx->method;
    RuntimeExceptionHandlingClause_Array clauses = method->MethodBody->ExceptionHandlingClauses;

#ifdef JIT_IL_OUTPUT
    TRACE("%U::%U", method->DeclaringType->Name, method->Name);
#endif

    // setup the context that will be used for this method jitting
    jit_context_t ctx = {
        .method = method,
        .builder = builder,
    };
    eval_stack_t* stack = &ctx.stack;
    stack->max_depth = method->MethodBody->MaxStackSize;

    // take into account the first parameter might be an implicit
    // struct return pointer, we will just check if the stack size
    // is larger than 64bit, which can't be anything other than a
    // struct
    int args_offset = 0;
    if (method->ReturnParameter->ParameterType->StackSize > sizeof(uint64_t)) {
        args_offset = 1;
    }

    // get the this_type for future use
    RuntimeTypeInfo this_type = NULL;
    if (!method->Attributes.Static) {
        this_type = method->DeclaringType;
        if (tdn_type_is_valuetype(this_type)) {
            // this is a valuetype, the this is a reference
            CHECK_AND_RETHROW(tdn_get_byref_type(this_type, &this_type));
        }
    }

    // prepare the argument list by setting up their types, we
    // will later spill and set the value as needed
    int arg_count = (method->Parameters->Length + (this_type == NULL ? 0 : 1));
    arrsetlen(ctx.args, arg_count);
    for (int i = 0; i < arg_count; i++) {
        // resolve the parameter type
        RuntimeTypeInfo type;
        ParameterAttributes attributes = { .Attributes = 0 };
        CHECK_AND_RETHROW(jit_resolve_parameter_type(method, i, &type, &attributes));

        // set it up initially
        ctx.args[i].value = JIT_VALUE_INVALID;
        ctx.args[i].attributes = attributes;
        ctx.args[i].type = type;
        ctx.args[i].spilled = false;
    }

    // create the entry block
    bool created_entry_block = false;
    jit_block_t entry_block;

    // prepare the locals by allocating their stack slots already
    if (method->MethodBody->LocalVariables != NULL) {
        // using the entry block to zero the variables
        created_entry_block = true;
        entry_block = jit_builder_create_block(builder);
        jit_builder_set_block(builder, entry_block);
        jit_builder_set_entry_block(builder, entry_block);

        // setup the variables
        arrsetlen(ctx.locals, method->MethodBody->LocalVariables->Length);
        for (int i = 0; i < method->MethodBody->LocalVariables->Length; i++) {
            RuntimeLocalVariableInfo var = method->MethodBody->LocalVariables->Elements[i];

            // create it
            ctx.locals[i] = jit_builder_build_stackslot(builder,
                                                           var->LocalType->StackSize,
                                                           var->LocalType->StackAlignment);

            // clear it
            if (jit_is_struct_type(var->LocalType)) {
                jit_builder_build_call(builder, m_builtin_bzero, 2,
                                          (jit_value_t[]){
                                                  ctx.locals[i],
                                                  jit_builder_build_iconst(builder, JIT_TYPE_I64, var->LocalType->StackSize)
                                          });
            } else {
                jit_builder_build_store(builder,
                                           get_jit_mem_size(var->LocalType),
                                           jit_builder_build_iconst(builder, get_jit_mem_type(var->LocalType), 0),
                                           ctx.locals[i]);
            }
        }
    }

    //------------------------------------------------------------------------------------------------------------------
    // first pass, find all of the labels, this will
    // also create all the different basic blocks on
    // the way
    //------------------------------------------------------------------------------------------------------------------

    // get the rest of the blocks by creating the labels
    bool has_starg0_or_ldarga0 = false;
    uint32_t pc = 0;
    tdn_il_control_flow_t flow_control = TDN_IL_CF_FIRST;
    while (pc != method->MethodBody->ILSize) {
        // decode instruction
        tdn_il_inst_t inst;
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));
        tdn_normalize_inst(&inst);

        // don't support break for now
        CHECK(inst.control_flow != TDN_IL_CF_BREAK);

        //
        // check if we need to spill an argument
        //
        if (
            inst.opcode == CEE_LDARGA ||
            inst.opcode == CEE_STARG
        ) {
            int arg = inst.operand.variable;
            CHECK(arg < arg_count);
            RuntimeTypeInfo type = ctx.args[arg].type;

            // using the entry block to spill the parameters
            if (!created_entry_block) {
                created_entry_block = true;
                entry_block = jit_builder_create_block(builder);
                jit_builder_set_block(builder, entry_block);
                jit_builder_set_entry_block(builder, entry_block);
            }

            // create a stackslot for the spill
            if (!ctx.args[arg].spilled) {
                ctx.args[arg].value = jit_builder_build_stackslot(builder, type->StackSize, type->StackAlignment);
                ctx.args[arg].spilled = true;

                // store it, if its a value-type we need to copy it instead
                jit_value_t param_ref = jit_builder_build_param_ref(builder, args_offset + arg);
                if (jit_is_struct_type(type)) {
                    jit_emit_memcpy(builder,
                                    ctx.args[arg].value,
                                    param_ref,
                                    type->StackSize);
                } else {
                    jit_builder_build_store(builder,
                                               get_jit_mem_size(type),
                                               param_ref,
                                               ctx.args[arg].value);
                }
            }

            // needed for verifying call
            if (arg == 0) {
                has_starg0_or_ldarga0 = true;
            }
        }

        //
        // Handle label creation
        //

        // we have a branch target, create the label at that location
        if (inst.operand_type == TDN_IL_BRANCH_TARGET) {
            jit_add_label_location(&ctx, inst.operand.branch_target);
        }

        // if we are coming from a conditional branch then we can get to here
        // as well, so mark as a label
        if (flow_control == TDN_IL_CF_COND_BRANCH) {
            jit_add_label_location(&ctx, pc);
        }

        // check if we already have a label at this location and the last opcode
        // can flow into this location
        bool has_label = jit_get_label_location_index(&ctx, pc, true) >= 0;
        if (
            has_label &&
            (
                flow_control == TDN_IL_CF_NEXT ||
                flow_control == TDN_IL_CF_CALL ||
                flow_control == TDN_IL_CF_BREAK
            )
        ) {
            jit_add_label_location(&ctx, pc);
        }

        pc += inst.length;
        flow_control = inst.control_flow;
    }

    // finish up the args
    for (int i = 0; i < arrlen(ctx.args); i++) {
        if (!ctx.args[i].spilled) {
            ctx.args[i].value = jit_builder_build_param_ref(builder, args_offset + i);
        }
    }

    //------------------------------------------------------------------------------------------------------------------
    // The second pass
    //------------------------------------------------------------------------------------------------------------------

    // setup the try and fault handler regions
    size_t region_count = clauses != NULL ? clauses->Length : 0;
    arrsetlen(ctx.protected_regions, region_count);
    arrsetlen(ctx.handler_regions, region_count);

    memset(ctx.protected_regions, 0, region_count * sizeof(jit_region_t));
    memset(ctx.handler_regions, 0, region_count * sizeof(jit_region_t));

    // setup all the regions
    for (int i = 0; i < region_count; i++) {
        RuntimeExceptionHandlingClause c = clauses->Elements[i];

        // if this is a finally we need to create a common finally handlers block
        finally_handlers_t* handler = NULL;
        if (c->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
            handler = tdn_host_mallocz(sizeof(finally_handlers_t));
            CHECK_ERROR(handler != NULL, TDN_ERROR_OUT_OF_MEMORY);
        }

        memset(&ctx.protected_regions[i], 0, sizeof(jit_region_t));
        ctx.protected_regions[i].clause = c;
        ctx.protected_regions[i].clause_index = i;
        ctx.protected_regions[i].pc_start = c->TryOffset;
        ctx.protected_regions[i].pc_end = c->TryOffset + c->TryLength;
        ctx.protected_regions[i].finally_handlers = handler;
        ctx.protected_regions[i].current_finally_path = INT32_MAX;
        CHECK_AND_RETHROW(create_region_labels(&ctx, &ctx.protected_regions[i]));

        memset(&ctx.handler_regions[i], 0, sizeof(jit_region_t));
        ctx.handler_regions[i].clause = c;
        ctx.handler_regions[i].clause_index = i;
        ctx.handler_regions[i].pc_start = c->HandlerOffset;
        ctx.handler_regions[i].pc_end = c->HandlerOffset + c->HandlerLength;
        ctx.handler_regions[i].is_handler = true;
        ctx.handler_regions[i].finally_handlers = handler;
        ctx.handler_regions[i].current_finally_path = handler == NULL ? INT32_MAX : -1;
        CHECK_AND_RETHROW(create_region_labels(&ctx, &ctx.handler_regions[i]));
    }

    // finally create the top most region
    // the index is going to be set to the index of the last
    // clause so we will search all the handlers if we are
    // in the root
    jit_region_t root_region = {
        .pc_start = 0,
        .pc_end = method->MethodBody->ILSize,
        .clause = NULL,
        .clause_index = region_count - 1,
    };
    CHECK_AND_RETHROW(create_region_labels(&ctx, &root_region));

    // either branch from the entry to the region entry or set the region
    // entry as the actual entry
    if (created_entry_block) {
        jit_builder_build_branch(builder, root_region.entry_block);
    } else {
        jit_builder_set_entry_block(builder, root_region.entry_block);
    }

    // and push it as the starting region
    arrpush(ctx.regions, &root_region);

    // the root is the first block
    root_region.has_block = true;
    jit_builder_set_block(builder, root_region.entry_block);

    // for debug
    int indent = 0;

    pc = 0;
    flow_control = TDN_IL_CF_FIRST;
    while (pc < method->MethodBody->ILSize) {
        // decode the instruction
        tdn_il_inst_t inst;
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));

        //
        // Access checks
        //
        if (inst.operand_type == TDN_IL_FIELD) {
            CHECK(check_field_access(method, inst.operand.field, inst.operand_token),
                  "Failed to access %T::%U from %T::%U",
                  inst.operand.field->DeclaringType, inst.operand.field->Name,
                  method->DeclaringType, method->Name
            );
        } else if (inst.operand_type == TDN_IL_METHOD) {
            CHECK(check_method_access(method, inst.operand.method, inst.operand_token),
                "Failed to access %T::%U from %T::%U",
                inst.operand.method->DeclaringType, inst.operand.method->Name,
                method->DeclaringType, method->Name
            );
        } else if (inst.operand_type == TDN_IL_TYPE) {
            CHECK(check_type_access(method, inst.operand.type, inst.operand_token));
        }

#ifdef JIT_IL_OUTPUT
        //--------------------------------------------------------------------------------------------------------------
        // debug prints
        //--------------------------------------------------------------------------------------------------------------

        for (int i = 0; clauses != NULL && i < clauses->Length; i++) {
            RuntimeExceptionHandlingClause c = clauses->Elements[i];
            if (c->TryOffset == pc) {
                tdn_host_printf("[*] \t\t\t%*s.try\n", indent, "");
                tdn_host_printf("[*] \t\t\t%*s{\n", indent, "");
                indent += 4;

            } else if (c->HandlerOffset == pc) {
                if (c->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
                    string_builder_t temp_builder = {};
                    string_builder_push_type_signature(&temp_builder, c->CatchType);
                    tdn_host_printf("[*] \t\t\t%*scatch %s\n", indent, "", string_builder_build(&temp_builder));
                    string_builder_free(&temp_builder);
                } else if (c->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
                    tdn_host_printf("[*] \t\t\t%*sfinally\n", indent, "");
                } else {
                    CHECK_FAIL();
                }
                tdn_host_printf("[*] \t\t\t%*s{\n", indent, "");
                indent += 4;
            }
        }

        // check if we are getting nested

        tdn_host_printf("[*] \t\t\t%*sIL_%04x: %s", indent, "", pc, tdn_get_opcode_name(inst.opcode));
        switch (inst.operand_type) {
            case TDN_IL_BRANCH_TARGET: tdn_host_printf(" IL_%04x", inst.operand.branch_target); break;
            case TDN_IL_NO_OPERAND: break;
            case TDN_IL_VARIABLE: tdn_host_printf(" %d", inst.operand.variable); break;
            case TDN_IL_INT8: tdn_host_printf(" %d", inst.operand.int8); break;
            case TDN_IL_INT32: tdn_host_printf(" %d", inst.operand.int32); break;
            case TDN_IL_INT64: tdn_host_printf(" %lld", (long long int)inst.operand.int64); break;
            case TDN_IL_FLOAT32: tdn_host_printf(" %f", inst.operand.float32); break;
            case TDN_IL_FLOAT64: tdn_host_printf(" %f", inst.operand.float64); break;

            case TDN_IL_METHOD: {
                string_builder_t tmp_builder = {};
                string_builder_push_method_signature(&tmp_builder, inst.operand.method, true);
                tdn_host_printf(" %s", string_builder_build(&tmp_builder));
                string_builder_free(&tmp_builder);
            } break;

            case TDN_IL_FIELD: {
                string_builder_t tmp_builder = {};
                string_builder_push_type_signature(&tmp_builder, inst.operand.field->DeclaringType);
                tdn_host_printf(" %s::%U", string_builder_build(&tmp_builder), inst.operand.field->Name);
                string_builder_free(&tmp_builder);
            } break;

            case TDN_IL_TYPE: {
                string_builder_t tmp_builder = {};
                string_builder_push_type_signature(&tmp_builder, inst.operand.type);
                tdn_host_printf(" %s", string_builder_build(&tmp_builder));
                string_builder_free(&tmp_builder);
            } break;

            case TDN_IL_STRING: tdn_host_printf(" %U", inst.operand.string); break;
            case TDN_IL_SWITCH: CHECK_FAIL();
        }
        tdn_host_printf("\n");
#endif


        //--------------------------------------------------------------------------------------------------------------
        // Actual jit handling
        //--------------------------------------------------------------------------------------------------------------
        CHECK(arrlen(ctx.regions) > 0);
        jit_region_t* region = arrlast(ctx.regions);
        jit_region_t* new_region = NULL;

        // check if we entered a new region or not
        int start_index = region->clause_index == -1 ? 0 : region->clause_index;
        for (int i = 0; i <= region->clause_index; i++) {
            jit_region_t* protected_region = &ctx.protected_regions[i];
            jit_region_t* handler_region = &ctx.handler_regions[i];

            if (pc == protected_region->pc_start) {
                new_region = protected_region;

                // we got into a protected region
                if (
                    flow_control == TDN_IL_CF_NEXT ||
                    flow_control == TDN_IL_CF_CALL ||
                    flow_control == TDN_IL_CF_COND_BRANCH ||
                    flow_control == TDN_IL_CF_BREAK ||
                    flow_control == TDN_IL_CF_FIRST
                ) {
                    // we got a fallthrough, stack must be empty
                    CHECK(arrlen(stack->stack) == 0);

                    // put a branch into this region
                    jit_builder_build_branch(builder, new_region->entry_block);
                }

                // we will already set it in here
                new_region->has_block = true;
                jit_builder_set_block(builder, new_region->entry_block);
            } else if (pc == handler_region->pc_start) {
                // for finally we have duplicate paths for each of the valid entries
                // first is the fault path, the rest are the valid paths, get the correct
                // region to pass on
                if (handler_region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY) {
                    int finaly_path_index = handler_region->current_finally_path++;
                    CHECK(finaly_path_index < hmlen(handler_region->finally_handlers->finally_paths));
                    if (finaly_path_index >= 0) {
                        handler_region = handler_region->finally_handlers->finally_paths[finaly_path_index].region;
                    }
                }
                new_region = handler_region;

                // the current region no longer has a block it can use
                region->has_block = false;

                // we got into a fault handler, we are not allowed to fall into it
                CHECK(
                    flow_control == TDN_IL_CF_RETURN ||
                    flow_control == TDN_IL_CF_BRANCH ||
                    flow_control == TDN_IL_CF_THROW
                );

                // make sure it is empty
                CHECK(arrlen(stack->stack) == 0);

                // we will already set it in here
                new_region->has_block = true;
                jit_builder_set_block(builder, new_region->entry_block);

                // for exception handlers we need to set the first item as the exception
                // TODO: handler filters
                if (handler_region->clause->Flags == COR_ILEXCEPTION_CLAUSE_EXCEPTION) {
                    CHECK_AND_RETHROW(eval_stack_push(stack, new_region->clause->CatchType,
                                    jit_builder_build_call(builder,
                                                              m_builtin_get_exception, 0, NULL)));
                }
            } else {
                // continue on
                continue;
            }

            // means we found something
            break;
        }

        // if we entered a new region
        if (new_region != NULL) {
            // no longer has a block to continue on, must continue from a label or something
            region->has_block = false;
            region = new_region;
            arrpush(ctx.regions, new_region);
        }

        // check if there are more labels
        bool switched_block = false;
        if (region->label_index < arrlen(region->labels)) {
            // check if we have a new label in here
            if (region->labels[region->label_index].address == pc) {
                CHECK(flow_control != TDN_IL_CF_META); // no label between prefix and opcode
                jit_label_t* current = &region->labels[region->label_index++];
                current->visited = true; // we are now visiting it

                // merge with the current eval stack
                if (current->snapshot.initialized) {
                    // only need to merge if we got here from fallthrough, because otherwise the stack will
                    // be implicitly empty
                    if (
                        flow_control == TDN_IL_CF_NEXT ||
                        flow_control == TDN_IL_CF_BREAK ||
                        flow_control == TDN_IL_CF_CALL ||
                        flow_control == TDN_IL_CF_FIRST
                    ) {
                        CHECK_AND_RETHROW(eval_stack_merge(builder, stack, current, true));
                    } else {
                        // need to restore the stack from the initialized stack point
                        CHECK_AND_RETHROW(eval_stack_snapshot_restore(stack, &current->snapshot));
                    }
                } else {
                    CHECK_AND_RETHROW(eval_stack_snapshot(builder, stack, current));
                }

                // if we have full-through then create a branch
                // NOTE: we don't check FIRST since from the first
                //       opcode we don't need this branch
                if (
                    flow_control == TDN_IL_CF_NEXT ||
                    flow_control == TDN_IL_CF_BREAK ||
                    flow_control == TDN_IL_CF_CALL
                ) {
                    jit_builder_build_branch(builder, current->block);
                }

                // set the current block
                region->has_block = true;
                jit_builder_set_block(builder, current->block);
                switched_block = true;

                // if this has phis then update the eval stack to have
                // the phi values instead of whatever it currently has
                if (current->needs_phi) {
                    CHECK_AND_RETHROW(eval_stack_update_phis(stack, current));
                }
            } else {
                // make sure the label is after this pc, to make sure we don't cross segments
                CHECK(region->labels[region->label_index].address > pc);
            }
        }

        if (!switched_block) {
            // no label? that means we must have normal non-block breaking
            // instruction before
            CHECK(
                flow_control != TDN_IL_CF_RETURN &&
                flow_control != TDN_IL_CF_BRANCH &&
                flow_control != TDN_IL_CF_THROW
            );
        }

        // convert to jit
        tdn_normalize_inst(&inst);
        CHECK(region->has_block);
        CHECK_AND_RETHROW(jit_instruction(&ctx, region, inst, pc));

        //--------------------------------------------------------------------------------------------------------------
        // last handling
        //--------------------------------------------------------------------------------------------------------------

        // prepare for next iteration
        flow_control = inst.control_flow;
        pc += inst.length;

        // for verification, these should have no stack after wards
        if (
            flow_control == TDN_IL_CF_THROW ||
            flow_control == TDN_IL_CF_BRANCH ||
            flow_control == TDN_IL_CF_RETURN
        ) {
            CHECK(arrlen(stack->stack) == 0);
        }

        // we might be reaching the end of multiple nested blocks, so until we find a region which
        // does not end we need to pop it
        while (arrlen(ctx.regions) != 0 && pc == region->pc_end) {
            // we got to the end of a region, make sure we can't fallthrough from here,
            // if its the root then allow for return as well
            CHECK(
                flow_control == TDN_IL_CF_BRANCH ||
                flow_control == TDN_IL_CF_THROW ||
                flow_control == TDN_IL_CF_RETURN
            );

            // pop the region and clear it
            arrpop(ctx.regions);

            // finally is duplicated multiple times, the first path is a fault path
            // and the others are success paths, handle them right now if there are
            // more to handle
            if (
                region->is_handler &&
                region->clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY
            ) {
                int finaly_path_index = ctx.handler_regions[region->clause_index].current_finally_path;
                if (finaly_path_index < hmlen(region->finally_handlers->finally_paths)) {
                    pc = region->pc_start;

                    // we don't want to go completely out just yet, we have some
                    // more paths to take
                    break;
                }
            }

            // get the new last region for the while condition to get
            // updated
            region = arrlast(ctx.regions);
        }

#ifdef JIT_IL_OUTPUT
        //--------------------------------------------------------------------------------------------------------------
        // debug prints
        //--------------------------------------------------------------------------------------------------------------

        for (int i = 0; clauses != NULL && i < clauses->Length; i++) {
            RuntimeExceptionHandlingClause c = clauses->Elements[i];
            if (c->TryOffset + c->TryLength == pc) {
                indent -= 4;
                tdn_host_printf("[*] \t\t\t%*s} // end .try - %04x\n", indent, "");
            } else if (c->HandlerOffset + c->HandlerLength == pc) {
                indent -= 4;
                tdn_host_printf("[*] \t\t\t%*s} // end handler\n", indent, "");
            }
        }
#endif
    }

    // make sure we have no more regions in our stack
    CHECK(arrlen(ctx.regions) == 0);

cleanup:
    arrfree(ctx.regions);

    for (int i = 0; i < arrlen(ctx.protected_regions); i++) {
        jit_region_free(&ctx.protected_regions[i]);
    }
    arrfree(ctx.protected_regions);

    for (int i = 0; i < arrlen(ctx.handler_regions); i++) {
        jit_region_free(&ctx.handler_regions[i]);
    }
    arrfree(ctx.handler_regions);

    jit_region_free(&root_region);

    arrfree(ctx.locals);
    arrfree(ctx.args);
    arrfree(ctx.labels);
    eval_stack_free(&ctx.stack);

    // make the error go out
    builder_ctx->err = err;
}

/**
 * Prepares a method for jitting, this essentially creates the jit function
 * so it will be ready for when we call it
 */
static tdn_err_t jit_prepare_method(RuntimeMethodBase method, jit_module_t handle) {
    tdn_err_t err = TDN_NO_ERROR;
    jit_value_type_t* params = NULL;
    string_builder_t builder = {};

    // check if already jitted
    if (method->JitPrepared) {
        goto cleanup;
    }
    method->JitPrepared = 1;

    // prepre the type
    CHECK_AND_RETHROW(jit_prepare_type(method->DeclaringType));

    // handle the return value, we have a special case of returning a struct, if it can't be returned
    // by value then it will be returned by passing an implicit pointer
    jit_value_type_t ret_type = get_jit_return_type(method->ReturnParameter->ParameterType);
    if (ret_type == JIT_TYPE_NONE && method->ReturnParameter->ParameterType != tVoid) {
        arrpush(params, JIT_TYPE_PTR);
    }

    // handle the `this` argument, its always a pointer (byref for struct types)
    if (!method->Attributes.Static) {
        arrpush(params, JIT_TYPE_PTR);
    }

    // handle the arguments
    for (size_t i = 0; i < method->Parameters->Length; i++) {
        arrpush(params, get_jit_argument_type(method->Parameters->Elements[i]->ParameterType));
    }

    // generate the name
    string_builder_push_method_signature(&builder, method, true);
    const char* name = string_builder_build(&builder);

    // create the function itself
    jit_function_t func;
    if (method->MethodBody != NULL) {
        func = jit_module_create_function(
            handle,
            name, ret_type, arrlen(params), params
        );
    } else {
        // make sure the assembly is allowed to have external exports
        CHECK(method->Module->Assembly->AllowExternalExports);
        func = jit_module_create_extern_function(
            handle,
            name, ret_type, arrlen(params), params
        );
    }
    method->JitMethodId = jit_get_function_id(func);

    // queue to methods to jit if we didn't start with this already
    if (!method->JitStarted && method->MethodBody != NULL) {
        arrpush(m_methods_to_jit, method);
    }

cleanup:
    string_builder_free(&builder);
    arrfree(params);

    return err;
}

/**
 * Actually jits a method, prepares it if needed
 */
static tdn_err_t jit_method(RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;

    // check if already jitted
    if (method->JitStarted) {
        goto cleanup;
    }
    method->JitStarted = 1;

    // make sure the method is already prepared at this point
    CHECK(method->JitPrepared);

    // now call the builder so we can actually build it
    jit_builder_ctx_t ctx = {
        .method = method,
        .err = TDN_NO_ERROR
    };
    jit_module_build_function(m_jit_module,
                                 jit_get_function_from_id(method->JitMethodId),
                                 jit_method_callback, &ctx);
    CHECK_AND_RETHROW(ctx.err);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// High-level apis
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static tdn_err_t tdn_jit_method_internal(RuntimeMethodBase methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    // prepare it, this should queue the method
    CHECK_AND_RETHROW(jit_prepare_method(methodInfo, m_jit_module));

    // and now dequeue all the methods we need to jit
    while (arrlen(m_methods_to_jit) != 0) {
        RuntimeMethodBase method = arrpop(m_methods_to_jit);
        CHECK_AND_RETHROW(jit_method(method));
    }

cleanup:
    return err;
}

tdn_err_t tdn_jit_method(RuntimeMethodBase methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(methodInfo != NULL);

    // create a new module that we are going to use
    m_jit_module = jit_module_create();

    // actually jit it
    CHECK_AND_RETHROW(tdn_jit_method_internal(methodInfo));

    // and link that module
    jit_link_module(m_jit_module);
    m_jit_module = NULL;

cleanup:
    return err;
}

void* tdn_jit_get_method_address(RuntimeMethodBase methodInfo) {
    jit_function_t function = jit_get_function_from_id(methodInfo->JitMethodId);
    if (IS_FUNCTION_NULL(function)) {
        return NULL;
    }
    return jit_get_function_addr(function);
}

tdn_err_t tdn_jit_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // create a new module that we are going to use
    m_jit_module = jit_module_create();

    // jit all the virtual methods, as those are the one that can be called
    // by other stuff unknowingly, the rest are going to be jitted lazyily
    for (int i = 0; i < type->DeclaredMethods->Length; i++) {
        RuntimeMethodBase method = (RuntimeMethodBase)type->DeclaredMethods->Elements[i];
        if (!method->Attributes.Virtual) continue;
        CHECK_AND_RETHROW(tdn_jit_method_internal(method));
    }

    // and link that module
    jit_link_module(m_jit_module);
    m_jit_module = NULL;

cleanup:
    return err;
}
