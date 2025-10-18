#include "instruction.h"

#include "accessibility.h"
#include "casting.h"
#include "control_flow.h"
#include "internal.h"
#include "dotnet/types.h"
#include "util/except.h"
#include "util/string.h"


#define STACK_PUSH() \
    ({ \
        CHECK_ERROR(arrlen(block->stack) < function->method->MethodBody->MaxStackSize, TDN_ERROR_VERIFIER_STACK_OVERFLOW); \
        stack_value_t* __value = arraddnptr(block->stack, 1); \
        memset(__value, 0, sizeof(*__value)); \
        __value; \
    })

#define CHECK_INIT_THIS(value) \
    do { \
        stack_value_t* __value = value; \
        CHECK_ERROR(!function->track_ctor_state || !__value->flags.this_ptr || block->this_initialized, \
            TDN_ERROR_VERIFIER_UNINIT_STACK); \
    } while (0)

static RuntimeTypeInfo verifier_get_reduced_type(RuntimeTypeInfo type) {
    if (type == NULL) {
        return NULL;
    }

    type = verifier_get_underlying_type(type);
    if (type == tByte) {
        return tSByte;
    } else if (type == tUInt16) {
        return tInt16;
    } else if (type == tUInt32) {
        return tInt32;
    } else if (type == tUInt64) {
        return tInt64;
    } else if (type == tUIntPtr) {
        return tIntPtr;
    } else {
        return type;
    }
}

static RuntimeTypeInfo verifier_get_verification_type(RuntimeTypeInfo type) {
    if (type == NULL) {
        return NULL;
    }

    if (type->IsByRef) {
        type = verifier_get_verification_type(type->ElementType);
        ASSERT(tdn_get_byref_type(type, &type));
        return type;
    } else {
        RuntimeTypeInfo reduced_type = verifier_get_reduced_type(type);
        if (reduced_type == tBoolean) {
            return tSByte;
        } else if (reduced_type == tChar) {
            return tInt16;
        } else {
            return reduced_type;
        }
    }
}

static bool verifier_is_unsafe_assignable(function_t* function, stack_value_t* a, stack_value_t* b) {
    if (!function->allow_unsafe) {
        return false;
    }

    // convert by-ref and native int as much as they want
    if (a->kind == KIND_BY_REF && b->kind == KIND_NATIVE_INT) {
        return true;
    } else if (a->kind == KIND_NATIVE_INT && b->kind == KIND_BY_REF) {
        return true;
    }

    return false;
}

static bool verifier_is_same_reduced_type(RuntimeTypeInfo src, RuntimeTypeInfo dst) {
    return verifier_get_reduced_type(src) == verifier_get_reduced_type(dst);
}

static bool verifier_is_assignable_type(RuntimeTypeInfo src, RuntimeTypeInfo dst, bool allow_size_equivalence) {
    if (src == dst) {
        return true;
    }

    if (tdn_type_is_valuetype(src) || tdn_type_is_valuetype(dst)) {
        if (allow_size_equivalence && verifier_is_same_reduced_type(src, dst)) {
            return true;
        }

        return false;
    }

    return verifier_can_cast_to(src, dst);
}

static const char* KIND_str(stack_value_kind_t kind) {
    switch (kind) {
        case KIND_INT32: return "int32";
        case KIND_INT64: return "int64";
        case KIND_NATIVE_INT: return "native int";
        case KIND_FLOAT: return "float";
        case KIND_BY_REF: return "by-ref";
        case KIND_OBJ_REF: return "obj-ref";
        case KIND_VALUE_TYPE: return "value";
        default: return "?";
    }
}

#define CHECK_IS_ASSIGNABLE(a, b) \
    do { \
        stack_value_t* __a = a; \
        stack_value_t* __b = b; \
        if (!verifier_is_unsafe_assignable(function, __a, __b)) { \
            CHECK_ERROR(verifier_is_assignable(__a, __b), \
                TDN_ERROR_VERIFIER_STACK_UNEXPECTED, \
                "%T [%s%s] is-assignable-to %T [%s%s]", \
                    __a->type, __a->flags.ref_read_only ? "readonly " : "", KIND_str(__a->kind), \
                    __b->type, __b->flags.ref_read_only ? "readonly " : "", KIND_str(__b->kind)); \
        } \
    } while (0)

#define CHECK_IS_ASSIGNABLE_TYPE(a, b) \
    do { \
        RuntimeTypeInfo __a = a; \
        RuntimeTypeInfo __b = b; \
        CHECK_ERROR(verifier_is_assignable_type(__a, __b, false), \
            TDN_ERROR_VERIFIER_STACK_UNEXPECTED, \
            "%T is-assignable-to %T", __a, __b); \
    } while (0)

#define CHECK_IS_ARRAY_ELEMENT_COMPATIBLE_WITH(a, b) \
    do { \
        RuntimeTypeInfo __a = a; \
        RuntimeTypeInfo __b = b; \
        CHECK_ERROR(verifier_is_assignable_type(__a, __b, true), \
            TDN_ERROR_VERIFIER_STACK_UNEXPECTED, \
            "%T is-assignable-to %T", __a, __b); \
    } while (0)

//----------------------------------------------------------------------------------------------------------------------
// Misc instructions
//----------------------------------------------------------------------------------------------------------------------

// Use as a template for adding new instructions
static tdn_err_t verify_nop(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

cleanup:
    return err;
}

static tdn_err_t verify_sizeof(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_init(STACK_PUSH(), tInt32);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Local access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_load_local(function_t* function, block_t* block, tdn_il_inst_t* inst, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    block_local_t* block_locals = is_arg ? block->args : block->locals;
    local_t* function_locals = is_arg ? function->args : function->locals;

    // get the local slot
    CHECK_ERROR(inst->operand.variable < arrlen(block_locals),
        is_arg ? TDN_ERROR_VERIFIER_UNRECOGNIZED_ARGUMENT_NUMBER : TDN_ERROR_VERIFIER_UNRECOGNIZED_LOCAL_NUMBER);
    block_local_t block_local = block_locals[inst->operand.variable];
    local_t* func_local = &function_locals[inst->operand.variable];

    if (!function->allow_unsafe) {
        // verified code can't use pointers
        CHECK_ERROR(!func_local->type->IsPointer,
            TDN_ERROR_VERIFIER_UNMANAGED_POINTER);
    }

    // push the new type to the stack, copy the flags, those handle the this-state on their own
    stack_value_t* value = stack_value_init(STACK_PUSH(), func_local->type);
    value_flags_from_block_local(&value->flags, block_local);

    // Set the this_ptr when loading it (only valid on methods that have not
    if (inst->operand.variable == 0 && is_arg && !function->method->Attributes.Static && !function->modifies_this_type) {
        value->flags.this_ptr = true;
    }

cleanup:
    return err;
}

static tdn_err_t verify_store_local(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    block_local_t* block_locals = is_arg ? block->args : block->locals;
    local_t* function_locals = is_arg ? function->args : function->locals;

    // get the local slot
    CHECK_ERROR(inst->operand.variable < arrlen(block_locals),
        is_arg ? TDN_ERROR_VERIFIER_UNRECOGNIZED_ARGUMENT_NUMBER : TDN_ERROR_VERIFIER_UNRECOGNIZED_LOCAL_NUMBER);
    block_local_t* block_local = &block_locals[inst->operand.variable];
    local_t* func_local = &function_locals[inst->operand.variable];

    // don't allow to store the this if we are not ready yet
    if (is_arg && function->track_ctor_state && !block->this_initialized) {
        CHECK_ERROR(inst->operand.variable != 0, TDN_ERROR_VERIFIER_THIS_UNINIT_STORE);
    }

    // init the type location, inherit the flags
    // from the incoming value
    stack_value_t local_value = stack_value_create(func_local->type);
    CHECK_IS_ASSIGNABLE(stack, &local_value);

    // remember the flags of the local
    block_local_from_value_flags(block_local, stack->flags);

cleanup:
    return err;
}

static tdn_err_t verify_load_local_address(function_t* function, block_t* block, tdn_il_inst_t* inst, bool is_arg) {
    tdn_err_t err = TDN_NO_ERROR;

    block_local_t* block_locals = is_arg ? block->args : block->locals;
    local_t* function_locals = is_arg ? function->args : function->locals;

    // get the local slot
    CHECK_ERROR(inst->operand.variable < arrlen(block_locals),
        is_arg ? TDN_ERROR_VERIFIER_UNRECOGNIZED_ARGUMENT_NUMBER : TDN_ERROR_VERIFIER_UNRECOGNIZED_LOCAL_NUMBER);
    block_local_t* block_local = &block_locals[inst->operand.variable];
    local_t* func_local = &function_locals[inst->operand.variable];

    // don't allow to have nested byrefs
    CHECK_ERROR(!func_local->type->IsByRef, TDN_ERROR_VERIFIER_BYYREF_OF_BYREF);

    // setup the new stack value
    stack_value_t* value = STACK_PUSH();
    value->kind = KIND_BY_REF;
    value->type = func_local->type;

    // don't allow to load the this if we are not ready yet
    if (inst->operand.variable == 0 && is_arg && !function->method->Attributes.Static) {
        value->flags.this_ptr = true;

        CHECK_ERROR(!function->track_ctor_state || block->this_initialized,
            TDN_ERROR_VERIFIER_THIS_UNINIT_STORE);
    }

    // properties that are kept
    value->flags.ref_struct_non_local = block_local->ref_struct_non_local;

cleanup:
    return err;
}

static tdn_err_t verify_ldarg(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    return verify_load_local(function, block, inst, true);
}

static tdn_err_t verify_starg(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    return verify_store_local(function, block, inst, stack, true);
}

static tdn_err_t verify_ldarga(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    return verify_load_local_address(function, block, inst, true);
}

static tdn_err_t verify_ldloc(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    return verify_load_local(function, block, inst, false);
}

static tdn_err_t verify_stloc(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    return verify_store_local(function, block, inst, stack, false);
}

static tdn_err_t verify_ldloca(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    return verify_load_local_address(function, block, inst, false);
}

//----------------------------------------------------------------------------------------------------------------------
// Misc instructions
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_ldfld(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeFieldInfo field = inst->operand.field;

    value_flags_t flags = {};
    RuntimeTypeInfo instance = NULL;
    if (inst->opcode == CEE_LDSFLD) {
        CHECK_ERROR(field->Attributes.Static, TDN_ERROR_VERIFIER_EXPECTED_STATIC_FIELD);
    } else {
        RuntimeTypeInfo owning_type = field->DeclaringType;

        // treat as a by-ref if its a value type
        if (stack->kind == KIND_VALUE_TYPE) {
            stack->kind = KIND_BY_REF;
        }

        // setup the load type, for value types its a readonly
        // reference (since we are not modifying it in the load)
        stack_value_t declared_this = {};
        if (tdn_type_is_valuetype(owning_type)) {
            declared_this.kind = KIND_BY_REF;
            declared_this.type = owning_type;
            declared_this.flags.ref_read_only = true;
        } else {
            declared_this.kind = KIND_OBJ_REF;
            declared_this.type = owning_type;
        }

        // ensure that the this can be loaded properly
        if (stack->kind == KIND_NATIVE_INT) {
            CHECK(function->method->Module->Assembly->AllowUnsafe);
        } else {
            CHECK_IS_ASSIGNABLE(stack, &declared_this);
        }

        // if we are loading a ref field, and the ref-struct only contains non-local
        // references, then we are going to mark the reference as non-local
        if (field->FieldType->IsByRef && stack->flags.ref_struct_non_local) {
            flags.ref_non_local = true;
        }

        // the instance we are loading from
        instance = stack->type;
    }

    // if we are loading a readonly reference field
    // then mark it as such
    if (field->FieldType->IsByRef && field->ReferenceIsReadOnly) {
        flags.ref_read_only = true;
    }

    // check that we can access it
    CHECK_ERROR(verifier_can_access_field(function->method->DeclaringType, field, instance),
        TDN_ERROR_VERIFIER_FIELD_ACCESS, "Accessing %T::%U from %T::%U", field->DeclaringType, field->Name, function->method->DeclaringType, function->method->Name);

    // push the value to the stack
    stack_value_init(STACK_PUSH(), field->FieldType)->flags = flags;

cleanup:
    return err;
}

static tdn_err_t verify_ldflda(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeFieldInfo field = inst->operand.field;

    bool is_cctor = tdn_compare_string_to_cstr(function->method->Name, ".cctor") &&
            field->DeclaringType == function->method->DeclaringType;

    bool is_ctor = tdn_compare_string_to_cstr(function->method->Name, ".ctor") &&
            field->DeclaringType == function->method->DeclaringType;

    // TODO: treat IsExternalInit like is_ctor

    RuntimeTypeInfo instance = NULL;
    value_flags_t flags = {};
    if (inst->opcode == CEE_LDSFLDA) {
        CHECK_ERROR(field->Attributes.Static, TDN_ERROR_VERIFIER_EXPECTED_STATIC_FIELD);
        flags.ref_non_local = true;

    } else {
        RuntimeTypeInfo owning_type = field->DeclaringType;

        // treat as a by-ref if its a value type
        if (stack->kind == KIND_VALUE_TYPE) {
            stack->kind = KIND_BY_REF;
        }

        // setup the load type, for value types its a readonly
        // reference (since we are not modifying it in the load)
        stack_value_t declared_this = {};
        if (tdn_type_is_valuetype(owning_type)) {
            declared_this.kind = KIND_BY_REF;
            declared_this.type = owning_type;
            declared_this.flags.ref_read_only = true;
        } else {
            declared_this.kind = KIND_OBJ_REF;
            declared_this.type = owning_type;
        }

        // ensure that the this can be loaded properly
        CHECK_IS_ASSIGNABLE(stack, &declared_this);

        // if the reference is non-local or its an object reference then the new
        // reference is also going to be
        if (stack->flags.ref_non_local || stack->kind == KIND_OBJ_REF) {
            flags.ref_non_local = true;
        }

        // the instance we are loading from
        instance = stack->type;
    }

    // init only fields are treated as readonly references
    // unless its in the cctor/ctor itself
    if (field->Attributes.InitOnly) {
        if (field->Attributes.Static) {
            if (!is_cctor) {
                flags.ref_read_only = true;
            }
        } else if (!is_ctor) {
            flags.ref_read_only = true;
        }
    }

    // check that we can access it
    CHECK_ERROR(verifier_can_access_field(function->method->DeclaringType, field, instance),
        TDN_ERROR_VERIFIER_FIELD_ACCESS);

    // push the value to the stack
    stack_value_t* value = STACK_PUSH();
    value->kind = KIND_BY_REF;
    value->type = field->FieldType;
    value->flags = flags;

cleanup:
    return err;
}

static tdn_err_t verify_stfld(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeFieldInfo field = inst->operand.field;

    bool is_cctor = function->method->Attributes.RTSpecialName &&
        tdn_compare_string_to_cstr(function->method->Name, ".cctor") &&
            field->DeclaringType == function->method->DeclaringType;

    bool is_ctor = function->method->Attributes.RTSpecialName &&
        tdn_compare_string_to_cstr(function->method->Name, ".ctor") &&
            field->DeclaringType == function->method->DeclaringType;

    // TODO: treat IsExternalInit like is_ctor

    RuntimeTypeInfo instance = NULL;
    stack_value_t* value = NULL;
    if (inst->opcode == CEE_STSFLD) {
        value = &stack[0];
        CHECK_INIT_THIS(value);

        CHECK_ERROR(field->Attributes.Static, TDN_ERROR_VERIFIER_EXPECTED_STATIC_FIELD);

    } else {
        value = &stack[1];
        stack_value_t* actual_this = &stack[0];
        CHECK_INIT_THIS(value);

        RuntimeTypeInfo owning_type = field->DeclaringType;

        // treat as a by-ref if its a value type
        if (actual_this->kind == KIND_VALUE_TYPE) {
            actual_this->kind = KIND_BY_REF;
        }

        // setup the load type
        stack_value_t declared_this = {};
        if (tdn_type_is_valuetype(owning_type)) {
            declared_this.kind = KIND_BY_REF;
            declared_this.type = owning_type;
        } else {
            declared_this.kind = KIND_OBJ_REF;
            declared_this.type = owning_type;
        }

        // ensure that the this can be loaded properly
        CHECK_IS_ASSIGNABLE(actual_this, &declared_this);

        // the instance we are loading from
        instance = actual_this->type;
    }

    // only the ctor/cctor can write to the init only fields
    if (field->Attributes.InitOnly) {
        if (field->Attributes.Static) {
            CHECK_ERROR(is_cctor, TDN_ERROR_VERIFIER_INIT_ONLY);
        } else {
            CHECK_ERROR(is_ctor, TDN_ERROR_VERIFIER_INIT_ONLY);
        }
    }

    // check that we can access it
    CHECK_ERROR(verifier_can_access_field(function->method->DeclaringType, field, instance),
        TDN_ERROR_VERIFIER_FIELD_ACCESS);

    // setup the value for storing
    // TODO: non-local reference in a non-local ref-struct
    stack_value_t field_value = {
        .flags = {
            .ref_read_only = field->ReferenceIsReadOnly
        }
    };
    stack_value_init(&field_value, field->FieldType);
    CHECK_IS_ASSIGNABLE(value, &field_value);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Reference access
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_initobj(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // must be a byref
    CHECK_ERROR(stack->kind == KIND_BY_REF,
        TDN_ERROR_VERIFIER_STACK_BY_REF);

    // and check it matches properly
    stack_value_t value = {};
    value.type = inst->operand.type;
    value.kind = KIND_BY_REF;
    CHECK_IS_ASSIGNABLE(stack, &value);

cleanup:
    return err;
}

static tdn_err_t verify_localloc(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // both int32 and native int are allowed, we are going to let this pass verification
    // because the pointer is not something we care too much about
    CHECK(
        stack->kind == KIND_INT32 ||
        stack->kind == KIND_NATIVE_INT
    );

    // if the assembly requires unsafe then only allow
    // a constant in this, this is only valid because our
    // iterator fixes it up
    if (!function->method->Module->Assembly->AllowUnsafe) {
        CHECK(inst->operand_type == TDN_IL_INT32);
    }

    RuntimeTypeInfo ptr = NULL;
    CHECK_AND_RETHROW(tdn_get_pointer_type(tVoid, &ptr));

    // its just a pointer
    stack_value_init(STACK_PUSH(), ptr);

cleanup:
    return err;
}

static tdn_err_t verify_ldind(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_INIT_THIS(stack);

    if (function->method->Module->Assembly->AllowUnsafe) {
        CHECK(stack->kind == KIND_BY_REF || stack->kind == KIND_NATIVE_INT);

        if (inst->operand.type == NULL) {
            CHECK(tdn_type_is_gc_pointer(stack->type));
            inst->operand.type = stack->type;
        }
    } else {
        CHECK(stack->kind == KIND_BY_REF);

        if (inst->operand.type == NULL) {
            CHECK(tdn_type_is_gc_pointer(stack->type));
            inst->operand.type = stack->type;
        } else {
            CHECK_IS_ASSIGNABLE_TYPE(
                verifier_get_verification_type(stack->type),
                verifier_get_verification_type(inst->operand.type));
        }
    }

    stack_value_init(STACK_PUSH(), inst->operand.type);

cleanup:
    return err;
}

static tdn_err_t verify_stind(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_t* value = &stack[1];
    stack_value_t* address = &stack[0];

    CHECK_INIT_THIS(value);

    // must be writable to store
    CHECK_ERROR(!address->flags.ref_read_only,
        TDN_ERROR_VERIFIER_READONLY_ILLEGAL_WRITE);

    // must be a byref
    if (function->method->Module->Assembly->AllowUnsafe) {
        CHECK(stack->kind == KIND_BY_REF || stack->kind == KIND_NATIVE_INT);
    } else {
        CHECK(stack->kind == KIND_BY_REF);
    }

    // resolve the type if stind.ref
    if (inst->operand.type == NULL) {
        inst->operand.type = address->type;
    }

    stack_value_t type_val = stack_value_create(inst->operand.type);
    stack_value_t address_val = stack_value_create(address->type);

    if (!function->allow_unsafe) {
        CHECK_IS_ASSIGNABLE(&type_val, &address_val);
        CHECK_IS_ASSIGNABLE(value, &type_val);
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Stack manipulation
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_ldnull(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_t* value = STACK_PUSH();
    value->kind = KIND_OBJ_REF;
    value->type = NULL;

cleanup:
    return err;
}

static tdn_err_t verify_ldstr(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_init(STACK_PUSH(), tString);

cleanup:
    return err;
}

static tdn_err_t verify_ldtoken(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    RuntimeTypeInfo type = NULL;
    switch (inst->operand_type) {
        case TDN_IL_TYPE: type = tRuntimeTypeHandle; break;
        case TDN_IL_FIELD: type = tRuntimeFieldHandle; break;
        case TDN_IL_METHOD: type = tRuntimeMethodHandle; break;
        default: CHECK_FAIL();
    }
    stack_value_init(STACK_PUSH(), type);

cleanup:
    return err;
}

static tdn_err_t verify_ldc_i4(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_init(STACK_PUSH(), tInt32);

cleanup:
    return err;
}

static tdn_err_t verify_ldc_i8(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_init(STACK_PUSH(), tInt64);

cleanup:
    return err;
}

static tdn_err_t verify_ldc_r4(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_init(STACK_PUSH(), tSingle);

cleanup:
    return err;
}

static tdn_err_t verify_ldc_r8(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_init(STACK_PUSH(), tDouble);

cleanup:
    return err;
}

static tdn_err_t verify_dup(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    *STACK_PUSH() = *stack;
    *STACK_PUSH() = *stack;

cleanup:
    return err;
}

static tdn_err_t verify_pop(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    // TODO: allow uninit this
    return TDN_NO_ERROR;
}

//----------------------------------------------------------------------------------------------------------------------
// Arith and compare operations
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_binary_op(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // check that we got the expected item
    switch (inst->opcode) {
        case CEE_ADD:
        case CEE_SUB:
        case CEE_MUL:
        case CEE_DIV:
        case CEE_REM: {
            CHECK_ERROR(KIND_INT32 <= stack[0].kind && stack[0].kind <= KIND_FLOAT, TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE);
            CHECK_ERROR(KIND_INT32 <= stack[1].kind && stack[1].kind <= KIND_FLOAT, TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE);
        } break;

        default: {
            CHECK_ERROR(KIND_INT32 <= stack[0].kind && stack[0].kind <= KIND_NATIVE_INT, TDN_ERROR_VERIFIER_EXPECTED_INTEGER_TYPE);
            CHECK_ERROR(KIND_INT32 <= stack[1].kind && stack[1].kind <= KIND_NATIVE_INT, TDN_ERROR_VERIFIER_EXPECTED_INTEGER_TYPE);
        } break;
    }

    // Stack value kind is ordered to make this work
    stack_value_t result = (stack[0].kind > stack[1].kind) ? stack[0] : stack[1];

    // ensure that whatever the native int was its degraded into
    // an integer and not stay as a pointer
    if (result.kind == KIND_NATIVE_INT) {
        result.type = tIntPtr;
    }

    CHECK_ERROR((stack[0].kind == stack[1].kind) || (result.kind == KIND_NATIVE_INT),
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    *STACK_PUSH() = result;

cleanup:
    return err;
}

static tdn_err_t verify_unary_op(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure the input is correct
    if (inst->opcode == CEE_NEG) {
        CHECK_ERROR(KIND_INT32 <= stack->kind && stack->kind <= KIND_FLOAT,
            TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE);

    } else if (inst->opcode == CEE_NOT) {
        CHECK_ERROR(KIND_INT32 <= stack->kind && stack->kind <= KIND_NATIVE_INT,
            TDN_ERROR_VERIFIER_EXPECTED_INTEGER_TYPE);

    } else {
        CHECK_FAIL();
    }

    // same as input
    *STACK_PUSH() = *stack;

cleanup:
    return err;
}

static tdn_err_t verify_shift(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // the shift by should be int32 or native int
    CHECK_ERROR(stack[1].kind == KIND_INT32 || stack[1].kind == KIND_NATIVE_INT,
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    // to be shifted should be an integer
    CHECK_ERROR(KIND_INT32 <= stack[0].kind && stack[0].kind <= KIND_NATIVE_INT,
        TDN_ERROR_VERIFIER_EXPECTED_INTEGER_TYPE);

    // same as the to be shifted value
    *STACK_PUSH() = stack[0];

cleanup:
    return err;
}

static tdn_err_t verify_conv(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // validate that both are good, specifically for unsafe code allow to perform
    // the operation also on by-refs
    if (function->method->Module->Assembly->AllowUnsafe) {
        CHECK_ERROR(KIND_INT32 <= stack->kind && stack->kind <= KIND_OBJ_REF,
            TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE);
    } else {
        CHECK_ERROR(KIND_INT32 <= stack->kind && stack->kind <= KIND_FLOAT,
            TDN_ERROR_VERIFIER_EXPECTED_NUMERIC_TYPE);
    }

    // always pushes as an int32
    RuntimeTypeInfo type = NULL;
    switch (inst->opcode) {
        case CEE_CONV_OVF_I1:
        case CEE_CONV_OVF_I2:
        case CEE_CONV_OVF_I4:
        case CEE_CONV_OVF_U1:
        case CEE_CONV_OVF_U2:
        case CEE_CONV_OVF_U4:
        case CEE_CONV_OVF_I1_UN:
        case CEE_CONV_OVF_I2_UN:
        case CEE_CONV_OVF_I4_UN:
        case CEE_CONV_OVF_U1_UN:
        case CEE_CONV_OVF_U2_UN:
        case CEE_CONV_OVF_U4_UN:
        case CEE_CONV_I1:
        case CEE_CONV_I2:
        case CEE_CONV_I4:
        case CEE_CONV_U1:
        case CEE_CONV_U2:
        case CEE_CONV_U4: type = tInt32; break;
        case CEE_CONV_OVF_I8:
        case CEE_CONV_OVF_U8:
        case CEE_CONV_OVF_I8_UN:
        case CEE_CONV_OVF_U8_UN:
        case CEE_CONV_I8:
        case CEE_CONV_U8: type = tInt64; break;
        case CEE_CONV_OVF_I:
        case CEE_CONV_OVF_U:
        case CEE_CONV_OVF_I_UN:
        case CEE_CONV_OVF_U_UN:
        case CEE_CONV_I:
        case CEE_CONV_U: type = tIntPtr; break;
        case CEE_CONV_R4: type = tSingle; break;
        case CEE_CONV_R_UN:
        case CEE_CONV_R8: type = tDouble; break;
        default: CHECK_FAIL();
    }

    stack_value_init(STACK_PUSH(), type);

cleanup:
    return err;
}

static bool verifier_is_binary_comparable(stack_value_t* a, stack_value_t* b, tdn_il_opcode_t opcode) {
    if (a->kind == b->kind && a->type == b->type) {
        return true;
    }

    switch (a->kind) {
        case KIND_OBJ_REF: {
            if (b->kind != KIND_OBJ_REF) {
                return false;
            }

            // ECMA-335 III.1.5 Operand type table, P. 303:
            // __cgt.un__ is allowed and verifiable on ObjectRefs (O). This is commonly used when
            // comparing an ObjectRef with null(there is no "compare - not - equal" instruction, which
            // would otherwise be a more obvious solution)
            return opcode == CEE_BEQ || opcode == CEE_BNE_UN || opcode == CEE_CEQ || opcode == CEE_CGT_UN;
        } break;

        case KIND_VALUE_TYPE:
            return false;

        case KIND_BY_REF: {
            if (b->kind == KIND_BY_REF) {
                return true;
            }

            if (b->kind == KIND_NATIVE_INT) {
                return opcode == CEE_BEQ || opcode == CEE_BNE_UN || opcode == CEE_CEQ;
            }

            return false;
        } break;

        case KIND_INT32:
            return b->kind == KIND_INT64 || b->kind == KIND_NATIVE_INT;

        case KIND_INT64:
            return b->kind == KIND_INT64;

        case KIND_NATIVE_INT: {
            if (b->kind == KIND_INT32 || b->kind == KIND_NATIVE_INT) {
                return true;
            }

            if (b->kind == KIND_BY_REF) {
                return opcode == CEE_BEQ || opcode == CEE_BNE_UN || opcode == CEE_CEQ;
            }

            return false;
        } break;

        case KIND_FLOAT:
            return b->kind == KIND_FLOAT;

        default:
            ASSERT(!"Invalid kind");
    }
}

static tdn_err_t verify_compare(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_ERROR(verifier_is_binary_comparable(&stack[0], &stack[1], inst->opcode),
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    // always pushed as int32
    stack_value_init(STACK_PUSH(), tInt32);

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Array related
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_newarr(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_t* length = &stack[0];
    CHECK_ERROR(
        length->kind == KIND_INT32 || length->kind == KIND_NATIVE_INT,
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    // can't create an array of by-refs
    CHECK_ERROR(!inst->operand.type->IsByRef,
        TDN_ERROR_VERIFIER_ARRAY_BY_REF);

    RuntimeTypeInfo array_type;
    CHECK_AND_RETHROW(tdn_get_array_type(inst->operand.type, &array_type));

    stack_value_init(STACK_PUSH(), array_type);

cleanup:
    return err;
}

static tdn_err_t verify_ldlen(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK(stack->type->IsArray);

    // return value is an intptr
    stack_value_init(STACK_PUSH(), tIntPtr);

cleanup:
    return err;
}

static tdn_err_t verify_ldelem(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_t* index = &stack[1];
    stack_value_t* array = &stack[0];

    CHECK_ERROR(index->kind == KIND_INT32 || index->kind == KIND_NATIVE_INT,
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    CHECK_ERROR(array->type->IsArray,
        TDN_ERROR_VERIFIER_EXPECTED_ARRAY);

    RuntimeTypeInfo element_type = inst->operand.type;
    RuntimeTypeInfo actual_element_type = array->type->ElementType;
    if (element_type != NULL) {
        CHECK_IS_ARRAY_ELEMENT_COMPATIBLE_WITH(
            verifier_get_verification_type(actual_element_type),
            element_type
        );
    } else {
        element_type = actual_element_type;
        CHECK(tdn_type_is_gc_pointer(element_type));
    }

    stack_value_init(STACK_PUSH(), element_type);

cleanup:
    return err;
}

static tdn_err_t verify_stelem(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_t* value = &stack[2];
    stack_value_t* index = &stack[1];
    stack_value_t* array = &stack[0];

    CHECK_INIT_THIS(value);

    CHECK_ERROR(index->kind == KIND_INT32 || index->kind == KIND_NATIVE_INT,
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    CHECK_ERROR(array->type->IsArray,
        TDN_ERROR_VERIFIER_EXPECTED_ARRAY);

    RuntimeTypeInfo element_type = inst->operand.type;
    RuntimeTypeInfo actual_element_type = array->type->ElementType;
    if (element_type != NULL) {
        CHECK_IS_ARRAY_ELEMENT_COMPATIBLE_WITH(
            verifier_get_verification_type(actual_element_type),
            element_type
        );
    } else {
        element_type = actual_element_type;
        CHECK_ERROR(tdn_type_is_gc_pointer(element_type), TDN_ERROR_VERIFIER_STACK_OBJ_REF);
    }

    // ensure the value matches
    stack_value_t element_val = stack_value_create(element_type);
    CHECK_IS_ASSIGNABLE(value, &element_val);

cleanup:
    return err;
}

static tdn_err_t verify_ldelema(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_t* index = &stack[1];
    stack_value_t* array = &stack[0];

    CHECK_ERROR(index->kind == KIND_INT32 || index->kind == KIND_NATIVE_INT,
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    CHECK_ERROR(array->type->IsArray,
        TDN_ERROR_VERIFIER_EXPECTED_ARRAY);

    RuntimeTypeInfo element_type = inst->operand.type;
    RuntimeTypeInfo actual_element_type = array->type->ElementType;
    CHECK_ERROR(
        actual_element_type == element_type ||
        verifier_is_same_reduced_type(actual_element_type, element_type),
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED_ARRAY_TYPE
    );

    stack_value_t* push = STACK_PUSH();
    push->flags.ref_non_local = true;
    push->kind = KIND_BY_REF;
    push->type = element_type;
    if (inst->prefixes & TDN_IL_PREFIX_READONLY) {
        push->flags.ref_read_only = true;
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Method related
//----------------------------------------------------------------------------------------------------------------------


static tdn_err_t verify_ldftn(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = inst->operand.method;


    // must not be a constructor
    CHECK_ERROR(!tdn_compare_string_to_cstr(inst->operand.method->Name, ".ctor"),
        TDN_ERROR_VERIFIER_LDFTN_CTOR);

    RuntimeTypeInfo instance = NULL;
    if (inst->opcode == CEE_LDVIRTFTN) {
        // TODO: devirt at this point already?
        CHECK(!method->Attributes.Static);

        // We want the boxed value for the comparison
        stack_value_t declared_type = {
            .kind = KIND_OBJ_REF,
            .type = method->DeclaringType
        };

        stack_value_t* this_ptr = &stack[0];
        CHECK_INIT_THIS(this_ptr);

        CHECK(this_ptr->kind == KIND_OBJ_REF);
        CHECK_IS_ASSIGNABLE(this_ptr, &declared_type);
    }

    // ensure we can access this method
    CHECK_ERROR(verifier_can_access_method(function->method->DeclaringType, method, instance),
        TDN_ERROR_VERIFIER_METHOD_ACCESS);

    // special case, since its only used to pass it to the newobj, we verify
    // externally that the opcodes are valid
    stack_value_t* value = STACK_PUSH();
    value->kind = KIND_NATIVE_INT;
    value->method = method;
    value->flags.ldftn = inst->opcode == CEE_LDFTN;

cleanup:
    return err;
}

static tdn_err_t verify_call(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // TODO: tail call support

    RuntimeMethodBase method = inst->operand.method;
    RuntimeTypeInfo method_type = method->Attributes.Static ? NULL : method->DeclaringType;

    // check for methods that are considered unsafe to call unless the assembly
    // is allowed unsafe
    if (
        method->DeclaringType == tUnsafe
    ) {
        CHECK(function->method->Module->Assembly->AllowUnsafe,
            "Assembly is not allowed to call %T::%U", method->DeclaringType, method->Name);
    }

    if (inst->opcode == CEE_CALLVIRT) {
        CHECK_ERROR(method_type != NULL, TDN_ERROR_VERIFIER_CALLVIRT_ON_STATIC);
        CHECK_ERROR(!tdn_type_is_valuetype(method_type), TDN_ERROR_VERIFIER_CALLVIRT_ON_VALUE_TYPE);

    } else if (inst->opcode == CEE_CALL) {
        // this is most likely a static interface function, resolve it
        if (inst->constrained != NULL) {
            CHECK(method->Attributes.Static);
        } else {
            CHECK_ERROR(!method->Attributes.Abstract, TDN_ERROR_VERIFIER_CALL_ABSTRACT);
        }
    }

    bool might_leak_local_ref = false;

    if (inst->opcode == CEE_NEWOBJ && tdn_is_delegate(method_type)) {
        // creating a delegate, ensure we call the ctor properly
        CHECK_ERROR(method->Parameters->Length == 2, TDN_ERROR_VERIFIER_DELEGATE_CTOR);
        stack_value_t declared_obj = stack_value_create(method->Parameters->Elements[0]->ParameterType);
        stack_value_t declared_ftn = stack_value_create(method->Parameters->Elements[1]->ParameterType);

        CHECK_ERROR(declared_ftn.kind == KIND_NATIVE_INT,
            TDN_ERROR_VERIFIER_DELEGATE_CTOR_SIG_I);

        stack_value_t* actual_ftn = &stack[1];
        stack_value_t* actual_obj = &stack[0];
        CHECK_INIT_THIS(actual_obj);

        // ensure we have a method in here, and not a normal native int
        CHECK_ERROR(actual_ftn->kind == KIND_NATIVE_INT && actual_ftn->method != NULL,
            TDN_ERROR_VERIFIER_STACK_METHOD);

        CHECK_IS_ASSIGNABLE(actual_obj, &declared_obj);
        CHECK_ERROR(actual_obj->kind == KIND_OBJ_REF,
            TDN_ERROR_VERIFIER_DELEGATE_CTOR_SIG_O);

        // we can only load non-final virtual functions that are from the base and from
        // the this pointer from ldftn, any other virtual function is not allowed
        if (actual_ftn->flags.ldftn) {
            if (actual_ftn->method->Attributes.Virtual && !actual_ftn->method->Attributes.Final && !verifier_is_boxed_value_type(actual_obj)) {
                if (!actual_ftn->method->DeclaringType->Attributes.Sealed) {
                    CHECK_ERROR(actual_obj->flags.this_ptr,
                        TDN_ERROR_VERIFIER_LDFTN_NON_FINAL_VIRTUAL);
                }
            }
        }

        // TODO: check delegate assignable
    } else {
        // check that the args match, ignore the instance for now
        int off = (method_type != NULL && inst->opcode != CEE_NEWOBJ) ? 1 : 0;
        CHECK(arrlen(stack) == (off + method->Parameters->Length));
        for (int i = 0; i < method->Parameters->Length; i++) {
            ParameterInfo param = method->Parameters->Elements[i];
            stack_value_t* actual = &stack[off + i];
            stack_value_t declared = stack_value_create(param->ParameterType);
            declared.flags.ref_read_only = param->Attributes.In;
            CHECK_IS_ASSIGNABLE(actual, &declared);

            // if this is a ref-struct, and its has local refs, then it might
            // leak a local ref from that
            if (actual->type != NULL && actual->type->IsByRefStruct) {
                might_leak_local_ref = !actual->flags.ref_struct_non_local;
            }

            // TODO: unscoped this support

            // if this is a ref-struct, and it contains local fields, ensure
            // we don't leak don't leak them accidently
            if (actual->type != NULL && actual->type->IsByRefStruct && !actual->flags.ref_struct_non_local) {
                might_leak_local_ref = true;
            }
        }
    }

    RuntimeTypeInfo instance = NULL;
    if (inst->opcode == CEE_NEWOBJ) {
        // ensure that this is a valid ctor
        CHECK_ERROR(tdn_compare_string_to_cstr(method->Name, ".ctor"), TDN_ERROR_VERIFIER_CTOR_EXPECTED);
        CHECK_ERROR(!method->Attributes.Static, TDN_ERROR_VERIFIER_CTOR_SIG);
        CHECK_ERROR(method_type != NULL, TDN_ERROR_VERIFIER_CTOR_SIG);
        CHECK_ERROR(!method->Attributes.Abstract, TDN_ERROR_VERIFIER_CTOR_SIG);

        // TODO: array ctor???

        // must not be an abstract class
        CHECK_ERROR(!method_type->Attributes.Abstract, TDN_ERROR_VERIFIER_NEWOBJ_ABSTRACT_CLASS);

    } else if (method_type != NULL) {
        stack_value_t actual_this = stack[0];
        instance = actual_this.type;
        stack_value_t declared_this = {
            .kind = tdn_type_is_valuetype(method_type) ? KIND_BY_REF : KIND_OBJ_REF,
            .type = method_type
        };

        // direct calls to ctor are mostly not allowed
        if (tdn_compare_string_to_cstr(method->Name, ".ctor")) {
            if (function->track_ctor_state && actual_this.flags.this_ptr && (method_type == method->DeclaringType || method_type == method->DeclaringType->BaseType)) {
                // we have called the base ctor, so its not initialized properly in this block
                block->this_initialized = true;
            } else {
                // allow direct calls to valuetype ctors
                CHECK_ERROR(actual_this.kind == KIND_BY_REF && tdn_type_is_valuetype(actual_this.type),
                    TDN_ERROR_VERIFIER_CALL_CTOR);
            }
        }

        if (inst->constrained != NULL) {
            // must be a by ref
            CHECK_ERROR(actual_this.kind == KIND_BY_REF,
                TDN_ERROR_VERIFIER_CONSTRAINED_CALL_WITH_NON_BYREF_THIS);

            // ensure the constrained matches the actual this type
            CHECK_ERROR(actual_this.type == inst->constrained,
                TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

            // turn into am objref
            actual_this.kind = KIND_OBJ_REF;
        }

        // if the method or type is marked as readonly then the
        // reference is also considered readonly
        if ((method_type->IsReadOnly || method->IsReadOnly) && declared_this.kind == KIND_BY_REF) {
            declared_this.flags.ref_read_only = true;
        }

        CHECK_IS_ASSIGNABLE(&actual_this, &declared_this);

        // TODO: unscoped reference

        if (inst->opcode == CEE_CALL) {
            // just like the rules for creating delegates with ldftn and virtual functions, but now for
            // normal calls
            if (method->Attributes.Virtual && !method->Attributes.Final && !verifier_is_boxed_value_type(&actual_this)) {
                if (!method_type->Attributes.Sealed) {
                    CHECK_ERROR(actual_this.flags.this_ptr, TDN_ERROR_VERIFIER_THIS_MISMATCH);
                }
            }
        }
    }

    // Check we can access the method
    CHECK_ERROR(verifier_can_access_method(function->method->DeclaringType, method, instance),
        TDN_ERROR_VERIFIER_METHOD_ACCESS);

    if (inst->opcode == CEE_NEWOBJ) {
        stack_value_t* value = STACK_PUSH();

        // push the type
        stack_value_init(value, method_type);

        // if we constructed the ref-struct only from non-local
        // references, then we can mark the struct itself also
        // as containing non-local references
        if (method_type->IsByRefStruct) {
            value->flags.ref_struct_non_local = !might_leak_local_ref;
        }

    } else if (method->ReturnParameter->ParameterType != tVoid) {
        stack_value_t* return_value = STACK_PUSH();
        stack_value_init(return_value, method->ReturnParameter->ParameterType);

        if (return_value->kind == KIND_BY_REF) {
            // if we won't leak a local reference, this result is a non-local reference
            return_value->flags.ref_non_local = !might_leak_local_ref;

            if (method->ReturnParameter->ReturnRefIsReadonly) {
                // the reference is readonly
                return_value->flags.ref_read_only = true;
            }

        }

        if (return_value->type->IsByRefStruct) {
            // returned a ref-struct, can't contain local references as long as the input
            // can't contain them
            return_value->flags.ref_struct_non_local = !might_leak_local_ref;
        }
    }

cleanup:
    return err;
}

static tdn_err_t verify_ret(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure that we don't return before the base ctor was called
    if (function->track_ctor_state) {
        CHECK_ERROR(block->this_initialized || function->method->DeclaringType == tObject,
            TDN_ERROR_VERIFIER_THIS_UNINIT_RETURN);
    }

    // Check we don't return from a protected block
    CHECK_ERROR(block->block.filter_clause == NULL, TDN_ERROR_VERIFIER_RETURN_FROM_FILTER);
    CHECK_ERROR(block->block.try_clause == NULL, TDN_ERROR_VERIFIER_RETURN_FROM_TRY);
    CHECK_ERROR(block->block.handler_clause == NULL, TDN_ERROR_VERIFIER_RETURN_FROM_HANDLER);

    RuntimeTypeInfo declared_return_type = function->method->ReturnParameter->ParameterType;
    if (declared_return_type == tVoid) {
        CHECK_ERROR(arrlen(block->stack) == 0, TDN_ERROR_VERIFIER_RETURN_VOID);
    } else {
        CHECK_ERROR(arrlen(block->stack) == 0, TDN_ERROR_VERIFIER_RETURN_EMPTY);
        CHECK_INIT_THIS(stack);

        stack_value_t ret_val = stack_value_create(declared_return_type);
        CHECK_IS_ASSIGNABLE(stack, &ret_val);

        if (stack->kind == KIND_BY_REF) {
            // ensure we don't return a pointer to the stack
            CHECK_ERROR(stack->flags.ref_non_local,
                TDN_ERROR_VERIFIER_RETURN_PTR_TO_STACK);

            // ensure we don't return a readonly ref
            // when the function return is non-readonly
            if (stack->flags.ref_read_only) {
                CHECK(function->method->ReturnParameter->ReturnRefIsReadonly);
            }
        }

        // if we are returning a ref-struct, then it must
        // have non-local references, otherwise we might
        // leak it
        if (stack->type != NULL && stack->type->IsByRefStruct) {
            CHECK(stack->flags.ref_struct_non_local);
        }
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// type checking
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_castclass(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    stack_value_t* value = &stack[0];
    CHECK_INIT_THIS(value);

    CHECK_ERROR(value->kind == KIND_OBJ_REF, TDN_ERROR_VERIFIER_STACK_OBJ_REF);

    // ensure we can access that type
    CHECK_ERROR(verifier_can_access_type(function->method->DeclaringType, inst->operand.type),
        TDN_ERROR_VERIFIER_TYPE_ACCESS);

    // and push it
    stack_value_t* pushed = STACK_PUSH();
    pushed->kind = KIND_OBJ_REF;
    pushed->type = inst->operand.type;

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Boxing
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_box(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeTypeInfo type = inst->operand.type;

    stack_value_t* value = &stack[0];
    CHECK_INIT_THIS(value);

    stack_value_t target_type = stack_value_create(type);

    // can't box a byref
    CHECK_ERROR(target_type.kind != KIND_BY_REF && !target_type.type->IsByRefStruct,
        TDN_ERROR_VERIFIER_BOX_BYREF);

    CHECK_ERROR(
        tdn_is_primitive(type) ||
        target_type.kind == KIND_OBJ_REF ||
        tdn_is_generic_parameter(type) ||
        tdn_type_is_valuetype(type),
        TDN_ERROR_VERIFIER_EXPECTED_VAL_CLASS_OBJ_REF_VARIABLE
    );

    // TODO: check constraints? what does that do?

    // ensure we can access the boxed type
    CHECK_ERROR(verifier_can_access_type(function->method->DeclaringType, type),
        TDN_ERROR_VERIFIER_TYPE_ACCESS);

    CHECK_IS_ASSIGNABLE(value, &target_type);

    // For Nullable<T> we push T
    stack_value_t* pushed = STACK_PUSH();
    pushed->kind = KIND_OBJ_REF;
    pushed->type = tdn_type_is_nullable(type) ? type->GenericArguments->Elements[0] : type;

cleanup:
    return err;
}

static tdn_err_t verify_unbox(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_INIT_THIS(stack);

    if (inst->opcode == CEE_UNBOX_ANY) {
        stack_value_init(STACK_PUSH(), inst->operand.type);
    } else {
        CHECK_ERROR(tdn_type_is_valuetype(inst->operand.type),
            TDN_ERROR_VERIFIER_VALUE_TYPE_EXPEXCTED);

        stack_value_t* value = STACK_PUSH();
        value->flags.ref_read_only = true;
        value->flags.ref_non_local = true;
        value->kind = KIND_BY_REF;
        value->type = inst->operand.type;
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Branching
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_br(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // get the target block
    block_t* target = verifier_get_block(function, inst->operand.branch_target);
    CHECK(target != NULL);

    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, target, false));

cleanup:
    return err;
}

static tdn_err_t verify_br_unary_cond(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_INIT_THIS(stack);
    CHECK_ERROR(
        stack->kind >= KIND_INT32 && stack->kind <= KIND_NATIVE_INT ||
        stack->kind == KIND_OBJ_REF ||
        stack->kind == KIND_BY_REF,
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED
    );

    // get the target block
    block_t* target = verifier_get_block(function, inst->operand.branch_target);
    CHECK(target != NULL);
    block_t* next = verifier_get_block(function, block->block.end);
    CHECK(next != NULL);

    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, target, false));
    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, next, true));

cleanup:
    return err;
}

static tdn_err_t verify_br_binary_cond(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_INIT_THIS(&stack[0]);
    CHECK_INIT_THIS(&stack[1]);

    CHECK_ERROR(verifier_is_binary_comparable(&stack[0], &stack[1], inst->opcode),
        TDN_ERROR_VERIFIER_STACK_UNEXPECTED);

    // get the target block
    block_t* target = verifier_get_block(function, inst->operand.branch_target);
    CHECK(target != NULL);
    block_t* next = verifier_get_block(function, block->block.end);
    CHECK(next != NULL);

    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, target, false));
    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, next, true));

cleanup:
    return err;
}

static tdn_err_t verify_switch(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure we have an int32 on the stack
    // TODO: can we have anything else?
    CHECK(stack->kind == KIND_INT32);

    // verify the default case
    block_t* next = verifier_get_block(function, block->block.end);
    CHECK(next != NULL);
    CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, next, true));

    // verify the rest of the blocks
    for (int i = 0; i < arrlen(inst->operand.switch_targets); i++) {
        block_t* target = verifier_get_block(function, inst->operand.switch_targets[i]);
        CHECK(target != NULL);
        CHECK_AND_RETHROW(verifier_propagate_control_flow(function, block, target, false));
    }

cleanup:
    return err;
}

//----------------------------------------------------------------------------------------------------------------------
// Exceptions
//----------------------------------------------------------------------------------------------------------------------

static tdn_err_t verify_leave(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // empty the stack
    arrsetlen(block->stack, 0);

    block_t* target = verifier_get_block(function, inst->operand.branch_target);
    CHECK(target != NULL);

    // propagate everything that is not the stack
    verifier_propagate_state(block, target);
    verifier_mark_block(function, target);

    // ensure this is a proper leave target
    CHECK_AND_RETHROW(verifier_is_valid_leave_target(function, block, target));

cleanup:
    return err;
}

static tdn_err_t verify_endfinally(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    // ensure we have a handler clause for the block
    CHECK_ERROR(block->block.handler_clause != NULL,
        TDN_ERROR_VERIFIER_ENDFINALLY);

    // ensure the handler is a finally/fault region
    CHECK_ERROR(
        block->block.handler_clause->Flags == COR_ILEXCEPTION_CLAUSE_FINALLY ||
        block->block.handler_clause->Flags == COR_ILEXCEPTION_CLAUSE_FAULT,
        TDN_ERROR_VERIFIER_ENDFINALLY);

    // empty the stack
    arrsetlen(block->stack, 0);

cleanup:
    return err;
}

static tdn_err_t verify_throw(function_t* function, block_t* block, tdn_il_inst_t* inst, stack_value_t* stack) {
    tdn_err_t err = TDN_NO_ERROR;

    CHECK_INIT_THIS(stack);
    CHECK(stack[0].kind == KIND_OBJ_REF);

    // Ensure that this is an exception type, with the exception that we allow to throw
    // an object type (?)
    if (stack[0].type != tObject) {
        stack_value_t temp = stack_value_create(tException);
        CHECK_ERROR(verifier_is_assignable(&stack[0], &temp),
            TDN_ERROR_VERIFIER_THROW_OR_CATCH_ONLY_EXCEPTION_TYPE,
            "%T [%s] is-assignable-to %T [%s]", stack->type, KIND_str(stack->kind), temp.type, KIND_str(temp.kind));
    }

    // empty the stack
    arrsetlen(block->stack, 0);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Dispatch tables
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

verify_instruction_t g_verify_dispatch_table[] = {
    [CEE_NOP] = verify_nop,

    [CEE_LDARG] = verify_ldarg,
    [CEE_STARG] = verify_starg,
    [CEE_LDARGA] = verify_ldarga,

    [CEE_LDLOC] = verify_ldloc,
    [CEE_STLOC] = verify_stloc,
    [CEE_LDLOCA] = verify_ldloca,

    [CEE_LDFLDA] = verify_ldflda,
    [CEE_LDFLD] = verify_ldfld,
    [CEE_STFLD] = verify_stfld,
    [CEE_LDSFLDA] = verify_ldflda,
    [CEE_LDSFLD] = verify_ldfld,
    [CEE_STSFLD] = verify_stfld,

    [CEE_INITOBJ] = verify_initobj,
    [CEE_LOCALLOC] = verify_localloc,

    [CEE_LDIND_I1] = verify_ldind,
    [CEE_LDIND_U1] = verify_ldind,
    [CEE_LDIND_I2] = verify_ldind,
    [CEE_LDIND_U2] = verify_ldind,
    [CEE_LDIND_I4] = verify_ldind,
    [CEE_LDIND_U4] = verify_ldind,
    [CEE_LDIND_I8] = verify_ldind,
    [CEE_LDIND_I] = verify_ldind,
    [CEE_LDIND_REF] = verify_ldind,
    [CEE_LDIND_R8] = verify_ldind,
    [CEE_LDOBJ] = verify_ldind,

    [CEE_STIND_I] = verify_stind,
    [CEE_STIND_I1] = verify_stind,
    [CEE_STIND_I2] = verify_stind,
    [CEE_STIND_I4] = verify_stind,
    [CEE_STIND_I8] = verify_stind,
    [CEE_STIND_REF] = verify_stind,
    [CEE_STIND_R8] = verify_stind,
    [CEE_STOBJ] = verify_stind,

    [CEE_LDNULL] = verify_ldnull,
    [CEE_LDSTR] = verify_ldstr,
    [CEE_LDTOKEN] = verify_ldtoken,
    [CEE_LDC_I4] = verify_ldc_i4,
    [CEE_LDC_I8] = verify_ldc_i8,
    [CEE_LDC_R4] = verify_ldc_r4,
    [CEE_LDC_R8] = verify_ldc_r8,
    [CEE_DUP] = verify_dup,
    [CEE_POP] = verify_pop,

    [CEE_ADD] = verify_binary_op,
    [CEE_SUB] = verify_binary_op,
    [CEE_MUL] = verify_binary_op,
    [CEE_DIV] = verify_binary_op,
    [CEE_DIV_UN] = verify_binary_op,
    [CEE_REM] = verify_binary_op,
    [CEE_REM_UN] = verify_binary_op,
    [CEE_AND] = verify_binary_op,
    [CEE_OR] = verify_binary_op,
    [CEE_XOR] = verify_binary_op,
    [CEE_ADD_OVF] = verify_binary_op,
    [CEE_ADD_OVF_UN] = verify_binary_op,
    [CEE_SUB_OVF] = verify_binary_op,
    [CEE_SUB_OVF_UN] = verify_binary_op,
    [CEE_MUL_OVF] = verify_binary_op,
    [CEE_MUL_OVF_UN] = verify_binary_op,

    [CEE_NEG] = verify_unary_op,
    [CEE_NOT] = verify_unary_op,

    [CEE_SHR] = verify_shift,
    [CEE_SHL] = verify_shift,
    [CEE_SHR_UN] = verify_shift,

    [CEE_CONV_OVF_I1] = verify_conv,
    [CEE_CONV_OVF_I2] = verify_conv,
    [CEE_CONV_OVF_I4] = verify_conv,
    [CEE_CONV_OVF_U1] = verify_conv,
    [CEE_CONV_OVF_U2] = verify_conv,
    [CEE_CONV_OVF_U4] = verify_conv,
    [CEE_CONV_OVF_I1_UN] = verify_conv,
    [CEE_CONV_OVF_I2_UN] = verify_conv,
    [CEE_CONV_OVF_I4_UN] = verify_conv,
    [CEE_CONV_OVF_U1_UN] = verify_conv,
    [CEE_CONV_OVF_U2_UN] = verify_conv,
    [CEE_CONV_OVF_U4_UN] = verify_conv,
    [CEE_CONV_OVF_I8] = verify_conv,
    [CEE_CONV_OVF_U8] = verify_conv,
    [CEE_CONV_OVF_I8_UN] = verify_conv,
    [CEE_CONV_OVF_U8_UN] = verify_conv,
    [CEE_CONV_OVF_I] = verify_conv,
    [CEE_CONV_OVF_U] = verify_conv,
    [CEE_CONV_OVF_I_UN] = verify_conv,
    [CEE_CONV_OVF_U_UN] = verify_conv,
    [CEE_CONV_I1] = verify_conv,
    [CEE_CONV_I2] = verify_conv,
    [CEE_CONV_I4] = verify_conv,
    [CEE_CONV_I8] = verify_conv,
    [CEE_CONV_U1] = verify_conv,
    [CEE_CONV_U2] = verify_conv,
    [CEE_CONV_U4] = verify_conv,
    [CEE_CONV_U8] = verify_conv,
    [CEE_CONV_I] = verify_conv,
    [CEE_CONV_U] = verify_conv,
    [CEE_CONV_R4] = verify_conv,
    [CEE_CONV_R8] = verify_conv,
    [CEE_CONV_R_UN] = verify_conv,

    [CEE_CEQ] = verify_compare,
    [CEE_CGT] = verify_compare,
    [CEE_CGT_UN] = verify_compare,
    [CEE_CLT] = verify_compare,
    [CEE_CLT_UN] = verify_compare,

    [CEE_NEWARR] = verify_newarr,
    [CEE_LDLEN] = verify_ldlen,
    [CEE_LDELEMA] = verify_ldelema,
    [CEE_LDELEM] = verify_ldelem,
    [CEE_LDELEM_REF] = verify_ldelem,
    [CEE_STELEM] = verify_stelem,
    [CEE_STELEM_REF] = verify_stelem,

    [CEE_LDFTN] = verify_ldftn,
    [CEE_LDVIRTFTN] = verify_ldftn,

    [CEE_NEWOBJ] = verify_call,
    [CEE_CALL] = verify_call,
    [CEE_CALLVIRT] = verify_call,

    [CEE_RET] = verify_ret,

    [CEE_CASTCLASS] = verify_castclass,
    [CEE_ISINST] = verify_castclass,

    [CEE_BOX] = verify_box,
    [CEE_UNBOX] = verify_unbox,
    [CEE_UNBOX_ANY] = verify_unbox,

    [CEE_BR] = verify_br,
    [CEE_BRFALSE] = verify_br_unary_cond,
    [CEE_BRTRUE] = verify_br_unary_cond,
    [CEE_BEQ] = verify_br_binary_cond,
    [CEE_BGE] = verify_br_binary_cond,
    [CEE_BGT] = verify_br_binary_cond,
    [CEE_BLE] = verify_br_binary_cond,
    [CEE_BLT] = verify_br_binary_cond,
    [CEE_BNE_UN] = verify_br_binary_cond,
    [CEE_BGE_UN] = verify_br_binary_cond,
    [CEE_BGT_UN] = verify_br_binary_cond,
    [CEE_BLE_UN] = verify_br_binary_cond,
    [CEE_BLT_UN] = verify_br_binary_cond,
    [CEE_SWITCH] = verify_switch,

    [CEE_LEAVE] = verify_leave,
    [CEE_ENDFINALLY] = verify_endfinally,
    [CEE_THROW] = verify_throw,

    [CEE_SIZEOF] = verify_sizeof,
};
size_t g_verify_dispatch_table_size = ARRAY_LENGTH(g_verify_dispatch_table);
