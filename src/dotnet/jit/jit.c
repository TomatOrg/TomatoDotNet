#include "tinydotnet/types/type.h"
#include "tinydotnet/jit/jit.h"
#include "util/except.h"
#include "util/stb_ds.h"
#include "tinydotnet/disasm.h"
#include "util/string_builder.h"
#include "jit_internal.h"

// TODO: lock

#include <spidir/spidir.h>
#include <stdbool.h>

static spidir_module_handle_t m_spidir_module;

// buildin functions
static spidir_function_t m_builtin_memset;
static spidir_function_t m_builtin_memcpy;
static spidir_function_t m_builtin_gc_new;
static spidir_function_t m_builtin_throw;

static RuntimeConstructorInfo m_IndexOutOfBoundsException_ctor;
static RuntimeConstructorInfo m_NullReferenceException_ctor;

/**
 * Get the default ctor for the given type, NULL if there isn't one
 */
static RuntimeConstructorInfo get_default_ctor(RuntimeTypeInfo type) {
    RuntimeConstructorInfo ctor = NULL;
    for (int i = 0; i < type->DeclaredConstructors->Length; i++) {
        RuntimeConstructorInfo c = type->DeclaredConstructors->Elements[i];
        if (c->Parameters->Length == 0) {
            ctor = c;
            break;
        }
    }
    return ctor;
}

tdn_err_t tdn_jit_init() {
    tdn_err_t err = TDN_NO_ERROR;

    // get all the default ctors
    m_IndexOutOfBoundsException_ctor = get_default_ctor(tIndexOutOfRangeException);
    CHECK(m_IndexOutOfBoundsException_ctor != NULL);

    m_NullReferenceException_ctor = get_default_ctor(tNullReferenceException);
    CHECK(m_NullReferenceException_ctor != NULL);


    m_spidir_module = spidir_module_create();

    //
    // Create built-in types
    //
    m_builtin_memset = spidir_module_create_extern_function(m_spidir_module,
                                         "memset",
                                         SPIDIR_TYPE_PTR,
                                         3, (spidir_value_type_t[]){
                                            SPIDIR_TYPE_PTR,
                                            SPIDIR_TYPE_I32,
                                            SPIDIR_TYPE_I64
                                        });

    m_builtin_memcpy = spidir_module_create_extern_function(m_spidir_module,
                                                "memcpy",
                                                SPIDIR_TYPE_PTR,
                                                3, (spidir_value_type_t[]){
                                                SPIDIR_TYPE_PTR,
                                                SPIDIR_TYPE_PTR,
                                                SPIDIR_TYPE_I64 });

    // TODO: change to have a ptr parameter
    m_builtin_gc_new = spidir_module_create_extern_function(m_spidir_module,
                                                            "gc_new",
                                                            SPIDIR_TYPE_PTR,
                                                            2, (spidir_value_type_t[]){ SPIDIR_TYPE_I64, SPIDIR_TYPE_I64 });

    m_builtin_throw = spidir_module_create_extern_function(m_spidir_module,
                                                              "throw",
                                                              SPIDIR_TYPE_NONE,
                                                              1, (spidir_value_type_t[]){ SPIDIR_TYPE_PTR });

cleanup:
    return err;
}

static spidir_dump_status_t stdout_dump_callback(const char* s, size_t size, void* ctx) {
    (void) ctx;
    tdn_host_printf("%.*s", size, s);
    return SPIDIR_DUMP_CONTINUE;
}

void tdn_jit_dump() {
    spidir_module_dump(m_spidir_module, stdout_dump_callback, NULL);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// SPIDIR helpers
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static spidir_value_type_t get_spidir_argument_type(RuntimeTypeInfo type) {
    if (
        type == tSByte || type == tByte ||
        type == tInt16 || type == tUInt16 ||
        type == tInt32 || type == tUInt32 ||
        type == tBoolean
    ) {
        return SPIDIR_TYPE_I32;
    } else if (type->BaseType == tEnum) {
        return get_spidir_argument_type(type->EnumUnderlyingType);
    } else if (
        type == tInt64 || type == tUInt64 ||
        type == tIntPtr || type == tUIntPtr
    ) {
        return SPIDIR_TYPE_I64;
    } else if (type == tVoid) {
        return SPIDIR_TYPE_NONE;
    } else if (tdn_type_is_valuetype(type)) {
        // pass by-reference, copied by the caller
        return SPIDIR_TYPE_PTR;
    } else {
        // this is def a pointer
        return SPIDIR_TYPE_PTR;
    }
}


static spidir_value_type_t get_spidir_return_type(RuntimeTypeInfo type) {
    if (
        type == tSByte || type == tByte ||
        type == tInt16 || type == tUInt16 ||
        type == tInt32 || type == tUInt32 ||
        type == tBoolean
    ) {
        return SPIDIR_TYPE_I32;
    } else if (type->BaseType == tEnum) {
        return get_spidir_return_type(type->EnumUnderlyingType);
    } else if (
        type == tInt64 || type == tUInt64 ||
        type == tIntPtr || type == tUIntPtr
    ) {
        return SPIDIR_TYPE_I64;
    } else if (type == tVoid) {
        return SPIDIR_TYPE_NONE;
    } else if (tdn_type_is_valuetype(type)) {
        // argument is passed by reference
        return SPIDIR_TYPE_NONE;
    } else {
        // this is def a pointer
        return SPIDIR_TYPE_PTR;
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// The jitter itself
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define SWAP(a, b) \
    do { \
        typeof(a) __temp = a; \
        a = b; \
        b = __temp; \
    } while (0)

/**
 * Helper to check if a type is a struct type and not any other type
 */
static bool jit_is_struct_type(RuntimeTypeInfo type) {
    type = tdn_get_intermediate_type(type);
    return
        tdn_type_is_valuetype(type) &&
        type != tInt32 &&
        type != tInt64 &&
        type != tIntPtr;
}

/**
 * Emit a memcpy with a known size
 */
static void jit_emit_memcpy(spidir_builder_handle_t builder, spidir_value_t ptr1, spidir_value_t ptr2, size_t size) {
    spidir_builder_build_call(builder, SPIDIR_TYPE_NONE, m_builtin_memcpy, 3,
                              (spidir_value_t[]){
                                        ptr1, ptr2,
                                        spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, size)
                                    });
}

/**
 * Search for the exception clause that the current PC is in
 */
static RuntimeExceptionHandlingClause find_exception_clause_for_pc(RuntimeMethodBase method, uint32_t pc) {
    RuntimeExceptionHandlingClause_Array arr = method->MethodBody->ExceptionHandlingClauses;
    if (arr == NULL) {
        return NULL;
    }

    for (int i = 0; i < arr->Length; i++) {
        RuntimeExceptionHandlingClause c = arr->Elements[i];
        if (c->TryOffset <= pc && pc < c->TryOffset + c->TryLength) {
            return c;
        }
    }

    return NULL;
}

/**
 * Throw a new exception
 */
static tdn_err_t jit_emit_throw_new(spidir_builder_handle_t builder, RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // create the new exception object
    spidir_value_t new = spidir_builder_build_call(builder, SPIDIR_TYPE_PTR, m_builtin_gc_new, 2,
                              (spidir_value_t[]){
                                      spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, (uint64_t)type),
                                      spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, type->StackSize)
                              });

    // find the default ctor
    // TODO: in the future we might have certain opcodes for this instead
    RuntimeConstructorInfo ctor;
    if (type == tIndexOutOfRangeException) {
        ctor = m_IndexOutOfBoundsException_ctor;
    } else if (type == tNullReferenceException) {
        ctor = m_NullReferenceException_ctor;
    } else {
        CHECK_FAIL();
    }
    CHECK(ctor->JitPrepared);

    // call it
    spidir_builder_build_call(builder, SPIDIR_TYPE_NONE, (spidir_function_t){ ctor->JitMethodId }, 1, (spidir_value_t[]){ new });

    // TODO: check if we can catch the exception right away
    // call the throwing
    spidir_builder_build_call(builder, SPIDIR_TYPE_NONE, m_builtin_throw, 1, (spidir_value_t[]){ new });

cleanup:
    return err;
}

static tdn_err_t jit_resolve_parameter_type(RuntimeMethodBase method, int arg, RuntimeTypeInfo* type) {
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
    }

    // return it
    *type = arg_type;

cleanup:
    return err;
}

static tdn_err_t resolve_and_verify_branch_target(eval_stack_t* stack, jit_label_t* labels, uint32_t target, jit_label_t** out_label) {
    tdn_err_t err = TDN_NO_ERROR;

    // if we have a branch target make sure we have the target label
    jit_label_t* target_label = jit_get_label(labels, target);
    CHECK(target_label != NULL);

    // stack consistency check
    if (target_label->snapshot.initialized) {
        // we have a snapshot, perform a merge as needed, only modify if we have not visited it yet
        CHECK_AND_RETHROW(eval_stack_merge(stack, &target_label->snapshot, !target_label->visited));
    } else {
        // otherwise create a snapshot of our stack
        CHECK_AND_RETHROW(eval_stack_snapshot(stack, &target_label->snapshot));
    }

    // give it back
    *out_label = target_label;

cleanup:
    return err;
}

static void jit_method_callback(spidir_builder_handle_t builder, void* _ctx) {
    jit_context_t* ctx = _ctx;
    tdn_err_t err = TDN_NO_ERROR;
    jit_label_t* labels = NULL;
    RuntimeTypeInfo* call_args_types = NULL;
    spidir_value_t* call_args_values = NULL;
    RuntimeMethodBase method = ctx->method;
    RuntimeExceptionHandlingClause_Array clauses = method->MethodBody->ExceptionHandlingClauses;
    struct {
        spidir_value_t value;
        RuntimeTypeInfo type;
        bool spilled;
    }* args = NULL;
    spidir_value_t* locals = NULL;
    eval_stack_t stack = {
        .max_depth = method->MethodBody->MaxStackSize
    };

    // take into account the first parameter might be an implicit
    // struct return pointer, we will just check if the stack size
    // is larger than 64bit, which can't be anything other than a
    // struct
    int args_offset = 0;
    if (method->ReturnParameter->ParameterType->StackSize > sizeof(uint64_t)) {
        args_offset = 1;
    }

    TRACE("%U::%U", ctx->method->DeclaringType->Name, ctx->method->Name);

    // get the this_type for future use
    RuntimeTypeInfo this_type = NULL;
    if (!method->Attributes.Static) {
        this_type = method->DeclaringType;
        if (tdn_type_is_valuetype(this_type)) {
            // this is a valuetype, the this is a reference
            CHECK_AND_RETHROW(tdn_get_byref_type(this_type, &this_type));
        }
    }

    // prepare table that will either save an argument reference or a spill,
    // fill with invalid entries
    int arg_count = (method->Parameters->Length + (this_type == NULL ? 0 : 1));
    args = tdn_host_mallocz(sizeof(*args) * arg_count);
    CHECK(args != NULL);
    for (int i = 0; i < arg_count; i++) {
        // resolve the parameter type
        RuntimeTypeInfo type;
        CHECK_AND_RETHROW(jit_resolve_parameter_type(method, i, &type));

        args[i].value = SPIDIR_VALUE_INVALID;
        args[i].type = type;
        args[i].spilled = false;
    }

    // prepare the locals by allocating their stack slots already
    if (method->MethodBody->LocalVariables != NULL) {
        locals = tdn_host_mallocz(sizeof(spidir_value_t) * method->MethodBody->LocalVariables->Length);
        for (int i = 0; i < method->MethodBody->LocalVariables->Length; i++) {
            RuntimeLocalVariableInfo var = method->MethodBody->LocalVariables->Elements[i];
            locals[i] = spidir_builder_build_stackslot(builder, var->LocalType->StackSize, var->LocalType->StackAlignment);
        }
    }

    //
    // first pass, find all of the labels, this will
    // also create all the different basic blocks on
    // the way
    //

    // entry block
    jit_label_t* entry_label = jit_add_label(&labels, 0);
    CHECK(entry_label != NULL);
    entry_label->block = spidir_builder_create_block(builder);
    spidir_builder_set_entry_block(builder, entry_label->block);
    spidir_builder_set_block(builder, entry_label->block);

    // get the rest of the blocks by creating the labels
    bool spilled = false;
    bool has_starg0_or_ldarga0 = false;
    uint32_t pc = 0;
    tdn_il_control_flow_t flow_control = TDN_IL_NEXT;
    while (pc != ctx->method->MethodBody->ILSize) {
        // decode instruction
        tdn_il_inst_t inst;
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));

        // check if we need to spill an argument
        if (inst.opcode == CEE_LDARGA || inst.opcode == CEE_STARG) {
            int arg = inst.operand.variable;
            CHECK(arg < arg_count);
            RuntimeTypeInfo type = args[arg].type;

            // create a stackslot for the spill
            if (!args[arg].spilled) {
                args[arg].value = spidir_builder_build_stackslot(builder, type->StackSize, type->StackAlignment);
                args[arg].spilled = true;

                // store it, if its a value-type we need to copy it instead
                spidir_value_t param_ref = spidir_builder_build_param_ref(builder, args_offset + arg);
                if (jit_is_struct_type(type)) {
                    jit_emit_memcpy(builder,
                                    args[arg].value,
                                    param_ref,
                                    type->StackSize);
                } else {
                    spidir_builder_build_store(builder, param_ref, args[arg].value);
                }
            }

            // needed for verifying call
            if (arg == 0) {
                has_starg0_or_ldarga0 = true;
            }

            // mark that we had a spill
            spilled = true;
        }

        // check the operand if it has any target, if it has we need to create all
        // the related labels
        // TODO: do switch
        if (inst.operand_type == TDN_IL_BRANCH_TARGET) {
            // add label for the destination
            jit_label_t* label = jit_add_label(&labels, inst.operand.branch_target);
            if (label != NULL) {
                label->block = spidir_builder_create_block(builder);
            }
        } else if (inst.operand_type == TDN_IL_SWITCH) {
            CHECK_FAIL();
        }

        // check how we got here, if we got here from one
        // of the following control flows we need to add
        // a label onto ourselves
        if (
            flow_control == TDN_IL_RETURN ||
            flow_control == TDN_IL_BRANCH ||
            flow_control == TDN_IL_COND_BRANCH ||
            flow_control == TDN_IL_THROW
        ) {
            jit_label_t* label = jit_add_label(&labels, pc);
            if (label != NULL) {
                label->block = spidir_builder_create_block(builder);
            }
        }

        pc += inst.length;
        flow_control = inst.control_flow;
    }

    // if we had a spill, the label at the start is no longer
    // actually at the start since it has spills, it should be
    // right after that
    if (spilled) {
        spidir_block_t entry = entry_label->block;
        entry_label->block = spidir_builder_create_block(builder);
        spidir_builder_build_branch(builder, entry_label->block);
        spidir_builder_set_block(builder, entry_label->block);
    }

    // TODO: support protected blocks properly

    //
    // the main jit function
    //
    int indent = 0;
    RuntimeExceptionHandlingClause current_clause = NULL;
    bool is_handler_clause = false;
    pc = 0;
    flow_control = TDN_IL_NEXT;
    int label_idx = 1; // skip the entry label,
                       // since its a special case
    while (pc != ctx->method->MethodBody->ILSize) {
        tdn_il_inst_t inst;
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));
        uint32_t next_pc = pc + inst.length;

        if (clauses != NULL) {
            for (int i = 0; i < clauses->Length; i++) {
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
        }

        // For debug, print the instruction
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
                string_builder_push_type_signature(&tmp_builder, inst.operand.field->DeclaringType);
                tdn_host_printf(" %s", string_builder_build(&tmp_builder));
                string_builder_free(&tmp_builder);
            } break;

            case TDN_IL_STRING: tdn_host_printf(" %U", inst.operand.string); break;
            case TDN_IL_SWITCH: CHECK_FAIL();
        }
        tdn_host_printf("\n");

        // normalize the instruction for easier processing now that we printed it
        tdn_normalize_inst(&inst);

        // check if there are more labels, if we are at a label we
        // need to properly switch to it
        bool has_label = false;
        if (label_idx < arrlen(labels)) {
            if (pc == labels[label_idx].address) {
                // found the current label
                jit_label_t* label = &labels[label_idx];
                spidir_block_t block = label->block;
                label->visited = true;
                has_label = true;

                // can't have a label between a
                // prefix and instruction, it must jump
                // to the first prefix
                CHECK(flow_control != TDN_IL_META);

                // check if we already have a stack slot at this location
                if (label->snapshot.initialized) {
                    // we do, perform a merge of the stack, only need the merge if we got
                    // to here via normal control flow
                    if (
                        flow_control == TDN_IL_NEXT ||
                        flow_control == TDN_IL_BREAK ||
                        flow_control == TDN_IL_CALL ||
                        flow_control == TDN_IL_COND_BRANCH
                    ) {
                        CHECK_AND_RETHROW(eval_stack_merge(&stack, &label->snapshot, true));
                    }
                } else {
                    // for verification, if we got to here via an instruction that can't jump
                    // to us make sure the stack got cleared correctly
                    if (
                        flow_control == TDN_IL_RETURN ||
                        flow_control == TDN_IL_BRANCH ||
                        flow_control == TDN_IL_THROW
                    ) {
                        CHECK(arrlen(stack.stack) == 0);
                    }

                    // and now take a snapshot of it
                    CHECK_AND_RETHROW(eval_stack_snapshot(&stack, &label->snapshot));
                }

                // check the last opcode to see how we got to this
                // new label
                if (
                    flow_control == TDN_IL_NEXT ||
                    flow_control == TDN_IL_BREAK ||
                    flow_control == TDN_IL_CALL
                ) {
                    CHECK_AND_RETHROW(eval_stack_move_to_slots(&stack, builder));

                    // we got from a normal instruction, insert a jump
                    spidir_builder_build_branch(builder, block);
                }

                // we are now at this block
                spidir_builder_set_block(builder, block);

                // and increment so we will check
                // against the next label
                label_idx++;
            }

            // make sure the label is always after or is at
            // the next pc, if there is another label
            if (label_idx < arrlen(labels)) {
                CHECK(labels[label_idx].address >= pc);
            }
        }

        // TODO: control flow handling

        //
        // Update the context for the jitting function
        //
        ctx->next_pc = next_pc;
        ctx->pc = pc;
        ctx->inst = &inst;
        ctx->stack = &stack;

        //
        // the main instruction jitting
        // TODO: split this to multiple functions in different places
        //
        switch (inst.opcode) {
            // load an argument
            case CEE_LDARG: {
                uint16_t arg = inst.operand.variable;
                CHECK(arg < arg_count);

                // get the argument we are loading
                RuntimeTypeInfo arg_type = tdn_get_intermediate_type(args[arg].type);

                if (args[arg].spilled) {
                    // was spilled, this is a stack slot
                    if (jit_is_struct_type(arg_type)) {
                        // use memcpy
                        spidir_value_t location;
                        CHECK_AND_RETHROW(eval_stack_alloc(&stack, builder, arg_type, &location));
                        jit_emit_memcpy(builder, location, args[arg].value, arg_type->StackSize);
                    } else if (arg_type == tInt32) {
                        // use a 32bit load
                        CHECK_AND_RETHROW(eval_stack_push(&stack, arg_type,
                                                          spidir_builder_build_load(builder, SPIDIR_TYPE_I32, args[arg].value)));
                    } else if (arg_type == tInt64 || arg_type == tIntPtr) {
                        // use a 64bit load
                        CHECK_AND_RETHROW(eval_stack_push(&stack, arg_type,
                                                          spidir_builder_build_load(builder, SPIDIR_TYPE_I64, args[arg].value)));
                    } else {
                        // use a pointer load
                        CHECK_AND_RETHROW(eval_stack_push(&stack, arg_type,
                                                          spidir_builder_build_load(builder, SPIDIR_TYPE_PTR, args[arg].value)));
                    }
                } else {
                    spidir_value_t param_ref = spidir_builder_build_param_ref(builder, args_offset + arg);

                    // was not spilled, this is a param-ref
                    if (jit_is_struct_type(arg_type)) {
                        // passed by pointer, memcpy to the stack
                        spidir_value_t location;
                        CHECK_AND_RETHROW(eval_stack_alloc(&stack, builder, arg_type, &location));
                        jit_emit_memcpy(builder, location, param_ref, arg_type->StackSize);
                    } else {
                        // just push it
                        stack_meta_t meta = {
                            .came_from_ldarg0 = arg == 0
                        };
                        CHECK_AND_RETHROW(eval_stack_push_with_meta(&stack, arg_type, param_ref, meta));
                    }
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Locals
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // load a local variable
            case CEE_LDLOC: {
                // verify the argument and get the stack type
                int var = inst.operand.variable;
                CHECK(var < method->MethodBody->LocalVariables->Length);

                RuntimeTypeInfo type = method->MethodBody->LocalVariables->Elements[var]->LocalType;
                type = tdn_get_intermediate_type(type);

                if (jit_is_struct_type(type)) {
                    // struct type, copy the stack slot to the eval stack
                    spidir_value_t loc;
                    CHECK_AND_RETHROW(eval_stack_alloc(&stack, builder, type, &loc));
                    jit_emit_memcpy(builder, loc, locals[var], type->StackSize);
                } else {
                    // not a struct type, load it from the stack slot
                    spidir_value_t value = spidir_builder_build_load(builder, get_spidir_return_type(type), locals[var]);
                    CHECK_AND_RETHROW(eval_stack_push(&stack, type, value));
                }
            } break;

            // load the pointer to a local variable
            case CEE_LDLOCA: {
                int var = inst.operand.variable;
                CHECK(var < method->MethodBody->LocalVariables->Length);

                RuntimeTypeInfo type = method->MethodBody->LocalVariables->Elements[var]->LocalType;
                type = tdn_get_verification_type(type);
                CHECK_AND_RETHROW(tdn_get_byref_type(type, &type));

                CHECK_AND_RETHROW(eval_stack_push(&stack, type, locals[var]));
            } break;

            // store to a local variable
            case CEE_STLOC: {
                // verify the argument and get the stack type
                int var = inst.operand.variable;
                CHECK(var < method->MethodBody->LocalVariables->Length);

                spidir_value_t value;
                RuntimeTypeInfo value_type;
                CHECK_AND_RETHROW(eval_stack_pop(&stack, builder, &value_type, &value, NULL));

                // check the type
                CHECK(tdn_type_verifier_assignable_to(
                        value_type,
                        method->MethodBody->LocalVariables->Elements[var]->LocalType));

                if (jit_is_struct_type(value_type)) {
                    // struct type, copy the stack slot to the eval stack
                    jit_emit_memcpy(builder, locals[var], value, value_type->StackSize);
                } else {
                    // not a struct type, just store it
                    spidir_builder_build_store(builder, value, locals[var]);
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Object related
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // load a field
            case CEE_LDFLD:
            case CEE_LDFLDA: {
                RuntimeFieldInfo field = inst.operand.field;
                bool ldarga = inst.opcode == CEE_LDFLDA;

                // TODO: check field accessibility

                // pop the item
                spidir_value_t obj;
                RuntimeTypeInfo obj_type;
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &obj_type, &obj, NULL));

                // check this is either an object or a managed pointer
                CHECK(
                    obj_type->IsByRef ||
                    tdn_type_is_referencetype(obj_type) ||
                    (jit_is_struct_type(obj_type) && !ldarga)
                );

                // TODO: verify the field is contained within the given object

                // get the stack type of the field
                RuntimeTypeInfo field_type = inst.operand.field->FieldType;
                RuntimeTypeInfo value_type = tdn_get_intermediate_type(field_type);

                // figure the pointer to the field itself
                spidir_value_t field_ptr;
                if (field->Attributes.Static) {
                    // static field
                    // TODO: get a pointer to the static field
                    CHECK_FAIL();
                } else {
                    // instance field
                    if (field->FieldOffset == 0) {
                        // field is at offset zero, just load it
                        field_ptr = obj;
                    } else {
                        // build an offset to the field
                        field_ptr = spidir_builder_build_ptroff(builder, obj,
                                                          spidir_builder_build_iconst(builder,
                                                                                      SPIDIR_TYPE_I64,
                                                                                      field->FieldOffset));
                    }
                }

                if (ldarga) {
                    // TODO: emit null check

                    // for reference to field we don't need the load
                    CHECK_AND_RETHROW(tdn_get_byref_type(value_type, &value_type));
                    CHECK_AND_RETHROW(eval_stack_push(&stack, value_type, field_ptr));
                } else {
                    // perform the actual load
                    if (jit_is_struct_type(field_type)) {
                        // we are copying a struct to the stack
                        spidir_value_t value;
                        CHECK_AND_RETHROW(eval_stack_alloc(&stack, builder, value_type, &value));
                        jit_emit_memcpy(builder, value, field_ptr, field_type->StackSize);
                    } else {
                        // we are copying a simpler value
                        // TODO: once supported set the load size
                        spidir_value_type_t type;
                        if (value_type == tInt32) {
                            type = SPIDIR_TYPE_I32;
                        } else if (value_type == tInt64 || value_type == tIntPtr) {
                            type = SPIDIR_TYPE_I64;
                        } else {
                            type = SPIDIR_TYPE_PTR;
                        }
                        spidir_value_t value = spidir_builder_build_load(builder, type, field_ptr);
                        CHECK_AND_RETHROW(eval_stack_push(&stack, value_type, value));
                    }
                }

            } break;

            // load an element from an array
            case CEE_LDELEMA:
            case CEE_LDELEM:
            case CEE_LDELEM_REF: {
                // pop the items
                spidir_value_t index, array;
                RuntimeTypeInfo index_type, array_type;
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &index_type, &index, NULL));
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &array_type, &array, NULL));

                // verify the types
                CHECK(array_type->IsArray);
                CHECK(index_type == tInt32 || index_type == tIntPtr);

                // TODO: extend the index to 64bit for the length check

                // figure the element type
                RuntimeTypeInfo type = inst.operand.type;
                if (inst.opcode == CEE_LDELEM_REF) {
                    // figure from the array, must be a pointer type
                    type = array_type->ElementType;
                    CHECK(tdn_type_is_referencetype(type));
                } else {
                    // make sure the wanted type matches the array type
                    CHECK(tdn_type_array_element_compatible_with(array_type->ElementType, type));
                }
                RuntimeTypeInfo tracked = tdn_get_verification_type(type);

                spidir_block_t length_is_valid = spidir_builder_create_block(builder);
                spidir_block_t length_is_invalid = spidir_builder_create_block(builder);

                // length check, this will also take care of the NULL check since if
                // we have a null value we will just fault in here
                spidir_value_t length_offset = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, offsetof(struct Array, Length));
                spidir_value_t length_ptr = spidir_builder_build_ptroff(builder, array, length_offset);
                spidir_value_t length = spidir_builder_build_load(builder, SPIDIR_TYPE_I32, length_ptr);
                // TODO: extend the length to a 64bit number for the comparison

                // make sure the index < length
                spidir_value_t length_check = spidir_builder_build_icmp(builder, SPIDIR_ICMP_SLT, SPIDIR_TYPE_I32, index, length);
                spidir_builder_build_brcond(builder, length_check, length_is_valid, length_is_invalid);

                // perform the invalid length branch, throw an exception
                spidir_builder_set_block(builder, length_is_invalid);
                jit_emit_throw_new(builder, tIndexOutOfRangeException);

                // perform the valid length branch, load the value from the array
                spidir_builder_set_block(builder, length_is_valid);
                spidir_value_t element_size = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, tracked->StackSize);
                spidir_value_t array_size = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, tArray->HeapSize);
                spidir_value_t byte_offset = spidir_builder_build_imul(builder, element_size, index);
                spidir_value_t abs_offset = spidir_builder_build_iadd(builder, array_size, byte_offset);
                spidir_value_t element_ptr = spidir_builder_build_ptroff(builder, array, abs_offset);

                if (inst.opcode == CEE_LDELEMA) {
                    // just need the address, we don't need an explicit null check
                    // since we are going to get a fault on the length check if
                    // the array is null
                    CHECK_AND_RETHROW(tdn_get_byref_type(tracked, &tracked));
                    CHECK_AND_RETHROW(eval_stack_push(&stack, tracked, element_ptr));
                } else {
                    // perform the actual load
                    if (jit_is_struct_type(tracked)) {
                        // we are copying a struct to the stack
                        spidir_value_t value;
                        CHECK_AND_RETHROW(eval_stack_alloc(&stack, builder, tracked, &value));
                        jit_emit_memcpy(builder, value, element_ptr, tracked->StackSize);
                    } else {
                        // we are copying a simpler value
                        // TODO: once supported set the load size
                        spidir_value_type_t spidir_type;
                        if (tracked == tInt32) {
                            spidir_type = SPIDIR_TYPE_I32;
                        } else if (tracked == tInt64 || tracked == tIntPtr) {
                            spidir_type = SPIDIR_TYPE_I64;
                        } else {
                            spidir_type = SPIDIR_TYPE_PTR;
                        }
                        spidir_value_t value = spidir_builder_build_load(builder, spidir_type, element_ptr);
                        CHECK_AND_RETHROW(eval_stack_push(&stack, tracked, value));
                    }
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Control flow
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_CALL:
            case CEE_CALLVIRT: {
                RuntimeMethodBase target = inst.operand.method;
                bool is_static = target->Attributes.Static;
                bool is_call = inst.opcode == CEE_CALL;
                bool is_callvirt = inst.opcode == CEE_CALLVIRT;

                // make sure we can actually call it
                CHECK(target->JitPrepared);

                // TODO: check method accessibility

                // verify we can call it
                CHECK(!target->Attributes.Abstract);
                if (is_callvirt) CHECK(!target->Attributes.Static);

                // get all the arguments
                int call_args_count = target->Parameters->Length + (is_static ? 0 : 1);
                arrsetlen(call_args_types, call_args_count);
                arrsetlen(call_args_values, call_args_count);
                for (int i = call_args_count - 1; i >= 0; i--) {
                    // pop it
                    stack_meta_t meta;
                    CHECK_AND_RETHROW(eval_stack_pop(&stack, builder, &call_args_types[i], &call_args_values[i], &meta));

                    // validate the stack type
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
                                // we are doing a direct call, so we need to emit an explicit null check
                                spidir_block_t null_ptr = spidir_builder_create_block(builder);
                                spidir_block_t non_null_ptr = spidir_builder_create_block(builder);

                                // make sure the ptr != NULL
                                spidir_value_t null_check = spidir_builder_build_icmp(builder, SPIDIR_ICMP_NE, SPIDIR_TYPE_I32,
                                                                                      call_args_values[0],
                                                                                      spidir_builder_build_iconst(builder, SPIDIR_TYPE_PTR, 0));
                                spidir_builder_build_brcond(builder, null_check, null_ptr, non_null_ptr);

                                // perform the null pointer branch, throw an exception
                                spidir_builder_set_block(builder, null_ptr);
                                jit_emit_throw_new(builder, tNullReferenceException);

                                // and return to the non-null flow
                                spidir_builder_set_block(builder, non_null_ptr);
                            }
                        } else {
                            target_type = target->Parameters->Elements[i - 1]->ParameterType;
                        }
                    } else {
                        target_type = target->Parameters->Elements[i]->ParameterType;
                    }

                    // check that we can do the assignment
                    CHECK(tdn_type_verifier_assignable_to(call_args_types[i], target_type));
                }

                // TODO: indirect calls, de-virt

                // handle the return type
                RuntimeTypeInfo ret_type = tdn_get_intermediate_type(target->ReturnParameter->ParameterType);
                if (jit_is_struct_type(ret_type)) {
                    // need to allocate the space in the caller
                    CHECK_FAIL();
                } else {
                    // emit the actual call
                    spidir_value_t value = spidir_builder_build_call(builder,
                                                                     get_spidir_return_type(ret_type),
                                                                     (spidir_function_t){ target->JitMethodId },
                                                                     call_args_count, call_args_values);

                    // if has a return type then push it
                    if (ret_type != tVoid) {
                        CHECK_AND_RETHROW(eval_stack_push(&stack, ret_type, value));
                    }
                }

                // cleanup everything
                arrsetlen(call_args_types, 0);
                arrsetlen(call_args_values, 0);
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Control flow
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // unconditional branch
            case CEE_BR: {
                CHECK_AND_RETHROW(eval_stack_move_to_slots(&stack, builder));

                // get and validate the label
                jit_label_t* target_label = NULL;
                CHECK_AND_RETHROW(resolve_and_verify_branch_target(&stack, labels, inst.operand.branch_target, &target_label));
                CHECK(find_exception_clause_for_pc(method, target_label->address) == current_clause);

                // a branch, emit the branch
                spidir_builder_build_branch(builder, target_label->block);

                // because we don't fall-through we clear the stack
                eval_stack_clear(&stack);
            } break;

            // conditional branches
            case CEE_BRFALSE:
            case CEE_BRTRUE: {
                // pop the item
                spidir_value_t value;
                RuntimeTypeInfo value_type;
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value_type, &value, NULL));

                // ECMA-335 doesn't say brtrue takes in anything but
                // O and native int, but I think its just an oversight
                CHECK(
                    tdn_type_is_referencetype(value_type) ||
                    value_type == tInt32 ||
                    value_type == tInt64 ||
                    value_type == tIntPtr);

                // create the comparison
                spidir_icmp_kind_t kind = inst.opcode == CEE_BRTRUE ? SPIDIR_ICMP_NE : SPIDIR_ICMP_EQ;
                spidir_value_t cmp = spidir_builder_build_icmp(builder,
                                            kind, SPIDIR_TYPE_I32, value,
                                            spidir_builder_build_iconst(
                                                    builder, get_spidir_argument_type(value_type), 0));

                // check if one of the blocks needs to have the stack in stack slots
                CHECK_AND_RETHROW(eval_stack_move_to_slots(&stack, builder));

                // get the jump locations
                jit_label_t* target_label = NULL;
                CHECK_AND_RETHROW(resolve_and_verify_branch_target(&stack, labels, inst.operand.branch_target, &target_label));
                CHECK(find_exception_clause_for_pc(method, target_label->address) == current_clause);

                jit_label_t* next_label = NULL;
                CHECK_AND_RETHROW(resolve_and_verify_branch_target(&stack, labels, next_pc, &next_label));
                CHECK(find_exception_clause_for_pc(method, next_label->address) == current_clause);

                // a branch, emit the branch
                spidir_builder_build_brcond(builder, cmp, target_label->block, next_label->block);
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
                spidir_value_t value1, value2;
                RuntimeTypeInfo value1_type, value2_type;
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value2_type, &value2, NULL));
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value1_type, &value1, NULL));

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

                // spidir only has the one side, need to flip for the other side
                spidir_icmp_kind_t kind;
                bool compare = false;
                switch (inst.opcode) {
                    case CEE_CEQ: compare = true;
                    case CEE_BEQ: kind = SPIDIR_ICMP_EQ; break;
                    case CEE_BGE: kind = SPIDIR_ICMP_SLE; SWAP(value1, value2); break;
                    case CEE_CGT: compare = true;
                    case CEE_BGT: kind = SPIDIR_ICMP_SLT; SWAP(value1, value2); break;
                    case CEE_BLE: kind = SPIDIR_ICMP_SLE; break;
                    case CEE_CLT: compare = true;
                    case CEE_BLT: kind = SPIDIR_ICMP_SLT; break;
                    case CEE_BNE_UN: kind = SPIDIR_ICMP_NE; break;
                    case CEE_BGE_UN: kind = SPIDIR_ICMP_ULE; SWAP(value1, value2); break;
                    case CEE_CGT_UN: compare = true;
                    case CEE_BGT_UN: kind = SPIDIR_ICMP_ULT; SWAP(value1, value2); break;
                    case CEE_BLE_UN: kind = SPIDIR_ICMP_ULE; break;
                    case CEE_CLT_UN: compare = true;
                    case CEE_BLT_UN: kind = SPIDIR_ICMP_ULT; break;
                    default: CHECK_FAIL();
                }

                // create the comparison
                spidir_value_t cmp = spidir_builder_build_icmp(builder,
                                                               kind,
                                                               SPIDIR_TYPE_I32,
                                                               value1, value2);

                // check if its a compare or not
                if (compare) {
                    // a compare, just push the result as an int32
                    eval_stack_push(&stack, tInt32, cmp);
                } else {
                    // get the jump locations
                    jit_label_t* target_label = NULL;
                    CHECK_AND_RETHROW(resolve_and_verify_branch_target(&stack, labels, inst.operand.branch_target, &target_label));
                    CHECK(find_exception_clause_for_pc(method, target_label->address) == current_clause);

                    jit_label_t* next_label = NULL;
                    CHECK_AND_RETHROW(resolve_and_verify_branch_target(&stack, labels, next_pc, &next_label));
                    CHECK(find_exception_clause_for_pc(method, next_label->address) == current_clause);

                    // move to slots before we jump
                    CHECK_AND_RETHROW(eval_stack_move_to_slots(&stack, builder));

                    // a branch, emit the branch
                    spidir_builder_build_brcond(builder, cmp, target_label->block, next_label->block);
                }
            } break;

            // return value from the function
            case CEE_RET: {
                // make sure we are not trying to return from within a protected block
                CHECK(current_clause == NULL);

                RuntimeTypeInfo wanted_ret_type = method->ReturnParameter->ParameterType;
                if (wanted_ret_type == tVoid) {
                    spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
                } else {
                    RuntimeTypeInfo ret_type;
                    spidir_value_t ret_value;
                    eval_stack_pop(&stack, builder, &ret_type, &ret_value, NULL);

                    // make sure the type is a valid return target
                    CHECK(tdn_type_verifier_assignable_to(ret_type, wanted_ret_type));

                    if (jit_is_struct_type(ret_type)) {
                        // returning a struct, need to use the implicit
                        // ret pointer
                        jit_emit_memcpy(builder,
                                        spidir_builder_build_param_ref(builder, 0),
                                        ret_value,
                                        ret_type->StackSize);

                        // and return without a ret value
                        spidir_builder_build_return(builder, SPIDIR_VALUE_INVALID);
                    } else {
                        // returning a normal pointer sized thing
                        spidir_builder_build_return(builder, ret_value);
                    }
                }
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Exception control flow
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_LEAVE: {
                CHECK_FAIL();
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Stack manipulation
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // Push an int32 to the stack
            case CEE_LDC_I4: {
                // NOTE: we are treating the value as a uint32 so it will not sign extend it
                CHECK_AND_RETHROW(eval_stack_push(&stack, tInt32,
                                spidir_builder_build_iconst(builder,
                                                            SPIDIR_TYPE_I32, inst.operand.uint32)));
            } break;

            // Push an int64
            case CEE_LDC_I8: {
                CHECK_AND_RETHROW(eval_stack_push(&stack, tInt64,
                                spidir_builder_build_iconst(builder,
                                                            SPIDIR_TYPE_I64, inst.operand.uint64)));
            } break;

            // Pop a value and dup it
            case CEE_DUP: {
                // pop the value and ignore it
                RuntimeTypeInfo type;
                spidir_value_t value;
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &type, &value, NULL));

                // and now push it twice
                CHECK_AND_RETHROW(eval_stack_push(&stack, type, value));
                CHECK_AND_RETHROW(eval_stack_push(&stack, type, value));
            } break;

            // Pop a value and ignore it
            case CEE_POP: {
                // pop the value and ignore it
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, NULL, NULL, NULL));
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Math related
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            // shifts
            case CEE_SHL:
            case CEE_SHR:
            case CEE_SHR_UN: {
                // pop the items
                spidir_value_t value, shift_amount;
                RuntimeTypeInfo value_type, shift_amount_type;
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &shift_amount_type, &shift_amount, NULL));
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value_type, &value, NULL));

                // type check
                CHECK(value_type == tInt32 || value_type == tInt64 || value_type == tIntPtr);
                CHECK(shift_amount_type == tInt32 || shift_amount_type == tIntPtr);

                // perform the operation
                spidir_value_t result_value;
                switch (inst.opcode) {
                    case CEE_SHL: result_value = spidir_builder_build_shl(builder, value, shift_amount); break;
                    case CEE_SHR: result_value = spidir_builder_build_ashr(builder, value, shift_amount); break;
                    case CEE_SHR_UN: result_value = spidir_builder_build_lshr(builder, value, shift_amount); break;
                    default: CHECK_FAIL();
                }

                // push it to the stack
                CHECK_AND_RETHROW(eval_stack_push(&stack, value_type, result_value));
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
                spidir_value_t value1, value2;
                RuntimeTypeInfo value1_type, value2_type;
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value2_type, &value2, NULL));
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value1_type, &value1, NULL));

                // figure the type we are going to use
                RuntimeTypeInfo result = NULL;
                if (value1_type == tInt32) {
                    if (value2_type == tInt32) {
                        result = tInt32;
                    } else if (value2_type == tIntPtr) {
                        result = tIntPtr;
                    } else {
                        CHECK_FAIL();
                    }
                } else if (value1_type == tInt64) {
                    CHECK(value2_type == tInt64);
                    result = tInt64;
                } else if (value1_type == tIntPtr) {
                    if (value2_type == tInt32 || value2_type == tIntPtr) {
                        result = tIntPtr;
                    } else {
                        CHECK_FAIL();
                    }
                } else {
                    CHECK_FAIL();
                }

                // TODO: for floats make sure it is an instruction that can take floats

                // create the operation
                spidir_value_t result_value;
                switch (inst.opcode) {
                    case CEE_ADD: result_value = spidir_builder_build_iadd(builder, value1, value2); break;
                    case CEE_SUB: result_value = spidir_builder_build_isub(builder, value1, value2); break;
                    case CEE_AND: result_value = spidir_builder_build_and(builder, value1, value2); break;
                    case CEE_OR: result_value = spidir_builder_build_or(builder, value1, value2); break;
                    case CEE_XOR: result_value = spidir_builder_build_xor(builder, value1, value2); break;
                    case CEE_MUL: result_value = spidir_builder_build_imul(builder, value1, value2); break;
                    case CEE_DIV: result_value = spidir_builder_build_sdiv(builder, value1, value2); break;
                    case CEE_DIV_UN: result_value = spidir_builder_build_udiv(builder, value1, value2); break;
                    case CEE_REM: CHECK_FAIL("TODO: spidir is missing spidir_builder_build_irem");
                    case CEE_REM_UN: CHECK_FAIL("TODO: spidir is missing spidir_builder_build_urem");
                    default: CHECK_FAIL();
                }

                // push it to the stack
                CHECK_AND_RETHROW(eval_stack_push(&stack, result, result_value));
            } break;

            // bitwise not, emulate with value ^ ~0
            case CEE_NOT: {
                // pop the item
                spidir_value_t value;
                RuntimeTypeInfo value_type;
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value_type, &value, NULL));

                // type check, also get the ones value for the width
                // for the xor operation
                spidir_value_t ones;
                if (value_type == tInt32) {
                    ones = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I32, (uint32_t)~0u);
                } else if (value_type == tInt64 || value_type == tIntPtr) {
                    ones = spidir_builder_build_iconst(builder, SPIDIR_TYPE_I64, (uint64_t)~0ull);
                } else {
                    CHECK_FAIL();
                }

                // create the operation
                spidir_value_t result_value = spidir_builder_build_xor(builder, value, ones);

                // push it to the stack
                CHECK_AND_RETHROW(eval_stack_push(&stack, value_type, result_value));
            } break;

            // negation, emulate with 0 - value
            case CEE_NEG: {
                // pop the item
                spidir_value_t value;
                RuntimeTypeInfo value_type;
                CHECK_AND_RETHROW(eval_stack_pop(ctx->stack, builder, &value_type, &value, NULL));

                // type check, also get the ones value for the width
                // for the xor operation
                spidir_value_type_t type;
                if (value_type == tInt32) {
                    type = SPIDIR_TYPE_I32;
                } else if (value_type == tInt64 || value_type == tIntPtr) {
                    type = SPIDIR_TYPE_I64;
                } else {
                    CHECK_FAIL();
                }

                // TODO: floating point

                // create the operation
                spidir_value_t zero = spidir_builder_build_iconst(builder, type, 0);
                spidir_value_t result_value = spidir_builder_build_isub(builder, zero, value);

                // push it to the stack
                CHECK_AND_RETHROW(eval_stack_push(&stack, value_type, result_value));
            } break;

            ////////////////////////////////////////////////////////////////////////////////////////////////////////////
            // Misc operations
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////

            case CEE_NOP: {
                // do nothing
            } break;

            default:
                CHECK_FAIL();
        }

        // move the pc forward
        pc = next_pc;
        flow_control = inst.control_flow;

        if (clauses != NULL) {
            for (int i = 0; i < clauses->Length; i++) {
                RuntimeExceptionHandlingClause c = clauses->Elements[i];
                if (c->TryOffset + c->TryLength == pc) {
                    indent -= 4;
                    tdn_host_printf("[*] \t\t\t%*s} // end .try\n", indent, "");
                } else if (c->HandlerOffset + c->HandlerLength == pc) {
                    indent -= 4;
                    tdn_host_printf("[*] \t\t\t%*s} // end handler\n", indent, "");
                }
            }
        }
    }

    // make sure we went over all of the labels
    CHECK(label_idx == arrlen(labels));

cleanup:
    for (int i = 0; i < arrlen(labels); i++) {
        arrfree(labels[i].snapshot.stack);
    }
    arrfree(labels);
    arrfree(call_args_values);
    arrfree(call_args_types);
    eval_stack_free(&stack);
    tdn_host_free(locals);
    tdn_host_free(args);
}

/**
 * Prepares a method for jitting, this essentially creates the spidir function
 * so it will be ready for when we call it
 */
static tdn_err_t jit_prepare_method(RuntimeMethodBase method) {
    tdn_err_t err = TDN_NO_ERROR;
    spidir_value_type_t* params = NULL;
    string_builder_t builder = {};

    // check if already jitted
    if (method->JitPrepared) {
        goto cleanup;
    }
    method->JitPrepared = 1;

    // TODO: external methods need special handling

    // handle the return value, we have a special case of returning a struct, if it can't be returned
    // by value then it will be returned by passing an implicit pointer
    spidir_value_type_t ret_type = get_spidir_return_type(method->ReturnParameter->ParameterType);
    if (ret_type == SPIDIR_TYPE_NONE && method->ReturnParameter->ParameterType != tVoid) {
        arrpush(params, SPIDIR_TYPE_PTR);
    }

    // handle the `this` argument, its always a
    // pointer (byref for struct types)
    if (!method->Attributes.Static) {
        arrpush(params, SPIDIR_TYPE_PTR);
    }

    // handle the arguments
    for (size_t i = 0; i < method->Parameters->Length; i++) {
        arrpush(params, get_spidir_argument_type(method->Parameters->Elements[i]->ParameterType));
    }

    // generate the name
    string_builder_push_method_signature(&builder, method, true);
    const char* name = string_builder_build(&builder);

    // create the function itself
    spidir_function_t func = spidir_module_create_function(
        m_spidir_module,
        name, ret_type, arrlen(params), params
    );
    method->JitMethodId = func.id;

cleanup:
    string_builder_free(&builder);
    arrfree(params);

    return err;
}

/**
 * Actually jits a method, prepares it if needed
 */
static tdn_err_t jit_method(jit_context_t* ctx) {
    tdn_err_t err = TDN_NO_ERROR;
    RuntimeMethodBase method = ctx->method;

    // check if already jitted
    if (method->JitStarted) {
        goto cleanup;
    }
    method->JitStarted = 1;

    // prepare the method
    CHECK_AND_RETHROW(jit_prepare_method(method));

    // perform a first pass to find all the methods we call from this function
    // and prepare them so we can call them inside this function
    uint32_t pc = 0;
    tdn_il_control_flow_t flow_control = TDN_IL_NEXT;
    while (pc != ctx->method->MethodBody->ILSize) {
        // decode instruction
        tdn_il_inst_t inst;
        CHECK_AND_RETHROW(tdn_disasm_inst(method, pc, &inst));
        pc += inst.length;

        if (
            inst.opcode == CEE_CALL ||
            inst.opcode == CEE_NEWOBJ
        ) {
            // for direct calls we call it directly
            CHECK_AND_RETHROW(jit_prepare_method(inst.operand.method));
        } else if (
            inst.opcode == CEE_LDELEM ||
            inst.opcode == CEE_LDELEMA ||
            inst.opcode == CEE_LDELEM_REF ||
            inst.opcode == CEE_STELEM ||
            inst.opcode == CEE_STELEM_REF
        ) {
            // these have an explicit length-check
            CHECK_AND_RETHROW(jit_prepare_method((RuntimeMethodBase)m_IndexOutOfBoundsException_ctor));
        } else if (
            inst.opcode == CEE_CALLVIRT
        ) {
            // these have an explicit null check
            CHECK_AND_RETHROW(jit_prepare_method((RuntimeMethodBase)m_NullReferenceException_ctor));
        }
    }

    // now call the builder so we can actually build it
    spidir_module_build_function(m_spidir_module,
                                 (spidir_function_t){ method->JitMethodId },
                                 jit_method_callback, ctx);

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// High-level apis
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tdn_err_t tdn_jit_method(RuntimeMethodBase methodInfo) {
    tdn_err_t err = TDN_NO_ERROR;

    jit_context_t ctx = {
        .method = methodInfo
    };
    CHECK_AND_RETHROW(jit_method(&ctx));

cleanup:
    return err;
}

tdn_err_t tdn_jit_type(RuntimeTypeInfo type) {
    tdn_err_t err = TDN_NO_ERROR;

    // jit all the virtual methods, as those are the one that can be called
    // by other stuff unknowingly, the rest are going to be jitted lazyily
    for (int i = 0; i < type->DeclaredMethods->Length; i++) {
        RuntimeMethodBase method = (RuntimeMethodBase)type->DeclaredMethods->Elements[i];
        if (!method->Attributes.Virtual) continue;
        CHECK_AND_RETHROW(tdn_jit_method(method));
    }

cleanup:
    return err;
}
