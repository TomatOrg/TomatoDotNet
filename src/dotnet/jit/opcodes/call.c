#include "../jit_internal.h"
#include "util/except.h"
#include "dotnet/opcodes.h"

#ifdef JIT_TRACE_MIR
#define MIR_append_insn(...) MIR_append_insn_output(__VA_ARGS__)
#endif


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Handles all 3 of the calling functions:
//      - call
//      - callvirt
//      - newobj
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#define constrainedType ctx->constrainedType
#define ftnMethod ctx->ftnMethod

/**
 * we are going to do NEWOBJ in here as well, because it is essentially like a call
 * but we create the object right now instead of getting it from the stack, so I
 * think this will remove alot of duplicate code if we just handle it in here
 */
err_t jit_emit_call(jit_method_context_t* ctx, opcode_t opcode) {
    err_t err = NO_ERROR;
    MIR_op_t* arg_ops = NULL;

    System_Type ret_type = type_get_underlying_type(operand_method->ReturnType);

    // count the amount of arguments, +1 if we have a this
    int arg_count = operand_method->Parameters->Length;
    bool aggressive_inlining = method_is_aggressive_inlining(operand_method);

    if (opcode == CEE_NEWOBJ) {
        // we are creating a new instance of this class, make sure it is
        // properly filled and that we create all its virtual methods
        CHECK_AND_RETHROW(jit_prepare_instance_type(ctx->ctx, operand_method->DeclaringType));

        // newobj must call a ctor, we verify that ctors are good
        // in the loader
        CHECK(method_is_rt_special_name(operand_method));
        CHECK(string_equals_cstr(operand_method->Name, ".ctor"));

        if (ftnMethod != NULL) {
            // we had an ldftn/ldvirtftb, meaning that we are going to
            // create a delegate now, make sure of that
            CHECK(operand_method->DeclaringType->BaseType == tSystem_MulticastDelegate);
            CHECK(operand_method->Parameters->Length == 2);
            CHECK(operand_method->Parameters->Data[0]->ParameterType == tSystem_Object);
            CHECK(operand_method->Parameters->Data[1]->ParameterType == tSystem_IntPtr);

            // verify that the method signature matches the delegate we
            // want to create
            System_Reflection_MethodInfo signature = operand_method->DeclaringType->DelegateSignature;
            CHECK(signature != NULL);
            CHECK(signature->ReturnType == ftnMethod->ReturnType);
            CHECK(signature->Parameters->Length == ftnMethod->Parameters->Length);
            for (int i = 0; i < signature->Parameters->Length; i++) {
                CHECK(signature->Parameters->Data[i]->ParameterType == ftnMethod->Parameters->Data[i]->ParameterType);
            }
        } else {
            // make sure that this is *NOT* a delegate
            CHECK(operand_method->DeclaringType->BaseType != tSystem_MulticastDelegate);
        }
    } else if (opcode == CEE_CALLVIRT) {
        // callvirt must call an instance methods
        CHECK(!method_is_static(operand_method));
    } else {
        // call must call a method with a body
        CHECK(!method_is_abstract(operand_method));
    }

    // prepare array of all the operands
    // 1st is the prototype
    // 2nd is the reference
    // 3rd is exception return
    // 4rd is return type (optionally)
    // 5th is this type (optionally)
    // Rest are the arguments
    size_t other_args = 3;
    if (ret_type != NULL) other_args++;
    if (!method_is_static(operand_method)) other_args++;
    arg_ops = malloc((other_args + arg_count) * sizeof(MIR_op_t));

    // to track if we passed local refs
    bool ret_is_non_local_ref = true;

    // pop all the arguments from the stack
    int i;
    for (i = arg_count + other_args - 1; i >= other_args; i--) {
        System_Reflection_ParameterInfo parameter_info = operand_method->Parameters->Data[i - other_args];
        System_Type signature_type = parameter_info->ParameterType;

        // get the argument value
        stack_entry_t arg_entry;
        MIR_reg_t arg_reg;
        System_Type arg_type;
        CHECK_AND_RETHROW(stack_pop(ctx, &arg_type, &arg_reg, &arg_entry));

        // do implicit conversion as needed
        bool mem_op = false;
        switch (type_get_stack_type(arg_type)) {
            case STACK_TYPE_O: {
                if (type_is_interface(signature_type)) {
                    if (!type_is_interface(arg_type)) {
                        // object --> interface
                        MIR_reg_t int_reg = jit_new_temp_reg(ctx, signature_type);
                        CHECK_AND_RETHROW(jit_cast_obj_to_interface(ctx, int_reg, arg_reg, arg_type, signature_type));

                        // we now have that class
                        arg_reg = int_reg;
                        arg_type = signature_type;
                    }

                    // pass by value
                    mem_op = true;
                } else {
                    if (type_is_interface(arg_type)) {
                        // interface --> object
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                         MIR_new_reg_op(mir_ctx, arg_reg),
                                                         MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), arg_reg, 0, 1)));
                    }
                }
            } break;

            case STACK_TYPE_INT32: {
                if (
                    signature_type == tSystem_SByte ||
                    signature_type == tSystem_Byte ||
                    signature_type == tSystem_Boolean ||
                    signature_type == tSystem_Int16 ||
                    signature_type == tSystem_UInt16 ||
                    signature_type == tSystem_Char
                ) {
                    // truncate, going to be done implicitly by mir
                    arg_type = signature_type;
                } else if (signature_type == tSystem_IntPtr) {
                    // sign extend
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_EXT32,
                                                 MIR_new_reg_op(mir_ctx, arg_reg),
                                                 MIR_new_reg_op(mir_ctx, arg_reg)));
                    arg_type = signature_type;
                } else if (signature_type == tSystem_UIntPtr) {
                    // zero extend
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_UEXT32,
                                                 MIR_new_reg_op(mir_ctx, arg_reg),
                                                 MIR_new_reg_op(mir_ctx, arg_reg)));
                    arg_type = signature_type;
                }
            } break;

            case STACK_TYPE_INTPTR: {
                if (type_is_integer(signature_type)) {
                    // truncate or nop, we don't really care
                    arg_type = signature_type;
                }
            } break;

            case STACK_TYPE_FLOAT: {
                // handle implicit float casting
                if (arg_type == tSystem_Single) {
                    if (signature_type == tSystem_Double) {
                        // float->double conversion
                        MIR_reg_t real_arg_reg = jit_new_temp_reg(ctx, tSystem_Double);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_F2D,
                                                     MIR_new_reg_op(mir_ctx, real_arg_reg),
                                                     MIR_new_reg_op(mir_ctx, arg_reg)));
                        arg_reg = real_arg_reg;
                        arg_type = signature_type;
                    }
                } else if (arg_type == tSystem_Double) {
                    if (signature_type == tSystem_Single) {
                        // double->float conversion
                        MIR_reg_t real_arg_reg = jit_new_temp_reg(ctx, tSystem_Single);
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_D2F,
                                                     MIR_new_reg_op(mir_ctx, real_arg_reg),
                                                     MIR_new_reg_op(mir_ctx, arg_reg)));
                        arg_reg = real_arg_reg;
                        arg_type = signature_type;
                    }
                }
            } break;

            case STACK_TYPE_REF: {
                // if this is a readonly reference, then we are only allowed
                // to pass it to parameters marked as in
                if (arg_entry.readonly_ref) {
                    CHECK(parameter_is_in(parameter_info));
                }

                // if this is a reference that might be returned from the method we are calling
                // to, and the argument is a local reference on its own, then we need to mark
                // that the  reference type can't be trusted
                if (!arg_entry.non_local_ref && type_is_verifier_assignable_to(arg_type, ctx->method->ReturnType)) {
                    ret_is_non_local_ref = false;
                }
            } break;

            // nothing to do
            case STACK_TYPE_INT64: break;

            // in mir when calling
            case STACK_TYPE_VALUE_TYPE: {
                mem_op = true;
            } break;

            default:
                CHECK_FAIL();
        }

        // we are creating a new delegate and this is the target parameter
        if (i - other_args == 0 && ftnMethod != NULL) {
            if (!method_is_static(ftnMethod)) {
                // this is an instance method, emit a null check on the target
                // to make sure that it is not null
                CHECK_AND_RETHROW(jit_null_check(ctx, arg_reg, arg_type));
            } else {
                // this is a static method, we need a null target, if already null
                // ignore it, otherwise just zero the reg
                if (arg_type != NULL) {
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, arg_reg),
                                                 MIR_new_int_op(mir_ctx, 0)));
                }
            }

            // make sure to reset it now
            ftnMethod = NULL;
        }

        // set the op, for anything passed by value we need to use MIR_T_BLK with the disp
        // being the size instead of the displacement
        if (mem_op) {
            arg_ops[i] = MIR_new_mem_op(mir_ctx, MIR_T_BLK, arg_type->StackSize, arg_reg, 0, 1);
        } else {
            arg_ops[i] = MIR_new_reg_op(mir_ctx, arg_reg);
        }

        // verify a normal argument
        CHECK(type_is_verifier_assignable_to(arg_type, signature_type));
    }

    // handle the `this` argument
    MIR_reg_t number_reg = 0;
    MIR_reg_t this_reg = 0;
    System_Type this_type;
    if (!method_is_static(operand_method)) {
        if (opcode == CEE_NEWOBJ) {
            // this is the this_type
            this_type = operand_method->DeclaringType;

            // make sure this is a type we can actually create
            CHECK(!type_is_abstract(this_type));
            CHECK(!type_is_interface(this_type));

            CHECK_AND_RETHROW(jit_stack_push(ctx, operand_method->DeclaringType, &this_reg));

            if (this_type->IsValueType) {
                if (type_get_stack_type(this_type) != STACK_TYPE_VALUE_TYPE) {
                    // this is an integer/float type, so allocate it on the stack
                    // so we can pass it as a reference and then just copy it into
                    // the eval stack as a normal variable

                    // save the position on the eval stack
                    number_reg = this_reg;

                    // set a temp new location
                    this_reg = jit_new_temp_reg(ctx, tSystem_IntPtr);
                    MIR_prepend_insn(mir_ctx, mir_func,
                                     MIR_new_insn(mir_ctx, MIR_ALLOCA,
                                                  MIR_new_reg_op(mir_ctx, this_reg),
                                                  MIR_new_int_op(mir_ctx, operand_method->DeclaringType->StackSize)));
                }

                // For a value type we just need to zero it out before calling the ctor
                jit_emit_zerofill(ctx, this_reg, this_type->StackSize);
            } else {
                MIR_op_t size_op;
                if (this_type == tSystem_String) {
                    // special handling for string
                    MIR_reg_t size_reg = jit_new_temp_reg(ctx, tSystem_Int64);

                    // we need to figure what the length is from the ctor, the ctor will
                    // handle actually copying the data nicely
                    if (operand_method->Parameters->Length == 3) {
                        // String(char[] args, startIndex, length), the size is
                        // the last argument times two....
                        MIR_append_insn(mir_ctx, mir_func,
                                        MIR_new_insn(mir_ctx, MIR_MOV,
                                                     MIR_new_reg_op(mir_ctx, size_reg),
                                                     arg_ops[other_args + 2]));

                    } else if (operand_method->Parameters->Length == 1) {
                        ASSERT(operand_method->Parameters->Length == 1);
                        System_Type arg0_type = operand_method->Parameters->Data[0]->ParameterType;

                        if (arg0_type->IsArray) {
                            // String(char[] chars), the size is the array length
                            ASSERT(arg_ops[other_args].mode == MIR_OP_REG);
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, MIR_MOV,
                                                         MIR_new_reg_op(mir_ctx, size_reg),
                                                         MIR_new_mem_op(mir_ctx, MIR_T_I32,
                                                                        offsetof(struct System_Array, Length),
                                                    arg_ops[other_args].u.reg, 0, 1)));
                        } else if (
                            arg0_type->GenericTypeDefinition == tSystem_ReadOnlySpan &&
                            arg0_type->GenericArguments->Length == 1 &&
                            arg0_type->GenericArguments->Data[0] == tSystem_Char
                        ) {
                            // String(ReadOnlySpan<char> value), the size is the span length
                            ASSERT(arg_ops[other_args].mode == MIR_OP_MEM);
                            ASSERT(arg_ops[other_args].u.mem.type == MIR_T_BLK);
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, MIR_MOV,
                                                         MIR_new_reg_op(mir_ctx, size_reg),
                                                         MIR_new_mem_op(mir_ctx, MIR_T_I32,
                                                                        offsetof(struct System_Span, Length),
                                                    arg_ops[other_args].u.mem.base, 0, 1)));

                        } else if (arg0_type == tSystem_Int32) {
                            // String(int length), the size is length
                            MIR_append_insn(mir_ctx, mir_func,
                                            MIR_new_insn(mir_ctx, MIR_MOV,
                                                         MIR_new_reg_op(mir_ctx, size_reg),
                                                         arg_ops[other_args]));

                        } else {
                            // invalid string ctor
                            CHECK_FAIL();
                        }
                    } else {
                        CHECK_FAIL();
                    }

                    // multiply by char size
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MUL,
                                                 MIR_new_reg_op(mir_ctx, size_reg),
                                                 MIR_new_reg_op(mir_ctx, size_reg),
                                                 MIR_new_int_op(mir_ctx, sizeof(System_Char))));

                    // add the header size
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_ADD,
                                                 MIR_new_reg_op(mir_ctx, size_reg),
                                                 MIR_new_reg_op(mir_ctx, size_reg),
                                                 MIR_new_int_op(mir_ctx, tSystem_String->ManagedSize)));

                    // create the op
                    size_op = MIR_new_reg_op(mir_ctx, size_reg);
                } else {
                    // Normal object
                    size_op = MIR_new_int_op(mir_ctx, operand_method->DeclaringType->ManagedSize);
                }

                // allocate the new object
                CHECK_AND_RETHROW(jit_new(ctx, this_reg,
                                          operand_method->DeclaringType, size_op));
            }
        } else {
            // this is a call, get it from the stack
            CHECK_AND_RETHROW(stack_pop(ctx, &this_type, &this_reg, NULL));

            // Value types have their this as a by-ref
            System_Type signature_this_type = operand_method->DeclaringType;
            if (signature_this_type->IsValueType) {
                signature_this_type = get_by_ref_type(signature_this_type);
            }

            // NOTE: for interfaces the `this` is unpacked properly at a later stage since
            //       we also need the vtable from the original `this`

            if (constrainedType != NULL) {
                CHECK(this_type->IsByRef);
                CHECK(type_is_verifier_assignable_to(this_type->BaseType, constrainedType));

                // If this_type is a reference type (as opposed to a value type)
                if (type_is_object_ref(constrainedType)) {
                    // ptr is dereferenced and passed as the ‘this’ pointer to the callvirt of method
                    this_type = constrainedType;
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                    MIR_new_reg_op(mir_ctx, this_reg),
                                                    MIR_new_mem_op(mir_ctx, MIR_T_P, 0, this_reg, 0, 1)));
                } else {
                    // this is a value type, the signature this is a reference
                    signature_this_type = get_by_ref_type(constrainedType);
                }

                // get the static dispatch, the call later will
                // actually handle making sure this is correct

                // figure the real method for this dispatch
                int vtable_offset = operand_method->VTableOffset;
                if (type_is_interface(operand_method->DeclaringType)) {
                    CHECK(operand_method->VTableOffset < constrainedType->VirtualMethods->Length);

                    // comes from interface, need to give a static offset. If the types are the same
                    // the offset is actually zero, so no nede to check for impl
                    if (constrainedType != operand_method->DeclaringType) {
                        TinyDotNet_Reflection_InterfaceImpl impl = type_get_interface_impl(constrainedType, operand_method->DeclaringType);
                        vtable_offset += impl->VTableOffset;
                    }
                } else if (type_is_interface(constrainedType)) {
                    // I think this is the only case, in this case we are trying to call
                    // one of the base virtual functions (GetHashCode/Equals/ToString) on
                    // an interface, which does not have these functions inlined
                    CHECK(operand_method->DeclaringType == tSystem_Object);

                    // we need to essentially cast to an object
                    this_type = tSystem_Object;
                    constrainedType = tSystem_Object;

                    // upack the this so it will be a simple object
                    MIR_append_insn(mir_ctx, mir_func,
                                    MIR_new_insn(mir_ctx, MIR_MOV,
                                                 MIR_new_reg_op(mir_ctx, this_reg),
                                                 MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), this_reg, 0, 1)));
                }
                operand_method = constrainedType->VirtualMethods->Data[vtable_offset];

                // clear the type
                constrainedType = NULL;
            }

            // verify a normal argument
            if (signature_this_type == tSystem_Object && this_type->BaseType == tSystem_ValueType) {
                // this is an edge case, we are going to politely ignore it
                // in short, for whatever reason the compiler generates a ctor in System.ValueType
                // that calls the ctor of System.Object ????
            } else {
                CHECK(type_is_verifier_assignable_to(this_type, signature_this_type));
            }

            // make sure that the object is not null, only if not a byref
            if (this_type == NULL || !this_type->IsByRef) {
                CHECK_AND_RETHROW(jit_null_check(ctx, this_reg, this_type));
            }
        }

        arg_ops[i] = MIR_new_reg_op(mir_ctx, this_reg);
    }

    // get the MIR signature and address
    // TODO: in theory in here we only need the signature, and not a full jit
    CHECK_AND_RETHROW(jit_prepare_method(ctx->ctx, operand_method));
    arg_ops[0] = MIR_new_ref_op(mir_ctx, operand_method->MirProto);

    if (
        opcode == CEE_CALLVIRT &&
        method_is_virtual(operand_method)
    ) {
        // we are using callvirt and this is a virtual method, so we have to
        // use a dynamic dispatch

        MIR_reg_t temp_reg = jit_new_temp_reg(ctx, tSystem_Type);

        // get the vtable pointer from the object, it is at the first
        // item for both an interface and an object
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_MOV,
                                     MIR_new_reg_op(mir_ctx, temp_reg),
                                     MIR_new_mem_op(mir_ctx, MIR_T_P, 0, this_reg, 0, 1)));

        // get the base offset of the method
        size_t vtable_index = operand_method->VTableOffset;
        if (type_is_interface(operand_method->DeclaringType) && operand_method->DeclaringType != this_type) {
            // the method we want to call is an interface method, and its not the same type
            // as the this_type meaning we need to find the real implementation offset
            vtable_index += type_get_interface_impl(this_type, operand_method->DeclaringType)->VTableOffset;
        }

        // read the actual instance pointer of the interface, so we can use it
        // when calling the function
        if (type_is_interface(this_type)) {
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_reg_op(mir_ctx, this_reg),
                                         MIR_new_mem_op(mir_ctx, MIR_T_P, sizeof(void*), this_reg, 0, 1)));
        }

        // get the real method we are going to call now
        System_Reflection_MethodInfo real_method;
        bool sealed;
        if (this_type->IsByRef) {
            real_method = this_type->BaseType->VirtualMethods->Data[vtable_index];
            sealed = type_is_sealed(this_type->BaseType);
        } else {
            real_method = this_type->VirtualMethods->Data[vtable_index];
            sealed = type_is_sealed(this_type);
        }

        // prepare the method
        MIR_item_t func;
        CHECK_AND_RETHROW(jit_prepare_method(ctx->ctx, real_method));
        if (this_type->IsByRef) {
            func = real_method->MirFunc;
        } else {
            func = real_method->MirUnboxerFunc ?: real_method->MirFunc;
        }

        if (sealed || method_is_final(real_method)) {
            // this is either a sealed class or a final method, meaning that no
            // one can inherit from them, so we can de-virtualize the call safely
            // without worrying about anything else
            arg_ops[1] = MIR_new_ref_op(mir_ctx, func);

        } else {
            // get the address of the function from the vtable
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, MIR_MOV,
                                         MIR_new_reg_op(mir_ctx, temp_reg),
                                         MIR_new_mem_op(mir_ctx, MIR_T_P,
                                                        vtable_index * sizeof(void*),
                                                        temp_reg, 0, 1)));

            // indirect call
            arg_ops[1] = MIR_new_reg_op(mir_ctx, temp_reg);
        }
    } else {
        // static dispatch
        CHECK_AND_RETHROW(jit_prepare_method(ctx->ctx, operand_method));
        arg_ops[1] = MIR_new_ref_op(mir_ctx, operand_method->MirFunc);
    }

    // get it to a temp register
    MIR_reg_t exception_reg = jit_new_temp_reg(ctx, tSystem_Exception);
    arg_ops[2] = MIR_new_reg_op(mir_ctx, exception_reg);

    #define IS_INTRINSIC(name) \
        ((name) != NULL && operand_method->GenericMethodDefinition == (name))

    //
    // Unsafe.SizeOf, inline it as a number
    //
    if (IS_INTRINSIC(m_Unsafe_SizeOf)) {
        System_Type T = operand_method->GenericArguments->Data[0];

        MIR_reg_t ret_reg;
        CHECK_AND_RETHROW(jit_stack_push(ctx, type_get_intermediate_type(tSystem_Int32), &ret_reg));
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_MOV,
                                     MIR_new_reg_op(mir_ctx, ret_reg),
                                     MIR_new_int_op(mir_ctx, T->StackSize)));

    //
    // RuntimeHelpers.IsReferenceOrContainsReferences, inline it as a 1 or 0
    //
    } else if (IS_INTRINSIC(m_RuntimeHelpers_IsReferenceOrContainsReferences)) {
        System_Type T = operand_method->GenericArguments->Data[0];

        bool is_reference = type_is_object_ref(T);
        bool has_references = arrlen(T->ManagedPointersOffsets) != 0;

        MIR_reg_t ret_reg;
        CHECK_AND_RETHROW(jit_stack_push(ctx, type_get_intermediate_type(tSystem_Boolean), &ret_reg));
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_MOV,
                                     MIR_new_reg_op(mir_ctx, ret_reg),
                                     MIR_new_int_op(mir_ctx, is_reference || has_references)));

    //
    // default handling, normal function call
    //
    } else {
        if (operand_method->ReturnType != NULL) {
            MIR_reg_t ret_reg;
            CHECK_AND_RETHROW(
                    jit_stack_push(ctx, type_get_intermediate_type(operand_method->ReturnType), &ret_reg));

            if (type_get_stack_type(operand_method->ReturnType) == STACK_TYPE_REF) {
                // we did not pass any local references to this, so we know for sure it can't be a local address
                // being returned from the method
                STACK_TOP.non_local_ref = ret_is_non_local_ref;
            }

            // this should just work, because if the value is a struct it is going to be allocated properly
            // in the stack push, and it is going to be passed by a pointer that we give, and everything will
            // just work out because of how we have the order of everything :)
            arg_ops[3] = MIR_new_reg_op(mir_ctx, ret_reg);
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn_arr(mir_ctx, aggressive_inlining ? MIR_INLINE : MIR_CALL,
                                             other_args + arg_count,
                                             arg_ops));
        } else {
            // Does not have a return argument, no need to handle
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn_arr(mir_ctx, aggressive_inlining ? MIR_INLINE : MIR_CALL,
                                             other_args + arg_count,
                                             arg_ops));
        }

        // handle any exception which might have been thrown
        MIR_insn_t label = MIR_new_label(mir_ctx);

        // if we have a zero value skip the return
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_BF,
                                     MIR_new_label_op(mir_ctx, label),
                                     MIR_new_reg_op(mir_ctx, exception_reg)));

        // set the exception register to have the new exception
        MIR_append_insn(mir_ctx, mir_func,
                        MIR_new_insn(mir_ctx, MIR_MOV,
                                     MIR_new_reg_op(mir_ctx, ctx->exception_reg),
                                     MIR_new_reg_op(mir_ctx, exception_reg)));

        // throw the error, it has an unknown type
        CHECK_AND_RETHROW(jit_throw(ctx, NULL, true));

        // insert the skip label
        MIR_append_insn(mir_ctx, mir_func, label);

        // check if we need to copy the left out value from the stack
        // to the eval stack
        if (
            opcode == CEE_NEWOBJ &&
            operand_method->DeclaringType->IsValueType &&
            type_get_stack_type(operand_method->DeclaringType) != STACK_TYPE_VALUE_TYPE
        ) {
            MIR_append_insn(mir_ctx, mir_func,
                            MIR_new_insn(mir_ctx, jit_mov_insn_code(operand_method->DeclaringType),
                                         MIR_new_reg_op(mir_ctx, number_reg),
                                         MIR_new_mem_op(mir_ctx,
                                                        jit_get_mir_type(operand_method->DeclaringType), 0, this_reg, 0, 1)));
        }
    }

cleanup:
    SAFE_FREE(arg_ops);

    return err;
}
