#include "filler.h"
#include "dotnet/gc/gc.h"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Actual type filling
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static System_Type* m_type_fill = NULL;

static mutex_t m_type_fill_lock = INIT_MUTEX();

static void queue_type(System_Type type) {
    if (type->TypeQueued || type->TypeFilled)
        return;

    // make sure this is a fully setup type...
    ASSERT(type->IsSetupFinished);

    type->TypeQueued = true;
    arrpush(m_type_fill, type);
}

static System_Type pop_type() {
    System_Type type = arrlen(m_type_fill) == 0 ? NULL : arrpop(m_type_fill);
    return type;
}

static err_t fill_type_managed_size(System_Type type);

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static err_t fill_type_stack_size(System_Type type) {
    err_t err = NO_ERROR;

    if (type->StackSizeFilled) {
        return NO_ERROR;
    }

    // make sure no recurssion
    CHECK(!type->StackSizeBeingFilled);
    type->StackSizeBeingFilled = true;

    // first we need the size of the base type
    if (type->BaseType != NULL) {
        CHECK_AND_RETHROW(fill_type_stack_size(type->BaseType));

        // copy the value type and stack type
        type->IsValueType = type->BaseType->IsValueType;
        type->StackType = type->BaseType->StackType;

        if (type->IsValueType) {
            ASSERT(!type_is_object_ref(type));
        } else {
            ASSERT(type_is_object_ref(type));
        }

        if (type->IsValueType) {
            // Can not inherit from value types, except for enum which is allowed
            CHECK(type->BaseType == tSystem_ValueType || type->BaseType == tSystem_Enum);
        }
    }

    // check if we can finish up with the size
    if (type_is_object_ref(type)) {
        if (type_is_interface(type)) {
            type->StackSize = sizeof(void*) * 2;
            type->StackAlignment = alignof(void*);
        } else {
            type->StackSize = sizeof(void*);
            type->StackAlignment = alignof(void*);
        }
    } else {
        // this is a value type, we get its size from the
        // managed size
        CHECK_AND_RETHROW(fill_type_managed_size(type));

        type->StackSize = type->ManagedSize;
        type->StackAlignment = type->ManagedAlignment;
    }

    // we are done
    type->StackSizeFilled = true;

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static err_t fill_type_managed_size(System_Type type) {
    err_t err = NO_ERROR;

    if (type->ManagedSizeFilled) {
        goto cleanup;
    }

    // if we are not a value type we need to make sure we
    // init the stack size, this can actually turn us into a
    // value type because it will initialize the chain, this
    // may even already do our job in some cases
    if (!type->IsValueType) {
        CHECK_AND_RETHROW(fill_type_stack_size(type));

        // if this caused the size to be filled then nothing to do
        if (type->ManagedSizeFilled) {
            goto cleanup;
        }
    }

    int expected_size = type->ManagedSize;
    int expected_alignment = type->ManagedAlignment;

    CHECK(!type->ManagedSizeBeingFilled);
    type->ManagedSizeBeingFilled = true;

    int current_size = 0;
    int current_alignment = 1;
    int max_alignment = type->PackingSize ?: 128;

    // fill the size of the parent and verify it
    if (type->BaseType != NULL) {
        CHECK_AND_RETHROW(fill_type_managed_size(type->BaseType));

        current_size = type->BaseType->ManagedSize;
        current_alignment = type->BaseType->ManagedAlignment;
        CHECK(current_alignment <= max_alignment);

        // copy the managed pointers offsets
        for (int i = 0; i < arrlen(type->BaseType->ManagedPointersOffsets); i++) {
            arrpush(type->ManagedPointersOffsets, type->BaseType->ManagedPointersOffsets[i]);
        }
    }

    // handle class size parameter
    if (type->ClassSize != 0) {
        CHECK(type->ClassSize >= current_size);
    }

    int prev_size = current_size;

    for (int i = 0; i < type->Fields->Length; i++) {
        System_Reflection_FieldInfo fieldInfo = type->Fields->Data[i];

        // queue for full init
        queue_type(fieldInfo->FieldType);

        // ignore static fields for now
        if (field_is_static(fieldInfo)) {
            continue;
        }

        // an interface can not have non-static fields
        CHECK(!type_is_interface(type));

        // non-static field, get the stack size
        CHECK_AND_RETHROW(fill_type_stack_size(fieldInfo->FieldType));

        // lay it out where we want to
        switch (type_layout(type)) {
            // For now do the same as sequential
            case TYPE_AUTO_LAYOUT:
                // fallthrough

            // for sequential layout just align the size and add the current field's size
            case TYPE_SEQUENTIAL_LAYOUT: {
                // align the size of the type for the current field
                int alignment = MIN(max_alignment, fieldInfo->FieldType->StackAlignment);
                current_size = ALIGN_UP(current_size, alignment);
                CHECK(current_size >= prev_size);

                fieldInfo->MemoryOffset = current_size;

                // add the field size
                current_size += fieldInfo->FieldType->StackSize;
                CHECK(current_size >= prev_size);

                // set new type alignment
                current_alignment = MAX(current_alignment, fieldInfo->FieldType->StackAlignment);
                current_alignment = MIN(max_alignment, current_alignment);
            } break;

            // for explicit layout the size is preconfigured, just
            // make sure it does not go out of the class size
            case TYPE_EXPLICIT_LAYOUT: {
                int ends_at = fieldInfo->MemoryOffset + fieldInfo->FieldType->StackSize;
                CHECK(ends_at >= 0);

                // don't allow non-value types for now
                CHECK(fieldInfo->FieldType->IsValueType);

                // make sure it ends before the class size
                CHECK(ends_at < type->ClassSize);
            } break;

            default:
                CHECK_FAIL();
        }

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
                *offsets = fieldInfo->MemoryOffset + offset;
            }
        }
    }

    // finalize the managed size
    if (type->ClassSize == 0) {
        // align the class size properly
        current_size = ALIGN_UP(current_size, current_alignment);
    } else {
        // make sure the size is still in bounds
        CHECK(current_size <= type->ClassSize);

        // set the size as the class size
        current_size = type->ClassSize;
    }

    // if this is an interface it has a constant managed size
    if (type_is_interface(type)) {
        // all interfaces have a single managed pointer which
        // is the instance field, no need to create this per
        // type so create this once
        static int* interface_pointers = NULL;
        if (interface_pointers) {
                    arrsetlen(interface_pointers, 1);
            interface_pointers[0] = sizeof(void*);
        }

        // same as the stack size
        current_size = type->StackSize;
        current_alignment = type->StackAlignment;
    }

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

    // max size of value types is 1mb
    if (type->IsValueType) {
        CHECK(current_size <= SIZE_1MB);
    }

    if (expected_size != 0) {
        CHECK(expected_size == current_size, "unexpected size for type %U (expected=%d, current=%d)", type->Name, expected_size, current_size);
        CHECK(expected_alignment == current_alignment, "unexpected alignment for type %U (expected=%d, current=%d)", type->Name, expected_alignment, current_alignment);
    }

    // set the proper alignment requirements
    type->ManagedSize = current_size;
    type->ManagedAlignment = current_alignment;

    if (type->ManagedSize != 0) {
        // runtime constraint, we can only make sure the alignment is right if the objects
        // have their size bigger than the alignment wanted
        CHECK(type->ManagedAlignment <= type->ManagedSize);
    }

    // we are done
    type->ManagedSizeFilled = true;

cleanup:
    return err;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

static err_t fill_methods(System_Type type) {
    err_t err = NO_ERROR;

    if (type->MethodsFilled) {
        return NO_ERROR;
    }

    CHECK(!type->MethodsBeingFilled);
    type->MethodsBeingFilled = true;

    // the amount of virtual methods before us
    int virtual_count = 0;

    // start with the parent, since we need to work with his
    // virtual methods
    if (type->BaseType != NULL) {
        CHECK_AND_RETHROW(fill_methods(type->BaseType));

        // now check if it has virtual methods
        if (type->BaseType->VirtualMethods != NULL) {
            virtual_count = type->BaseType->VirtualMethods->Length;
        }
    }

    //------------------------------------------------------------------------------------------------------------------
    // first we need to take care of the methods and mark everything that needs to be marked
    // and any virtual method that overrides an existing one can be handled right here
    //------------------------------------------------------------------------------------------------------------------

    for (int i = 0; i < type->Methods->Length; i++) {
        System_Reflection_MethodInfo methodInfo = type->Methods->Data[i];

        if (method_is_virtual(methodInfo)) {
            // we don't support generic virtual methods for now, mostly because it is
            // a fucking mess in terms of vtables
            CHECK(methodInfo->GenericArguments == NULL);

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
            // TODO: make sure no other method like this exists

            // queue return type
            if (methodInfo->ReturnType != NULL)
                queue_type(methodInfo->ReturnType);

            // queue the parameters, skip generic parameters
            for (int j = 0; j < methodInfo->Parameters->Length; j++) {
                System_Reflection_ParameterInfo parameterInfo = methodInfo->Parameters->Data[j];
                if (type_is_generic_parameter(parameterInfo->ParameterType)) {
                    continue;
                }

                queue_type(parameterInfo->ParameterType);
            }
        }

        if (method_is_rt_special_name(methodInfo)) {
            CHECK(method_is_special_name(methodInfo));

            if (string_equals_cstr(methodInfo->Name, ".cctor")) {
                CHECK(method_is_static(methodInfo));
                CHECK(methodInfo->Parameters->Length == 0);
                CHECK(methodInfo->ReturnType == NULL);
                CHECK(type->TypeInitializer == NULL);
                GC_UPDATE(type, TypeInitializer, methodInfo);
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

        // for interfaces all methods need to be abstract
        if (type_is_interface(type)) {
            CHECK(method_is_abstract(methodInfo));
        }
    }

    //------------------------------------------------------------------------------------------------------------------
    // now we need to handle all the interfaces, all of them need to have their
    // virtual methods resolved properly
    //------------------------------------------------------------------------------------------------------------------

    if (type->InterfaceImpls != NULL) {
        // TODO: validate that we have all the interfaces the parent has

        // go over and expand all the interfaces needed
        for (int i = 0; i < type->InterfaceImpls->Length; i++) {
            TinyDotNet_Reflection_InterfaceImpl impl = type->InterfaceImpls->Data[i];
            System_Type interface = impl->InterfaceType;

            // queue the interface
            queue_type(interface);

            // we need the virtual methods of that type
            CHECK_AND_RETHROW(fill_methods(interface));
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

    //------------------------------------------------------------------------------------------------------------------
    // now that we have set all the methods that are part of interfaces, check how many
    // of these methods are not in interfaces, and set them up to be after all of the interfaces
    //------------------------------------------------------------------------------------------------------------------

    for (int i = 0; i < type->Methods->Length; i++) {
        System_Reflection_MethodInfo method = type->Methods->Data[i];
        if (!method_is_virtual(method)) continue;
        if (method->VTableOffset != -1) continue;

        // set its offset
        method->VTableOffset = virtual_count++;
    }

    //------------------------------------------------------------------------------------------------------------------
    // now we know the exact amount of virtual methods that we have for this type
    //------------------------------------------------------------------------------------------------------------------

    GC_UPDATE(type, VirtualMethods, GC_NEW_ARRAY(tSystem_Reflection_MethodInfo, virtual_count));
    if (virtual_count != 0) {
        type->VTable = malloc(sizeof(void*) * virtual_count);
        CHECK_ERROR(type->VTable != NULL, ERROR_OUT_OF_MEMORY);
    } else {
        type->VTable = NULL;
    }

    //------------------------------------------------------------------------------------------------------------------
    // copy the parent's vtable
    //------------------------------------------------------------------------------------------------------------------

    if (type->BaseType != NULL) {
        for (int i = 0; i < type->BaseType->VirtualMethods->Length; i++) {
            GC_UPDATE_ARRAY(type->VirtualMethods, i, type->BaseType->VirtualMethods->Data[i]);
        }
    }

    //------------------------------------------------------------------------------------------------------------------
    // now place all of our methods in the vtable
    //------------------------------------------------------------------------------------------------------------------

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

    //------------------------------------------------------------------------------------------------------------------
    // just in case, make sure all the methods are set properly
    //------------------------------------------------------------------------------------------------------------------

    for (int i = 0; i < type->VirtualMethods->Length; i++) {
        CHECK(type->VirtualMethods->Data[i] != NULL);
    }

    // we are done
    type->MethodsFilled = true;

cleanup:
    return err;
}

err_t filler_fill_type(System_Type type) {
    err_t err = NO_ERROR;

    // fast path
    if (type->TypeFilled) {
        return NO_ERROR;
    }

    mutex_lock(&m_type_fill_lock);

    queue_type(type);

    while (true) {
        type = pop_type();
        if (type == NULL)
            break;

        // skip if already done
        if (type->TypeFilled)
            continue;

        // skip types which have generic stuff in them, since nothing in them
        // actually needs to be filled
        if (type_is_generic_definition(type) || type_is_generic_parameter(type)) {
            type->TypeFilled = true;
            continue;
        }

        // validate the base type
        if (type->BaseType != NULL) {
            // queue the base type
            queue_type(type->BaseType);

            if (type_is_interface(type)) {
                // interfaces must inherit from System.Object
                CHECK(type->BaseType == tSystem_Object);
            }

            // validate that we don't inherit from a sealed type
            CHECK(!type_is_sealed(type->BaseType));
        }

        // set the namespace if this is a nested type
        if (type->DeclaringType != NULL) {
            System_Type rootType = type->DeclaringType;
            while (rootType->DeclaringType != NULL) {
                rootType = rootType->DeclaringType;
            }
            GC_UPDATE(type, Namespace, rootType->Namespace);
        }

        // do all initializations now
        CHECK_AND_RETHROW(fill_type_stack_size(type));
        CHECK_AND_RETHROW(fill_type_managed_size(type));
        CHECK_AND_RETHROW(fill_methods(type));

        // the type is now fully filled up
        type->TypeFilled = true;
    }

cleanup:
    mutex_unlock(&m_type_fill_lock);
    return err;
}

err_t filler_fill_stack_size(System_Type type) {
    err_t err = NO_ERROR;

    mutex_lock(&m_type_fill_lock);

    CHECK_AND_RETHROW(fill_type_stack_size(type));

    // clear the types that have been queued, and mark them as not queued, as
    // we don't actually want to initialize any of them yet other than getting
    // the basic stack type of the specific type
    while (true) {
        type = pop_type();
        if (type == NULL)
            break;

        type->TypeQueued = false;
    }

cleanup:
    mutex_unlock(&m_type_fill_lock);
    return err;
}
