#include "activator.h"
#include "dotnet/gc/gc.h"
#include "dotnet/jit/jit.h"

err_t activator_create_instance(System_Type type, System_Object* args, int argsCount, System_Object* created) {
    err_t err = NO_ERROR;

    // TODO: method access????

    // make sure that all the methods of this type
    // are properly jitted and ready to be called
    CHECK_AND_RETHROW(jit_type(type));

    // reset
    *created = NULL;

    System_Object new;
    if (type == tSystem_String) {
        // Strings are variable length, so need to
        // figure it properly
        CHECK_FAIL("TODO: string from activator");
    } else {
        new = UNSAFE_GC_NEW(type);
        if (new == NULL) {
            // handle quietly
            err = ERROR_OUT_OF_MEMORY;
            goto cleanup;
        }
    }

    // find the ctor
    int index = 0;
    System_Reflection_MethodInfo ctor = NULL;
    while ((ctor = type_iterate_methods_cstr(type, ".ctor", &index)) != NULL) {
        if (!method_is_rt_special_name(ctor)) continue;

        // special case this
        if (args == NULL) {
            if (ctor->Parameters->Length == 0) {
                break;
            }
            continue;
        } else if (ctor->Parameters->Length != argsCount) {
            continue;
        }

        // check that the parameters are the same
        bool found = true;
        for (int pi = 0; pi < ctor->Parameters->Length; pi++) {
            System_Type argType = OBJECT_TYPE(args[pi]);
            System_Type paramType = ctor->Parameters->Data[pi]->ParameterType;
            if (!type_is_verifier_assignable_to(argType, paramType)) {
                found = false;
                break;
            }
        }

        if (found) {
            break;
        }
    }

    if (ctor == NULL) {
        err = ERROR_MISSING_METHOD;
        goto cleanup;
    }

    // jit the ctor itself and wait for the result, this will make sure the previous request is also finished
    CHECK_AND_RETHROW(jit_method(ctor));

    // check the access
    // TODO: we could in theory do it by the caller of this instead
    if (method_get_access(ctor) != METHOD_PUBLIC) {
        err = ERROR_MEMBER_ACCESS;
        goto cleanup;
    }

    ASSERT(ctor->MirFunc != NULL);
    ASSERT(ctor->MirFunc->addr != NULL);

    // actually invoke the ctor
    System_Exception exception = NULL;
    if (argsCount == 0) {
        // small optimization for default ctors
        exception = ((System_Exception(*)(System_Object))(ctor->MirFunc->addr))(new);
    } else {
        // build the arguments nicely
        MIR_val_t vals[1 + argsCount];
        // first parameter to pass to constructors is the to-fill object
        // compare with CEE_NEWOBJ implementation
        vals[0].a = new;
        // fill the remaining parameters, i starts at 1 because 0 is already filled
        for (int i = 0; i < ctor->Parameters->Length; i++) {
            System_Type paramType = ctor->Parameters->Data[i]->ParameterType;
            System_Object value = args[i];
            System_Type valueType = OBJECT_TYPE(value);

            switch (type_get_stack_type(paramType)) {
                case STACK_TYPE_O: {
                    if (type_is_interface(paramType)) {
                        CHECK_FAIL("TODO: activator pass interface");
                    } else {
                        // because we box everything, interfaces are going to
                        // be turned into objects, so we can't have this in here
                        ASSERT(!type_is_interface(valueType));
                        vals[1 + i].a = value;
                    }
                } break;

                // integer types, copy the value from the object, remember
                // to zero it first so the copy will work nicely
                case STACK_TYPE_INT32:
                case STACK_TYPE_INT64:
                case STACK_TYPE_INTPTR: {
                    vals[1 + i].u = 0;
                    memcpy(&vals[1 + i].u, value + 1, valueType->ManagedSize);
                    // TODO: do we need to sign extend in here?
                } break;

                case STACK_TYPE_FLOAT: {
                    if (paramType == tSystem_Single) {
                        vals[1 + i].f = *(float*)(value + 1);
                    } else {
                        vals[1 + i].d = *(double*)(value + 1);
                    }
                } break;

                case STACK_TYPE_VALUE_TYPE: {
                    CHECK_FAIL("TODO: pass by value");
                } break;

                case STACK_TYPE_REF: {
                    CHECK_FAIL("TODO: is this even valid");
                } break;
            }
        }

        // now that we built the arguments, call into it
        MIR_val_t result = { 0 };
        MIR_context_t ctx = jit_get_mir_context();
        // it needs to be a MIR call because the number of argument varies
        // and consider the first argument is the this object
        MIR_interp_arr(ctx, ctor->MirFunc, &result, 1 + argsCount, vals);
        jit_release_mir_context();
        exception = result.a;
    }

    // check the exception
    if (exception != NULL) {
        // handle quietly
        err = ERROR_TARGET_INVOCATION;
        *created = (System_Object)exception;
    } else {
        *created = new;
    }

cleanup:
    return err;
}

System_Exception activator_create_exception(System_Type type) {
    System_Object object;
    err_t err = activator_create_instance(type, NULL, 0, &object);
    if (IS_ERROR(err)) {
        if (err == ERROR_OUT_OF_MEMORY) {
            // we ran out of memory
            // TODO: use the out of memory cache
            ASSERT(type != tSystem_OutOfMemoryException);
            return activator_create_exception(tSystem_OutOfMemoryException);

        } else {
            ASSERT(!"got error creating an exception");
        }
    }
    return (System_Exception)object;
}
