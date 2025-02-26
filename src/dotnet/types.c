#include "types.h"

#include <util/stb_ds.h>

#include "util/except.h"

RuntimeTypeInfo tObject = NULL;
RuntimeTypeInfo tValueType = NULL;
RuntimeTypeInfo tEnum = NULL;

RuntimeTypeInfo tVoid = NULL;
RuntimeTypeInfo tBoolean = NULL;
RuntimeTypeInfo tChar = NULL;

RuntimeTypeInfo tSByte = NULL;
RuntimeTypeInfo tInt16 = NULL;
RuntimeTypeInfo tInt32 = NULL;
RuntimeTypeInfo tInt64 = NULL;
RuntimeTypeInfo tIntPtr = NULL;

RuntimeTypeInfo tByte = NULL;
RuntimeTypeInfo tUInt16 = NULL;
RuntimeTypeInfo tUInt32 = NULL;
RuntimeTypeInfo tUInt64 = NULL;
RuntimeTypeInfo tUIntPtr = NULL;

RuntimeTypeInfo tArray = NULL;
RuntimeTypeInfo tString = NULL;

RuntimeTypeInfo tMethodBase = NULL;
RuntimeTypeInfo tRuntimeAssembly = NULL;
RuntimeTypeInfo tRuntimeModule = NULL;
RuntimeTypeInfo tRuntimeFieldInfo = NULL;
RuntimeTypeInfo tRuntimeMethodBody = NULL;
RuntimeTypeInfo tRuntimeMethodInfo = NULL;
RuntimeTypeInfo tRuntimeConstructorInfo = NULL;
RuntimeTypeInfo tRuntimeLocalVariableInfo = NULL;
RuntimeTypeInfo tRuntimeTypeInfo = NULL;
RuntimeTypeInfo tParameterInfo = NULL;
RuntimeTypeInfo tRuntimeExceptionHandlingClause = NULL;
RuntimeTypeInfo tRuntimeTypeHandle = NULL;

RuntimeTypeInfo tUnsafe = NULL;

RuntimeTypeInfo tInAttribute = NULL;
RuntimeTypeInfo tIsVolatile = NULL;
RuntimeTypeInfo tIsReadOnlyAttribute = NULL;
RuntimeTypeInfo tIsByRefLikeAttribute = NULL;
RuntimeTypeInfo tUnmanagedType = NULL;

RuntimeTypeInfo tDelegate = NULL;
RuntimeTypeInfo tMulticastDelegate = NULL;

tdn_err_t tdn_check_generic_argument_constraints(
    RuntimeTypeInfo arg_type,
    GenericParameterAttributes attributes,
    RuntimeTypeInfo_Array constraints,
    RuntimeTypeInfo_Array typeArgs,
    RuntimeTypeInfo_Array methodArgs
) {
    tdn_err_t err = TDN_NO_ERROR;

    // special constraints
    if (attributes.SpecialConstraint & TDN_GENERIC_PARAM_CONSTRAINT_REFERENCE_TYPE) {
        CHECK(!tdn_type_is_valuetype(arg_type));
    }

    if (attributes.SpecialConstraint & TDN_GENERIC_PARAM_CONSTRAINT_NON_NULLABLE_VALUE_TYPE) {
        // CHECK(arg_type->GenericTypeDefinition != tNullable);
        CHECK(tdn_type_is_valuetype(arg_type));
    }

    if (attributes.SpecialConstraint & TDN_GENERIC_PARAM_CONSTRAINT_DEFAULT_CONSTRUCTOR) {
        bool found = false;
        for (int j = 0; j < arg_type->DeclaredConstructors->Length; j++) {
            RuntimeConstructorInfo ctor = arg_type->DeclaredConstructors->Elements[j];
            if (ctor->Attributes.Static) continue;
            if (ctor->Parameters->Length != 0) continue;
            found = true;
            break;
        }
        CHECK(found);
    }

    if (constraints != NULL) {
        for (int i = 0; i < constraints->Length; i++) {
            RuntimeTypeInfo constraint = constraints->Elements[i];
            if (constraint == tUnmanagedType) {
                CHECK(arg_type->IsUnmanaged);
            } else {
                CHECK(tdn_is_instance(arg_type, constraint, typeArgs, methodArgs),
                    "%T is-instance %T", arg_type, constraint);
            }
        }
    }

cleanup:
    return err;
}

bool tdn_is_instance(
    RuntimeTypeInfo type,
    RuntimeTypeInfo base,
    RuntimeTypeInfo_Array typeArgs,
    RuntimeTypeInfo_Array methodArgs
) {
    if (base->Attributes.Interface) {
        // fast path if its exactly
        int idx = hmgeti(type->InterfaceImpls, base);
        if (idx >= 0) {
            return true;
        }

        // if not a generic type we can return right now, or we have
        // no generic arguments given, we can return right now, otherwise
        // we need to perform a more manual check in case the arguments
        // have changed
        if (
            !tdn_type_is_generic(base->GenericTypeDefinition) ||
            (typeArgs == NULL && methodArgs == NULL)
        ) {
            return false;
        }

        // slow path for generics
        for (int i = 0; i < hmlen(type->InterfaceImpls); i++) {
            RuntimeTypeInfo iface = type->InterfaceImpls[i].key;

            if (iface->GenericTypeDefinition == base->GenericTypeDefinition) {
                bool matched = true;
                for (int j = 0; j < iface->GenericArguments->Length; j++) {
                    RuntimeTypeInfo in_arg = iface->GenericArguments->Elements[j];
                    RuntimeTypeInfo want_arg = base->GenericArguments->Elements[j];

                    // resolve the real argument
                    if (want_arg->IsGenericTypeParameter) {
                        if (typeArgs != NULL && typeArgs->Length > want_arg->GenericParameterPosition) {
                            want_arg = typeArgs->Elements[want_arg->GenericParameterPosition];
                        } else {
                            want_arg = NULL;
                        }
                    } else if (want_arg->IsGenericMethodParameter) {
                        if (methodArgs != NULL && methodArgs->Length > want_arg->GenericParameterPosition) {
                            want_arg = methodArgs->Elements[want_arg->GenericParameterPosition];
                        } else {
                            want_arg = NULL;
                        }
                    }

                    // and now check they match
                    if (in_arg != want_arg) {
                        matched = false;
                        break;
                    }
                }

                if (matched) {
                    return true;
                }
            }
        }
    } else {
        // go over the inheritance
        do {
            if (type == base) {
                return true;
            }
            type = type->BaseType;
        } while (type != NULL);
    }
    return false;
}
