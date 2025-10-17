#include "types.h"

#include "tomatodotnet/util/stb_ds.h"

#include "util/except.h"
#include "verifier/casting.h"

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

RuntimeTypeInfo tSingle = NULL;
RuntimeTypeInfo tDouble = NULL;

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
RuntimeTypeInfo tRuntimeFieldHandle = NULL;
RuntimeTypeInfo tRuntimeMethodHandle = NULL;
RuntimeTypeInfo tRuntimeTypeHandle = NULL;

RuntimeTypeInfo tNullable = NULL;
RuntimeTypeInfo tSpan = NULL;

RuntimeTypeInfo tUnsafe = NULL;
RuntimeTypeInfo tMemoryMarshal = NULL;
RuntimeTypeInfo tRuntimeHelpers = NULL;

RuntimeTypeInfo tInAttribute = NULL;
RuntimeTypeInfo tIsVolatile = NULL;
RuntimeTypeInfo tIsReadOnlyAttribute = NULL;
RuntimeTypeInfo tIsByRefLikeAttribute = NULL;
RuntimeTypeInfo tUnmanagedType = NULL;

RuntimeTypeInfo tException = NULL;

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

    CHECK(!arg_type->IsByRef);
    CHECK(!arg_type->IsByRefStruct);

    // special constraints
    if (attributes.SpecialConstraint & TDN_GENERIC_PARAM_CONSTRAINT_REFERENCE_TYPE) {
        CHECK(tdn_type_is_gc_pointer(arg_type));
    }

    if (attributes.SpecialConstraint & TDN_GENERIC_PARAM_CONSTRAINT_NON_NULLABLE_VALUE_TYPE) {
        CHECK(arg_type->GenericTypeDefinition != tNullable);
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
                // only check for unmanaged if the type is non-generic
                if (!tdn_is_generic_parameter(arg_type)) {
                    CHECK(arg_type->IsUnmanaged);
                }
            } else {
                // TODO: implement the proper cast check
                CHECK_FAIL();
            }
        }
    }

cleanup:
    return err;
}
