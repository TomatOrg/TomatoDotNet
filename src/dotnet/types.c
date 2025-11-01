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
RuntimeTypeInfo tActivator = NULL;

RuntimeTypeInfo tInAttribute = NULL;
RuntimeTypeInfo tIsVolatile = NULL;
RuntimeTypeInfo tIsReadOnlyAttribute = NULL;
RuntimeTypeInfo tIsByRefLikeAttribute = NULL;
RuntimeTypeInfo tScopedRefAttribute = NULL;
RuntimeTypeInfo tUnmanagedType = NULL;

RuntimeTypeInfo tException = NULL;

RuntimeTypeInfo tDelegate = NULL;
RuntimeTypeInfo tMulticastDelegate = NULL;
