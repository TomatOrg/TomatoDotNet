// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

.assembly extern legacy library mscorlib {}



.namespace ldarg_ref
{
.class public _ldarg {

.method public		void .ctor() {
.maxstack	1
    ldarg.0
    call instance void [System.Runtime]System.Object::.ctor()
	ret
}

.method public	class ldarg_ref._ldarg args(class ldarg_ref._ldarg) {
.maxstack	5
	ldarg	0
	ret
}


.method public static	int32 main() {
.entrypoint
.locals(class ldarg_ref._ldarg)
.maxstack		2
	newobj		instance void ldarg_ref._ldarg::.ctor()
	stloc		0
	ldloc		0
	ldloc		0
	call		instance class ldarg_ref._ldarg ldarg_ref._ldarg::args(class ldarg_ref._ldarg)
	ldloc		0
	ceq
	brfalse		FAIL

	ldc.i4 100
	ret
FAIL:
	ldc.i4	0x0
	ret
}
}

.assembly ldarg_ref{}
}
