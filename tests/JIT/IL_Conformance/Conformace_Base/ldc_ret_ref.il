// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.


.assembly extern legacy library mscorlib {}


//
//======================================

//---- CLASS ----------------
.namespace ldc_ret_ref
{
.class public		_ret {

//---- GLOBAL DATA ----------

    .data DATA = int32(0x00000000)

//---- METHODS --------------

.method public static		class ldc_ret_ref._ret ret_test_0(class ldc_ret_ref._ret) {
.maxstack	1
	ldarg	0
	ret

}
.method public static		class ldc_ret_ref._ret ret_test_1(class ldc_ret_ref._ret) {
.maxstack	1
	ldarg	0
	ret

}
.method public static		class ldc_ret_ref._ret ret_test_2(class ldc_ret_ref._ret) {
.maxstack	1
	ldarg	0
	ret

}
.method public static		class ldc_ret_ref._ret ret_test_3(class ldc_ret_ref._ret) {
.maxstack	1
	ldarg	0
	ret

}
.method public static		class ldc_ret_ref._ret ret_test_4(class ldc_ret_ref._ret) {
.maxstack	1
	ldarg	0
	ret

}
.method public static		class ldc_ret_ref._ret ret_test_5(class ldc_ret_ref._ret) {
.maxstack	1
	ldarg	0
	ret

}
//---- CONSTRUCTOR ----------
.method public		void .ctor() {
.maxstack	1
    ldarg.0
    call instance void [System.Runtime]System.Object::.ctor()
	ret
}

//---- MAIN -----------------
.method public static	int32 main() {
.entrypoint
.maxstack		5
//====== begin testing ======

	newobj	instance 	void ldc_ret_ref._ret::.ctor()
	dup
	call		class ldc_ret_ref._ret ldc_ret_ref._ret::ret_test_0(class ldc_ret_ref._ret)
	ceq
	brfalse		FAIL

	newobj	instance 	void ldc_ret_ref._ret::.ctor()
	dup
	call		class ldc_ret_ref._ret ldc_ret_ref._ret::ret_test_1(class ldc_ret_ref._ret)
	ceq
	brfalse		FAIL

	newobj	instance 	void ldc_ret_ref._ret::.ctor()
	dup
	call		class ldc_ret_ref._ret ldc_ret_ref._ret::ret_test_2(class ldc_ret_ref._ret)
	ceq
	brfalse		FAIL

	newobj	instance 	void ldc_ret_ref._ret::.ctor()
	dup
	call		class ldc_ret_ref._ret ldc_ret_ref._ret::ret_test_3(class ldc_ret_ref._ret)
	ceq
	brfalse		FAIL

	newobj	instance 	void ldc_ret_ref._ret::.ctor()
	dup
	call		class ldc_ret_ref._ret ldc_ret_ref._ret::ret_test_4(class ldc_ret_ref._ret)
	ceq
	brfalse		FAIL

	newobj	instance 	void ldc_ret_ref._ret::.ctor()
	dup
	call		class ldc_ret_ref._ret ldc_ret_ref._ret::ret_test_5(class ldc_ret_ref._ret)
	ceq
	brfalse		FAIL


//====== end testing ========

//---- branch here on pass --
PASS:
	ldc.i4 100
	br		END

//---- branch here on fail --
FAIL:
	ldc.i4 101

//---- return the result ----
END:
	ret
//---- END OF METHOD --------
}
//---- EOF ------------------
}



.assembly ldc_ret_ref{}

}
