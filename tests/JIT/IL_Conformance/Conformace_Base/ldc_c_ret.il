// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.


.assembly extern legacy library mscorlib {}

//---- CLASS ----------------
.namespace ldc_c_ret
{
.class public          _ret {

//---- GLOBAL DATA ----------


//---- METHODS --------------

.method public static          void ret_test_0() {
.maxstack       0
        ret

}
.method public static          void ret_test_1() {
.maxstack       0
        ret

}
.method public static          void ret_test_2() {
.maxstack       0
        ret

}
.method public static          void ret_test_3() {
.maxstack       0
        ret

}
.method public static          void ret_test_4() {
.maxstack       0
        ret

}
.method public static          void ret_test_5() {
.maxstack       0
        ret

}
.method public static          void ret_test_6() {
.maxstack       0
        ret

}
//---- CONSTRUCTOR ----------
.method public         void _ret() {
.maxstack       0
        ret
}

//---- MAIN -----------------
.method public static  int32 main() {
.entrypoint
.maxstack               5
//====== begin testing ======
        ldc.i4  0xFEFE
        call    void ldc_c_ret._ret::ret_test_0()
        call    void ldc_c_ret._ret::ret_test_1()
        call    void ldc_c_ret._ret::ret_test_2()
        call    void ldc_c_ret._ret::ret_test_3()
        call    void ldc_c_ret._ret::ret_test_4()
        call    void ldc_c_ret._ret::ret_test_5()
        call    void ldc_c_ret._ret::ret_test_6()
        ldc.i4  0xFEFE
        ceq
        brfalse FAIL
//====== end testing ========

//---- branch here on pass --
PASS:
	ldc.i4 100
        br              END

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


.assembly ldc_c_ret {}


}
