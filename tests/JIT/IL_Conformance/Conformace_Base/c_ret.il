// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

.assembly extern legacy library mscorlib {}
.namespace c_ret
{
.class public          _ret {



.method public static          void ret_test() {
.maxstack       0
        ret
}

.method public         void _ret() {
.maxstack       0
        ret
}

.method public static  int32 main() {
.entrypoint
.maxstack               5
        ldc.i4  0xFEFE
        call    void c_ret._ret::ret_test()
        call    void c_ret._ret::ret_test()
        call    void c_ret._ret::ret_test()
        call    void c_ret._ret::ret_test()
        call    void c_ret._ret::ret_test()
        call    void c_ret._ret::ret_test()
        call    void c_ret._ret::ret_test()
        ldc.i4  0xFEFE
        ceq
        brfalse FAIL

PASS:
        ldc.i4 100
        br              END

FAIL:
        ldc.i4  0x00000000

END:
        ret
}
}


.assembly c_ret {}

}
