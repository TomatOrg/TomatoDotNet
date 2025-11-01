// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

.assembly 'implicitConv' {}
.assembly extern mscorlib{auto}

.class public ILGEN_0xc60e022c {

.method static int16  Method_0xf7bb2d95() {
.zeroinit 
.maxstack	3

 BLOCK_1:
    ldc.r4 float32(0x4ec956e5)
    conv.i4
    ret
}
.method public static int32 Main() {
.entrypoint
.maxstack	15

    //If the implicit conversion below is not correct, the test will return a non 100 (FAIL) return code.
    call int16  ILGEN_0xc60e022c::Method_0xf7bb2d95()
    conv.i4
    ldc.i4 29212
    sub
    ret
}

}


