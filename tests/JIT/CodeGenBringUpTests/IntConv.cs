// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//


using System;
using System.Runtime.CompilerServices;

public class BringUpTest_IntConv
{
    const int Pass = 100;
    const int Fail = -1;

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static long IntConv(int x) { return (long) x; }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static long IntConv(UInt32 x) { return (long) x; }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int IntConv(UInt16 x) { return (int)x; }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int IntConv(byte x) { return (int)x; }

    //[MethodImplAttribute(MethodImplOptions.NoInlining)]
    //public static UInt16 IntConv(byte x) { return (UInt16)x; }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static byte IntConv(Int16 x) { return (byte)x; }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static UInt32 IntConv(long x) { return (UInt32)x; }

    public static int Main()
    {
        long x = IntConv((int)3);
        if (x != 3) return Fail;

        x = IntConv((UInt32)3294168832);
        if (x != 3294168832L) return Fail;

        int z = IntConv((UInt16) 123);
        if (z != 123) return Fail;

        z = IntConv((byte)3);
        if (z != 3) return Fail;

        byte w = IntConv((Int16)3);
        if (w != 3) return Fail;

        UInt32 y = IntConv(1234L);
        if (y != 1234U) return Fail;



        return Pass;
    }
}
