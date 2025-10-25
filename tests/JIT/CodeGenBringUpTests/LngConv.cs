// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//


using System;
using System.Runtime.CompilerServices;
public class BringUpTest_LngConv
{
    const int Pass = 100;
    const int Fail = -1;

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int LngConv(long x, out int y) { return y = (int) x; }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static UInt32 LngConv(long x, out UInt32 y) { return y = (UInt32) x; }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static UInt16 LngConv(long x, out UInt16 y) { return y = (UInt16)x; }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static byte LngConv(long x, out byte y) { return y = (byte)x; }

    //[MethodImplAttribute(MethodImplOptions.NoInlining)]
    //public static UInt16 LngConv(byte x) { return (UInt16)x; }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static Int16 LngConv(long x, out Int16 y) { return y = (Int16)x; }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static sbyte LngConv(long x, out sbyte y) { return y = (sbyte)x; }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static uint LngConv() 
    {
        uint num6 = (uint)((IntPtr)0x4234abcdL);
        if (num6 != 0x4234abcd)
        {
            return 1;
        }
        return num6;
    }


    public static int Main()
    {
        int a;
        UInt32 b;
        Int16 c;
        UInt16 d;
        sbyte e;
        byte f;
        long x = 3294168832L;

        LngConv();

        LngConv(x, out a);
        if (a != -1000798464) return Fail;
        
        LngConv(x, out b);
        if (b != 3294168832U) return Fail;

        LngConv(x, out c);
        if (c != 1792) return Fail;

        LngConv(x, out d);
        if (d != 1792) return Fail;
               
        LngConv(x, out e);
        if (e != 0) return Fail;

        LngConv(x, out f);
        if (f != 0) return Fail;

        return Pass;
    }
}
