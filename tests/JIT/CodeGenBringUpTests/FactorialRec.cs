// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//


using System;
using System.Runtime.CompilerServices;

public class BringUpTest_FactorialRec
{
    const int Pass = 100;
    const int Fail = -1;

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int FactorialRec(int a)
    {
        int result;
        if (a == 0)
            result = 1;
        else
        {
            result = a * FactorialRec(a - 1);
        }
        return result;
    }

    public static int Main()
    {
        int s = FactorialRec(5);
        if (s != 120) return Fail;
        return Pass;
    }
}
