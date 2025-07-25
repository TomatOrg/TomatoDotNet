// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//


using System;
using System.Runtime.CompilerServices;

public class BringUpTest_AsgXor1
{
    const int Pass = 100;
    const int Fail = -1;

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int AsgXor1(int x) { x ^= 0xf; return x; }

    public static int Main()
    {
        if (AsgXor1(13) == 2) return Pass;
        else return Fail;
    }
}
