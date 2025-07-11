// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//


using System;
using System.Runtime.CompilerServices;

public class child
{
    public static int Main()
    {
        const int Pass = 100;
        const int Fail = -1;

        int x = 13;
        int result = OrRef(15, ref x);

        if (result == 15)
            return Pass;
        else
            return Fail;
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int OrRef(int x, ref int a)
    {
        x |= a;
        return x;
    }
}

