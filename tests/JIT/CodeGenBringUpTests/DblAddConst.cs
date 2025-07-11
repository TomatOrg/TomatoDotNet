// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//


using System;
using System.Runtime.CompilerServices;
public class BringUpTest_DblAddConst
{
    const int Pass = 100;
    const int Fail = -1;

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static double DblAddConst(double x) { return x+1; }

    public static int Main()
    {
        double y = DblAddConst(13d);
        if (System.Math.Abs(y-14d) <= Double.Epsilon) return Pass;
        else return Fail;
    }
}
