// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//


using System;
using System.Runtime.CompilerServices;
public class BringUpTest_DblDiv
{
    const int Pass = 100;
    const int Fail = -1;

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static double DblDiv(double x, double y) { return x/y; }

    public static int Main()
    {
        double y = DblDiv(81d, 3d);
        if (System.Math.Abs(y-27d) <= Double.Epsilon) return Pass;
        else return Fail;
    }
}
