// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//


using System;
using System.Runtime.CompilerServices;
public class BringUpTest_DblSub
{
    const int Pass = 100;
    const int Fail = -1;

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static Double DblSub(Double x, Double y) { return x-y; }

    public static int Main()
    {
        Double y = DblSub(17d, 9d);
        if (System.Math.Abs(y-8f) <= Double.Epsilon) return Pass;
        else return Fail;
    }
}
