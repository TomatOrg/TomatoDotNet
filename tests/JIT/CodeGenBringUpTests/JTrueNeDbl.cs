// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//


using System;
using System.Runtime.CompilerServices;
public class BringUpTest_JTrueNeDbl
{
    const int Pass = 100;
    const int Fail = -1;

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int JTrueNeDbl(double x)
    {
        int returnValue = -1;

        if (x != -1d)
        {
            if (x != 0d)
            {
                if (x != 1d)
                {
                    returnValue = 4;
                }
                returnValue = 3;
            }
            else returnValue = 2;
        }
        else returnValue = 1;


        return returnValue;
    }

    public static int Main()
    {
        int returnValue = Pass;

        if (JTrueNeDbl(-1d) != 1) { returnValue = Fail; }
        if (JTrueNeDbl(0d) != 2) { returnValue = Fail; }
        if (JTrueNeDbl(1d) != 3) { returnValue = Fail; }

        return returnValue;
    }
}
