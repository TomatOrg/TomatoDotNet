// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//


using System;
using System.Runtime.CompilerServices;

struct vc
{
    public int x;
    public int y;
    public int z;
    public vc (int xx, int yy, int zz) { x = xx; y = yy; z = zz; }
}

public class child
{
    const int Pass = 100;
    const int Fail = -1;

    public static int Main()
    {
        int result=  mul3(3);
        if (result == 15369)
            return Pass;
        else
            return Fail;
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)] 
    public static int mul3(int a)
    {
        return a*5123;
    }
    
}

