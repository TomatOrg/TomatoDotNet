// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System;

internal delegate T GenDelegate<T>(T p1, out T p2);

internal interface IFoo
{
    int Function(int i, out int j);
}

internal class Foo : IFoo
{
    public virtual int Function(int i, out int j)
    {
        j = i;
        return i;
    }
}

public class Test_Delegate017
{
    public static int Main()
    {
        int i, j;
        IFoo inst = new Foo();
        GenDelegate<int> MyDelegate = new GenDelegate<int>(inst.Function);
        i = MyDelegate(10, out j);

        if ((i != 10) || (j != 10))
        {
            return 1;
        }

        return 100;
    }
}

