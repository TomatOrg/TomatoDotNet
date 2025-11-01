// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System;

internal delegate T GenDelegate<T>(T p1, out T p2);

internal struct Foo
{
    public T Function<T>(T i, out T j)
    {
        j = i;
        return i;
    }
}

public class Test_Delegate011
{
    public static int Main()
    {
        int i, j;
        Foo inst = new Foo();
        GenDelegate<int> MyDelegate = new GenDelegate<int>(inst.Function<int>);
        i = MyDelegate(10, out j);

        if ((i != 10) || (j != 10))
        {
            return 1;
        }

        return 100;
    }
}

