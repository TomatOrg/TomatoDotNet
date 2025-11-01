// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System;
using System.Threading;

internal delegate T GenDelegate<T>(T p1, out T p2);

internal struct Foo<T>
{
    public T Function(T i, out T j)
    {
        j = i;
        return i;
    }
}

public class Test_Delegate010
{
    public static int Main()
    {
        int i, j;
        Foo<int> inst = new Foo<int>();
        GenDelegate<int> MyDelegate = new GenDelegate<int>(inst.Function);
        i = MyDelegate(10, out j);

        if ((i != 10) || (j != 10))
        {
            return 1;
        }

        return 100;
    }
}

