// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//

using System;

struct Gen<T>
{
    public T Field;

    public T[] TArray;

    public T Property
    {
        get { return Field; }
        set { Field = value; }
    }

    public T this[int i]
    {
        get { return TArray[i]; }
        set { TArray[i] = value; }
    }

    public T Method(T t)
    {
        return t;
    }
}

public class Test_struct_instance01
{
    public static int Main()
    {
        int ret = 100;

        Gen<int> GenInt = new Gen<int>();

        GenInt.Field = 5;
        if (GenInt.Field != 5)
        {
            ret = 1;
        }

        GenInt.Property = 10;

        if (GenInt.Property != 10)
        {
            ret = 1;
        }

        GenInt.TArray = new int[10];

        if (GenInt.TArray.Length != 10)
        {
            ret = 1;
        }

        for (int i = 0; (i < 10); i++)
        {
            GenInt[i] = 15;
            if (GenInt[i] != 15)
            {
                ret = 1;
            }
        }

        if (GenInt.Method(20) != 20)
        {
            ret = 1;
        }

        Gen<String> GenString = new Gen<String>();

        GenString.Field = "Field";
        if (GenString.Field != "Field")
        {
            ret = 1;
        }

        GenString.Property = "Property";

        if (GenString.Property != "Property")
        {
            ret = 1;
        }

        GenString.TArray = new String[10];

        if (GenString.TArray.Length != 10)
        {
            ret = 1;
        }

        for (int i = 0; (i < 10); i++)
        {
            GenString[i] = "ArrayString";
            if (GenString[i] != "ArrayString")
            {
                ret = 1;
            }
        }

        if (GenString.Method("Method") != "Method")
        {
            ret = 1;
        }

        return ret;

    }
}
