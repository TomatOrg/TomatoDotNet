// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//

using System;

interface IGen<T>
{
    T Property
    {
        get;
        set;
    }

    T this[int i]
    {
        get;
        set;
    }

    T Method(T t);
}

struct GenInt : IGen<int>
{
    public int Field;

    public int[] TArray;

    public int Property
    {
        get { return Field; }
        set { Field = value; }
    }

    public int this[int i]
    {
        get { return TArray[i]; }
        set { TArray[i] = value; }
    }

    public int Method(int t)
    {
        return t;
    }

}

struct GenString : IGen<string>
{
    public string Field;

    public string[] TArray;

    public string Property
    {
        get { return Field; }
        set { Field = value; }
    }

    public string this[int i]
    {
        get { return TArray[i]; }
        set { TArray[i] = value; }
    }

    public string Method(string t)
    {
        return t;
    }

}

public class Test_interface_struct02
{
    public static int Main()
    {
        int ret = 100;

        GenInt GenIntStruct = new GenInt();
        GenIntStruct.TArray = new int[10];
        IGen<int> IGenInt = GenIntStruct;

        IGenInt.Property = 10;

        if (IGenInt.Property != 10)
        {
            ret = 1;
        }

        for (int i = 0; (i < 10); i++)
        {
            IGenInt[i] = 15;
            if (IGenInt[i] != 15)
            {
                ret = 1;
            }
        }

        if (IGenInt.Method(20) != 20)
        {
            ret = 1;
        }

        GenString GenStringStruct = new GenString();
        GenStringStruct.TArray = new string[10];
        IGen<string> IGenString = GenStringStruct;

        IGenString.Property = "Property";

        if (IGenString.Property != "Property")
        {
            ret = 1;
        }

        for (int i = 0; (i < 10); i++)
        {
            IGenString[i] = "ArrayString";
            if (IGenString[i] != "ArrayString")
            {
                ret = 1;
            }
        }

        if (IGenString.Method("Method") != "Method")
        {
            ret = 1;
        }

        return ret;

    }
}
