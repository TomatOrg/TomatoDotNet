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

    T VMethod(T t);
}

class GenInt : IGen<int>
{
    public GenInt()
    {
        TArray = new int[10];
    }

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

    public virtual int VMethod(int t)
    {
        return t;
    }

}

class GenString : IGen<string>
{
    public GenString()
    {
        TArray = new string[10];
    }

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

    public virtual string VMethod(string t)
    {
        return t;
    }

}

public class Test_interface_class02
{
    public static int Main()
    {
        int ret = 100;

        IGen<int> Gen_Int = new GenInt();

        Gen_Int.Property = 10;

        if (Gen_Int.Property != 10)
        {
            ret = 1;
        }

        for (int i = 0; (i < 10); i++)
        {
            Gen_Int[i] = 15;
            if (Gen_Int[i] != 15)
            {
                ret = 1;
            }
        }

        if (Gen_Int.Method(20) != 20)
        {
            ret = 1;
        }

        if (Gen_Int.VMethod(25) != 25)
        {
            ret = 1;
        }

        IGen<String> Gen_String = new GenString();

        Gen_String.Property = "Property";

        if (Gen_String.Property != "Property")
        {
            ret = 1;
        }

        for (int i = 0; (i < 10); i++)
        {
            Gen_String[i] = "ArrayString";
            if (Gen_String[i] != "ArrayString")
            {
                ret = 1;
            }
        }

        if (Gen_String.Method("Method") != "Method")
        {
            ret = 1;
        }

        if (Gen_String.VMethod("VirtualMethod") != "VirtualMethod")
        {
            ret = 1;
        }

        return ret;

    }
}
