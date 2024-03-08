using System.Reflection;

namespace System;

public class Test
{

    public T A<T>(T a)
    {
        return a;
    }

    public int B<T>(int a)
    {
        return A<int>(a);
    }
    
    public int Add(int a)
    {
        return B<int>(a);
    }
    
}