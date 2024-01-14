using System.Reflection;

namespace System;

public class Test
{

    public object Add(int? a, int? b)
    {
        return a ?? 0 + b ?? 0;
    }

    // public int Add(int a, int b)
    // {
    //     return a + b;
    // }
    
}