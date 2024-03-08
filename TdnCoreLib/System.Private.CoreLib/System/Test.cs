using System.Reflection;

namespace System;

public class Test
{

    public class A<T>
    {

        public static TK B<TK>(TK a, T b)
        {
            return default;
        }
        
    }

    public class Lol<T>
    {
        public static TK B<TK>(TK a, T b)
        {
            return A<T>.B<TK>(a, b);
        }
    }
    
    public int Add(int a)
    {
        return Lol<object>.B<int>(a, null);
    }
    
}