using System;
using System.Runtime.CompilerServices;

namespace Tests;

public class GenericFields
{

    public class TestGetClassFromMethodParam
    {
        public class Foo<F>
        {
            public static string Value;

            // [MethodImpl(MethodImplOptions.Synchronized | MethodImplOptions.NoInlining)]
            [MethodImpl(MethodImplOptions.NoInlining)]
            public static void Action<T>(T value)
            {
                Value = value.ToString();
            }
        }
        
        public class Dummy { }

        public static bool Run()
        {
            string s = "hello";

            // Foo<object>.Action<string>(s);
            // if (Foo<object>.Value != s)
            //     throw new Exception();
            //
            int i = 10;
            // Foo<Dummy>.Action<int>(i);
            // if (Foo<object>.Value != s)
            //     throw new Exception();
            // if (Foo<Dummy>.Value != i.ToString())
            //     throw new Exception();
            //
            // object o = new object();
            // Foo<int>.Action<object>(o);
            // if (Foo<int>.Value != o.ToString())
            //     throw new Exception();
            // if (Foo<object>.Value != s)
            //     throw new Exception();
            if (Foo<Dummy>.Value != i.ToString())
                throw new Exception();

            return true;
        }
    }

    public static bool Run()
    {
        if (TestGetClassFromMethodParam.Run()) return false;

        return true;
    }
    
}