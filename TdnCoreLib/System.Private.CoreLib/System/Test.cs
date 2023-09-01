namespace System;

public class Test
{

    public struct Lol
    {
        public int a;
        public int b;
    }
    
    public bool Do(Lol[] arr, int[] arr2)
    {
        if (arr.Length * 2 != arr2.Length)
        {
            return false;
        }
        
        for (var i = 0; i < 10; i++)
        {
            arr[i].a = arr2[i * 2];
            arr[i].b = arr2[i * 2 + 1];
        }

        return true;
    }
    
}