namespace System;

public class Test
{

    public int Do(int[] arr)
    {
        var sum = 0;
        for (var i = 0; i < arr.Length; i++)
        {
            sum += arr[i];
        }
        return sum;
    }
    
}