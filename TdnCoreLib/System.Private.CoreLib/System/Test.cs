namespace System;

public class Test
{
    
    public int Do(int a, int b)
    {
        if (a == b)
        {
            return -1;
        }

        return a > b ? b : a;
    }
    
}