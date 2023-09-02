namespace System;

public class Test
{

    public uint Do(int a)
    {
        return checked((uint)a);
    }
    
}