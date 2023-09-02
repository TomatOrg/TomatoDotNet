namespace System;

public class Test
{

    public uint Do(ulong a)
    {
        return checked((uint)a);
    }
    
}