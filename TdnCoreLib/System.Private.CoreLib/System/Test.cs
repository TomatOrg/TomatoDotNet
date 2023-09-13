namespace System;

public class Test
{

    public bool Do(long a)
    {
        try
        {
            checked
            {
                _ = (int)a;
            }
        }
        catch (OverflowException e)
        {
            return false;
        }

        return true;
    }
    
}