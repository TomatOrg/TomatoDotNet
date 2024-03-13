namespace System.Diagnostics;

public static class Debug
{
    
    [Conditional("DEBUG")]
    public static void Assert(bool condition)
    {
        if (!condition)
        {
            throw new Exception();
        }
    }
    
}