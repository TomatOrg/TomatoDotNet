namespace System.Runtime.CompilerServices;

public static class RuntimeFeature
{
    
    public const string ByRefFields = nameof(ByRefFields);
    public const string NumericIntPtr = nameof(NumericIntPtr);

    public static bool IsSupported(string feature)
    {
        switch (feature)
        {
            case ByRefFields:
            case NumericIntPtr:
                return true;
            
            default:
                return false;
        }
    }
    
}