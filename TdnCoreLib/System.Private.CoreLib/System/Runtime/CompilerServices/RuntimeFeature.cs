namespace System.Runtime.CompilerServices;

public static class RuntimeFeature
{
    
    public const string ByRefFields = nameof(ByRefFields);
    public const string NumericIntPtr = nameof(NumericIntPtr);
    public const string VirtualStaticsInInterfaces = nameof(VirtualStaticsInInterfaces);

    public static bool IsSupported(string feature)
    {
        return feature switch
        {
            ByRefFields or NumericIntPtr or VirtualStaticsInInterfaces => true,
            _ => false
        };
    }
    
}