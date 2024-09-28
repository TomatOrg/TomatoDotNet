namespace System.Runtime.CompilerServices;

public static class RuntimeHelpers
{
 
    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern bool IsReferenceOrContainsReferences<T>();
    
}