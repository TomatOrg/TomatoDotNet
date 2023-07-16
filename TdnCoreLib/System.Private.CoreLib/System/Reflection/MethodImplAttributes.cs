namespace System.Reflection;

[Flags]
public enum MethodImplAttributes
{
    
    AggressiveInlining = 256, 	
    AggressiveOptimization = 512, 	
    CodeTypeMask = 3, 	
    ForwardRef = 16, 	
    IL = 0, 	
    InternalCall = 4096, 	
    Managed = 0, 	
    ManagedMask = 4, 	
    MaxMethodImplVal = 65535, 	
    Native = 1, 	
    NoInlining = 8, 	
    NoOptimization = 64, 	
    OPTIL = 2, 	
    PreserveSig = 128, 	
    Runtime = 3, 	
    Synchronized = 32, 	
    Unmanaged = 4, 	
    
}