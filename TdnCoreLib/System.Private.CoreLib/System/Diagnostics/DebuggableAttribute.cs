namespace System.Diagnostics;

[AttributeUsage(AttributeTargets.Assembly | AttributeTargets.Module, AllowMultiple = false)]
public sealed class DebuggableAttribute : Attribute
{
    
    [Flags]
    public enum DebuggingModes
    {
        None = 0x0,
        Default = 0x1,
        DisableOptimizations = 0x100,
        IgnoreSymbolStoreSequencePoints = 0x2,
        EnableEditAndContinue = 0x4
    }
    
}