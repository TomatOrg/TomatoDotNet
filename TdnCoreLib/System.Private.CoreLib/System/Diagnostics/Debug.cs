using System.Diagnostics.CodeAnalysis;

namespace System.Diagnostics;

public static class Debug
{
    
    [Conditional("DEBUG")]
    public static void Assert([DoesNotReturnIf(false)] bool condition) =>
        Assert(condition, string.Empty, string.Empty);
    
    [Conditional("DEBUG")]
    public static void Assert([DoesNotReturnIf(false)] bool condition, string? message) =>
        Assert(condition, message, string.Empty);
    
    [Conditional("DEBUG")]
    public static void Assert([DoesNotReturnIf(false)] bool condition, string? message, string? detailMessage)
    {
        if (!condition)
        {
            throw new Exception(message);
        }
    }
}