using System.Runtime.CompilerServices;

namespace System;

public sealed partial class String
{
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool EqualsHelper(string strA, string strB)
    {
        if (strA.Length != strB.Length)
        {
            return false;
        }
        
        // TODO: comapre actual chars
        
        return true;
    }
    
    public static bool operator ==(string? a, string? b) => string.Equals(a, b);

    public static bool operator !=(string? a, string? b) => !string.Equals(a, b);
    
    public static bool Equals(string? a, string? b)
    {
        if (object.ReferenceEquals(a, b))
        {
            return true;
        }

        if (a is null || b is null || a.Length != b.Length)
        {
            return false;
        }

        return EqualsHelper(a, b);
    }
    
}