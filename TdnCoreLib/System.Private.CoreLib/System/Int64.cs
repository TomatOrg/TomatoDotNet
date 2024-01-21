using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public readonly struct Int64
{
    
    public const long MaxValue = 9223372036854775807;
    public const long MinValue = -9223372036854775808;
    
    private readonly long _value;
    
}