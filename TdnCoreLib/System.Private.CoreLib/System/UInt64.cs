using System.Runtime.InteropServices;

namespace System;


[StructLayout(LayoutKind.Sequential)]
public readonly struct UInt64
{
    
    public const ulong MaxValue = 18446744073709551615;
    public const ulong MinValue = 0;
    
    private readonly ulong m_value;
    
}