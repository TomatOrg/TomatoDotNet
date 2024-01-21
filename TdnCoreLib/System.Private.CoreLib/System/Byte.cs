using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public readonly struct Byte
{
    
    public const byte MaxValue = 255;
    public const byte MinValue = 0;
    
    private readonly byte _value;
    
}