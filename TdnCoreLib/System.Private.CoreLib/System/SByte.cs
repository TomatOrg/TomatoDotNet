using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public readonly struct SByte
{

    public const sbyte MaxValue = 127;
    public const sbyte MinValue = -128;
    
    private readonly sbyte _value;

}