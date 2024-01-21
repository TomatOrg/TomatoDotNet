using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public readonly struct UInt16
{
    
    public const ushort MaxValue = 65535;
    public const ushort MinValue = 0;
    
    private readonly ushort _value;

}