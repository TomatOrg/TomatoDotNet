using System.Numerics;
using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public readonly struct UInt32
{
    
    public const uint MaxValue = 4294967295;
    public const uint MinValue = 0;
    
    private readonly uint _value;
    
    public static uint Log2(uint value) => (uint)BitOperations.Log2(value);
    
}