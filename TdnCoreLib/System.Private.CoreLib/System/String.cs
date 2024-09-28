using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public sealed partial class String
{

    private int _length;

    public int Length => _length;
    
    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public extern String(ReadOnlySpan<char> value);
    
    public override string ToString()
    {
        return this;
    }
}