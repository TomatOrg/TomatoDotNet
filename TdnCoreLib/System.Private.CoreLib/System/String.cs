using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public sealed partial class String
{

    private int _length;
    private byte _padding0;
    private byte _padding1;
    private byte _padding2;
    private byte _padding3;

    public int Length => _length;
    
    public override string ToString()
    {
        return this;
    }
}