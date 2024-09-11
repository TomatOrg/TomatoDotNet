using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public sealed partial class String
{

    private int _length;

    public int Length => _length;
    
    public override string ToString()
    {
        return this;
    }
}