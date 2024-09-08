using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public sealed class Array
{

    private int _length;

    public int Length => _length;

}