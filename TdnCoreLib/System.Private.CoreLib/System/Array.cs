using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public sealed class Array
{

    private static class EmptyArray<T>
    {
        internal static readonly T[] Value = new T[0];
    }

    public static T[] Empty<T>()
    {
        return EmptyArray<T>.Value;
    }
    
    private int _length;

    public int Length => _length;

}