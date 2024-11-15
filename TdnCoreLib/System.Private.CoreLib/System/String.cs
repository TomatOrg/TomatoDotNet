using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public sealed partial class String
{

    public static readonly string Empty = "";
    
    public int Length => _length;

    private int _length;
    private char _firstChar;

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public extern String(ReadOnlySpan<char> value);
    
    public static bool IsNullOrEmpty([NotNullWhen(false)] string? value)
    {
        return value == null || value.Length == 0;
    }
    
    public override string ToString()
    {
        return this;
    }
    
}