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

    public String(ReadOnlySpan<char> value)
    {
        _length = value.Length;
        Buffer.Memmove(ref _firstChar, ref value._reference, (nuint)value.Length);
    }

    internal String(int length)
    {
        _length = length;
    }
    
    [IndexerName("Chars")]
    public char this[int index]
    {
        get
        {
            if ((uint)index >= (uint)_length)
                ThrowHelper.ThrowIndexOutOfRangeException();
            return Unsafe.Add(ref Unsafe.AsRef(ref _firstChar), (nint)(uint)index /* force zero-extension */);
        }
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static implicit operator ReadOnlySpan<char>(string? value) =>
        value != null ? new ReadOnlySpan<char>(ref value.GetRawStringData(), value.Length) : default;
    
    internal ref char GetRawStringData() => ref _firstChar;
    
    public override string ToString()
    {
        return this;
    }

    public static bool IsNullOrEmpty([NotNullWhen(false)] string? value)
    {
        return value == null || value.Length == 0;
    }
    
}