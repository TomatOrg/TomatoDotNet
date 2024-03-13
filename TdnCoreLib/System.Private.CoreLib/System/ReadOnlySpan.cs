using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace System;

public readonly ref struct ReadOnlySpan<T>
{
    
    public static ReadOnlySpan<T> Empty => default;
    
    private readonly ref T _reference;
    private readonly int _length;

    public int Length => _length;
    
    public bool IsEmpty => _length == 0;
    
    public ReadOnlySpan(ref T reference)
    {
        _reference = ref Unsafe.AsRef(in reference);
        _length = 1;
    }
    
    public ReadOnlySpan(T[]? array)
    {
        if (array == null)
        {
            this = default;
            return; // returns default
        }
    
        _reference = ref MemoryMarshal.GetArrayDataReference(array);
        _length = array.Length;
    }
    
    public ReadOnlySpan(T[]? array, int start, int length)
    {
        if (array == null)
        {
            if (start != 0 || length != 0)
                ThrowHelper.ThrowArgumentOutOfRangeException();
            this = default;
            return; // returns default
        }
        
        if ((ulong)(uint)start + (ulong)(uint)length > (ulong)(uint)array.Length)
            ThrowHelper.ThrowArgumentOutOfRangeException();
        
        _reference = ref Unsafe.Add(ref MemoryMarshal.GetArrayDataReference(array), (nint)(uint)start /* force zero-extension */);
        _length = length;
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal ReadOnlySpan(ref T reference, int length)
    {
        Debug.Assert(length >= 0);
    
        _reference = ref reference;
        _length = length;
    }
    
    public ref readonly T this[int index]
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get
        {
            if ((uint)index >= (uint)_length)
                ThrowHelper.ThrowIndexOutOfRangeException();
            return ref Unsafe.Add(ref _reference, (nint)(uint)index /* force zero-extension */);
        }
    }
    
    public static bool operator !=(ReadOnlySpan<T> left, ReadOnlySpan<T> right) => !(left == right);
    
    public override bool Equals(object? obj) =>
        throw new NotSupportedException(SR.NotSupported_CannotCallEqualsOnSpan);
    
    public override int GetHashCode() =>
        throw new NotSupportedException(SR.NotSupported_CannotCallGetHashCodeOnSpan);
    
    public static implicit operator ReadOnlySpan<T>(T[]? array) => new(array);
    
    public static bool operator ==(ReadOnlySpan<T> left, ReadOnlySpan<T> right) =>
        left._length == right._length &&
        Unsafe.AreSame(ref left._reference, ref right._reference);
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public ReadOnlySpan<T> Slice(int start)
    {
        if ((uint)start > (uint)_length)
            ThrowHelper.ThrowArgumentOutOfRangeException();
        return new ReadOnlySpan<T>(ref Unsafe.Add(ref _reference, (nint)(uint)start /* force zero-extension */), _length - start);
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public ReadOnlySpan<T> Slice(int start, int length)
    {
        if ((ulong)(uint)start + (ulong)(uint)length > (ulong)(uint)_length)
            ThrowHelper.ThrowArgumentOutOfRangeException();
        return new ReadOnlySpan<T>(ref Unsafe.Add(ref _reference, (nint)(uint)start /* force zero-extension */), length);
    }
    
}