using System.Runtime.CompilerServices;

namespace System.Runtime.InteropServices;

public static class MemoryMarshal
{
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe Span<byte> AsBytes<T>(Span<T> span)
        where T : unmanaged
    {
        return new Span<byte>(
            ref Unsafe.As<T, byte>(ref GetReference(span)),
            checked(span.Length * sizeof(T)));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe ReadOnlySpan<byte> AsBytes<T>(ReadOnlySpan<T> span)
        where T : unmanaged
    {
        return new ReadOnlySpan<byte>(
            ref Unsafe.As<T, byte>(ref GetReference(span)),
            checked(span.Length * sizeof(T)));
    }
    
    // public static Memory<T> AsMemory<T>(ReadOnlyMemory<T> memory) =>
    //     Unsafe.As<ReadOnlyMemory<T>, Memory<T>>(ref memory);

    internal static ref T GetReference<T>(Span<T> span) => ref span._reference;
    internal static ref T GetReference<T>(ReadOnlySpan<T> span) => ref span._reference;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Span<TTo> Cast<TFrom, TTo>(Span<TFrom> span)
        where TFrom : unmanaged
        where TTo : unmanaged
    {
        // Use unsigned integers - unsigned division by constant (especially by power of 2)
        // and checked casts are faster and smaller.
        uint fromSize = (uint)Unsafe.SizeOf<TFrom>();
        uint toSize = (uint)Unsafe.SizeOf<TTo>();
        uint fromLength = (uint)span.Length;
        int toLength;
        if (fromSize == toSize)
        {
            // Special case for same size types - `(ulong)fromLength * (ulong)fromSize / (ulong)toSize`
            // should be optimized to just `length` but the JIT doesn't do that today.
            toLength = (int)fromLength;
        }
        else if (fromSize == 1)
        {
            // Special case for byte sized TFrom - `(ulong)fromLength * (ulong)fromSize / (ulong)toSize`
            // becomes `(ulong)fromLength / (ulong)toSize` but the JIT can't narrow it down to `int`
            // and can't eliminate the checked cast. This also avoids a 32 bit specific issue,
            // the JIT can't eliminate long multiply by 1.
            toLength = (int)(fromLength / toSize);
        }
        else
        {
            // Ensure that casts are done in such a way that the JIT is able to "see"
            // the uint->ulong casts and the multiply together so that on 32 bit targets
            // 32x32to64 multiplication is used.
            ulong toLengthUInt64 = (ulong)fromLength * (ulong)fromSize / (ulong)toSize;
            toLength = checked((int)toLengthUInt64);
        }

        return new Span<TTo>(
            ref Unsafe.As<TFrom, TTo>(ref span._reference),
            toLength);
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static ReadOnlySpan<TTo> Cast<TFrom, TTo>(ReadOnlySpan<TFrom> span)
        where TFrom : unmanaged
        where TTo : unmanaged
    {
        // Use unsigned integers - unsigned division by constant (especially by power of 2)
        // and checked casts are faster and smaller.
        uint fromSize = (uint)Unsafe.SizeOf<TFrom>();
        uint toSize = (uint)Unsafe.SizeOf<TTo>();
        uint fromLength = (uint)span.Length;
        int toLength;
        if (fromSize == toSize)
        {
            // Special case for same size types - `(ulong)fromLength * (ulong)fromSize / (ulong)toSize`
            // should be optimized to just `length` but the JIT doesn't do that today.
            toLength = (int)fromLength;
        }
        else if (fromSize == 1)
        {
            // Special case for byte sized TFrom - `(ulong)fromLength * (ulong)fromSize / (ulong)toSize`
            // becomes `(ulong)fromLength / (ulong)toSize` but the JIT can't narrow it down to `int`
            // and can't eliminate the checked cast. This also avoids a 32 bit specific issue,
            // the JIT can't eliminate long multiply by 1.
            toLength = (int)(fromLength / toSize);
        }
        else
        {
            // Ensure that casts are done in such a way that the JIT is able to "see"
            // the uint->ulong casts and the multiply together so that on 32 bit targets
            // 32x32to64 multiplication is used.
            ulong toLengthUInt64 = (ulong)fromLength * (ulong)fromSize / (ulong)toSize;
            toLength = checked((int)toLengthUInt64);
        }

        return new ReadOnlySpan<TTo>(
            ref Unsafe.As<TFrom, TTo>(ref GetReference(span)),
            toLength);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe T Read<T>(ReadOnlySpan<byte> source)
        where T : unmanaged
    {
        if (sizeof(T) > source.Length)
        {
            ThrowHelper.ThrowArgumentOutOfRangeException("length");
        }
        return Unsafe.ReadUnaligned<T>(ref GetReference(source));
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe bool TryRead<T>(ReadOnlySpan<byte> source, out T value)
        where T : unmanaged
    {
        if (sizeof(T) > (uint)source.Length)
        {
            value = default;
            return false;
        }
        value = Unsafe.ReadUnaligned<T>(ref GetReference(source));
        return true;
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe void Write<T>(Span<byte> destination, in T value)
        where T : unmanaged
    {
        if ((uint)sizeof(T) > (uint)destination.Length)
        {
            ThrowHelper.ThrowArgumentOutOfRangeException("length");
        }
        Unsafe.WriteUnaligned(ref GetReference(destination), value);
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe bool TryWrite<T>(Span<byte> destination, in T value)
        where T : unmanaged
    {
        if (sizeof(T) > (uint)destination.Length)
        {
            return false;
        }
        Unsafe.WriteUnaligned(ref GetReference(destination), value);
        return true;
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe ref T AsRef<T>(Span<byte> span)
        where T : unmanaged
    {
        if (sizeof(T) > (uint)span.Length)
        {
            ThrowHelper.ThrowArgumentOutOfRangeException("length");
        }
        return ref Unsafe.As<byte, T>(ref GetReference(span));
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static unsafe ref readonly T AsRef<T>(ReadOnlySpan<byte> span)
        where T : unmanaged
    {
        if (sizeof(T) > (uint)span.Length)
        {
            ThrowHelper.ThrowArgumentOutOfRangeException("length");
        }
        return ref Unsafe.As<byte, T>(ref GetReference(span));
    }
    
    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern ref T GetArrayDataReference<T>(T[] array);

}