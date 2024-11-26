using System.Diagnostics.CodeAnalysis;

namespace System.Runtime.CompilerServices;

public static unsafe class Unsafe
{

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int SizeOf<T>()
    {
        return sizeof(T);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    internal static extern T As<T>(object? o) where T : class?;

    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    internal static extern ref TTo As<TFrom, TTo>(ref TFrom source);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static ref T Add<T>(ref T source, int elementOffset)
    {
        return ref AddByteOffset(ref source, (IntPtr)(elementOffset * (nint)sizeof(T)));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static ref T Add<T>(ref T source, IntPtr elementOffset)
    {
        return ref AddByteOffset(ref source, (IntPtr)((nint)elementOffset * (nint)sizeof(T)));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static ref T Add<T>(ref T source, nuint elementOffset)
    {
        return ref AddByteOffset(ref source, (nuint)(elementOffset * (nuint)sizeof(T)));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static ref T AddByteOffset<T>(ref T source, nuint byteOffset)
    {
        return ref AddByteOffset(ref source, (IntPtr)byteOffset);
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    internal static extern bool AreSame<T>(ref T left, ref T right);
    
    // TODO: BitCast

    // TODO: CopyBlock

    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    internal static extern void CopyBlockUnaligned(ref byte startAddress, ref readonly byte source, uint byteCount);

    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    public static extern bool IsAddressGreaterThan<T>([AllowNull] ref readonly T left, [AllowNull] ref readonly T right);

    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    public static extern bool IsAddressLessThan<T>([AllowNull] ref readonly T left, [AllowNull] ref readonly T right);

    // TODO: InitBlock

    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    internal static extern void InitBlockUnaligned(ref byte startAddress, byte value, uint byteCount);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static T ReadUnaligned<T>(ref readonly byte source)
    {
        return As<byte, T>(ref AsRef(in source));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static void WriteUnaligned<T>(ref byte destination, T value)
    {
        As<byte, T>(ref destination) = value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    internal static extern ref T AddByteOffset<T>(ref T source, IntPtr byteOffset);

    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    internal static extern ref T AsRef<T>(scoped ref readonly T source);

    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    public static extern IntPtr ByteOffset<T>([AllowNull] ref readonly T origin, [AllowNull] ref readonly T target);

    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    public static extern ref T NullRef<T>();

    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    public static extern bool IsNullRef<T>(ref readonly T source);

        
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static ref T Subtract<T>(ref T source, int elementOffset)
    {
        return ref SubtractByteOffset(ref source, (IntPtr)(elementOffset * (nint)sizeof(T)));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static ref T Subtract<T>(ref T source, IntPtr elementOffset)
    {
        return ref SubtractByteOffset(ref source, (IntPtr)((nint)elementOffset * (nint)sizeof(T)));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static ref T Subtract<T>(ref T source, nuint elementOffset)
    {
        return ref SubtractByteOffset(ref source, (nuint)(elementOffset * (nuint)sizeof(T)));
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static ref T SubtractByteOffset<T>(ref T source, IntPtr byteOffset)
    {
        return ref AddByteOffset(ref source, -byteOffset);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static ref T SubtractByteOffset<T>(ref T source, nuint byteOffset)
    {
        return ref SubtractByteOffset(ref source, (IntPtr)byteOffset);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining, MethodCodeType = MethodCodeType.Runtime)]
    internal static extern ref T Unbox<T>(object box) where T : struct;

}