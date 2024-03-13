namespace System.Runtime.CompilerServices;

public static unsafe class Unsafe
{

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int SizeOf<T>()
    {
        return sizeof(T);
    }

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern T As<T>(object? o) where T : class?;

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern ref TTo As<TFrom, TTo>(ref TFrom source);
    
    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern ref T Add<T>(ref T source, int elementOffset);
    
    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern ref T Add<T>(ref T source, IntPtr elementOffset);
    
    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern ref T Add<T>(ref T source, nuint elementOffset);

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern ref T AddByteOffset<T>(ref T source, IntPtr byteOffset);

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern ref T AddByteOffset<T>(ref T source, nuint byteOffset);

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern ref T AsRef<T>(void* source);

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern ref T AsRef<T>(scoped ref readonly T source);

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern bool AreSame<T>(ref T left, ref T right);

}