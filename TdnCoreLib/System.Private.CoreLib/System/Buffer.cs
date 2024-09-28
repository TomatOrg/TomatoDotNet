using System.Runtime.CompilerServices;

namespace System;

public static class Buffer
{

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    internal static extern void Memmove(ref byte dest, ref byte src, nuint len);

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    internal static extern void Memmove<T>(ref T destination, ref T source, nuint elementCount);

}