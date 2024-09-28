using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace System;

internal static class SpanHelpers
{

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern void ClearWithoutReferences(ref byte b, nuint byteLength);
    
    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern void ClearWithReferences(ref IntPtr ip, nuint pointerSizeLength);

    public static void Fill<T>(ref T refData, nuint numElements, T value)
    {
        for (nuint i = 0; i < numElements; i++)
        {
            Unsafe.Add(ref refData, i) = value;
        }
    }

}