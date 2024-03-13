using System.Runtime.CompilerServices;

namespace System.Runtime.InteropServices;

public static class MemoryMarshal
{

    public static ref T GetArrayDataReference<T>(T[] array)
    {
        return ref Unsafe.As<byte, T>(ref Unsafe.AddByteOffset(ref array._padding3, 1));
    }
    
}