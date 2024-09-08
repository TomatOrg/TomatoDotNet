using System.Runtime.CompilerServices;

namespace System.Runtime.InteropServices;

public static class MemoryMarshal
{

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern ref T GetArrayDataReference<T>(T[] array);

}