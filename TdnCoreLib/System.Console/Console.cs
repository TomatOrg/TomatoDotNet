using System.Runtime.CompilerServices;

namespace System;

public static class Console
{

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern void Write(string? value);

    [MethodImpl(MethodCodeType = MethodCodeType.Runtime)]
    public static extern void WriteLine(string? value);

}