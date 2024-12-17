using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace System;

public static class Console
{

    public static void WriteLine(string? value)
    {
        Debug.Print(value!);
    }

}