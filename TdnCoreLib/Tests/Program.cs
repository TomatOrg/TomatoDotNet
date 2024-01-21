
using System;
using System.Runtime.CompilerServices;

namespace Tests;

public static class Program
{

    public static int Main()
    {
        if (!CodeGenBringUpTests.Run()) return 1;
        if (!BitTest.Run()) return 2;

        return 0;
    }
    
}