
using System;
using System.Runtime.CompilerServices;

namespace Tests;

public static class Program
{

    public static int Main()
    {
        if (!CodeGenBringUpTests.Run()) return 1;
        if (!BitTest.Run()) return 2;
        if (Bool_And_Op.Run() != 100) return 3;

        return 0;
    }
    
}