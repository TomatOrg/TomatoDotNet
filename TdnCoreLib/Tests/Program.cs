
using System;
using System.Runtime.CompilerServices;

namespace Tests;

public static class Program
{

    public static int Main()
    {
        if (!CodeGenBringUpTests.Run()) return 1;
        if (!BitTest.Run()) return 2;
        if (!Bool_And_Op.Run()) return 3;
        if (!Bool_No_Op.Run()) return 4;
        if (!Int_No_Op.Run()) return 5;

        return 0;
    }
    
}