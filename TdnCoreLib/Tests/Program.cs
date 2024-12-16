
using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Tests;

public static class Program
{

    interface IA
    {
        public int Get();
    }
    
    class A : IA
    {
        public int Get()
        {
            return 123;
        }
    }

    public static int Lol(int a)
    {
        try
        {
            if (a < 0)
            {
                return 123;
            }
        } finally
        {
            Console.WriteLine("Finally!");
        }

        return a;
    }
    
    public static int Main()
    {
        // if (CodeGenBringUpTests.Run() != 0) return 1;
        // if (!BitTest.Run()) return 2;
        // if (!Bool_And_Op.Run()) return 3;
        // if (!Bool_No_Op.Run()) return 4;
        // if (!Int_No_Op.Run()) return 5;
        // if (!Arrays.Run()) return 6;
        // if (!ConstantFolding.Run()) return 7;
        // if (!Shifts.Run()) return 8;
    
        return Lol(0);
    }
}