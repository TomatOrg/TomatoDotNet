﻿
using System;
using System.Runtime.CompilerServices;

namespace Tests;

public static class Program
{

    public interface IA
    {
        public int Test();
    }

    public class A : IA
    {
        public int Test()
        {
            return 123;
        }
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

        int thing = 123;
        Span<int> lol = new Span<int>(ref thing);

        var sum = 0;
        foreach(var value in lol) {
            sum += value;
        }

        return sum;
    }
    
}