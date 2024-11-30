
using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Tests;

public static class Program
{

    public static T Reduce<T>(Func<T, T, T> func, T[] arr)
    {
        if (arr.Length == 0)
        {
            return default!;
        }
        
        var result = arr[0];
        for (int i = 1; i < arr.Length; i++)
        {
            result = func(result, arr[i]);
        }

        return result;
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

        int[] lis = [1, 3, 5, 6, 2];
        return Reduce(Math.Max, lis);
    }
}