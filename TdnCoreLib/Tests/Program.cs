
using System;
using System.Runtime.CompilerServices;

namespace Tests;

public static class Program
{

    #region Add1
    
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int Add1(int x) { return x+1; }

    public static bool TestAdd1()
    {
        int y = Add1(1);
        if (y == 2) return true;
        else return false;
    }
    
    #endregion

    #region addref

    public static bool TestAddref()
    {
        int x = 1;
        int result = Addref(1, ref x);

        if (result == 2)
            return true;
        else
            return false;

    }
    
    [MethodImplAttribute(MethodImplOptions.NoInlining)] 
    public static int Addref(int x, ref int a)
    {
        x += a;
        return x;
    }


    #endregion

    #region And1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int And1(int x) { return x & 1; }

    public static bool TestAnd1()
    {
        int y = And1(17);
        if (y == 1) return true;
        else return false;
    }

    #endregion

    #region AndRef

    public static bool TestAndref()
    {
        int x = 13;
        int result = AndRef(15, ref x);

        if (result == 13)
            return true;
        else
            return false;
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)] 
    public static int AndRef(int x, ref int a)
    {
        x &= a;
        return x;
    }

    #endregion

    #region Args4

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int Args4(int a, int b, int c, int d)
    {
        return a+b+c+d;
    }

    public static bool TestArgs4()
    {
        int y = Args4(1,2,3,4);
        if (y == 10) return true;
        else return false;
    }

    #endregion
    
    // TODO: array tests

    #region AsgAdd1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int AsgAdd1(int x) { x += 1; return x; }

    public static bool TestAsgAdd1()
    {
        if (AsgAdd1(0) == 1) return true;
        else return false;
    }

    #endregion

    #region AsgAnd1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int AsgAnd1(int x) { x &= 3; return x; }

    public static bool TestAsgAnd1()
    {
        if (AsgAnd1(0xf) == 3) return true;
        else return false;
    }

    #endregion
    
    #region AsgOr1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int AsgOr1(int x) { x |= 0xa; return x; }

    public static bool TestAsgOr1()
    {
        if (AsgOr1(4) == 0xe) return true;
        else return false;
    }

    #endregion

    #region AsgSub1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int AsgSub1(int x) { x -= 1; return x; }

    public static bool TestAsgSub1()
    {
        if (AsgSub1(1) == 0) return true;
        else return false;
    }

    #endregion

    #region AsgXor1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int AsgXor1(int x) { x ^= 0xf; return x; }

    public static bool TestAsgXor1()
    {
        if (AsgXor1(13) == 2) return true;
        else return false;
    }

    #endregion

    #region BinaryRMW

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static void BinaryRMW(ref int x, int y)
    {
        x += y;
        x |= 2;
    }

    public static bool TestBinaryRMW()
    {
        int x = 12;
        BinaryRMW(ref x, 17);
        if (x == 31) return true;
        else return false;
    }

    #endregion
    
    // TODO: Call1
    
    // TODO: CastThenBinop

    #region CnsBool

    // Returns !b
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static bool CnsBool(bool b) 
    { 
        // Thisis just to exercise bool constants.
        // Otherwise we could write this as "return !b"
        if (b == true)
            return false;

        return true;
    }

    public static bool TestCnsBool()
    {
        bool b = CnsBool(false);
        if (b) return true;
        else return false;
    }

    #endregion

    #region CnsLng1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static long CnsLng1() { return 1; }

    public static bool TestCnsLng1()
    {
        long y = CnsLng1();
        if (y == 1) return true;
        else return false;
    }

    #endregion
    
    // TODO: dbl

    #region Div1
    
    static bool TestDiv1()
    {
        int result = Div1(12, 4);
        if (result == 3)
            return true;
        else
            return false;
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int Div1(int a, int b)
    {

        return a / b;
    }
    
    #endregion
    
    // TODO: div2
    
    // TODO: DivConst

    #region DivRef

    static bool TestDivRef()
    {
        int b = 5;
        int result = DivRef(12, ref b);
        if (result == 2)
            return true;
        else
            return false;
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int DivRef(int a, ref int b)
    {
        return a / b;
    }

    #endregion

    #region Eq1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static bool Eq1(int x)
    {
        return x == 1;
    }

    public static bool TestEq1()
    {
        bool y = Eq1(1);
        if (y == true) return true;
        else return false;
    }

    #endregion

    #region FactorialRec

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int FactorialRec(int a)
    {
        int result;
        if (a == 0)
            result = 1;
        else
        {
            result = a * FactorialRec(a - 1);
        }
        return result;
    }

    public static bool TestFactorialRec()
    {
        int s = FactorialRec(5);
        if (s != 120) return false;
        return true;
    }

    #endregion

    #region FibLoop

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int FibLoop(int x)
    {
        int curr = 0;
        int next = 1;

        for (int i = 0; i < x; i++)
        {
            int temp = curr + next;
            curr = next;
            next = temp;
        }
        return curr;
    }

    public static bool TestFibLoop()
    {
        int y = FibLoop(7);
        if (y == 13) return true;
        else return false;
    }

    #endregion

    #region FiboRec

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int FiboRec(int f0, int f1, int n)
    {
        if (n <= 0) return f0;
        if (n == 1) return f1;

        int a = FiboRec(f0, f1, n - 1) + FiboRec(f0, f1, n - 2);
        return a;
    }

    public static bool TestFiboRec()
    {
        int y = FiboRec(0, 1, 7);
        if (y == 13) return true;
        else return false;
    }

    #endregion
    
    // TODO: FP

    #region GCD

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int Gcd(int a, int b)
    {
        int result;
        if (b == 0) 
            result = a;
        else if (a < b) 
            result = Gcd(b, a);
        else
            result = Gcd(b, a%b);

        return result;
    }


    public static bool TestGcd()
    {
        int s = Gcd(36, 81);
        if (s != 9) return false;
        return true;
    }

    #endregion

    #region Ge1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static bool Ge1(int x)
    {
        return x >= 1;
    }

    public static bool TestGe1()
    {
        bool y = Ge1(1);
        if (y == true) return true;
        else return false;
    }

    #endregion
    
    public static int Main()
    {
        if (!TestAdd1()) return 1;
        if (!TestAddref()) return 2;
        if (!TestAnd1()) return 3;
        if (!TestAndref()) return 4;
        if (!TestArgs4()) return 5;
        if (!TestAsgAdd1()) return 6;
        if (!TestAsgAnd1()) return 7;
        if (!TestAsgOr1()) return 8;
        if (!TestAsgSub1()) return 9;
        if (!TestAsgXor1()) return 10;
        if (!TestBinaryRMW()) return 11;
        if (!TestCnsBool()) return 12;
        if (!TestCnsLng1()) return 13;
        if (!TestDiv1()) return 14;
        if (!TestDivRef()) return 15;
        if (!TestEq1()) return 16;
        if (!TestFactorialRec()) return 17;
        if (!TestFibLoop()) return 18;
        if (!TestFiboRec()) return 19;
        if (!TestGcd()) return 20;
        if (!TestGe1()) return 21;
        
        return 0;
    }
    
}