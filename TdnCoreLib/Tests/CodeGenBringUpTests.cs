using System;
using System.Runtime.CompilerServices;

namespace Tests;

public class CodeGenBringUpTests
{
    
    #region Add1

    [MethodImpl(MethodImplOptions.NoInlining)]
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

    [MethodImpl(MethodImplOptions.NoInlining)] 
    public static int Addref(int x, ref int a)
    {
        x += a;
        return x;
    }

    #endregion

    #region And1

    [MethodImpl(MethodImplOptions.NoInlining)]
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

    [MethodImpl(MethodImplOptions.NoInlining)] 
    public static int AndRef(int x, ref int a)
    {
        x &= a;
        return x;
    }

    #endregion

    #region Args4

    [MethodImpl(MethodImplOptions.NoInlining)]
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

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int AsgAdd1(int x) { x += 1; return x; }

    public static bool TestAsgAdd1()
    {
        if (AsgAdd1(0) == 1) return true;
        else return false;
    }

    #endregion

    #region AsgAnd1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int AsgAnd1(int x) { x &= 3; return x; }

    public static bool TestAsgAnd1()
    {
        if (AsgAnd1(0xf) == 3) return true;
        else return false;
    }

    #endregion
    
    #region AsgOr1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int AsgOr1(int x) { x |= 0xa; return x; }

    public static bool TestAsgOr1()
    {
        if (AsgOr1(4) == 0xe) return true;
        else return false;
    }

    #endregion

    #region AsgSub1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int AsgSub1(int x) { x -= 1; return x; }

    public static bool TestAsgSub1()
    {
        if (AsgSub1(1) == 0) return true;
        else return false;
    }

    #endregion

    #region AsgXor1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int AsgXor1(int x) { x ^= 0xf; return x; }

    public static bool TestAsgXor1()
    {
        if (AsgXor1(13) == 2) return true;
        else return false;
    }

    #endregion

    #region BinaryRMW

    [MethodImpl(MethodImplOptions.NoInlining)]
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

    [MethodImpl(MethodImplOptions.NoInlining)]
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

    [MethodImpl(MethodImplOptions.NoInlining)]
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

    private static bool TestDiv1()
    {
        int result = Div1(12, 4);
        if (result == 3)
            return true;
        else
            return false;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int Div1(int a, int b)
    {

        return a / b;
    }

    #endregion
    
    // TODO: div2
    
    // TODO: DivConst

    #region DivRef

    private static bool TestDivRef()
    {
        int b = 5;
        int result = DivRef(12, ref b);
        if (result == 2)
            return true;
        else
            return false;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int DivRef(int a, ref int b)
    {
        return a / b;
    }

    #endregion

    #region Eq1

    [MethodImpl(MethodImplOptions.NoInlining)]
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

    [MethodImpl(MethodImplOptions.NoInlining)]
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

    [MethodImpl(MethodImplOptions.NoInlining)]
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

    [MethodImpl(MethodImplOptions.NoInlining)]
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

    [MethodImpl(MethodImplOptions.NoInlining)]
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

    [MethodImpl(MethodImplOptions.NoInlining)]
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

    #region Gt1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static bool Gt1(int x)
    {
        return x > 1;
    }

    public static bool TestGt1()
    {
        bool y = Gt1(1);
        if (y == false) return true;
        else return false;
    }

    #endregion

    #region Ind1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void Ind1(ref int x) { x = 1; return; }

    public static bool TestInd1()
    {
        int y = 0;
        Ind1(ref y);
        if (y == 1) return true;
        else return false;
    }

    #endregion

    #region InitObj

    public struct MyClass
    {
        public int x;
        public int y;
        public int z;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static bool InitObj()
    {
        MyClass c = new MyClass();
        return c.x == c.y &&
               c.y == c.z &&
               c.z == 0;
    }


    public static bool TestInitObj()
    {
        if (InitObj())
        {
            return true;
        }
        else
        {
            return false;
        }
    }

    #endregion
    
    // TODO: InstanceCalls
    
    // TODO: IntArraySum

    #region IntConv

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static long IntConv(int x) { return (long) x; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static long IntConv(UInt32 x) { return (long) x; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int IntConv(UInt16 x) { return (int)x; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int IntConv(byte x) { return (int)x; }

    //[MethodImplAttribute(MethodImplOptions.NoInlining)]
    //public static UInt16 IntConv(byte x) { return (UInt16)x; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static byte IntConv(Int16 x) { return (byte)x; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static UInt32 IntConv(long x) { return (UInt32)x; }

    public static bool TestIntConv()
    {
        long x = IntConv((int)3);
        if (x != 3) return false;
        
        x = IntConv((UInt32)3294168832);
        if (x != 3294168832L) return false;

        int z = IntConv((UInt16) 123);
        if (z != 123) return false;

        z = IntConv((byte)3);
        if (z != 3) return false;
               
        byte w = IntConv((Int16)3);
        if (w != 3) return false;

        UInt32 y = IntConv(1234L);
        if (y != 1234U) return false;

        return true;
    }

    #endregion

    #region Jmp1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int Jmp1(int x)
    {
        goto L1;
        L2:
        x = x+1;
        goto L3;
        L1:
        x = x+1;
        goto L2;
        L3:
        return x+1;
    }

    public static bool TestJmp1()
    {
        int y = Jmp1(1);
        if (y == 4) return true;
        else return false;
    }

    #endregion

    #region JTrue1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int JTrue1(int x)
    {
        if (x == 1)
            return x+1;
        return 0;
    }

    public static bool TestJTrue1()
    {
        int y = JTrue1(1);
        if (y == 2) return true;
        else return false;
    }

    #endregion
    
    // TODO: JTrueEqFP

    #region JTrueEqInt1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int JTrueEqInt1(int x)
    {
        int returnValue = 0;

        if (x == int.MinValue) returnValue = 1;
        else if (x == -1) returnValue = 2;
        else if (x == 0) returnValue = 3;
        else if (x == 1) returnValue = 4;
        else if (x == int.MaxValue) returnValue = 5;

        return returnValue;
    }

    public static bool TestJTrueEqInt1()
    {
        bool returnValue = true;

        if (JTrueEqInt1(int.MinValue)   != 1) returnValue = false;
        if (JTrueEqInt1(int.MinValue+1) != 0) returnValue = false;
        if (JTrueEqInt1(-1)             != 2) returnValue = false;
        if (JTrueEqInt1(0)              != 3) returnValue = false;
        if (JTrueEqInt1(1)              != 4) returnValue = false;
        if (JTrueEqInt1(int.MaxValue-1) != 0) returnValue = false;
        if (JTrueEqInt1(int.MaxValue)   != 5) returnValue = false;

        return returnValue;
    }

    #endregion

    // TODO: JTrueGeDbl
    // TODO: JTrueGeFP

    #region JTrueGeInt1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int JTrueGeInt1(int x)
    {
        int returnValue = -1;

        if (x >= int.MaxValue)          returnValue = 7;
        else if (x >= 2)                returnValue = 6;
        else if (x >= 1)                returnValue = 5;
        else if (x >= 0)                returnValue = 4;
        else if (x >= -1)               returnValue = 3;
        else if (x >= (int.MinValue+1)) returnValue = 2;
        else if (x >= int.MinValue)     returnValue = 1;

        return returnValue;
    }

    public static bool TestJTrueGeInt1()
    {
        bool returnValue = true;

        if (JTrueGeInt1(int.MinValue)   != 1) returnValue = false;
        if (JTrueGeInt1(int.MinValue+1) != 2) returnValue = false;
        if (JTrueGeInt1(-1)             != 3) returnValue = false;
        if (JTrueGeInt1(0)              != 4) returnValue = false;
        if (JTrueGeInt1(1)              != 5) returnValue = false;
        if (JTrueGeInt1(int.MaxValue-1) != 6) returnValue = false;
        if (JTrueGeInt1(int.MaxValue)   != 7) returnValue = false;

        return returnValue;
    }

    #endregion
    
    // TODO: JTrueGtDbl
    // TODO: JTrueGtFP

    #region JTrueGtInt1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int JTrueGtInt1(int x)
    {
        int returnValue = -1;

        if (x > int.MaxValue)          returnValue = 0;    // Never true
        else if (x > (int.MaxValue-1)) returnValue = 7;
        else if (x > 1)                returnValue = 6;
        else if (x > 0)                returnValue = 5;
        else if (x > -1)               returnValue = 4;
        else if (x > (int.MinValue+1)) returnValue = 3;
        else if (x > int.MinValue)     returnValue = 2;
        else                           returnValue = 1;

        return returnValue;
    }

    public static bool TestJTrueGtInt1()
    {
        bool returnValue = true;

        if (JTrueGtInt1(int.MinValue)   != 1) returnValue = false;
        if (JTrueGtInt1(int.MinValue+1) != 2) returnValue = false;
        if (JTrueGtInt1(-1)             != 3) returnValue = false;
        if (JTrueGtInt1(0)              != 4) returnValue = false;
        if (JTrueGtInt1(1)              != 5) returnValue = false;
        if (JTrueGtInt1(int.MaxValue-1) != 6) returnValue = false;
        if (JTrueGtInt1(int.MaxValue)   != 7) returnValue = false;

        return returnValue;
    }

    #endregion
    
    // TODO: JTrueLeDbl
    // TODO: JTrueLeFP

    #region JTrueLeInt1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int JTrueLeInt1(int x)
    {
        int returnValue = -1;

        if (x <= int.MinValue)          returnValue = 1;
        else if (x <= -2)               returnValue = 2;
        else if (x <= -1)               returnValue = 3;
        else if (x <= 0)                returnValue = 4;
        else if (x <= 1)                returnValue = 5;
        else if (x <= (int.MaxValue-1)) returnValue = 6;
        else if (x <= int.MaxValue)     returnValue = 7;

        return returnValue;
    }

    public static bool TestJTrueLeInt1()
    {
        bool returnValue = true;

        if (JTrueLeInt1(int.MinValue)   != 1) returnValue = false;
        if (JTrueLeInt1(int.MinValue+1) != 2) returnValue = false;
        if (JTrueLeInt1(-1)             != 3) returnValue = false;
        if (JTrueLeInt1(0)              != 4) returnValue = false;
        if (JTrueLeInt1(1)              != 5) returnValue = false;
        if (JTrueLeInt1(int.MaxValue-1) != 6) returnValue = false;
        if (JTrueLeInt1(int.MaxValue)   != 7) returnValue = false;

        return returnValue;
    }

    #endregion
    
    // TODO: JTrueLtDbl
    // TODO: JTrueLtFP

    #region JTrueLtInt1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int JTrueLtInt1(int x)
    {
        int returnValue = -1;

        if (x < int.MinValue)          returnValue = 0;    // Never true
        else if (x < (int.MinValue+1)) returnValue = 1;
        else if (x < -1)               returnValue = 2;
        else if (x < 0)                returnValue = 3;
        else if (x < 1)                returnValue = 4;
        else if (x < (int.MaxValue-1)) returnValue = 5;
        else if (x < int.MaxValue)     returnValue = 6;
        else                           returnValue = 7;

        return returnValue;
    }

    public static bool TestJTrueLtInt1()
    {
        bool returnValue = true;

        if (JTrueLtInt1(int.MinValue)   != 1) returnValue = false;
        if (JTrueLtInt1(int.MinValue+1) != 2) returnValue = false;
        if (JTrueLtInt1(-1)             != 3) returnValue = false;
        if (JTrueLtInt1(0)              != 4) returnValue = false;
        if (JTrueLtInt1(1)              != 5) returnValue = false;
        if (JTrueLtInt1(int.MaxValue-1) != 6) returnValue = false;
        if (JTrueLtInt1(int.MaxValue)   != 7) returnValue = false;

        return returnValue;
    }

    #endregion
    
    // TODO: JTrueNeDbl
    // TODO: JTrueNeFP

    #region JTrueNeInt1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int JTrueNeInt1(int x)
    {
        int returnValue = -1;

        if (x != int.MinValue)
        {
            if (x != -1)
            {
                if (x != 0)
                {
                    if (x != 1)
                    {
                        if (x != int.MaxValue) returnValue = 0;
                        else returnValue = 5;
                    }
                    else returnValue = 4;
                }
                else returnValue = 3;
            }
            else returnValue = 2;
        }
        else returnValue = 1;

        return returnValue;
    }

    public static bool TestJTrueNeInt1()
    {
        bool returnValue = true;

        if (JTrueNeInt1(int.MinValue)   != 1) returnValue = false;
        if (JTrueNeInt1(int.MinValue+1) != 0) returnValue = false;
        if (JTrueNeInt1(-1)             != 2) returnValue = false;
        if (JTrueNeInt1(0)              != 3) returnValue = false;
        if (JTrueNeInt1(1)              != 4) returnValue = false;
        if (JTrueNeInt1(int.MaxValue-1) != 0) returnValue = false;
        if (JTrueNeInt1(int.MaxValue)   != 5) returnValue = false;

        return returnValue;
    }

    #endregion
    
    #region Le1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static bool Le1(int x)
    {
        return x <= 1;
    }

    public static bool TestLe1()
    {
        bool y = Le1(1);
        if (y == true) return true;
        else return false;
    }

    #endregion

    #region LeftShift

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int LeftShift(int x, int y) { return x << y; }

    public static bool TestLeftShift()
    {
        int y = LeftShift(12, 3);
        if (y == 96) return true;
        else return false;
    }

    #endregion

    #region LngConv

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int LngConv(long x, out int y) { return y = (int) x; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static UInt32 LngConv(long x, out UInt32 y) { return y = (UInt32) x; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static UInt16 LngConv(long x, out UInt16 y) { return y = (UInt16)x; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static byte LngConv(long x, out byte y) { return y = (byte)x; }

    //[MethodImplAttribute(MethodImplOptions.NoInlining)]
    //public static UInt16 LngConv(byte x) { return (UInt16)x; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static Int16 LngConv(long x, out Int16 y) { return y = (Int16)x; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static sbyte LngConv(long x, out sbyte y) { return y = (sbyte)x; }

    // TODO: why does this have IntPtr in it 
    // [MethodImplAttribute(MethodImplOptions.NoInlining)]
    // public static uint LngConv() 
    // {
    //     uint num6 = (uint)((IntPtr)0x4234abcdL);
    //     if (num6 != 0x4234abcd)
    //     {
    //         return 1;
    //     }
    //     return num6;
    // }

    public static bool TestLngConv()
    {
        int a;
        UInt32 b;
        Int16 c;
        UInt16 d;
        sbyte e;
        byte f;
        long x = 3294168832L;

        // LngConv();

        LngConv(x, out a);
        if (a != -1000798464) return false;

        LngConv(x, out b);
        if (b != 3294168832U) return false;

        LngConv(x, out c);
        if (c != 1792) return false;

        LngConv(x, out d);
        if (d != 1792) return false;

        LngConv(x, out e);
        if (e != 0) return false;

        LngConv(x, out f);
        if (f != 0) return false;

        return true;
    }

    #endregion
    
    // TODO: LocalLoc

    #region LongArgsAndReturn

    // Returns max of two longs

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static long LongArgsAndReturn(long a, long b)
    {
        return a>b ? a : b;
    }


    public static bool TestLongArgsAndReturn()
    {
        long m = LongArgsAndReturn(10L, 20L);
        if (m != 20L) return false;
        return true;
    }

    #endregion

    #region Lt1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static bool Lt1(int x)
    {
        return x < 1;
    }

    public static bool TestLt1()
    {
        bool y = Lt1(1);
        if (y == false) return true;
        else return false;
    }

    #endregion
    
    // TODO: ModConst

    #region Mul1

    private static bool TestMul1()
    {
        int result = Mul1(3, 7);
        if (result == 21)
            return true;
        else
            return false;
    }

    [MethodImpl(MethodImplOptions.NoInlining)] 
    public static int Mul1(int a, int b)
    {
        return a*b;
    }

    #endregion

    #region Mul2

    private static bool TestMul2()
    {
        int result = Mul2(3);
        if (result == 15)
            return true;
        else
            return false;
    }

    [MethodImpl(MethodImplOptions.NoInlining)] 
    public static int Mul2(int a)
    {
        return a*5;
    }

    #endregion

    #region Mul3

    private static bool TestMul3()
    {
        int result= Mul3(3);
        if (result == 15369)
            return true;
        else
            return false;
    }

    [MethodImpl(MethodImplOptions.NoInlining)] 
    public static int Mul3(int a)
    {
        return a*5123;
    }

    #endregion

    #region Mul4

    private static bool TestMul4()
    {
        int a = 2;
        int result = Mul4(ref a);
        if (result == 2076)
            return true;
        else
            return false;
		
    }

    [MethodImpl(MethodImplOptions.NoInlining)] 
    public static int Mul4(ref int a)
    {
        return a*1038;
    }

    #endregion

    #region Ne1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static bool Ne1(int x)
    {
        return x != 1;
    }

    public static bool TestNe1()
    {
        bool y = Ne1(1);
        if (y == false) return true;
        else return false;
    }

    #endregion

    #region NegRMW

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void NegRMW(ref int x) { x = -x; }

    public static bool TestNegRMW()
    {
        int x = 12;
        NegRMW(ref x);
        if (x == -12) return true;
        else return false;
    }

    #endregion

    #region NestedCalls

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int NestedCall(int x)
    {
        return x * x;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int NestedCall(int a, int b)
    {
        int c = NestedCall(NestedCall(a)) + NestedCall(NestedCall(b));
        return c;
    }

    public static bool TestNestedCalls()
    {
        int y = NestedCall(2, 3);
        if (y == 97) return true;
        else return false;
    }

    #endregion

    #region NotAndNeg

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int NotAndNeg(int x, int y) { return -x ^ ~y; }

    public static bool TestNotAndNeg()
    {
        int y = NotAndNeg(1, 0);
        if (y == 0) return true;
        else return false;
    }

    #endregion

    #region NotRMW

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void NotRMW(ref int x) { x = ~x; }

    public static bool TestNotRMW()
    {
        int x = -1;
        NotRMW(ref x);
        if (x == 0) return true;
        else return false;
    }

    #endregion

    #region ObjAlloc

    public class Point2
    {
        int x;
        int y;

        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        public Point2(int a, int b) { x=a; y = b; }

        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        public bool IsOrigin() { return (x==0 && y == 0); }

        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        public int DistanceSquared(Point2 p) { return (x-p.x)*(x-p.x) + (y-p.y)*(y-p.y); }
    }
    
    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static Point2 ObjAlloc()
    {
        Point2 p1 = new Point2(10,20);
        Point2 p2 = new Point2(10,20);

        int d = p1.DistanceSquared(p2);
        if (d != 0) return null;
        
        return new Point2(0,0);
    }


    public static bool TestObjAlloc()
    {
        Point2 obj = ObjAlloc();
        if (obj == null) return false;
        return true;
    }

    #endregion
    
    // TODO: OpMemberOfSTructLocal

    #region Or1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int Or1(int x) { return x | 0xa; }

    public static bool TestOr1()
    {
        int y = Or1(4);
        if (y == 14) return true;
        else return false;
    }

    #endregion

    #region OrRef

    private static bool TestOrRef()
    {
        int x = 13;
        int result = OrRef(15, ref x);

        if (result == 15)
            return true;
        else
            return false;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int OrRef(int x, ref int a)
    {
        x |= a;
        return x;
    }

    #endregion
    
    // TODO: RecursiveTailCall

    #region Rem1

    private static bool TestRem1()
    {
        int result = Rem1(12, 5);
        if (result == 2)
            return true;
        else
            return false;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int Rem1(int a, int b)
    {
        return a % b;
    }

    #endregion

    #region RightShiftRef

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void RightShiftRef(ref int x, int y) { x >>= y; }

    public static bool TestRightShiftRef()
    {
        int x = 36;
        RightShiftRef(ref x, 3);
        if (x == 4) return true;
        else return false;
    }

    #endregion
    
    // TODO: rotate

    #region Shift

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static ulong shl64(ulong shift, int count)
    {
        return shift << count;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static ulong shl64_32_inplace(ulong shift, ulong addit)
    {
        ulong x = shift + addit;
        x = x << 32;
        return x;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static ulong shl64_33_inplace(ulong shift, ulong addit)
    {
        ulong x = shift + addit;
        x = x << 33;
        return x;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static ulong shr64(ulong shift, int count)
    {
        return shift >> count;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static ulong shr64_32_inplace(ulong shift, ulong addit)
    {
        ulong x = shift + addit;
        x = x >> 32;
        return x;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static ulong shr1_32_add(ulong shift, ulong addit)
    {
        ulong x = (addit + (shift >> 1)) >> 31;
        return x;
    }

    public static bool TestShift()
    {
        if (shl64_32_inplace(0x123456789abcdef, 0) != shl64(0x123456789abcdef, 32))
        {
            return false;
        }

        if (shl64_33_inplace(0x123456789abcdef, 0) != shl64(0x123456789abcdef, 33))
        {
            return false;
        }

        if (shr64_32_inplace(0x123456789abcdef, 0) != shr64(0x123456789abcdef, 32))
        {
            return false;
        }

        if (shr1_32_add(0x123456789abcdef, 0) != shr64(0x123456789abcdef, 32))
        {
            return false;
        }

        return true;
    }

    #endregion

    // TODO: StaticCalls
    
    // TODO: StaticValueField
    
    // TODO: Struct16Args

    #region StructFldAddr

    public struct Rational
    {
        public int num;
        public int den;
    }

    public struct RationalPolynomial
    {
        public Rational a;
        public Rational b;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int StructFldAddr(RationalPolynomial rp)
    {
        return rp.a.num + rp.b.num;
    }

    public static bool TestStructFldAddr()
    {
        Rational a = new Rational();
        Rational b = new Rational();
        a.num = 3;
        a.den = 4;
        b.num = 2;
        b.den = 3;
        RationalPolynomial rp = new RationalPolynomial();
        rp.a = a;
        rp.b = b;
        int y = StructFldAddr(rp);
        if (y == 5) return true;
        else return false;
    }

    #endregion

    #region StructInstMethod

    public struct Point
    {
        public int x;
        public int y;
        public int z;

        [MethodImpl(MethodImplOptions.NoInlining)]
        public Point(int a, int b, int c) { x = a; y = b; z = c; }

        public int X
        {
            [MethodImpl(MethodImplOptions.NoInlining)]
            get { return this.x; }

            [MethodImpl(MethodImplOptions.NoInlining)]
            set { this.x = value; }
        }

        public int Y
        {
            [MethodImpl(MethodImplOptions.NoInlining)]
            get { return this.y; }

            [MethodImpl(MethodImplOptions.NoInlining)]
            set { this.y = value; }
        }

        public int Z
        {
            [MethodImpl(MethodImplOptions.NoInlining)]
            get { return this.z; }

            [MethodImpl(MethodImplOptions.NoInlining)]
            set { this.z = value; }
        }

        // Returns true if this represents 'origin' otherwise false.
        [MethodImpl(MethodImplOptions.NoInlining)]
        public bool StructInstMethod() { return (x == 0 && y == 0 && z == 0); }

        [MethodImpl(MethodImplOptions.NoInlining)]
        public int StructInstMethod(ref Point p)
        {
            int a = X;
            return a + p.x;
        }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static bool StructInstMethod(ref Point p2)
    {
        Point p1 = new Point(10, 20, 30);

        p1.StructInstMethod();

        if (p1.StructInstMethod()) return false;
        if (!p2.StructInstMethod()) return false;

        int a = p1.StructInstMethod(ref p2);
        int b = p1.X;
        if (a != b) return false;

        return true;
    }

    public static bool TestStructInstMethod()
    {
        Point p = new Point(10, 20, 30);
        if (p.StructInstMethod()) return false;

        if (p.StructInstMethod(ref p) != 20) return false;

        Point p2 = new Point(0, 0, 0);
        return StructInstMethod(ref p2);
    }

    #endregion
    
    // TODO: StructRreturn

    #region Sub1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int Sub1(int x) { return x - 1; }

    public static bool TestSub1()
    {
        int y = Sub1(1);
        if (y == 0) return true;
        else return false;
    }

    #endregion

    #region SubRef

    private static bool TestSubRef()
    {
        int x = 13;
        int result = SubRef(15, ref x);

        if (result == 2)
            return true;
        else
            return false;
    }

    [MethodImpl(MethodImplOptions.NoInlining)] 
    public static int SubRef(int x, ref int a)
    {
        x -= a;
        return x;
    }

    #endregion

    #region Swap

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void Swap(ref int a, ref int b)
    {
        int t = a;
        a = b;
        b = t;
    }

    public static bool TestSwap()
    {
        int a = 10, b= 20;
        Swap(ref a, ref b);
        if (a==20 && b== 10) return true;
        return false;
    }

    #endregion
    
    // TODO: switch
    
    // TODO: UDivConst
    
    // TODO: UModConst

    #region Unbox

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int Unbox(object o)
    {
        return (int)o;
    }

    public static bool TestUnbox()
    {
        int r = 3;
        object o = r;
        int y = Unbox(o);
        if (y == 3) return true;
        else return false;
    }
    
    #endregion

    #region Xor1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int Xor1(int x) { return x ^ 15; }

    public static bool TestXor1()
    {
        int y = Xor1(13);
        if (y == 2) return true;
        else return false;
    }

    #endregion

    #region XorRef

    private static bool TestXorRef()
    {
        int x = 13;
        int result = XorRef(15, ref x);

        if (result == 2)
            return true;
        else
            return false;
    }

    [MethodImpl(MethodImplOptions.NoInlining)] 
    public static int XorRef(int x, ref int a)
    {
        x ^= a;
        return x;
    }

    #endregion

    public static bool Run()
    {
        if (!TestAdd1()) return false;
        if (!TestAddref()) return false;
        if (!TestAnd1()) return false;
        if (!TestAndref()) return false;
        if (!TestArgs4()) return false;
        if (!TestAsgAdd1()) return false;
        if (!TestAsgAnd1()) return false;
        if (!TestAsgOr1()) return false;
        if (!TestAsgSub1()) return false;
        if (!TestAsgXor1()) return false;
        if (!TestBinaryRMW()) return false;
        if (!TestCnsBool()) return false;
        if (!TestCnsLng1()) return false;
        if (!TestDiv1()) return false;
        if (!TestDivRef()) return false;
        if (!TestEq1()) return false;
        if (!TestFactorialRec()) return false;
        if (!TestFibLoop()) return false;
        if (!TestFiboRec()) return false;
        if (!TestGcd()) return false;
        if (!TestGe1()) return false;
        if (!TestGt1()) return false;
        if (!TestInd1()) return false;
        if (!TestInitObj()) return false;
        if (!TestIntConv()) return false;
        if (!TestJmp1()) return false;
        if (!TestJTrue1()) return false;
        if (!TestJTrueEqInt1()) return false;
        if (!TestJTrueGeInt1()) return false;
        if (!TestJTrueGtInt1()) return false;
        if (!TestJTrueLeInt1()) return false;
        if (!TestJTrueLtInt1()) return false;
        if (!TestJTrueNeInt1()) return false;
        if (!TestLe1()) return false;
        if (!TestLeftShift()) return false;
        if (!TestLngConv()) return false;
        if (!TestLongArgsAndReturn()) return false;
        if (!TestLt1()) return false;
        if (!TestMul1()) return false;
        if (!TestMul2()) return false;
        if (!TestMul3()) return false;
        if (!TestMul4()) return false;
        if (!TestNe1()) return false;
        if (!TestNegRMW()) return false;
        if (!TestNestedCalls()) return false;
        if (!TestNotAndNeg()) return false;
        if (!TestNotRMW()) return false;
        if (!TestObjAlloc()) return false;
        if (!TestOr1()) return false;
        if (!TestOrRef()) return false;
        if (!TestRem1()) return false;
        if (!TestRightShiftRef()) return false;
        if (!TestShift()) return false;
        if (!TestStructFldAddr()) return false;
        if (!TestStructInstMethod()) return false;
        if (!TestSub1()) return false;
        if (!TestSubRef()) return false;
        if (!TestSwap()) return false;
        if (!TestUnbox()) return false;
        if (!TestXor1()) return false;
        if (!TestXorRef()) return false;
        
        return true;
    }
}