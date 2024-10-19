using System;
using System.Runtime.CompilerServices;

namespace Tests;

public class CodeGenBringUpTests
{
    #region Add1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int Add1(int x)
    {
        return x + 1;
    }

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
    public static int And1(int x)
    {
        return x & 1;
    }

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
        return a + b + c + d;
    }

    public static bool TestArgs4()
    {
        int y = Args4(1, 2, 3, 4);
        if (y == 10) return true;
        else return false;
    }

    #endregion

    #region Args5

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int Args5(int a, int b, int c, int d, int e)
    {
        return a + b + c + d + e;
    }

    public static bool TestArgs5()
    {
        int y = Args5(1, 2, 3, 4, 5);
        if (y == 15) return true;
        else return false;
    }

    #endregion

    #region Array1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    static void Array1(int[] a)
    {
        a[1] = 5;
    }

    static bool TestArray1()
    {
        int[] a = { 1, 2, 3, 4 };
        Array1(a);

        if (a[1] != 5) return false;
        return true;
    }

    #endregion

    #region Array2

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    static int Array2(int[] a)
    {
        return a[1];
    }

    static bool TestArray2()
    {
        int[] a = { 1, 2, 3, 4 };
        if (Array2(a) != 2) return false;
        return true;
    }

    #endregion

    #region Array3

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    static int Array3()
    {
        int[] a = { 1, 2, 3, 4 };
        a[1] = 5;
        return a[1];
    }

    static bool TestArray3()
    {
        if (Array3() != 5) return false;
        return true;
    }

    #endregion

    #region Array4

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    static int Array4(int i)
    {
        int[] a = { 1, 2, 3, 4 };
        return a[i];
    }

    static bool TestArray4()
    {
        if (Array4(1) != 2) return false;
        return true;
    }

    #endregion

    // #region ArrayExc
    //
    // [MethodImplAttribute(MethodImplOptions.NoInlining)]
    // static int ArrayExc()
    // {
    //     int[] a = {1, 2, 3, 4};
    //     return a[5];
    // }
    //
    // static bool TestArrayExc()
    // {
    //     try
    //     {
    //         ArrayExc();
    //         return false;
    //     }
    //     catch (System.IndexOutOfRangeException)
    //     {
    //         // OK
    //     }
    //     return true;
    // }
    //
    // #endregion

    #region ArrayJagged

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    static int ArrayJagged(int i)
    {
        int[][] a = new int[2][];
        a[0] = new int[2] { 0, 1 };
        a[1] = new int[2] { 2, 3 };
        return a[1][i];
    }

    static bool TestArrayJagged()
    {
        if (ArrayJagged(1) != 3) return false;
        return true;
    }

    #endregion

    // TODO: ArrayMD

    #region ArrayObj

    class Dummy
    {
        public int field;

        public Dummy(int f)
        {
            field = f;
        }
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    static int ArrayObj(int i)
    {
        Dummy[] a =
        {
            new Dummy(0), new Dummy(1), new Dummy(2), new Dummy(3), new Dummy(4),
            new Dummy(5), new Dummy(6), new Dummy(7), new Dummy(8), new Dummy(9)
        };
        return a[i].field;
    }

    static bool TestArrayObj()
    {
        if (ArrayObj(1) != 1) return false;
        return true;
    }

    #endregion

    #region AsgAdd1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int AsgAdd1(int x)
    {
        x += 1;
        return x;
    }

    public static bool TestAsgAdd1()
    {
        if (AsgAdd1(0) == 1) return true;
        else return false;
    }

    #endregion

    #region AsgAnd1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int AsgAnd1(int x)
    {
        x &= 3;
        return x;
    }

    public static bool TestAsgAnd1()
    {
        if (AsgAnd1(0xf) == 3) return true;
        else return false;
    }

    #endregion

    #region AsgOr1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int AsgOr1(int x)
    {
        x |= 0xa;
        return x;
    }

    public static bool TestAsgOr1()
    {
        if (AsgOr1(4) == 0xe) return true;
        else return false;
    }

    #endregion

    #region AsgSub1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int AsgSub1(int x)
    {
        x -= 1;
        return x;
    }

    public static bool TestAsgSub1()
    {
        if (AsgSub1(1) == 0) return true;
        else return false;
    }

    #endregion

    #region AsgXor1

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int AsgXor1(int x)
    {
        x ^= 0xf;
        return x;
    }

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

    #region Call1

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static void M()
    {
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static void Call1()
    {
        M();
    }

    public static bool TestCall1()
    {
        Call1();
        return true;
    }

    #endregion

    #region CastThenBinop

    // TODO: CastThenBinop

    #endregion

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
    public static long CnsLng1()
    {
        return 1;
    }

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
            result = Gcd(b, a % b);

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
    public static void Ind1(ref int x)
    {
        x = 1;
        return;
    }

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

    #region IntArraySum

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int IntArraySum(int[] a, int n)
    {
        int sum = 0;
        for (int i = 0; i < n; ++i)
            sum += a[i];
        return sum;
    }


    public static bool TestIntArraySum()
    {
        int[] a = new int[5] { 1, 2, 3, 4, 5 };
        int result = IntArraySum(a, a.Length);
        if (result == 15) return true;
        return false;
    }

    #endregion

    #region IntConv

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static long IntConv(int x)
    {
        return (long)x;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static long IntConv(UInt32 x)
    {
        return (long)x;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int IntConv(UInt16 x)
    {
        return (int)x;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int IntConv(byte x)
    {
        return (int)x;
    }

    //[MethodImplAttribute(MethodImplOptions.NoInlining)]
    //public static UInt16 IntConv(byte x) { return (UInt16)x; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static byte IntConv(Int16 x)
    {
        return (byte)x;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static UInt32 IntConv(long x)
    {
        return (UInt32)x;
    }

    public static bool TestIntConv()
    {
        long x = IntConv((int)3);
        if (x != 3) return false;

        x = IntConv((UInt32)3294168832);
        if (x != 3294168832L) return false;

        int z = IntConv((UInt16)123);
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
        x = x + 1;
        goto L3;
        L1:
        x = x + 1;
        goto L2;
        L3:
        return x + 1;
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
            return x + 1;
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

        if (JTrueEqInt1(int.MinValue) != 1) returnValue = false;
        if (JTrueEqInt1(int.MinValue + 1) != 0) returnValue = false;
        if (JTrueEqInt1(-1) != 2) returnValue = false;
        if (JTrueEqInt1(0) != 3) returnValue = false;
        if (JTrueEqInt1(1) != 4) returnValue = false;
        if (JTrueEqInt1(int.MaxValue - 1) != 0) returnValue = false;
        if (JTrueEqInt1(int.MaxValue) != 5) returnValue = false;

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

        if (x >= int.MaxValue) returnValue = 7;
        else if (x >= 2) returnValue = 6;
        else if (x >= 1) returnValue = 5;
        else if (x >= 0) returnValue = 4;
        else if (x >= -1) returnValue = 3;
        else if (x >= (int.MinValue + 1)) returnValue = 2;
        else if (x >= int.MinValue) returnValue = 1;

        return returnValue;
    }

    public static bool TestJTrueGeInt1()
    {
        bool returnValue = true;

        if (JTrueGeInt1(int.MinValue) != 1) returnValue = false;
        if (JTrueGeInt1(int.MinValue + 1) != 2) returnValue = false;
        if (JTrueGeInt1(-1) != 3) returnValue = false;
        if (JTrueGeInt1(0) != 4) returnValue = false;
        if (JTrueGeInt1(1) != 5) returnValue = false;
        if (JTrueGeInt1(int.MaxValue - 1) != 6) returnValue = false;
        if (JTrueGeInt1(int.MaxValue) != 7) returnValue = false;

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

        if (x > int.MaxValue) returnValue = 0; // Never true
        else if (x > (int.MaxValue - 1)) returnValue = 7;
        else if (x > 1) returnValue = 6;
        else if (x > 0) returnValue = 5;
        else if (x > -1) returnValue = 4;
        else if (x > (int.MinValue + 1)) returnValue = 3;
        else if (x > int.MinValue) returnValue = 2;
        else returnValue = 1;

        return returnValue;
    }

    public static bool TestJTrueGtInt1()
    {
        bool returnValue = true;

        if (JTrueGtInt1(int.MinValue) != 1) returnValue = false;
        if (JTrueGtInt1(int.MinValue + 1) != 2) returnValue = false;
        if (JTrueGtInt1(-1) != 3) returnValue = false;
        if (JTrueGtInt1(0) != 4) returnValue = false;
        if (JTrueGtInt1(1) != 5) returnValue = false;
        if (JTrueGtInt1(int.MaxValue - 1) != 6) returnValue = false;
        if (JTrueGtInt1(int.MaxValue) != 7) returnValue = false;

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

        if (x <= int.MinValue) returnValue = 1;
        else if (x <= -2) returnValue = 2;
        else if (x <= -1) returnValue = 3;
        else if (x <= 0) returnValue = 4;
        else if (x <= 1) returnValue = 5;
        else if (x <= (int.MaxValue - 1)) returnValue = 6;
        else if (x <= int.MaxValue) returnValue = 7;

        return returnValue;
    }

    public static bool TestJTrueLeInt1()
    {
        bool returnValue = true;

        if (JTrueLeInt1(int.MinValue) != 1) returnValue = false;
        if (JTrueLeInt1(int.MinValue + 1) != 2) returnValue = false;
        if (JTrueLeInt1(-1) != 3) returnValue = false;
        if (JTrueLeInt1(0) != 4) returnValue = false;
        if (JTrueLeInt1(1) != 5) returnValue = false;
        if (JTrueLeInt1(int.MaxValue - 1) != 6) returnValue = false;
        if (JTrueLeInt1(int.MaxValue) != 7) returnValue = false;

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

        if (x < int.MinValue) returnValue = 0; // Never true
        else if (x < (int.MinValue + 1)) returnValue = 1;
        else if (x < -1) returnValue = 2;
        else if (x < 0) returnValue = 3;
        else if (x < 1) returnValue = 4;
        else if (x < (int.MaxValue - 1)) returnValue = 5;
        else if (x < int.MaxValue) returnValue = 6;
        else returnValue = 7;

        return returnValue;
    }

    public static bool TestJTrueLtInt1()
    {
        bool returnValue = true;

        if (JTrueLtInt1(int.MinValue) != 1) returnValue = false;
        if (JTrueLtInt1(int.MinValue + 1) != 2) returnValue = false;
        if (JTrueLtInt1(-1) != 3) returnValue = false;
        if (JTrueLtInt1(0) != 4) returnValue = false;
        if (JTrueLtInt1(1) != 5) returnValue = false;
        if (JTrueLtInt1(int.MaxValue - 1) != 6) returnValue = false;
        if (JTrueLtInt1(int.MaxValue) != 7) returnValue = false;

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

        if (JTrueNeInt1(int.MinValue) != 1) returnValue = false;
        if (JTrueNeInt1(int.MinValue + 1) != 0) returnValue = false;
        if (JTrueNeInt1(-1) != 2) returnValue = false;
        if (JTrueNeInt1(0) != 3) returnValue = false;
        if (JTrueNeInt1(1) != 4) returnValue = false;
        if (JTrueNeInt1(int.MaxValue - 1) != 0) returnValue = false;
        if (JTrueNeInt1(int.MaxValue) != 5) returnValue = false;

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
    public static int LeftShift(int x, int y)
    {
        return x << y;
    }

    public static bool TestLeftShift()
    {
        int y = LeftShift(12, 3);
        if (y == 96) return true;
        else return false;
    }

    #endregion

    #region LngConv

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int LngConv(long x, out int y)
    {
        return y = (int)x;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static UInt32 LngConv(long x, out UInt32 y)
    {
        return y = (UInt32)x;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static UInt16 LngConv(long x, out UInt16 y)
    {
        return y = (UInt16)x;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static byte LngConv(long x, out byte y)
    {
        return y = (byte)x;
    }

    //[MethodImplAttribute(MethodImplOptions.NoInlining)]
    //public static UInt16 LngConv(byte x) { return (UInt16)x; }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static Int16 LngConv(long x, out Int16 y)
    {
        return y = (Int16)x;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static sbyte LngConv(long x, out sbyte y)
    {
        return y = (sbyte)x;
    }

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
        return a > b ? a : b;
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
        return a * b;
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
        return a * 5;
    }

    #endregion

    #region Mul3

    private static bool TestMul3()
    {
        int result = Mul3(3);
        if (result == 15369)
            return true;
        else
            return false;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static int Mul3(int a)
    {
        return a * 5123;
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
        return a * 1038;
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
    public static void NegRMW(ref int x)
    {
        x = -x;
    }

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
    public static int NotAndNeg(int x, int y)
    {
        return -x ^ ~y;
    }

    public static bool TestNotAndNeg()
    {
        int y = NotAndNeg(1, 0);
        if (y == 0) return true;
        else return false;
    }

    #endregion

    #region NotRMW

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void NotRMW(ref int x)
    {
        x = ~x;
    }

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
        public Point2(int a, int b)
        {
            x = a;
            y = b;
        }

        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        public bool IsOrigin()
        {
            return (x == 0 && y == 0);
        }

        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        public int DistanceSquared(Point2 p)
        {
            return (x - p.x) * (x - p.x) + (y - p.y) * (y - p.y);
        }
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static Point2 ObjAlloc()
    {
        Point2 p1 = new Point2(10, 20);
        Point2 p2 = new Point2(10, 20);

        int d = p1.DistanceSquared(p2);
        if (d != 0) return null;

        return new Point2(0, 0);
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
    public static int Or1(int x)
    {
        return x | 0xa;
    }

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
    public static void RightShiftRef(ref int x, int y)
    {
        x >>= y;
    }

    public static bool TestRightShiftRef()
    {
        int x = 36;
        RightShiftRef(ref x, 3);
        if (x == 4) return true;
        else return false;
    }

    #endregion

    #region Rotate

    public class Test_Rotate
    {
        static ulong s_field;

        ulong field;

        volatile uint volatile_field;

        ushort usfield;

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint rol32(uint value, int amount)
        {
            return (value << amount) | (value >> (32 - amount));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint rol32_1(uint value)
        {
            return (value << 1) | (value >> (32 - 1));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint rol32_3(uint value)
        {
            return (value << 3) | (value >> (32 - 3));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint rol32comm(uint value, int amount)
        {
            return (value >> (32 - amount)) | (value << amount);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static bool flag()
        {
            return true;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint rol32const()
        {
            uint value = flag() ? (uint)0x12345678 : (uint)0x12345678;
            int amount = 16;
            return (value >> (32 - amount)) | (value << amount);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint rol32xor(uint value, int amount)
        {
            return (value << amount) ^ (value >> (32 - amount));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint ror32(uint value, int amount)
        {
            return (value << ((32 - amount))) | (value >> amount);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint ror32comm(uint value, int amount)
        {
            return (value >> amount) | (value << ((32 - amount)));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint ror32const()
        {
            uint value = flag() ? (uint)0x12345678 : (uint)0x12345678;
            int amount = flag() ? 12 : 12;
            return (value >> amount) | (value << ((32 - amount)));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        uint ror32vfield(int amount)
        {
            return (volatile_field << ((32 - amount))) | (volatile_field >> amount);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong rol64(ulong value, int amount)
        {
            return (value << amount) | (value >> (64 - amount));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong rol64comm(ulong value, int amount)
        {
            return (value >> (64 - amount)) | (value << amount);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong rol64const()
        {
            ulong value = flag() ? (ulong)0x123456789abcdef : (ulong)0xabcdef123456789;
            int amount = 16;
            return (value >> (64 - amount)) | (value << amount);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong rol64_16(ulong value)
        {
            return (value >> (64 - 16)) | (value << 16);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong rol64_32(ulong value)
        {
            return (value >> (64 - 32)) | (value << 32);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong rol64_32_inplace(ulong value, ulong added)
        {
            ulong x = value + added;
            x = (x >> (64 - 32)) | (x << 32);
            return x;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong rol64_33(ulong value)
        {
            return (value >> (64 - 33)) | (value << 33);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        ulong rol64field(int amount)
        {
            return (field << amount) | (field >> (64 - amount));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong ror64(ulong value, int amount)
        {
            return (value << (64 - amount)) | (value >> amount);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong ror64comm(ulong value, int amount)
        {
            return (value >> amount) | (value << (64 - amount));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong ror64const()
        {
            ulong value = flag() ? (ulong)0x123456789abcdef : (ulong)0xabcdef123456789;
            int amount = flag() ? 5 : 5;
            return (value << (64 - amount)) | (value >> amount);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong ror64_5(ulong value)
        {
            return (value << (64 - 5)) | (value >> 5);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong ror64_32(ulong value)
        {
            return (value << (64 - 32)) | (value >> 32);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong ror64_33(ulong value)
        {
            return (value << (64 - 33)) | (value >> 33);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong ror64_32_inplace(ulong value, ulong added)
        {
            ulong x = value + added;
            x = (x << (64 - 32)) | (x >> 32);
            return x;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static ulong ror64sfield(int amount)
        {
            return (s_field << (64 - amount)) | (s_field >> amount);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint rol32_call(uint value, int amount)
        {
            return (foo(value) << amount) | (foo(value) >> (32 - amount));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint foo(uint value)
        {
            return value;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint rol32_and(uint value, int amount)
        {
            return (value << amount) | (value >> ((32 - amount) & 31));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint two_left_shifts(uint value, int amount)
        {
            return (value << amount) | (value << (32 - amount));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static uint not_rotation(uint value)
        {
            return (value >> 10) | (value << 5);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        uint rol32ushort(int amount)
        {
            return ((uint)usfield << amount) | ((uint)usfield >> (32 - amount));
        }

        Test_Rotate(ulong i, uint j, ushort k)
        {
            field = i;
            volatile_field = j;
            usfield = k;
        }

        public static bool Test()
        {
            s_field = 0x123456789abcdef;

            if (rol32(0x12345678, 16) != 0x56781234)
            {
                return false;
            }

            if (rol32_1(0x12345678) != 0x2468ACF0)
            {
                return false;
            }

            if (rol32_3(0x12345678) != 0x91A2B3C0)
            {
                return false;
            }

            if (rol32comm(0x12345678, 16) != 0x56781234)
            {
                return false;
            }

            if (rol32const() != 0x56781234)
            {
                return false;
            }

            if (ror32(0x12345678, 12) != 0x67812345)
            {
                return false;
            }

            if (ror32comm(0x12345678, 12) != 0x67812345)
            {
                return false;
            }

            if (ror32const() != 0x67812345)
            {
                return false;
            }

            if (rol64(0x123456789abcdef, 32) != 0x89abcdef01234567)
            {
                return false;
            }

            if (rol64comm(0x123456789abcdef, 32) != 0x89abcdef01234567)
            {
                return false;
            }

            if (rol64const() != 0x456789abcdef0123)
            {
                return false;
            }

            if (rol64_16(0x123456789abcdef) != 0x456789abcdef0123)
            {
                return false;
            }

            if (rol64_32(0x123456789abcdef) != rol64(0x123456789abcdef, 32))
            {
                return false;
            }

            if (rol64_33(0x123456789abcdef) != rol64(0x123456789abcdef, 33))
            {
                return false;
            }

            if (rol64_32_inplace(0x123456789abcdef, 0) != rol64(0x123456789abcdef, 32))
            {
                return false;
            }

            if (ror64(0x123456789abcdef, 0) != 0x123456789abcdef)
            {
                return false;
            }

            if (ror64comm(0x123456789abcdef, 0) != 0x123456789abcdef)
            {
                return false;
            }

            if (ror64const() != 0x78091a2b3c4d5e6f)
            {
                return false;
            }

            if (ror64_5(0x123456789abcdef) != 0x78091a2b3c4d5e6f)
            {
                return false;
            }

            if (ror64_32(0x123456789abcdef) != ror64(0x123456789abcdef, 32))
            {
                return false;
            }

            if (ror64_33(0x123456789abcdef) != ror64(0x123456789abcdef, 33))
            {
                return false;
            }

            if (ror64_32_inplace(0x123456789abcdef, 0) != ror64(0x123456789abcdef, 32))
            {
                return false;
            }

            if (rol32_call(0x12345678, 16) != 0x56781234)
            {
                return false;
            }

            if (rol32_and(0x12345678, 16) != 0x56781234)
            {
                return false;
            }

            if (two_left_shifts(0x12345678, 7) != 0xfa2b3c00)
            {
                return false;
            }

            if (not_rotation(0x87654321) != 0xeca9fd70)
            {
                return false;
            }

            if (rol32xor(0x12345678, 16) != 0x56781234)
            {
                return false;
            }

            if (ror64sfield(7) != 0xde02468acf13579b)
            {
                return false;
            }

            Test_Rotate test = new Test_Rotate(0x123456789abcdef, 0x12345678, 0x1234);

            if (test.rol64field(11) != 0x1a2b3c4d5e6f7809)
            {
                return false;
            }

            if (test.ror32vfield(3) != 0x2468acf)
            {
                return false;
            }

            if (test.rol32ushort(25) != 0x68000024)
            {
                return false;
            }

            return true;
        }
    }

    #endregion

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

    #region StaticClass

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int Max(int a, int b)
    {
        int result = a > b ? a : b;
        return result;
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static bool IsLessThan(int a, int b)
    {        
        return a<b;
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static bool IsEqual(int a, int b)
    {        
        return !IsLessThan(a, b) && !IsLessThan(b, a);
    }

    // [MethodImplAttribute(MethodImplOptions.NoInlining)]
    // public static bool IsEqual(float a, float b)
    // {        
    //     return System.Math.Abs(a-b) <= Single.Epsilon;
    // }
    //
    // [MethodImplAttribute(MethodImplOptions.NoInlining)]
    // public static bool IsEqual(double a, double b)
    // {        
    //     return System.Math.Abs(a-b) <= Double.Epsilon;
    // }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int Sum(int a, int b, int c, int d)
    {
        int result = a+b+c+d;
        return result;
    }

    // [MethodImplAttribute(MethodImplOptions.NoInlining)]
    // public static float Sum(float a, float b, float c, float d)
    // {
    //     float result = a+b+c+d;
    //     return result;
    // }
    //
    // [MethodImplAttribute(MethodImplOptions.NoInlining)]
    // public static double Sum(double a, double b, double c, double d)
    // {
    //     double result = a+b+c+d;
    //     return result;
    // }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int Sum(int a, int b, int c, int d, int e)
    {
        int result = a+b+c+d+e;
        return result;
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static int Sum(int a, int b, int c, int d, int e, int f)
    {
        int result = a+b+c+d+e+f;
        return result;
    }

    // [MethodImplAttribute(MethodImplOptions.NoInlining)]
    // public static float Sum(float a, float b, float c, float d, float e, float f)
    // {
    //     float result = a+b+c+d+e+f;
    //     return result;
    // }
    //
    // [MethodImplAttribute(MethodImplOptions.NoInlining)]
    // public static double Sum(double a, double b, double c, double d, double e, double f)
    // {
    //     double result = a+b+c+d+e+f;
    //     return result;
    // }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static void Print(int s)
    {
    }

    [MethodImplAttribute(MethodImplOptions.NoInlining)]
    public static bool TestStaticCalls()
    {
        int a = 1;
        int b = 2;
        int c = 3;
        int d = 4;
        int e = 5;
        int f = 6;
        bool result = true;

        int s = Sum(1,2,3,4);
        if (s != 10) result = false;
        
        s = Sum(1,2,3,4,5);
        if (s != 15) result = false;

        s = Sum(1,2,3,4,5,6);
        if (s != 21) result = false;

        s = Sum(a,b,c,d);
        if (s != 10) result = false;

        s = Sum(a,b,c,d,e);
        if (s != 15) result = false;

        s = Sum(a,b,c,d,e,f);
        if (s != 21) result = false;

        s = Max(b,f);
        if (s != f) result = false;


        bool equal = IsEqual(d, d);
        if (!equal) result = false;


        // float f1 = 1f;
        // float f2 = 2f;
        // float f3 = 3f;
        // float f4 = 4f;
        // float f5 = 5f;
        // float f6 = 6f;
        // float fsum = Sum(1f,2f,3f,4f);
        // if (!IsEqual(fsum, 10f)) result = false;
        //
        // fsum = Sum(1f, 2f, 3f, 4f, 5f, 6f);
        // if (!IsEqual(fsum, 21f)) result = false;                 
        //
        // fsum = Sum(f1,f2,f3,f4);
        // if (!IsEqual(fsum, 10f)) result = false;
        //
        // fsum = Sum(f1,f2,f3,f4, f5, f6);
        // if (!IsEqual(fsum, 21f)) result = false;


        // double d1 = 1d;
        // double d2 = 2d;
        // double d3 = 3d;
        // double d4 = 4d;
        // double d5 = 5d;
        // double d6 = 6d;
        // double dsum = Sum(1d,2d,3d,4d);
        // if (!IsEqual(dsum, 10d)) result = false;
        //
        // dsum = Sum(1d, 2d, 3d, 4d, 5d, 6d);
        // if (!IsEqual(dsum, 21d)) result = false;                 
        //
        // dsum = Sum(d1,d2,d3,d4);
        // if (!IsEqual(dsum, 10d)) result = false;
        //
        // dsum = Sum(d1,d2,d3,d4,d5,d6);
        // if (!IsEqual(dsum, 21d)) result = false;

        return result;
    }
    
    #endregion

    #region StaticValueField

    struct TestValue
    {
        public int a;
        public short b;
        public long c;
    }

    static TestValue sField;

    public static void Init()
    {
        TestValue v = new TestValue();
        v.a = 100;
        v.b = 200;
        v.c = 300;
        sField = v;
    }

    public static bool TestStaticValueField()
    {
        Init();
        if (sField.a == 100
            && sField.b == 200
            && sField.c == 300)
        {
            return true;
        }

        return false;
    }

    #endregion

    public class TestStruct16Args
    {
        
        public struct Point
        {
          public int w;
          public int x;
          public int y;
          public int z;
        
          [MethodImplAttribute(MethodImplOptions.NoInlining)]
          public Point(int a, int b, int c, int d) { w=a; x=a; y=b; z=d; }
        
          public int W
          {
             [MethodImplAttribute(MethodImplOptions.NoInlining)]
             get { return this.w; }
        
             [MethodImplAttribute(MethodImplOptions.NoInlining)]
             set { this.w = value; }
          }
        
          public int X
          {
             [MethodImplAttribute(MethodImplOptions.NoInlining)]
             get { return this.x; }
        
             [MethodImplAttribute(MethodImplOptions.NoInlining)]
             set { this.x = value; }
          }
        
          public int Y
          {
             [MethodImplAttribute(MethodImplOptions.NoInlining)]
             get { return this.y; }
        
             [MethodImplAttribute(MethodImplOptions.NoInlining)]
             set { this.y = value; }
          }
        
          public int Z
          {
             [MethodImplAttribute(MethodImplOptions.NoInlining)]
             get { return this.z; }
        
             [MethodImplAttribute(MethodImplOptions.NoInlining)]
             set { this.z = value; }
          }
        
          // Returns true if this represents 'origin' otherwise false.
          [MethodImplAttribute(MethodImplOptions.NoInlining)]
          public bool StructInstMethod() { return (x==0 && y == 0 && z==0); }
        
        }
        
        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        public static bool method_4S(Point p0, Point p1, Point p2, Point p3)
        {
            if (p0.W != 0)
                return false;
            
            if (p0.X != 0)
                return false;
            
            if (p0.Y != 0)
                return false;
            
            if (p0.Z != 0)
                return false;

            if (p1.W != 1)
                return false;
            
            if (p1.X != 1)
                return false;
            
            if (p1.Y != 1)
                return false;
            
            if (p1.Z != 1)
                return false;
            
            if (p2.W != 9)
                return false;
            
            if (p2.X != 99)
                return false;
            
            if (p2.Y != 999)
                return false;
            
            if (p2.Z != 9999)
                return false;
            
            if (p3.W != 10)
                return false;
            
            if (p3.X != 100)
                return false;
            
            if (p3.Y != 1000)
                return false;
            
            if (p3.Z != 10000)
                return false;
            
            return true;
        }

        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        public static bool method_4S4I(Point p0, Point p1, Point p2, Point p3, int i0, int i1, int i2, int i3)
        {
            if (i0 != 2)
                return false;

            if (i1 != 3)
                return false;

            if (i2 != 5)
                return false;

            if (i3 != 7)
                return false;

            if (p0.W != 0)
                return false;

            if (p0.X != 0)
                return false;

            if (p0.Y != 0)
                return false;

            if (p0.Z != 0)
                return false;

            if (p1.W != 1)
                return false;

            if (p1.X != 1)
                return false;

            if (p1.Y != 1)
                return false;

            if (p1.Z != 1)
                return false;

            if (p2.W != 9)
                return false;

            if (p2.X != 99)
                return false;

            if (p2.Y != 999)
                return false;

            if (p2.Z != 9999)
                return false;

            if (p3.W != 10)
                return false;

            if (p3.X != 100)
                return false;

            if (p3.Y != 1000)
                return false;

            if (p3.Z != 10000)
                return false;
            
            return true;
        }

        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        public static bool method_4I4S(int i0, int i1, int i2, int i3, Point p0, Point p1, Point p2, Point p3)
        {
            if (i0 != 2)
                return false;

            if (i1 != 3)
                return false;

            if (i2 != 5)
                return false;

            if (i3 != 7)
                return false;

            if (p0.W != 0)
                return false;

            if (p0.X != 0)
                return false;

            if (p0.Y != 0)
                return false;

            if (p0.Z != 0)
                return false;

            if (p1.W != 1)
                return false;

            if (p1.X != 1)
                return false;

            if (p1.Y != 1)
                return false;

            if (p1.Z != 1)
                return false;

            if (p2.W != 9)
                return false;

            if (p2.X != 99)
                return false;

            if (p2.Y != 999)
                return false;

            if (p2.Z != 9999)
                return false;

            if (p3.W != 10)
                return false;

            if (p3.X != 100)
                return false;

            if (p3.Y != 1000)
                return false;

            if (p3.Z != 10000)
                return false;
            
            return true;
        }

        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        public static bool method_1I4S(int i0, Point p0, Point p1, Point p2, Point p3)
        {
            if (i0 != 2)
                return false;

            if (p0.W != 0)
                return false;

            if (p0.X != 0)
                return false;

            if (p0.Y != 0)
                return false;

            if (p0.Z != 0)
                return false;

            if (p1.W != 1)
                return false;

            if (p1.X != 1)
                return false;

            if (p1.Y != 1)
                return false;

            if (p1.Z != 1)
                return false;

            if (p2.W != 9)
                return false;

            if (p2.X != 99)
                return false;

            if (p2.Y != 999)
                return false;

            if (p2.Z != 9999)
                return false;

            if (p3.W != 10)
                return false;

            if (p3.X != 100)
                return false;

            if (p3.Y != 1000)
                return false;

            if (p3.Z != 10000)
                return false;
            
            return true;
        }

        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        public static bool method_2I4S(int i0, int i1, Point p0, Point p1, Point p2, Point p3)
        {
            if (i0 != 2)
                return false;

            if (i1 != 3)
                return false;

            if (p0.W != 0)
                return false;

            if (p0.X != 0)
                return false;

            if (p0.Y != 0)
                return false;

            if (p0.Z != 0)
                return false;

            if (p1.W != 1)
                return false;

            if (p1.X != 1)
                return false;

            if (p1.Y != 1)
                return false;

            if (p1.Z != 1)
                return false;

            if (p2.W != 9)
                return false;

            if (p2.X != 99)
                return false;

            if (p2.Y != 999)
                return false;

            if (p2.Z != 9999)
                return false;

            if (p3.W != 10)
                return false;

            if (p3.X != 100)
                return false;

            if (p3.Y != 1000)
                return false;

            if (p3.Z != 10000)
                return false;
            
            return true;
        }

        [MethodImplAttribute(MethodImplOptions.NoInlining)]
        public static bool method_3I4S(int i0, int i1, int i2, Point p0, Point p1, Point p2, Point p3)
        {
            if (i0 != 2)
                return false;

            if (i1 != 3)
                return false;

            if (i2 != 5)
                return false;

            if (p0.W != 0)
                return false;

            if (p0.X != 0)
                return false;

            if (p0.Y != 0)
                return false;

            if (p0.Z != 0)
                return false;

            if (p1.W != 1)
                return false;

            if (p1.X != 1)
                return false;

            if (p1.Y != 1)
                return false;

            if (p1.Z != 1)
                return false;

            if (p2.W != 9)
                return false;

            if (p2.X != 99)
                return false;

            if (p2.Y != 999)
                return false;

            if (p2.Z != 9999)
                return false;

            if (p3.W != 10)
                return false;

            if (p3.X != 100)
                return false;

            if (p3.Y != 1000)
                return false;

            if (p3.Z != 10000)
                return false;
            
            return true;
        }

        // [MethodImplAttribute(MethodImplOptions.NoInlining)]
        // public static bool method_2I4S2D(int i0, int i1, Point p0, Point p1, Point p2, Point p3, double d0, double d1)
        // {
        //     Console.Write("method_2I4S2D");
        //
        //     if (i0 != 2)
        //         return false;
        //
        //     if (i1 != 3)
        //         return false;
        //
        //     if (d0 != 11.0d)
        //         return false;
        //
        //     if (d1 != 13.0d)
        //         return false;
        //
        //     if (p0.W != 0)
        //         return false;
        //
        //     if (p0.X != 0)
        //         return false;
        //
        //     if (p0.Y != 0)
        //         return false;
        //
        //     if (p0.Z != 0)
        //         return false;
        //
        //     if (p1.W != 1)
        //         return false;
        //
        //     if (p1.X != 1)
        //         return false;
        //
        //     if (p1.Y != 1)
        //         return false;
        //
        //     if (p1.Z != 1)
        //         return false;
        //
        //     if (p2.W != 9)
        //         return false;
        //
        //     if (p2.X != 99)
        //         return false;
        //
        //     if (p2.Y != 999)
        //         return false;
        //
        //     if (p2.Z != 9999)
        //         return false;
        //
        //     if (p3.W != 10)
        //         return false;
        //
        //     if (p3.X != 100)
        //         return false;
        //
        //     if (p3.Y != 1000)
        //         return false;
        //
        //     if (p3.Z != 10000)
        //         return false;
        //
        //     Console.WriteLine(" true");
        //
        //     return true;
        // }

        // [MethodImplAttribute(MethodImplOptions.NoInlining)]
        // public static bool method_2I2D4S(int i0, int i1, double d0, double d1, Point p0, Point p1, Point p2, Point p3)
        // {
        //     Console.Write("method_2I2D4S");
        //
        //     if (i0 != 2)
        //         return false;
        //
        //     if (i1 != 3)
        //         return false;
        //
        //     if (d0 != 11.0d)
        //         return false;
        //
        //     if (d1 != 13.0d)
        //         return false;
        //
        //     if (p0.W != 0)
        //         return false;
        //
        //     if (p0.X != 0)
        //         return false;
        //
        //     if (p0.Y != 0)
        //         return false;
        //
        //     if (p0.Z != 0)
        //         return false;
        //
        //     if (p1.W != 1)
        //         return false;
        //
        //     if (p1.X != 1)
        //         return false;
        //
        //     if (p1.Y != 1)
        //         return false;
        //
        //     if (p1.Z != 1)
        //         return false;
        //
        //     if (p2.W != 9)
        //         return false;
        //
        //     if (p2.X != 99)
        //         return false;
        //
        //     if (p2.Y != 999)
        //         return false;
        //
        //     if (p2.Z != 9999)
        //         return false;
        //
        //     if (p3.W != 10)
        //         return false;
        //
        //     if (p3.X != 100)
        //         return false;
        //
        //     if (p3.Y != 1000)
        //         return false;
        //
        //     if (p3.Z != 10000)
        //         return false;
        //
        //     Console.WriteLine(" true");
        //
        //     return true;
        // }

        // [MethodImplAttribute(MethodImplOptions.NoInlining)]
        // public static bool method_2I2D4S2D(int i0, int i1, double d0, double d1, Point p0, Point p1, Point p2, Point p3, double d2, double d3)
        // {
        //     Console.Write("method_2I2D4S2D");
        //
        //     if (i0 != 2)
        //         return false;
        //
        //     if (i1 != 3)
        //         return false;
        //
        //     if (d0 != 11.0d)
        //         return false;
        //
        //     if (d1 != 13.0d)
        //         return false;
        //
        //     if (d2 != 15.0d)
        //         return false;
        //
        //     if (d3 != 17.0d)
        //         return false;
        //
        //     if (p0.W != 0)
        //         return false;
        //
        //     if (p0.X != 0)
        //         return false;
        //
        //     if (p0.Y != 0)
        //         return false;
        //
        //     if (p0.Z != 0)
        //         return false;
        //
        //     if (p1.W != 1)
        //         return false;
        //
        //     if (p1.X != 1)
        //         return false;
        //
        //     if (p1.Y != 1)
        //         return false;
        //
        //     if (p1.Z != 1)
        //         return false;
        //
        //     if (p2.W != 9)
        //         return false;
        //
        //     if (p2.X != 99)
        //         return false;
        //
        //     if (p2.Y != 999)
        //         return false;
        //
        //     if (p2.Z != 9999)
        //         return false;
        //
        //     if (p3.W != 10)
        //         return false;
        //
        //     if (p3.X != 100)
        //         return false;
        //
        //     if (p3.Y != 1000)
        //         return false;
        //
        //     if (p3.Z != 10000)
        //         return false;
        //
        //     Console.WriteLine(" true");
        //
        //     return true;
        // }

        public static bool Run()
        {       
           int i0 = 2;
           int i1 = 3;
           int i2 = 5;
           int i3 = 7;

           // double d0 = 11.0d;
           // double d1 = 13.0d;
           // double d2 = 15.0d;
           // double d3 = 17.0d;

           Point p0;
           Point p1;
           Point p2;
           Point p3;

           p0.w = 0;
           p0.x = 0;
           p0.y = 0;
           p0.z = 0;

           p1.w = 1;
           p1.x = 1;
           p1.y = 1;
           p1.z = 1;

           p2.w = 9;
           p2.x = 99;
           p2.y = 999;
           p2.z = 9999;

           p3.w = 10;
           p3.x = 100;
           p3.y = 1000;
           p3.z = 10000;

           if (method_4S(p0,p1,p2,p3) != true)
               return false;

           if (method_4S4I(p0,p1,p2,p3, i0,i1,i2,i3) != true)
               return false;

           if (method_4I4S(i0,i1,i2,i3, p0,p1,p2,p3) != true)
               return false;

           if (method_1I4S(i0, p0,p1,p2,p3) != true)
               return false;

           if (method_2I4S(i0,i1, p0,p1,p2,p3) != true)
               return false;

           if (method_3I4S(i0,i1,i2, p0,p1,p2,p3) != true)
               return false;

           // if (method_2I4S2D(i0,i1, p0,p1,p2,p3, d0,d1) != true)
           //     return false;
           //
           // if (method_2I2D4S(i0,i1, d0,d1, p0,p1,p2,p3) != true)
           //     return false;
           //
           // if (method_2I2D4S2D(i0,i1, d0,d1, p0,p1,p2,p3, d2,d3) != true)
           //     return false;

           return true;
        }
    }

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
        public Point(int a, int b, int c)
        {
            x = a;
            y = b;
            z = c;
        }

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
        public bool StructInstMethod()
        {
            return (x == 0 && y == 0 && z == 0);
        }

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
    public static int Sub1(int x)
    {
        return x - 1;
    }

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
        int a = 10, b = 20;
        Swap(ref a, ref b);
        if (a == 20 && b == 10) return true;
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
    public static int Xor1(int x)
    {
        return x ^ 15;
    }

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

    public static int Run()
    {
        if (!TestAdd1()) return 1;
        if (!TestAddref()) return 2;
        if (!TestAnd1()) return 3;
        if (!TestAndref()) return 4;
        if (!TestArgs4()) return 5;
        if (!TestArgs5()) return 6;
        if (!TestArray1()) return 7;
        if (!TestArray2()) return 8;
        if (!TestArray3()) return 9;
        if (!TestArray4()) return 10;
        // if (!TestArrayExc()) return 11;
        if (!TestArrayJagged()) return 12;
        if (!TestArrayObj()) return 13;
        if (!TestAsgAdd1()) return 14;
        if (!TestAsgAnd1()) return 15;
        if (!TestAsgOr1()) return 16;
        if (!TestAsgSub1()) return 17;
        if (!TestAsgXor1()) return 18;
        if (!TestBinaryRMW()) return 19;
        if (!TestCall1()) return 20;
        if (!TestCnsBool()) return 21;
        if (!TestCnsLng1()) return 22;
        if (!TestDiv1()) return 23;
        if (!TestDivRef()) return 24;
        if (!TestEq1()) return 25;
        if (!TestFactorialRec()) return 26;
        if (!TestFibLoop()) return 27;
        if (!TestFiboRec()) return 28;
        if (!TestGcd()) return 29;
        if (!TestGe1()) return 30;
        if (!TestGt1()) return 31;
        if (!TestInd1()) return 32;
        if (!TestInitObj()) return 33;
        if (!TestIntArraySum()) return 34;
        if (!TestIntConv()) return 35;
        if (!TestJmp1()) return 36;
        if (!TestJTrue1()) return 37;
        if (!TestJTrueEqInt1()) return 38;
        if (!TestJTrueGeInt1()) return 39;
        if (!TestJTrueGtInt1()) return 40;
        if (!TestJTrueLeInt1()) return 41;
        if (!TestJTrueLtInt1()) return 42;
        if (!TestJTrueNeInt1()) return 43;
        if (!TestLe1()) return 44;
        if (!TestLeftShift()) return 45;
        if (!TestLngConv()) return 46;
        if (!TestLongArgsAndReturn()) return 47;
        if (!TestLt1()) return 48;
        if (!TestMul1()) return 49;
        if (!TestMul2()) return 50;
        if (!TestMul3()) return 51;
        if (!TestMul4()) return 52;
        if (!TestNe1()) return 53;
        if (!TestNegRMW()) return 54;
        if (!TestNestedCalls()) return 55;
        if (!TestNotAndNeg()) return 56;
        if (!TestNotRMW()) return 57;
        if (!TestObjAlloc()) return 58;
        if (!TestOr1()) return 59;
        if (!TestOrRef()) return 60;
        if (!TestRem1()) return 61;
        if (!TestRightShiftRef()) return 62;
        if (!Test_Rotate.Test()) return 63;
        if (!TestShift()) return 64;
        if (!TestStaticCalls()) return 65;
        if (!TestStaticValueField()) return 66;
        if (!TestStruct16Args.Run()) return 67;
        if (!TestStructFldAddr()) return 68;
        if (!TestStructInstMethod()) return 69;
        if (!TestSub1()) return 70;
        if (!TestSubRef()) return 71;
        if (!TestSwap()) return 72;
        if (!TestUnbox()) return 73;
        if (!TestXor1()) return 74;
        if (!TestXorRef()) return 75;

        return 0;
    }
}