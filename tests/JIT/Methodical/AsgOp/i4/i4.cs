// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System;

namespace Test_i4_cs
{
public class test
{
    private static int f00(int x, int y)
    {
        x = x + y;
        return x;
    }

    private static int f01(int x, int y)
    {
        x = x - y;
        return x;
    }

    private static int f02(int x, int y)
    {
        x = x * y;
        return x;
    }

    private static int f03(int x, int y)
    {
        x = x / y;
        return x;
    }

    private static int f04(int x, int y)
    {
        x = x % y;
        return x;
    }

    private static int f05(int x, int y)
    {
        x = x << y;
        return x;
    }

    private static int f06(int x, int y)
    {
        x = x >> y;
        return x;
    }

    private static int f07(int x, int y)
    {
        x = x & y;
        return x;
    }

    private static int f08(int x, int y)
    {
        x = x ^ y;
        return x;
    }

    private static int f09(int x, int y)
    {
        x = x | y;
        return x;
    }

    private static int f10(int x, int y)
    {
        x += x + y;
        return x;
    }

    private static int f11(int x, int y)
    {
        x += x - y;
        return x;
    }

    private static int f12(int x, int y)
    {
        x += x * y;
        return x;
    }

    private static int f13(int x, int y)
    {
        x += x / y;
        return x;
    }

    private static int f14(int x, int y)
    {
        x += x % y;
        return x;
    }

    private static int f15(int x, int y)
    {
        x += x << y;
        return x;
    }

    private static int f16(int x, int y)
    {
        x += x >> y;
        return x;
    }

    private static int f17(int x, int y)
    {
        x += x & y;
        return x;
    }

    private static int f18(int x, int y)
    {
        x += x ^ y;
        return x;
    }

    private static int f19(int x, int y)
    {
        x += x | y;
        return x;
    }

    private static int f20(int x, int y)
    {
        x -= x + y;
        return x;
    }

    private static int f21(int x, int y)
    {
        x -= x - y;
        return x;
    }

    private static int f22(int x, int y)
    {
        x -= x * y;
        return x;
    }

    private static int f23(int x, int y)
    {
        x -= x / y;
        return x;
    }

    private static int f24(int x, int y)
    {
        x -= x % y;
        return x;
    }

    private static int f25(int x, int y)
    {
        x -= x << y;
        return x;
    }

    private static int f26(int x, int y)
    {
        x -= x >> y;
        return x;
    }

    private static int f27(int x, int y)
    {
        x -= x & y;
        return x;
    }

    private static int f28(int x, int y)
    {
        x -= x ^ y;
        return x;
    }

    private static int f29(int x, int y)
    {
        x -= x | y;
        return x;
    }

    private static int f30(int x, int y)
    {
        x *= x + y;
        return x;
    }

    private static int f31(int x, int y)
    {
        x *= x - y;
        return x;
    }

    private static int f32(int x, int y)
    {
        x *= x * y;
        return x;
    }

    private static int f33(int x, int y)
    {
        x *= x / y;
        return x;
    }

    private static int f34(int x, int y)
    {
        x *= x % y;
        return x;
    }

    private static int f35(int x, int y)
    {
        x *= x << y;
        return x;
    }

    private static int f36(int x, int y)
    {
        x *= x >> y;
        return x;
    }

    private static int f37(int x, int y)
    {
        x *= x & y;
        return x;
    }

    private static int f38(int x, int y)
    {
        x *= x ^ y;
        return x;
    }

    private static int f39(int x, int y)
    {
        x *= x | y;
        return x;
    }

    private static int f40(int x, int y)
    {
        x /= x + y;
        return x;
    }

    private static int f41(int x, int y)
    {
        x /= x - y;
        return x;
    }

    private static int f42(int x, int y)
    {
        x /= x * y;
        return x;
    }

    private static int f43(int x, int y)
    {
        x /= x / y;
        return x;
    }

    private static int f44(int x, int y)
    {
        x /= x % y;
        return x;
    }

    private static int f45(int x, int y)
    {
        x /= x << y;
        return x;
    }

    private static int f46(int x, int y)
    {
        x /= x >> y;
        return x;
    }

    private static int f47(int x, int y)
    {
        x /= x & y;
        return x;
    }

    private static int f48(int x, int y)
    {
        x /= x ^ y;
        return x;
    }

    private static int f49(int x, int y)
    {
        x /= x | y;
        return x;
    }

    private static int f50(int x, int y)
    {
        x %= x + y;
        return x;
    }

    private static int f51(int x, int y)
    {
        x %= x - y;
        return x;
    }

    private static int f52(int x, int y)
    {
        x %= x * y;
        return x;
    }

    private static int f53(int x, int y)
    {
        x %= x / y;
        return x;
    }

    private static int f54(int x, int y)
    {
        x %= x % y;
        return x;
    }

    private static int f55(int x, int y)
    {
        x %= x << y;
        return x;
    }

    private static int f56(int x, int y)
    {
        x %= x >> y;
        return x;
    }

    private static int f57(int x, int y)
    {
        x %= x & y;
        return x;
    }

    private static int f58(int x, int y)
    {
        x %= x ^ y;
        return x;
    }

    private static int f59(int x, int y)
    {
        x %= x | y;
        return x;
    }

    private static int f60(int x, int y)
    {
        x <<= x + y;
        return x;
    }

    private static int f61(int x, int y)
    {
        x <<= x - y;
        return x;
    }

    private static int f62(int x, int y)
    {
        x <<= x * y;
        return x;
    }

    private static int f63(int x, int y)
    {
        x <<= x / y;
        return x;
    }

    private static int f64(int x, int y)
    {
        x <<= x % y;
        return x;
    }

    private static int f65(int x, int y)
    {
        x <<= x << y;
        return x;
    }

    private static int f66(int x, int y)
    {
        x <<= x >> y;
        return x;
    }

    private static int f67(int x, int y)
    {
        x <<= x & y;
        return x;
    }

    private static int f68(int x, int y)
    {
        x <<= x ^ y;
        return x;
    }

    private static int f69(int x, int y)
    {
        x <<= x | y;
        return x;
    }

    private static int f70(int x, int y)
    {
        x >>= x + y;
        return x;
    }

    private static int f71(int x, int y)
    {
        x >>= x - y;
        return x;
    }

    private static int f72(int x, int y)
    {
        x >>= x * y;
        return x;
    }

    private static int f73(int x, int y)
    {
        x >>= x / y;
        return x;
    }

    private static int f74(int x, int y)
    {
        x >>= x % y;
        return x;
    }

    private static int f75(int x, int y)
    {
        x >>= x << y;
        return x;
    }

    private static int f76(int x, int y)
    {
        x >>= x >> y;
        return x;
    }

    private static int f77(int x, int y)
    {
        x >>= x & y;
        return x;
    }

    private static int f78(int x, int y)
    {
        x >>= x ^ y;
        return x;
    }

    private static int f79(int x, int y)
    {
        x >>= x | y;
        return x;
    }

    private static int f80(int x, int y)
    {
        x &= x + y;
        return x;
    }

    private static int f81(int x, int y)
    {
        x &= x - y;
        return x;
    }

    private static int f82(int x, int y)
    {
        x &= x * y;
        return x;
    }

    private static int f83(int x, int y)
    {
        x &= x / y;
        return x;
    }

    private static int f84(int x, int y)
    {
        x &= x % y;
        return x;
    }

    private static int f85(int x, int y)
    {
        x &= x << y;
        return x;
    }

    private static int f86(int x, int y)
    {
        x &= x >> y;
        return x;
    }

    private static int f87(int x, int y)
    {
        x &= x & y;
        return x;
    }

    private static int f88(int x, int y)
    {
        x &= x ^ y;
        return x;
    }

    private static int f89(int x, int y)
    {
        x &= x | y;
        return x;
    }

    private static int f90(int x, int y)
    {
        x ^= x + y;
        return x;
    }

    private static int f91(int x, int y)
    {
        x ^= x - y;
        return x;
    }

    private static int f92(int x, int y)
    {
        x ^= x * y;
        return x;
    }

    private static int f93(int x, int y)
    {
        x ^= x / y;
        return x;
    }

    private static int f94(int x, int y)
    {
        x ^= x % y;
        return x;
    }

    private static int f95(int x, int y)
    {
        x ^= x << y;
        return x;
    }

    private static int f96(int x, int y)
    {
        x ^= x >> y;
        return x;
    }

    private static int f97(int x, int y)
    {
        x ^= x & y;
        return x;
    }

    private static int f98(int x, int y)
    {
        x ^= x ^ y;
        return x;
    }

    private static int f99(int x, int y)
    {
        x ^= x | y;
        return x;
    }

    private static int f100(int x, int y)
    {
        x |= x + y;
        return x;
    }

    private static int f101(int x, int y)
    {
        x |= x - y;
        return x;
    }

    private static int f102(int x, int y)
    {
        x |= x * y;
        return x;
    }

    private static int f103(int x, int y)
    {
        x |= x / y;
        return x;
    }

    private static int f104(int x, int y)
    {
        x |= x % y;
        return x;
    }

    private static int f105(int x, int y)
    {
        x |= x << y;
        return x;
    }

    private static int f106(int x, int y)
    {
        x |= x >> y;
        return x;
    }

    private static int f107(int x, int y)
    {
        x |= x & y;
        return x;
    }

    private static int f108(int x, int y)
    {
        x |= x ^ y;
        return x;
    }

    private static int f109(int x, int y)
    {
        x |= x | y;
        return x;
    }


    public static int Main()
    {
        int x;
        bool pass = true;

        x = f00(-10, 4);
        if (x != -6)
        {
            pass = false;
        }

        x = f01(-10, 4);
        if (x != -14)
        {
            pass = false;
        }

        x = f02(-10, 4);
        if (x != -40)
        {
            pass = false;
        }

        x = f03(-10, 4);
        if (x != -2)
        {
            pass = false;
        }

        x = f04(-10, 4);
        if (x != -2)
        {
            pass = false;
        }

        x = f05(-10, 4);
        if (x != -160)
        {
            pass = false;
        }

        x = f06(-10, 4);
        if (x != -1)
        {
            pass = false;
        }

        x = f07(-10, 4);
        if (x != 4)
        {
            pass = false;
        }

        x = f08(-10, 4);
        if (x != -14)
        {
            pass = false;
        }

        x = f09(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f10(-10, 4);
        if (x != -16)
        {
            pass = false;
        }

        x = f11(-10, 4);
        if (x != -24)
        {
            pass = false;
        }

        x = f12(-10, 4);
        if (x != -50)
        {
            pass = false;
        }

        x = f13(-10, 4);
        if (x != -12)
        {
            pass = false;
        }

        x = f14(-10, 4);
        if (x != -12)
        {
            pass = false;
        }

        x = f15(-10, 4);
        if (x != -170)
        {
            pass = false;
        }

        x = f16(-10, 4);
        if (x != -11)
        {
            pass = false;
        }

        x = f17(-10, 4);
        if (x != -6)
        {
            pass = false;
        }

        x = f18(-10, 4);
        if (x != -24)
        {
            pass = false;
        }

        x = f19(-10, 4);
        if (x != -20)
        {
            pass = false;
        }

        x = f20(-10, 4);
        if (x != -4)
        {
            pass = false;
        }

        x = f21(-10, 4);
        if (x != 4)
        {
            pass = false;
        }

        x = f22(-10, 4);
        if (x != 30)
        {
            pass = false;
        }

        x = f23(-10, 4);
        if (x != -8)
        {
            pass = false;
        }

        x = f24(-10, 4);
        if (x != -8)
        {
            pass = false;
        }

        x = f25(-10, 4);
        if (x != 150)
        {
            pass = false;
        }

        x = f26(-10, 4);
        if (x != -9)
        {
            pass = false;
        }

        x = f27(-10, 4);
        if (x != -14)
        {
            pass = false;
        }

        x = f28(-10, 4);
        if (x != 4)
        {
            pass = false;
        }

        x = f29(-10, 4);
        if (x != 0)
        {
            pass = false;
        }

        x = f30(-10, 4);
        if (x != 60)
        {
            pass = false;
        }

        x = f31(-10, 4);
        if (x != 140)
        {
            pass = false;
        }

        x = f32(-10, 4);
        if (x != 400)
        {
            pass = false;
        }

        x = f33(-10, 4);
        if (x != 20)
        {
            pass = false;
        }

        x = f34(-10, 4);
        if (x != 20)
        {
            pass = false;
        }

        x = f35(-10, 4);
        if (x != 1600)
        {
            pass = false;
        }

        x = f36(-10, 4);
        if (x != 10)
        {
            pass = false;
        }

        x = f37(-10, 4);
        if (x != -40)
        {
            pass = false;
        }

        x = f38(-10, 4);
        if (x != 140)
        {
            pass = false;
        }

        x = f39(-10, 4);
        if (x != 100)
        {
            pass = false;
        }

        x = f40(-10, 4);
        if (x != 1)
        {
            pass = false;
        }

        x = f41(-10, 4);
        if (x != 0)
        {
            pass = false;
        }

        x = f42(-10, 4);
        if (x != 0)
        {
            pass = false;
        }

        x = f43(-10, 4);
        if (x != 5)
        {
            pass = false;
        }

        x = f44(-10, 4);
        if (x != 5)
        {
            pass = false;
        }

        x = f45(-10, 4);
        if (x != 0)
        {
            pass = false;
        }

        x = f46(-10, 4);
        if (x != 10)
        {
            pass = false;
        }

        x = f47(-10, 4);
        if (x != -2)
        {
            pass = false;
        }

        x = f48(-10, 4);
        if (x != 0)
        {
            pass = false;
        }

        x = f49(-10, 4);
        if (x != 1)
        {
            pass = false;
        }

        x = f50(-10, 4);
        if (x != -4)
        {
            pass = false;
        }

        x = f51(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f52(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f53(-10, 4);
        if (x != 0)
        {
            pass = false;
        }

        x = f54(-10, 4);
        if (x != 0)
        {
            pass = false;
        }

        x = f55(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f56(-10, 4);
        if (x != 0)
        {
            pass = false;
        }

        x = f57(-10, 4);
        if (x != -2)
        {
            pass = false;
        }

        x = f58(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f59(-10, 4);
        if (x != 0)
        {
            pass = false;
        }

        x = f60(-10, 4);
        if (x != -671088640)
        {
            pass = false;
        }

        x = f61(-10, 4);
        if (x != -2621440)
        {
            pass = false;
        }

        x = f62(-10, 4);
        if (x != -167772160)
        {
            pass = false;
        }

        x = f63(-10, 4);
        if (x != -2147483648)
        {
            pass = false;
        }

        x = f64(-10, 4);
        if (x != -2147483648)
        {
            pass = false;
        }

        x = f65(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f66(-10, 4);
        if (x != 0)
        {
            pass = false;
        }

        x = f67(-10, 4);
        if (x != -160)
        {
            pass = false;
        }

        x = f68(-10, 4);
        if (x != -2621440)
        {
            pass = false;
        }

        x = f69(-10, 4);
        if (x != -41943040)
        {
            pass = false;
        }

        x = f70(-10, 4);
        if (x != -1)
        {
            pass = false;
        }

        x = f71(-10, 4);
        if (x != -1)
        {
            pass = false;
        }

        x = f72(-10, 4);
        if (x != -1)
        {
            pass = false;
        }

        x = f73(-10, 4);
        if (x != -1)
        {
            pass = false;
        }

        x = f74(-10, 4);
        if (x != -1)
        {
            pass = false;
        }

        x = f75(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f76(-10, 4);
        if (x != -1)
        {
            pass = false;
        }

        x = f77(-10, 4);
        if (x != -1)
        {
            pass = false;
        }

        x = f78(-10, 4);
        if (x != -1)
        {
            pass = false;
        }

        x = f79(-10, 4);
        if (x != -1)
        {
            pass = false;
        }

        x = f80(-10, 4);
        if (x != -14)
        {
            pass = false;
        }

        x = f81(-10, 4);
        if (x != -14)
        {
            pass = false;
        }

        x = f82(-10, 4);
        if (x != -48)
        {
            pass = false;
        }

        x = f83(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f84(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f85(-10, 4);
        if (x != -160)
        {
            pass = false;
        }

        x = f86(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f87(-10, 4);
        if (x != 4)
        {
            pass = false;
        }

        x = f88(-10, 4);
        if (x != -14)
        {
            pass = false;
        }

        x = f89(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f90(-10, 4);
        if (x != 12)
        {
            pass = false;
        }

        x = f91(-10, 4);
        if (x != 4)
        {
            pass = false;
        }

        x = f92(-10, 4);
        if (x != 46)
        {
            pass = false;
        }

        x = f93(-10, 4);
        if (x != 8)
        {
            pass = false;
        }

        x = f94(-10, 4);
        if (x != 8)
        {
            pass = false;
        }

        x = f95(-10, 4);
        if (x != 150)
        {
            pass = false;
        }

        x = f96(-10, 4);
        if (x != 9)
        {
            pass = false;
        }

        x = f97(-10, 4);
        if (x != -14)
        {
            pass = false;
        }

        x = f98(-10, 4);
        if (x != 4)
        {
            pass = false;
        }

        x = f99(-10, 4);
        if (x != 0)
        {
            pass = false;
        }

        x = f100(-10, 4);
        if (x != -2)
        {
            pass = false;
        }

        x = f101(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f102(-10, 4);
        if (x != -2)
        {
            pass = false;
        }

        x = f103(-10, 4);
        if (x != -2)
        {
            pass = false;
        }

        x = f104(-10, 4);
        if (x != -2)
        {
            pass = false;
        }

        x = f105(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f106(-10, 4);
        if (x != -1)
        {
            pass = false;
        }

        x = f107(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f108(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        x = f109(-10, 4);
        if (x != -10)
        {
            pass = false;
        }

        if (pass)
        {
            return 100;
        }
        else
            return 1;
    }
}
}
