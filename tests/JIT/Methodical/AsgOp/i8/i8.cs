// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System;

namespace Test_i8_cs
{
public class test
{
    private static Int64 f00(Int64 x, Int64 y)
    {
        x = x + y;
        return x;
    }

    private static Int64 f01(Int64 x, Int64 y)
    {
        x = x - y;
        return x;
    }

    private static Int64 f02(Int64 x, Int64 y)
    {
        x = x * y;
        return x;
    }

    private static Int64 f03(Int64 x, Int64 y)
    {
        x = x / y;
        return x;
    }

    private static Int64 f04(Int64 x, Int64 y)
    {
        x = x % y;
        return x;
    }

    private static Int64 f05(Int64 x, Int64 y)
    {
        x = x << (int)y;
        return x;
    }

    private static Int64 f06(Int64 x, Int64 y)
    {
        x = x >> (int)y;
        return x;
    }

    private static Int64 f07(Int64 x, Int64 y)
    {
        x = x & y;
        return x;
    }

    private static Int64 f08(Int64 x, Int64 y)
    {
        x = x ^ y;
        return x;
    }

    private static Int64 f09(Int64 x, Int64 y)
    {
        x = x | y;
        return x;
    }

    private static Int64 f10(Int64 x, Int64 y)
    {
        x += x + y;
        return x;
    }

    private static Int64 f11(Int64 x, Int64 y)
    {
        x += x - y;
        return x;
    }

    private static Int64 f12(Int64 x, Int64 y)
    {
        x += x * y;
        return x;
    }

    private static Int64 f13(Int64 x, Int64 y)
    {
        x += x / y;
        return x;
    }

    private static Int64 f14(Int64 x, Int64 y)
    {
        x += x % y;
        return x;
    }

    private static Int64 f15(Int64 x, Int64 y)
    {
        x += x << (int)y;
        return x;
    }

    private static Int64 f16(Int64 x, Int64 y)
    {
        x += x >> (int)y;
        return x;
    }

    private static Int64 f17(Int64 x, Int64 y)
    {
        x += x & y;
        return x;
    }

    private static Int64 f18(Int64 x, Int64 y)
    {
        x += x ^ y;
        return x;
    }

    private static Int64 f19(Int64 x, Int64 y)
    {
        x += x | y;
        return x;
    }

    private static Int64 f20(Int64 x, Int64 y)
    {
        x -= x + y;
        return x;
    }

    private static Int64 f21(Int64 x, Int64 y)
    {
        x -= x - y;
        return x;
    }

    private static Int64 f22(Int64 x, Int64 y)
    {
        x -= x * y;
        return x;
    }

    private static Int64 f23(Int64 x, Int64 y)
    {
        x -= x / y;
        return x;
    }

    private static Int64 f24(Int64 x, Int64 y)
    {
        x -= x % y;
        return x;
    }

    private static Int64 f25(Int64 x, Int64 y)
    {
        x -= x << (int)y;
        return x;
    }

    private static Int64 f26(Int64 x, Int64 y)
    {
        x -= x >> (int)y;
        return x;
    }

    private static Int64 f27(Int64 x, Int64 y)
    {
        x -= x & y;
        return x;
    }

    private static Int64 f28(Int64 x, Int64 y)
    {
        x -= x ^ y;
        return x;
    }

    private static Int64 f29(Int64 x, Int64 y)
    {
        x -= x | y;
        return x;
    }

    private static Int64 f30(Int64 x, Int64 y)
    {
        x *= x + y;
        return x;
    }

    private static Int64 f31(Int64 x, Int64 y)
    {
        x *= x - y;
        return x;
    }

    private static Int64 f32(Int64 x, Int64 y)
    {
        x *= x * y;
        return x;
    }

    private static Int64 f33(Int64 x, Int64 y)
    {
        x *= x / y;
        return x;
    }

    private static Int64 f34(Int64 x, Int64 y)
    {
        x *= x % y;
        return x;
    }

    private static Int64 f35(Int64 x, Int64 y)
    {
        x *= x << (int)y;
        return x;
    }

    private static Int64 f36(Int64 x, Int64 y)
    {
        x *= x >> (int)y;
        return x;
    }

    private static Int64 f37(Int64 x, Int64 y)
    {
        x *= x & y;
        return x;
    }

    private static Int64 f38(Int64 x, Int64 y)
    {
        x *= x ^ y;
        return x;
    }

    private static Int64 f39(Int64 x, Int64 y)
    {
        x *= x | y;
        return x;
    }

    private static Int64 f40(Int64 x, Int64 y)
    {
        x /= x + y;
        return x;
    }

    private static Int64 f41(Int64 x, Int64 y)
    {
        x /= x - y;
        return x;
    }

    private static Int64 f42(Int64 x, Int64 y)
    {
        x /= x * y;
        return x;
    }

    private static Int64 f43(Int64 x, Int64 y)
    {
        x /= x / y;
        return x;
    }

    private static Int64 f44(Int64 x, Int64 y)
    {
        x /= x % y;
        return x;
    }

    private static Int64 f45(Int64 x, Int64 y)
    {
        x /= x << (int)y;
        return x;
    }

    private static Int64 f46(Int64 x, Int64 y)
    {
        x /= x >> (int)y;
        return x;
    }

    private static Int64 f47(Int64 x, Int64 y)
    {
        x /= x & y;
        return x;
    }

    private static Int64 f48(Int64 x, Int64 y)
    {
        x /= x ^ y;
        return x;
    }

    private static Int64 f49(Int64 x, Int64 y)
    {
        x /= x | y;
        return x;
    }

    private static Int64 f50(Int64 x, Int64 y)
    {
        x %= x + y;
        return x;
    }

    private static Int64 f51(Int64 x, Int64 y)
    {
        x %= x - y;
        return x;
    }

    private static Int64 f52(Int64 x, Int64 y)
    {
        x %= x * y;
        return x;
    }

    private static Int64 f53(Int64 x, Int64 y)
    {
        x %= x / y;
        return x;
    }

    private static Int64 f54(Int64 x, Int64 y)
    {
        x %= x % y;
        return x;
    }

    private static Int64 f55(Int64 x, Int64 y)
    {
        x %= x << (int)y;
        return x;
    }

    private static Int64 f56(Int64 x, Int64 y)
    {
        x %= x >> (int)y;
        return x;
    }

    private static Int64 f57(Int64 x, Int64 y)
    {
        x %= x & y;
        return x;
    }

    private static Int64 f58(Int64 x, Int64 y)
    {
        x %= x ^ y;
        return x;
    }

    private static Int64 f59(Int64 x, Int64 y)
    {
        x %= x | y;
        return x;
    }

    private static Int64 f60(Int64 x, Int64 y)
    {
        x <<= (int)(x + y);
        return x;
    }

    private static Int64 f61(Int64 x, Int64 y)
    {
        x <<= (int)(x - y);
        return x;
    }

    private static Int64 f62(Int64 x, Int64 y)
    {
        x <<= (int)(x * y);
        return x;
    }

    private static Int64 f63(Int64 x, Int64 y)
    {
        x <<= (int)(x / y);
        return x;
    }

    private static Int64 f64(Int64 x, Int64 y)
    {
        x <<= (int)(x % y);
        return x;
    }

    private static Int64 f65(Int64 x, Int64 y)
    {
        x <<= (int)(x << (int)y);
        return x;
    }

    private static Int64 f66(Int64 x, Int64 y)
    {
        x <<= (int)(x >> (int)y);
        return x;
    }

    private static Int64 f67(Int64 x, Int64 y)
    {
        x <<= (int)(x & y);
        return x;
    }

    private static Int64 f68(Int64 x, Int64 y)
    {
        x <<= (int)(x ^ y);
        return x;
    }

    private static Int64 f69(Int64 x, Int64 y)
    {
        x <<= (int)(x | y);
        return x;
    }

    private static Int64 f70(Int64 x, Int64 y)
    {
        x >>= (int)(x + y);
        return x;
    }

    private static Int64 f71(Int64 x, Int64 y)
    {
        x >>= (int)(x - y);
        return x;
    }

    private static Int64 f72(Int64 x, Int64 y)
    {
        x >>= (int)(x * y);
        return x;
    }

    private static Int64 f73(Int64 x, Int64 y)
    {
        x >>= (int)(x / y);
        return x;
    }

    private static Int64 f74(Int64 x, Int64 y)
    {
        x >>= (int)(x % y);
        return x;
    }

    private static Int64 f75(Int64 x, Int64 y)
    {
        x >>= (int)(x << (int)y);
        return x;
    }

    private static Int64 f76(Int64 x, Int64 y)
    {
        x >>= (int)(x >> (int)y);
        return x;
    }

    private static Int64 f77(Int64 x, Int64 y)
    {
        x >>= (int)(x & y);
        return x;
    }

    private static Int64 f78(Int64 x, Int64 y)
    {
        x >>= (int)(x ^ y);
        return x;
    }

    private static Int64 f79(Int64 x, Int64 y)
    {
        x >>= (int)(x | y);
        return x;
    }

    private static Int64 f80(Int64 x, Int64 y)
    {
        x &= x + y;
        return x;
    }

    private static Int64 f81(Int64 x, Int64 y)
    {
        x &= x - y;
        return x;
    }

    private static Int64 f82(Int64 x, Int64 y)
    {
        x &= x * y;
        return x;
    }

    private static Int64 f83(Int64 x, Int64 y)
    {
        x &= x / y;
        return x;
    }

    private static Int64 f84(Int64 x, Int64 y)
    {
        x &= x % y;
        return x;
    }

    private static Int64 f85(Int64 x, Int64 y)
    {
        x &= x << (int)y;
        return x;
    }

    private static Int64 f86(Int64 x, Int64 y)
    {
        x &= x >> (int)y;
        return x;
    }

    private static Int64 f87(Int64 x, Int64 y)
    {
        x &= x & y;
        return x;
    }

    private static Int64 f88(Int64 x, Int64 y)
    {
        x &= x ^ y;
        return x;
    }

    private static Int64 f89(Int64 x, Int64 y)
    {
        x &= x | y;
        return x;
    }

    private static Int64 f90(Int64 x, Int64 y)
    {
        x ^= x + y;
        return x;
    }

    private static Int64 f91(Int64 x, Int64 y)
    {
        x ^= x - y;
        return x;
    }

    private static Int64 f92(Int64 x, Int64 y)
    {
        x ^= x * y;
        return x;
    }

    private static Int64 f93(Int64 x, Int64 y)
    {
        x ^= x / y;
        return x;
    }

    private static Int64 f94(Int64 x, Int64 y)
    {
        x ^= x % y;
        return x;
    }

    private static Int64 f95(Int64 x, Int64 y)
    {
        x ^= x << (int)y;
        return x;
    }

    private static Int64 f96(Int64 x, Int64 y)
    {
        x ^= x >> (int)y;
        return x;
    }

    private static Int64 f97(Int64 x, Int64 y)
    {
        x ^= x & y;
        return x;
    }

    private static Int64 f98(Int64 x, Int64 y)
    {
        x ^= x ^ y;
        return x;
    }

    private static Int64 f99(Int64 x, Int64 y)
    {
        x ^= x | y;
        return x;
    }

    private static Int64 f100(Int64 x, Int64 y)
    {
        x |= x + y;
        return x;
    }

    private static Int64 f101(Int64 x, Int64 y)
    {
        x |= x - y;
        return x;
    }

    private static Int64 f102(Int64 x, Int64 y)
    {
        x |= x * y;
        return x;
    }

    private static Int64 f103(Int64 x, Int64 y)
    {
        x |= x / y;
        return x;
    }

    private static Int64 f104(Int64 x, Int64 y)
    {
        x |= x % y;
        return x;
    }

    private static Int64 f105(Int64 x, Int64 y)
    {
        x |= x << (int)y;
        return x;
    }

    private static Int64 f106(Int64 x, Int64 y)
    {
        x |= x >> (int)y;
        return x;
    }

    private static Int64 f107(Int64 x, Int64 y)
    {
        x |= x & y;
        return x;
    }

    private static Int64 f108(Int64 x, Int64 y)
    {
        x |= x ^ y;
        return x;
    }

    private static Int64 f109(Int64 x, Int64 y)
    {
        x |= x | y;
        return x;
    }


    public static int Main()
    {
        Int64 x;
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

        /*
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
		*/

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

        /*
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
		*/

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

        /*
		x = f75(-10, 4);
		if (x != -10)
		{
			pass = false;
		}
		*/

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
