// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System;

namespace Test_i8flat_cs
{
public class test
{
    public static int Main()
    {
        Int64 x;
        Int64 y;

        bool pass = true;

        x = -10;
        y = 4;

        x = x + y;
        if (x != -6)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x = x - y;
        if (x != -14)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x = x * y;
        if (x != -40)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x = x / y;
        if (x != -2)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x = x % y;
        if (x != -2)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x = x << (int)y;
        if (x != -160)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x = x >> (int)y;
        if (x != -1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x = x & y;
        if (x != 4)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x = x ^ y;
        if (x != -14)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x = x | y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x += x + y;
        if (x != -16)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x += x - y;
        if (x != -24)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x += x * y;
        if (x != -50)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x += x / y;
        if (x != -12)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x += x % y;
        if (x != -12)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x += x << (int)y;
        if (x != -170)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x += x >> (int)y;
        if (x != -11)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x += x & y;
        if (x != -6)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x += x ^ y;
        if (x != -24)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x += x | y;
        if (x != -20)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x -= x + y;
        if (x != -4)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x -= x - y;
        if (x != 4)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x -= x * y;
        if (x != 30)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x -= x / y;
        if (x != -8)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x -= x % y;
        if (x != -8)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x -= x << (int)y;
        if (x != 150)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x -= x >> (int)y;
        if (x != -9)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x -= x & y;
        if (x != -14)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x -= x ^ y;
        if (x != 4)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x -= x | y;
        if (x != 0)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x *= x + y;
        if (x != 60)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x *= x - y;
        if (x != 140)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x *= x * y;
        if (x != 400)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x *= x / y;
        if (x != 20)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x *= x % y;
        if (x != 20)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x *= x << (int)y;
        if (x != 1600)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x *= x >> (int)y;
        if (x != 10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x *= x & y;
        if (x != -40)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x *= x ^ y;
        if (x != 140)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x *= x | y;
        if (x != 100)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x /= x + y;
        if (x != 1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x /= x - y;
        if (x != 0)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x /= x * y;
        if (x != 0)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x /= x / y;
        if (x != 5)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x /= x % y;
        if (x != 5)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x /= x << (int)y;
        if (x != 0)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x /= x >> (int)y;
        if (x != 10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x /= x & y;
        if (x != -2)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x /= x ^ y;
        if (x != 0)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x /= x | y;
        if (x != 1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x %= x + y;
        if (x != -4)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x %= x - y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x %= x * y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x %= x / y;
        if (x != 0)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x %= x % y;
        if (x != 0)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x %= x << (int)y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x %= x >> (int)y;
        if (x != 0)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x %= x & y;
        if (x != -2)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x %= x ^ y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x %= x | y;
        if (x != 0)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        /*
		x <<= (int)( x + y);
		if (x != -671088640)
		{
			pass = false;
		}

		x = -10;
		y = 4;

		x <<= (int)( x - y);
		if (x != -2621440)
		{
			pass = false;
		}

		x = -10;
		y = 4;

		x <<= (int)( x * y);
		if (x != -167772160)
		{
			pass = false;
		}

		x = -10;
		y = 4;

		x <<= (int)( x / y);
		if (x != -2147483648)
		{
			pass = false;
		}

		x = -10;
		y = 4;

		x <<= (int)( x % y);
		if (x != -2147483648)
		{
			pass = false;
		}

		x = -10;
		y = 4;

		x <<= (int)( x << (int)y);
		if (x != -10)
		{
			pass = false;
		}
		*/

        x = -10;
        y = 4;

        x <<= (int)(x >> (int)y);
        if (x != 0)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x <<= (int)(x & y);
        if (x != -160)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        /*
		x <<= (int)( x ^ y);
		if (x != -2621440)
		{
			pass = false;
		}

		x = -10;
		y = 4;

		x <<= (int)( x | y);
		if (x != -41943040)
		{
			pass = false;
		}
		*/

        x = -10;
        y = 4;

        x >>= (int)(x + y);
        if (x != -1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x >>= (int)(x - y);
        if (x != -1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x >>= (int)(x * y);
        if (x != -1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x >>= (int)(x / y);
        if (x != -1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x >>= (int)(x % y);
        if (x != -1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        /*
		x >>= (int)( x << (int)y);
		if (x != -10)
		{
			pass = false;
		}
		*/

        x = -10;
        y = 4;

        x >>= (int)(x >> (int)y);
        if (x != -1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x >>= (int)(x & y);
        if (x != -1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x >>= (int)(x ^ y);
        if (x != -1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x >>= (int)(x | y);
        if (x != -1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x &= x + y;
        if (x != -14)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x &= x - y;
        if (x != -14)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x &= x * y;
        if (x != -48)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x &= x / y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x &= x % y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x &= x << (int)y;
        if (x != -160)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x &= x >> (int)y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x &= x & y;
        if (x != 4)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x &= x ^ y;
        if (x != -14)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x &= x | y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x ^= x + y;
        if (x != 12)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x ^= x - y;
        if (x != 4)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x ^= x * y;
        if (x != 46)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x ^= x / y;
        if (x != 8)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x ^= x % y;
        if (x != 8)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x ^= x << (int)y;
        if (x != 150)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x ^= x >> (int)y;
        if (x != 9)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x ^= x & y;
        if (x != -14)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x ^= x ^ y;
        if (x != 4)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x ^= x | y;
        if (x != 0)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x |= x + y;
        if (x != -2)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x |= x - y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x |= x * y;
        if (x != -2)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x |= x / y;
        if (x != -2)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x |= x % y;
        if (x != -2)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x |= x << (int)y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x |= x >> (int)y;
        if (x != -1)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x |= x & y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x |= x ^ y;
        if (x != -10)
        {
            pass = false;
        }

        x = -10;
        y = 4;

        x |= x | y;
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
