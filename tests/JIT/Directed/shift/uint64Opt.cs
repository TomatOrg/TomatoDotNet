// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//

using System;

// Test long and ulong shifts by 0, 32 and 64 constants that can be optimized.
namespace ShiftTest
{

    public class ulong64Test
    {
        static int shiftByZero(ulong arg)
        {
            if (arg != arg << 0)
            {
                return -1;
            }
            if (arg != arg >> 0)
            {
                return -1;
            }
            return 100;
        }

        static int shiftBy32(ulong arg)
        {
            ulong powerOfTwo = 0x100000000UL;
            if (arg * powerOfTwo != arg << 32)
            {
                return -1;
            }
            if (arg / powerOfTwo != arg >> 32)
            {
                return -1;
            }
            return 100;
        }

        static int shiftBy64(ulong arg)
        {
            // The shift count is computed from count & 0x3F.
            if (arg != arg << 64)
            {
                return -1;
            }
            if (arg != arg >> 64)
            {
                return -1;
            }
            return 100;
        }

        public static int run(ulong arg)
        {
            bool passed = true;
            if (shiftByZero(arg) != 100)
            {
                passed = false;
            }
            if (shiftBy32(arg) != 100)
            {
                passed = false;
            }
            if (shiftBy64(arg) != 100)
            {
                passed = false;
            }
            if (passed)
            {
                return 100;
            }
            return -1;
        }
    }

    public class long64Test
    {
        static int shiftByZero(long arg)
        {
            if (arg != arg << 0)
            {
                return -1;
            }
            if (arg != arg >> 0)
            {
                return -1;
            }
            return 100;
        }

        static int shiftBy32(long arg)
        {
            long powerOfTwo = 0x100000000L;
            if (arg * powerOfTwo != arg << 32)
            {
                return -1;
            }
            if (arg / powerOfTwo != arg >> 32)
            {
                return -1;
            }
            return 100;
        }

        static int shiftBy64(long arg)
        {
            // The shift count is computed from count & 0x3F.
            if (arg != arg << 64)
            {
                return -1;
            }
            if (arg != arg >> 64)
            {
                return -1;
            }
            if (-arg != -arg >> 64)
            {
                return -1;
            }
            return 100;
        }

        public static int run(long arg)
        {
            bool passed = true;
            if (shiftByZero(arg) != 100)
            {
                passed = false;
            }
            if (shiftBy32(arg) != 100)
            {
                passed = false;
            }
            if (shiftBy64(arg) != 100)
            {
                passed = false;
            }
            if (passed)
            {
                return 100;
            }
            return -1;
        }
    }

    public class Test
    {
        public static int Main()
        {
            bool passed = true;
            ulong ulongArg = 0x3F134;
            if (ulong64Test.run(ulongArg) != 100)
            {
                passed = false;
            }
            long longArg = 0x3F134;
            if (long64Test.run(longArg) != 100)
            {
                passed = false;
            }
            if (passed)
            {
                return 100;
            }
            return -1;
        }
    }
}
