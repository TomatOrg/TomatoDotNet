// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//

using System;

namespace ShiftTest
{
    public class CL
    {
        public long clm_data = 0x7FFFFFFFFFFFFFFF;
    }
    public struct VT
    {
        public long vtm_data;
    }
    public class longTest
    {
        private static long s_data = 0x7FFFFFFFFFFFFFFF;
        public static long f1(long arg_data)
        {
            arg_data >>= 8;
            return arg_data;
        }
        public static long f2(long arg_data)
        {
            arg_data <<= 8;
            return arg_data;
        }
        public static int Main()
        {
            long loc_data = 0x7FFFFFFFFFFFFFFF;

            long[] arr_data = new long[1];

            CL cl = new CL();
            VT vt;

            s_data = 0x7FFFFFFFFFFFFFFF;
            loc_data = 0x7FFFFFFFFFFFFFFF;
            arr_data[0] = 0x7FFFFFFFFFFFFFFF;
            cl.clm_data = 0x7FFFFFFFFFFFFFFF;
            vt.vtm_data = 0x7FFFFFFFFFFFFFFF;

            // Test >>


            loc_data >>= 8;
            s_data >>= 8;
            arr_data[0] >>= 8;
            cl.clm_data >>= 8;
            vt.vtm_data >>= 8;


            if (loc_data != (0x7FFFFFFFFFFFFFFF >> 8))
            {
                return -1;
            }
            if (f1(0x7FFFFFFFFFFFFFFF) != (0x7FFFFFFFFFFFFFFF >> 8))
            {
                return -1;
            }
            if (s_data != (0x7FFFFFFFFFFFFFFF >> 8))
            {
                return -1;
            }
            if (arr_data[0] != (0x7FFFFFFFFFFFFFFF >> 8))
            {
                return -1;
            }
            if (cl.clm_data != (0x7FFFFFFFFFFFFFFF >> 8))
            {
                return -1;
            }
            if (vt.vtm_data != (0x7FFFFFFFFFFFFFFF >> 8))
            {
                return -1;
            }

            // Test <<

            s_data = 0x1;
            loc_data = 0x1;
            arr_data[0] = 0x1;
            cl.clm_data = 0x1;
            vt.vtm_data = 0x1;


            loc_data <<= 8;
            s_data <<= 8;
            arr_data[0] <<= 8;
            cl.clm_data <<= 8;
            vt.vtm_data <<= 8;


            if (loc_data != (0x1 << 8))
            {
                return -1;
            }
            if (f2(0x1) != (0x1 << 8))
            {
                return -1;
            }
            if (s_data != (0x1 << 8))
            {
                return -1;
            }
            if (arr_data[0] != (0x1 << 8))
            {
                return -1;
            }
            if (cl.clm_data != (0x1 << 8))
            {
                return -1;
            }
            if (vt.vtm_data != (0x1 << 8))
            {
                return -1;
            }

            return 100;
        }
    }
}
