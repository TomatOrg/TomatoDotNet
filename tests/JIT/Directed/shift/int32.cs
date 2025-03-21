// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//

using System;

namespace ShiftTest
{
    public class CL
    {
        public int clm_data = 0x7FFFFFFF;
    }
    public struct VT
    {
        public int vtm_data;
    }
    public class int32Test
    {
        private static int s_data = 0x7FFFFFFF;
        public static int f1(int arg_data)
        {
            arg_data >>= 4;
            return arg_data;
        }
        public static int f2(int arg_data)
        {
            arg_data <<= 4;
            return arg_data;
        }
        public static int Main()
        {
            int loc_data = 0x7FFFFFFF;

            int[] arr_data = new int[1];

            CL cl = new CL();
            VT vt;

            s_data = 0x7FFFFFFF;
            loc_data = 0x7FFFFFFF;
            arr_data[0] = 0x7FFFFFFF;
            cl.clm_data = 0x7FFFFFFF;
            vt.vtm_data = 0x7FFFFFFF;

            // Test >>


            loc_data >>= 4;
            s_data >>= 4;
            arr_data[0] >>= 4;
            cl.clm_data >>= 4;
            vt.vtm_data >>= 4;


            if (loc_data != (0x7FFFFFFF >> 4))
            {
                return -1;
            }
            if (f1(0x7FFFFFFF) != (0x7FFFFFFF >> 4))
            {
                return -1;
            }
            if (s_data != (0x7FFFFFFF >> 4))
            {
                return -1;
            }
            if (arr_data[0] != (0x7FFFFFFF >> 4))
            {
                return -1;
            }
            if (cl.clm_data != (0x7FFFFFFF >> 4))
            {
                return -1;
            }
            if (vt.vtm_data != (0x7FFFFFFF >> 4))
            {
                return -1;
            }

            // Test <<

            s_data = 0x1;
            loc_data = 0x1;
            arr_data[0] = 0x1;
            cl.clm_data = 0x1;
            vt.vtm_data = 0x1;


            loc_data <<= 4;
            s_data <<= 4;
            arr_data[0] <<= 4;
            cl.clm_data <<= 4;
            vt.vtm_data <<= 4;


            if (loc_data != (0x1 << 4))
            {
                return -1;
            }
            if (f2(0x1) != (0x1 << 4))
            {
                return -1;
            }
            if (s_data != (0x1 << 4))
            {
                return -1;
            }
            if (arr_data[0] != (0x1 << 4))
            {
                return -1;
            }
            if (cl.clm_data != (0x1 << 4))
            {
                return -1;
            }
            if (vt.vtm_data != (0x1 << 4))
            {
                return -1;
            }

            return 100;
        }
    }
}
