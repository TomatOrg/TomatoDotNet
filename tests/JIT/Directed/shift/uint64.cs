// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//

using System;

namespace ShiftTest
{
    public class CL
    {
        public ulong clm_data = 0xFFFFFFFFFFFFFFFF;
    }
    public struct VT
    {
        public ulong vtm_data;
    }
    public class ulong32Test
    {
        private static ulong s_data = 0xFFFFFFFFFFFFFFFF;
        public static ulong f1(ulong arg_data)
        {
            arg_data >>= 8;
            return arg_data;
        }
        public static ulong f2(ulong arg_data)
        {
            arg_data <<= 8;
            return arg_data;
        }
        public static int Main()
        {
            ulong loc_data = 0xFFFFFFFFFFFFFFFF;

            ulong[] arr_data = new ulong[1];

            CL cl = new CL();
            VT vt;

            s_data = 0xFFFFFFFFFFFFFFFF;
            loc_data = 0xFFFFFFFFFFFFFFFF;
            arr_data[0] = 0xFFFFFFFFFFFFFFFF;
            cl.clm_data = 0xFFFFFFFFFFFFFFFF;
            vt.vtm_data = 0xFFFFFFFFFFFFFFFF;

            // Test >>


            loc_data >>= 8;
            s_data >>= 8;
            arr_data[0] >>= 8;
            cl.clm_data >>= 8;
            vt.vtm_data >>= 8;


            if (loc_data != (0xFFFFFFFFFFFFFFFF >> 8))
            {
                return -1;
            }
            if (f1(0xFFFFFFFFFFFFFFFF) != (0xFFFFFFFFFFFFFFFF >> 8))
            {
                return -1;
            }
            if (s_data != (0xFFFFFFFFFFFFFFFF >> 8))
            {
                return -1;
            }
            if (arr_data[0] != (0xFFFFFFFFFFFFFFFF >> 8))
            {
                return -1;
            }
            if (cl.clm_data != (0xFFFFFFFFFFFFFFFF >> 8))
            {
                return -1;
            }
            if (vt.vtm_data != (0xFFFFFFFFFFFFFFFF >> 8))
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
