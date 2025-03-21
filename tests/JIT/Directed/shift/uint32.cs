// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//

using System;

namespace ShiftTest
{
    public class CL
    {
        public uint clm_data = 0xFFFFFFFF;
    }
    public struct VT
    {
        public uint vtm_data;
    }
    public class uint32Test
    {
        private static uint s_data = 0xFFFFFFFF;
        public static uint f1(uint arg_data)
        {
            arg_data >>= 4;
            return arg_data;
        }
        public static uint f2(uint arg_data)
        {
            arg_data <<= 4;
            return arg_data;
        }
        public static int Main()
        {
            uint loc_data = 0xFFFFFFFF;

            uint[] arr_data = new uint[1];

            CL cl = new CL();
            VT vt;

            s_data = 0xFFFFFFFF;
            loc_data = 0xFFFFFFFF;
            arr_data[0] = 0xFFFFFFFF;
            cl.clm_data = 0xFFFFFFFF;
            vt.vtm_data = 0xFFFFFFFF;

            // Test >>


            loc_data >>= 4;
            s_data >>= 4;
            arr_data[0] >>= 4;
            cl.clm_data >>= 4;
            vt.vtm_data >>= 4;


            if (loc_data != (0xFFFFFFFF >> 4))
            {
                return -1;
            }
            if (f1(0xFFFFFFFF) != (0xFFFFFFFF >> 4))
            {
                return -1;
            }
            if (s_data != (0xFFFFFFFF >> 4))
            {
                return -1;
            }
            if (arr_data[0] != (0xFFFFFFFF >> 4))
            {
                return -1;
            }
            if (cl.clm_data != (0xFFFFFFFF >> 4))
            {
                return -1;
            }
            if (vt.vtm_data != (0xFFFFFFFF >> 4))
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
