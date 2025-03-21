// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//

using System;

namespace ShiftTest
{
    public class CL
    {
        public ushort clm_data = 0xFFFF;
    }
    public struct VT
    {
        public ushort vtm_data;
    }
    public class ushort32Test
    {
        private static ushort s_data = 0xFFFF;
        public static ushort f1(ushort arg_data)
        {
            arg_data >>= 4;
            return arg_data;
        }
        public static ushort f2(ushort arg_data)
        {
            arg_data <<= 4;
            return arg_data;
        }
        public static int Main()
        {
            ushort loc_data = 0xFFFF;

            ushort[] arr_data = new ushort[1];

            CL cl = new CL();
            VT vt;

            s_data = 0xFFFF;
            loc_data = 0xFFFF;
            arr_data[0] = 0xFFFF;
            cl.clm_data = 0xFFFF;
            vt.vtm_data = 0xFFFF;

            // Test >>


            loc_data >>= 4;
            s_data >>= 4;
            arr_data[0] >>= 4;
            cl.clm_data >>= 4;
            vt.vtm_data >>= 4;


            if (loc_data != (0xFFFF >> 4))
            {
                return -1;
            }
            if (f1(0xFFFF) != (0xFFFF >> 4))
            {
                return -1;
            }
            if (s_data != (0xFFFF >> 4))
            {
                return -1;
            }
            if (arr_data[0] != (0xFFFF >> 4))
            {
                return -1;
            }
            if (cl.clm_data != (0xFFFF >> 4))
            {
                return -1;
            }
            if (vt.vtm_data != (0xFFFF >> 4))
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
