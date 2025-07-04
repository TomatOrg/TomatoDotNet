// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
//

using System;

namespace ShiftTest
{
    public class CL
    {
        public short clm_data = 0x7FFF;
    }
    public struct VT
    {
        public short vtm_data;
    }
    public class short32Test
    {
        private static short s_data = 0x7FFF;
        public static short f1(short arg_data)
        {
            arg_data >>= 4;
            return arg_data;
        }
        public static short f2(short arg_data)
        {
            arg_data <<= 4;
            return arg_data;
        }
        public static int Main()
        {
            short loc_data = 0x7FFF;

            short[] arr_data = new short[1];

            CL cl = new CL();
            VT vt;

            s_data = 0x7FFF;
            loc_data = 0x7FFF;
            arr_data[0] = 0x7FFF;
            cl.clm_data = 0x7FFF;
            vt.vtm_data = 0x7FFF;

            // Test >>


            loc_data >>= 4;
            s_data >>= 4;
            arr_data[0] >>= 4;
            cl.clm_data >>= 4;
            vt.vtm_data >>= 4;


            if (loc_data != (0x7FFF >> 4))
            {
                return -1;
            }
            if (f1(0x7FFF) != (0x7FFF >> 4))
            {
                return -1;
            }
            if (s_data != (0x7FFF >> 4))
            {
                return -1;
            }
            if (arr_data[0] != (0x7FFF >> 4))
            {
                return -1;
            }
            if (cl.clm_data != (0x7FFF >> 4))
            {
                return -1;
            }
            if (vt.vtm_data != (0x7FFF >> 4))
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
