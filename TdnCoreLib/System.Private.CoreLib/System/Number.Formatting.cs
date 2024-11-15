using System.Buffers.Text;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.CompilerServices;

namespace System;

internal static partial class Number
{

    private const int SmallNumberCacheLength = 500;
    
    private static readonly string[] SmallNumberCache = new string[SmallNumberCacheLength];

    private static string UInt32ToDecStr(uint value)
    {
        // For small numbers, consult a lazily-populated cache.
        if (value < SmallNumberCacheLength)
        {
            return UInt32ToDecStrForKnownSmallNumber(value);
        }

        return UInt32ToDecStr_NoSmallNumberCheck(value);
    }

    private static string UInt32ToDecStrForKnownSmallNumber(uint value)
    {
        Debug.Assert(value < SmallNumberCacheLength);
        return SmallNumberCache[value] ?? CreateAndCacheString(value);

        [MethodImpl(MethodImplOptions.NoInlining)] // keep rare usage out of fast path
        static string CreateAndCacheString(uint value) =>
            SmallNumberCache[value] = UInt32ToDecStr_NoSmallNumberCheck(value);
    }
    
    private static string UInt32ToDecStr_NoSmallNumberCheck(uint value)
    {
        var bufferLength = FormattingHelpers.CountDigits(value);

        // string result = string.FastAllocateString(bufferLength);
        // fixed (char* buffer = result)
        // {
        //     char* p = buffer + bufferLength;
        //     p = UInt32ToDecChars(p, value);
        //     Debug.Assert(p == buffer);
        // }
        // return result;
        return null;
    }
    
    public static string Int32ToDecStr(int value) =>
        value >= 0 ?
            UInt32ToDecStr((uint)value) :
            NegativeInt32ToDecStr(value, -1, NumberFormatInfo.CurrentInfo.NegativeSign);
    
    private static string NegativeInt32ToDecStr(int value, int digits, string sNegative)
    {
        Debug.Assert(value < 0);

        if (digits < 1)
        {
            digits = 1;
        }

        // var bufferLength = Math.Max(digits, FormattingHelpers.CountDigits((uint)(-value))) + sNegative.Length;
        // string result = string.FastAllocateString(bufferLength);
        // fixed (char* buffer = result)
        // {
        //     char* p = UInt32ToDecChars(buffer + bufferLength, (uint)(-value), digits);
        //     Debug.Assert(p == buffer + sNegative.Length);
        //
        //     for (int i = sNegative.Length - 1; i >= 0; i--)
        //     {
        //         *(--p) = sNegative[i];
        //     }
        //     Debug.Assert(p == buffer);
        // }
        // return result;
        return null;
    }

    
}