using System.Buffers.Text;
using System.Diagnostics;
using System.Globalization;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace System;

internal static partial class Number
{
    
    // Optimizations using "TwoDigits" inspired by:
    // https://engineering.fb.com/2013/03/15/developer-tools/three-optimization-tips-for-c/
    private static readonly byte[] TwoDigitsCharsAsBytes =
        MemoryMarshal.AsBytes<char>("00010203040506070809" +
                                    "10111213141516171819" +
                                    "20212223242526272829" +
                                    "30313233343536373839" +
                                    "40414243444546474849" +
                                    "50515253545556575859" +
                                    "60616263646566676869" +
                                    "70717273747576777879" +
                                    "80818283848586878889" +
                                    "90919293949596979899").ToArray();

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
        
        var result = new string(bufferLength);
        ref var p = ref result.GetRawStringData();
        p = ref UInt32ToDecChars(ref Unsafe.Add(ref p, bufferLength), value);
        Debug.Assert(Unsafe.AreSame(ref p, ref result.GetRawStringData()));
        return result;
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
        
        var bufferLength = Math.Max(digits, FormattingHelpers.CountDigits((uint)(-value))) + sNegative.Length;
        var result = new string(bufferLength);
        ref var buffer = ref result.GetRawStringData();
        ref var p = ref UInt32ToDecChars(ref Unsafe.Add(ref buffer, bufferLength), (uint)(-value), digits);
        Debug.Assert(Unsafe.AreSame(ref p, ref Unsafe.Add(ref buffer, sNegative.Length)));
    
        for (var i = sNegative.Length - 1; i >= 0; i--)
        {
            p = ref Unsafe.Subtract(ref p, 1);
            p = sNegative[i];
        }
        Debug.Assert(Unsafe.AreSame(ref p, ref buffer));
        return result;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static ref char UInt32ToDecChars(ref char bufferEnd, uint value)
    {
        if (value >= 10)
        {
            // Handle all values >= 100 two-digits at a time so as to avoid expensive integer division operations.
            while (value >= 100)
            {
                bufferEnd = ref Unsafe.Subtract(ref bufferEnd, 2);
                (value, var remainder) = Math.DivRem(value, 100);
                WriteTwoDigits(remainder, ref bufferEnd);
            }

            // If there are two digits remaining, store them.
            if (value >= 10)
            {
                bufferEnd = ref Unsafe.Subtract(ref bufferEnd, 2);
                WriteTwoDigits(value, ref bufferEnd);
                return ref bufferEnd;
            }
        }

        // Otherwise, store the single digit remaining.
        bufferEnd = ref Unsafe.Subtract(ref bufferEnd, 1);
        bufferEnd = (char)(value + '0');
        return ref bufferEnd;
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static ref char UInt32ToDecChars(ref char bufferEnd, uint value, int digits)
    {
        uint remainder;
        while (value >= 100)
        {
            bufferEnd = ref Unsafe.Subtract(ref bufferEnd, 2);
            digits -= 2;
            (value, remainder) = Math.DivRem(value, 100);
            WriteTwoDigits(remainder, ref bufferEnd);
        }

        while (value != 0 || digits > 0)
        {
            digits--;
            (value, remainder) = Math.DivRem(value, 10);
            bufferEnd = ref Unsafe.Subtract(ref bufferEnd, 1);
            bufferEnd = (char)(remainder + '0');
        }

        return ref bufferEnd;
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void WriteTwoDigits(uint value, ref char ptr)
    {
        Debug.Assert(value <= 99);

        Unsafe.CopyBlockUnaligned(
            ref Unsafe.As<char, byte>(ref ptr),
            ref Unsafe.Add(ref MemoryMarshal.GetArrayDataReference(TwoDigitsCharsAsBytes), (uint)sizeof(char) * 2 * value),
            (uint)sizeof(char) * 2);
    }
    
}