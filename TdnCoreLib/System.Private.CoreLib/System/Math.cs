using System.Runtime.CompilerServices;

namespace System;

public static class Math
{
    
    public static byte Max(byte val1, byte val2)
    {
        return (val1 >= val2) ? val1 : val2;
    }
    
    public static short Max(short val1, short val2)
    {
        return (val1 >= val2) ? val1 : val2;
    }

    public static int Max(int val1, int val2)
    {
        return (val1 >= val2) ? val1 : val2;
    }

    public static long Max(long val1, long val2)
    {
        return (val1 >= val2) ? val1 : val2;
    }
    
    public static nint Max(nint val1, nint val2)
    {
        return (val1 >= val2) ? val1 : val2;
    }
    
    public static sbyte Max(sbyte val1, sbyte val2)
    {
        return (val1 >= val2) ? val1 : val2;
    }
    
    public static ushort Max(ushort val1, ushort val2)
    {
        return (val1 >= val2) ? val1 : val2;
    }
    
    public static uint Max(uint val1, uint val2)
    {
        return (val1 >= val2) ? val1 : val2;
    }

    public static ulong Max(ulong val1, ulong val2)
    {
        return (val1 >= val2) ? val1 : val2;
    }
    
    public static nuint Max(nuint val1, nuint val2)
    {
        return (val1 >= val2) ? val1 : val2;
    }
    
    
    public static int DivRem(int a, int b, out int result)
    {
        result = a % b;
        return a / b;
    }

    public static long DivRem(long a, long b, out long result)
    {
        result = a % b;
        return a / b;
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static (sbyte Quotient, sbyte Remainder) DivRem(sbyte left, sbyte right)
    {
        sbyte quotient = (sbyte)(left / right);
        return (quotient, (sbyte)(left % right));
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static (byte Quotient, byte Remainder) DivRem(byte left, byte right)
    {
        byte quotient = (byte)(left / right);
        return (quotient, (byte)(left % right));
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static (short Quotient, short Remainder) DivRem(short left, short right)
    {
        short quotient = (short)(left / right);
        return (quotient, (short)(left % right));
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static (ushort Quotient, ushort Remainder) DivRem(ushort left, ushort right)
    {
        ushort quotient = (ushort)(left / right);
        return (quotient, (ushort)(left % right));
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static (int Quotient, int Remainder) DivRem(int left, int right)
    {
        int quotient = left / right;
        return (quotient, left % right);
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static (uint Quotient, uint Remainder) DivRem(uint left, uint right)
    {
        uint quotient = left / right;
        return (quotient, left % right);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static (long Quotient, long Remainder) DivRem(long left, long right)
    {
        long quotient = left / right;
        return (quotient, left % right);
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static (ulong Quotient, ulong Remainder) DivRem(ulong left, ulong right)
    {
        ulong quotient = left / right;
        return (quotient, left % right);
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static (nint Quotient, nint Remainder) DivRem(nint left, nint right)
    {
        nint quotient = left / right;
        return (quotient, left % right);
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static (nuint Quotient, nuint Remainder) DivRem(nuint left, nuint right)
    {
        nuint quotient = left / right;
        return (quotient, left % right);
    }
    
}