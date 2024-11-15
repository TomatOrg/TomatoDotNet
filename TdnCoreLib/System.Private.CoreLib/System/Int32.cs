// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.


using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public readonly struct Int32
    : IComparable
    , IComparable<int>
    , IEquatable<int>
{
    private readonly int _value;

    public const int MaxValue = 0x7fffffff;
    public const int MinValue = unchecked((int)0x80000000);

    // Compares this object to another object, returning an integer that
    // indicates the relationship.
    // Returns :
    // 0 if the values are equal
    // Negative number if _value is less than value
    // Positive number if _value is more than value
    // null is considered to be less than any instance, hence returns positive number
    // If object is not of type Int32, this method throws an ArgumentException.
    //
    public int CompareTo(object? value)
    {
        if (value == null)
        {
            return 1;
        }

        // NOTE: Cannot use return (_value - value) as this causes a wrap
        // around in cases where _value - value > MaxValue.
        if (value is int i)
        {
            if (_value < i) return -1;
            if (_value > i) return 1;
            return 0;
        }

        throw new ArgumentException(SR.Arg_MustBeInt32);
    }

    public int CompareTo(int value)
    {
        // NOTE: Cannot use return (_value - value) as this causes a wrap
        // around in cases where _value - value > MaxValue.
        if (_value < value) return -1;
        if (_value > value) return 1;
        return 0;
    }

    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        if (obj is not int i)
        {
            return false;
        }
        return _value == i;
    }

    public bool Equals(int obj)
    {
        return _value == obj;
    }

    // The absolute value of the int contained.
    public override int GetHashCode()
    {
        return _value;
    }

    public override string ToString()
    {
        return Number.Int32ToDecStr(_value);
    }

}