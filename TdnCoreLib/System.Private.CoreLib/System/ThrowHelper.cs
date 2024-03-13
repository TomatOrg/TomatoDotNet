namespace System;

internal static class ThrowHelper
{
    
    internal static void ThrowInvalidOperationException_InvalidOperation_NoValue()
    {
        throw new InvalidOperationException(SR.InvalidOperation_NoValue);
    }

    internal static void ThrowIndexOutOfRangeException()
    {
        throw new IndexOutOfRangeException();
    }
    
    internal static void ThrowArgumentOutOfRangeException()
    {
        throw new ArgumentOutOfRangeException();
    }
    
}