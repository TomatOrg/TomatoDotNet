namespace System;

public class ThrowHelper
{
    
    internal static void ThrowInvalidOperationException_InvalidOperation_NoValue()
    {
        throw new InvalidOperationException(SR.InvalidOperation_NoValue);
    }

    
}