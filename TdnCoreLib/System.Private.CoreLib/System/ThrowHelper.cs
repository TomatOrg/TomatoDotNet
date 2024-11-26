namespace System;

internal static class ThrowHelper
{
    
    internal static void ThrowArgumentNullException(string argument)
    {
        throw new ArgumentNullException(argument);
    }
    
    internal static void ThrowInvalidOperationException_InvalidOperation_NoValue()
    {
        throw new InvalidOperationException(SR.InvalidOperation_NoValue);
    }

    internal static void ThrowArgumentException_TupleIncorrectType(object obj)
    {
        // TODO: formatting 
        throw new ArgumentException();
    }
    
    internal static void ThrowIndexOutOfRangeException()
    {
        throw new IndexOutOfRangeException();
    }
    
    internal static void ThrowArgumentOutOfRangeException()
    {
        throw new ArgumentOutOfRangeException();
    }
    
    internal static void ThrowArgumentOutOfRangeException(string argument)
    {
        throw new ArgumentOutOfRangeException(argument);
    }
    
    internal static void ThrowArgumentException_DestinationTooShort()
    {
        throw new ArgumentException(SR.Argument_DestinationTooShort, "destination");
    }
    
}