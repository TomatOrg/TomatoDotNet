namespace System;

public class OverflowException : ArithmeticException
{
    
    public OverflowException()
        : base(SR.Arg_OverflowException)
    {
    }

    public OverflowException(string? message)
        : base(message)
    {
    }

    public OverflowException(string? message, Exception? innerException)
        : base(message, innerException)
    {
    }

    
}