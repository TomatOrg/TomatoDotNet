namespace System;

public class ArithmeticException : SystemException
{
    
    public ArithmeticException()
        : base(SR.Arg_ArithmeticException)
    {
    }
    
    public ArithmeticException(string? message)
        : base(message)
    {
    }

    public ArithmeticException(string? message, Exception? innerException)
        : base(message, innerException)
    {
    }
    
}