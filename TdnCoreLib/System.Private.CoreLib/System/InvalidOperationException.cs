namespace System;

public class InvalidOperationException : SystemException
{
    
    public InvalidOperationException()
        : base(SR.Arg_InvalidOperationException)
    {
    }

    public InvalidOperationException(string? message)
        : base(message)
    {
    }

    public InvalidOperationException(string? message, Exception? innerException)
        : base(message, innerException)
    {
    }
    
}