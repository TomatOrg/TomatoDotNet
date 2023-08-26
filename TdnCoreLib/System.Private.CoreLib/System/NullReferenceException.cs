namespace System;

public class NullReferenceException : SystemException
{
    
    public NullReferenceException()
        : base(SR.Arg_NullReferenceException)
    {
    }

    public NullReferenceException(string? message)
        : base(message)
    {
    }

    public NullReferenceException(string? message, Exception? innerException)
        : base(message, innerException)
    {
    }
    
}