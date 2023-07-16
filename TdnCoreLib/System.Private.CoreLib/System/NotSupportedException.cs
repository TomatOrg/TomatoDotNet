namespace System;

public class NotSupportedException : SystemException
{

    public NotSupportedException()
        : base(SR.Arg_NotSupportedException)
    {
    }
    
    public NotSupportedException(string? message)
        : base(message)
    {
    }

    public NotSupportedException(string? message, Exception? innerException)
        : base(message, innerException)
    {
    }

}