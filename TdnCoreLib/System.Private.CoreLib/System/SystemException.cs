namespace System;

public class SystemException : Exception
{
    
    public SystemException()
        : base(SR.Arg_SystemException)
    {
    }

    public SystemException(string? message)
        : base(message)
    {
    }

    public SystemException(string? message, Exception? innerException)
        : base(message, innerException)
    {
    }

}