namespace System;

public class ArgumentException : SystemException
{
    
    private readonly string? _paramName;

    public ArgumentException()
        : base(SR.Arg_ArgumentException)
    {
    }
    
    public ArgumentException(string? message)
        : base(message)
    {
    }
    
    public ArgumentException(string? message, Exception? innerException)
        : base(message, innerException)
    {
    }

    public ArgumentException(string? message, string? paramName, Exception? innerException)
        : base(message, innerException)
    {
        _paramName = paramName;
    }
    
    public ArgumentException(string? message, string? paramName)
        : base(message)
    {
        _paramName = paramName;
    }

}