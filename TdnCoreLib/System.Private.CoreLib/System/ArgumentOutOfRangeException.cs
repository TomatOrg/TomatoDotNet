namespace System;

public class ArgumentOutOfRangeException : ArgumentException
{

    private readonly object? _actualValue;

    public ArgumentOutOfRangeException()
        : base(SR.Arg_ArgumentOutOfRangeException)
    {
    }

    public ArgumentOutOfRangeException(string? paramName)
        : base(SR.Arg_ArgumentOutOfRangeException, paramName)
    {
    }

    public ArgumentOutOfRangeException(string? paramName, string? message)
        : base(message, paramName)
    {
    }

    public ArgumentOutOfRangeException(string? message, Exception? innerException)
        : base(message, innerException)
    {
    }

    public ArgumentOutOfRangeException(string? paramName, object? actualValue, string? message)
        : base(message, paramName)
    {
        _actualValue = actualValue;
    }

}