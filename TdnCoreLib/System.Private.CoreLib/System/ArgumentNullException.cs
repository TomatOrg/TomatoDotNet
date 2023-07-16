namespace System;

public class ArgumentNullException : ArgumentException
{
    
    public ArgumentNullException()
        : base(SR.ArgumentNull_Generic)
    {
    }

    public ArgumentNullException(string? paramName)
        : base(SR.ArgumentNull_Generic, paramName)
    {
    }

    public ArgumentNullException(string? message, Exception? innerException)
        : base(message, innerException)
    {
    }
    
    public ArgumentNullException(string? paramName, string? message)
        : base(message, paramName)
    {
    }

    public static void ThrowIfNull(object? argument, string? paramName = null)
    {
        if (argument is null)
        {
            Throw(paramName);
        }
    }
    
    internal static void Throw(string? paramName) =>
        throw new ArgumentNullException(paramName);

}