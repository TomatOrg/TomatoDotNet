namespace System;

public class Exception
{

    private string? _message;
    private Exception? _innerException;
    
    public Exception()
    {
    }

    public Exception(string? message)
    {
        _message = message;
    }
    
    public Exception(string? message, Exception? innerException)
    {
        _message = message;
        _innerException = innerException;
    }

    
}