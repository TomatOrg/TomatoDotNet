// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

namespace System;

/// <summary>
/// The exception that is thrown for invalid casting or explicit conversion.
/// </summary>
public class InvalidCastException : SystemException
{
    public InvalidCastException()
        : base(SR.Arg_InvalidCastException)
    {
    }

    public InvalidCastException(string? message)
        : base(message)
    {
    }

    public InvalidCastException(string? message, Exception? innerException)
        : base(message, innerException)
    {
    }

    public InvalidCastException(string? message, int errorCode)
        : base(message)
    {
    }
}