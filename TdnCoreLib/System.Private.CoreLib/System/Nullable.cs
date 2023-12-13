namespace System;

public struct Nullable<T> where T : struct
{

    private readonly bool _hasValue;
    private T _value;

    public Nullable(T value)
    {
        _value = value;
        _hasValue = true;
    }

    public readonly bool HasValue => _hasValue;

    public readonly T Value
    {
        get
        {
            if (!_hasValue)
            {
                ThrowHelper.ThrowInvalidOperationException_InvalidOperation_NoValue();
            }

            return _value;
        }
    }

    public readonly T GetValueOrDefault()
    {
        return _value;
    }

    public readonly T GetValueOrDefault(T defaultValue)
    {
        return _hasValue ? _value : defaultValue;
    }

    public override bool Equals(object? obj)
    {
        if (!_hasValue)
        {
            return obj == null;
        }

        if (obj == null)
        {
            return false;
        }

        return _value.Equals(obj);
    }

    public override int GetHashCode()
    {
        return _hasValue ? _value.GetHashCode() : 0;
    }

    public override string ToString()
    {
        return _hasValue  ? _value.ToString() : "";
    }
    
}
