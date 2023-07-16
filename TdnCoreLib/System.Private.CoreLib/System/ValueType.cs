namespace System;

public abstract class ValueType
{
    public override bool Equals(object? obj)
    {
        if (null == obj)
        {
            return false;
        }

        var type = GetType();
        if (type != obj.GetType())
        {
            return false;
        }

        // TODO: compare bits 
        
        return false;
    }

    public override int GetHashCode()
    {
        return -1;
    }

    public override string? ToString()
    {
        return GetType().ToString();
    }
}