namespace System;

public interface IComparable
{
    int CompareTo(object? other);
}

public interface IComparable<in T>
{
    int CompareTo(T? other);
}