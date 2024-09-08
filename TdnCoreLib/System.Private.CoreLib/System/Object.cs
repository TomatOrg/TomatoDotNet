using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public class Object
{

    private uint _vtable;
    private uint _typeId;
    private byte _monitorLock;
    private byte _monitorCondVar;
    private ushort _reserved0;
    private uint _reserved1;

    public Object()
    {
    }
    
    ~Object()
    {
    }

    public Type GetType()
    {
        return null;
    }
    
    public virtual string? ToString()
    {
        return GetType().ToString();
    }

    public virtual bool Equals(object? obj)
    {
        return this == obj;
    }

    public virtual int GetHashCode()
    {
        return -1;
    }

    protected object MemberwiseClone()
    {
        return null;
    }
    
    public static bool Equals(object? objA, object? objB)
    {
        if (objA == objB)
        {
            return true;
        }

        if (objA == null || objB == null)
        {
            return false;
        }

        return objA.Equals(objB);
    }

    public static bool ReferenceEquals(object? objA, object? objB)
    {
        return objA == objB;
    }

}