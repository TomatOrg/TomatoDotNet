using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public class Object
{

    private uint _vtable;
    private uint _itable;
    internal byte _monitorLock;
    internal byte _monitorCondVar;
    internal ushort _monitorOwnerThreadId;
    internal uint _monitorDepthAndGcFlags;
    private Type _type;
    private unsafe void* _next;

    ~Object()
    {
    }

    public Type GetType()
    {
        return _type;
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