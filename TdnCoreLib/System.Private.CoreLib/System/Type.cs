using System.Reflection;

namespace System;

public abstract class Type : MemberInfo
{

    protected Type()
    {
    }

    public static Type? GetTypeFromHandle(RuntimeTypeHandle handle)
    {
        return handle._value;
    }
    
}