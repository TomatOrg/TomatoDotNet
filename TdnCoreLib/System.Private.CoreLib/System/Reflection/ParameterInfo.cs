using System.Runtime.InteropServices;

namespace System.Reflection;

[StructLayout(LayoutKind.Sequential)]
public class ParameterInfo
{

    private ParameterAttributes _attributes;
    private RuntimeTypeInfo _parameterType;
    private MemberInfo _member;
    private string _name;
    private int _position;
    
    protected ParameterInfo()
    {
    }
    
}