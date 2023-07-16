using System.Runtime.InteropServices;

namespace System.Reflection;

[StructLayout(LayoutKind.Sequential)]
internal sealed class RuntimeMethodInfo : MethodInfo
{
    
    private ParameterInfo[] _parameters;
    private MethodAttributes _attributes;
    private MethodImplAttributes _methodImplFlags;
    private RuntimeMethodBody _methodBody;
    private ParameterInfo ReturnParameter;

}