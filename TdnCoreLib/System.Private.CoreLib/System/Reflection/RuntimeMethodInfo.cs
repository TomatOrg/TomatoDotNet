using System.Runtime.InteropServices;

namespace System.Reflection;

[StructLayout(LayoutKind.Sequential)]
internal sealed class RuntimeMethodInfo : MethodInfo
{
    
    private ParameterInfo[] _parameters;
    private MethodAttributes _attributes;
    private MethodImplAttributes _methodImplFlags;
    private RuntimeMethodBody _methodBody;
    private ParameterInfo _returnParameter;

    private RuntimeTypeInfo[] _genericArguments;
    private RuntimeMethodInfo _genericMethodDefinition;
    private unsafe void* _genericMethodInstances;

    private ulong _jitMethod;
    private uint _flags;

}