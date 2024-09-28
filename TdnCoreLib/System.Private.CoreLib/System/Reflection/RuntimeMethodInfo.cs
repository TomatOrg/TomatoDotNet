using System.Runtime.InteropServices;

namespace System.Reflection;

[StructLayout(LayoutKind.Sequential)]
internal sealed class RuntimeMethodInfo : MethodInfo
{
    private RuntimeTypeInfo _declaringType;
    private RuntimeModule _module;
    private string _name;
    private int _metadataToken;
    
    private ParameterInfo[] _parameters;
    private MethodAttributes _attributes;
    private MethodImplAttributes _methodImplFlags;
    private RuntimeMethodBody _methodBody;
    private ParameterInfo _returnParameter;

    private RuntimeTypeInfo[] _genericArguments;
    private RuntimeMethodInfo _genericMethodDefinition;
    private ulong _genericMethodInstances;

    private ulong _methodPtr;
    private ulong _methodSize;

    private ulong _thunkPtr;
    private ulong _thunkSize;
    
    private uint _vtableOffset;

    public override string Name => _name;
}