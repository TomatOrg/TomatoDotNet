using System.Runtime.InteropServices;

namespace System.Reflection;

[StructLayout(LayoutKind.Sequential)]
internal sealed class RuntimeFieldInfo : FieldInfo
{

    private RuntimeTypeInfo _declaringType;
    private RuntimeModule _module;
    private string _name;
    private int _metadataToken;
    
    private FieldAttributes _attributes;
    private RuntimeTypeInfo _fieldType;
    private ulong _jitFieldId;
    private int _fieldOffset;
    private uint _flags;

}