using System.Runtime.InteropServices;

namespace System.Reflection;

[StructLayout(LayoutKind.Sequential)]
internal sealed class RuntimeFieldInfo : FieldInfo
{

    private FieldAttributes _attributes;
    private RuntimeTypeInfo _fieldType;
    private int _fieldOffset;

}