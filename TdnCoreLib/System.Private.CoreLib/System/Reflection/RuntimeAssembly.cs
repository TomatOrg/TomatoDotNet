using System.Runtime.InteropServices;

namespace System.Reflection;

[StructLayout(LayoutKind.Sequential)]
internal sealed class RuntimeAssembly : Assembly
{
    private RuntimeTypeInfo[] _typeDefs;
    private MethodBase[] _methodDefs;
    private RuntimeFieldInfo[] _fields;
}