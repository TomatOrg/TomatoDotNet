using System.Runtime.InteropServices;

namespace System.Reflection;

[StructLayout(LayoutKind.Sequential)]
internal sealed class RuntimeLocalVariableInfo : LocalVariableInfo
{

    private RuntimeTypeInfo _localType;
    private int _localIndex;
    private bool _isPinned;

}