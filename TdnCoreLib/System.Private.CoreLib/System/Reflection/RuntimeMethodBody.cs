using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace System.Reflection;

[StructLayout(LayoutKind.Sequential)]
internal sealed class RuntimeMethodBody : MethodBody
{

    private RuntimeLocalVariableInfo[] _localVariables;
    private unsafe void* _il;
    private int _localSignatureMetadataToken;
    private int _ilSize;
    private int _maxStackSize;
    private bool _initLocals;

}