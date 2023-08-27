using System.Runtime.InteropServices;

namespace System.Reflection;

[StructLayout(LayoutKind.Sequential)]
internal sealed class RuntimeExceptionHandlingClause : ExceptionHandlingClause
{

    private RuntimeTypeInfo _catchType;
    private int _flags;
    private int _filterOffset;
    private int _handlerLength;
    private int _handlerOffset;
    private int _tryLength;
    private int _tryOffset;

}