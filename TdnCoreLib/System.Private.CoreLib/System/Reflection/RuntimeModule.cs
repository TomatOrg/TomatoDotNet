using System.Runtime.InteropServices;

namespace System.Reflection;

[StructLayout(LayoutKind.Sequential)]
internal sealed class RuntimeModule : Module
{

    private RuntimeAssembly _assembly;

}