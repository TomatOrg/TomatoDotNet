namespace System.Runtime.CompilerServices;

[AttributeUsage(AttributeTargets.Assembly | AttributeTargets.Module | AttributeTargets.Class | AttributeTargets.Method)]
public class CompilationRelaxationsAttribute : Attribute
{

    public int CompilationRelaxations { get; }
    
    public CompilationRelaxationsAttribute(int relaxations)
    {
        CompilationRelaxations = relaxations;
    }

    public CompilationRelaxationsAttribute(CompilationRelaxations relaxations)
    {
        CompilationRelaxations = (int)relaxations;
    }

}