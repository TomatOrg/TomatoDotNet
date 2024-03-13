namespace System.Diagnostics;

[AttributeUsage(AttributeTargets.Method | AttributeTargets.Class, AllowMultiple = true)]
public sealed class ConditionalAttribute : Attribute
{

    public string ConditionString { get; }
    
    public ConditionalAttribute(string conditionString)
    {
        ConditionString = conditionString;
    }
}