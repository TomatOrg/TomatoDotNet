namespace System.Reflection;

[Flags]
public enum ExceptionHandlingClauseOptions
{
    Clause = 0,
    Fault = 4,
    Filter = 1,
    Finally = 1,
}