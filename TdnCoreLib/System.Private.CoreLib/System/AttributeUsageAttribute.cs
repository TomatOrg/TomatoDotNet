namespace System;

[AttributeUsage(AttributeTargets.Class, Inherited = true)]
public sealed class AttributeUsageAttribute : Attribute
{
    private readonly AttributeTargets _attributeTarget;
    private bool _allowMultiple;
    private bool _inherited;

    internal static readonly AttributeUsageAttribute Default = new AttributeUsageAttribute(AttributeTargets.All);

    public AttributeUsageAttribute(AttributeTargets validOn)
    {
        _attributeTarget = validOn;
        _inherited = true;
    }

    internal AttributeUsageAttribute(AttributeTargets validOn, bool allowMultiple, bool inherited)
    {
        _attributeTarget = validOn;
        _allowMultiple = allowMultiple;
        _inherited = inherited;
    }

    public AttributeTargets ValidOn => _attributeTarget;

    public bool AllowMultiple
    {
        get => _allowMultiple;
        set => _allowMultiple = value;
    }

    public bool Inherited
    {
        get => _inherited;
        set => _inherited = value;
    }
}