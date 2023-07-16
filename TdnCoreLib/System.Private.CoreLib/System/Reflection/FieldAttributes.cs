namespace System.Reflection;

[Flags]
public enum FieldAttributes
{

    Assembly = 3,
    FamANDAssem = 2,
    Family = 4,
    FamORAssem = 5,
    FieldAccessMask = 7,
    HasDefault = 32768,
    HasFieldMarshal = 4096,
    HasFieldRVA = 256,
    InitOnly = 32,
    Literal = 64,
    NotSerialized = 128,
    PinvokeImpl = 8192,
    Private = 1,
    PrivateScope = 0,
    Public = 6,
    ReservedMask = 38144,
    RTSpecialName = 1024,
    SpecialName = 512,
    Static = 16,
    
}