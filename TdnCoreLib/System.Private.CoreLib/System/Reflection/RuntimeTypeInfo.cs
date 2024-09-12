using System.Runtime.InteropServices;

namespace System.Reflection;

[StructLayout(LayoutKind.Sequential)]
internal sealed class RuntimeTypeInfo : TypeInfo
{

    private RuntimeTypeInfo _declaringType;
    private RuntimeModule _module;
    private string _name;
    private int _metadataToken;

    private RuntimeTypeInfo _arrayType;
    private RuntimeTypeInfo _byrefType;
    private RuntimeTypeInfo _pointerType;

    private string _namespace;
    private TypeAttributes _attributes;

    private RuntimeTypeInfo _baseType;
    private ulong _genericTypeInstances;
    
    private RuntimeConstructorInfo[] _declaredConstructors;
    private RuntimeMethodInfo[] _declaredMethods;
    private RuntimeFieldInfo[] _declaredFields;

    private RuntimeMethodInfo[] _vtable;
    private ulong _jitVtable;

    private RuntimeTypeInfo _declaredNestedTypes;
    private RuntimeTypeInfo _nextNestedType;

    private RuntimeTypeInfo _elementType;
    private RuntimeTypeInfo _enumUnderlyingType;

    private uint _stackSize;
    private uint _stackAlignment;
    private uint _heapSize;
    private uint _heapAlignment;
    private uint _packing;

    private RuntimeMethodInfo _declaringMethod;
    private RuntimeTypeInfo[] _genericArguments;
    private RuntimeTypeInfo[] _genericParameterConstraints;
    private RuntimeTypeInfo _genericTypeDefinition;
    private GenericParameterAttributes _genericParameterAttributes;
    private uint _genericParameterPosition;

    private uint _flags;
    
    public override string ToString()
    {
        // TODO: something more correct or idk
        return _name;
    }
}