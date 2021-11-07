namespace System
{
    public readonly struct UIntPtr
    {

        private readonly unsafe void* _value;
    
        public static int Size => sizeof(ulong);
        public static UIntPtr MaxValue => (UIntPtr)ulong.MaxValue;
        public static UIntPtr MinValue => (UIntPtr)ulong.MinValue;

        public static readonly UIntPtr Zero;

        public unsafe UIntPtr(uint value)
        {
            _value = (void*)value;
        }
        
        public unsafe UIntPtr(ulong value)
        {
            _value = (void*)value;
        }

        public unsafe UIntPtr(void* value)
        {
            _value = value;
        }

        public unsafe uint ToUInt32()
        {
            return checked((uint)_value);
        }
        
        public unsafe ulong ToUInt64()
        {
            return (ulong)_value;
        }

        public unsafe void* ToPointer()
        {
            return _value;
        }

        #region Cast operators

        public static explicit operator UIntPtr(uint value)
        {
            return new UIntPtr(value);
        }
        
        public static explicit operator UIntPtr(ulong value)
        {
            return new UIntPtr(value);
        }

        public static unsafe explicit operator UIntPtr(void* value)
        {
            return new UIntPtr(value);
        }

        public static explicit operator uint(UIntPtr value)
        {
            return value.ToUInt32();
        }
        
        public static explicit operator ulong(UIntPtr value)
        {
            return value.ToUInt64();
        }
        
        #endregion

        #region Comparison operators

        public static unsafe bool operator ==(UIntPtr value1, UIntPtr value2)
        {
            return value1._value == value2._value;
        }
        
        public static unsafe bool operator !=(UIntPtr value1, UIntPtr value2)
        {
            return value1._value != value2._value;
        }
        
        #endregion

    }
}