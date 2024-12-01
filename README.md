# Tomato.NET

This is a custom C# runtime made for TomatOS with 3 design goals:
- Hostable: should be easy to write a host for it so we can run it in kernel space 
- Fast: We compromised some C# features (see below) in order to have the runtime be faster
- Enforce memory safety: We specifically enforce the memory safety at the IL level, with every method getting verified 
                         to be memory safe, this is to ensure that loading an untrusted binary would not result in
                         memory safety and unsafe related issues.


## Implemented

- Mostly complete basic MSIL support 
  - Properly verifies all operations are memory safe 
  - Simple object and struct operations 
  - Type and Method generics
  - Interfaces
  - Delegates
- (mostly) Full support for references
  - properly checking references don't escape
  - Ref-struct support
  - Readonly reference tracking

### To be implemented
- explicit scoped and unscoped reference support
- safe stackalloc (which returns Span)
  - the generated MSIL is not safe by default
- Exceptions
    - This requires unwind support from the jit which is currently missing
- Much more of the standard library

## Implementation details

- The GC is controlled by the host and not the runtime
  - This is to allow control for the host to use OS features for it

- Objects have a small 8 bytes header
  - 4 byte for VTable pointer
  - 4 bytes currently reserved

- Optimized type checking
  - Uses a simple bitmask to encode the type hierarchy for normal objects 
    - Type check translates to `(instance->vtable->hierarchy & <constant mask>) == (constant id)`
  - Uses prime products for interfaces 
    - Type check translates to `(instance->vtable->product % <constant id>) == 0`
    - This is still experimental and I am not sure how well this is going to scale  
  - Uses vtable comparison for boxed value types
    - Type check translates to `(instance->vtable == <constant vtable>)`
  - All of this makes it so type switches have a higher potential for optimizations and inline 

- Interfaces are implemented using fat pointers, this allows for very fast virtual dispatch, and the vtable is 
  built so any object/interface->interface upcast is a single constant pointer movement, making the interface 
  casts also very cheap without needing to touch the this pointer, allowing boxing easily as well.
  - Variance between object<->interface is not supported but is planned
  - Interface calls translate to `interface.vtable[<constnt function offset>](interface.instance, ...)`
  - Interface upcast is `&interface.vtable[<constant offset>]`
  - It does mean storing an interface takes 16 bytes instead of 8

- Delegates are implemented using fat pointers, this makes it so most delegate operations don't require a single 
  allocation, and in most cases will require only a single indirection.
    - Variance between object<->interface is not supported but is planned
  - Delegate calls translate to `delegate.function(delegate.instance, ...)`
  - For static types a small stub is placed that turns the function into `thiscall` (obviously ignoring the this)
  - It does mean storing a delegate takes 16 bytes instead of 8

## Compromises

- Only 64bit support, we have no plans on supporting 64bit, this simplifies alot of design choices

- No support for array variance
    - requires more hidden type checks to work:
        ```
        object[] a = new string[]{ "123" };
        // would need to fail at runtime
        a[0] = new A();
        ```
    - Given how interfaces and delegates are implemented as fat pointers, the cast
      doesn't really work on anything that does object<->interface in the cast

- No support for virtual generic methods
  - Requires a more complex (and expensive) runtime lookup and potentially jitting of a method  

- Boxing a delegate into either a Delegate/MulticastDelegate/object is currently not supported
  - We might in the future support such things by allocating

- Assembly unload is not supported

- Reflection is not supported 
  - Main reason is that it allows for bypassing type safety, maybe eventually we will figure a way to support it nicely
