# TinyDotNet

A dotnet runtime designed to be more light weight and also work on embedded (inside pentagon more specifically) 
but also has a linux runtime for ease of development and use.

In addition to attempting to be small, it also attempts to fully verify the CIL that is being run on top of it 
to be fully memory safe, this allows for proper Type isolation and security, but does prevent some of the more 
advanced and unsafe features of C# to not be usable (for example `stackalloc`).

Another bonus with this design is that porting to new kernels and hosted environments should not be too hard 
alongside the ability to embed inside programs as something more light weight than mono.

## Progress

Right now the main work is on the runtime itself, the main features of the runtime:
- On-the-fly Garbage Collector for pause free garbage collection
  - Including support for finalization and reviving
  - A really cheap write-barrier
- Full support for reference types
  - With abstract/virtual methods support
  - Upcasting fully implemented
- Full support for exceptions
  - Done using generated control flow 
- Full support for integer and floating point types
- Full support for array types
- Full support for struct types
- Full support for interface types
  - Implemented using Fat-Pointers and implicitly casting as needed
  - Casting down from one interface/object to another is essentially zero cost (offseting the vtable of the parent)
- Full support for managed references
  - Supports locals, fields and array elements
- Full support for boxing/unboxing 
  - Including Nullable
- Full support for Generics (Type and Methods)
- Full support for delegates and multicast delegates
  - Missing some corelib methods to make them fully work
- Most common CIL instructions implemented
- Visibility and Accessibility checking

Note that the runtime is still not stable and even tho the features are implemented bugs are likely to be found, we try to fix anything we find.

### Corelib support
- Basic types 
  - All built-in types
  - Basic exceptions 
- Basic reflection
  - The base classes are defined, no actual methods or attributes 
- Basic collections 
  - Enumerators
  - Basic generic List implementation 
- Mostly implemented threading library
  - Interlocked class (missing some signatures)
  - WaitHandle with WaitEvent, Mutex, Semaphore 
  - Thread class
  - Monitor support
    - no support for timeout
- Support for Span
  - Only from array types, as void* is not valid

### Main missing features
- Async runtime support (wip)
- Proper for `ref struct` (check they are only stored in other ref structs or on the stack)
- Proper collections library 
- String manipulation
- Streams core functionality 
- Controlled-mutability managed pointers
- Overflow math (will come once MIR supports them)
- Stack trace (will need some form of JIT support)
