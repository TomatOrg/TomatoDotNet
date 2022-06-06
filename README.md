# TinyDotNet

A dotnet runtime designed to be more light weight and also work on embedded (inside pentagon more specifically) 
but also has a linux runtime for ease of development and use.

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
- Full support for integer and floating point types
- Full support for array types
- Full support for struct types
- Full support for interface types
  - Implemented using Fat-Pointers and implicitly casting as needed
- Full support for managed references
  - Supports locals, fields and array elements
- Full support for boxing/unboxing 
- Most common CIL instructions implemented
- Visibility and Accessibility checking

### Main missing features
- Bit-shifting
- Generics
- Delegates
- Overflow math
