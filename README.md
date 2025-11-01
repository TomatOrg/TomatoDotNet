# Tomato.NET

This is a custom C# runtime made for TomatOS with 3 design goals:
- Hostable: should be easy to write a host for it so we can run it in kernel space 
- Fast: We compromised some C# features (see below) in order to have the runtime be faster
- Enforce memory safety: We specifically enforce the memory safety at the IL level, with every method getting verified 
                         to be memory safe, this is to ensure that loading an untrusted binary would not result in
                         memory safety and unsafe related issues.

## Implemented

- Mostly complete basic C#/MSIL support
  - Verifies all operations are memory and type safe
  - Classes and Structs
  - Generics
  - Interfaces
    - Virtual static methods
    - Default implementations
  - Delegates
  - Finally clauses
- Full support for references
  - Ensures references don't escape
  - Readonly reference tracking
  - Ref-structs
  - Safe stackalloc into a Slice
- Corelib
  - All integer methods
  - All double methods
  - Most string methods (missing alot of manipulation functions to do with Culture)
  - All span methods and extensions
  - All the debug class

### Short term TODO

These are things we are planning todo on the short term
- Generic constraints verification
- Culture support
- Custom attributes

### Long term TODO

These are planned but will take time until we will actually have proper full support for them
- Exceptions (missing jit support)
- Virtual generic methods (Need a good design)
- Boxing a delegate into either a Delegate/MulticastDelegate/object (changes allocation semantics greatly)
- Reflection (Need a good design)
- Assembly unload (No idea how to even approach)
- Much more of the standard library

## Compiling

### Library

First you will want to invoke the makefile from your makefile giving the following variables:
- `CC` (defaults to `clang`): The compiler to use
- `AR` (defaults to `llvm-ar`): The archive utility to use
- `LD` (defaults to `ld.lld`): The linker to use
- `CFLAGS`: additional CFLAGS to add, you will most likely want at least optimization level
- `DEBUG`: should we compile as debug or release

We have some rust code in the project, these are the settings that can be controled to compile it:
- `CARGO_TARGET_NAME` (defaults to `x86_64-unknown-none`): The target name to pass to cargo
- `REUSTUP_TOOLCHAIN` (defaults to `nightly-2025-05-07`): The toolchain to pass to rustup
- `RUSTC_FLAGS`: additional flags to pass to rustc

Then you will want to include it in your source:
- Include paths:
  - `./include`: This has the main Tomato.net headers
  - `./libs/spidir/c-api/include`: This has the JIT's includes, which are sometimes needed by the host headers
- Compiler flags:
  - `-fms-extensions -Wno-microsoft-anon-tag`: An extension that we use for defining objects more easily
- Libraries:
  - `./out/{debug,release}/libtdn.a`: The actual static library
- C# Core libraries:
  - `./out/{debug,release}/bin/System.Private.CoreLib.dll`: The main corelib, must be loaded first

All you need to do then is to call `tdn_load_assembly_from_memory` giving the corelib, once you loaded the corelib 
you can continue and load the rest of the DLLs you need. 

For each one (excluding the corelib) you will most likely want to also call the entry point:
```c
  ASSERT(tdn_jit_method(tests->EntryPoint) == TDN_NO_ERROR);
  int (*entry_point)() = tests->EntryPoint->MethodPtr;
  int return_value = entry_point();
```

### Linux host

The repo includes an example linux host which can be built:
```bash
make -C host/linux
```

And then under `out/bin/tdn.elf` you can run the binary

## Implementation details

- The GC is controlled by the host and not the runtime
  - This is to allow control for the host to use OS features for it

- Objects have a 16 bytes header
  - 8 byte for VTable pointer
  - 2 bytes for flags
  - 2 byte mutex depth
  - 2 byte mutex thread id
  - 1 byte condvar
  - 1 byte mutex

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

- Only 64bit support, we have no plans on supporting 32bit instruction sets, this simplifies alot of design choices

- All fat pointers must be 16-byte aligned to ensure single-copy-atomicity for the fat pointers themselves, this also
  limits the supported arches only to those who have 16-byte single-copy-atomicity (like x86-64 and aarch64)

- No support for array variance
    - requires more hidden type checks to work:
        ```
        object[] a = new string[]{ "123" };
        // would need to fail at runtime
        a[0] = new A();
        ```
    - Given how interfaces and delegates are implemented as fat pointers, the cast
      doesn't really work on anything that does object<->interface in the cast
