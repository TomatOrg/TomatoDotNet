using System.Runtime.InteropServices;

namespace System;

[StructLayout(LayoutKind.Sequential)]
public readonly struct Guid
{

    private readonly int _a;
    private readonly short _b;
    private readonly short _c;
    private readonly byte _d;
    private readonly byte _e;
    private readonly byte _f;
    private readonly byte _g;
    private readonly byte _h;
    private readonly byte _i;
    private readonly byte _j;
    private readonly byte _k;
    
}