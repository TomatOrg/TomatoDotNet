// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System;

namespace Test_commonBase_cs
{
internal class Base { };
internal class Sibling1 : Base { };
internal class Sibling2 : Base { };

public static class Repro
{
    private static int Bug(object o)
    {
        Base b = o as Sibling1;
        if (b == null)
        {
            b = o as Sibling2;
        }

        // At this point b is either null, Sibling1, or Sibling2
        if (b != null)
        {
            // But the bug makes us think it is only Sibling1 here (since we've eliminated null)
            if (b is Sibling2)
            {
                return 100;
            }
            else
            {
                return 9;
            }
        }
        return 0;
    }

    public static int Main()
    {
        return Bug(new Sibling2());
    }
}
}
