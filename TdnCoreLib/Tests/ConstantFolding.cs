namespace Tests;

public class ConstantFolding
{

    private static void Use(int a) { }
    
    public static bool TestFoldingExtendsInt32On64BitHosts()
    {
        var r1 = 31;
        // "Poisoned" value.
        var s1 = 0b11 << r1;

        if (s1 == 0b11 << 31)
        {
            return true;
        }

        // Just so that Roslyn actually uses locals.
        Use(s1);
        Use(r1);

        return false;
    }
    
    public static bool Run()
    {
        if (!TestFoldingExtendsInt32On64BitHosts()) return false;

        return true;
    }
    
}