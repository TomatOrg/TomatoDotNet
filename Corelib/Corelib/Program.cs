namespace Corelib
{
    public class Program
    {

struct Test
{
    public int a;
    public int b;
}

public static int Main()
{
    var test = new Test
    {
        a = 123,
        b = 456
    };
    return test.a + test.b;
}
        
    }
}