namespace Corelib
{
    public class Program
    {

class Test
{
    public int a;
    public int b;

    public int Add()
    {
        return this.a + this.b;
    }
}

public static int Main()
{
    var test = new Test
    {
        a = 123,
        b = 456
    };
    return test.Add();
}
        
    }
}