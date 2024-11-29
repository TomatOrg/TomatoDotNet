
using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace Tests;

public static class Program
{

    public interface ICovariant<out T> { }
    public interface IContravariant<in T> { }

    public class CovariantExample<T> : ICovariant<T> { }
    public class ContravariantExample<T> : IContravariant<T> { }

    public class Fruit { }
    public class Apple : Fruit { }

    public static void Covariant(ICovariant<Fruit> fruit)
    {
        Console.WriteLine(fruit.ToString());
    }
    
    public static int Main()
    {
        var fruit = new CovariantExample<Fruit>();
        var apple = new CovariantExample<Apple>();
        
        Covariant(fruit);
        // Covariant(apple);
        
        return 0;
    }
}