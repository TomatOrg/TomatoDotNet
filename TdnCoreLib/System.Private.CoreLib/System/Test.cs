using System.Reflection;

namespace System;

public class Test
{

    class Thing
    {
        public void Dispose()
        {
        }
        
        public int GetThing()
        {
            return 1;
        }
    }
    
    
    public int Add(long b)
    {
        Thing thing = new Thing();
        try
        {
            Thing thing2 = new Thing();
            try
            {
                return thing.GetThing() + thing2.GetThing();
            }
            finally
            {
                thing2.Dispose();
            }
        }
        finally
        {
            thing.Dispose();
        }
    }
    
}