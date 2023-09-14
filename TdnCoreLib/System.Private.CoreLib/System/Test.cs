using System.Reflection;

namespace System;

public class Test
{

    class Lol
    {

        public void Something()
        {
            
        }
        
        public void Dispose()
        {
            
        }
        
    }
    
    public void Do()
    {
        var lol = new Lol();
        try
        {
            lol.Something();
        }
        finally
        {
            lol.Dispose();
        }
    }
    
}