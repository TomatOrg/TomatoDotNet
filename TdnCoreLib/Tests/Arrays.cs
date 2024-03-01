using System;

namespace Tests;

public class Arrays
{
    
    private static bool TestSimple1()
    {
        int SIZE = 10;

        //Create an array that is jagged.
        // in last 2d, the array looks like:
        //  Int32
        //  Int32 Int32 
        //  Int32 Int32 Int32 
        //  Int32 Int32 Int32 Int32 
        //  Int32 Int32 Int32 Int32 Int32 
        //

        Int32 [][][][][] foo = new Int32[SIZE][][][][];
        int i,j,k,l,m;
        Int64 sum=0;

        for(i=0;i<SIZE;i++)
        {
            foo[i] = new Int32[i][][][];
            for(j=0;j<i;j++)
            {
                foo[i][j] = new Int32[j][][];
                for(k=0;k<j;k++)
                {
                    foo[i][j][k] = new Int32[k][];
                    for(l=0;l<k;l++)
                    {
                        foo[i][j][k][l] = new Int32[l];
                        for(m=0;m<l;m++)
                        {
                            foo[i][j][k][l][m] = i*j*k*l*m;
                        }
                    }
                }
            }
        }

        for(i=0;i<SIZE;i++)
        for(j=0;j<i;j++)
        for(k=0;k<j;k++)
        for(l=0;l<k;l++)
        for(m=0;m<l;m++)
        { 
            sum+=foo[i][j][k][l][m];
        }
        
        if(sum==269325)
        {
            return true;
        }
        else
        {
            return false;
        }
    }
    
    public static bool Run()
    {
        return TestSimple1();
    }
}