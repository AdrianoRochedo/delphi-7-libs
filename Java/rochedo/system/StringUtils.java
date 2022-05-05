/*
 * Created on 15/03/2005
 *
 */
package rochedo.system;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * @author Adriano Rochedo Conceicao
 * @version 1.0
 */
public class StringUtils  
{
    public static String fill(char c, int count) 
    {
        StringBuffer s = new StringBuffer();
        s.setLength(count);
        for (int i = 0; i < count; i++) s.setCharAt(i, c);
        return s.toString();
    }
    
    public static String spaces(int count) 
    {
        return fill(' ', count);
    }
    
    public static double toDouble(String s)
    {
        return Double.parseDouble(s);
    }
    
    public static int toInt(String s)
    {
        return Integer.parseInt(s);
    }
    
    public static Date toDate(String s)
    {        
        Date d;
        try
          { 
             d = dateFormatter.parse(s);
          }
        catch (ParseException e)
          {
             d = null;
          }
        return d; 
    }
    
    public static Date toDate(String s, String format)
    {        
        Date d;
        SimpleDateFormat f = new SimpleDateFormat (format);
        try
          { 
             d = f.parse(s);
          }
        catch (ParseException e)
          {
             d = null;
          }
        return d; 
    }
    
    public static String toStr(Date d)
    {
        return dateFormatter.format(d);
    }
    
    public static String toStr(double v)
    {
        return String.valueOf(v);
    }
    
    public static String toStr(int v)
    {
        return String.valueOf(v);
    }
    
    public static void setDateFormat(String format)
    {
        dateFormatter = new SimpleDateFormat (format);
    }

    // Dados privados -----------------------------------------------------------------------------
    
    private static SimpleDateFormat dateFormatter = new SimpleDateFormat ("dd/MM/yyyy");
 }
