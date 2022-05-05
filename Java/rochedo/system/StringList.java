/*
 * Created on 18/03/2005
 *
 */
package rochedo.system;

import java.io.*;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * @author Adriano Rochedo Conceicao
 * @version 1.0
 */
public class StringList {
    
    public StringList()
    {
        List = new ArrayList(); 
    }
    
    public String get(int index)
    {
        return (String) List.get(index);
    }    
    
    public void set(int index, String s)
    {
        List.set(index, s);
    }

    public int add(String s)
    {
        List.add(s);
        return List.size() - 1;
    }

    public void clear()
    {
        List.clear();
    }
    
    public int length()
    {
        return List.size();   
    }    

    public boolean loadFromFile(String filename) 
    {
        try
          {
          this.error = "";
          BufferedReader in = new BufferedReader ( new FileReader(filename) );
          this.clear();
          String s;
          while ((s = in.readLine()) != null) this.add(s);
          in.close();
          return true;
          }
        catch (IOException e)
          {
          this.error = e.getMessage();  
          return false;
          }
    }
    
    public boolean saveToFile(String filename)
    {
        try
          {
          this.error = "";  
          PrintWriter out = new PrintWriter ( new FileWriter(filename) );
          Iterator it = List.iterator();
          while (it.hasNext()) 
             out.println((String) it.next());
          out.close();
          return true;
          }
        catch (IOException e)
          {
          this.error = e.getMessage();
          return false;  
          }
    }
    
    public String getLastError()
    {
        return error;
    }
    
    // Dados privados ---------------------------------------------------------
    
    private ArrayList List;
    private String error;
}
