/*
 * Created on 13/03/2005
 *
 */
package rochedo.system;

/**
 * @author Adriano Rochedo Conceicao
 * @version 1.0
 *
 */
public class Application { 
    
    public Application(String appName, String version) 
    {
      this.appName = appName;
      this.version = version;
    }
    
    public void run()
    {
      // nothing
    }
    
    public static void write(String message)
    {
        System.out.println(message);
    }
    
    public static void write(int ident, String message)
    {
        System.out.println(StringUtils.spaces(ident) + message);
    }

    public String getCaption()
    {
      return appName + " - " + version;
    }
    
    private String appName;
    private String version;
}
