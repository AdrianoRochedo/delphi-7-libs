using System;
using System.Web;
using System.Data;
using System.Data.SqlClient;
using MySQLDriverCS;

namespace Rochedo.Data {

  public abstract class SQL : IDisposable {

    // protected Fields and Methods ---------------------------------------------

    protected IDbConnection FConn;
    protected string FCommand;

    // public Fields and Methods ----------------------------------------------

    public SQL ()
    {

    }

    public void Dispose()
    {
      CloseConnection();
    }

    public SQL (IDbConnection Connection)
    {
      FConn = Connection;
    }

    public override string ToString()
    {
      return "SQL: " + FCommand;
    }

    public void CloseConnection()
    {
      FConn.Close();
    }

    // Devera ser utilizado com comandos que retornem um conjunto de
    // dados, tais como o "SELECT".
    public abstract IDataReader Open();

    // Utilize este método para comandos que não retornem resultados
    // tais como "Update, Insert, Delete, etc"
    public abstract int Exec();

    // properties -------------------------------------------------------------

    public string Command
    {
      get { return FCommand;  }
      set { FCommand = value; }
    }

    public IDbConnection Connection
    {
      get { return FConn;  }
      set { FConn = value; }
    }

  } // class SQL


/*******************************************************************
 MySQL:
    Ex:
         public static void Main(string[] args)
         {
             Console.WriteLine("MySQL Teste!");

             // Uma coneccao sera criada automaticamente
             SQL sql = new MySQL();
             sql.Command = "SELECT * FROM usuarios";

             MySQLDataReader r = (MySQLDataReader) sql.Open();
             while (r.Read())
             {
                 Console.WriteLine(r.GetString(1) + " - " + r.GetString(2));
             }
         }
********************************************************************/


  public class MySQL : SQL {

    // Neste constructor uma coneccao é criada automaticamente
    public MySQL ()
    {
      MySQL_Database DB = new MySQL_Database(true);
      FConn = (DB.Conn as IDbConnection);
    }

    
    // Cria uma instancia utilizando uma coneccao existente
    public MySQL (IDbConnection Connection) : base (Connection) { }

    public override int Exec()
    {
      MySQLCommand c = new MySQLCommand(FCommand, (MySQLConnection) FConn);
      FConn.Open();

      return c.ExecuteNonQuery();
    }

    public override IDataReader Open()
    {
      MySQLCommand c = new MySQLCommand(FCommand, (MySQLConnection) FConn);
      FConn.Open();

      MySQLDataReader r = c.ExecuteReaderEx();

      return r;
    }

  } // class MySQL

}