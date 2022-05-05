using System;
using System.Data;

namespace Rochedo.Data {

  public abstract class Connection {

      // Private Fields -------------------------------------------------------

      private string F_SQLConnectString;

      // Protected Fields and Methods -----------------------------------------

      protected IDbConnection F_DbConnection;

      protected abstract string InitConnectionString();
      protected abstract IDbConnection CreateConnection(string ConnectionString);

      // Public Methods -------------------------------------------------------

      public Connection()
      {
        F_SQLConnectString = InitConnectionString();
      }

      public Connection(bool Open)
      {
      	F_SQLConnectString = InitConnectionString();
        if (Open) this.Open();
      }

      ~Connection()
      {
         F_DbConnection.Close();
      }

      public virtual void ConnectionError(Exception e)
      {
        throw e;
      }

      public virtual void Open()
      {
        if ( F_DbConnection == null ||
             F_DbConnection.State == System.Data.ConnectionState.Closed ) {
             try {
               F_DbConnection = CreateConnection(F_SQLConnectString);
               F_DbConnection.Open();
             }
             catch(Exception e) {
               ConnectionError(e);
             }
        }
      }

      // Properties -----------------------------------------------------------

      public IDbConnection Conn
      {
        get { return F_DbConnection; }
      }

  } // class

}  // namespace
