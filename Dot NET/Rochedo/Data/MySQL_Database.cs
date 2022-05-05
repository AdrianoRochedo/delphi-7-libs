using System;
using System.Data;
using MySQLDriverCS;

namespace Rochedo.Data {

  public class MySQL_Database : Rochedo.Data.Database {

      public MySQL_Database(bool Open) : base (Open)
      {
      }

      // Protected Methods ----------------------------------------------------

      protected override string InitConnectionString() {
        return  "Location=localhost; Data Source=alm; User ID=root; " +
                "Port=3306; Extended Properties=\"\"";
      }

      protected override IDbConnection CreateConnection(string ConnectionString)
      {
        return new MySQLConnection(ConnectionString);
      }

  } // class

}  // namespace
