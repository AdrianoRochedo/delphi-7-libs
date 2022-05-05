using System;
using System.Data;
using MySQLDriverCS;

namespace Rochedo.Data {

  public class MySQL_Connection : Rochedo.Data.Connection {

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
