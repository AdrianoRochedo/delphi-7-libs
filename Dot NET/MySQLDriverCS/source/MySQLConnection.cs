#region LICENSE
/*
	MySQLDriverCS: An C# driver for MySQL.
	Copyright (c) 2002 Manuel Lucas Viñas Livschitz.

	This file is part of MySQLDriverCS.

    MySQLDriverCS is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    MySQLDriverCS is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MySQLDriverCS; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
#endregion

using System;
using System.Data;
using System.Runtime.InteropServices;

namespace MySQLDriverCS
{
	/// <summary>
	/// This class is IDbConnection compliant so take a look into MSDN help to understand how it works. 
	/// </summary>
	/// <remarks>All members are supported now (2002-10-28)</remarks>
	public class MySQLConnection : IDbConnection
	{
		private string connectionString;
		internal string dbname = null;
		unsafe internal void* handle = null;
		#region Constructors & Destructor
		/// <summary>
		/// Creates a connection
		/// </summary>
		public MySQLConnection()
		{
		}
		/// <summary>
		/// Creates a connection with a connection string
		/// </summary>
		/// <param name="cs"></param>
		public MySQLConnection(string cs)
		{
			this.connectionString=cs;
		}
		private bool bDisposed = false;
		/// <summary>
		/// Dispose destructor
		/// </summary>
		unsafe public void Dispose()
		{
			if(bDisposed) return;
			this.Close();
			bDisposed=true;
		}
		#endregion
		#region ConnectionString property
		/// <summary>
		/// Gets or sets the string used to open a database.
		/// </summary>
		public string ConnectionString
		{
			get
			{
				return connectionString;
			}
			set
			{
				connectionString = value;
			}
		}
		#endregion
		#region Port property
		/// <summary>
		/// Gets the time to wait while trying to establish a connection before terminating the attempt and generating an error.
		/// </summary>
		public int Port 
		{
			get
			{ 
				string val = FindValueInCS("Port"); 
				if(val=="") 
					return 3306; 
				else 
					return Convert.ToInt32(val); 
			}
		}
		#endregion
		#region ConnectionTimeout property
		/// <summary>
		/// Gets the time to wait while trying to establish a connection before terminating the attempt and generating an error.
		/// </summary>
		public int ConnectionTimeout 
		{
			get
			{ 
				string val = FindValueInCS("timeout"); 
				if(val=="") 
					return 60; 
				else 
					return Convert.ToInt32(val); 
			}
		}
		#endregion
		#region Database property
		/// <summary>
		/// Gets the name of the current database or the database to be used once a connection is open.
		/// </summary>
		public string Database
		{
			get
			{
				if(dbname!=null)
					return dbname;
				else
					return FindValueInCS("Data Source=");
			}
		}
		#endregion
		#region FindValueInCS
		private string FindValueInCS(string keyword)
		{
			keyword = keyword.ToLower();
			string connectionStringLo = connectionString.ToLower();
			int pos = connectionStringLo.IndexOf(keyword);
			if(pos==-1)
			{
				return "";
			}
			else
			{
				pos=connectionString.IndexOf('=',pos)+1;
				int pos2=connectionString.IndexOf(';',pos);
				if(pos2==-1)
				{
					return connectionString.Substring(pos);
				}
				else
				{
					return connectionString.Substring(pos,pos2-pos);
				}
			}
		}
		#endregion
		#region State property
		/// <summary>
		/// Gets the current state of the connection.
		/// </summary>
		unsafe public ConnectionState State 
		{
			get
			{
				if(handle==null)
					return ConnectionState.Closed;
				else
					return ConnectionState.Open;
			}
		}
		#endregion
		#region BeginTransaction(IsolationLevel il)
		/// <summary>
		/// Begins a transaction
		/// </summary>
		/// <param name="il"></param>
		/// <returns></returns>
		public IDbTransaction BeginTransaction(IsolationLevel il)
		{
			return new MySQLTransaction(this,il);
		}
		#endregion
		#region BeginTransaction 
		/// <summary>
		/// Begins a transaction
		/// </summary>
		/// <returns></returns>
		public IDbTransaction BeginTransaction()
		{
			return new MySQLTransaction(this,IsolationLevel.Unspecified);
		}
		#endregion
		#region ChangeDatabase
		/// <summary>
		/// Changes database
		/// </summary>
		/// <param name="databaseName"></param>
		unsafe public void ChangeDatabase(string databaseName)
		{
			if(0==CPrototypes.mysql_select_db(handle,databaseName))
				this.dbname=databaseName;
			else
			{
				throw new MySQLException("MySQLDriverCS Error: change database failed, perhaps user is not authorized to access that database."+CPrototypes.mysql_error(handle));
			}
		}
		#endregion
		#region CreateCommand 
		/// <summary>
		/// Creates an empty command linked to this connection
		/// </summary>
		/// <returns></returns>
		public IDbCommand CreateCommand()
		{
			return new MySQLCommand("",this);
		}
		#endregion
		#region Open
		/// <summary>
		/// Opens a database connection with the settings specified by the ConnectionString property of the provider-specific Connection object.
		/// </summary>
		unsafe public void Open()
		{
			string database=this.FindValueInCS("Data Source=");
			string location=this.FindValueInCS("Location=");
			string userid=this.FindValueInCS("User ID=");
			string password=this.FindValueInCS("Password=");
			string port=this.FindValueInCS("Port=");
			if(port=="") port="3306";
			if(handle!=null)
			{
				Close();
			}
			handle = CPrototypes.mysql_init(null);
			if(handle==null)
				throw new MySQLException("MySQLDriverCS Error: can't create client.");
			void* retval = CPrototypes.mysql_real_connect(handle,location,userid,password,database,Convert.ToUInt32(port),null,0);
			if(retval==null)
				throw new MySQLException("MySQLDriverCS Error: can't connect.");
		}
		#endregion
		#region Close
		/// <summary>
		/// Closes the connection to the database.
		/// </summary>
		unsafe public void Close()
		{
			if(handle==null) return;
			CPrototypes.mysql_close(handle);
			handle=null;
		}
		#endregion

	}
	/// <summary>
	/// Simple exception. Can show multiple messages in one message sepparated by CR/LF
	/// </summary>
	public class MySQLException : Exception
	{
		internal string message;
		/// <summary>
		/// Constructor
		/// </summary>
		/// <param name="_message"></param>
		public MySQLException(string _message)
		{
			this.message=_message;
		}
		/// <summary>
		/// Overload to show real message string
		/// </summary>
		public override string Message
		{
			get
			{
				return "MySQLDriverCS Exception: "+message;
			}
		}
	}
}
