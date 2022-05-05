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
	/// This class is IDbCommand compliant so take a look into MSDN help to unserstand how it works
	/// </summary>
	public class MySQLCommand : IDbCommand
	{
		private string query = "";
		private MySQLConnection connection;
		internal IDbTransaction _Transaction = null;
		internal bool bTryToCancel = false;
		/// <summary>Initializes a new instance of the MySQLCommand class.</summary>
		public MySQLCommand() 
		{
		}
		/// <summary>Initializes a new instance of the MySQLCommand class with the text of the query.</summary>
		/// <param name="cmdText"></param>
		public MySQLCommand(string cmdText) 
		{
			query=cmdText;
		}
		/// <summary>Initializes a new instance of the MySQLCommand class with the text of the query and a MySQLConnection.</summary>
		/// <param name="cmdText"></param>
		/// <param name="_connection"></param>
		public MySQLCommand(string cmdText, MySQLConnection _connection)
		{
			query=cmdText;
			connection=_connection;
		}
		/// <summary>Initializes a new instance of the MySQLCommand class with the text of the query, a MySQLConnection, and the IDbTransaction. 
		/// </summary>
		/// <param name="cmdText"></param>
		/// <param name="connection"></param>
		/// <param name="transaction"></param>
		public MySQLCommand(string cmdText, MySQLConnection connection, IDbTransaction transaction):this(cmdText,connection) 
		{
			_Transaction=transaction;
		}
		/// <summary>
		/// Gets or sets the text command to run against the data source.
		/// </summary>
		public string CommandText {get{return query;} set{query=value;}}
		/// <summary>
		/// Gets or sets the wait time before terminating the attempt to execute a command and generating an error.
		/// Set Is unsupported.
		/// </summary>
		public int CommandTimeout {get{return 60;} set{}}
		/// <summary>
		/// Indicates or specifies how the CommandText property is interpreted.
		/// Allways set to CommandType.Text . Set Is unsupported.
		/// </summary>
		public CommandType CommandType {get{ return CommandType.Text;} set{}}
		/// <summary>
		/// Gets or sets the IDbConnection used by this instance of the IDbCommand.
		/// </summary>
		public IDbConnection Connection {get{return connection;} set{connection=value as MySQLConnection;}}
		/// <summary>
		/// Operation not supported.
		/// </summary>
		public IDataParameterCollection Parameters {get{ return null;} }
		/// <summary>
		/// Sets and gets transaction
		/// </summary>
		public IDbTransaction Transaction{get{return _Transaction;} set{_Transaction=value;}}
		/// <summary>
		/// Operation not supported.
		/// </summary>
		public UpdateRowSource UpdatedRowSource {get{throw new MySQLException("Operation not supported");} set{}}
		/// <summary>
		/// Multithreding operation: cancels current reading.
		/// </summary>
		public void Cancel()
		{
			lock(this)
			{
				bTryToCancel=true;
			}
			//throw new MySQLException("Operation not supported");
		}
		/// <summary>
		/// Operation not supported.
		/// </summary>
		public IDbDataParameter CreateParameter(){throw new MySQLException("Operation not supported");}
		/// <summary>
		/// Executes a SQL statement against the Connection object, and returns updated rows count.
		/// </summary>
		/// <returns></returns>
		unsafe public int ExecuteNonQuery()
		{
			if(connection==null){throw new MySQLException("Connection must be open");}
			void*result;
			if(CPrototypes.mysql_query(connection.handle,query)!=0)
			{
				// error
				throw new MySQLException("MySQLDriverCS Error: wrong query."+CPrototypes.mysql_error(connection.handle));
			}
			else // query succeeded, process any data returned by it
			{
				result = CPrototypes.mysql_store_result(connection.handle);
				if(result!=null)  // there are rows
				{
					//num_fields = mysql_num_fields(result);
					// retrieve rows, then call mysql_free_result(result)
					CPrototypes.mysql_free_result(result);
					return 0;
				}
				else  // mysql_store_result() returned nothing; should it have?
				{
					if(CPrototypes.mysql_errno(connection.handle)!=0)
					{
						throw new MySQLException("MySQLDriverCS Error: "+CPrototypes.mysql_error(connection.handle));
					}
					else if (CPrototypes.mysql_field_count(connection.handle) == 0)
					{
						// query does not return data
						// (it was not a SELECT)
						return (int)CPrototypes.mysql_affected_rows(connection.handle);
					}
					else
						return 0;
				}
			}
		}
		/// <summary>
		/// Executes the CommandText against the Connection and builds an MySQLDataReader.
		/// </summary>
		/// <returns>MySQLDataReader</returns>
		public MySQLDataReader ExecuteReaderEx()
		{
			return (MySQLDataReader)ExecuteReader();
		}
		/// <summary>
		/// Overloaded. Executes the CommandText against the Connection and builds an IDataReader.
		/// </summary>
		/// <remarks>Use <c>ExecuteReaderEx</c> to avoid conversions.</remarks>
		/// <returns>IDataReader</returns>
		unsafe public IDataReader ExecuteReader()
		{
			bTryToCancel=false;
			if(connection==null){throw new MySQLException("Connection must be open");}
			void*result;
			if(CPrototypes.mysql_query(connection.handle,query)!=0)
			{
				// error
				throw new MySQLException("MySQLDriverCS Error: wrong query."+CPrototypes.mysql_error(connection.handle));
			}
			else // query succeeded, process any data returned by it
			{
				result = CPrototypes.mysql_store_result(connection.handle);
				if(result!=null)  // there are rows
				{
					//num_fields = mysql_num_fields(result);
					// retrieve rows, then call mysql_free_result(result)
					MySQLDataReader dr = new MySQLDataReader(result,this.connection,this);
					CPrototypes.mysql_free_result(result);
					return dr;
				}
				else  // mysql_store_result() returned nothing; should it have?
				{
					if(CPrototypes.mysql_errno(connection.handle)!=0)
					{
						throw new MySQLException("MySQLDriverCS Error: "+CPrototypes.mysql_error(connection.handle));
					}
					else if (CPrototypes.mysql_field_count(connection.handle) == 0)
					{
						// query does not return data
						// (it was not a SELECT)
						return null;
					}
					else
						return null;
				}
			}
		}
		/// <summary>
		/// Overloaded. Executes the CommandText against the Connection and builds an IDataReader.
		/// </summary>
		/// <param name="behavior">Don't care</param>
		/// <returns></returns>
		public IDataReader ExecuteReader(CommandBehavior behavior){return ExecuteReader();}
		/// <summary>
		/// Operation not supported.
		/// </summary>
		public object ExecuteScalar(){throw new MySQLException("Operation not supported");}
		/// <summary>
		/// Operation ignored.
		/// </summary>
		public void Prepare(){}
		private bool bDisposed=false;
		/// <summary>
		/// Dispose command
		/// </summary>
		public void Dispose()
		{
			if(bDisposed) return;
			if(_Transaction!=null)
				_Transaction.Dispose();
			connection=null;
			bDisposed=true;
		}
	}
}
