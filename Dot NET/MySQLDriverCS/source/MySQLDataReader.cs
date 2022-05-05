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
using System.Globalization;
namespace MySQLDriverCS
{
	/// <summary>
	/// This class is IDataReader compliant so take a look into MSDN help to understand how it works
	/// </summary>
	public class MySQLDataReader : IDataReader
	{
		internal DataTable dt;
		internal MySQLConnection connection;
		#region Constructor & destructor
		unsafe internal MySQLDataReader(void* result,MySQLConnection _connection, MySQLCommand _cmd)
		{
			connection=_connection;
			dt = new DataTable();
			uint i; ulong j;
			uint num_fields = CPrototypes.mysql_num_fields(result);
			ulong num_rows = CPrototypes.mysql_num_rows(result);
			for(i=0; i<num_fields;i++)
			{
				MYSQL_FIELD field = new MYSQL_FIELD();
				IntPtr ptr;
				ptr = CPrototypes.mysql_fetch_field_direct(result, i);
				Marshal.PtrToStructure(ptr,field);
				dt.Columns.Add( field.name );
			}
			/*
				MYSQL_ROW row;
				unsigned int num_fields;
				unsigned int i;
				num_fields = mysql_num_fields(result);
				while ((row = mysql_fetch_row(result)))
				{
				unsigned long *lengths;
				lengths = mysql_fetch_lengths(result);
				for(i = 0; i < num_fields; i++)
				{
				printf("[%.*s] ", (int) lengths[i], row[i] ? row[i] : "NULL");
				}
				printf("\n");
				}			 
			 */
			for(j=0; j<num_rows;j++)
			{
				lock(_cmd)
				{
					if(_cmd.bTryToCancel) break;
				}
				//string[] row = null;
				IntPtr myrow = CPrototypes.mysql_fetch_row(result);
				if(myrow.ToPointer()==null)
				{
					throw new MySQLException("MySQLDriverCS Error: "+CPrototypes.mysql_error(this.connection.handle));
				}
				DataRow dr = dt.NewRow();
				for(i=0; i<num_fields;i++)
				{
					IntPtr ptr = Marshal.ReadIntPtr(myrow,(int)i*sizeof(IntPtr));
					string val = Marshal.PtrToStringAnsi(ptr);
					if(val==null)
						dr[(int)i]=null;
					else
						dr[(int)i]=val;
				}
				dt.Rows.Add( dr );
			}
		}
		/// <summary>
		/// Closes this reader
		/// </summary>
		public void Dispose()
		{
			Close();
		}
		#endregion
		#region IDataReader
		/// <summary>
		/// returns 1
		/// </summary>
		public int Depth {get{return 1;}}
		/// <summary>
		/// Gets a value indicating whether the data reader is closed.
		/// </summary>
		public bool IsClosed {get{return dt==null;}}
		private int _RecordsAffected=0;
		/// <summary>
		/// Gets the number of rows changed, inserted, or deleted by execution of the SQL statement.
		/// </summary>
		/// <remarks>The RecordsAffected property is not set until all rows are read and you close the MySQLDataReader.</remarks>
		public int RecordsAffected {get{return _RecordsAffected;}}
		/// <summary>
		/// Closes the MySQLDataReader 0bject.
		/// </summary>
		public void Close()
		{
			if(dt==null) return;
			_RecordsAffected=dt.Rows.Count;
			dt.Dispose();
			dt=null;
		}
		/// <summary>
		/// Returns a DataTable that describes the column metadata of the MySQLDataReader and it's values.
		/// </summary>
		/// <returns></returns>
		public DataTable GetSchemaTable()
		{
			return dt;
		}
		int rowpos = -1;
		/// <summary>
		/// Unsupported
		/// </summary>
		/// <returns></returns>
		public bool NextResult()
		{
			throw new MySQLException("Operation not supported");
		}
		/// <summary>
		/// Advances the MySQLDataReader to the next record.
		/// </summary>
		/// <returns></returns>
		public bool Read()
		{
			if(IsClosed) return false;
			rowpos++;
			if(rowpos<dt.Rows.Count) 
				return true; 
			else 
				return false;
		}
		#endregion
		#region IDataRecord
		/// <summary>
		/// Number of fields returned
		/// </summary>
		public int FieldCount{ get{ if(IsClosed) return 0; else return dt.Columns.Count; }}
		/// <summary>
		/// Get value by name
		/// </summary>
		public object this[string name]{ get{
				if(IsClosed) throw new MySQLException("Reader must be open");
				return dt.Rows[rowpos][name];
		}}
		/// <summary>
		/// Get value by index
		/// </summary>
		public object this[int i]{ get{
				if(IsClosed) throw new MySQLException("Reader must be open");
				return dt.Rows[rowpos][i];
		}}
		/// <summary>
		/// Get as boolean
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public bool GetBoolean(int i) { return Convert.ToBoolean( this[i] ); }
		/// <summary>
		/// Get as byte
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public byte GetByte(int i) { return Convert.ToByte( this[i] ); }
		/// <summary>
		/// Unsupported
		/// </summary>
		/// <param name="i"></param>
		/// <param name="fieldOffset"></param>
		/// <param name="buffer"></param>
		/// <param name="bufferoffset"></param>
		/// <param name="length"></param>
		/// <returns></returns>
		public long GetBytes( int i, long fieldOffset, byte[] buffer, int bufferoffset, int length ){ throw new MySQLException("Operation not supported"); }
		/// <summary>
		/// Get as char
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public char GetChar(int i) { return Convert.ToChar( this[i] ); }
		/// <summary>
		/// Unsupported
		/// </summary>
		/// <param name="i"></param>
		/// <param name="fieldoffset"></param>
		/// <param name="buffer"></param>
		/// <param name="bufferoffset"></param>
		/// <param name="length"></param>
		/// <returns></returns>
		public long GetChars( int i, long fieldoffset, char[] buffer, int bufferoffset, int length ){ throw new MySQLException("Operation not supported"); }
		/// <summary>
		/// Unsupported
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public IDataReader GetData(int i){ throw new MySQLException("Operation not supported"); }
		/// <summary>
		/// Unsupported
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public string GetDataTypeName( int i ){ throw new MySQLException("Operation not supported"); }
		/// <summary>
		/// Get as DateTime
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public DateTime GetDateTime( int i ){ return Convert.ToDateTime( this[i] ); }
		/// <summary>
		/// Get as decimal
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public decimal GetDecimal( int i ){ return Convert.ToDecimal( this[i] ); }
		/// <summary>
		/// Get as double
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public double GetDouble( int i ){ return Convert.ToDouble( this[i].ToString(), new CultureInfo("en-US").NumberFormat ); }
		/// <summary>
		/// Get field type (class Type)
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public Type GetFieldType( int i ){ return typeof(string); }
		/// <summary>
		/// Get as float
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public float GetFloat( int i ){ return (float)Convert.ToDouble( this[i].ToString(), new CultureInfo("en-US").NumberFormat ); }
		/// <summary>
		/// Unsupported
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public Guid GetGuid( int i ){ throw new MySQLException("Operation not supported"); }
		/// <summary>
		/// Get as Int16
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public short GetInt16( int i ){ return Convert.ToInt16( this[i] ); }
		/// <summary>
		/// Get as Int32
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public int GetInt32( int i ){ return Convert.ToInt32( this[i] ); }
		/// <summary>
		/// Get as Int64
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public long GetInt64( int i ){ return Convert.ToInt64( this[i] ); }
		/// <summary>
		/// Get name of field by index
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public string GetName( int i ){ return dt.Columns[i].ColumnName; }
		/// <summary>
		/// Get index of field by name
		/// </summary>
		/// <param name="name"></param>
		/// <returns></returns>
		public int GetOrdinal( string name ){ return dt.Columns[name].Ordinal;}
		/// <summary>
		/// Get as string
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public string GetString( int i ){ return Convert.ToString( this[i] ); }
		/// <summary>
		/// Get as object
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public object GetValue( int i ){ return this[i]; }
		/// <summary>
		/// Get full row as array of object
		/// </summary>
		/// <param name="values"></param>
		/// <returns>Fields retrived, if it fails returns -1.</returns>
		/// <remarks>
		/// <list type="bullet">
		/// <listheader><item><description>Bugfixed:</description></item></listheader>
		/// <item><term>2002-10-28</term><description>Values was ignored</description></item>
		/// </list>
		/// </remarks>
		public int GetValues( object[] values )
		{ 
			if(values.Length<FieldCount)
				return -1;
			for(int i=0;i<FieldCount;i++) 
				values[i]=GetValue(i); 
			return FieldCount;
		}
		/// <summary>
		/// Unsupported
		/// </summary>
		/// <param name="i"></param>
		/// <returns></returns>
		public bool IsDBNull( int i ){ throw new MySQLException("Operation not supported"); }
		#endregion
	}
}
