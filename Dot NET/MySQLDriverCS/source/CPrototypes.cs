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
	/// Field descriptor
	/// </summary>
	[StructLayout(LayoutKind.Sequential)]
	internal class MYSQL_FIELD 
	{
		/// <summary>
		/// Name of column 
		/// </summary>
		public string name;	
		/// <summary>
		/// Table of column if column was a field
		/// </summary>
		public string table;			/*  */
		/// <summary>
		///  Default value (set by mysql_list_fields) 
		/// </summary>
		public string def;			/**/
		/// <summary>
		/// Type of field. Se mysql_com.h for types
		/// </summary>
		public uint type;	/*  */
		/// <summary>
		/// Width of column
		/// </summary>
		public uint length;		/*  */
		/// <summary>
		/// Max width of selected set
		/// </summary>
		public uint max_length;	/*  */
		/// <summary>
		/// Div flags
		/// </summary>
		public uint flags;		/*  */
		/// <summary>
		/// Number of decimals in field
		/// </summary>
		public uint decimals;	/*  */
	}  
	/// <summary>
	/// C prototypes warpper for mysqllib.
	/// </summary>
	internal class CPrototypes
	{
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_init" )]
		unsafe public static extern void* mysql_init(void* must_be_null);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_close" )]
		unsafe public static extern void mysql_close(void* handle);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_real_connect" )]
		unsafe public static extern void* mysql_real_connect(void* mysql, string host, string user, string passwd, string db, uint port, string unix_socket, int client_flag);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_query" )]
		unsafe public static extern int mysql_query(void*mysql, string query);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_store_result" )]
		unsafe public static extern void *mysql_store_result(void *mysql);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_free_result" )]
		unsafe public static extern void mysql_free_result(void*result);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_errno" )]
		unsafe public static extern uint mysql_errno(void*mysql);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_error" )]
		unsafe public static extern string mysql_error(void*mysql);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_field_count" )]
		unsafe public static extern uint mysql_field_count(void*mysql);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_affected_rows" )]
		unsafe public static extern ulong mysql_affected_rows(void*mysql);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_num_fields" )]
		unsafe public static extern uint mysql_num_fields(void*result);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_num_rows" )]
		unsafe public static extern ulong mysql_num_rows(void *result);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_fetch_field_direct" )]
		unsafe public static extern IntPtr mysql_fetch_field_direct(void*result, uint fieldnr);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_fetch_row" )]
		unsafe public static extern IntPtr mysql_fetch_row(void*result);
		[ DllImport( "libmySQL.dll", EntryPoint="mysql_select_db" )]
		unsafe public static extern int mysql_select_db(void*mysql,string dbname);
	}
}
