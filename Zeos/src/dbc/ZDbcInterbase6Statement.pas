{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{    Copyright (c) 1999-2003 Zeos Development Group       }
{            Written by Sergey Merkuriev                  }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZDbcInterbase6Statement;

interface

uses Classes, SysUtils, ZDbcIntfs, ZDbcStatement, ZDbcInterbase6, ZPlainInterbase6,
  ZDbcInterbase6Utils, ZDbcInterbase6ResultSet, ZPlainInterbaseDriver;

type

  {** Implements Generic Interbase6 Statement. }
  TZInterbase6Statement = class(TZAbstractStatement)
  private
    FStatusVector: TARRAY_ISC_STATUS;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure CheckInterbase6Error(Sql: string = '');
  public
    constructor Create(Connection: IZConnection);

    function ExecuteQuery(SQL: string): IZResultSet; override;
    function ExecuteUpdate(SQL: string): Integer; override;
    function Execute(SQL: string): Boolean; override;
  end;

    {** Implements Prepared SQL Statement. }
  TZInterbase6PreparedStatement = class(TZAbstractPreparedStatement)
  private
    FStatusVector: TARRAY_ISC_STATUS;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure CheckInterbase6Error(Sql: string = '');    
  public
    constructor Create(Connection: IZConnection; SQL: string);

    function ExecuteQuery(SQL: string): IZResultSet; override;
    function ExecuteUpdate(SQL: string): Integer; override;
    function Execute(SQL: string): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

implementation

uses ZDbcCachedResultSet;

{ TZInterbase6Statement }

{**
   Chack interbase error status
   @param Sql the used sql tring
}
procedure TZInterbase6Statement.CheckInterbase6Error(Sql: string = '');
begin
  ZDbcInterbase6Utils.CheckInterbase6Error(FIBConnection.GetPlainDriver,
    FStatusVector, SQL);
end;


{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
}
constructor TZInterbase6Statement.Create(Connection: IZConnection);
begin
  inherited Create(Connection);
  FIBConnection := Connection as IZInterbase6Connection;
  ResultSetType := rtScrollInsensitive;
end;

{**
  Destroys this object and cleanups the memory.
}
{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZInterbase6Statement.ExecuteQuery(SQL: string): IZResultSet;
var
  Cursor: string;
  SQLData: IZResultSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  StmtHandle := nil;

  with FIBConnection do
  begin
    SQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);

    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, SQL, StmtHandle, SQLData);

      if StatementType = stExecProc then
        raise EZSQLException.Create('The execution stored procedures is not allowed');

      GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle,
        @StmtHandle, GetDialect, SQLData.GetData);
      CheckInterbase6Error(SQL);

      Cursor := RandomString(12);
      GetPlainDriver.isc_dsql_set_cursor_name(@FStatusVector,
        @StmtHandle, PChar(Cursor), 0);
      CheckInterbase6Error(SQL);

      Result := TZInterbase6ResultSet.Create(Self, SQL, StmtHandle, Cursor,
        SQLData, nil);
      Result := GetCachedResultSet(SQL, Self, Result);
    except
      FreeStatement(GetPlainDriver, StmtHandle);
    end;
  end;  
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZInterbase6Statement.ExecuteUpdate(SQL: string): Integer;
var
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  Result := -1;
  StmtHandle := nil;
  with FIBConnection do
  begin
    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, SQL, StmtHandle, nil);

      if StatementType = stExecProc then
        raise EZSQLException.Create('The execution stored procedures is not allowed');

      GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle,
        @StmtHandle, GetDialect, nil, nil);
      CheckInterbase6Error(SQL);

      Result := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);
      LastUpdateCount := Result;

      case StatementType of
        stCommit, stRollback, stUnknown: Result := -1;
      end;

      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
    finally
      FreeStatement(GetPlainDriver, StmtHandle);
    end;
  end;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZInterbase6Statement.Execute(SQL: string): Boolean;
var
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  Result := False;
  StmtHandle := nil;
  with FIBConnection do
  begin
    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver, GetDBHandle, GetTrHandle,
        GetDialect, SQL, StmtHandle, nil);

      if StatementType = stExecProc then
        raise EZSQLException.Create('The execution stored procedures is not allowed');

      GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle,
              @StmtHandle, GetDialect, nil);
      CheckInterbase6Error(Sql);

      LastUpdateCount := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);

      case StatementType of
        stInsert, stDelete, stUpdate, stSelectForUpdate: Result := False;
      else
        Result := True;
      end;

     { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
    finally
      FreeStatement(GetPlainDriver, StmtHandle);
    end;
  end;  
end;

{ TZInterbase6PreparedStatement }

{**
   Chack interbase error status
   @param Sql the used sql tring
}
procedure TZInterbase6PreparedStatement.CheckInterbase6Error(Sql: string);
begin
  ZDbcInterbase6Utils.CheckInterbase6Error(FIBConnection.GetPlainDriver,
    FStatusVector, SQL);
end;


{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
}
constructor TZInterbase6PreparedStatement.Create(Connection: IZConnection;
  SQL: string);
begin
  inherited Create(Connection, SQL);
  FIBConnection := Connection as IZInterbase6Connection;
  ResultSetType := rtScrollInsensitive;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}

function TZInterbase6PreparedStatement.Execute(SQL: string): Boolean;
begin
  Self.SQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZInterbase6PreparedStatement.ExecutePrepared: Boolean;
var
  ParamSQLData: IZParamsSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  Result := False;
  StmtHandle := nil;
  with FIBConnection do
  begin
    ParamSQLData := TZParamsSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);

    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver, GetDBHandle, GetTrHandle,
        GetDialect, SQL, StmtHandle, nil);

      if StatementType = stExecProc then
        raise EZSQLException.Create('The execution stored procedures is not allowed');

      PrepareParameters(GetPlainDriver, SQL, InParamValues, InParamTypes,
        InParamCount, GetDialect, StmtHandle, ParamSQLData);

      GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @StmtHandle,
            GetDialect, ParamSQLData.GetData, nil);
      CheckInterbase6Error(SQL);

      LastUpdateCount := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);

      case StatementType of
        stInsert, stDelete, stUpdate, stSelectForUpdate: Result := False;
      else
        Result := True;
      end;

      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
    finally
      FreeStatement(GetPlainDriver, StmtHandle);
    end;
  end;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZInterbase6PreparedStatement.ExecuteQuery(SQL: string): IZResultSet;
begin
  Self.SQL := SQL;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZInterbase6PreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  Cursor: string;
  SQLData: IZResultSQLDA;
  ParamSQLData: IZParamsSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  StmtHandle := nil;
  with FIBConnection do
  begin
    SQLData := TZResultSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);
    ParamSQLData := TZParamsSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);

    StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
      GetDBHandle, GetTrHandle, GetDialect, SQL, StmtHandle, SQLData);
    try
      if StatementType = stExecProc then
        raise EZSQLException.Create('The execution stored procedures is not allowed');

      PrepareParameters(GetPlainDriver, SQL, InParamValues, InParamTypes,
        InParamCount, GetDialect, StmtHandle,
        ParamSQLData);

      GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @StmtHandle,
        GetDialect, ParamSQLData.GetData, nil);
      CheckInterbase6Error(SQL);

      Cursor := RandomString(12);
      GetPlainDriver.isc_dsql_set_cursor_name(@FStatusVector,
        @StmtHandle, PChar(Cursor), 0);
      CheckInterbase6Error(SQL);

      Result := TZInterbase6ResultSet.Create(Self, SQL, StmtHandle, Cursor,
        SQLData, nil);
      Result := GetCachedResultSet(SQL, Self, Result);
    except
      FreeStatement(GetPlainDriver, StmtHandle);
    end;
  end;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZInterbase6PreparedStatement.ExecuteUpdate(SQL: string): Integer;
begin
  Self.SQL := SQL;
  Result := ExecuteUpdatePrepared;
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZInterbase6PreparedStatement.ExecuteUpdatePrepared: Integer;
var
  ParamSQLData: IZParamsSQLDA;
  StmtHandle: TISC_STMT_HANDLE;
  StatementType: TZIbSqlStatementType;
begin
  Result := -1;
  StmtHandle := nil;

  with FIBConnection do
  begin
    ParamSQLData := TZParamsSQLDA.Create(GetPlainDriver, GetDBHandle, GetTrHandle);

    try
      StatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver, GetDBHandle,
        GetTrHandle, GetDialect, SQL, StmtHandle, nil);

      if StatementType = stExecProc then
        raise EZSQLException.Create('The execution stored procedures is not allowed');

      PrepareParameters(GetPlainDriver, SQL, InParamValues, InParamTypes,
        InParamCount, GetDialect, StmtHandle, ParamSQLData);

      GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle,
        @StmtHandle, GetDialect, ParamSQLData.GetData);
      CheckInterbase6Error(SQL);

      Result := GetAffectedRows(GetPlainDriver, StmtHandle, StatementType);
      LastUpdateCount := Result;

      case StatementType of
        stCommit, stRollback, stUnknown: Result := -1;
      end;

      { Autocommit statement. }
      if Connection.GetAutoCommit then
        Connection.Commit;
    finally
      FreeStatement(GetPlainDriver, StmtHandle);
    end;
  end;
end;


end.


