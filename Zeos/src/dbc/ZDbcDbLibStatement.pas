{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          DBLib Statement common functionality           }
{                                                         }
{    Copyright (c) 1999-2003 Zeos Development Group       }
{            Written by Janos Fegyverneki                 }
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

unit ZDbcDbLibStatement;

interface

{$I ZDbc.inc}

uses Classes, SysUtils, ZCompatibility, ZClasses, ZSysUtils, ZCollections,
  ZParseIntfs, ZDbcIntfs, ZDbcStatement, ZDbcDbLib, ZPlainDbLibDriver;

type
  {** Implements Generic DBLib Statement. }
  TZDBLibStatement = class(TZAbstractStatement)
  protected
    FSQL: string;
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: IZDBLibPlainDriver;
    FHandle: PDBPROCESS;
    FResults: IZCollection;
    procedure InternalExecuteStatement(SQL: string); virtual;
    procedure FetchResults; virtual;
  public
    constructor Create(Connection: IZConnection; Handle: PDBPROCESS);
    destructor Destroy; override;

    function GetMoreResults: Boolean; override;
    function ExecuteQuery(Sql: string): IZResultSet; override;
    function ExecuteUpdate(SQL: string): Integer; override;
    function Execute(SQL: string): Boolean; override;
  end;

  {** Implements Prepared SQL Statement. With emulation}
  TZDBLibPreparedStatementEmulated = class(TZEmulatedPreparedStatement)
  protected
    FHandle: PDBPROCESS;
  protected
    function GetEscapeString(Value: string): string;
    function PrepareSqlParam(ParamIndex: Integer): string; override;
    function CreateExecStatement: IZStatement; override;
  public
    constructor Create(Connection: IZConnection; SQL: string; Handle: PDBPROCESS);
    function GetMetaData: IZResultSetMetaData; override;
  end;

type
  {** Interface for storing counter. }
  IZUpdateCount = interface(IZInterface)
    ['{03219BB4-E07F-4A50-80CD-291FEA629697}']
    procedure SetCount(Value: Integer);
    function GetCount: Integer;
  end;

  TZUpdateCount = class(TInterfacedObject, IZUpdateCount)
  private
    FCount: Integer;
  public
    constructor Create(ACount: Integer);
    procedure SetCount(Value: Integer); virtual;
    function GetCount: Integer; virtual;
    property Count: Integer read GetCount write SetCount;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZDbcCachedResultSet, ZDbcDbLibUtils, ZDbcDbLibResultSet, ZSybaseToken;

constructor TZUpdateCount.Create(ACount: Integer);
begin
  inherited Create;
  FCount := ACount;
end;

procedure TZUpdateCount.SetCount(Value: Integer);
begin
  FCount := Value;
end;

function TZUpdateCount.GetCount: Integer;
begin
  Result := FCount;
end;

{ TZDBLibStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
}
constructor TZDBLibStatement.Create(Connection: IZConnection; Handle: PDBPROCESS);
begin
  inherited Create(Connection);
  Connection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  if Assigned(FDBLibConnection) then
    FPLainDriver := FDBLibConnection.GetPlainDriver;
  FHandle := Handle;
  ResultSetType := rtScrollInsensitive;
  FResults := TZCollection.Create;
end;

destructor TZDBLibStatement.Destroy;
begin
  inherited Destroy;
end;

{**
  Executes a Statement.
  Used internally to execute statements.

  @param Handle a DBLib connection handle.
  @sql string containing the statements to execute
}
procedure TZDBLibStatement.InternalExecuteStatement(SQL: string);
begin
  FPlainDriver := FDBLibConnection.GetPlainDriver;
  if FPlainDriver.dbcancel(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError;
  if FPlainDriver.GetProtocol = 'mssql' then
    SQL := StringReplace(Sql, '\'#13, '\\'#13, [rfReplaceAll]);
  if FPlainDriver.dbcmd(FHandle, PChar(SQL)) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError;
  if FPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError;
end;

{**
  Moves to a <code>Statement</code> object's next result.  It returns
  <code>true</code> if this result is a <code>ResultSet</code> object.
  This method also implicitly closes any current <code>ResultSet</code>
  object obtained with the method <code>getResultSet</code>.

  <P>There are no more results when the following is true:
  <PRE>
        <code>(!getMoreResults() && (getUpdateCount() == -1)</code>
  </PRE>

 @return <code>true</code> if the next result is a <code>ResultSet</code> object;
   <code>false</code> if it is an update count or there are no more results
 @see #execute
}
function TZDBLibStatement.GetMoreResults: Boolean;
var
  ResultSet: IZResultSet;
  UpdateCount: IZUpdateCount;
begin
  Result := False;
  LastResultSet := nil;
  LastUpdateCount := -1;
  if FResults.Count > 0 then
  begin
    try
      Result := FResults.Items[0].QueryInterface(IZResultSet, ResultSet) = 0;
      if Result then
      begin
        LastResultSet := ResultSet;
        LastUpdateCount := 0;
      end
      else
        if FResults.Items[0].QueryInterface(IZUpdateCount, UpdateCount) = 0 then
          LastUpdateCount := UpdateCount.GetCount;
      FResults.Delete(0);
    finally
      ResultSet := nil;
      UpdateCount := nil;
    end;
  end;
end;

{**
  Fetches all results and creates a cachedresultset object for each resultset
  and a ZUpdateCount object for each count value.
}
procedure TZDBLibStatement.FetchResults;
var
  NativeResultSet: TZDBLibResultSet;
  CachedResultSet: TZCachedResultSet;
  RowsAffected: Integer;
begin
//Sybase does not seem to return dbCount at all, so a workaround is made
  RowsAffected := -2;
  while FPlainDriver.dbresults(FHandle) = DBSUCCEED do
  begin
    if FPlainDriver.dbcmdrow(FHandle) = DBSUCCEED then
    begin
      NativeResultSet := TZDBLibResultSet.Create(Self, FSQL, FHandle);
      NativeResultSet.SetConcurrency(rcReadOnly);
      CachedResultSet := TZCachedResultSet.Create(NativeResultSet, FSQL, nil);
      CachedResultSet.SetType(rtScrollInsensitive);//!!!Cached resultsets are allways this
      CachedResultSet.Last; CachedResultSet.BeforeFirst; //!!!Just to invoke fetchall
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      FResults.Add(CachedResultSet);
    end
    else
    begin
      RowsAffected := FPlainDriver.dbCount(FHandle);
      if RowsAffected > -1 then
        FResults.Add(TZUpdateCount.Create(RowsAffected));
    end;
  end;
  FDBLibConnection.CheckDBLibError;
  if RowsAffected = -1 then
  begin
    FDBLibConnection.InternalExecuteStatement('select @@rowcount');
    try
      FPlainDriver.dbresults(FHandle);
      NativeResultSet := TZDBLibResultSet.Create(Self, 'select @@rowcount', FHandle);
      if NativeResultset.Next then
        RowsAffected := NativeResultSet.GetInt(1);
      FResults.Add(TZUpdateCount.Create(RowsAffected));
    finally
      FPlainDriver.dbCancel(FHandle);
    end;
    FDBLibConnection.CheckDBLibError;
  end;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZDBLibStatement.ExecuteQuery(Sql: string): IZResultSet;
begin
  Result := nil;
  FSQL := SQL;
  try
    InternalExecuteStatement(Sql);
    FetchResults;
    repeat
      if GetMoreResults then
        Result := LastResultSet
      else if LastUpdateCount = -1 then
        Break;
    until False;
  finally
    LastResultSet := nil;
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
function TZDBLibStatement.ExecuteUpdate(SQL: string): Integer;
begin
  FSQL := SQL;
  InternalExecuteStatement(SQL);
  FetchResults;
  GetMoreResults;
  Result := LastUpdateCount;
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
}
function TZDBLibStatement.Execute(SQL: string): Boolean;
begin
  FSQL := SQL;
  InternalExecuteStatement(SQL);
  FetchResults;
  Result := GetMoreResults;
end;

{ TZDBLibPreparedStatementEmulated }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
}
constructor TZDBLibPreparedStatementEmulated.Create(Connection: IZConnection;
  Sql: string; Handle: PDBPROCESS);
begin
  inherited Create(Connection, Sql);
  FHandle := Handle;
  ResultSetType := rtScrollInsensitive;
end;

{**
  Converts an string into escape DBLib format.
  @param Value a regular string.
  @return a string in DBLib escape format.
}
function TZDBLibPreparedStatementEmulated.GetEscapeString(Value: string): string;
begin
  Result := AnsiQuotedStr(Value, '''');
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZDBLibPreparedStatementEmulated.PrepareSqlParam(
  ParamIndex: Integer): string;
begin
  if InParamCount <= ParamIndex then
    Result := 'NULL'
  else
  begin
    Result := PrepareSqlParameter(InParamValues[ParamIndex],
      InParamTypes[ParamIndex]);
  end;
end;

{**
  Gets the number, types and properties of a <code>ResultSet</code>
  object's columns.
  @return the description of a <code>ResultSet</code> object's columns
}
function TZDBLibPreparedStatementEmulated.GetMetaData: IZResultSetMetaData;
begin
  Result := nil;
end;

{**
  Creates a temporary statement which executes queries.
  @return a created statement object.
}
function TZDBLibPreparedStatementEmulated.CreateExecStatement: IZStatement;
begin
  Result := TZDBLibStatement.Create(Connection, FHandle);
end;

end.

