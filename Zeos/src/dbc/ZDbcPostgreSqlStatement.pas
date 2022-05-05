{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
{                                                         }
{    Copyright (c) 1999-2003 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
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

unit ZDbcPostgreSqlStatement;

interface

{$I ZDbc.inc}

uses Classes, SysUtils, ZSysUtils, ZParseIntfs, ZDbcIntfs, ZDbcStatement,
  ZPlainPostgreSqlDriver, ZCompatibility;

type

  {** Implements Generic PostgreSQL Statement. }
  TZPostgreSQLStatement = class(TZAbstractStatement)
  private
    FHandle: PZPostgreSQLConnect;
    FPlainDriver: IZPostgreSQLPlainDriver;
  protected
    function CreateResultSet(SQL: string;
      QueryHandle: PZPostgreSQLResult): IZResultSet;
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver;
      Connection: IZConnection; Handle: PZPostgreSQLConnect);
    destructor Destroy; override;

    function ExecuteQuery(SQL: string): IZResultSet; override;
    function ExecuteUpdate(SQL: string): Integer; override;
    function Execute(SQL: string): Boolean; override;
  end;

  {** Implements Prepared SQL Statement. }
  TZPostgreSQLPreparedStatement = class(TZEmulatedPreparedStatement)
  private
    FHandle: PZPostgreSQLConnect;
    FPlainDriver: IZPostgreSQLPlainDriver;
  protected
    function CreateExecStatement: IZStatement; override;
    function PrepareSQLParam(ParamIndex: Integer): string; override;
  public
    constructor Create(PlainDriver: IZPostgreSQLPlainDriver;
      Connection: IZConnection; SQL: string; Handle: PZPostgreSQLConnect);
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZDbcPostgreSql, ZDbcPostgreSqlResultSet, ZPostgreSqlToken,
  ZDbcPostgreSqlUtils, ZDbcCachedResultSet;

{ TZPostgreSQLStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a PostgreSQL plain driver.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
}
constructor TZPostgreSQLStatement.Create(PlainDriver: IZPostgreSQLPlainDriver;
  Connection: IZConnection; Handle: PZPostgreSQLConnect);
begin
  inherited Create(Connection);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZPostgreSQLStatement.Destroy;
begin
  inherited Destroy;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZPostgreSQLStatement.CreateResultSet(SQL: string;
  QueryHandle: PZPostgreSQLResult): IZResultSet;
var
  NativeResultSet: TZPostgreSQLResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZPostgreSQLResultSet.Create(FPlainDriver, Self, SQL,
    FHandle, QueryHandle);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if GetResultSetConcurrency = rcUpdatable then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, nil);
    CachedResultSet.SetConcurrency(rcUpdatable);
    Result := CachedResultSet;
  end else
    Result := NativeResultSet;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLStatement.ExecuteQuery(SQL: string): IZResultSet;
var
  QueryHandle: PZPostgreSQLResult;
begin
  Result := nil;
  QueryHandle := FPlainDriver.ExecuteQuery(FHandle, PChar(SQL));
  CheckPostgreSQLError(FPlainDriver, FHandle);
  if QueryHandle <> nil then
    Result := CreateResultSet(SQL, QueryHandle)
  else Result := nil;
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
function TZPostgreSQLStatement.ExecuteUpdate(SQL: string): Integer;
var
  QueryHandle: PZPostgreSQLResult;
begin
  Result := -1;
  QueryHandle := FPlainDriver.ExecuteQuery(FHandle, PChar(SQL));
  CheckPostgreSQLError(FPlainDriver, FHandle);

  if QueryHandle <> nil then
  begin
    Result := StrToIntDef(StrPas(FPlainDriver.GetCommandTuples(QueryHandle)), 0);
    FPlainDriver.Clear(QueryHandle);
  end;

  { Autocommit statement. }
  if Connection.GetAutoCommit then
    Connection.Commit;
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
function TZPostgreSQLStatement.Execute(SQL: string): Boolean;
var
  QueryHandle: PZPostgreSQLResult;
  ResultStatus: TZPostgreSQLExecStatusType;
begin
  QueryHandle := FPlainDriver.ExecuteQuery(FHandle, PChar(SQL));
  CheckPostgreSQLError(FPlainDriver, FHandle);

  { Process queries with result sets }
  ResultStatus := FPlainDriver.GetResultStatus(QueryHandle);
  case ResultStatus of
    PGRES_TUPLES_OK:
      begin
        Result := True;
        LastResultSet := CreateResultSet(SQL, QueryHandle);
      end;
    PGRES_COMMAND_OK:
      begin
        Result := False;
        LastUpdateCount := StrToIntDef(StrPas(
          FPlainDriver.GetCommandTuples(QueryHandle)), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
    else
      begin
        Result := False;
        LastUpdateCount := StrToIntDef(StrPas(
          FPlainDriver.GetCommandTuples(QueryHandle)), 0);
        FPlainDriver.Clear(QueryHandle);
      end;
  end;

  { Autocommit statement. }
  if not Result and Connection.GetAutoCommit then
    Connection.Commit;
end;

{ TZPostgreSQLPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a PostgreSQL plain driver.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
}
constructor TZPostgreSQLPreparedStatement.Create(
  PlainDriver: IZPostgreSQLPlainDriver; Connection: IZConnection;
  SQL: string; Handle: PZPostgreSQLConnect);
begin
  inherited Create(Connection, SQL);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
end;

{**
  Creates a temporary statement which executes queries.
  @return a created statement object.
}
function TZPostgreSQLPreparedStatement.CreateExecStatement: IZStatement;
begin
  Result := TZPostgreSQLStatement.Create(FPlainDriver, Connection, FHandle);
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZPostgreSQLPreparedStatement.PrepareSQLParam(
  ParamIndex: Integer): string;
var
  I: Integer;
  Value: Variant;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
  TempStream: TStream;
  WriteTempBlob: IZPostgreSQLBlob;
begin
  TempBytes := nil;
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create('Input parameter count is less then expected.');

  Value := InParamValues[ParamIndex];
  if VarIsEmpty(Value) or VarIsNull(Value)  then
    Result := 'NULL'
  else begin
    case InParamTypes[ParamIndex] of
      stBoolean:
        if Value then Result := 'TRUE'
        else Result := 'FALSE';
      stByte, stShort, stInteger, stLong, stBigDecimal:
        Result := VarToStr(Value);
      stFloat, stDouble:
        Result := FloatToSQLStr(Value);
      stString:
        Result := EncodeString(VarToStr(Value));
      stBytes:
        begin
          TempBytes := TByteDynArray(Value);
          for I := 0 to High(TempBytes) do
            Result := Result + Char(TempBytes[I]);
          Result := EncodeString(Result);
        end;
      stDate:
        Result := Format('''%s''::date',
          [FormatDateTime('yyyy-mm-dd', VarToDateTime(Value))]);
      stTime:
        Result := Format('''%s''::time',
          [FormatDateTime('hh:mm:ss', VarToDateTime(Value))]);
      stTimestamp:
        Result := Format('''%s''::timestamp',
          [FormatDateTime('yyyy-mm-dd hh:mm:ss', VarToDateTime(Value))]);
      stAsciiStream, stUnicodeStream:
        begin
          if VarType(Value) = varUnknown then
          begin
            TempBlob := IZBlob(TVarData(Value).VUnknown);
            if not TempBlob.IsEmpty then
            begin
              Result := EncodeString(TempBlob.GetString)
            end else Result := 'NULL';
          end else
            Result := 'NULL';
        end;
      stBinaryStream:
        begin
          if VarType(Value) = varUnknown then
          begin
            TempBlob := IZBlob(TVarData(Value).VUnknown);
            if not TempBlob.IsEmpty then
            begin
              if (GetConnection as IZPostgreSQLConnection).IsOidAsBlob then
              begin
                TempStream := TempBlob.GetStream;
                try
                  WriteTempBlob := TZPostgreSQLBlob.Create(FPlainDriver,
                    nil, 0, FHandle, 0);
                  WriteTempBlob.SetStream(TempStream);
                  WriteTempBlob.WriteBlob;
                  Result := IntToStr(WriteTempBlob.GetBlobOid);
                finally
                  WriteTempBlob := nil;
                  TempStream.Free;
                end;
              end
              else
              begin
                Result := EncodeString(TempBlob.GetString);
                Result := Copy(Result, 2, Length(Result) - 2);
                Result := EncodeString(Result);
              end;
            end else
              Result := 'NULL';
          end else
            Result := 'NULL';
        end;
    end;
  end;
end;

end.
