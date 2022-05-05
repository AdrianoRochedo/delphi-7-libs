{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
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

unit ZDbcMySqlStatement;

interface

{$I ZDbc.inc}

uses Classes, SysUtils, ZParseIntfs, ZDbcIntfs, ZDbcStatement,
  ZPlainMySqlDriver, ZCompatibility;

type

  {** Implements Generic MySQL Statement. }
  TZMySQLStatement = class(TZAbstractStatement)
  private
    FHandle: PZMySQLConnect;
    FPlainDriver: IZMySQLPlainDriver;

    function CreateResultSet(SQL: string): IZResultSet;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver;
      Connection: IZConnection; Handle: PZMySQLConnect);

    function ExecuteQuery(SQL: string): IZResultSet; override;
    function ExecuteUpdate(SQL: string): Integer; override;
    function Execute(SQL: string): Boolean; override;
  end;

  {** Implements Prepared SQL Statement. }
  TZMySQLPreparedStatement = class(TZEmulatedPreparedStatement)
  private
    FHandle: PZMySQLConnect;
    FPlainDriver: IZMySQLPlainDriver;
  protected
    function CreateExecStatement: IZStatement; override;
    function GetEscapeString(Value: string): string;
    function PrepareSQLParam(ParamIndex: Integer): string; override;
  public
    constructor Create(PlainDriver: IZMySQLPlainDriver;
      Connection: IZConnection; SQL: string; Handle: PZMySQLConnect);
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZDbcMySql, ZDbcMySqlUtils, ZDbcMySqlResultSet, ZMySqlToken, ZSysUtils,
  ZDbcCachedResultSet;

{ TZMySQLStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native MySQL plain driver.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
}
constructor TZMySQLStatement.Create(PlainDriver: IZMySQLPlainDriver;
  Connection: IZConnection; Handle: PZMySQLConnect);
begin
  inherited Create(Connection);
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZMySQLStatement.CreateResultSet(SQL: string): IZResultSet;
var
  Connection: IZMySQLConnection;
  NativeResultSet: TZMySQLResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  Connection := GetConnection as IZMySQLConnection;
  NativeResultSet := TZMySQLResultSet.Create(FPlainDriver, Self, SQL, FHandle,
    Connection.IsUseResult);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if (GetResultSetConcurrency <> rcReadOnly) or (Connection.IsUseResult
    and (GetResultSetType <> rtForwardOnly)) then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, nil);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);
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
function TZMySQLStatement.ExecuteQuery(SQL: string): IZResultSet;
begin
  Result := nil;
  if FPlainDriver.ExecQuery(FHandle, PChar(SQL)) = 0 then
    Result := CreateResultSet(SQL)
  else CheckMySQLError(FPlainDriver, FHandle);
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
function TZMySQLStatement.ExecuteUpdate(SQL: string): Integer;
var
  QueryHandle: PZMySQLResult;
begin
  Result := -1;
  if FPlainDriver.ExecQuery(FHandle, PChar(SQL)) = 0 then
  begin
    { Process queries with result sets }
    if FPlainDriver.GetStatus(FHandle) <> MYSQL_STATUS_READY then
    begin
      QueryHandle := FPlainDriver.StoreResult(FHandle);
      if QueryHandle <> nil then
      begin
        Result := FPlainDriver.GetRowCount(QueryHandle);
        FPlainDriver.FreeResult(QueryHandle);
      end else
        Result := FPlainDriver.GetAffectedRows(FHandle);
    end
    { Process regular query }
    else Result := FPlainDriver.GetAffectedRows(FHandle);
  end else
    CheckMySQLError(FPlainDriver, FHandle);
  LastUpdateCount := Result;
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
function TZMySQLStatement.Execute(SQL: string): Boolean;
begin
  Result := False;
  if FPlainDriver.ExecQuery(FHandle, PChar(SQL)) = 0 then
  begin
    { Process queries with result sets }
    if FPlainDriver.GetStatus(FHandle) <> MYSQL_STATUS_READY then
    begin
      Result := True;
      LastResultSet := CreateResultSet(SQL);
    end
    { Processes regular query. }
    else
    begin
      Result := False;
      LastUpdateCount := FPlainDriver.GetAffectedRows(FHandle);
    end;
  end else
    CheckMySQLError(FPlainDriver, FHandle);
end;

{ TZMySQLPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native MySQL Plain driver.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
}
constructor TZMySQLPreparedStatement.Create(PlainDriver: IZMySQLPlainDriver;
  Connection: IZConnection; SQL: string; Handle: PZMySQLConnect);
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
function TZMySQLPreparedStatement.CreateExecStatement: IZStatement;
begin
  Result := TZMySQLStatement.Create(FPlainDriver, Connection, FHandle);
end;

{**
  Converts an string into escape MySQL format.
  @param Value a regular string.
  @return a string in MySQL escape format.
}
function TZMySQLPreparedStatement.GetEscapeString(Value: string): string;
var
  BufferLen: Integer;
  Buffer: PChar;
begin
  BufferLen := Length(Value) * 2 + 1;
  GetMem(Buffer, BufferLen);
  BufferLen := FPlainDriver.GetEscapeString(Buffer, PChar(Value), Length(Value));
  Result := '''' + BufferToStr(Buffer, BufferLen) + '''';
  FreeMem(Buffer);
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZMySQLPreparedStatement.PrepareSQLParam(ParamIndex: Integer): string;
var
  I: Integer;
  Value: Variant;
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
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
        if Value then Result := '''Y'''
        else Result := '''N''';
      stByte, stShort, stInteger, stLong, stBigDecimal:
        Result := VarToStr(Value);
      stFloat, stDouble:
        Result := FloatToSQLStr(Value);
      stString:
        Result := GetEscapeString(VarToStr(Value));
      stBytes:
        begin
          TempBytes := TByteDynArray(Value);
          for I := 0 to High(TempBytes) do
            Result := Result + Char(TempBytes[I]);
          Result := GetEscapeString(Result);
        end;
      stDate:
        Result := '''' + FormatDateTime('yyyy-mm-dd', VarToDateTime(Value)) + '''';
      stTime:
        Result := '''' + FormatDateTime('hh:mm:ss', VarToDateTime(Value)) + '''';
      stTimestamp:
        Result := '''' + FormatDateTime('yyyy-mm-dd hh:mm:ss',
          VarToDateTime(Value)) + '''';
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          if VarType(Value) = varUnknown then
          begin
            TempBlob := IZBlob(TVarData(Value).VUnknown);
            if not TempBlob.IsEmpty then
              Result := GetEscapeString(TempBlob.GetString)
            else Result := 'NULL';
          end else
            Result := 'NULL';
        end;
    end;
  end;
end;

end.
