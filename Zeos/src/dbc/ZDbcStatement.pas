{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Abstract Database Connectivity Classes          }
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

unit ZDbcStatement;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZSysUtils, ZParseIntfs, ZDbcIntfs, ZCompatibility,
  ZClasses;

type

  TVarArray = array of Variant;
  TZSQLTypeArray = array of TZSQLType;

  {** Implements Abstract Generic SQL Statement. }
  TZAbstractStatement = class(TInterfacedObject, IZStatement)
  private
    FMaxFieldSize: Integer;
    FMaxRows: Integer;
    FEscapeProcessing: Boolean;
    FQueryTimeout: Integer;
    FLastUpdateCount: Integer;
    FLastResultSet: IZResultSet;
    FFetchDirection: TZFetchDirection;
    FFetchSize: Integer;
    FResultSetConcurrency: TZResultSetConcurrency;
    FResultSetType: TZResultSetType;
    FCursorName: string;
    FBatchQueries: TStringList;
    FConnection: IZConnection;

    procedure SetLastResultSet(ResultSet: IZResultSet);
  protected
    constructor Create(Connection: IZConnection);
    procedure RaiseUnsupportedException;

    property MaxFieldSize: Integer read FMaxFieldSize write FMaxFieldSize;
    property MaxRows: Integer read FMaxRows write FMaxRows;
    property EscapeProcessing: Boolean
      read FEscapeProcessing write FEscapeProcessing;
    property QueryTimeout: Integer read FQueryTimeout write FQueryTimeout;
    property LastUpdateCount: Integer
      read FLastUpdateCount write FLastUpdateCount;
    property LastResultSet: IZResultSet
      read FLastResultSet write SetLastResultSet;
    property FetchDirection: TZFetchDirection
      read FFetchDirection write FFetchDirection;
    property FetchSize: Integer read FFetchSize write FFetchSize;
    property ResultSetConcurrency: TZResultSetConcurrency
      read FResultSetConcurrency write FResultSetConcurrency;
    property ResultSetType: TZResultSetType
      read FResultSetType write FResultSetType;
    property CursorName: string read FCursorName write FCursorName;
    property BatchQueries: TStringList read FBatchQueries;
    property Connection: IZConnection read FConnection;
  public
    destructor Destroy; override;

    function ExecuteQuery(SQL: string): IZResultSet; virtual;
    function ExecuteUpdate(SQL: string): Integer; virtual;
    procedure Close; virtual;

    function GetMaxFieldSize: Integer; virtual;
    procedure SetMaxFieldSize(Max: Integer); virtual;
    function GetMaxRows: Integer; virtual;
    procedure SetMaxRows(Max: Integer); virtual;
    procedure SetEscapeProcessing(Enable: Boolean); virtual;
    function GetQueryTimeout: Integer; virtual;
    procedure SetQueryTimeout(Seconds: Integer); virtual;
    procedure Cancel; virtual;
    procedure SetCursorName(Name: string); virtual;

    function Execute(SQL: string): Boolean; virtual;
    function GetResultSet: IZResultSet; virtual;
    function GetUpdateCount: Integer; virtual;
    function GetMoreResults: Boolean; virtual;

    procedure SetFetchDirection(Direction: TZFetchDirection); virtual;
    function GetFetchDirection: TZFetchDirection; virtual;
    procedure SetFetchSize(Rows: Integer); virtual;
    function GetFetchSize: Integer; virtual;

    procedure SetResultSetConcurrency(Concurrency: TZResultSetConcurrency);
      virtual;
    function GetResultSetConcurrency: TZResultSetConcurrency; virtual;
    procedure SetResultSetType(ResultSetType: TZResultSetType); virtual;
    function GetResultSetType: TZResultSetType; virtual;

    procedure AddBatch(SQL: string); virtual;
    procedure ClearBatch; virtual;
    function ExecuteBatch: TIntegerDynArray; virtual;

    function GetConnection: IZConnection; virtual;

    function GetWarnings: EZSQLWarning; virtual;
    procedure ClearWarnings; virtual;
  end;

  {** Implements Abstract Prepared SQL Statement. }
  TZAbstractPreparedStatement = class(TZAbstractStatement, IZPreparedStatement)
  private
    FSQL: string;
    FInParamValues: TVarArray;
    FInParamTypes: TZSQLTypeArray;
    FInParamCount: Integer;
  protected
    constructor Create(Connection: IZConnection; SQL: string);
    procedure SetInParamCount(NewParamCount: Integer); virtual;
    procedure SetInParam(ParameterIndex: Integer; SQLType: TZSQLType;
      Value: Variant); virtual;

    property SQL: string read FSQL write FSQL;
    property InParamValues: TVarArray
      read FInParamValues write FInParamValues;
    property InParamTypes: TZSQLTypeArray
      read FInParamTypes write FInParamTypes;
    property InParamCount: Integer read FInParamCount write FInParamCount;
  public
    destructor Destroy; override;

    function ExecuteQueryPrepared: IZResultSet; virtual;
    function ExecuteUpdatePrepared: Integer; virtual;
    function ExecutePrepared: Boolean; virtual;

    procedure SetNull(ParameterIndex: Integer; SqlType: TZSqlType); virtual;
    procedure SetBoolean(ParameterIndex: Integer; Value: Boolean); virtual;
    procedure SetByte(ParameterIndex: Integer; Value: ShortInt); virtual;
    procedure SetShort(ParameterIndex: Integer; Value: SmallInt); virtual;
    procedure SetInt(ParameterIndex: Integer; Value: Integer); virtual;
    procedure SetLong(ParameterIndex: Integer; Value: LongInt); virtual;
    procedure SetFloat(ParameterIndex: Integer; Value: Single); virtual;
    procedure SetDouble(ParameterIndex: Integer; Value: Double); virtual;
    procedure SetBigDecimal(ParameterIndex: Integer; Value: Int64); virtual;
    procedure SetPChar(ParameterIndex: Integer; Value: PChar); virtual;
    procedure SetString(ParameterIndex: Integer; Value: string); virtual;
    procedure SetUnicodeString(ParameterIndex: Integer;
      Value: WideString); virtual;
    procedure SetBytes(ParameterIndex: Integer; Value: TByteDynArray); virtual;
    procedure SetDate(ParameterIndex: Integer; Value: TDateTime); virtual;
    procedure SetTime(ParameterIndex: Integer; Value: TDateTime); virtual;
    procedure SetTimestamp(ParameterIndex: Integer; Value: TDateTime); virtual;
    procedure SetAsciiStream(ParameterIndex: Integer; Value: TStream); virtual;
    procedure SetUnicodeStream(ParameterIndex: Integer; Value: TStream); virtual;
    procedure SetBinaryStream(ParameterIndex: Integer; Value: TStream); virtual;
    procedure SetBlob(ParameterIndex: Integer; SqlType: TZSqlType;
      Value: IZBlob); virtual;

    procedure ClearParameters; virtual;

    procedure AddBatchPrepared; virtual;
    function GetMetaData: IZResultSetMetaData; virtual;
  end;

  {** Implements Abstract Callable SQL statement. }
  TZAbstractCallableStatement = class(TZAbstractPreparedStatement,
    IZCallableStatement)
  private
    FOutParamValues: TVarArray;
    FOutParamTypes: TZSQLTypeArray;
    FOutParamCount: Integer;
    FLastWasNull: Boolean;
  protected
    constructor Create(Connection: IZConnection; SQL: string);
    procedure SetOutParamCount(NewParamCount: Integer); virtual;
    function GetOutParam(ParameterIndex: Integer): Variant; virtual;

    property OutParamValues: TVarArray
      read FOutParamValues write FOutParamValues;
    property OutParamTypes: TZSQLTypeArray
      read FOutParamTypes write FOutParamTypes;
    property OutParamCount: Integer read FOutParamCount write FOutParamCount;
    property LastWasNull: Boolean read FLastWasNull write FLastWasNull;
  public
    destructor Destroy; override;

    procedure RegisterOutParameter(ParameterIndex: Integer;
      SqlType: Integer); virtual;
    function WasNull: Boolean; virtual;

    function IsNull(ParameterIndex: Integer): Boolean; virtual;
    function GetPChar(ParameterIndex: Integer): PChar; virtual;
    function GetString(ParameterIndex: Integer): string; virtual;
    function GetUnicodeString(ParameterIndex: Integer): WideString; virtual;
    function GetBoolean(ParameterIndex: Integer): Boolean; virtual;
    function GetByte(ParameterIndex: Integer): ShortInt; virtual;
    function GetShort(ParameterIndex: Integer): SmallInt; virtual;
    function GetInt(ParameterIndex: Integer): Integer; virtual;
    function GetLong(ParameterIndex: Integer): LongInt; virtual;
    function GetFloat(ParameterIndex: Integer): Single; virtual;
    function GetDouble(ParameterIndex: Integer): Double; virtual;
    function GetBigDecimal(ParameterIndex: Integer): Int64; virtual;
    function GetBytes(ParameterIndex: Integer): TByteDynArray; virtual;
    function GetDate(ParameterIndex: Integer): TDateTime; virtual;
    function GetTime(ParameterIndex: Integer): TDateTime; virtual;
    function GetTimestamp(ParameterIndex: Integer): TDateTime; virtual;
  end;

  {** Implements an Emulated Prepared SQL Statement. }
  TZEmulatedPreparedStatement = class(TZAbstractPreparedStatement)
  private
    FExecStatement: IZStatement;
    FSections: IZCollection;

  protected
    property ExecStatement: IZStatement read FExecStatement
      write FExecStatement;
    property Sections: IZCollection read FSections write FSections;

    function CreateExecStatement: IZStatement; virtual; abstract;
    function PrepareSQLParam(ParamIndex: Integer): string; virtual; abstract;
    function GetExecStatement: IZStatement;
    function SplitSQLQuery: IZCollection;
    function PrepareSQLQuery: string; virtual;

  public
    function ExecuteQuery(SQL: string): IZResultSet; override;
    function ExecuteUpdate(SQL: string): Integer; override;
    function Execute(SQL: string): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZCollections, ZTokenizer, ZGenericSqlAnalyser, ZDbcResultSet;

{ TZAbstractStatement }

{**
  Constructs this class and defines the main properties.
  @param Connection a database connection object.
}
constructor TZAbstractStatement.Create(Connection: IZConnection);
begin
  { Sets the default properties. }
  FMaxFieldSize := 0;
  FMaxRows := 0;
  FEscapeProcessing := False;
  FQueryTimeout := 0;
  FLastUpdateCount := -1;
  FLastResultSet := nil;
  FFetchDirection := fdForward;
  FFetchSize := 0;
  FResultSetConcurrency := rcReadOnly;
  FResultSetType := rtForwardOnly;
  FCursorName := '';

  FConnection := Connection;
  FBatchQueries := TStringList.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractStatement.Destroy;
begin
  FConnection := nil;
  FBatchQueries.Free;
//  LastResultSet := nil;
  inherited Destroy;
end;

{**
  Raises unsupported operation exception.
}
procedure TZAbstractStatement.RaiseUnsupportedException;
begin
  raise EZSQLException.Create('Unsupported operation.');
end;

{**
  Sets a last result set to avoid problems with reference counting.
  @param ResultSet the lastest executed result set.
}
procedure TZAbstractStatement.SetLastResultSet(ResultSet: IZResultSet);
begin
  if FLastResultSet <> nil then
    Self._AddRef;
  FLastResultSet := ResultSet;
  if FLastResultSet <> nil then
    Self._Release;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractStatement.ExecuteQuery(SQL: string): IZResultSet;
begin
  Result := nil;
  RaiseUnsupportedException;
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
function TZAbstractStatement.ExecuteUpdate(SQL: string): Integer;
begin
  Result := 0;
  RaiseUnsupportedException;
end;

{**
  Releases this <code>Statement</code> object's database
  and JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.
  It is generally good practice to release resources as soon as
  you are finished with them to avoid tying up database
  resources.
  <P><B>Note:</B> A <code>Statement</code> object is automatically closed when it is
  garbage collected. When a <code>Statement</code> object is closed, its current
  <code>ResultSet</code> object, if one exists, is also closed.
}
procedure TZAbstractStatement.Close;
begin
  LastResultSet := nil;
end;

{**
  Returns the maximum number of bytes allowed
  for any column value.
  This limit is the maximum number of bytes that can be
  returned for any column value.
  The limit applies only to <code>BINARY</code>,
  <code>VARBINARY</code>, <code>LONGVARBINARY</code>, <code>CHAR</code>, <code>VARCHAR</code>, and <code>LONGVARCHAR</code>
  columns.  If the limit is exceeded, the excess data is silently
  discarded.
  @return the current max column size limit; zero means unlimited
}
function TZAbstractStatement.GetMaxFieldSize: Integer;
begin
  Result := FMaxFieldSize;
end;

{**
  Sets the limit for the maximum number of bytes in a column to
  the given number of bytes.  This is the maximum number of bytes
  that can be returned for any column value.  This limit applies
  only to <code>BINARY</code>, <code>VARBINARY</code>,
  <code>LONGVARBINARY</code>, <code>CHAR</code>, <code>VARCHAR</code>, and
  <code>LONGVARCHAR</code> fields.  If the limit is exceeded, the excess data
  is silently discarded. For maximum portability, use values
  greater than 256.

  @param max the new max column size limit; zero means unlimited
}
procedure TZAbstractStatement.SetMaxFieldSize(Max: Integer);
begin
  FMaxFieldSize := Max;
end;

{**
  Retrieves the maximum number of rows that a
  <code>ResultSet</code> object can contain.  If the limit is exceeded, the excess
  rows are silently dropped.

  @return the current max row limit; zero means unlimited
}
function TZAbstractStatement.GetMaxRows: Integer;
begin
  Result := FMaxRows;
end;

{**
  Sets the limit for the maximum number of rows that any
  <code>ResultSet</code> object can contain to the given number.
  If the limit is exceeded, the excess rows are silently dropped.

  @param max the new max rows limit; zero means unlimited
}
procedure TZAbstractStatement.SetMaxRows(Max: Integer);
begin
  FMaxRows := Max;
end;

{**
  Sets escape processing on or off.
  If escape scanning is on (the default), the driver will do
  escape substitution before sending the SQL to the database.

  Note: Since prepared statements have usually been parsed prior
  to making this call, disabling escape processing for prepared
  statements will have no effect.

  @param enable <code>true</code> to enable; <code>false</code> to disable
}
procedure TZAbstractStatement.SetEscapeProcessing(Enable: Boolean);
begin
  FEscapeProcessing := Enable;
end;

{**
  Retrieves the number of seconds the driver will
  wait for a <code>Statement</code> object to execute. If the limit is exceeded, a
  <code>SQLException</code> is thrown.

  @return the current query timeout limit in seconds; zero means unlimited
}
function TZAbstractStatement.GetQueryTimeout: Integer;
begin
  Result := FQueryTimeout;
end;

{**
  Sets the number of seconds the driver will
  wait for a <code>Statement</code> object to execute to the given number of seconds.
  If the limit is exceeded, an <code>SQLException</code> is thrown.

  @param seconds the new query timeout limit in seconds; zero means unlimited
}
procedure TZAbstractStatement.SetQueryTimeout(Seconds: Integer);
begin
  FQueryTimeout := Seconds;
end;

{**
  Cancels this <code>Statement</code> object if both the DBMS and
  driver support aborting an SQL statement.
  This method can be used by one thread to cancel a statement that
  is being executed by another thread.
}
procedure TZAbstractStatement.Cancel;
begin
  RaiseUnsupportedException;
end;

{**
  Retrieves the first warning reported by calls on this <code>Statement</code> object.
  Subsequent <code>Statement</code> object warnings will be chained to this
  <code>SQLWarning</code> object.

  <p>The warning chain is automatically cleared each time
    a statement is (re)executed.

  <P><B>Note:</B> If you are processing a <code>ResultSet</code> object, any
  warnings associated with reads on that <code>ResultSet</code> object
  will be chained on it.

  @return the first <code>SQLWarning</code> object or <code>null</code>
}
function TZAbstractStatement.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

{**
  Clears all the warnings reported on this <code>Statement</code>
  object. After a call to this method,
  the method <code>getWarnings</code> will return
  <code>null</code> until a new warning is reported for this
  <code>Statement</code> object.
}
procedure TZAbstractStatement.ClearWarnings;
begin
end;

{**
  Defines the SQL cursor name that will be used by
  subsequent <code>Statement</code> object <code>execute</code> methods.
  This name can then be
  used in SQL positioned update/delete statements to identify the
  current row in the <code>ResultSet</code> object generated by this statement.  If
  the database doesn't support positioned update/delete, this
  method is a noop.  To insure that a cursor has the proper isolation
  level to support updates, the cursor's <code>SELECT</code> statement should be
  of the form 'select for update ...'. If the 'for update' phrase is
  omitted, positioned updates may fail.

  <P><B>Note:</B> By definition, positioned update/delete
  execution must be done by a different <code>Statement</code> object than the one
  which generated the <code>ResultSet</code> object being used for positioning. Also,
  cursor names must be unique within a connection.

  @param name the new cursor name, which must be unique within a connection
}
procedure TZAbstractStatement.SetCursorName(Name: string);
begin
  FCursorName := Name;
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
function TZAbstractStatement.Execute(SQL: string): Boolean;
begin
  Result := False;
  LastResultSet := nil;
  LastUpdateCount := -1;
end;

{**
  Returns the current result as a <code>ResultSet</code> object.
  This method should be called only once per result.

  @return the current result as a <code>ResultSet</code> object;
  <code>null</code> if the result is an update count or there are no more results
  @see #execute
}
function TZAbstractStatement.GetResultSet: IZResultSet;
begin
  Result := LastResultSet;
end;

{**
  Returns the current result as an update count;
  if the result is a <code>ResultSet</code> object or there are no more results, -1
  is returned. This method should be called only once per result.

  @return the current result as an update count; -1 if the current result is a
    <code>ResultSet</code> object or there are no more results
  @see #execute
}
function TZAbstractStatement.GetUpdateCount: Integer;
begin
  Result := FLastUpdateCount;
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
function TZAbstractStatement.GetMoreResults: Boolean;
begin
  Result := False;
end;

{**
  Retrieves the direction for fetching rows from
  database tables that is the default for result sets
  generated from this <code>Statement</code> object.
  If this <code>Statement</code> object has not set
  a fetch direction by calling the method <code>setFetchDirection</code>,
  the return value is implementation-specific.

  @return the default fetch direction for result sets generated
    from this <code>Statement</code> object
}
function TZAbstractStatement.GetFetchDirection: TZFetchDirection;
begin
  Result := FFetchDirection;
end;

{**
  Gives the driver a hint as to the direction in which
  the rows in a result set
  will be processed. The hint applies only to result sets created
  using this <code>Statement</code> object.  The default value is
  <code>ResultSet.FETCH_FORWARD</code>.
  <p>Note that this method sets the default fetch direction for
  result sets generated by this <code>Statement</code> object.
  Each result set has its own methods for getting and setting
  its own fetch direction.
  @param direction the initial direction for processing rows
}
procedure TZAbstractStatement.SetFetchDirection(Direction: TZFetchDirection);
begin
  FFetchDirection := Direction;
end;

{**
  Retrieves the number of result set rows that is the default
  fetch size for result sets
  generated from this <code>Statement</code> object.
  If this <code>Statement</code> object has not set
  a fetch size by calling the method <code>setFetchSize</code>,
  the return value is implementation-specific.
  @return the default fetch size for result sets generated
    from this <code>Statement</code> object
}
function TZAbstractStatement.GetFetchSize: Integer;
begin
  Result := FFetchSize;
end;

{**
  Gives the JDBC driver a hint as to the number of rows that should
  be fetched from the database when more rows are needed.  The number
  of rows specified affects only result sets created using this
  statement. If the value specified is zero, then the hint is ignored.
  The default value is zero.

  @param rows the number of rows to fetch
}
procedure TZAbstractStatement.SetFetchSize(Rows: Integer);
begin
  FFetchSize := Rows;
end;

{**
  Sets a result set concurrency for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param Concurrency either <code>ResultSet.CONCUR_READ_ONLY</code> or
  <code>ResultSet.CONCUR_UPDATABLE</code>
}
procedure TZAbstractStatement.SetResultSetConcurrency(
  Concurrency: TZResultSetConcurrency);
begin
  FResultSetConcurrency := Concurrency;
end;

{**
  Retrieves the result set concurrency for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @return either <code>ResultSet.CONCUR_READ_ONLY</code> or
  <code>ResultSet.CONCUR_UPDATABLE</code>
}
function TZAbstractStatement.GetResultSetConcurrency: TZResultSetConcurrency;
begin
  Result := FResultSetConcurrency;
end;

{**
  Sets a result set type for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param ResultSetType one of <code>ResultSet.TYPE_FORWARD_ONLY</code>,
    <code>ResultSet.TYPE_SCROLL_INSENSITIVE</code>, or
    <code>ResultSet.TYPE_SCROLL_SENSITIVE</code>
}
procedure TZAbstractStatement.SetResultSetType(
  ResultSetType: TZResultSetType);
begin
  FResultSetType := ResultSetType;
end;

{**
  Retrieves the result set type for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @return one of <code>ResultSet.TYPE_FORWARD_ONLY</code>,
    <code>ResultSet.TYPE_SCROLL_INSENSITIVE</code>, or
    <code>ResultSet.TYPE_SCROLL_SENSITIVE</code>
}
function TZAbstractStatement.GetResultSetType: TZResultSetType;
begin
  Result := FResultSetType;
end;

{**
  Adds an SQL command to the current batch of commmands for this
  <code>Statement</code> object. This method is optional.

  @param sql typically this is a static SQL <code>INSERT</code> or
  <code>UPDATE</code> statement
}
procedure TZAbstractStatement.AddBatch(SQL: string);
begin
  FBatchQueries.Add(SQL);
end;

{**
  Makes the set of commands in the current batch empty.
  This method is optional.
}
procedure TZAbstractStatement.ClearBatch;
begin
  FBatchQueries.Clear;
end;

{**
  Submits a batch of commands to the database for execution and
  if all commands execute successfully, returns an array of update counts.
  The <code>int</code> elements of the array that is returned are ordered
  to correspond to the commands in the batch, which are ordered
  according to the order in which they were added to the batch.
  The elements in the array returned by the method <code>executeBatch</code>
  may be one of the following:
  <OL>
  <LI>A number greater than or equal to zero -- indicates that the
  command was processed successfully and is an update count giving the
  number of rows in the database that were affected by the command's
  execution
  <LI>A value of <code>-2</code> -- indicates that the command was
  processed successfully but that the number of rows affected is
  unknown
  <P>
  If one of the commands in a batch update fails to execute properly,
  this method throws a <code>BatchUpdateException</code>, and a JDBC
  driver may or may not continue to process the remaining commands in
  the batch.  However, the driver's behavior must be consistent with a
  particular DBMS, either always continuing to process commands or never
  continuing to process commands.  If the driver continues processing
  after a failure, the array returned by the method
  <code>BatchUpdateException.getUpdateCounts</code>
  will contain as many elements as there are commands in the batch, and
  at least one of the elements will be the following:
  <P>
  <LI>A value of <code>-3</code> -- indicates that the command failed
  to execute successfully and occurs only if a driver continues to
  process commands after a command fails
  </OL>
  <P>
  A driver is not required to implement this method.
  The possible implementations and return values have been modified in
  the Java 2 SDK, Standard Edition, version 1.3 to
  accommodate the option of continuing to proccess commands in a batch
  update after a <code>BatchUpdateException</code> obejct has been thrown.

  @return an array of update counts containing one element for each
  command in the batch.  The elements of the array are ordered according
  to the order in which commands were added to the batch.
}
function TZAbstractStatement.ExecuteBatch: TIntegerDynArray;
var
  I: Integer;
begin
  SetLength(Result, FBatchQueries.Count);
  for I := 0 to FBatchQueries.Count -1 do
  begin
    Result[I] := ExecuteUpdate(FBatchQueries[I]);
  end;
  ClearBatch;
end;

{**
  Returns the <code>Connection</code> object
  that produced this <code>Statement</code> object.
  @return the connection that produced this statement
}
function TZAbstractStatement.GetConnection: IZConnection;
begin
  Result := FConnection;
end;

{ TZAbstractPreparedStatement }

{**
  Constructs this object and assigns main properties.
  @param Connection a database connection object.
  @param Sql a prepared Sql statement.
}
constructor TZAbstractPreparedStatement.Create(Connection: IZConnection;
  SQL: string);
begin
  inherited Create(Connection);
  FSQL := SQL;
  FInParamCount := 0;
  SetInParamCount(0);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractPreparedStatement.Destroy;
begin
  inherited Destroy;
  ClearParameters;
end;

{**
  Sets a new parameter count and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractPreparedStatement.SetInParamCount(NewParamCount: Integer);
var
  I: Integer;
begin
  SetLength(FInParamValues, NewParamCount);
  SetLength(FInParamTypes, NewParamCount);
  for I := FInParamCount to NewParamCount - 1 do
  begin
    FInParamValues[I] := Unassigned;
    FInParamTypes[I] := stUnknown;
  end;
  FInParamCount := NewParamCount;
end;

{**
  Sets a variant value into specified parameter.
  @param ParameterIndex a index of the parameter.
  @param SqlType a parameter SQL type.
  @paran Value a new parameter value.
}
procedure TZAbstractPreparedStatement.SetInParam(ParameterIndex: Integer;
  SQLType: TZSQLType; Value: Variant);
begin
  if ParameterIndex >= FInParamCount then
    SetInParamCount(ParameterIndex);

  FInParamTypes[ParameterIndex - 1] := SQLType;
  FInParamValues[ParameterIndex - 1] := Value;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAbstractPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Result := nil;
  RaiseUnsupportedException;
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
function TZAbstractPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := -1;
  RaiseUnsupportedException;
end;

{**
  Sets the designated parameter to SQL <code>NULL</code>.
  <P><B>Note:</B> You must specify the parameter's SQL type.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param sqlType the SQL type code defined in <code>java.sql.Types</code>
}
procedure TZAbstractPreparedStatement.SetNull(ParameterIndex: Integer;
  SqlType: TZSqlType);
begin
  SetInParam(ParameterIndex, SqlType, Null);
end;

{**
  Sets the designated parameter to a Java <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetBoolean(ParameterIndex: Integer;
  Value: Boolean);
begin
  SetInParam(ParameterIndex, stBoolean, Value);
end;

{**
  Sets the designated parameter to a Java <code>byte</code> value.
  The driver converts this
  to an SQL <code>TINYINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetByte(ParameterIndex: Integer;
  Value: ShortInt);
begin
  SetInParam(ParameterIndex, stByte, Value);
end;

{**
  Sets the designated parameter to a Java <code>short</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetShort(ParameterIndex: Integer;
  Value: SmallInt);
begin
  SetInParam(ParameterIndex, stShort, Value);
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetInt(ParameterIndex,
  Value: Integer);
begin
  SetInParam(ParameterIndex, stInteger, Value);
end;

{**
  Sets the designated parameter to a Java <code>long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetLong(ParameterIndex,
  Value: LongInt);
begin
  SetInParam(ParameterIndex, stLong, Value);
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetFloat(ParameterIndex: Integer;
  Value: Single);
begin
  SetInParam(ParameterIndex, stFloat, Value);
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetDouble(ParameterIndex: Integer;
  Value: Double);
begin
  SetInParam(ParameterIndex, stDouble, Value);
end;

{**
  Sets the designated parameter to a <code>java.math.BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetBigDecimal(
  ParameterIndex: Integer; Value: Int64);
begin
{$IFDEF VER130BELOW}
  SetInParam(ParameterIndex, stBigDecimal, Integer(Value));
{$ELSE}
  SetInParam(ParameterIndex, stBigDecimal, Value);
{$ENDIF}
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetPChar(ParameterIndex: Integer;
  Value: PChar);
begin
  SetInParam(ParameterIndex, stString, string(Value));
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetString(ParameterIndex: Integer;
  Value: string);
begin
  SetInParam(ParameterIndex, stString, Value);
end;

{**
  Sets the designated parameter to a Object Pascal <code>WideString</code>
  value. The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetUnicodeString(ParameterIndex: Integer;
  Value: WideString);
begin
  SetInParam(ParameterIndex, stUnicodeString, Value);
end;

{**
  Sets the designated parameter to a Java array of bytes.  The driver converts
  this to an SQL <code>VARBINARY</code> or <code>LONGVARBINARY</code>
  (depending on the argument's size relative to the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetBytes(ParameterIndex: Integer;
  Value: TByteDynArray);
begin
  SetInParam(ParameterIndex, stBytes, Value);
end;

{**
  Sets the designated parameter to a <code<java.sql.Date</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetDate(ParameterIndex: Integer;
  Value: TDateTime);
begin
  SetInParam(ParameterIndex, stDate, Value);
end;

{**
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetTime(ParameterIndex: Integer;
  Value: TDateTime);
begin
  SetInParam(ParameterIndex, stTime, Value);
end;

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetTimestamp(ParameterIndex: Integer;
  Value: TDateTime);
begin
  SetInParam(ParameterIndex, stTimestamp, Value);
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large ASCII value is input to a <code>LONGVARCHAR</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code>. Data will be read from the stream
  as needed until end-of-file is reached.  The JDBC driver will
  do any necessary conversion from ASCII to the database char format.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the Java input stream that contains the ASCII parameter value
  @param length the number of bytes in the stream
}
procedure TZAbstractPreparedStatement.SetAsciiStream(
  ParameterIndex: Integer; Value: TStream);
begin
  SetBlob(ParameterIndex, stAsciiStream, TZAbstractBlob.CreateWithStream(Value));
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large UNICODE value is input to a <code>LONGVARCHAR</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code> object. The data will be read from the stream
  as needed until end-of-file is reached.  The JDBC driver will
  do any necessary conversion from UNICODE to the database char format.
  The byte format of the Unicode stream must be Java UTF-8, as
  defined in the Java Virtual Machine Specification.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the java input stream which contains the UNICODE parameter value
}
procedure TZAbstractPreparedStatement.SetUnicodeStream(
  ParameterIndex: Integer; Value: TStream);
begin
  SetBlob(ParameterIndex, stUnicodeStream, TZAbstractBlob.CreateWithStream(Value));
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large binary value is input to a <code>LONGVARBINARY</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code> object. The data will be read from the stream
  as needed until end-of-file is reached.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the java input stream which contains the binary parameter value
}
procedure TZAbstractPreparedStatement.SetBinaryStream(
  ParameterIndex: Integer; Value: TStream);
begin
  SetBlob(ParameterIndex, stBinaryStream, TZAbstractBlob.CreateWithStream(Value));
end;

{**
  Sets a blob object for the specified parameter.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @param Value the java blob object.
}
procedure TZAbstractPreparedStatement.SetBlob(ParameterIndex: Integer;
  SqlType: TZSqlType; Value: IZBlob);
begin
  if not (SqlType in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
    raise EZSQLException.Create('Wrong type for Blob parameter.');
  SetInParam(ParameterIndex, SqlType, Value);
end;

{**
  Clears the current parameter values immediately.
  <P>In general, parameter values remain in force for repeated use of a
  statement. Setting a parameter value automatically clears its
  previous value.  However, in some cases it is useful to immediately
  release the resources used by the current parameter values; this can
  be done by calling the method <code>clearParameters</code>.
}
procedure TZAbstractPreparedStatement.ClearParameters;
var
  I: Integer;
begin
  for I := 1 to FInParamCount do
    SetInParam(I, stUnknown, Unassigned);
  SetInParamCount(0);
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAbstractPreparedStatement.ExecutePrepared: Boolean;
begin
  Result := False;
  RaiseUnsupportedException;
end;

{**
  Adds a set of parameters to this <code>PreparedStatement</code>
  object's batch of commands.
  @see Statement#addBatch
}
procedure TZAbstractPreparedStatement.AddBatchPrepared;
begin
end;

{**
  Gets the number, types and properties of a <code>ResultSet</code>
  object's columns.
  @return the description of a <code>ResultSet</code> object's columns
}
function TZAbstractPreparedStatement.GetMetaData: IZResultSetMetaData;
begin
  Result := nil;
  RaiseUnsupportedException;
end;

{ TZAbstractCallableStatement }

{**
  Constructs this object and assigns main properties.
  @param Connection a database connection object.
  @param Sql a prepared Sql statement.
}
constructor TZAbstractCallableStatement.Create(Connection: IZConnection;
  SQL: string);
begin
  inherited Create(Connection, SQL);
  FOutParamCount := 0;
  SetOutParamCount(0);
  FLastWasNull := True;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractCallableStatement.Destroy;
begin
  inherited Destroy;
end;

{**
  Sets a new parameter count and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractCallableStatement.SetOutParamCount(NewParamCount: Integer);
var
  I: Integer;
begin
  SetLength(FOutParamValues, NewParamCount);
  SetLength(FOutParamTypes, NewParamCount);
  for I := FOutParamCount to NewParamCount - 1 do
  begin
    FOutParamValues[I] := Unassigned;
    FOutParamTypes[I] := stUnknown;
  end;
  FOutParamCount := NewParamCount;
end;

{**
  Registers the OUT parameter in ordinal position
  <code>parameterIndex</code> to the JDBC type
  <code>sqlType</code>.  All OUT parameters must be registered
  before a stored procedure is executed.
  <p>
  The JDBC type specified by <code>sqlType</code> for an OUT
  parameter determines the Java type that must be used
  in the <code>get</code> method to read the value of that parameter.
  <p>
  If the JDBC type expected to be returned to this output parameter
  is specific to this particular database, <code>sqlType</code>
  should be <code>java.sql.Types.OTHER</code>.  The method retrieves the value.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @param sqlType the JDBC type code defined by <code>java.sql.Types</code>.
  If the parameter is of JDBC type <code>NUMERIC</code>
  or <code>DECIMAL</code>, the version of
  <code>registerOutParameter</code> that accepts a scale value should be used.
}
procedure TZAbstractCallableStatement.RegisterOutParameter(ParameterIndex,
  SqlType: Integer);
begin

end;

function TZAbstractCallableStatement.GetOutParam(ParameterIndex: Integer): Variant;
begin
  Result := OutParamValues[ParameterIndex - 1];
  FLastWasNull := VarIsNull(Result) or VarIsEmpty(Result);
end;

{**
  Indicates whether or not the last OUT parameter read had the value of
  SQL <code>NULL</code>.  Note that this method should be called only after
  calling a <code>getXXX</code> method; otherwise, there is no value to use in
  determining whether it is <code>null</code> or not.
  @return <code>true</code> if the last parameter read was SQL
  <code>NULL</code>; <code>false</code> otherwise
}
function TZAbstractCallableStatement.WasNull: Boolean;
begin
  Result := FLastWasNull;
end;

{**
  Indicates whether or not the specified OUT parameter read had the value of
  SQL <code>NULL</code>.
  @return <code>true</code> if the parameter read was SQL
  <code>NULL</code>; <code>false</code> otherwise
}
function TZAbstractCallableStatement.IsNull(ParameterIndex: Integer): Boolean;
begin
  GetOutParam(ParameterIndex);
  Result := FLastWasNull;
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>String</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement.GetPChar(
  ParameterIndex: Integer): PChar;
begin
  Result := PChar(GetString(ParameterIndex));
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>String</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement.GetString(
  ParameterIndex: Integer): string;
begin
  Result := GetOutParam(ParameterIndex);
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>WideString</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement.GetUnicodeString(
  ParameterIndex: Integer): WideString;
begin
  Result := GetOutParam(ParameterIndex);
end;

{**
  Gets the value of a JDBC <code>BIT</code> parameter as a <code>boolean</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>false</code>.
}
function TZAbstractCallableStatement.GetBoolean(
  ParameterIndex: Integer): Boolean;
begin
  Result := GetOutParam(ParameterIndex);
end;

{**
  Gets the value of a JDBC <code>TINYINT</code> parameter as a <code>byte</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetByte(
  ParameterIndex: Integer): ShortInt;
begin
  Result := GetOutParam(ParameterIndex);
end;

{**
  Gets the value of a JDBC <code>SMALLINT</code> parameter as a <code>short</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetShort(
  ParameterIndex: Integer): SmallInt;
begin
  Result := GetOutParam(ParameterIndex);
end;

{**
  Gets the value of a JDBC <code>INTEGER</code> parameter as an <code>int</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetInt(
  ParameterIndex: Integer): Integer;
begin
  Result := GetOutParam(ParameterIndex);
end;

{**
  Gets the value of a JDBC <code>BIGINT</code> parameter as a <code>long</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetLong(
  ParameterIndex: Integer): LongInt;
begin
  Result := GetOutParam(ParameterIndex);
end;

{**
  Gets the value of a JDBC <code>FLOAT</code> parameter as a <code>float</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetFloat(
  ParameterIndex: Integer): Single;
begin
  Result := GetOutParam(ParameterIndex);
end;

{**
  Gets the value of a JDBC <code>DOUBLE</code> parameter as a <code>double</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetDouble(
  ParameterIndex: Integer): Double;
begin
  Result := GetOutParam(ParameterIndex);
end;

{**
  Gets the value of a JDBC <code>NUMERIC</code> parameter as a
  <code>java.math.BigDecimal</code> object with scale digits to
  the right of the decimal point.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result is
  <code>null</code>.
}
function TZAbstractCallableStatement.GetBigDecimal(
  ParameterIndex: Integer): Int64;
begin
{$IFDEF VER130BELOW}
   Result := Integer(VarAsType(GetOutParam(ParameterIndex), varInteger));
{$ELSE}
  Result := VarAsType(GetOutParam(ParameterIndex), varInt64);
{$ENDIF}
end;

{**
  Gets the value of a JDBC <code>BINARY</code> or <code>VARBINARY</code>
  parameter as an array of <code>byte</code> values in the Java
  programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result is
   <code>null</code>.
}
function TZAbstractCallableStatement.GetBytes(
  ParameterIndex: Integer): TByteDynArray;
begin
  Result := GetOutParam(ParameterIndex);
end;

{**
  Gets the value of a JDBC <code>DATE</code> parameter as a
  <code>java.sql.Date</code> object.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
}
function TZAbstractCallableStatement.GetDate(
  ParameterIndex: Integer): TDateTime;
begin
  Result := GetOutParam(ParameterIndex);
end;

{**
  Get the value of a JDBC <code>TIME</code> parameter as a
  <code>java.sql.Time</code> object.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
}
function TZAbstractCallableStatement.GetTime(ParameterIndex: Integer): TDateTime;
begin
  Result := GetOutParam(ParameterIndex);
end;

{**
  Gets the value of a JDBC <code>TIMESTAMP</code> parameter as a
  <code>java.sql.Timestamp</code> object.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
}
function TZAbstractCallableStatement.GetTimestamp(
  ParameterIndex: Integer): TDateTime;
begin
  Result := GetOutParam(ParameterIndex);
end;

{ TZEmulatedPreparedStatement }

{**
  Creates a temporary statement which executes queries.
  @return a created statement object.
}
function TZEmulatedPreparedStatement.GetExecStatement: IZStatement;
begin
  if ExecStatement = nil then
  begin
    ExecStatement := CreateExecStatement;

    ExecStatement.SetMaxFieldSize(GetMaxFieldSize);
    ExecStatement.SetMaxRows(GetMaxRows);
    ExecStatement.SetEscapeProcessing(EscapeProcessing);
    ExecStatement.SetQueryTimeout(GetQueryTimeout);
    ExecStatement.SetCursorName(CursorName);

    ExecStatement.SetFetchDirection(GetFetchDirection);
    ExecStatement.SetFetchSize(GetFetchSize);
    ExecStatement.SetResultSetConcurrency(GetResultSetConcurrency);
    ExecStatement.SetResultSetType(GetResultSetType);
  end;
  Result := ExecStatement;
end;

{**
  Splits a SQL query into a list of sections.
  @returns a list of splitted sections.
}
function TZEmulatedPreparedStatement.SplitSQLQuery: IZCollection;
var
  Tokenizer: IZTokenizer;
  Analyser: IZStatementAnalyser;
begin
  if FSections = nil then
  begin
    Tokenizer := (Connection.GetMetadata as IZExtraDatabaseMetadata).
      GetTokenizer;
    Analyser := (Connection.GetMetadata as IZExtraDatabaseMetadata).
      GetStatementAnalyser;
    FSections := Analyser.SplitSections(
      Analyser.SplitTokens(Tokenizer, SQL, True));
  end;
  Result := FSections.Clone as IZCollection;
end;

{**
  Prepares an SQL statement and inserts all data values.
  @return a prepared SQL statement.
}
function TZEmulatedPreparedStatement.PrepareSQLQuery: string;
var
  I, J: Integer;
  ParamIndex: Integer;
  ResultSections: IZCollection;
  Section: IZStatementSection;
  Tokens: IZCollection;
  WhereTokens: IZCollection;
  Analyser: IZStatementAnalyser;
  ParamValue: string;
  ReplaceNulls: Boolean;
begin
  { Optimization for queries without parameters. }
  if Pos('?', SQL) = 0 then
  begin
    Result := SQL;
    Exit;
  end;

  ParamIndex := 0;
  Result := '';
  WhereTokens := nil;
  if Sections = nil then
    Sections := SplitSQLQuery;
  Analyser := (Connection.GetMetadata as IZExtraDatabaseMetadata).
    GetStatementAnalyser;
  ReplaceNulls := False;
  ResultSections := TZCollection.Create;

  { Insert parameters and define a where section. }
  for I := 0 to Sections.Count - 1 do
  begin
    Section := (Sections[I] as IZStatementSection).Clone as IZStatementSection;
    ResultSections.Add(Section);
    Tokens := Section.GetTokens;
    if Section.GetName = 'WHERE' then
      WhereTokens := Tokens;
    J := 0;
    while J < Tokens.Count do
    begin
      if (Tokens[J] as IZToken).GetString = '?' then
      begin
        if (Tokens.Count > J + 1)
          and ((Tokens[J + 1] as IZToken).GetString = '?') then
        begin
          Inc(J);
        end
        else
        begin
          ParamValue := PrepareSQLParam(ParamIndex);
          Tokens[J] := TZToken.CreateWithType('', ParamValue);
          if (ParamValue = 'NULL') and (Tokens = WhereTokens) then
            ReplaceNulls := True;
          Inc(ParamIndex);
        end;
      end;
      Inc(J);
    end;
  end;

  { Replace nulls in the where section. }
  if ReplaceNulls and (WhereTokens <> nil) then
  begin
    Tokens := Analyser.ReplaceIsNull(WhereTokens);
    WhereTokens.Clear;
    WhereTokens.AddAll(Tokens);
  end;

  { Composes the result. }
  Result := Analyser.ComposeSections(ResultSections);
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
function TZEmulatedPreparedStatement.Execute(SQL: string): Boolean;
var
  ExecStatement: IZStatement;
begin
  ExecStatement := GetExecStatement;
  Result := ExecStatement.Execute(SQL);
  if Result then
    LastResultSet := ExecStatement.GetResultSet
  else
    LastUpdateCount := ExecStatement.GetUpdateCount;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZEmulatedPreparedStatement.ExecuteQuery(SQL: string): IZResultSet;
begin
  Result := GetExecStatement.ExecuteQuery(SQL);
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
function TZEmulatedPreparedStatement.ExecuteUpdate(SQL: string): Integer;
begin
  Result := GetExecStatement.ExecuteUpdate(SQL);
  LastUpdateCount := Result;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZEmulatedPreparedStatement.ExecutePrepared: Boolean;
begin
  Result := Execute(PrepareSQLQuery);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZEmulatedPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Result := ExecuteQuery(PrepareSQLQuery);
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
function TZEmulatedPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := ExecuteUpdate(PrepareSQLQuery);
end;

end.
