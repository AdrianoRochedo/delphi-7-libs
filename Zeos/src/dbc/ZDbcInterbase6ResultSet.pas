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

unit ZDbcInterbase6ResultSet;

interface

{$I ZDbc.inc}

uses
  Classes, ZSysUtils, ZCollections, ZDbcIntfs, ZDbcResultSet, ZDbcInterbase6,
  ZPlainInterbaseDriver, ZClasses, ZCompatibility, ZDbcResultSetMetadata,
  ZGenericSelectParser, ZDbcInterbase6Utils;

type

  {** Implements Interbase ResultSet Metadata. }
  TZInterbaseResultSetMetadata = class(TZAbstractResultSetMetadata)
  end;

  {** Implements PostgreSql ResultSet. }
  TZInterbase6ResultSet = class(TZAbstractResultSet)
  private
    FFetchStat: Integer;
    FCursorName: string;
    FStmtHandle: TISC_STMT_HANDLE;
    FSqlData: IZResultSQLDA;
    FParamsSqlData: IZParamsSQLDA;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure Open; override;
    function CreateResultSetMetadata(SQL: string; ColumnsInfo: IZCollection):
      IZResultSetMetadata; override;
    function GetFieldValue(ColumnIndex: Integer): Variant;
  public
    constructor Create(Statement: IZStatement; SQL: string;
      var StatementHandle: TISC_STMT_HANDLE; CursorName: string;
      SqlData: IZResultSQLDA; ParamsSqlData: IZParamsSQLDA);
    destructor Destroy; override;

    procedure Close; override;

    function GetCursorName: string; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetString(ColumnIndex: Integer): string; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): ShortInt; override;
    function GetShort(ColumnIndex: Integer): SmallInt; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): LongInt; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Int64; override;
    function GetBytes(ColumnIndex: Integer): TByteDynArray; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;

    function MoveAbsolute(Row: Integer): Boolean; override;
    function Next: Boolean; override;
  end;

  {** Implements external blob wrapper object for PostgreSQL. }
  TZInterbase6Blob = class(TZAbstractBlob)
  private
    FBlobId: TISC_QUAD;
    FBlobRead: Boolean;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure ReadBlob;
  public
    constructor Create(IBConnection: IZInterbase6Connection;
      var BlobId: TISC_QUAD);

    function IsEmpty: Boolean; override;
    function Clone: IZBlob; override;
    function GetStream: TStream; override;
    function GetString: string; override;
    function GetUnicodeString: WideString; override;
    function GetBytes: TByteDynArray; override;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZDbcUtils, SysUtils, Math, ZInterbaseSelectParser;

{ TZInterbase6ResultSet }

{**
  Releases this <code>ResultSet</code> object's database and
  JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZInterbase6ResultSet.Close;
begin
  if FStmtHandle <> nil then
  begin
    { Free output allocated memory }
    FSqlData := nil;
    FParamsSqlData := nil;
    { Free allocate sql statement }
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle);
  end;
inherited Close;
end;

{**
  Creates a result set a specific metadata object.
  @param SQL a SQL statement.
  @param ColumnsInfo a columns information object.
  @return a result set metadata object.
}
function TZInterbase6ResultSet.CreateResultSetMetadata(SQL: string;
  ColumnsInfo: IZCollection): IZResultSetMetadata;
var
  Metadata: IZDatabaseMetadata;
begin
  if GetStatement <> nil then
    Metadata := GetStatement.GetConnection.GetMetadata
  else Metadata := nil;
  Result := TZInterbaseResultSetMetadata.Create(Metadata, SQL, ColumnsInfo);
end;

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param Statement a related SQL statement object.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
  @param the sql out data previously allocated
  @param the Interbase sql dialect
}
constructor TZInterbase6ResultSet.Create(Statement: IZStatement; SQL: string;
    var StatementHandle: TISC_STMT_HANDLE; CursorName: string;
    SqlData: IZResultSQLDA; ParamsSqlData: IZParamsSQLDA);
begin
 inherited Create(Statement, SQL);
 FFetchStat := 0;
 FSqlData := SqlData;
 FCursorName := CursorName;
 FIBConnection := Statement.GetConnection as IZInterbase6Connection;


 FParamsSqlData := ParamsSqlData;
 FStmtHandle := StatementHandle;
 ResultSetType := rtForwardOnly;
 ResultSetConcurrency := rcReadOnly;
 Open;
end;

{**
   Free memory and destriy component
}
destructor TZInterbase6ResultSet.Destroy;
begin
  if not Closed then
    Close;
  inherited Destroy;
end;

{**
   Return field value by it index
   @param the index column 0 first, 1 second ...
   @return the field value as variant type
}
function TZInterbase6ResultSet.GetFieldValue(ColumnIndex: Integer): Variant;
begin
  CheckClosed;
  Result := FSqlData.GetValue(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6ResultSet.GetBigDecimal(ColumnIndex: Integer): Int64;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
  Result := FSqlData.GetBigDecimal(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZInterbase6ResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  BlobId: TISC_QUAD;
begin
  Result := nil;
  CheckClosed;
  CheckBlobColumn(ColumnIndex);

  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then Exit;

  if not LastWasNull then
  begin
    BlobId := FSqlData.GetQuad(ColumnIndex - 1);
    Result := TZInterbase6Blob.Create(FIBConnection, BlobId);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZInterbase6ResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBoolean);
  Result := FSqlData.GetBoolean(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6ResultSet.GetByte(ColumnIndex: Integer): ShortInt;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stByte);
  Result := FSqlData.GetByte(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6ResultSet.GetBytes(
  ColumnIndex: Integer): TByteDynArray;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBytes);
  Result := FSqlData.GetBytes(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6ResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDate);
  Result := FSqlData.GetDate(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6ResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDouble);
  Result := FSqlData.GetDouble(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6ResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stFloat);
  Result := FSqlData.GetFloat(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6ResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stInteger);
  Result := FSqlData.GetInt(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6ResultSet.GetLong(ColumnIndex: Integer): LongInt;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stLong);
  Result := FSqlData.GetLong(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6ResultSet.GetShort(ColumnIndex: Integer): SmallInt;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stShort);
  Result := FSqlData.GetShort(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6ResultSet.GetString(ColumnIndex: Integer): string;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stString);
  LastWasNull := IsNull(ColumnIndex);
  Result := FSqlData.GetString(ColumnIndex - 1);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6ResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTime);
  Result := FSqlData.GetTime(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
function TZInterbase6ResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
begin
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTimestamp);
  Result := FSqlData.GetTimestamp(ColumnIndex - 1);
  LastWasNull := IsNull(ColumnIndex);
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZInterbase6ResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
  Result := FSqlData.IsNull(ColumnIndex - 1);
end;

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
function TZInterbase6ResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := False;
  RaiseForwardOnlyException;
end;

{**
  Moves the cursor down one row from its current position.
  A <code>ResultSet</code> cursor is initially positioned
  before the first row; the first call to the method
  <code>next</code> makes the first row the current row; the
  second call makes the second row the current row, and so on.

  <P>If an input stream is open for the current row, a call
  to the method <code>next</code> will
  implicitly close it. A <code>ResultSet</code> object's
  warning chain is cleared when a new row is read.

  @return <code>true</code> if the new current row is valid;
    <code>false</code> if there are no more rows
}
function TZInterbase6ResultSet.Next: Boolean;
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (LastRowNo >= MaxRows) then
    Exit;

  { Fetch row. }
  if (ResultSetType = rtForwardOnly) and (FFetchStat = 0) then
  begin
    with FIBConnection do
    begin
      FFetchStat := GetPlainDriver.isc_dsql_fetch(@StatusVector,
        @FStmtHandle, GetDialect, FSqlData.GetData);
      CheckInterbase6Error(GetPlainDriver, StatusVector);
    end;

    if FFetchStat = 0 then
    begin
      RowNo := RowNo + 1;
      LastRowNo := RowNo;
      Result := True;
    end;
  end;
end;

{**
  Opens this recordset.
}
procedure TZInterbase6ResultSet.Open;
var
  I: Integer;
  FieldSqlType: TZSQLType;
  ColumnInfo: TZColumnInfo;
begin
  if not Assigned(FStmtHandle) then
    raise EZSQLException.Create('Can not retrieve a result set data.');

  ColumnsInfo.Clear;
  {$RANGECHECKS OFF}
  for I := 0 to FSqlData.GetFieldCount - 1 do
  begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo, FSqlData  do
    begin
      FieldSqlType := GetFieldSqlType(I);
      SetColumnName(GetSqlName(I));
      SetTableName(GetRelName(I));
      SetColumnLabel(GetAliasName(I));
      SetColumnType(FieldSqlType);

      case FieldSqlType of
        stString,
        stUnicodeString: SetPrecision(GetFieldLength(I));
      end;

      SetReadOnly((GetRelName(I) = '') or (GetSqlName(I) = '')
        or (GetSqlName(I) = 'RDB$DB_KEY') or (FieldSqlType = ZDbcIntfs.stUnknown ));

      if IsNullable(I) then
        SetNullable(ntNullable)
      else
        SetNullable(ntNoNulls);

      SetScale(GetFieldScale(I));
      SetAutoIncrement(False);
      //SetSigned(False);
      //SetCaseSensitive(True);
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;
  inherited Open;
end;

function TZInterbase6ResultSet.GetCursorName: string;
begin
  Result := FCursorName;
end;

{ TZInterbase6Blob }


function TZInterbase6Blob.Clone: IZBlob;
begin
  Result := TZInterbase6Blob.Create(FIBConnection, FBlobId);
end;
{**
  Reads the blob information by blob handle.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
}
constructor TZInterbase6Blob.Create(IBConnection: IZInterbase6Connection;
  var BlobId: TISC_QUAD);
begin
  FBlobId := BlobId;
  FBlobRead := False;
  FIBConnection := IBConnection;
end;

function TZInterbase6Blob.GetBytes: TByteDynArray;
begin
  ReadBlob;
  Result := inherited GetBytes;
end;

function TZInterbase6Blob.GetStream: TStream;
begin
  ReadBlob;
  Result := inherited GetStream;
end;

function TZInterbase6Blob.GetString: string;
begin
  ReadBlob;
  Result := inherited GetString;
end;

function TZInterbase6Blob.GetUnicodeString: WideString;
begin
  ReadBlob;
  Result := inherited GetUnicodeString;
end;

function TZInterbase6Blob.IsEmpty: Boolean;
begin
  ReadBlob;
  Result := inherited IsEmpty;
end;

procedure TZInterbase6Blob.ReadBlob;
var
  Size: Integer;
  Buffer: Pointer;
begin
  if FBlobRead then
   Exit;

  with FIBConnection do
    ReadBlobBufer(GetPlainDriver, GetDBHandle, GetTrHandle,
      FBlobId, Size, Buffer);
  BlobSize := Size;
  BlobData := Buffer;
  FBlobRead := True;
end;

end.
