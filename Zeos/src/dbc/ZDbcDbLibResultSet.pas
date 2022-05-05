{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         DBLib Resultset common functionality            }
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

unit ZDbcDbLibResultSet;

interface

{$I ZDbc.inc}

uses
{$IFNDEF VER130BELOW}
  DateUtils,
{$ENDIF}
  Classes, SysUtils, ZClasses, ZSysUtils, ZCollections, ZDbcIntfs, ZDbcResultSet,
  ZDbcResultsetMetadata, ZCompatibility, ZGenericSelectParser, ZDbcDBLib,
  ZPlainDBLibDriver;

type
  {** Implements DBLib ResultSet. }
  TZDBLibResultSet = class(TZAbstractResultSet)
  private
    FHandle: PDBPROCESS;
    DBLibColTypeCache: TSmallIntDynArray;
    DBLibColumnCount: Integer;
    procedure CheckColumnIndex(ColumnIndex: Integer);
  protected
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: IZDBLibPlainDriver;
    procedure Open; override;
  public
    constructor Create(Statement: IZStatement; SQL: string; Handle: PDBPROCESS);
    destructor Destroy; override;

    procedure Close; override;

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

implementation

uses Math, ZDbcUtils, ZDbcDBLibUtils, ZSybaseSelectParser;

{ TZDBLibResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param Statement a related SQL statement object.
  @param Handle a DBLib specific query handle.
}
constructor TZDBLibResultSet.Create(Statement: IZStatement; SQL: string;
  Handle: PDBPROCESS);
begin
  inherited Create(Statement, SQL);
  Statement.GetConnection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  FPlainDriver := FDBLibConnection.GetPlainDriver;
  FHandle := Handle;
  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZDBLibResultSet.Destroy;
begin
{ TODO -ofjanos -cGeneral : Does it need close here? }
  Close;
  inherited Destroy;
end;

{**
  Opens this recordset.
}
procedure TZDBLibResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  ColName: string;
  ColType: Integer;
begin
//Check if the current statement can return rows
  if FPlainDriver.dbCmdRow(FHandle) <> DBSUCCEED then
    raise EZSQLException.Create('Can not retrieve result set data.');

  { Fills the column info }
  ColumnsInfo.Clear;
  DBLibColumnCount := FPlainDriver.dbnumcols(FHandle);
  SetLength(DBLibColTypeCache, DBLibColumnCount + 1);
  for I := 1 to DBLibColumnCount do
  begin
    ColumnInfo := TZColumnInfo.Create;
    ColName := FPlainDriver.dbColName(FHandle, I);
    ColType := FPlainDriver.dbcoltype(FHandle, I);
    ColumnInfo.SetColumnLabel(ColName);
    ColumnInfo.SetColumnName(ColName);
    ColumnInfo.SetColumnType(ConvertDBLibToSqlType(ColType));
    ColumnInfo.SetPrecision(FPlainDriver.dbcollen(FHandle, I));
    ColumnInfo.SetCurrency(ColType in [SQLMONEY, SQLMONEY4, SQLMONEYN]);
    if ColType = SQLINT1 then
      ColumnInfo.SetSigned(False)
    else
      ColumnInfo.SetSigned(True);
    ColumnsInfo.Add(ColumnInfo);

    DBLibColTypeCache[I] := ColType;
  end;
  inherited Open;
end;

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
procedure TZDBLibResultSet.Close;
begin
{ TODO -ofjanos -cGeneral : Maybe it needs a dbcanquery here. }
//  if Assigned(FHandle) then
//    if FPlainDriver.dbCanQuery(FHandle) <> DBSUCCEED then
//      FDBLibConnection.CheckDBLibError;
  FHandle := nil;
  SetLength(DBLibColTypeCache, 0);
  inherited Close;
end;

{**
  Checks if the columnindex is in the proper range.
  An exception is generated if somthing is not ok.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZDBLibResultSet.CheckColumnIndex(ColumnIndex: Integer);
begin
  if (ColumnIndex > DBLibColumnCount) or (ColumnIndex < 1) then
    raise EZSQLException.Create(
      Format('Column with index %d is not accessable.', [ColumnIndex]));
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZDBLibResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);
  Result := FPlainDriver.dbData(FHandle, ColumnIndex) = nil;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.GetString(ColumnIndex: Integer): string;
var
  DL: Integer;
  Data: Pointer;
  DT: Integer;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);

  DL := FPlainDriver.dbDatLen(FHandle, ColumnIndex);
  Data := FPlainDriver.dbdata(FHandle, ColumnIndex);
  DT := DBLibColTypeCache[ColumnIndex];
  LastWasNull := Data = nil;

  if Assigned(Data) then
  begin
    if DT = SQLCHAR then
    begin
      while (DL > 0) and (PChar(Integer(Data) + DL - 1)^ = ' ') do Dec(DL);
      if DL > 0 then
      begin
        SetLength(Result, DL);
        Move(Data^, PChar(Result)^, DL);
      end;
    end
    else
    begin
      SetLength(Result, 4001);
      DL := FPlainDriver.dbconvert(FHandle, DT, Data, DL, SQLCHAR, Pointer(PChar(Result)), Length(Result));
      while (DL > 0) and (Result[DL] = ' ') do Dec(DL);
      SetLength(Result, DL);
    end;
  end
  else
    Result := '';
  FDBLibConnection.CheckDBLibError;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZDBLibResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  DL: Integer;
  Data: Pointer;
  DT: Integer;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);

  DL := FPlainDriver.dbdatlen(FHandle, ColumnIndex);
  Data := FPlainDriver.dbdata(FHandle, ColumnIndex);
  DT := DBLibColTypeCache[ColumnIndex];
  LastWasNull := Data = nil;

  Result := False;
  if Assigned(Data) then
  begin
    if DT = SQLBIT then
      Result := PBoolean(Data)^
    else
    begin
      FPlainDriver.dbconvert(FHandle, DT, Data, DL, SQLBIT,
        @Result, SizeOf(Result));
    end;
  end;
  FDBLibConnection.CheckDBLibError;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetByte(ColumnIndex: Integer): ShortInt;
var
  DL: Integer;
  Data: Pointer;
  DT: Integer;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);

  DL := FPlainDriver.dbdatlen(FHandle, ColumnIndex);
  Data := FPlainDriver.dbdata(FHandle, ColumnIndex);
  DT := DBLibColTypeCache[ColumnIndex];
  LastWasNull := Data = nil;

  Result := 0;
  if Assigned(Data) then
  begin
    if DT = SQLINT1 then
      Result := PShortInt(Data)^
    else
    begin
      FPlainDriver.dbconvert(FHandle, DT, Data, DL, SQLINT1,
        @Result, SizeOf(Result));
    end;
  end;
  FDBLibConnection.CheckDBLibError;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetShort(ColumnIndex: Integer): SmallInt;
var
  DL: Integer;
  Data: Pointer;
  DT: Integer;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);

  DL := FPlainDriver.dbdatlen(FHandle, ColumnIndex);
  Data := FPlainDriver.dbdata(FHandle, ColumnIndex);
  DT := DBLibColTypeCache[ColumnIndex];
  LastWasNull := Data = nil;

  Result := 0;
  if Assigned(Data) then
  begin
    if DT = SQLINT2 then
      Result := PSmallInt(Data)^
    else
    begin
      FPlainDriver.dbconvert(FHandle, DT, Data, DL, SQLINT2,
        @Result, SizeOf(Result));
    end;
  end;
  FDBLibConnection.CheckDBLibError;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetInt(ColumnIndex: Integer): Integer;
var
  DL: Integer;
  Data: Pointer;
  DT: Integer;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);

  DL := FPlainDriver.dbdatlen(FHandle, ColumnIndex);
  Data := FPlainDriver.dbdata(FHandle, ColumnIndex);
  DT := DBLibColTypeCache[ColumnIndex];
  LastWasNull := Data = nil;

  Result := 0;
  if Assigned(Data) then
  begin
    if DT = SQLINT4 then
      Result := PLongint(Data)^
    else
    begin
      FPlainDriver.dbconvert(FHandle, DT, Data, DL, SQLINT4,
        @Result, SizeOf(Result));
    end;
  end;
  FDBLibConnection.CheckDBLibError;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetLong(ColumnIndex: Integer): LongInt;
begin
  Result := GetInt(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  DL: Integer;
  Data: Pointer;
  DT: Integer;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);

  DL := FPlainDriver.dbdatlen(FHandle, ColumnIndex);
  Data := FPlainDriver.dbdata(FHandle, ColumnIndex);
  DT := DBLibColTypeCache[ColumnIndex];
  LastWasNull := Data = nil;

  Result := 0;
  if Assigned(Data) then
  begin
    if DT = SQLFLT4 then
      Result := PSingle(Data)^
    else
    begin
      FPlainDriver.dbconvert(FHandle, DT, Data, DL, SQLFLT4,
        @Result, SizeOf(Result));
    end;
  end;
  FDBLibConnection.CheckDBLibError;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  DL: Integer;
  Data: Pointer;
  DT: Integer;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);

  DL := FPlainDriver.dbdatlen(FHandle, ColumnIndex);
  Data := FPlainDriver.dbdata(FHandle, ColumnIndex);
  DT := DBLibColTypeCache[ColumnIndex];
  LastWasNull := Data = nil;

  Result := 0;
  if Assigned(Data) then
  begin
    if DT = SQLFLT8 then
      Result := PDouble(Data)^
    else
    begin
      FPlainDriver.dbconvert(FHandle, DT, Data, DL, SQLFLT8,
        @Result, SizeOf(Result));
    end;
  end;
  FDBLibConnection.CheckDBLibError;
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
function TZDBLibResultSet.GetBigDecimal(ColumnIndex: Integer): Int64;
begin
  Result := GetInt(ColumnIndex);
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
function TZDBLibResultSet.GetBytes(ColumnIndex: Integer): TByteDynArray;
var
  DL: Integer;
  Data: Pointer;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);

  DL := FPlainDriver.dbdatlen(FHandle, ColumnIndex);
  Data := FPlainDriver.dbdata(FHandle, ColumnIndex);
  FDBLibConnection.CheckDBLibError;
  LastWasNull := Data = nil;

  SetLength(Result, DL);
  if Assigned(Data) then
    Move(PChar(Data)^, Result[0], DL);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
  Result := Int(GetTimestamp(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
  Result := Frac(GetTimestamp(ColumnIndex));
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
function TZDBLibResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  DL: Integer;
  Data: Pointer;
  DT: Integer;
  TempDate: DBDATETIME;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);

  DL := FPlainDriver.dbdatlen(FHandle, ColumnIndex);
  Data := FPlainDriver.dbdata(FHandle, ColumnIndex);
  DT := DBLibColTypeCache[ColumnIndex];
  LastWasNull := Data = nil;

  Result := 0;
  if Assigned(Data) then
  begin
    if DT = SQLDATETIME then
      Move(Data^, TempDate, SizeOf(TempDate))
    else
    begin
      FPlainDriver.dbconvert(FHandle, DT, Data, DL, SQLDATETIME,
        @TempDate, SizeOf(TempDate));
    end;
    Result := TempDate.dtdays + 2 + (TempDate.dttime / 25920000);
    //Perfect conversion no need to crack and reencode the date.
  end;
  FDBLibConnection.CheckDBLibError;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZDBLibResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  DL: Integer;
  Data: Pointer;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);
  CheckBlobColumn(ColumnIndex);

  DL := FPlainDriver.dbdatlen(FHandle, ColumnIndex);
  Data := FPlainDriver.dbdata(FHandle, ColumnIndex);
  LastWasNull := Data = nil;
  Result := TZAbstractBlob.CreateWithData(Data, DL);
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
function TZDBLibResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := False;
  RaiseUnsupportedException;
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
function TZDBLibResultSet.Next: Boolean;
begin
  Result := False;
  case FPlainDriver.dbnextrow(FHandle) of
    REG_ROW: Result := True;
    NO_MORE_ROWS: ;
    DBFAIL: FDBLibConnection.CheckDBLibError;
    BUF_FULL: ;//should not happen because we are not using dblibc buffering.
  else
   // If a compute row is read, the computeid of the row is returned
    Result := False;
  end;
end;

end.
