{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Caching Classes and Interfaces               }
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

unit ZDbcCachedResultSet;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, ZClasses, ZSysUtils, ZDbcIntfs, ZDbcResultSet, ZDbcCache,
  ZCompatibility;

type

  IZCachedResultSet = interface;

  {** Resolver to post updates. }
  IZCachedResolver = interface (IZInterface)
    ['{546ED716-BB88-468C-8CCE-D7111CF5E1EF}']

    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor);
  end;

  {** Represents a cached result set. }
  IZCachedResultSet = interface (IZResultSet)
    ['{BAF24A92-C8CE-4AB4-AEBC-3D4A9BCB0946}']

    function GetResolver: IZCachedResolver;
    procedure SetResolver(Resolver: IZCachedResolver);

    function IsCachedUpdates: Boolean;
    procedure SetCachedUpdates(Value: Boolean);
    function IsPendingUpdates: Boolean;

    procedure PostUpdates;
    procedure CancelUpdates;
    procedure RevertRecord;
    procedure MoveToInitialRow;
  end;

  {** Implements cached ResultSet. }
  TZAbstractCachedResultSet = class (TZAbstractResultSet, IZCachedResultSet)
  private
    FCachedUpdates: Boolean;
    FRowsList: TList;
    FInitialRowsList: TList;
    FCurrentRowsList: TList;
    FUpdatedRow: PZRowBuffer;
    FInsertedRow: PZRowBuffer;
    FRowAccessor: TZRowAccessor;
    FNewRowAccessor: TZRowAccessor;
    FOldRowAccessor: TZRowAccessor;
    FNextRowIndex: Integer;
    FResolver: IZCachedResolver;
  protected
    constructor CreateWithStatement(SQL: string; Statement: IZStatement);

    procedure CheckAvailable;
    procedure CheckUpdatable;
    procedure Open; override;
    function GetNextRowIndex: Integer;

    procedure PostRowUpdates(OldRowAccessor,
      NewRowAccessor: TZRowAccessor); virtual;
    function LocateRow(RowsList: TList; RowIndex: Integer): Integer;
    function AppendRow(Row: PZRowBuffer): PZRowBuffer;

    property CachedUpdates: Boolean read FCachedUpdates write FCachedUpdates;
    property RowsList: TList read FRowsList write FRowsList;
    property InitialRowsList: TList read FInitialRowsList
      write FInitialRowsList;
    property CurrentRowsList: TList read FCurrentRowsList
      write FCurrentRowsList;
    property UpdatedRow: PZRowBuffer read FUpdatedRow write FUpdatedRow;
    property InsertedRow: PZRowBuffer read FInsertedRow write FInsertedRow;
    property RowAccessor: TZRowAccessor read FRowAccessor write FRowAccessor;
    property OldRowAccessor: TZRowAccessor read FOldRowAccessor
      write FOldRowAccessor;
    property NewRowAccessor: TZRowAccessor read FNewRowAccessor
      write FNewRowAccessor;
    property NextRowIndex: Integer read FNextRowIndex write FNextRowIndex;
    property Resolver: IZCachedResolver read FResolver write FResolver;
  public
    constructor CreateWithColumns(ColumnsInfo: IZCollection; SQL: string);
    destructor Destroy; override;

    procedure Close; override;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPChar(ColumnIndex: Integer): PChar; override;
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

    //---------------------------------------------------------------------
    // Traversal/Positioning
    //---------------------------------------------------------------------

    function MoveAbsolute(Row: Integer): Boolean; override;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    function RowUpdated: Boolean; override;
    function RowInserted: Boolean; override;
    function RowDeleted: Boolean; override;

    procedure UpdateNull(ColumnIndex: Integer); override;
    procedure UpdateBoolean(ColumnIndex: Integer; Value: Boolean); override;
    procedure UpdateByte(ColumnIndex: Integer; Value: ShortInt); override;
    procedure UpdateShort(ColumnIndex: Integer; Value: SmallInt); override;
    procedure UpdateInt(ColumnIndex: Integer; Value: Integer); override;
    procedure UpdateLong(ColumnIndex: Integer; Value: LongInt); override;
    procedure UpdateFloat(ColumnIndex: Integer; Value: Single); override;
    procedure UpdateDouble(ColumnIndex: Integer; Value: Double); override;
    procedure UpdateBigDecimal(ColumnIndex: Integer; Value: Int64); override;
    procedure UpdatePChar(ColumnIndex: Integer; Value: PChar); override;
    procedure UpdateString(ColumnIndex: Integer; Value: string); override;
    procedure UpdateBytes(ColumnIndex: Integer; Value: TByteDynArray); override;
    procedure UpdateDate(ColumnIndex: Integer; Value: TDateTime); override;
    procedure UpdateTime(ColumnIndex: Integer; Value: TDateTime); override;
    procedure UpdateTimestamp(ColumnIndex: Integer; Value: TDateTime); override;
    procedure UpdateAsciiStream(ColumnIndex: Integer; Value: TStream); override;
    procedure UpdateUnicodeStream(ColumnIndex: Integer; Value: TStream); override;
    procedure UpdateBinaryStream(ColumnIndex: Integer; Value: TStream); override;

    procedure InsertRow; override;
    procedure UpdateRow; override;
    procedure DeleteRow; override;
    procedure CancelRowUpdates; override;
    procedure MoveToInsertRow; override;
    procedure MoveToCurrentRow; override;

    //---------------------------------------------------------------------
    // Cached Updates
    //---------------------------------------------------------------------

    function GetResolver: IZCachedResolver;
    procedure SetResolver(Resolver: IZCachedResolver);

    function IsCachedUpdates: Boolean;
    procedure SetCachedUpdates(Value: Boolean);
    function IsPendingUpdates: Boolean; virtual;

    procedure PostUpdates; virtual;
    procedure CancelUpdates; virtual;
    procedure RevertRecord; virtual;
    procedure MoveToInitialRow; virtual;
  end;

  {**
    Implements Abstract cached ResultSet. This class should be extended
    with database specific logic to form SQL data manipulation statements.
  }
  TZCachedResultSet = class(TZAbstractCachedResultSet)
  private
    FResultSet: IZResultSet;
  protected
    procedure Open; override;
    function Fetch: Boolean; virtual;
    procedure FetchAll; virtual;

    property ResultSet: IZResultSet read FResultSet write FResultSet;
  public
    constructor Create(ResultSet: IZResultSet; SQL: string;
      Resolver: IZCachedResolver);
    destructor Destroy; override;

    procedure Close; override;
    function GetMetaData: IZResultSetMetaData; override;

    function IsAfterLast: Boolean; override;
    function IsLast: Boolean; override;
    procedure AfterLast; override;
    function Last: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
  end;

implementation

uses ZDbcResultSetMetadata, ZDbcGenericResolver, ZDbcUtils;

{ TZAbstractCachedResultSet }

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query.
}
constructor TZAbstractCachedResultSet.CreateWithStatement( SQL: string;
   Statement: IZStatement);
begin
  inherited Create(Statement, SQL);
  FCachedUpdates := False;
end;

{**
  Creates this object and assignes the main properties.
  @param SQL an SQL query.
  @param ColumnsInfo a columns info for cached rows.
}
constructor TZAbstractCachedResultSet.CreateWithColumns(
  ColumnsInfo: IZCollection; SQL: string);
begin
  inherited Create(nil, SQL);
  if ColumnsInfo <> nil then
    inherited ColumnsInfo.AddAll(ColumnsInfo);
  FCachedUpdates := False;
  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractCachedResultSet.Destroy;
begin
  FResolver := nil;
  inherited Destroy;
end;

{**
  Checks for availability of the cached buffer.
}
procedure TZAbstractCachedResultSet.CheckAvailable;
begin
  CheckClosed;
  if (FRowAccessor = nil) or (FRowAccessor.RowBuffer = nil) then
    raise EZSQLException.Create('Row data is not available.');
end;

{**
  Checks is the cached buffer updatable.
}
procedure TZAbstractCachedResultSet.CheckUpdatable;
begin
  CheckAvailable;
  if ResultSetConcurrency <> rcUpdatable then
    RaiseReadOnlyException;
end;

{**
  Generates the next row index value.
  @return the new generated row index.
}
function TZAbstractCachedResultSet.GetNextRowIndex: Integer;
begin
  Result := FNextRowIndex;
  Inc(FNextRowIndex);
end;

{**
  Finds a row with specified index among list of rows.
  @param RowsList a list of rows.
  @param Index a row index.
  @return a found row buffer of <code>null</code> otherwise.
}
function TZAbstractCachedResultSet.LocateRow(RowsList: TList;
  RowIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to RowsList.Count - 1 do
  begin
    if PZRowBuffer(RowsList[I]).Index = RowIndex then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{**
  Appends a row to the list of rows if such row is not exist.
  @param Row a row buffer.
  @return an appended row buffer.
}
function TZAbstractCachedResultSet.AppendRow(Row: PZRowBuffer): PZRowBuffer;
begin
  if LocateRow(FInitialRowsList, Row.Index) < 0 then
  begin
    FRowAccessor.AllocBuffer(Result);
    FRowAccessor.CopyBuffer(Row, Result);
    FInitialRowsList.Add(Result);
    FCurrentRowsList.Add(Row);
  end else
    Result := nil;
end;

{**
  Post changes to database server.
  @param OldRowAccessor a row accessor which contains old column values.
  @param NewRowAccessor a row accessor which contains new or updated
    column values.
}
procedure TZAbstractCachedResultSet.PostRowUpdates(OldRowAccessor,
  NewRowAccessor: TZRowAccessor);
begin
{$IFNDEF DISABLE_CHECKING}
  if Resolver = nil then
    raise EZSQLException.Create('Resolver is not specified for the result set.');
{$ENDIF}
  Resolver.PostUpdates(Self, NewRowAccessor.RowBuffer.UpdateType,
    OldRowAccessor, NewRowAccessor);
end;

{**
  Gets a cached updates resolver object.
  @return a cached updates resolver object.
}
function TZAbstractCachedResultSet.GetResolver: IZCachedResolver;
begin
  Result := FResolver;
end;

{**
  Sets a new cached updates resolver object.
  @param Resolver a cached updates resolver object.
}
procedure TZAbstractCachedResultSet.SetResolver(Resolver: IZCachedResolver);
begin
  FResolver := Resolver;
end;

{**
  Checks is the cached updates mode turned on.
  @return <code>True</code> if the cached updates mode turned on.
}
function TZAbstractCachedResultSet.IsCachedUpdates: Boolean;
begin
  Result := FCachedUpdates;
end;

{**
  Switched the cached updates mode.
  @param Value boolean flag which turns on/off the cached updates mode.
}
procedure TZAbstractCachedResultSet.SetCachedUpdates(Value: Boolean);
begin
  if FCachedUpdates <> Value then
  begin
    FCachedUpdates := Value;
    if not FCachedUpdates then
      PostUpdates;
  end;
end;

{**
  Checks is cached updates pending.
  @return <code>True</code> if the cached updates pending.
}
function TZAbstractCachedResultSet.IsPendingUpdates: Boolean;
begin
  Result := FInitialRowsList.Count > 0;
end;

{**
  Moves to the current row with initial column values.
}
procedure TZAbstractCachedResultSet.MoveToInitialRow;
var
  Index: Integer;
begin
  CheckClosed;
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  begin
    Index := LocateRow(FInitialRowsList, FUpdatedRow.Index);
    if Index >= 0 then
      FRowAccessor.RowBuffer := FInitialRowsList[Index]
    else FRowAccessor.RowBuffer := FUpdatedRow;
  end else
    FRowAccessor.RowBuffer := nil;
end;

{**
  Posts all saved updates to the server.
}
procedure TZAbstractCachedResultSet.PostUpdates;
begin
  CheckClosed;
  if FInitialRowsList.Count > 0 then
  begin
    while FInitialRowsList.Count > 0 do
    begin
      OldRowAccessor.RowBuffer := PZRowBuffer(FInitialRowsList[0]);
      NewRowAccessor.RowBuffer := PZRowBuffer(FCurrentRowsList[0]);

      { Posts row updates and processes the exceptions. }
      PostRowUpdates(OldRowAccessor, NewRowAccessor);

      { If post was Ok - update the row update type. }
      if NewRowAccessor.RowBuffer.UpdateType <> utDeleted then
      begin
        NewRowAccessor.RowBuffer.UpdateType := utUnmodified;
        if FUpdatedRow.Index = NewRowAccessor.RowBuffer.Index then
          FUpdatedRow.UpdateType := utUnmodified;
      end;

      { Removes cached rows. }
      OldRowAccessor.Dispose;
      FInitialRowsList.Delete(0);
      FCurrentRowsList.Delete(0);
    end;
  end;
end;

{**
  Cancels updates for all rows.
}
procedure TZAbstractCachedResultSet.CancelUpdates;
var
  InitialRow, CurrentRow: PZRowBuffer;
begin
  CheckClosed;
  while FInitialRowsList.Count > 0 do
  begin
    InitialRow := PZRowBuffer(FInitialRowsList[0]);
    CurrentRow := PZRowBuffer(FCurrentRowsList[0]);

    if CurrentRow.UpdateType = utInserted then
      InitialRow.UpdateType := utDeleted;

    FRowAccessor.CopyBuffer(InitialRow, CurrentRow);
    if (FUpdatedRow <> nil) and (FUpdatedRow.Index = InitialRow.Index) then
      FRowAccessor.CopyBuffer(InitialRow, FUpdatedRow);

    FRowAccessor.DisposeBuffer(InitialRow);
    FInitialRowsList.Delete(0);
    FCurrentRowsList.Delete(0);
  end;
end;

{**
  Cancels updates for the current row.
}
procedure TZAbstractCachedResultSet.RevertRecord;
var
  Index: Integer;
  InitialRow, CurrentRow: PZRowBuffer;
begin
  CheckClosed;
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  begin
    Index := LocateRow(FInitialRowsList, FUpdatedRow.Index);
    if Index >= 0 then
    begin
      InitialRow := PZRowBuffer(FInitialRowsList[Index]);
      CurrentRow := PZRowBuffer(FRowsList[RowNo - 1]);

      if CurrentRow.UpdateType = utInserted then
        InitialRow.UpdateType := utDeleted;
      FRowAccessor.CopyBuffer(InitialRow, CurrentRow);
      FRowAccessor.CopyBuffer(InitialRow, FUpdatedRow);

      FRowAccessor.DisposeBuffer(InitialRow);
      FInitialRowsList.Delete(Index);
      FCurrentRowsList.Delete(Index);
    end;
  end;
end;

{**
  Opens this recordset.
}
procedure TZAbstractCachedResultSet.Open;
begin
  if not Closed then
    raise EZSQLException.Create('Resultset is already opened.');

  FRowsList := TList.Create;
  FInitialRowsList := TList.Create;
  FCurrentRowsList := TList.Create;

  FRowAccessor := TZRowAccessor.Create(ColumnsInfo);
  FOldRowAccessor := TZRowAccessor.Create(ColumnsInfo);
  FNewRowAccessor := TZRowAccessor.Create(ColumnsInfo);

  FRowAccessor.AllocBuffer(FUpdatedRow);
  FRowAccessor.AllocBuffer(FInsertedRow);

  FNextRowIndex := 0;

  if Self.GetConcurrency = rcUpdatable then
    Resolver := TZGenericCachedResolver.Create(GetStatement, GetMetadata);

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
procedure TZAbstractCachedResultSet.Close;
var
  I: Integer;
begin
  inherited Close;

  if Assigned(FRowAccessor) then
  begin
    for I := 0 to FRowsList.Count - 1 do
      FRowAccessor.DisposeBuffer(PZRowBuffer(FRowsList[I]));
    for I := 0 to FInitialRowsList.Count - 1 do
      FRowAccessor.DisposeBuffer(PZRowBuffer(FInitialRowsList[I]));

    FRowAccessor.DisposeBuffer(FUpdatedRow);
    FUpdatedRow := nil;
    FRowAccessor.DisposeBuffer(FInsertedRow);
    FInsertedRow := nil;

    FRowsList.Free;
    FRowsList := nil;
    FInitialRowsList.Free;
    FInitialRowsList := nil;
    FCurrentRowsList.Free;
    FCurrentRowsList := nil;

    FRowAccessor.Free;
    FRowAccessor := nil;
    FOldRowAccessor.Free;
    FOldRowAccessor := nil;
    FNewRowAccessor.Free;
    FNewRowAccessor := nil;
  end;
end;

//======================================================================
// Methods for accessing results by column index
//======================================================================

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractCachedResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetPChar(ColumnIndex: Integer): PChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetPChar(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetString(ColumnIndex: Integer): string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetString(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAbstractCachedResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetBoolean(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetByte(ColumnIndex: Integer): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetByte(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetShort(ColumnIndex: Integer): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetShort(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetInt(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetLong(ColumnIndex: Integer): LongInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetLong(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetFloat(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetDouble(ColumnIndex, LastWasNull);
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
function TZAbstractCachedResultSet.GetBigDecimal(ColumnIndex: Integer): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetBigDecimal(ColumnIndex, LastWasNull);
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
function TZAbstractCachedResultSet.GetBytes(ColumnIndex: Integer): TByteDynArray;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetBytes(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetDate(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetTime(ColumnIndex, LastWasNull);
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
function TZAbstractCachedResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetTimestamp(ColumnIndex, LastWasNull);
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAbstractCachedResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetBlob(ColumnIndex, LastWasNull);
end;

//---------------------------------------------------------------------
// Updates
//---------------------------------------------------------------------

{**
  Gives a nullable column a null value.

  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code>
  or <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZAbstractCachedResultSet.UpdateNull(ColumnIndex: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetNull(ColumnIndex);
end;

{**
  Updates the designated column with a <code>boolean</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateBoolean(ColumnIndex: Integer;
  Value: Boolean);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetBoolean(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>byte</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateByte(ColumnIndex: Integer;
  Value: ShortInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetByte(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>short</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateShort(ColumnIndex: Integer;
  Value: SmallInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetShort(ColumnIndex, Value);
end;

{**
  Updates the designated column with an <code>int</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateInt(ColumnIndex, Value: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetInt(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>long</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateLong(ColumnIndex,
  Value: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetLong(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>float</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateFloat(ColumnIndex: Integer;
  Value: Single);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetFloat(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>double</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateDouble(ColumnIndex: Integer;
  Value: Double);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetDouble(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>java.math.BigDecimal</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateBigDecimal(ColumnIndex: Integer;
  Value: Int64);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetBigDecimal(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdatePChar(ColumnIndex: Integer;
  Value: PChar);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetPChar(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateString(ColumnIndex: Integer;
  Value: string);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetString(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>byte</code> array value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateBytes(ColumnIndex: Integer;
  Value: TByteDynArray);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetBytes(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>java.sql.Date</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateDate(ColumnIndex: Integer;
  Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetDate(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>java.sql.Time</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateTime(ColumnIndex: Integer;
  Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetTime(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateTimestamp(ColumnIndex: Integer;
  Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetTimestamp(ColumnIndex, Value);
end;

{**
  Updates the designated column with an ascii stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateAsciiStream(ColumnIndex: Integer;
  Value: TStream);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetAsciiStream(ColumnIndex, Value);
end;

{**
  Updates the designated column with a binary stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
  @param length the length of the stream
}
procedure TZAbstractCachedResultSet.UpdateBinaryStream(
  ColumnIndex: Integer; Value: TStream);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetBinaryStream(ColumnIndex, Value);
end;

{**
  Updates the designated column with a character stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateUnicodeStream(
  ColumnIndex: Integer; Value: TStream);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  FRowAccessor.SetUnicodeStream(ColumnIndex, Value);
end;

//---------------------------------------------------------------------
// Processing methods
//---------------------------------------------------------------------

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
function TZAbstractCachedResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (ResultSetType = rtForwardOnly) and (Row < RowNo) then
    RaiseForwardOnlyException;
{$ENDIF}

  if (Row >= 0) and (Row <= LastRowNo + 1) then
  begin
    RowNo := Row;
    if (Row >= 1) and (Row <= LastRowNo) then
    begin
      Result := True;
      RowAccessor.RowBuffer := FUpdatedRow;
      RowAccessor.CopyFrom(PZRowBuffer(FRowsList[Row - 1]));
    end
    else
    begin
      Result := False;
      RowAccessor.ClearBuffer(FUpdatedRow);
      RowAccessor.RowBuffer := nil;
    end;
  end else
    Result := False;
end;

{**
  Indicates whether the current row has been updated.  The value returned
  depends on whether or not the result set can detect updates.

  @return <code>true</code> if the row has been visibly updated
    by the owner or another, and updates are detected
}
function TZAbstractCachedResultSet.RowUpdated: Boolean;
var
  CurrentRow: PZRowBuffer;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  begin
    CurrentRow := PZRowBuffer(FRowsList[RowNo - 1]);
    Result := CurrentRow^.UpdateType = utModified;
  end else
    Result := False;
end;

{**
  Indicates whether the current row has had an insertion.
  The value returned depends on whether or not this
  <code>ResultSet</code> object can detect visible inserts.

  @return <code>true</code> if a row has had an insertion
    and insertions are detected; <code>false</code> otherwise
}
function TZAbstractCachedResultSet.RowInserted: Boolean;
var
  CurrentRow: PZRowBuffer;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  begin
    CurrentRow := PZRowBuffer(FRowsList[RowNo - 1]);
    Result := CurrentRow^.UpdateType = utInserted;
  end else
    Result := False;
end;

{**
  Indicates whether a row has been deleted.  A deleted row may leave
  a visible "hole" in a result set.  This method can be used to
  detect holes in a result set.  The value returned depends on whether
  or not this <code>ResultSet</code> object can detect deletions.

  @return <code>true</code> if a row was deleted and deletions are detected;
    <code>false</code> otherwise
}
function TZAbstractCachedResultSet.RowDeleted: Boolean;
var
  UpdateType: TZRowUpdateType;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  begin
    UpdateType := PZRowBuffer(FRowsList[RowNo - 1])^.UpdateType;
    Result := UpdateType = utDeleted;
  end else
    Result := False;
end;

{**
  Inserts the contents of the insert row into this
  <code>ResultSet</code> object and into the database.
  The cursor must be on the insert row when this method is called.
}
procedure TZAbstractCachedResultSet.InsertRow;
var
  TempRowBuffer: PZRowBuffer;
begin
  CheckClosed;

  { Creates a new row. }
  TempRowBuffer := FRowAccessor.RowBuffer;
  FRowAccessor.Alloc;
  FRowAccessor.MoveFrom(FInsertedRow);
  FRowAccessor.RowBuffer^.UpdateType := utInserted;
  FRowAccessor.RowBuffer^.Index := GetNextRowIndex;

  AppendRow(FRowAccessor.RowBuffer);

  { Posts non-cached updates. }
  if not FCachedUpdates then
  begin
    try
      PostUpdates;
    except
      on E: Exception do
      begin
        { Restore the previous state. }
        FRowAccessor.DisposeBuffer(FInitialRowsList[FInitialRowsList.Count - 1]);
        FInitialRowsList.Delete(FInitialRowsList.Count - 1);
        FRowAccessor.DisposeBuffer(FCurrentRowsList[FCurrentRowsList.Count - 1]);
        FCurrentRowsList.Delete(FCurrentRowsList.Count - 1);
        FRowAccessor.RowBuffer := TempRowBuffer;

        { Reraises the exception. }
        RaiseSQLException(E);
      end;
    end;
  end;

  FRowsList.Add(FRowAccessor.RowBuffer);
  LastRowNo := FRowsList.Count;
  MoveAbsolute(LastRowNo);
end;

{**
  Updates the underlying database with the new contents of the
  current row of this <code>ResultSet</code> object.
  This method cannot be called when the cursor is on the insert row.
}
procedure TZAbstractCachedResultSet.UpdateRow;
begin
  CheckUpdatable;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create('Can not update empty row.');

  if PZRowBuffer(FRowsList[RowNo - 1]).UpdateType = utDeleted then
    raise EZSQLException.Create('Can not update deleted row.');

  AppendRow(FRowsList[RowNo - 1]);

  FRowAccessor.CopyBuffer(FUpdatedRow, FRowsList[RowNo - 1]);
  if PZRowBuffer(FRowsList[RowNo - 1]).UpdateType = utUnmodified then
    PZRowBuffer(FRowsList[RowNo - 1]).UpdateType := utModified;

  { Posts non-cached updates. }
  if not FCachedUpdates then
  begin
    try
      PostUpdates;
    except
      on E: Exception do
      begin
        { Restore the previous state. }
        FRowAccessor.DisposeBuffer(FRowsList[RowNo - 1]);
        FRowsList[RowNo - 1] := FInitialRowsList[FInitialRowsList.Count - 1];
        FInitialRowsList.Delete(FInitialRowsList.Count - 1);
        FCurrentRowsList.Delete(FCurrentRowsList.Count - 1);

        { Reraises the exception. }
        RaiseSQLException(E);
      end;
    end;
  end;
end;

{**
  Deletes the current row from this <code>ResultSet</code> object
  and from the underlying database.  This method cannot be called when
  the cursor is on the insert row.
}
procedure TZAbstractCachedResultSet.DeleteRow;
begin
  CheckUpdatable;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create('Can not delete empty row.');

  if FUpdatedRow^.UpdateType = utInserted then
    RevertRecord
  else begin
    AppendRow(FRowsList[RowNo - 1]);

    FUpdatedRow^.UpdateType := utDeleted;
    FRowAccessor.CopyBuffer(FUpdatedRow, FRowsList[RowNo - 1]);

    { Posts non-cached updates. }
    if not FCachedUpdates then
    begin
      try
        PostUpdates;
      except
        on E: Exception do
        begin
          { Restore the previous state. }
          FRowAccessor.DisposeBuffer(FRowsList[RowNo - 1]);
          FRowsList[RowNo - 1] := FInitialRowsList[FInitialRowsList.Count - 1];
          FUpdatedRow^.UpdateType :=
            PZRowBuffer(FRowsList[RowNo - 1])^.UpdateType;
          FInitialRowsList.Delete(FInitialRowsList.Count - 1);
          FCurrentRowsList.Delete(FCurrentRowsList.Count - 1);

          { Reraises the exception. }
          RaiseSQLException(E);
        end;
      end;
    end;
  end;
end;

{**
  Cancels the updates made to the current row in this
  <code>ResultSet</code> object.
  This method may be called after calling an
  <code>updateXXX</code> method(s) and before calling
  the method <code>updateRow</code> to roll back
  the updates made to a row.  If no updates have been made or
  <code>updateRow</code> has already been called, this method has no
  effect.
}
procedure TZAbstractCachedResultSet.CancelRowUpdates;
begin
  MoveAbsolute(RowNo);
end;

{**
  Moves the cursor to the insert row.  The current cursor position is
  remembered while the cursor is positioned on the insert row.

  The insert row is a special row associated with an updatable
  result set.  It is essentially a buffer where a new row may
  be constructed by calling the <code>updateXXX</code> methods prior to
  inserting the row into the result set.

  Only the <code>updateXXX</code>, <code>getXXX</code>,
  and <code>insertRow</code> methods may be
  called when the cursor is on the insert row.  All of the columns in
  a result set must be given a value each time this method is
  called before calling <code>insertRow</code>.
  An <code>updateXXX</code> method must be called before a
  <code>getXXX</code> method can be called on a column value.
}
procedure TZAbstractCachedResultSet.MoveToInsertRow;
begin
  CheckClosed;
  FRowAccessor.RowBuffer := FInsertedRow;
end;

{**
  Moves the cursor to the remembered cursor position, usually the
  current row.  This method has no effect if the cursor is not on
  the insert row.
}
procedure TZAbstractCachedResultSet.MoveToCurrentRow;
begin
  CheckClosed;
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
    FRowAccessor.RowBuffer := FUpdatedRow
  else FRowAccessor.RowBuffer := nil;
end;


{ TZCachedResultSet }

{**
  Creates this object and assignes the main properties.
  @param ResultSet a wrapped resultset object.
  @param Resolver a cached updates resolver object.
}
constructor TZCachedResultSet.Create(ResultSet: IZResultSet; SQL: string;
  Resolver: IZCachedResolver);
begin
  inherited Create(ResultSet.GetStatement, SQL);
  FResultSet := ResultSet;
  FResolver := Resolver;
  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZCachedResultSet.Destroy;
begin
  inherited Destroy;
end;

{**
  Fetches one row from the wrapped result set object.
  @return <code>True</code> if row was successfuly fetched
    or <code>False</code> otherwise.
}
function TZCachedResultSet.Fetch: Boolean;
var
  I: Integer;
  Current: IZColumnInfo;
  TempRowBuffer: PZRowBuffer;
begin
  Result := FResultSet.Next;
  if not Result or ((MaxRows > 0) and (LastRowNo >= MaxRows)) then
    Exit;

  TempRowBuffer := RowAccessor.RowBuffer;
  try
    RowAccessor.Alloc;
    RowAccessor.RowBuffer.Index := GetNextRowIndex;
    RowAccessor.RowBuffer.UpdateType := utUnmodified;

    for I := 1 to ColumnsInfo.Count do
    begin
      Current := ColumnsInfo[I - 1] as IZColumnInfo;
      case Current.GetColumnType of
        stBoolean: RowAccessor.SetBoolean(I, ResultSet.GetBoolean(I));
        stByte: RowAccessor.SetByte(I, ResultSet.GetByte(I));
        stShort: RowAccessor.SetShort(I, ResultSet.GetShort(I));
        stInteger: RowAccessor.SetInt(I, ResultSet.GetInt(I));
        stLong: RowAccessor.SetLong(I, ResultSet.GetLong(I));
        stFloat: RowAccessor.SetFloat(I, ResultSet.GetFloat(I));
        stDouble: RowAccessor.SetDouble(I, ResultSet.GetDouble(I));
        stBigDecimal: RowAccessor.SetBigDecimal(I, ResultSet.GetBigDecimal(I));
        stString: RowAccessor.SetPChar(I, ResultSet.GetPChar(I));
        stBytes: RowAccessor.SetBytes(I, ResultSet.GetBytes(I));
        stDate: RowAccessor.SetDate(I, ResultSet.GetDate(I));
        stTime: RowAccessor.SetTime(I, ResultSet.GetTime(I));
        stTimestamp: RowAccessor.SetTimestamp(I, ResultSet.GetTimestamp(I));
        stAsciiStream, stUnicodeStream, stBinaryStream:
          RowAccessor.SetBlob(I, ResultSet.GetBlob(I));
      end;
      if ResultSet.WasNull then
        RowAccessor.SetNull(I);
    end;

    RowsList.Add(RowAccessor.RowBuffer);
    LastRowNo := RowsList.Count;
  finally
    RowAccessor.RowBuffer := TempRowBuffer;
  end;
end;

{**
  Fetches all of the rest rows from the wrapped result set.
}
procedure TZCachedResultSet.FetchAll;
begin
  while Fetch do;
end;

{**
  Opens this recordset.
}
procedure TZCachedResultSet.Open;
var
  I: Integer;
  ColumnInfo: IZColumnInfo;
begin
  ColumnsInfo.Clear;
  for I := 1 to FResultSet.GetMetadata.GetColumnCount do
  begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo do
    begin
      SetCurrency(FResultSet.GetMetadata.IsCurrency(I));
      SetSigned(FResultSet.GetMetadata.IsSigned(I));
      SetColumnDisplaySize(FResultSet.GetMetadata.GetColumnDisplaySize(I));
      SetColumnLabel(FResultSet.GetMetadata.GetColumnLabel(I));
      SetPrecision(FResultSet.GetMetadata.GetPrecision(I));
      SetScale(FResultSet.GetMetadata.GetScale(I));
      SetColumnType(FResultSet.GetMetadata.GetColumnType(I));
    end;
    ColumnsInfo.Add(ColumnInfo);
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
procedure TZCachedResultSet.Close;
begin
  inherited Close;
  ColumnsInfo.Clear;
end;

{**
  Retrieves the  number, types and properties of
  this <code>ResultSet</code> object's columns.
  @return the description of this <code>ResultSet</code> object's columns
}
function TZCachedResultSet.GetMetaData: IZResultSetMetaData;
begin
  Result := ResultSet.GetMetadata;
end;

{**
  Indicates whether the cursor is after the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is after the last row;
    <code>false</code> if the cursor is at any other position or the
    result set contains no rows
}
function TZCachedResultSet.IsAfterLast: Boolean;
begin
  FetchAll;
  Result := inherited IsAfterLast;
end;

{**
  Moves the cursor to the end of
  this <code>ResultSet</code> object, just after the
  last row. This method has no effect if the result set contains no rows.
}
procedure TZCachedResultSet.AfterLast;
begin
  FetchAll;
  inherited AfterLast;
end;

{**
  Indicates whether the cursor is on the last row of
  this <code>ResultSet</code> object.
  Note: Calling the method <code>isLast</code> may be expensive
  because the JDBC driver
  might need to fetch ahead one row in order to determine
  whether the current row is the last row in the result set.

  @return <code>true</code> if the cursor is on the last row;
    <code>false</code> otherwise
}
function TZCachedResultSet.IsLast: Boolean;
begin
  FetchAll;
  Result := inherited IsLast;
end;

{**
  Moves the cursor to the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if there are no rows in the result set
}
function TZCachedResultSet.Last: Boolean;
begin
  FetchAll;
  Result := inherited Last;
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
function TZCachedResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (Row > MaxRows) then
    Exit;

  { Processes negative rows }
  if Row < 0 then
  begin
    FetchAll;
    Row := LastRowNo - Row + 1;
    if Row < 0 then Row := 0;
  end else
  { Processes moving after last row }
    while (LastRowNo < Row) and Fetch do;

  Result := inherited MoveAbsolute(Row);
end;

end.

