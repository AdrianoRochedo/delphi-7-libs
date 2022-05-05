{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Abstract Read/Only Dataset component           }
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

unit ZAbstractRODataset;

interface

{$I ZComponent.inc}

uses
  {$IFNDEF LINUX}
  Windows,
  {$ENDIF}
  SysUtils, DB, Classes, ZSysUtils, ZConnection, ZDbcIntfs, ZSqlStrings,
  ZDbcCache, ZDbcCachedResultSet, ZCompatibility;

type

  {** Record update type. }
  TZUpdateRecordType = TZRowUpdateType;

  {** Set of known record update types. }
  TZUpdateRecordTypes = set of TZUpdateRecordType;

  {** Abstract dataset component optimized for read/only access. }
  TZAbstractRODataset = class(TDataSet)
  private
{$IFDEF VER130BELOW}
    FUniDirectional: Boolean;
{$ENDIF}
    FCurrentRow: Integer;
    FRowAccessor: TZRowAccessor;
    FOldRowBuffer: PZRowBuffer;
    FNewRowBuffer: PZRowBuffer;
    FCurrentRows: TList;
    FFetchCount: Integer;
    FFilterEnabled: Boolean;

    FRequestLive: Boolean;
    FSQL: TZSQLStrings;
    FParamCheck: Boolean;
    FParams: TParams;
    FShowRecordTypes: TZUpdateRecordTypes;

    FConnection: TZConnection;
    FStatement: IZPreparedStatement;
    FResultSet: IZResultSet;

    FRefreshInProgress: Boolean;

  private
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    function GetSQL: TStrings;
    procedure SetSQL(Value: TStrings);
    function GetParamCheck: Boolean;
    procedure SetParamCheck(Value: Boolean);
    procedure SetParams(Value: TParams);
    procedure SetShowRecordTypes(Value: TZUpdateRecordTypes);
    procedure SetConnection(Value: TZConnection);

    procedure UpdateSQLStrings(Sender: TObject);
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);

  protected
    procedure CheckConnected;
    procedure CheckBiDirectional;
    procedure CheckSQLQuery;
    procedure RaiseReadOnlyError;

    function FetchOneRow: Boolean;
    function FetchRows(RowCount: Integer): Boolean;
    function FilterRow(RowNo: Integer): Boolean;
    procedure RereadRows;

  protected
    { Internal protected properties. }
    property RowAccessor: TZRowAccessor read FRowAccessor write FRowAccessor;
    property CurrentRow: Integer read FCurrentRow write FCurrentRow;
    property OldRowBuffer: PZRowBuffer read FOldRowBuffer write FOldRowBuffer;
    property NewRowBuffer: PZRowBuffer read FNewRowBuffer write FNewRowBuffer;
    property CurrentRows: TList read FCurrentRows write FCurrentRows;
    property FetchCount: Integer read FFetchCount write FFetchCount;
    property FilterEnabled: Boolean read FFilterEnabled write FFilterEnabled;

    property Statement: IZPreparedStatement read FStatement write FStatement;
    property ResultSet: IZResultSet read FResultSet write FResultSet;

    { External protected properties. }
    property RequestLive: Boolean read FRequestLive write FRequestLive;
    property SQL: TStrings read GetSQL write SetSQL;
    property ParamCheck: Boolean read GetParamCheck write SetParamCheck;
    property Params: TParams read FParams write SetParams;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property ShowRecordTypes: TZUpdateRecordTypes read FShowRecordTypes
      write SetShowRecordTypes;
{$IFDEF VER130BELOW}
    property IsUniDirectional: Boolean read FUniDirectional
      write FUnidirectional;
{$ENDIF}

  protected
    { Abstracts methods }
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;

    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure DefineProperties(Filer: TFiler); override;

    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean):
      TGetResult; override;
    function GetRecordSize: Word; override;
    function GetActiveBuffer(var RowBuffer: PZRowBuffer): Boolean;
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure CloseBlob(Field: TField); override;
    function CreateStatement(SQL: string): IZPreparedStatement; virtual;

    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalRefresh; override;
    procedure InternalHandleException; override;
    procedure InternalSetToRecord(Buffer: PChar); override;

    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;

    function InternalLocate(KeyFields: string; KeyValues: Variant;
      Options: TLocateOptions): LongInt;
    function FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure SetFiltered(Value: Boolean); override;

    function GetCanModify: Boolean; override;
    function GetRecNo: Integer; override;
    function GetRecordCount: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function IsCursorOpen: Boolean; override;

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ExecSQL; virtual;
    function RowsAffected: LongInt;
    function ParamByName(const Value: string): TParam;

    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    function IsSequenced: Boolean; override;

    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
      override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;

    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
      override;
    function UpdateStatus: TUpdateStatus; override;

  public
    property Active;
    property FieldDefs stored False;
    property DbcStatement: IZPreparedStatement read FStatement;
    property DbcResultSet: IZResultSet read FResultSet;

  published
    property Connection: TZConnection read FConnection write SetConnection;

    property AutoCalcFields;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
{$IFNDEF VER125BELOW}
    property BeforeRefresh;
    property AfterRefresh;
{$ENDIF}
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnFilterRecord;
//    property Filter;
    property Filtered;
//    property FilterOptions;
  end;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  Math, ZDatasetUtils, ZStreamBlob, ZGenericSqlToken;

{ TZAbstractRODataset }

{**
  Constructs this object and assignes the mail properties.
  @param AOwner a component owner.
}
constructor TZAbstractRODataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSQL := TZSQLStrings.Create(TZGenericSQLTokenizer.Create);
  FSQL.OnChange := UpdateSQLStrings;
  FParamCheck := True;
  FParams := TParams.Create(Self);
  FCurrentRows := TList.Create;
  BookmarkSize := SizeOf(Integer);
  FShowRecordTypes := [utModified, utInserted, utUnmodified];
  FRequestLive := False;
  FFilterEnabled := False;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractRODataset.Destroy;
begin
  if Connection <> nil then
  begin
    try
      SetConnection(nil);
    except
    end;
  end;

  FSQL.Free;
  FParams.Free;
  FCurrentRows.Free;

  inherited Destroy;
end;

{**
  Sets database connection object.
  @param Value a database connection object.
}
procedure TZAbstractRODataset.SetConnection(Value: TZConnection);
begin
  if FConnection <> Value then
  begin
    if Active then Close;
    if FConnection <> nil then
      FConnection.UnregisterDataSet(Self);
    FConnection := Value;
    if FConnection <> nil then
      FConnection.RegisterDataSet(Self);
  end;
end;

{**
  Gets the SQL query.
  @return the SQL query strings.
}
function TZAbstractRODataset.GetSQL: TStrings;
begin
  Result := FSQL;
end;

{**
  Sets a new SQL query.
  @param Value a new SQL query.
}
procedure TZAbstractRODataset.SetSQL(Value: TStrings);
begin
  FSQL.Assign(Value);
end;

{**
  Gets a parameters check value.
  @return a parameters check value.
}
function TZAbstractRODataset.GetParamCheck: Boolean;
begin
  Result := FSQL.ParamCheck;
end;

{**
  Sets a new parameters check value.
  @param Value a parameters check value.
}
procedure TZAbstractRODataset.SetParamCheck(Value: Boolean);
begin
  FSQL.ParamCheck := Value;
  UpdateSQLStrings(Self);
end;

{**
  Sets a new set of parameters.
  @param Value a set of parameters.
}
procedure TZAbstractRODataset.SetParams(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

{**
  Defines a persistent dataset properties.
  @param Filer a persistent manager object.
}
procedure TZAbstractRODataset.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TZAbstractRODataset(Filer.Ancestor).FParams)
    else Result := FParams.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

{**
  Reads parameter data from persistent storage.
  @param Reader an input data stream.
}
procedure TZAbstractRODataset.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

{**
  Writes parameter data from persistent storage.
  @param Writer an output data stream.
}
procedure TZAbstractRODataset.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

{**
  Gets a SQL parameter by its name.
  @param Value a parameter name.
  @return a found parameter object.
}
function TZAbstractRODataset.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

{**
  Updates parameters from SQL statement.
  @param Sender an event sender object.
}
procedure TZAbstractRODataset.UpdateSQLStrings(Sender: TObject);
var
  I: Integer;
  OldParams: TParams;
begin
  FieldDefs.Clear;
  if Active then Close
  else Statement := nil;
  OldParams := TParams.Create;
  OldParams.Assign(FParams);
  FParams.Clear;
  try
    for I := 0 to FSQL.ParamCount - 1 do
    begin
      FParams.CreateParam(ftUnknown, FSQL.ParamNames[I], ptUnknown);
    end;
    FParams.AssignValues(OldParams);
  finally
    OldParams.Free;
  end;
end;

{**
  Gets the ReadOnly property.
  @return <code>True</code> if the opened result set read only.
}
function TZAbstractRODataset.GetReadOnly: Boolean;
begin
  Result := not RequestLive;
end;

{**
  Sets a new ReadOnly property.
  @param Value <code>True</code> to set result set read-only.
}
procedure TZAbstractRODataset.SetReadOnly(Value: Boolean);
begin
  RequestLive := not Value;
end;

{**
  Sets a new visible updated records types.
  @param Value a new visible UpdateRecordTypes value.
}
procedure TZAbstractRODataset.SetShowRecordTypes(Value: TZUpdateRecordTypes);
begin
  FShowRecordTypes := Value;

  if not (State in [dsInactive])
    and not IsUniDirectional then
    RereadRows;
end;

{**
  Checks is the database connection assignes and tries to connect.
}
procedure TZAbstractRODataset.CheckConnected;
begin
  if Connection = nil then
    DatabaseError('Database connection component is not assigned.');
  Connection.Connect;
end;

{**
  Checks is the database has bidirectional access.
}
procedure TZAbstractRODataset.CheckBiDirectional;
begin
  if IsUniDirectional then
    DatabaseError('Database connection component is not assigned.');
end;

{**
  Checks the correct SQL query.
}
procedure TZAbstractRODataset.CheckSQLQuery;
begin
  if FSQL.StatementCount < 1 then
    DatabaseError('SQL Query is empty.');
  if FSQL.StatementCount > 1 then
    DatabaseError('Cannot execute more then one query.');
end;

{**
  Raises an error 'Operation is not allowed in read-only dataset.
}
procedure TZAbstractRODataset.RaiseReadOnlyError;
begin
  DatabaseError('Operation is not allowed in ReadOnly dataset.');
end;

{**
  Fetches specified number of records.
  @param RowCount a specified number of rows to be fetched.
  @return <code>True</code> if all required rows were fetched.
}
function TZAbstractRODataset.FetchRows(RowCount: Integer): Boolean;
begin
  if RowCount = 0 then
  begin
    while FetchOneRow do;
    Result := True;
  end
  else
  begin
    while (CurrentRows.Count < RowCount) do
    begin
      if not FetchOneRow then
        Break;
    end;
    Result := CurrentRows.Count >= RowCount;
  end;
end;

{**
  Fetches one row from the result set.
  @return <code>True</code> if record was successfully fetched.
}
function TZAbstractRODataset.FetchOneRow: Boolean;
begin
  repeat
    if (FetchCount = 0) or (ResultSet.GetRow = FetchCount)
      or ResultSet.MoveAbsolute(FetchCount) then
      Result := ResultSet.Next
    else Result := False;
    if Result then
    begin
      Inc(FFetchCount);
      if FilterRow(ResultSet.GetRow) then
        CurrentRows.Add(Pointer(ResultSet.GetRow))
      else Continue;
    end;
  until True;
end;

{**
  Checks the specified row with the all filters.
  @param RowNo a number of the row.
  @return <code>True</code> if the row sutisfy to all filters.
}
function TZAbstractRODataset.FilterRow(RowNo: Integer): Boolean;
var
  SavedRow: Integer;
  SavedRows: TList;
  SavedState: TDatasetState;
begin
  Result := True;

  { Locates the result set to the specified row. }
  if ResultSet.GetRow <> RowNo then
  begin
    if not ResultSet.MoveAbsolute(RowNo) then
      Result := False;
  end;
  if not Result then Exit;

  { Checks record by ShowRecordType }
  if ResultSet.RowUpdated then
    Result := utModified in ShowRecordTypes
  else if ResultSet.RowInserted then
    Result := utInserted in ShowRecordTypes
  else if ResultSet.RowDeleted then
    Result := utDeleted in ShowRecordTypes
  else Result := utUnmodified in ShowRecordTypes;
  if not Result then Exit;

  { Checks record by OnFilterRecord event }
  if FilterEnabled and Assigned(OnFilterRecord) then
  begin
    SavedState  := State;
    SavedRow := CurrentRow;
    SavedRows := CurrentRows;
    CurrentRows := TList.Create;
    try
      CurrentRows.Add(Pointer(RowNo));
      CurrentRow := 1;
      SetState(dsNewValue);
      OnFilterRecord(Self, Result);
    finally
      CurrentRow := SavedRow;
      CurrentRows.Free;
      CurrentRows := SavedRows;
      SetState(SavedState);
    end;
  end;
  if not Result then Exit;
end;

{**
  Rereads all rows and applies a filter.
}
procedure TZAbstractRODataset.RereadRows;
var
  I, RowNo: Integer;
begin
  if (CurrentRow <= CurrentRows.Count) and (CurrentRows.Count > 0) then
    RowNo := Integer(CurrentRows[CurrentRow - 1])
  else RowNo := -1;
  CurrentRows.Clear;

  for I := 1 to FetchCount do
  begin
    if FilterRow(I) then
      CurrentRows.Add(Pointer(I));
  end;

  CurrentRow := CurrentRows.IndexOf(Pointer(RowNo)) + 1;
  CurrentRow := Min(Max(1, CurrentRow), CurrentRows.Count);

  if not (State in [dsInactive]) then
    Resync([]);
end;

{**
  Locates a specified record in dataset.
  @param Buffer a record buffer to put the contents of the row.
  @param GetMode a location mode.
  @param DoCheck flag to perform checking.
  @return a location result.
}
function TZAbstractRODataset.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  RowNo: Integer;
begin
  Result := grOK;
  case GetMode of
    gmNext:
      begin
        if FetchRows(CurrentRow + 1) then
          CurrentRow := CurrentRow + 1
        else Result := grEOF;
      end;
    gmPrior:
      begin
        CheckBiDirectional;
        if (CurrentRow > 1) and (CurrentRows.Count > 0) then
          CurrentRow := CurrentRow - 1
        else Result := grBOF;
      end;
    gmCurrent:
      begin
        if CurrentRow < CurrentRows.Count then
          CheckBiDirectional;

        if CurrentRow = 0 then
        begin
          if CurrentRows.Count = 0 then
            FetchRows(1);
          CurrentRow := Min(CurrentRows.Count, 1);
        end
        else if not FetchRows(CurrentRow) then
          CurrentRow := Max(1, Min(CurrentRows.Count, CurrentRow));

        if CurrentRows.Count = 0 then
          Result := grError;
      end;
  end;

  if Result = grOK then
  begin
    RowNo := Integer(CurrentRows[CurrentRow - 1]);
    if ResultSet.GetRow <> RowNo then
      ResultSet.MoveAbsolute(RowNo);
    RowAccessor.RowBuffer := PZRowBuffer(Buffer);
    RowAccessor.RowBuffer^.Index := RowNo;
    FetchFromResultSet(ResultSet, Fields, RowAccessor);
    FRowAccessor.RowBuffer^.BookmarkFlag := Ord(bfCurrent);
    GetCalcFields(Buffer);
  end;

  if (Result = grError) and DoCheck then
    DatabaseError('No more records in the result set.');
end;

{**
  Gets the current record buffer depended on the current dataset state.
  @param RowBuffer a reference to the result row buffer.
  @return <code>True</code> if the buffer was defined.
}
function TZAbstractRODataset.GetActiveBuffer(var RowBuffer: PZRowBuffer):
  Boolean;
var
  RowNo: Integer;
  CachedResultSet: IZCachedResultSet;
begin
  RowBuffer := nil;
  case State of
    dsBrowse:
      if not IsEmpty then
        RowBuffer := PZRowBuffer(ActiveBuffer);
    dsEdit, dsInsert:
      RowBuffer := PZRowBuffer(ActiveBuffer);
    dsCalcFields:
      RowBuffer := PZRowBuffer(CalcBuffer);
    dsOldValue, dsNewValue, dsCurValue:
      begin
        RowNo := Integer(CurrentRows[CurrentRow - 1]);
        if RowNo <> ResultSet.GetRow then
          CheckBiDirectional;

        if State = dsOldValue then
          RowBuffer := OldRowBuffer
        else RowBuffer := NewRowBuffer;

        if RowBuffer.Index <> RowNo then
        begin
          RowAccessor.RowBuffer := RowBuffer;
          RowAccessor.Clear;
          if (ResultSet.GetRow = RowNo) or ResultSet.MoveAbsolute(RowNo) then
          begin
            if (State = dsOldValue) and (ResultSet.
              QueryInterface(IZCachedResultSet, CachedResultSet) = 0) then
              CachedResultSet.MoveToInitialRow;
            FetchFromResultSet(ResultSet, Fields, RowAccessor);
            RowBuffer.Index := RowNo;
            ResultSet.MoveToCurrentRow;
          end else
            RowBuffer := nil;
        end;
      end;
  end;
  Result := RowBuffer <> nil;
end;

{**
  Retrieves the column value and stores it into the field buffer.
  @param Field an field object to be retrieved.
  @param Buffer a field value buffer.
  @return <code>True</code> if non-null value was retrieved.
}
function TZAbstractRODataset.GetFieldData(Field: TField;
  Buffer: Pointer): Boolean;
var
  ColumnIndex: Integer;
  RowBuffer: PZRowBuffer;
begin
  if GetActiveBuffer(RowBuffer) then
  begin
    ColumnIndex := Field.Index + 1;
    RowAccessor.RowBuffer := RowBuffer;
    if Buffer <> nil then
    begin
      case Field.DataType of
        { Processes DateTime fields. }
        ftDate, ftTime, ftDateTime:
          begin
            if Field.DataType <> ftTime then
            begin
              DateTimeToNative(Field.DataType,
                RowAccessor.GetTimestamp(ColumnIndex, Result), Buffer);
              Result := not Result;
            end
            else
            begin
              DateTimeToNative(Field.DataType,
                RowAccessor.GetTime(ColumnIndex, Result), Buffer);
              Result := not Result;
            end;
          end;
        { Processes binary array fields. }
        ftBytes:
          begin
//            PVariant(Buffer)^ := RowAccessor.GetBytes(ColumnIndex, Result);
            System.Move((PChar(RowAccessor.GetColumnData(ColumnIndex, Result)) + 2)^, Buffer^,
              RowAccessor.GetColumnDataSize(ColumnIndex)-2);
            Result := not Result;
          end;
        { Processes blob fields. }
        ftBlob, ftMemo, ftGraphic, ftFmtMemo:
          begin
            Result := not RowAccessor.GetBlob(ColumnIndex, Result).IsEmpty;
          end;
        { Processes all other fields. }
        else
          begin
            System.Move(RowAccessor.GetColumnData(ColumnIndex, Result)^, Buffer^,
              RowAccessor.GetColumnDataSize(ColumnIndex));
            Result := not Result;
          end;
      end;
    end
    else
    begin
      if Field.DataType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo] then
        Result := not RowAccessor.GetBlob(ColumnIndex, Result).IsEmpty
      else Result := not RowAccessor.IsNull(ColumnIndex);
    end;
  end else
    Result := False;
end;

{**
  Stores the column value from the field buffer.
  @param Field an field object to be stored.
  @param Buffer a field value buffer.
}
procedure TZAbstractRODataset.SetFieldData(Field: TField; Buffer: Pointer);
var
  ColumnIndex: Integer;
  RowBuffer: PZRowBuffer;
  WasNull: Boolean;
begin
  if not Active then
    DatabaseError('Operation is not allowed for closed dataset.');
  if not RequestLive and (Field.FieldKind = fkData) then
    RaiseReadOnlyError;

  if GetActiveBuffer(RowBuffer) then
  begin
    ColumnIndex := Field.Index + 1;
    RowAccessor.RowBuffer := RowBuffer;

    if State in [dsEdit, dsInsert] then
      Field.Validate(Buffer);

    if Buffer <> nil then
    begin
      { Processes DateTime fields. }
      if Field.DataType in [ftDate, ftDateTime] then
      begin
        RowAccessor.SetTimestamp(ColumnIndex, NativeToDateTime(Field.DataType,
        Buffer));
      end
      { Processes Time fields. }
      else if Field.DataType = ftTime then
      begin
        RowAccessor.SetTime(ColumnIndex, NativeToDateTime(Field.DataType,
        Buffer));
      end
      { Processes binary array fields. }
      else if Field.DataType = ftBytes then
      begin
        RowAccessor.SetBytes(ColumnIndex, Var2Bytes(PVariant(Buffer)^));
      end
      { Processes all other fields. }
      else
      begin
        System.Move(Buffer^, RowAccessor.GetColumnData(ColumnIndex, WasNull)^,
          RowAccessor.GetColumnDataSize(ColumnIndex));
        RowAccessor.SetNotNull(ColumnIndex);
      end;
    end else
      RowAccessor.SetNull(ColumnIndex);

    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, LongInt(Field));
  end else
    DatabaseError('Row data is not available.');

  if Field.FieldKind = fkData then
  begin
    OldRowBuffer.Index := -1;
    NewRowBuffer.Index := -1;
  end;
end;

{**
  Checks is the cursor opened.
  @return <code>True</code> if the cursor is opened.
}
function TZAbstractRODataset.IsCursorOpen: Boolean;
begin
  Result := ResultSet <> nil;
end;

{**
  Gets an affected rows by the last executed statement.
  @return a number of last updated rows.
}
function TZAbstractRODataset.RowsAffected: LongInt;
begin
  if Statement <> nil then
    Result := Statement.GetUpdateCount
  else Result := 0;
end;

{**
  Gets the size of the record buffer.
  @return the size of the record buffer.
}
function TZAbstractRODataset.GetRecordSize: Word;
begin
  Result := RowAccessor.RowSize;
end;

{**
  Allocates a buffer for new record.
  @return an allocated record buffer.
}
function TZAbstractRODataset.AllocRecordBuffer: PChar;
begin
  Result := PChar(RowAccessor.Alloc);
end;

{**
  Frees a previously allocated record buffer.
  @param Buffer a previously allocated buffer.
}
procedure TZAbstractRODataset.FreeRecordBuffer(var Buffer: PChar);
begin
  RowAccessor.DisposeBuffer(PZRowBuffer(Buffer));
end;

{**
  Executes a DML SQL statement.
}
procedure TZAbstractRODataset.ExecSQL;
begin
  if Active then Close;

  CheckSQLQuery;
  CheckInactive;
  CheckConnected;

  if Statement = nil then
    Statement := CreateStatement(FSQL.Statements[0].SQL);

  if ParamCheck then
  begin
    FillStatementWithParams(Statement, FSQL.Statements[0].ParamNamesArray,
      FParams);
  end;
  Statement.ExecuteUpdatePrepared;
end;

{**
  Performs an internal initialization of field defiitions.
}
procedure TZAbstractRODataset.InternalInitFieldDefs;
var
  I, J, Size: Integer;
  AutoInit: Boolean;
  FieldType: TFieldType;
  Statement: IZPreparedStatement;
  ResultSet: IZResultSet;
  FieldName: string;
  FName: string;
begin
  FieldDefs.Clear;
  ResultSet := Self.ResultSet;
  AutoInit := ResultSet = nil;

  try
    { Opens an internal result set if query is closed. }
    if AutoInit then
    begin
      CheckSQLQuery;
      CheckConnected;
      Statement := Connection.DbcConnection.PrepareStatement(
        FSQL.Statements[0].SQL);
      if ParamCheck then
      begin
        FillStatementWithParams(Statement, FSQL.Statements[0].ParamNamesArray,
          FParams);
      end;
      ResultSet := Statement.ExecuteQueryPrepared;
    end;
    if not Assigned(ResultSet) then
      raise Exception.Create('Sorry, there was no resultset returned'); 

    { Reads metadata from resultset. }
    with ResultSet.GetMetadata do
    begin
      for I := 1 to GetColumnCount do
      begin
        FieldType := ConvertDbcToDatasetType(GetColumnType(I));
        if FieldType in [ftString, ftBytes] then
          Size := GetPrecision(I)
        else Size := 0;

        J := 0;
        FieldName := GetColumnLabel(I);
        FName := FieldName;
        while FieldDefs.IndexOf(FName) >= 0 do
        begin
          Inc(J);
          FName := Format('%s_%d', [FieldName, J]);
        end;

        with TFieldDef.Create(FieldDefs, FName, FieldType,
          Size, True, I) do
        begin
//This is added to avoid unneccesseary metadata reading
          if RequestLive then
          begin
            Required := IsNullable(I) = ntNoNulls;
            if IsReadOnly(I) then
              Attributes := Attributes + [faReadonly];
          end
          else
            Attributes := Attributes + [faReadonly];
          Precision := GetPrecision(I);
          DisplayName := FName;
        end;
      end;
    end;

  finally
    { Closes localy opened resultset. }
    if AutoInit then
    begin
      if ResultSet <> nil then
      begin
        ResultSet.Close;
        ResultSet := nil;
      end;
      if Statement <> nil then
      begin
        Statement.Close;
        Statement := nil;
      end;
    end;
  end;
end;

{**
  Creates a DBC statement for the query.
  @param SQL an SQL query.
  @returns a created DBC statement.
}
function TZAbstractRODataset.CreateStatement(SQL: string):
  IZPreparedStatement;
begin
  Result := FConnection.DbcConnection.PrepareStatement(SQL);
end;

{**
  Performs internal query opening.
}
procedure TZAbstractRODataset.InternalOpen;
begin
  CheckSQLQuery;
  CheckConnected;

  CurrentRow := 0;
  FetchCount := 0;
  CurrentRows.Clear;

  { Creates an SQL statement and resultsets }
  Statement := CreateStatement(FSQL.Statements[0].SQL);
  if ParamCheck then
  begin
    FillStatementWithParams(Statement, FSQL.Statements[0].ParamNamesArray,
      FParams);
  end;
  if RequestLive then
    Statement.SetResultSetConcurrency(rcUpdatable)
  else Statement.SetResultSetConcurrency(rcReadOnly);
  Statement.SetFetchDirection(fdForward);
  if IsUniDirectional then
    Statement.SetResultSetType(rtForwardOnly)
  else Statement.SetResultSetType(rtScrollInsensitive);
  ResultSet := Statement.ExecuteQueryPrepared;

  { Initializes field and index defs. }
  InternalInitFieldDefs;
  if DefaultFields and not FRefreshInProgress then
    CreateFields;
  BindFields(True);

  { Initializes accessors and buffers. }
  RowAccessor := TZRowAccessor.Create(ConvertFieldsToColumnInfo(Fields));
  FOldRowBuffer := PZRowBuffer(AllocRecordBuffer);
  FNewRowBuffer := PZRowBuffer(AllocRecordBuffer);
end;

{**
  Performs internal query closing.
}
procedure TZAbstractRODataset.InternalClose;
begin
  if ResultSet <> nil then
    ResultSet.Close;
  ResultSet := nil;
  if Statement <> nil then
    Statement.Close;
  Statement := nil;

  if FOldRowBuffer <> nil then
    FreeRecordBuffer(PChar(FOldRowBuffer));
  FOldRowBuffer := nil;
  if FNewRowBuffer <> nil then
    FreeRecordBuffer(PChar(FNewRowBuffer));
  FNewRowBuffer := nil;

  if RowAccessor <> nil then
    RowAccessor.Free;
  RowAccessor := nil;

  { Destroy default fields }
  if DefaultFields and not FRefreshInProgress then
    DestroyFields;

  CurrentRows.Clear;
end;

{**
  Performs internal go to first record.
}
procedure TZAbstractRODataset.InternalFirst;
begin
  if CurrentRow > 0 then
    CheckBiDirectional;
  CurrentRow := 0;
end;

{**
  Performs internal go to last record.
}
procedure TZAbstractRODataset.InternalLast;
begin
  FetchRows(0);
  if CurrentRows.Count > 0 then
    CurrentRow := CurrentRows.Count + 1
  else CurrentRow := 0;
end;

{**
  Processes internal exception handling.
}
procedure TZAbstractRODataset.InternalHandleException;
begin
//  Application.HandleException(Self);
end;

{**
  Gets the maximum records count.
  @return the maximum records count.
}
function TZAbstractRODataset.GetRecordCount: LongInt;
begin
  CheckActive;
  if not IsUniDirectional then
    FetchRows(0);
  Result := CurrentRows.Count;
end;

{**
  Gets the current record number.
  @return the current record number.
}
function TZAbstractRODataset.GetRecNo: Longint;
begin
  UpdateCursorPos;
  Result := CurrentRow;
end;

{**
  Sets a new currenct record number.
  @param Value a new current record number.
}
procedure TZAbstractRODataset.SetRecNo(Value: Integer);
var
  PreviousCurrentRow: Integer;
begin
  Value := Max(1, Value);
  if Value < CurrentRow then
    CheckBiDirectional;

  if FetchRows(Value) then
    CurrentRow := Value
  else CurrentRow := CurrentRows.Count;

  PreviousCurrentRow := CurrentRow;//Resync moves the current row away
  try
    if not (State in [dsInactive]) then Resync([]);
  finally
    CurrentRow := PreviousCurrentRow;
  end;
end;

{**
  Defines is the query editable?
  @return <code>True</code> if the query is editable.
}
function TZAbstractRODataset.GetCanModify: Boolean;
begin
  Result := RequestLive;
end;

{**
  Performs internal switch to the specified bookmark.
  @param Bookmark a specified bookmark.
}
procedure TZAbstractRODataset.InternalGotoBookmark(Bookmark: Pointer);
var
  Index: Integer;
begin
  Index := CurrentRows.IndexOf(PPointer(Bookmark)^);

  if Index < 0 then
    DatabaseError('Bookmark was not found.');
  if Index < CurrentRow then
    CheckBiDirectional;

  CurrentRow := Index + 1;
end;

{**
  Performs an internal switch to the specified record.
  @param Buffer the specified row buffer.
}
procedure TZAbstractRODataset.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@PZRowBuffer(Buffer)^.Index);
end;

{**
  Performs an internal adding a new record.
  @param Buffer a buffer of the new adding record.
  @param Append <code>True</code> if record should be added to the end
    of the result set.
}
procedure TZAbstractRODataset.InternalAddRecord(Buffer: Pointer;
  Append: Boolean);
begin
  RaiseReadOnlyError;
end;

{**
  Performs an internal record removing.
}
procedure TZAbstractRODataset.InternalDelete;
begin
  RaiseReadOnlyError;
end;

{**
  Performs an internal post updates.
}
procedure TZAbstractRODataset.InternalPost;
begin
  RaiseReadOnlyError;
end;

{**
  Gets a bookmark flag from the specified record.
  @param Buffer a pointer to the record buffer.
  @return a bookmark flag from the specified record.
}
function TZAbstractRODataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := TBookmarkFlag(PZRowBuffer(Buffer)^.BookmarkFlag);
end;

{**
  Sets a new bookmark flag to the specified record.
  @param Buffer a pointer to the record buffer.
  @param Value a new bookmark flag to the specified record.
}
procedure TZAbstractRODataset.SetBookmarkFlag(Buffer: PChar;
  Value: TBookmarkFlag);
begin
  PZRowBuffer(Buffer)^.BookmarkFlag := Ord(Value);
end;

{**
  Gets bookmark value from the specified record.
  @param Buffer a pointer to the record buffer.
  @param Data a pointer to the bookmark value.
}
procedure TZAbstractRODataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PInteger(Data)^ := PZRowBuffer(Buffer)^.Index;
end;

{**
  Sets a new bookmark value from the specified record.
  @param Buffer a pointer to the record buffer.
  @param Data a pointer to the bookmark value.
}
procedure TZAbstractRODataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PZRowBuffer(Buffer)^.Index := PInteger(Data)^;
end;

{**
  Compare two specified bookmarks.
  @param Bookmark1 the first bookmark object.
  @param Bookmark2 the second bookmark object.
  @return 0 if bookmarks are equal, -1 if the first bookmark is less,
    1 if the first bookmark is greatter.
}
function TZAbstractRODataset.CompareBookmarks(Bookmark1,
  Bookmark2: TBookmark): Integer;
var
  Index1, Index2: Integer;
begin
  Result := 0;
  if not Assigned(Bookmark1) or not Assigned(Bookmark2) then
    Exit;
  Index1 := CurrentRows.IndexOf(PPointer(Bookmark1)^);
  Index2 := CurrentRows.IndexOf(PPointer(Bookmark2)^);
  if Index1 < Index2 then Result := -1
  else if Index1 > Index2 then Result := 1;
end;

{**
  Checks is the specified bookmark valid.
  @param Bookmark a bookmark object.
  @return <code>True</code> if the bookmark is valid.
}
function TZAbstractRODataset.BookmarkValid(Bookmark: TBookmark): Boolean;
var
  Index: Integer;
begin
  Result := False;
  if Active and Assigned(Bookmark) and (FResultSet <> nil) then
    try
      Index := CurrentRows.IndexOf(PPointer(Bookmark)^);
      Result := Index >= 0;
    except
      Result := False;
    end;
end;

{**
  Performs an internal initialization of record buffer.
  @param Buffer a record buffer for initialization.
}
procedure TZAbstractRODataset.InternalInitRecord(Buffer: PChar);
begin
  RowAccessor.ClearBuffer(PZRowBuffer(Buffer));
end;

{**
  Performs an internal refreshing.
}
procedure TZAbstractRODataset.InternalRefresh;
var
  RowNo: Integer;
  Found: Boolean;
  KeyFields: string;
  Temp: Variant;
  KeyValues: Variant;
  FieldIndices: TIntegerDynArray;
  OnlyDataFields: Boolean;
begin
  FieldIndices := nil;
  if Active then
  begin
    if CurrentRow > 0 then
    begin
      RowNo := Integer(CurrentRows[CurrentRow - 1]);
      if ResultSet.GetRow <> RowNo then
        ResultSet.MoveAbsolute(RowNo);

      KeyFields := DefineKeyFields(Fields);
      FieldIndices := DefineFieldsIndices(Self, KeyFields, OnlyDataFields);
      Temp := RetrieveDataFields(Fields, FieldIndices, ResultSet);
      if Length(FieldIndices) = 1 then
        KeyValues := Temp[0]
      else KeyValues := Temp;
    end
    else
    begin
      KeyFields := '';
      KeyValues := Unassigned;
    end;

    DisableControls;
    try
      try
        FRefreshInProgress := True;
        InternalClose;
        InternalOpen;
      finally
        FRefreshInProgress := False;
      end;

      DoBeforeScroll;
      if KeyFields <> '' then
        Found := Locate(KeyFields, KeyValues, [])
      else Found := False;
    finally
      FRefreshInProgress := False;
      EnableControls;
    end;

    if not Found then
    begin
      DoBeforeScroll;
      DoAfterScroll;
    end;
  end;
end;

{**
  Finds the next record in a filtered query.
  @param Restart a <code>True</code> to find from the start of the query.
  @param GoForward <code>True</code> to navigate in the forward direction.
  @return <code>True</code> if a sutisfied row was found.
}
function TZAbstractRODataset.FindRecord(Restart, GoForward: Boolean): Boolean;
var
  Index: Integer;
  SavedFilterEnabled: Boolean;
begin
  { Checks the current state. }
  CheckBrowseMode;
  DoBeforeScroll;
  Result := False;

  { Defines an initial position position. }
  if Restart then
  begin
    if GoForward then
      Index := 1
    else
    begin
      FetchRows(0);
      Index := CurrentRows.Count;
    end
  end
  else
  begin
    Index := CurrentRow;
    if GoForward then
    begin
      Inc(Index);
      if Index > CurrentRows.Count then
        FetchOneRow;
    end else
      Dec(Index);
  end;

  { Finds a record. }
  SavedFilterEnabled := FilterEnabled;
  try
    FilterEnabled := True;
    while (Index >= 1) and (Index <= CurrentRows.Count) do
    begin
      if FilterRow(Index) then
      begin
        Result := True;
        Break;
      end;
      if GoForward then
      begin
        Inc(Index);
        if Index > CurrentRows.Count then
          FetchOneRow;
      end else
        Dec(Index)
    end
  finally
    FilterEnabled := SavedFilterEnabled;
  end;

  { Sets a new found position. }
  SetFound(Result);
  if Result then
  begin
    RecNo := Index;
    DoAfterScroll;
  end;
end;

{**
  Sets a filtering control flag.
  @param Value <code>True</code> to turn filtering On.
}
procedure TZAbstractRODataset.SetFiltered(Value: Boolean);
begin
  if Value <> FilterEnabled then
  begin
    FilterEnabled := Value;
    inherited SetFiltered(Value);

    if not (State in [dsInactive])
      and not IsUniDirectional then
      RereadRows;
  end;
end;

{**
  Checks is the opened resultset sequensed?
  @return <code>True</code> if the opened resultset is sequenced.
}
function TZAbstractRODataset.IsSequenced: Boolean;
begin
  Result := (not FilterEnabled);
end;

{**
  Processes component notifications.
  @param AComponent a changed component object.
  @param Operation a component operation code.
}
procedure TZAbstractRODataset.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = FConnection) then
  begin
    Close;
    FConnection := nil;
  end;
end;

{**
  Performs an internal record search.
  @param KeyFields a list of field names.
  @param KeyValues a list of field values.
  @param Options a search options.
  @return an index of found row or -1 if nothing was found.
}
function TZAbstractRODataset.InternalLocate(KeyFields: string;
  KeyValues: Variant; Options: TLocateOptions): LongInt;
var
  I, RowNo: Integer;
  FieldIndices: TIntegerDynArray;
  OnlyDataFields: Boolean;
  SearchRowBuffer: PZRowBuffer;
  RowValues: Variant;
begin
  CheckBrowseMode;
  Result := -1;
  FieldIndices := DefineFieldsIndices(Self, KeyFields, OnlyDataFields);

  if FieldIndices = nil then Exit;
  if Length(FieldIndices) = 1 then
  begin
    { !!! Just hack. Delphi  }
    if VarType(KeyValues) = $0014 then
      KeyValues := VarAsType(KeyValues, varInteger);
    { Allow users to specify a single value. }
    if VarArrayDimCount(KeyValues) = 0 then
      KeyValues := VarArrayOf([KeyValues]);
  end;
  { Checks for equal field and values number }
  if Length(FieldIndices) <> (VarArrayHighBound(KeyValues, 1)
    - VarArrayLowBound(KeyValues,1) + 1) then
  begin
    DatabaseError('Incorrect number of search field values.');
  end;

  if not OnlyDataFields then
    SearchRowBuffer := PZRowBuffer(AllocRecordBuffer)
  else SearchRowBuffer := nil;
  try
    I := 1;
    if I > CurrentRows.Count then
      FetchOneRow;
    while I <= CurrentRows.Count do
    begin
      RowNo := Integer(CurrentRows[I - 1]);
      if ResultSet.GetRow <> RowNo then
        ResultSet.MoveAbsolute(RowNo);

      if OnlyDataFields then
        RowValues := RetrieveDataFields(Fields, FieldIndices, ResultSet)
      else begin
        RowAccessor.RowBuffer := SearchRowBuffer;
        RowAccessor.RowBuffer^.Index := RowNo;
        FetchFromResultSet(ResultSet, Fields, RowAccessor);
        GetCalcFields(PChar(SearchRowBuffer));
        RowValues := RetrieveDataFields(Fields, FieldIndices, RowAccessor);
      end;

      if CompareDataFields(KeyValues, RowValues, Options) then
      begin
        Result := I;
        Break;
      end;

      Inc(I);
      if I > CurrentRows.Count then
        FetchOneRow;
    end;
  finally
    if SearchRowBuffer <> nil then
      FreeRecordBuffer(PChar(SearchRowBuffer));
  end;
end;

{**
  Locates an interested record by specified search criteria.
  @param KeyFields a list of field names.
  @param KeyValues a list of field values.
  @param Options a search options.
  @return <code>True</code> if record was found or <code>False</code> otherwise.
}
function TZAbstractRODataset.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  Index: Integer;
begin
  DoBeforeScroll;
  Index := InternalLocate(KeyFields, KeyValues, Options);
  if Index > 0 then
  begin
    RecNo := Index;
    Result := True;
    DoAfterScroll;
  end else
    Result := False;
  SetFound(Result);
end;

{**
  Lookups specified fields from the searched record.
  @param KeyValues a list of field names to search record.
  @param KeyValues an array of field values to search record.
  @param ResultFields a list of field names to return as a result.
  @return an array of requested field values.
}
function TZAbstractRODataset.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
var
  RowNo: Integer;
  FieldIndices: TIntegerDynArray;
  OnlyDataFields: Boolean;
  SearchRowBuffer: PZRowBuffer;
  FirstValue: Variant;
begin
  Result := Null;
  FieldIndices := DefineFieldsIndices(Self, ResultFields, OnlyDataFields);
  RowNo := InternalLocate(KeyFields, KeyValues, []);
  if RowNo < 0 then Exit;

  { Fill result array }
  SearchRowBuffer := PZRowBuffer(AllocRecordBuffer);
  try
    RowNo := Integer(CurrentRows[RowNo - 1]);
    if ResultSet.GetRow <> RowNo then
      ResultSet.MoveAbsolute(RowNo);

    RowAccessor.RowBuffer := SearchRowBuffer;
    RowAccessor.RowBuffer^.Index := RowNo;
    FetchFromResultSet(ResultSet, Fields, RowAccessor);
    GetCalcFields(PChar(SearchRowBuffer));
    Result := RetrieveDataFields(Fields, FieldIndices, RowAccessor);
    FirstValue := Result[0];
  finally
    FreeRecordBuffer(PChar(SearchRowBuffer));
  end;

  if Length(FieldIndices) = 1 then
    Result := FirstValue;
end;

{**
  Gets the updated status for the current row.
  @return the UpdateStatus value for the current row.
}
function TZAbstractRODataset.UpdateStatus: TUpdateStatus;
var
  RowNo: Integer;
begin
  Result := usUnmodified;
  if ResultSet <> nil then
  begin
    RowNo := Integer(CurrentRows[CurrentRow - 1]);
    if ResultSet.GetRow <> RowNo then
      ResultSet.MoveAbsolute(RowNo);

    if ResultSet.RowInserted then
      Result := usInserted
    else if ResultSet.RowUpdated then
      Result := usModified
    else if ResultSet.RowDeleted then
      Result := usDeleted;
  end;
end;

{**
  Creates a stream object for specified blob field.
  @param Field an interested field object.
  @param Mode a blob open mode.
  @return a created stream object.
}
function TZAbstractRODataset.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
var
  ColumnIndex: Integer;
  RowBuffer: PZRowBuffer;
  WasNull: Boolean;
begin
  Result := nil;
  if (Field.DataType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo])
    and GetActiveBuffer(RowBuffer) then
  begin
    ColumnIndex := Field.Index + 1;
    RowAccessor.RowBuffer := RowBuffer;

    if Mode = bmRead then
    begin
      if Field.DataType in [ftMemo, ftFmtMemo] then
        Result := RowAccessor.GetAsciiStream(ColumnIndex, WasNull)
      else Result := RowAccessor.GetBinaryStream(ColumnIndex, WasNull);
    end else
      Result := TZBlobStream.Create(
        RowAccessor.GetBlob(ColumnIndex, WasNull), Mode);
  end;
  if Result = nil then
    Result := TMemoryStream.Create;
end;

{**
  Closes the specified BLOB field.
  @param a BLOB field object.
}
procedure TZAbstractRODataset.CloseBlob(Field: TField);
begin
end;


end.

