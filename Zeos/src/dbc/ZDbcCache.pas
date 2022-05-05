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

unit ZDbcCache;

interface

{$I ZDbc.inc}

{$R-}

uses
  ZCompatibility, Classes, SysUtils, ZClasses, ZDbcIntfs, ZDbcResultSet,
  ZDbcResultSetMetadata;

type

  {** Defines a row status type. }
  TZRowUpdateType = (utModified, utInserted, utDeleted, utUnmodified);
  TZRowUpdateTypes = set of TZRowUpdateType;

  {** Defines a header for row buffer. }
  TZRowBuffer = packed record
    Index: Integer;
    UpdateType: TZRowUpdateType;
    BookmarkFlag: Byte;
    Columns: TByteArray;
  end;
  PZRowBuffer = ^TZRowBuffer;

  {** Implements a column buffer accessor. }
  TZRowAccessor = class(TObject)
  private
    FColumnsInfo: IZCollection;
    FRowSize: Integer;
    FColumnCount: Integer;
    FColumnNames: array of string;
    FColumnCases: array of Boolean;
    FColumnTypes: array of TZSQLType;
    FColumnLengths: array of Integer;
    FColumnOffsets: array of Integer;
    FBuffer: PZRowBuffer;

    function GetColumnSize(ColumnInfo: IZColumnInfo): Integer;
    function GetColumnsSize: Integer;
    function GetRowSize: Integer;
    function GetBuffer: PZRowBuffer;
    procedure SetBuffer(Value: PZRowBuffer);
    function GetBlobObject(Buffer: PZRowBuffer; ColumnIndex: Integer): IZBlob;
    procedure SetBlobObject(Buffer: PZRowBuffer; ColumnIndex: Integer;
      Value: IZBlob);

  protected
    procedure CheckColumnIndex(ColumnIndex: Integer);
    procedure CheckColumnConvertion(ColumnIndex: Integer; ResultType: TZSQLType);

  public
    constructor Create(ColumnsInfo: IZCollection);
    destructor Destroy; override;

    function AllocBuffer(var Buffer: PZRowBuffer): PZRowBuffer;
    procedure InitBuffer(Buffer: PZRowBuffer);
    procedure CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
    procedure MoveBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
    procedure ClearBuffer(Buffer: PZRowBuffer);
    procedure DisposeBuffer(Buffer: PZRowBuffer);

    function Alloc: PZRowBuffer;
    procedure Init;
    procedure CopyTo(DestBuffer: PZRowBuffer);
    procedure CopyFrom(SrcBuffer: PZRowBuffer);
    procedure MoveTo(DestBuffer: PZRowBuffer);
    procedure MoveFrom(SrcBuffer: PZRowBuffer);
    procedure Clear;
    procedure Dispose;

    function GetColumnData(ColumnIndex: Integer; var IsNull: Boolean): Pointer;
    function GetColumnDataSize(ColumnIndex: Integer): Integer;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(ColumnIndex: Integer): Boolean;
    function GetPChar(ColumnIndex: Integer; var IsNull: Boolean): PChar;
    function GetString(ColumnIndex: Integer; var IsNull: Boolean): string;
    function GetUnicodeString(ColumnIndex: Integer; var IsNull: Boolean): WideString;
    function GetBoolean(ColumnIndex: Integer; var IsNull: Boolean): Boolean;
    function GetByte(ColumnIndex: Integer; var IsNull: Boolean): ShortInt;
    function GetShort(ColumnIndex: Integer; var IsNull: Boolean): SmallInt;
    function GetInt(ColumnIndex: Integer; var IsNull: Boolean): Integer;
    function GetLong(ColumnIndex: Integer; var IsNull: Boolean): LongInt;
    function GetFloat(ColumnIndex: Integer; var IsNull: Boolean): Single;
    function GetDouble(ColumnIndex: Integer; var IsNull: Boolean): Double;
    function GetBigDecimal(ColumnIndex: Integer; var IsNull: Boolean): Int64;
    function GetBytes(ColumnIndex: Integer; var IsNull: Boolean): TByteDynArray;
    function GetDate(ColumnIndex: Integer; var IsNull: Boolean): TDateTime;
    function GetTime(ColumnIndex: Integer; var IsNull: Boolean): TDateTime;
    function GetTimestamp(ColumnIndex: Integer; var IsNull: Boolean): TDateTime;
    function GetAsciiStream(ColumnIndex: Integer; var IsNull: Boolean): TStream;
    function GetUnicodeStream(ColumnIndex: Integer; var IsNull: Boolean): TStream;
    function GetBinaryStream(ColumnIndex: Integer; var IsNull: Boolean): TStream;
    function GetBlob(ColumnIndex: Integer; var IsNull: Boolean): IZBlob;


    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    procedure SetNotNull(ColumnIndex: Integer);
    procedure SetNull(ColumnIndex: Integer);
    procedure SetBoolean(ColumnIndex: Integer; Value: Boolean);
    procedure SetByte(ColumnIndex: Integer; Value: ShortInt);
    procedure SetShort(ColumnIndex: Integer; Value: SmallInt);
    procedure SetInt(ColumnIndex: Integer; Value: Integer);
    procedure SetLong(ColumnIndex: Integer; Value: LongInt);
    procedure SetFloat(ColumnIndex: Integer; Value: Single);
    procedure SetDouble(ColumnIndex: Integer; Value: Double);
    procedure SetBigDecimal(ColumnIndex: Integer; Value: Int64);
    procedure SetPChar(ColumnIndex: Integer; Value: PChar);
    procedure SetString(ColumnIndex: Integer; Value: string);
    procedure SetUnicodeString(ColumnIndex: Integer; Value: WideString);
    procedure SetBytes(ColumnIndex: Integer; Value: TByteDynArray);
    procedure SetDate(ColumnIndex: Integer; Value: TDateTime);
    procedure SetTime(ColumnIndex: Integer; Value: TDateTime);
    procedure SetTimestamp(ColumnIndex: Integer; Value: TDateTime);
    procedure SetAsciiStream(ColumnIndex: Integer; Value: TStream);
    procedure SetUnicodeStream(ColumnIndex: Integer; Value: TStream);
    procedure SetBinaryStream(ColumnIndex: Integer; Value: TStream);
    procedure SetBlob(ColumnIndex: Integer; Value: IZBlob);

    property ColumnsInfo: IZCollection read FColumnsInfo;
    property ColumnsSize: Integer read GetColumnsSize;
    property RowSize: Integer read GetRowSize;
    property RowBuffer: PZRowBuffer read GetBuffer write SetBuffer;
  end;

const
  RowHeaderSize = SizeOf(TZRowBuffer) - SizeOf(TByteArray);

implementation

uses Math, ZSysUtils, ZDbcUtils, ZCollections;

{ TZRowAccessor }

{**
  Creates this object and assignes the main properties.
  @param ColumnsInfo a collection with column information.
}
constructor TZRowAccessor.Create(ColumnsInfo: IZCollection);
var
  I: Integer;
  Current: IZColumnInfo;
begin
  FColumnsInfo := TZCollection.Create;
  FColumnsInfo.AddAll(ColumnsInfo);

  FBuffer := nil;
  FColumnCount := ColumnsInfo.Count;
  FRowSize := 0;
  SetLength(FColumnNames, FColumnCount);
  SetLength(FColumnCases, FColumnCount);
  SetLength(FColumnTypes, FColumnCount);
  SetLength(FColumnLengths, FColumnCount);
  SetLength(FColumnOffsets, FColumnCount);

  for I := 0 to FColumnCount - 1 do
  begin
    Current := ColumnsInfo[I] as IZColumnInfo;
    FColumnNames[I] := Current.GetColumnName;
    FColumnCases[I] := Current.IsCaseSensitive;
    FColumnTypes[I] := Current.GetColumnType;
    FColumnLengths[I] := GetColumnSize(Current);
    FColumnOffsets[I] := FRowSize;
    FRowSize := FRowSize + FColumnLengths[I] + 1;
  end;
  FRowSize := FRowSize + RowHeaderSize;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZRowAccessor.Destroy;
begin
  inherited Destroy;
end;

{**
  Checks is the column index correct and row buffer is available.
  @param ColumnIndex an index of column.
}
procedure TZRowAccessor.CheckColumnIndex(ColumnIndex: Integer);
begin
  if not Assigned(FBuffer) then
    raise EZSQLException.Create('Row buffer is not assigned.');

  if (ColumnIndex <= 0) or (ColumnIndex > FColumnCount) then
  begin
    raise EZSQLException.Create(
      Format('Column with index %d is not accessable.', [ColumnIndex]));
  end;
end;

{**
  Checks is the column convertion from one type to another type allowed.
  @param ColumnIndex an index of column.
  @param ResultType a requested data type.
  @return <code>true</code> if convertion is allowed or throw exception
    otherwise.
}
procedure TZRowAccessor.CheckColumnConvertion(ColumnIndex: Integer;
  ResultType: TZSQLType);
begin
  if not Assigned(FBuffer) then
    raise EZSQLException.Create('Row buffer is not assigned.');

  if (ColumnIndex <= 0) or (ColumnIndex > FColumnCount) then
  begin
    raise EZSQLException.Create(
      Format('Column with index %d is not accessable.', [ColumnIndex]));
  end;

  if not CheckConvertion(FColumnTypes[ColumnIndex - 1], ResultType) then
  begin
    raise EZSQLException.Create(
      Format('Convertion is not possible for column %d from %s to %s.',
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex - 1]),
      DefineColumnTypeName(ResultType)]));
  end;
end;

{**
  Gets a size of column with the specified type.
  @param ColumnInfo a column information struct.
  @return a size for the column with the specified type.
}
function TZRowAccessor.GetColumnSize(ColumnInfo: IZColumnInfo): Integer;
begin
  case ColumnInfo.GetColumnType of
    stBoolean:
      Result := SizeOf(WordBool);
    stByte:
      Result := SizeOf(Byte);
    stShort:
      Result := SizeOf(SmallInt);
    stInteger:
      Result := SizeOf(Integer);
    stLong:
      Result := SizeOf(LongInt);
    stFloat:
      Result := SizeOf(Single);
    stDouble:
      Result := SizeOf(Double);
    stBigDecimal:
      Result := SizeOf(Int64);
    stString:
      Result := ColumnInfo.GetPrecision + 1;
    stBytes:
      Result := SizeOf(SmallInt) + ColumnInfo.GetPrecision;
    stDate, stTime, stTimestamp:
      Result := SizeOf(TDateTime);
    stAsciiStream, stUnicodeStream, stBinaryStream:
      Result := SizeOf(Pointer);
    else
      Result := 0;
  end;
end;

{**
  Gets a columns buffer size without row header.
  @return a columns buffer size without row header.
}
function TZRowAccessor.GetColumnsSize: Integer;
begin
  Result := FRowSize - RowHeaderSize;
end;

{**
  Gets a row buffer size.
  @return a row buffer size.
}
function TZRowAccessor.GetRowSize: Integer;
begin
  Result := FRowSize;
end;

{**
  Gets the associated row buffer pointer.
  @return the associated row buffer pointer.
}
function TZRowAccessor.GetBuffer: PZRowBuffer;
begin
  Result := FBuffer;
end;

{**
  Sets a new associated row buffer pointer.
  @param Value a new associated row buffer pointer.
}
procedure TZRowAccessor.SetBuffer(Value: PZRowBuffer);
begin
  FBuffer := PZRowBuffer(Value);
end;

{**
  Gets a stream from the specified columns.
  @param Buffer a row buffer.
  @param ColumnIndex an index of the column.
}
function TZRowAccessor.GetBlobObject(Buffer: PZRowBuffer;
  ColumnIndex: Integer): IZBlob;
var
  BlobPtr: PPointer;
  NullPtr: PByte;
begin
  BlobPtr := PPointer(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
  NullPtr := PByte(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1]]);

  if NullPtr^ = 0 then
    Result := IZBlob(BlobPtr^)
  else Result := nil;
end;

{**
  Sets a blob into the specified columns.
  @param Buffer a row buffer.
  @param ColumnIndex an index of the column.
  @param Value a stream object to be set.
}
procedure TZRowAccessor.SetBlobObject(Buffer: PZRowBuffer; ColumnIndex: Integer;
  Value: IZBlob);
var
  BlobPtr: PPointer;
  NullPtr: PByte;
begin
  BlobPtr := PPointer(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
  NullPtr := PByte(@Buffer.Columns[FColumnOffsets[ColumnIndex - 1]]);

  if NullPtr^ = 0 then
    IZBlob(BlobPtr^) := nil
  else BlobPtr^ := nil;

  IZBlob(BlobPtr^) := Value;

  if Value <> nil then
    NullPtr^ := 0
  else NullPtr^ := 1;
end;

{**
  Allocates a new row buffer and sets it into the variable.
  @param Buffer a pointer to row buffer.
  @return a pointer to the allocated buffer.
}
function TZRowAccessor.AllocBuffer(var Buffer: PZRowBuffer): PZRowBuffer;
begin
  GetMem(Buffer, GetRowSize);
  InitBuffer(Buffer);
  Result := Buffer;
end;

{**
  Disposes the specified row buffer.
  @param Buffer a pointer to row buffer.
}
procedure TZRowAccessor.DisposeBuffer(Buffer: PZRowBuffer);
begin
  if Assigned(Buffer) then
  begin
    ClearBuffer(Buffer);
    FreeMem(Buffer, GetRowSize);
  end;
end;

{**
  Initializes the row buffer.
  @param Buffer a pointer to row buffer.
}
procedure TZRowAccessor.InitBuffer(Buffer: PZRowBuffer);
begin
  if Assigned(Buffer) then
    with Buffer^ do
    begin
      Index := 0;
      BookmarkFlag := 0;//bfCurrent;
      UpdateType := utUnmodified;
      FillChar(Columns, GetColumnsSize, 1);
    end;
end;

{**
  Copies the row buffer from source to destination row.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZRowAccessor.CopyBuffer(SrcBuffer: PZRowBuffer; DestBuffer: PZRowBuffer);
var
  I: Integer;
begin
  ClearBuffer(DestBuffer);
  with DestBuffer^ do
  begin
    Index := SrcBuffer^.Index;
    UpdateType := SrcBuffer^.UpdateType;
    BookmarkFlag := SrcBuffer^.BookmarkFlag;
    System.Move(SrcBuffer^.Columns, Columns, GetColumnsSize);
    for I := 0 to FColumnCount - 1 do
    begin
      if (FColumnTypes[I] in [stAsciiStream, stUnicodeStream, stBinaryStream])
        and (Columns[FColumnOffsets[I]] = 0) then
      begin
        Columns[FColumnOffsets[I]] := 1;
        SetBlobObject(DestBuffer, I + 1, GetBlobObject(SrcBuffer, I + 1));
      end;
    end;
  end;
end;

{**
  Moves the row buffer from source to destination row.
  Source buffer is cleaned up after the operation.
  @param SrcBuffer a pointer to source row buffer.
  @param DestBuffer a pointer to destination row buffer.
}
procedure TZRowAccessor.MoveBuffer(SrcBuffer: PZRowBuffer;
  DestBuffer: PZRowBuffer);
begin
  CopyBuffer(SrcBuffer, DestBuffer);
  ClearBuffer(SrcBuffer);
end;

{**
  Cleans the specified row buffer.
  @param Buffer a pointer to row buffer.
}
procedure TZRowAccessor.ClearBuffer(Buffer: PZRowBuffer);
var
  I: Integer;
begin
  with Buffer^ do
  begin
    Index := -1;
    UpdateType := utUnmodified;
    BookmarkFlag := 0;
    for I := 0 to FColumnCount - 1 do
    begin
      if (FColumnTypes[I] in [stAsciiStream, stUnicodeStream, stBinaryStream])
        and (Columns[FColumnOffsets[I]] = 0) then
        SetBlobObject(Buffer, I + 1, nil);
    end;
    FillChar(Columns, GetColumnsSize, 1);
  end;
end;

{**
  Allocates a new row buffer.
  @return a pointer to the allocated buffer.
}
function TZRowAccessor.Alloc: PZRowBuffer;
begin
  Result := AllocBuffer(FBuffer);
end;

{**
  Disposes an associated row buffer.
}
procedure TZRowAccessor.Dispose;
begin
  DisposeBuffer(FBuffer);
  FBuffer := nil;
end;

{**
  Initializes the associated row buffer.
}
procedure TZRowAccessor.Init;
begin
  InitBuffer(FBuffer);
end;

{**
  Copies the associated row buffer into a specified one.
  @param DestBuffer a destination row buffer.
}
procedure TZRowAccessor.CopyTo(DestBuffer: PZRowBuffer);
begin
  CopyBuffer(FBuffer, DestBuffer);
end;

{**
  Copies the associated row buffer from a specified one.
  @param SrcBuffer a source row buffer.
}
procedure TZRowAccessor.CopyFrom(SrcBuffer: PZRowBuffer);
begin
  CopyBuffer(SrcBuffer, FBuffer);
end;

{**
  Moves the associated row buffer into a specified one.
  @param DestBuffer a destination row buffer.
}
procedure TZRowAccessor.MoveTo(DestBuffer: PZRowBuffer);
begin
  MoveBuffer(FBuffer, DestBuffer);
end;

{**
  Moves the associated row buffer from a specified one.
  @param SrcBuffer a source row buffer.
}
procedure TZRowAccessor.MoveFrom(SrcBuffer: PZRowBuffer);
begin
  MoveBuffer(SrcBuffer, FBuffer);
end;

{**
  Cleans the associated row buffer.
}
procedure TZRowAccessor.Clear;
begin
  ClearBuffer(FBuffer);
end;

{**
  Gets a pointer to the column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a pointer to the column data buffer.
}
function TZRowAccessor.GetColumnData(ColumnIndex: Integer;
  var IsNull: Boolean): Pointer;
begin
  CheckColumnConvertion(ColumnIndex, stString);
  Result := @FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1];
  IsNull := FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 1;
end;

{**
  Gets a size of the column data buffer.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a size of the column data buffer.
}
function TZRowAccessor.GetColumnDataSize(ColumnIndex: Integer): Integer;
begin
  CheckColumnConvertion(ColumnIndex, stString);
  Result := FColumnLengths[ColumnIndex - 1];
end;

//======================================================================
// Methods for accessing results by column index
//======================================================================

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZRowAccessor.IsNull(ColumnIndex: Integer): Boolean;
begin
  CheckColumnConvertion(ColumnIndex, stString);
  Result := FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 1;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetPChar(ColumnIndex: Integer;
  var IsNull: Boolean): PChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := nil;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stString:
        Result := @FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1];
      else
        Result := PChar(GetString(ColumnIndex, IsNull));
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetString(ColumnIndex: Integer;
  var IsNull: Boolean): string;
var
  TempBytes: TByteDynArray;
  TempStream: TStringStream;
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
  TempBytes := nil;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 'True'
        else Result := 'False';
      stByte: Result := IntToStr(GetByte(ColumnIndex, IsNull));
      stShort: Result := IntToStr(GetShort(ColumnIndex, IsNull));
      stInteger: Result := IntToStr(GetInt(ColumnIndex, IsNull));
      stLong: Result := IntToStr(GetLong(ColumnIndex, IsNull));
      stFloat: Result := FloatToStr(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := FloatToStr(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := IntToStr(GetBigDecimal(ColumnIndex, IsNull));
      stString:
        Result := PChar(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
      stUnicodeString: Result := GetUnicodeString(ColumnIndex, IsNull);
      stBytes: Result := Bytes2Str(GetBytes(ColumnIndex, IsNull));
      stDate: Result := FormatDateTime('yyyy-mm-dd', GetDate(ColumnIndex, IsNull));
      stTime: Result := FormatDateTime('hh:mm:ss', GetTime(ColumnIndex, IsNull));
      stTimestamp:
        Result := FormatDateTime('yyyy-mm-dd hh:mm:ss',
          GetTimestamp(ColumnIndex, IsNull));
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempStream := TStringStream.Create('');
          try
            TempBlob := GetBlobObject(FBuffer, ColumnIndex);
            if (TempBlob <> nil) and not TempBlob.IsEmpty then
              TempStream.CopyFrom(TempBlob.GetStream, 0);
            Result := TempStream.DataString;
          finally
            TempStream.Free;
          end;
        end;
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString</code> in the ObjectPascal programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetUnicodeString(ColumnIndex: Integer;
  var IsNull: Boolean): WideString;
var
  TempBytes: TByteDynArray;
  TempStream: TStringStream;
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  Result := '';
  TempBytes := nil;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stUnicodeString:
        Result := StrPas(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1]);
      stUnicodeStream:
        begin
          TempStream := TStringStream.Create('');
          try
            TempBlob := GetBlobObject(FBuffer, ColumnIndex);
            if (TempBlob <> nil) and not TempBlob.IsEmpty then
              TempStream.CopyFrom(TempBlob.GetStream, 0);
            Result := TempStream.DataString;
          finally
            TempStream.Free;
          end;
        end;
      else
        Result := GetString(ColumnIndex, IsNull);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZRowAccessor.GetBoolean(ColumnIndex: Integer;
  var IsNull: Boolean): Boolean;
var
  TempStr: string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Result := False;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        Result := PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stByte: Result := GetByte(ColumnIndex, IsNull) <> 0;
      stShort: Result := GetShort(ColumnIndex, IsNull) <> 0;
      stInteger: Result := GetInt(ColumnIndex, IsNull) <> 0;
      stLong: Result := GetLong(ColumnIndex, IsNull) <> 0;
      stFloat: Result := GetFloat(ColumnIndex, IsNull) <> 0;
      stDouble: Result := GetDouble(ColumnIndex, IsNull) <> 0;
      stBigDecimal: Result := GetBigDecimal(ColumnIndex, IsNull) <> 0;
      stString, stUnicodeString:
        begin
          TempStr := UpperCase(GetString(ColumnIndex, IsNull));
          Result := (TempStr = 'T') or (TempStr = 'Y') or (TempStr = 'TRUE')
            or (TempStr = 'YES');
        end;
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetByte(ColumnIndex: Integer;
  var IsNull: Boolean): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte:
        Result := PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stShort: Result := GetShort(ColumnIndex, IsNull);
      stInteger: Result := GetInt(ColumnIndex, IsNull);
      stLong: Result := GetLong(ColumnIndex, IsNull);
      stFloat: Result := Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := GetBigDecimal(ColumnIndex, IsNull);
      stString, stUnicodeString:
        Result := StrToIntDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetShort(ColumnIndex: Integer;
  var IsNull: Boolean): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte: Result := GetByte(ColumnIndex, IsNull);
      stShort:
        Result := PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stInteger: Result := GetInt(ColumnIndex, IsNull);
      stLong: Result := GetLong(ColumnIndex, IsNull);
      stFloat: Result := Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := GetBigDecimal(ColumnIndex, IsNull);
      stString, stUnicodeString:
        Result := StrToIntDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetInt(ColumnIndex: Integer;
  var IsNull: Boolean): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte: Result := GetByte(ColumnIndex, IsNull);
      stShort: Result := GetShort(ColumnIndex, IsNull);
      stInteger:
        Result := PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stLong: Result := GetLong(ColumnIndex, IsNull);
      stFloat: Result := Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := GetBigDecimal(ColumnIndex, IsNull);
      stString, stUnicodeString:
        Result := StrToIntDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetLong(ColumnIndex: Integer;
  var IsNull: Boolean): LongInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte: Result := GetByte(ColumnIndex, IsNull);
      stShort: Result := GetShort(ColumnIndex, IsNull);
      stInteger: Result := GetInt(ColumnIndex, IsNull);
      stLong:
        Result := PLongInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stFloat: Result := Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal: Result := GetBigDecimal(ColumnIndex, IsNull);
      stString, stUnicodeString:
        Result := StrToIntDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetFloat(ColumnIndex: Integer;
  var IsNull: Boolean): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte: Result := GetByte(ColumnIndex, IsNull);
      stShort: Result := GetShort(ColumnIndex, IsNull);
      stInteger: Result := GetInt(ColumnIndex, IsNull);
      stLong: Result := GetLong(ColumnIndex, IsNull);
      stFloat:
        Result := PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stDouble: Result := GetDouble(ColumnIndex, IsNull);
      stBigDecimal: Result := GetBigDecimal(ColumnIndex, IsNull);
      stString, stUnicodeString:
        Result := StrToFloatDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZRowAccessor.GetDouble(ColumnIndex: Integer;
  var IsNull: Boolean): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte: Result := GetByte(ColumnIndex, IsNull);
      stShort: Result := GetShort(ColumnIndex, IsNull);
      stInteger: Result := GetInt(ColumnIndex, IsNull);
      stLong: Result := GetLong(ColumnIndex, IsNull);
      stFloat: Result := GetFloat(ColumnIndex, IsNull);
      stDouble:
        Result := PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stBigDecimal: Result := GetBigDecimal(ColumnIndex, IsNull);
      stString, stUnicodeString:
        Result := StrToFloatDef(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
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
function TZRowAccessor.GetBigDecimal(ColumnIndex: Integer;
  var IsNull: Boolean): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBoolean:
        if GetBoolean(ColumnIndex, IsNull) then
          Result := 1
        else Result := 0;
      stByte: Result := GetByte(ColumnIndex, IsNull);
      stShort: Result := GetShort(ColumnIndex, IsNull);
      stInteger: Result := GetInt(ColumnIndex, IsNull);
      stLong: Result := GetLong(ColumnIndex, IsNull);
      stFloat: Result := Trunc(GetFloat(ColumnIndex, IsNull));
      stDouble: Result := Trunc(GetDouble(ColumnIndex, IsNull));
      stBigDecimal:
        Result := PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stString, stUnicodeString:
        Result := StrToInt64Def(GetString(ColumnIndex, IsNull), 0);
    end;
    IsNull := False;
  end else
    IsNull := True;
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
function TZRowAccessor.GetBytes(ColumnIndex: Integer;
  var IsNull: Boolean): TByteDynArray;
var
  I: Integer;
  TempShort: SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := nil;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBytes:
        begin
          TempShort := PSmallInt(@FBuffer.Columns[
            FColumnOffsets[ColumnIndex - 1] + 1])^;
          SetLength(Result, TempShort);
          for I := 0 to TempShort - 1 do
          begin
            Result[I] := FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]
              + 1 + SizeOf(SmallInt) + I];
          end;
        end;
      else
        Result := Str2Bytes(GetString(ColumnIndex, IsNull));
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetDate(ColumnIndex: Integer;
  var IsNull: Boolean): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stDate, stTime, stTimestamp:
        Result := Int(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
      stString, stUnicodeString:
        Result := Trunc(AnsiSQLDateToDateTime(GetString(ColumnIndex, IsNull)));
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZRowAccessor.GetTime(ColumnIndex: Integer;
  var IsNull: Boolean): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stDate, stTime, stTimestamp:
        Result := Frac(PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^);
      stString, stUnicodeString:
        Result := Frac(AnsiSQLDateToDateTime(GetString(ColumnIndex, IsNull)));
    end;
    IsNull := False;
  end else
    IsNull := True;
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
function TZRowAccessor.GetTimestamp(ColumnIndex: Integer;
  var IsNull: Boolean): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Result := 0;
  if FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0 then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stDate, stTime, stTimestamp:
        Result := PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^;
      stString, stUnicodeString:
        Result := AnsiSQLDateToDateTime(GetString(ColumnIndex, IsNull));
    end;
    IsNull := False;
  end else
    IsNull := True;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a stream of ASCII characters. The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <char>LONGVARCHAR</char> values.
  The JDBC driver will
  do any necessary conversion from the database format into ASCII.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream.  Also, a
  stream may return <code>0</code> when the method
  <code>InputStream.available</code>
  is called whether there is data available or not.

  @param columnIndex the first column is 1, the second is 2, ...
  @return a Java input stream that delivers the database column value
    as a stream of one-byte ASCII characters; if the value is SQL
    <code>NULL</code>, the value returned is <code>null</code>
}
function TZRowAccessor.GetAsciiStream(ColumnIndex: Integer;
  var IsNull: Boolean): TStream;
var
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stAsciiStream);
{$ENDIF}
  TempBlob := GetBlobObject(FBuffer, ColumnIndex);
  if (TempBlob <> nil) and not TempBlob.IsEmpty then
    Result := TempBlob.GetStream
  else Result := nil;
  IsNull := Result = nil;
end;

{**
  Gets the value of a column in the current row as a stream of
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  as a stream of Unicode characters.
  The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large<code>LONGVARCHAR</code>values.  The JDBC driver will
  do any necessary conversion from the database format into Unicode.
  The byte format of the Unicode stream must be Java UTF-8,
  as specified in the Java virtual machine specification.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream.  Also, a
  stream may return <code>0</code> when the method
  <code>InputStream.available</code>
  is called whether there is data available or not.

  @param columnIndex the first column is 1, the second is 2, ...
  @return a Java input stream that delivers the database column value
    as a stream in Java UTF-8 byte format; if the value is SQL
    <code>NULL</code>, the value returned is <code>null</code>
}
function TZRowAccessor.GetUnicodeStream(ColumnIndex: Integer;
  var IsNull: Boolean): TStream;
var
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  TempBlob := GetBlobObject(FBuffer, ColumnIndex);
  if (TempBlob <> nil) and not TempBlob.IsEmpty then
    Result := TempBlob.GetStream
  else Result := nil;
  IsNull := Result = nil;
end;

{**
  Gets the value of a column in the current row as a stream of
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a binary stream of
  uninterpreted bytes. The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <code>LONGVARBINARY</code> values.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream.  Also, a
  stream may return <code>0</code> when the method
  <code>InputStream.available</code>
  is called whether there is data available or not.

  @param columnIndex the first column is 1, the second is 2, ...
  @return a Java input stream that delivers the database column value
    as a stream of uninterpreted bytes;
    if the value is SQL <code>NULL</code>, the value returned is <code>null</code>
}
function TZRowAccessor.GetBinaryStream(ColumnIndex: Integer;
  var IsNull: Boolean): TStream;
var
  TempBlob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBinaryStream);
{$ENDIF}
  TempBlob := GetBlobObject(FBuffer, ColumnIndex);
  if (TempBlob <> nil) and not TempBlob.IsEmpty then
    Result := TempBlob.GetStream
  else Result := nil;
  IsNull := Result = nil;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZRowAccessor.GetBlob(ColumnIndex: Integer;
  var IsNull: Boolean): IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
  if not (FColumnTypes[ColumnIndex - 1] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    raise EZSQLException.Create(
      Format('Can not access blob record in column %d with type %s.',
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex - 1])]));
  end;
{$ENDIF}

  Result := GetBlobObject(FBuffer, ColumnIndex);
  IsNull := Result = nil;
  if Result = nil then
  begin
    Result := TZAbstractBlob.CreateWithStream(nil);
    SetBlobObject(FBuffer, ColumnIndex, Result);
  end;
end;

//---------------------------------------------------------------------
// Updates
//---------------------------------------------------------------------

{**
  Gives a not nullable column a null value.

  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code>
  or <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZRowAccessor.SetNotNull(ColumnIndex: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if (FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 1)
    and (FColumnTypes[ColumnIndex - 1] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    SetBlobObject(FBuffer, ColumnIndex, nil);
  end;
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
end;

{**
  Gives a nullable column a null value.

  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code>
  or <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZRowAccessor.SetNull(ColumnIndex: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  if (FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] = 0)
    and (FColumnTypes[ColumnIndex - 1] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    SetBlobObject(FBuffer, ColumnIndex, nil);
  end;
  FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 1;
end;

{**
  Sets the designated column with a <code>boolean</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetBoolean(ColumnIndex: Integer; Value: Boolean);
var
  TempInt: Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  if Value then TempInt := 1
  else TempInt := 0;

  case FColumnTypes[ColumnIndex - 1] of
    stBoolean:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PWordBool(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stByte: SetByte(ColumnIndex, TempInt);
    stShort: SetShort(ColumnIndex, TempInt);
    stInteger: SetInt(ColumnIndex, TempInt);
    stLong: SetLong(ColumnIndex, TempInt);
    stFloat: SetFloat(ColumnIndex, TempInt);
    stDouble: SetDouble(ColumnIndex, TempInt);
    stBigDecimal: SetBigDecimal(ColumnIndex, TempInt);
    stString, stUnicodeString:
      if Value then SetString(ColumnIndex, 'True')
      else SetString(ColumnIndex, 'False');
  end;
end;

{**
  Sets the designated column with a <code>byte</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.


  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetByte(ColumnIndex: Integer;
  Value: ShortInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PShortInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stShort: SetShort(ColumnIndex, Value);
    stInteger: SetInt(ColumnIndex, Value);
    stLong: SetLong(ColumnIndex, Value);
    stFloat: SetFloat(ColumnIndex, Value);
    stDouble: SetDouble(ColumnIndex, Value);
    stBigDecimal: SetBigDecimal(ColumnIndex, Value);
    stString, stUnicodeString:
      SetString(ColumnIndex, IntToStr(Value));
  end;
end;

{**
  Sets the designated column with a <code>short</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetShort(ColumnIndex: Integer; Value: SmallInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte: SetByte(ColumnIndex, Value);
    stShort:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stInteger: SetInt(ColumnIndex, Value);
    stLong: SetLong(ColumnIndex, Value);
    stFloat: SetFloat(ColumnIndex, Value);
    stDouble: SetDouble(ColumnIndex, Value);
    stBigDecimal: SetBigDecimal(ColumnIndex, Value);
    stString, stUnicodeString:
      SetString(ColumnIndex, IntToStr(Value));
  end;
end;

{**
  Sets the designated column with an <code>int</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetInt(ColumnIndex: Integer; Value: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte: SetByte(ColumnIndex, Value);
    stShort: SetShort(ColumnIndex, Value);
    stInteger:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PInteger(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stLong: SetLong(ColumnIndex, Value);
    stFloat: SetFloat(ColumnIndex, Value);
    stDouble: SetDouble(ColumnIndex, Value);
    stBigDecimal: SetBigDecimal(ColumnIndex, Value);
    stString, stUnicodeString:
      SetString(ColumnIndex, IntToStr(Value));
  end;
end;

{**
  Sets the designated column with a <code>long</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetLong(ColumnIndex: Integer; Value: LongInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte: SetByte(ColumnIndex, Value);
    stShort: SetShort(ColumnIndex, Value);
    stInteger: SetInt(ColumnIndex, Value);
    stLong:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PLongInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stFloat: SetFloat(ColumnIndex, Value);
    stDouble: SetDouble(ColumnIndex, Value);
    stBigDecimal: SetBigDecimal(ColumnIndex, Value);
    stString, stUnicodeString:
      SetString(ColumnIndex, IntToStr(Value));
  end;
end;

{**
  Sets the designated column with a <code>float</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetFloat(ColumnIndex: Integer; Value: Single);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte: SetByte(ColumnIndex, Trunc(Value));
    stShort: SetShort(ColumnIndex, Trunc(Value));
    stInteger: SetInt(ColumnIndex, Trunc(Value));
    stLong: SetLong(ColumnIndex, Trunc(Value));
    stFloat:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PSingle(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stDouble: SetDouble(ColumnIndex, Value);
    stBigDecimal: SetBigDecimal(ColumnIndex, Trunc(Value));
    stString, stUnicodeString:
      SetString(ColumnIndex, FloatToStr(Value));
  end;
end;

{**
  Sets the designated column with a <code>double</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetDouble(ColumnIndex: Integer; Value: Double);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte: SetByte(ColumnIndex, Trunc(Value));
    stShort: SetShort(ColumnIndex, Trunc(Value));
    stInteger: SetInt(ColumnIndex, Trunc(Value));
    stLong: SetLong(ColumnIndex, Trunc(Value));
    stFloat: SetFloat(ColumnIndex, Value);
    stDouble:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PDouble(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stBigDecimal: SetBigDecimal(ColumnIndex, Trunc(Value));
    stString, stUnicodeString:
      SetString(ColumnIndex, FloatToStr(Value));
  end;
end;

{**
  Sets the designated column with a <code>java.math.BigDecimal</code>
  value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetBigDecimal(ColumnIndex: Integer; Value: Int64);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean: SetBoolean(ColumnIndex, Value <> 0);
    stByte: SetByte(ColumnIndex, Value);
    stShort: SetShort(ColumnIndex, Value);
    stInteger: SetInt(ColumnIndex, Value);
    stLong: SetLong(ColumnIndex, Value);
    stFloat: SetFloat(ColumnIndex, Value);
    stDouble: SetDouble(ColumnIndex, Value);
    stBigDecimal:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PInt64(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stString, stUnicodeString:
      SetString(ColumnIndex, IntToStr(Value));
  end;
end;

{**
  Sets the designated column with a <code>String</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetPChar(ColumnIndex: Integer; Value: PChar);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stString:
      begin
        if Value <> nil then
        begin
          FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
          StrLCopy(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1], Value,
            FColumnLengths[ColumnIndex - 1] - 1);
        end else
          FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 1;
      end;
    else
      SetString(ColumnIndex, Value);
  end;
end;

{**
  Sets the designated column with a <code>String</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetString(ColumnIndex: Integer; Value: string);
var
  TempStr: string;
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stBoolean:
      begin
        TempStr := UpperCase(Value);
        SetBoolean(ColumnIndex, (TempStr = 'Y') or (TempStr = 'T')
          or (TempStr = 'YES') or (TempStr = 'TRUE'));
      end;
    stByte: SetByte(ColumnIndex, StrToIntDef(Value, 0));
    stShort: SetShort(ColumnIndex, StrToIntDef(Value, 0));
    stInteger: SetInt(ColumnIndex, StrToIntDef(Value, 0));
    stLong: SetLong(ColumnIndex, StrToIntDef(Value, 0));
    stFloat: SetFloat(ColumnIndex, StrToFloatDef(Value, 0));
    stDouble: SetDouble(ColumnIndex, StrToFloatDef(Value, 0));
    stBigDecimal: SetBigDecimal(ColumnIndex, StrToInt64Def(Value, 0));
    stString:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        StrPLCopy(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1], Value,
          FColumnLengths[ColumnIndex - 1] - 1);
      end;
    stUnicodeString: SetUnicodeString(ColumnIndex, Value);
    stBytes: SetBytes(ColumnIndex, Str2Bytes(Value));
    stDate: SetDate(ColumnIndex, AnsiSQLDateToDateTime(Value));
    stTime: SetTime(ColumnIndex, AnsiSQLDateToDateTime(Value));
    stTimestamp: SetTimestamp(ColumnIndex, AnsiSQLDateToDateTime(Value));
    stAsciiStream, stUnicodeStream, stBinaryStream:
      GetBlob(ColumnIndex, IsNull).SetStream(TStringStream.Create(Value));
  end;
end;

{**
  Sets the designated column with a <code>WideString</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetUnicodeString(ColumnIndex: Integer;
  Value: WideString);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stUnicodeString:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        Value := System.Copy(Value, 1, FColumnLengths[ColumnIndex - 1] - 1);
        StrPCopy(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1], Value);
      end;
    else
      SetString(ColumnIndex, Value);
  end;
end;

{**
  Sets the designated column with a <code>byte</code> array value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetBytes(ColumnIndex: Integer; Value: TByteDynArray);
var
  I: Integer;
  TempShort: SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  if Value <> nil then
  begin
    case FColumnTypes[ColumnIndex - 1] of
      stBytes:
        begin
          FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
          TempShort := MinIntValue([High(Value) + 1, FColumnLengths[ColumnIndex - 1]
            - SizeOf(SmallInt)]);
          PSmallInt(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
            TempShort;
          for I := 0 to TempShort - 1 do
          begin
            FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1
              + SizeOf(SmallInt) + I] := Value[I];
          end;
        end;
      else
        SetString(ColumnIndex, Bytes2Str(Value));
    end;
  end else
    SetNull(ColumnIndex);
end;

{**
  Sets the designated column with a <code>java.sql.Date</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetDate(ColumnIndex: Integer; Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stDate:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          Trunc(Value);
      end;
    stTimestamp: SetTimestamp(ColumnIndex, Trunc(Value));
    stString, stUnicodeString:
      SetString(ColumnIndex, FormatDateTime('yyyy-mm-dd', Value));
  end;
end;

{**
  Sets the designated column with a <code>java.sql.Time</code> value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetTime(ColumnIndex: Integer; Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stTime:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ :=
          Frac(Value);
      end;
    stTimestamp: SetTimestamp(ColumnIndex, Frac(Value));
    stString, stUnicodeString:
      SetString(ColumnIndex, FormatDateTime('hh:nn:ss', Value));
  end;
end;

{**
  Sets the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetTimestamp(ColumnIndex: Integer; Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  case FColumnTypes[ColumnIndex - 1] of
    stDate: SetDate(ColumnIndex, Value);
    stTime: SetTime(ColumnIndex, Value);
    stTimestamp:
      begin
        FBuffer.Columns[FColumnOffsets[ColumnIndex - 1]] := 0;
        PDateTime(@FBuffer.Columns[FColumnOffsets[ColumnIndex - 1] + 1])^ := Value;
      end;
    stString, stUnicodeString:
      SetString(ColumnIndex, FormatDateTime('yyyy-mm-dd hh:nn:ss', Value));
  end;
end;

{**
  Sets the designated column with an ascii stream value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetAsciiStream(ColumnIndex: Integer; Value: TStream);
var
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stAsciiStream);
{$ENDIF}
  GetBlob(ColumnIndex, IsNull).SetStream(Value);
end;

{**
  Sets the designated column with a binary stream value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
  @param length the length of the stream
}
procedure TZRowAccessor.SetBinaryStream(ColumnIndex: Integer; Value: TStream);
var
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBinaryStream);
{$ENDIF}
  GetBlob(ColumnIndex, IsNull).SetStream(Value);
end;

{**
  Sets the designated column with a character stream value.
  The <code>SetXXX</code> methods are used to Set column values in the
  current row or the insert row.  The <code>SetXXX</code> methods do not
  Set the underlying database; instead the <code>SetRow</code> or
  <code>insertRow</code> methods are called to Set the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZRowAccessor.SetUnicodeStream(ColumnIndex: Integer;
  Value: TStream);
var
  IsNull: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  GetBlob(ColumnIndex, IsNull).SetStream(Value);
end;

{**
  Sets the blob wrapper object to the specified column.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @param Value a blob wrapper object to be set.
}
procedure TZRowAccessor.SetBlob(ColumnIndex: Integer; Value: IZBlob);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnIndex(ColumnIndex);
  if not (FColumnTypes[ColumnIndex - 1] in [stAsciiStream, stBinaryStream,
    stUnicodeStream]) then
  begin
    raise EZSQLException.Create(
      Format('Can not access blob record in column %d with type %s.',
      [ColumnIndex, DefineColumnTypeName(FColumnTypes[ColumnIndex - 1])]));
  end;
{$ENDIF}

  SetBlobObject(FBuffer, ColumnIndex, Value);
end;

end.
