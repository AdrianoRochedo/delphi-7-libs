{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Dataset utility functions and classes            }
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

unit ZDatasetUtils;

interface

{$I ZComponent.inc}

uses
  Classes, SysUtils, Db, ZSysUtils, ZDbcIntfs, ZClasses, ZDbcCache,
  ZCompatibility;

{**
  Converts DBC Field Type to TDataset Field Type.
  @param Value an initial DBC field type.
  @return a converted TDataset field type.
}
function ConvertDbcToDatasetType(Value: TZSQLType): TFieldType;

{**
  Converts TDataset Field Type to DBC Field Type.
  @param Value an initial TDataset field type.
  @return a converted DBC field type.
}
function ConvertDatasetToDbcType(Value: TFieldType): TZSQLType;

{**
  Converts field definitions into column information objects.
  @param Fields a collection of field definitions.
  @return a collection of column information objects.
}
function ConvertFieldsToColumnInfo(Fields: TFields): IZCollection;

{**
  Fetches columns from specified resultset.
  @param ResultSet a source resultset.
  @param Fields a collection of field definitions.
  @param RowAccessor a destination row accessor.
}
procedure FetchFromResultSet(ResultSet: IZResultSet; Fields: TFields;
  RowAccessor: TZRowAccessor);

{**
  Posts columns from specified resultset.
  @param ResultSet a source resultset.
  @param Fields a collection of field definitions.
  @param RowAccessor a destination row accessor.
}
procedure PostToResultSet(ResultSet: IZResultSet; Fields: TFields;
  RowAccessor: TZRowAccessor);

{**
  Defines fields indices for the specified dataset.
  @param DataSet a dataset object.
  @param FieldNames a list of field names.
  @param OnlyDataFields <code>True</code> if only data fields selected.
}
function DefineFieldsIndices(DataSet: TDataset; FieldNames: string;
  var OnlyDataFields: Boolean): TIntegerDynArray;

{**
  Retrieves a set of specified field values.
  @param Fields a collection of dataset fields.
  @param FieldIndices an array with interested field indices.
  @param ResultSet an initial result set object.
  @return an array with field values.
}
function RetrieveDataFields(Fields: TFields; FieldIndices: TIntegerDynArray;
  ResultSet: IZResultSet): Variant; overload;

{**
  Retrieves a set of specified field values.
  @param Fields a collection of dataset fields.
  @param FieldIndices an array with interested field indices.
  @param RowAccessor a row accessor object.
  @return an array with field values.
}
function RetrieveDataFields(Fields: TFields; FieldIndices: TIntegerDynArray;
  RowAccessor: TZRowAccessor): Variant; overload;

{**
  Compares row field values with the given ones.
  @param KeyValues given values.
  @param RowValues row field values.
  @param Options compare options.
  @return <code> if values are equal.
}
function CompareDataFields(KeyValues, RowValues: Variant;
  Options: TLocateOptions): Boolean;

{**
  Defines a list of key field names.
  @param Fields a collection of dataset fields.
  @return a list of key field names.
}
function DefineKeyFields(Fields: TFields): string;

{**
  Fill prepared statement with parameters.
  @param Statement a prepared SQL statement.
  @param ParamNames an array of parameter names.
  @param Params a collection of SQL parameters.
}
procedure FillStatementWithParams(Statement: IZPreparedStatement;
  ParamNames: TStringDynArray; Params: TParams);

{**
  Converts datetime value into TDataset internal presentation.
  @param DataType a type of date-time field.
  @param Data a data which contains a value.
  @param Buffer a field buffer pointer
}
procedure DateTimeToNative(DataType: TFieldType; Data: TDateTime; Buffer: Pointer);

{**
  Converts date times from TDataset internal presentation into datetime value.
  @param DataType a type of date-time field.
  @param Buffer a field buffer pointer
  @return a data which contains a value.
}
function NativeToDateTime(DataType: TFieldType; Buffer: Pointer): TDateTime;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZCollections, ZDbcResultSet, ZParseIntfs, ZTokenizer, ZDbcResultSetMetadata;

{**
  Converts DBC Field Type to TDataset Field Type.
  @param Value an initial DBC field type.
  @return a converted TDataset field type.
}
function ConvertDbcToDatasetType(Value: TZSQLType): TFieldType;
begin
  case Value of
    stBoolean:
      Result := ftBoolean;
    stByte, stShort:
      Result := ftSmallInt;
    stInteger, stLong:
      Result := ftInteger;
    stFloat, stDouble:
      Result := ftFloat;
    stBigDecimal:
      Result := ftLargeInt;
    stString:
      Result := ftString;
    stBytes:
      Result := ftBytes;
    stDate:
      Result := ftDate;
    stTime:
      Result := ftTime;
    stTimestamp:
      Result := ftDateTime;
    stAsciiStream, stUnicodeStream:
      Result := ftMemo;
    stBinaryStream:
      Result := ftBlob;
    else
      Result := ftUnknown;
  end;
end;

{**
  Converts TDataset Field Type to DBC Field Type.
  @param Value an initial TDataset field type.
  @return a converted DBC field type.
}
function ConvertDatasetToDbcType(Value: TFieldType): TZSQLType;
begin
  case Value of
    ftBoolean:
      Result := stBoolean;
    ftSmallInt:
      Result := stShort;
    ftInteger:
      Result := stInteger;
    ftFloat:
      Result := stDouble;
    ftCurrency:
      Result := stDouble;
    ftLargeInt:
      Result := stLong;
    ftString:
      Result := stString;
    ftBytes:
      Result := stBytes;
    ftDate:
      Result := stDate;
    ftTime:
      Result := stTime;
    ftDateTime:
      Result := stTimestamp;
    ftMemo:
      Result := stAsciiStream;
    ftBlob:
      Result := stBinaryStream;
    else
      Result := stUnknown;
  end;
end;

{**
  Converts field definitions into column information objects.
  @param Fields a collection of field definitions.
  @return a collection of column information objects.
}
function ConvertFieldsToColumnInfo(Fields: TFields): IZCollection;
var
  I: Integer;
  Current: TField;
  ColumnInfo: TZColumnInfo;
begin
  Result := TZCollection.Create;
  for I := 0 to Fields.Count - 1 do
  begin
    Current := Fields[I];
    ColumnInfo := TZColumnInfo.Create;

    ColumnInfo.SetColumnType(ConvertDatasetToDbcType(Current.DataType));
    ColumnInfo.SetColumnName(Current.FieldName);
    ColumnInfo.SetPrecision(Current.Size);
    ColumnInfo.SetScale(0);
    ColumnInfo.SetColumnLabel(Current.DisplayName);

    Result.Add(ColumnInfo);
  end;
end;

{**
  Fetches columns from specified resultset.
  @param ResultSet a source resultset.
  @param Fields a collection of field definitions.
  @param RowAccessor a destination row accessor.
}
procedure FetchFromResultSet(ResultSet: IZResultSet; Fields: TFields;
  RowAccessor: TZRowAccessor);
var
  I: Integer;
  Current: TField;
  ColumnIndex, ColumnCount: Integer;
begin
  RowAccessor.RowBuffer.Index := ResultSet.GetRow;
  ColumnCount := ResultSet.GetMetadata.GetColumnCount;

  for I := 1 to Fields.Count do
  begin
    Current := Fields[I - 1];
    if Current.FieldKind <> fkData then
      Continue;

    ColumnIndex := Current.FieldNo;
    if (ColumnIndex < 1) or (ColumnIndex > ColumnCount) then
      Continue;

    case Current.DataType of
      ftBoolean:
        RowAccessor.SetBoolean(I, ResultSet.GetBoolean(ColumnIndex));
      ftSmallInt:
        RowAccessor.SetShort(I, ResultSet.GetShort(ColumnIndex));
      ftInteger:
        RowAccessor.SetInt(I, ResultSet.GetInt(ColumnIndex));
      ftFloat:
        RowAccessor.SetDouble(I, ResultSet.GetDouble(ColumnIndex));
      ftLargeInt:
        RowAccessor.SetBigDecimal(I, ResultSet.GetBigDecimal(ColumnIndex));
      ftCurrency:
        RowAccessor.SetDouble(I, ResultSet.GetDouble(ColumnIndex));
      ftString:
        RowAccessor.SetPChar(I, ResultSet.GetPChar(ColumnIndex));
      ftBytes:
        RowAccessor.SetBytes(I, ResultSet.GetBytes(ColumnIndex));
      ftDate:
        RowAccessor.SetDate(I, ResultSet.GetDate(ColumnIndex));
      ftTime:
        RowAccessor.SetTime(I, ResultSet.GetTime(ColumnIndex));
      ftDateTime:
        RowAccessor.SetTimestamp(I, ResultSet.GetTimestamp(ColumnIndex));
      ftMemo, ftBlob:
        RowAccessor.SetBlob(I, ResultSet.GetBlob(ColumnIndex));
    end;

    if ResultSet.WasNull then
      RowAccessor.SetNull(I);
  end;
end;

{**
  Posts columns from specified resultset.
  @param ResultSet a source resultset.
  @param Fields a collection of field definitions.
  @param RowAccessor a destination row accessor.
}
procedure PostToResultSet(ResultSet: IZResultSet; Fields: TFields;
  RowAccessor: TZRowAccessor);
var
  I: Integer;
  Current: TField;
  WasNull: Boolean;
  ColumnIndex, ColumnCount: Integer;
  Stream: TStream;
begin
  RowAccessor.RowBuffer.Index := ResultSet.GetRow;
  ColumnCount := ResultSet.GetMetadata.GetColumnCount;

  for I := 1 to Fields.Count do
  begin
    Current := Fields[I - 1];
    if Current.FieldKind <> fkData then
      Continue;

    ColumnIndex := Current.FieldNo;
    if (ColumnIndex < 1) or (ColumnIndex > ColumnCount) then
      Continue;

    case Current.DataType of
      ftBoolean:
        ResultSet.UpdateBoolean(ColumnIndex, RowAccessor.GetBoolean(I, WasNull));
      ftSmallInt:
        ResultSet.UpdateShort(ColumnIndex, RowAccessor.GetShort(I, WasNull));
      ftInteger:
        ResultSet.UpdateInt(ColumnIndex, RowAccessor.GetInt(I, WasNull));
      ftFloat:
        ResultSet.UpdateDouble(ColumnIndex, RowAccessor.GetDouble(I, WasNull));
      ftLargeInt:
        ResultSet.UpdateBigDecimal(ColumnIndex,
          RowAccessor.GetBigDecimal(I, WasNull));
      ftCurrency:
        ResultSet.UpdateDouble(ColumnIndex, RowAccessor.GetDouble(I, WasNull));
      ftString:
        ResultSet.UpdatePChar(ColumnIndex, RowAccessor.GetPChar(I, WasNull));
      ftBytes:
        ResultSet.UpdateBytes(ColumnIndex, RowAccessor.GetBytes(I, WasNull));
      ftDate:
        ResultSet.UpdateDate(ColumnIndex, RowAccessor.GetDate(I, WasNull));
      ftTime:
        ResultSet.UpdateTime(ColumnIndex, RowAccessor.GetTime(I, WasNull));
      ftDateTime:
        ResultSet.UpdateTimestamp(ColumnIndex,
          RowAccessor.GetTimestamp(I, WasNull));
      ftMemo:
        begin
          Stream := RowAccessor.GetAsciiStream(I, WasNull);
          try
            ResultSet.UpdateAsciiStream(ColumnIndex, Stream);
          finally
            Stream.Free;
          end;
        end;
      ftBlob:
        begin
          Stream := RowAccessor.GetBinaryStream(I, WasNull);
          try
            ResultSet.UpdateBinaryStream(ColumnIndex, Stream);
          finally
            Stream.Free;
          end;
        end;
    end;

    if WasNull then
      ResultSet.UpdateNull(ColumnIndex);
  end;
end;

{**
  Defines fields indices for the specified dataset.
  @param DataSet a dataset object.
  @param FieldNames a list of field names.
  @param OnlyDataFields <code>True</code> if only data fields selected.
}
function DefineFieldsIndices(DataSet: TDataset; FieldNames: string;
  var OnlyDataFields: Boolean): TIntegerDynArray;
var
  Tokenizer: IZTokenizer;
  Token: IZToken;
  Field: TField;
  FieldCount: Integer;
begin
  OnlyDataFields := True;
  FieldCount := 0;
  SetLength(Result, FieldCount);
  Tokenizer := TZTokenizer.CreateWithBuffer(FieldNames);
  Tokenizer.DefaultWordState.SetWordChars(' ', ' ', True);

  while True do
  begin
    Token := Tokenizer.NextToken;
    Field := nil;

    if Token.GetTokenType = TT_EOF then
      Break;

    if Token.GetTokenType = TT_WORD then
      Field := DataSet.FieldByName(Token.GetString)
    else if Token.GetTokenType = TT_QUOTED then
      Field := DataSet.FieldByName(AnsiDequotedStr(
        Token.GetString, Token.GetString[1]))
    else if ((Token.GetTokenType = TT_NUMBER)
      or (Token.GetTokenType = TT_INTEGER))
      and (Token.GetInteger < Dataset.Fields.Count) then
      Field := Dataset.Fields[Token.GetInteger]
    else if (Token.GetString <> ',') and (Token.GetString <> ';') then
      DatabaseError(Format('Incorrect symbol in field list "%s".',
        [Token.GetString]));

    if Field <> nil then
    begin
      OnlyDataFields := OnlyDataFields and (Field.FieldKind = fkData);
      Inc(FieldCount);
      SetLength(Result, FieldCount);
      Result[FieldCount - 1] := Field.Index;
    end;
  end;

  if Length(Result) = 0 then
    Result := nil;
end;

{**
  Retrieves a set of specified field values.
  @param Fields a collection of dataset fields.
  @param FieldIndices an array with interested field indices.
  @param ResultSet an initial result set object.
  @return an array with field values.
}
function RetrieveDataFields(Fields: TFields; FieldIndices: TIntegerDynArray;
  ResultSet: IZResultSet): Variant;
var
  I, ColumnIndex: Integer;
  Current: TField;
begin
  Result := VarArrayCreate([0, Length(FieldIndices) - 1], varVariant);
  for I := 0 to Length(FieldIndices) - 1 do
  begin
    Current := Fields[FieldIndices[I]];
    ColumnIndex := Current.FieldNo;
    if not ResultSet.IsNull(ColumnIndex) then
    begin
      case Current.DataType of
        ftBoolean:
          Result[I] := ResultSet.GetBoolean(ColumnIndex);
        ftSmallInt, ftInteger:
          Result[I] := ResultSet.GetInt(ColumnIndex);
        ftFloat, ftCurrency:
          Result[I] := ResultSet.GetDouble(ColumnIndex);
        ftLargeInt:
{$IFDEF VER130BELOW}
          Result[I] := Integer(ResultSet.GetBigDecimal(ColumnIndex));
{$ELSE}
          Result[I] := ResultSet.GetBigDecimal(ColumnIndex);
{$ENDIF}
        ftDate:
          Result[I] := ResultSet.GetDate(ColumnIndex);
        ftTime:
          Result[I] := ResultSet.GetTime(ColumnIndex);
        ftDateTime:
          Result[I] := ResultSet.GetTimestamp(ColumnIndex);
        else
          Result[I] := ResultSet.GetString(ColumnIndex);
      end;
    end else
      Result[I] := Null;
  end;
end;

{**
  Retrieves a set of specified field values.
  @param Fields a collection of dataset fields.
  @param FieldIndices an array with interested field indices.
  @param RowAccessor a row accessor object.
  @return an array with field values.
}
function RetrieveDataFields(Fields: TFields; FieldIndices: TIntegerDynArray;
  RowAccessor: TZRowAccessor): Variant; overload;
var
  I, ColumnIndex: Integer;
  WasNull: Boolean;
  Current: TField;
begin
  Result := VarArrayCreate([0, Length(FieldIndices) - 1], varVariant);
  for I := 0 to Length(FieldIndices) - 1 do
  begin
    Current := Fields[FieldIndices[I]];
    ColumnIndex := FieldIndices[I] + 1;
    case Current.DataType of
      ftBoolean:
        Result[I] := RowAccessor.GetBoolean(ColumnIndex, WasNull);
      ftSmallInt, ftInteger:
        Result[I] := RowAccessor.GetInt(ColumnIndex, WasNull);
      ftFloat, ftCurrency:
        Result[I] := RowAccessor.GetDouble(ColumnIndex, WasNull);
      ftLargeInt:
{$IFDEF VER130BELOW}
        Result[I] := Integer(RowAccessor.GetBigDecimal(ColumnIndex, WasNull));
{$ELSE}
        Result[I] := RowAccessor.GetBigDecimal(ColumnIndex, WasNull);
{$ENDIF}
      ftDate, ftTime, ftDateTime:
        Result[I] := RowAccessor.GetTimestamp(ColumnIndex, WasNull);
      else
        Result[I] := RowAccessor.GetString(ColumnIndex, WasNull);
    end;
    if WasNull then
      Result[I] := Null;
  end;
end;

{**
  Compares row field values with the given ones.
  @param KeyValues given values.
  @param RowValues row field values.
  @param Options compare options.
  @return <code> if values are equal.
}
function CompareDataFields(KeyValues, RowValues: Variant;
  Options: TLocateOptions): Boolean;
var
  I: Integer;
  Value1, Value2: Variant;
begin
  Result := True;
  for I := 0 to VarArrayHighBound(KeyValues,1)-VarArrayLowBound(KeyValues,1) do
  begin
    Value1 := KeyValues[I + VarArrayLowBound(KeyValues,1)];
    Value2 := RowValues[I + VarArrayLowBound(RowValues,1)];

    if loCaseInsensitive in Options then
    begin
      Value1 := AnsiUpperCase(Value1);
      Value2 := AnsiUpperCase(Value2);
    end;

    if loPartialKey in Options then
    begin
      Result := StrLComp(PChar(VarToStr(Value2)), PChar(VarToStr(Value1)),
        Length(Value1)) = 0;
    end else
      Result := Value1 = Value2;

    if not Result then
      Break;
  end;
end;

{**
  Defines a list of key field names.
  @param Fields a collection of dataset fields.
  @return a list of key field names.
}
function DefineKeyFields(Fields: TFields): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Fields.Count - 1 do
  begin
    if (Fields[I].FieldKind = fkData)
      and not (Fields[I].DataType in [ftBlob, ftMemo, ftBytes]) then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + Fields[I].FieldName;
    end;
  end;
end;

{**
  Fill prepared statement with parameters.
  @param Statement a prepared SQL statement.
  @param ParamNames an array of parameter names.
  @param Params a collection of SQL parameters.
}
procedure FillStatementWithParams(Statement: IZPreparedStatement;
  ParamNames: TStringDynArray; Params: TParams);
var
  I: Integer;
  Param: TParam;
  Stream: TStream;
begin
  for I := Low(ParamNames) to High(ParamNames) do
  begin
    Param := Params.FindParam(ParamNames[I]);
    if not Assigned(Param) or (Param.ParamType in [ptOutput, ptResult]) then
      Continue;

    if Param.IsNull then
      Statement.SetNull(I + 1, ConvertDatasetToDbcType(Param.DataType))
    else begin
      case Param.DataType of
        ftBoolean:
          Statement.SetBoolean(I + 1, Param.AsBoolean);
        ftSmallInt:
          Statement.SetShort(I + 1, Param.AsSmallInt);
        ftInteger:
          Statement.SetInt(I + 1, Param.AsInteger);
        ftFloat:
          Statement.SetDouble(I + 1, Param.AsFloat);
        ftCurrency:
          Statement.SetDouble(I + 1, Param.AsFloat);
        ftLargeInt:
          Statement.SetInt(I + 1, Param.AsInteger);
        ftString:
          Statement.SetString(I + 1, Param.AsString);
        ftBytes:
          Statement.SetString(I + 1, Param.AsString);
        ftDate:
          Statement.SetDate(I + 1, Param.AsDate);
        ftTime:
          Statement.SetTime(I + 1, Param.AsTime);
        ftDateTime:
          Statement.SetTimestamp(I + 1, Param.AsDateTime);
        ftMemo:
          begin
            Stream := TStringStream.Create(Param.AsMemo);
            try
              Statement.SetAsciiStream(I + 1, Stream);
            finally
              Stream.Free;
            end;
          end;
        ftBlob:
          begin
            Stream := TStringStream.Create(Param.AsBlob);
            try
              Statement.SetBinaryStream(I + 1, Stream);
            finally
              Stream.Free;
            end;
          end;
      end;
    end;
  end;
end;

{**
  Converts datetime value into TDataset internal presentation.
  @param DataType a type of date-time field.
  @param Data a data which contains a value.
  @param Buffer a field buffer pointer
}
procedure DateTimeToNative(DataType: TFieldType; Data: TDateTime; Buffer: Pointer);
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(Data);
  case DataType of
    ftDate: Integer(Buffer^) := TimeStamp.Date;
    ftTime: Integer(Buffer^) := TimeStamp.Time;
  else
    TDateTime(Buffer^) := TimeStampToMSecs(TimeStamp);
  end;
end;

{**
  Converts date times from TDataset internal presentation into datetime value.
  @param DataType a type of date-time field.
  @param Buffer a field buffer pointer
  @return a data which contains a value.
}
function NativeToDateTime(DataType: TFieldType; Buffer: Pointer): TDateTime;
var
  TimeStamp: TTimeStamp;
begin
  case DataType of
    ftDate:
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Integer(Buffer^);
      end;
    ftTime:
      begin
        TimeStamp.Time := Integer(Buffer^);
        TimeStamp.Date := DateDelta;
      end;
  else
    try
      TimeStamp := MSecsToTimeStamp(TDateTime(Buffer^));
    except
      TimeStamp.Time := 0;
      TimeStamp.Date := 0;
    end;
  end;
  Result := TimeStampToDateTime(TimeStamp);
end;

end.

