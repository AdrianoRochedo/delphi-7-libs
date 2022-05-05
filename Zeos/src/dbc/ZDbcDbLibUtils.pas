{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                DBLib Utility Functions                  }
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

unit ZDbcDbLibUtils;

interface

{$I ZDbc.inc}

uses Classes, SysUtils, ZDbcIntfs;

{**
  Converts an ODBC native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertODBCToSqlType(FieldType: SmallInt): TZSqlType;

{**
  Converts a DBLib native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertDBLibToSqlType(FieldType: SmallInt): TZSqlType;

{**
  Convert string DBLib field type to SqlType
  @param string field type value
  @result the SqlType field type value
}
function ConvertDBLibTypeToSqlType(Value: string): TZSqlType;

{**
  Converts ZDBC SQL types into MS SQL native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToDBLibTypeName(FieldType: TZSqlType): string;

{**
  Converts a DBLib nullability value into ZDBC TZColumnNullableType.
  @param DBLibNullability dblibc native nullability.
  @return a SQL TZColumnNullableType.
}
function ConvertDBLibNullability(DBLibNullability: Byte): TZColumnNullableType;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function PrepareSqlParameter(Value: Variant; ParamType: TZSQLType): string;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ENDIF}
  ZCompatibility, ZSysUtils, ZPlainDBLibDriver;

{**
  Converts an ODBC native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertODBCToSqlType(FieldType: SmallInt): TZSqlType;
begin
  case FieldType of
    1, 12, -8, -9: Result := stString;
    -7: Result := stBoolean;
    -6: Result := stByte;
    5: Result := stShort;
    4: Result := stInteger;
    2, 3, 6, 7, 8: Result := stDouble;
    11, 93: Result := stTimestamp;
    -1, -10: Result := stAsciiStream;
    -3, -4, -11: Result := stBinaryStream;
    -2: Result := stBytes;
  else
    Result := stUnknown;
  end;
end;

{**
  Converts a DBLib native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertDBLibToSqlType(FieldType: SmallInt): TZSqlType;
begin
  case FieldType of
    SQLCHAR: Result := stString;
    SQLBIT: Result := stBoolean;
    SQLINT1: Result := stByte;
    SQLINT2: Result := stShort;
    SQLINT4: Result := stInteger;
    SQLFLT4: Result := stDouble;
    SQLFLT8: Result := stDouble;
    SQLMONEY4: Result := stDouble;
    SQLMONEY: Result := stDouble;
    SQLDATETIM4: Result := stTimestamp;
    SQLDATETIME: Result := stTimestamp;
    SQLTEXT: Result := stAsciiStream;
    SQLIMAGE: Result := stBinaryStream;
    SQLBINARY: Result := stBinaryStream;
  else
    Result := stUnknown;
  end;
end;

{**
  Convert string DBLib field type to SqlType
  @param string field type value
  @result the SqlType field type value
}
function ConvertDBLibTypeToSqlType(Value: string): TZSqlType;
begin
  Result := stUnknown;
end;

{**
  Converts ZDBC SQL types into DBLib native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToDBLibTypeName(FieldType: TZSqlType): string;
begin
  Result := '';
  case FieldType of
    stBoolean: Result := 'bit';
    stByte: Result := 'tinyint';
    stShort: Result := 'smallint';
    stInteger: Result := 'int';
    stLong: Result := 'int';
    stFloat: Result := 'float(24)';
    stDouble: Result := 'float(53)';
    stBigDecimal: Result := 'int';
    stString: Result := 'varchar(8000)';
    stBytes: Result := 'varbinary(8000)';
    stDate: Result := 'datetime';
    stTime: Result := 'datetime';
    stTimestamp: Result := 'datetime';
    stAsciiStream: Result := 'text';
    stUnicodeStream: Result := 'ntext';
    stBinaryStream: Result := 'image';
  end;
end;

{**
  Converts a DBLib nullability value into ZDBC TZColumnNullableType.
  @param DBLibNullability dblibc native nullability.
  @return a SQL TZColumnNullableType.
}
function ConvertDBLibNullability(DBLibNullability: Byte): TZColumnNullableType;
const
  Nullability: array[0..2] of TZColumnNullableType =
    (ntNoNulls, ntNullable, ntNullableUnknown);
begin
  Result := Nullability[DBLibNullability];
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function PrepareSqlParameter(Value: Variant; ParamType: TZSQLType): string;
var
  TempBytes: TByteDynArray;
  TempBlob: IZBlob;
  TempString: string;
begin
  TempBytes := nil;

  if VarIsEmpty(Value) or VarIsNull(Value) then
    Result := 'NULL'
  else begin
    case ParamType of
      stBoolean:
        if Value then
          Result := '1'
        else Result := '0';
      stByte, stShort, stInteger, stLong, stBigDecimal:
        Result := VarToStr(Value);
      stFloat, stDouble:
        Result := FloatToSqlStr(Value);
      stString:
        Result := AnsiQuotedStr(VarToStr(Value), '''');
      stBytes:
        begin
          TempBytes := TByteDynArray(Value);
          if Length(TempBytes) = 0 then
            Result := 'NULL'
          else
          begin
            SetLength(Result, (2 * Length(TempBytes)));
            BinToHex(PChar(TempBytes), PChar(Result), Length(TempBytes));
            Result := '0x' + Result;
          end;
        end;
      stDate:
        Result := '''' + FormatDateTime('yyyymmdd', VarToDateTime(Value)) + '''';
      stTime:
        Result := '''' + FormatDateTime('hh":"mm":"ss":"zzz', VarToDateTime(Value)) + '''';
      stTimestamp:
        Result := '''' + FormatDateTime('yyyymmdd hh":"mm":"ss":"zzz',
          VarToDateTime(Value)) + '''';
      stAsciiStream, stUnicodeStream:
        begin
          if VarType(Value) = varUnknown then
          begin
            TempBlob := IZBlob(TVarData(Value).VUnknown);
            if not TempBlob.IsEmpty then
            begin
              Result := AnsiQuotedStr(TempBlob.GetString, '''')
            end else
              Result := 'NULL';
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
              TempString := TempBlob.GetString;
              SetLength(Result, (2 * Length(TempString)));
              BinToHex(PChar(TempString), PChar(Result), Length(TempString));
              Result := '0x' + Result;
            end
            else
              Result := 'NULL';
          end else
            Result := 'NULL';
        end;
      else
         Result := 'NULL';
    end;
  end;
end;


end.
