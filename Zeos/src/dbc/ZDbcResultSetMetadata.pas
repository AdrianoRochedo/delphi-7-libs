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

unit ZDbcResultSetMetadata;

interface

uses
  Classes, SysUtils, ZDbcIntfs, ZClasses, ZCollections, ZGenericSqlAnalyser,
  ZSelectSchema;

type
  {** Represents an interface to column information. }
  IZColumnInfo = interface(IZObject)
    ['{897285E7-E0B7-40EE-888C-E033FDDCE551}']

    function IsAutoIncrement: Boolean;
    procedure SetAutoIncrement(Value: Boolean);
    function IsCaseSensitive: Boolean;
    procedure SetCaseSensitive(Value: Boolean);
    function IsSearchable: Boolean;
    procedure SetSearchable(Value: Boolean);
    function IsCurrency: Boolean;
    procedure SetCurrency(Value: Boolean);
    function IsNullable: TZColumnNullableType;
    procedure SetNullable(Value: TZColumnNullableType);

    function IsSigned: Boolean;
    procedure SetSigned(Value: Boolean);
    function GetColumnDisplaySize: Integer;
    procedure SetColumnDisplaySize(Value: Integer);
    function GetColumnLabel: string;
    procedure SetColumnLabel(Value: string);
    function GetColumnName: string;
    procedure SetColumnName(Value: string);
    function GetSchemaName: string;
    procedure SetSchemaName(Value: string);
    function GetPrecision: Integer;
    procedure SetPrecision(Value: Integer);
    function GetScale: Integer;
    procedure SetScale(Value: Integer);
    function GetTableName: string;
    procedure SetTableName(Value: string);
    function GetCatalogName: string;
    procedure SetCatalogName(Value: string);
    function GetColumnType: TZSQLType;
    procedure SetColumnType(Value: TZSQLType);
    function GetColumnTypeName: string;
    function IsReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    function IsWritable: Boolean;
    procedure SetWritable(Value: Boolean);
    function IsDefinitelyWritable: Boolean;
    procedure SetDefinitelyWritable(Value: Boolean);
  end;

  {** Implements a column information structure. }
  TZColumnInfo = class(TZAbstractObject, IZColumnInfo, IZComparable)
  protected
    FAutoIncrement: Boolean;
    FCaseSensitive: Boolean;
    FSearchable: Boolean;
    FCurrency: Boolean;
    FNullable: TZColumnNullableType;
    FSigned: Boolean;
    FColumnDisplaySize: Integer;
    FColumnLabel: string;
    FColumnName: string;
    FSchemaName: string;
    FPrecision: Integer;
    FScale: Integer;
    FTableName: string;
    FCatalogName: string;
    FColumnType: TZSQLType;
    FReadOnly: Boolean;
    FWritable: Boolean;
    FDefinitelyWritable: Boolean;
  public
    constructor Create;
    function Equals(const Value: IZInterface): Boolean; override;

    function IsAutoIncrement: Boolean;
    procedure SetAutoIncrement(Value: Boolean);
    function IsCaseSensitive: Boolean;
    procedure SetCaseSensitive(Value: Boolean);
    function IsSearchable: Boolean;
    procedure SetSearchable(Value: Boolean);
    function IsCurrency: Boolean;
    procedure SetCurrency(Value: Boolean);
    function IsNullable: TZColumnNullableType;
    procedure SetNullable(Value: TZColumnNullableType);

    function IsSigned: Boolean;
    procedure SetSigned(Value: Boolean);
    function GetColumnDisplaySize: Integer;
    procedure SetColumnDisplaySize(Value: Integer);
    function GetColumnLabel: string;
    procedure SetColumnLabel(Value: string);
    function GetColumnName: string;
    procedure SetColumnName(Value: string);
    function GetSchemaName: string;
    procedure SetSchemaName(Value: string);
    function GetPrecision: Integer;
    procedure SetPrecision(Value: Integer);
    function GetScale: Integer;
    procedure SetScale(Value: Integer);
    function GetTableName: string;
    procedure SetTableName(Value: string);
    function GetCatalogName: string;
    procedure SetCatalogName(Value: string);
    function GetColumnType: TZSQLType;
    procedure SetColumnType(Value: TZSQLType);
    function GetColumnTypeName: string;
    function IsReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    function IsWritable: Boolean;
    procedure SetWritable(Value: Boolean);
    function IsDefinitelyWritable: Boolean;
    procedure SetDefinitelyWritable(Value: Boolean);
  end;

  {** Implements Abstract ResultSet Metadata. }
  TZAbstractResultSetMetadata = class(TInterfacedObject, IZResultSetMetaData)
  private
    FLoaded: Boolean;
    FMetadata: IZDatabaseMetadata;
    FColumnsInfo: IZCollection;
    FColumnsLabels: TStrings;
    FSQL: string;
    FTableColumns: IZHashMap;
    FIdentifierConvertor: IZIdentifierConvertor;
  protected
    procedure LoadColumn(ColumnInfo: IZColumnInfo;
      SelectSchema: IZSelectSchema); virtual;

    function GetTableColumns(TableRef: IZTableReference): IZResultSet;
    procedure ReadColumnByRef(FieldRef: IZFieldReference;
      ColumnInfo: IZColumnInfo);
    procedure ReadColumnByName(FieldName: string; TableRef: IZTableReference;
      ColumnInfo: IZColumnInfo);
    procedure ClearColumn(ColumnInfo: IZColumnInfo);
    procedure LoadColumns;

    property MetaData: IZDatabaseMetadata read FMetadata write FMetadata;
    property ColumnsInfo: IZCollection read FColumnsInfo write FColumnsInfo;
    property ColumnsLabels: TStrings read FColumnsLabels write FColumnsLabels;
    property SQL: string read FSQL write FSQL;
    property IdentifierConvertor: IZIdentifierConvertor
      read FIdentifierConvertor write FIdentifierConvertor;
    property Loaded: Boolean read FLoaded write FLoaded;
  public
    constructor Create(Metadata: IZDatabaseMetadata; SQL: string;
      ColumnsInfo: IZCollection);
    destructor Destroy; override;

    function GetColumnCount: Integer; virtual;
    function IsAutoIncrement(Column: Integer): Boolean; virtual;
    function IsCaseSensitive(Column: Integer): Boolean; virtual;
    function IsSearchable(Column: Integer): Boolean; virtual;
    function IsCurrency(Column: Integer): Boolean; virtual;
    function IsNullable(Column: Integer): TZColumnNullableType; virtual;

    function IsSigned(Column: Integer): Boolean; virtual;
    function GetColumnDisplaySize(Column: Integer): Integer; virtual;
    function GetColumnLabel(Column: Integer): string; virtual;
    function GetColumnName(Column: Integer): string; virtual;
    function GetSchemaName(Column: Integer): string; virtual;
    function GetPrecision(Column: Integer): Integer; virtual;
    function GetScale(Column: Integer): Integer; virtual;
    function GetTableName(Column: Integer): string; virtual;
    function GetCatalogName(Column: Integer): string; virtual;
    function GetColumnType(Column: Integer): TZSQLType; virtual;
    function GetColumnTypeName(Column: Integer): string; virtual;
    function IsReadOnly(Column: Integer): Boolean; virtual;
    function IsWritable(Column: Integer): Boolean; virtual;
    function IsDefinitelyWritable(Column: Integer): Boolean; virtual;
  end;

implementation

uses ZDbcUtils, ZParseIntfs, ZParseToken, ZDbcMetadata;

{ TZColumnInfo }

{**
  Constructs this object and assigns main properties.
}
constructor TZColumnInfo.Create;
begin
  FAutoIncrement := False;
  FCaseSensitive := False;
  FSearchable := False;
  FCurrency := False;
  FNullable := ntNullableUnknown;
  FSigned := False;
  FColumnDisplaySize := 0;
  FColumnLabel := '';
  FColumnName := '';
  FSchemaName := '';
  FPrecision := 0;
  FScale := 0;
  FTableName := '';
  FCatalogName := '';
  FColumnType := stUnknown;
  FReadOnly := True;
  FWritable := False;
  FDefinitelyWritable := False;
end;

{**
  Checks is the specified value equals to this object.
  @param Value an interface to some object.
  @return <code>True</code> if the objects are identical.
}
function TZColumnInfo.Equals(const Value: IZInterface): Boolean;
var
  Temp: IZColumnInfo;
begin
  if Value <> nil then
  begin
    if Value.QueryInterface(IZColumnInfo, Temp) = 0 then
    begin
      Result := (GetColumnName = Temp.GetColumnName)
        and (GetTableName = Temp.GetTableName);
      Temp := nil;
    end else
      Result := inherited Equals(Value);
  end else
    Result := False;
end;

{**
  Indicates whether the designated column is automatically numbered, thus read-only.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZColumnInfo.IsAutoIncrement: Boolean;
begin
  Result := FAutoIncrement;
end;

{**
  Sets if the designated column is automatically numbered, thus read-only.
  @param Value <code>true</code> if so; <code>false</code> otherwise
}
procedure TZColumnInfo.SetAutoIncrement(Value: Boolean);
begin
  FAutoIncrement := Value;
end;

{**
  Indicates whether a column's case matters.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZColumnInfo.IsCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

{**
  Sets if column's case matters.
  @param Value <code>true</code> if so; <code>false</code> otherwise
}
procedure TZColumnInfo.SetCaseSensitive(Value: Boolean);
begin
  FCaseSensitive := Value;
end;

{**
  Indicates whether the designated column can be used in a where clause.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZColumnInfo.IsSearchable: Boolean;
begin
  Result := FSearchable;
end;

{**
  Sets if designated column can be used in a where clause.
  @param Value <code>true</code> if so; <code>false</code> otherwise
}
procedure TZColumnInfo.SetSearchable(Value: Boolean);
begin
  FSearchable := Value;
end;

{**
  Indicates whether the designated column is a cash value.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZColumnInfo.IsCurrency: Boolean;
begin
  Result := FCurrency;
end;

{**
  Sets if the designated column is a cash value.
  @param Value <code>true</code> if so; <code>false</code> otherwise
}
procedure TZColumnInfo.SetCurrency(Value: Boolean);
begin
  FCurrency := Value;
end;

{**
  Indicates the nullability of values in the designated column.
  @return the nullability status of the given column;
    one of <code>columnNoNulls</code>, <code>columnNullable</code>
    or <code>columnNullableUnknown</code>
}
function TZColumnInfo.IsNullable: TZColumnNullableType;
begin
  Result := FNullable;
end;

{**
  Sets if the nullability of values in the designated column.
  @param Value the nullability status of the given column;
    one of <code>columnNoNulls</code>, <code>columnNullable</code>
    or <code>columnNullableUnknown</code>
}
procedure TZColumnInfo.SetNullable(Value: TZColumnNullableType);
begin
  FNullable := Value;
end;

{**
  Indicates whether values in the designated column are signed numbers.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZColumnInfo.IsSigned: Boolean;
begin
  Result := FSigned;
end;

{**
  Sets if values in the designated column are signed numbers.
  @param Value <code>true</code> if so; <code>false</code> otherwise
}
procedure TZColumnInfo.SetSigned(Value: Boolean);
begin
  FSigned := Value;
end;

{**
  Indicates the designated column's normal maximum width in characters.
  @return the normal maximum number of characters allowed as the width
    of the designated column
}
function TZColumnInfo.GetColumnDisplaySize: Integer;
begin
  Result := FColumnDisplaySize;
end;

{**
  Sets if the designated column's normal maximum width in characters.
  @param Value the normal maximum number of characters allowed as the width
    of the designated column
}
procedure TZColumnInfo.SetColumnDisplaySize(Value: Integer);
begin
  FColumnDisplaySize := Value;
end;

{**
  Gets the designated column's suggested title for use in printouts and
  displays.
  @return the suggested column title
}
function TZColumnInfo.GetColumnLabel: string;
begin
  Result := FColumnLabel;
end;

{**
  Sets the designated column's suggested title for use in printouts and
  displays.
  @param Value the suggested column title
}
procedure TZColumnInfo.SetColumnLabel(Value: string);
begin
  FColumnLabel := Value;
end;

{**
  Get the designated column's name.
  @return column name
}
function TZColumnInfo.GetColumnName: string;
begin
  Result := FColumnName;
end;

{**
  Set the designated column's name.
  @param Value column name
}
procedure TZColumnInfo.SetColumnName(Value: string);
begin
  FColumnName := Value;
end;

{**
  Get the designated column's table's schema.
  @return schema name or "" if not applicable
}
function TZColumnInfo.GetSchemaName: string;
begin
  Result := FSchemaName;
end;

{**
  Set the designated column's table's schema.
  @param Value schema name or "" if not applicable
}
procedure TZColumnInfo.SetSchemaName(Value: string);
begin
  FSchemaName := Value;
end;

{**
  Get the designated column's number of decimal digits.
  @return precision
}
function TZColumnInfo.GetPrecision: Integer;
begin
  Result := FPrecision;
end;

{**
  Set the designated column's number of decimal digits.
  @param Value precision
}
procedure TZColumnInfo.SetPrecision(Value: Integer);
begin
  FPrecision := Value;
end;

{**
  Gets the designated column's number of digits to right of the decimal point.
  @return scale
}
function TZColumnInfo.GetScale: Integer;
begin
  Result := FScale;
end;

{**
  Sets the designated column's number of digits to right of the decimal point.
  @param Value scale
}
procedure TZColumnInfo.SetScale(Value: Integer);
begin
  FScale := Value;
end;

{**
  Gets the designated column's table name.
  @return table name or "" if not applicable
}
function TZColumnInfo.GetTableName: string;
begin
  Result := FTableName;
end;

{**
  Sets the designated column's table name.
  @param Value table name or "" if not applicable
}
procedure TZColumnInfo.SetTableName(Value: string);
begin
  FTableName := Value;
end;

{**
  Gets the designated column's table's catalog name.
  @return column name or "" if not applicable
}
function TZColumnInfo.GetCatalogName: string;
begin
  Result := FCatalogName;
end;

{**
  Sets the designated column's table's catalog name.
  @param Value column name or "" if not applicable
}
procedure TZColumnInfo.SetCatalogName(Value: string);
begin
  FCatalogName := Value;
end;

{**
  Retrieves the designated column's SQL type.
  @return SQL type from java.sql.Types
}
function TZColumnInfo.GetColumnType: TZSQLType;
begin
  Result := FColumnType;
end;

{**
  Sets the designated column's SQL type.
  @param Value SQL type from java.sql.Types
}
procedure TZColumnInfo.SetColumnType(Value: TZSQLType);
begin
  FColumnType := Value;
end;

{**
  Retrieves the designated column's database-specific type name.
  @return type name used by the database. If the column type is
    a user-defined type, then a fully-qualified type name is returned.
}
function TZColumnInfo.GetColumnTypeName: string;
begin
  Result := DefineColumnTypeName(FColumnType);
end;

{**
  Indicates whether the designated column is definitely not writable.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZColumnInfo.IsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

{**
  Sets the designated column is definitely not writable.
  @param Value <code>true</code> if so; <code>false</code> otherwise
}
procedure TZColumnInfo.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

{**
  Indicates whether it is possible for a write on the designated column to succeed.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZColumnInfo.IsWritable: Boolean;
begin
  Result := FWritable;
end;

{**
  Sets if it is possible for a write on the designated column to succeed.
  @param Value <code>true</code> if so; <code>false</code> otherwise
}
procedure TZColumnInfo.SetWritable(Value: Boolean);
begin
  FWritable := Value;
end;

{**
  Indicates whether a write on the designated column will definitely succeed.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZColumnInfo.IsDefinitelyWritable: Boolean;
begin
  Result := FDefinitelyWritable;
end;

{**
  Sets a write on the designated column will definitely succeed.
  @param Value <code>true</code> if so; <code>false</code> otherwise
}
procedure TZColumnInfo.SetDefinitelyWritable(Value: Boolean);
begin
  FDefinitelyWritable := Value;
end;

{ TZAbstractResultSetMetadata }

{**
  Constructs this object and assignes the main properties.
  @param Metadata a database metadata object.
  @param SQL an SQL query statement.
  @param ColumnsInfo a collection of columns info.
}
constructor TZAbstractResultSetMetadata.Create(Metadata: IZDatabaseMetadata;
  SQL: string; ColumnsInfo: IZCollection);
begin
  FMetadata := Metadata;
  FSQL := SQL;
  FLoaded := not (FMetadata <> nil);
  if ColumnsInfo <> nil then
    FColumnsInfo := ColumnsInfo
  else FColumnsInfo := TZCollection.Create;
  FTableColumns := TZHashMap.Create;
  FIdentifierConvertor := TZDefaultIdentifierConvertor.Create(FMetadata);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractResultSetMetadata.Destroy;
begin
  FMetadata := nil;
  FColumnsInfo := nil;
  FTableColumns := nil;
  if FColumnsLabels <> nil then
    FColumnsLabels.Free;
  inherited Destroy;
end;

{**
  Returns the number of columns in this <code>ResultSet</code> object.
  @return the number of columns
}
function TZAbstractResultSetMetadata.GetColumnCount: Integer;
begin
  Result := FColumnsInfo.Count;
end;

{**
  Indicates whether the designated column is automatically numbered, thus read-only.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsAutoIncrement(Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).IsAutoIncrement;
end;

{**
  Indicates whether a column's case matters.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsCaseSensitive(Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).IsCaseSensitive;
end;

{**
  Indicates whether the designated column can be used in a where clause.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsSearchable(Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).IsSearchable;
end;

{**
  Indicates whether the designated column is a cash value.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsCurrency(Column: Integer): Boolean;
begin
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).IsCurrency;
end;

{**
  Indicates the nullability of values in the designated column.
  @param column the first column is 1, the second is 2, ...
  @return the nullability status of the given column; one of <code>columnNoNulls</code>,
    <code>columnNullable</code> or <code>columnNullableUnknown</code>
}
function TZAbstractResultSetMetadata.IsNullable(
  Column: Integer): TZColumnNullableType;
begin
  if not Loaded then LoadColumns;
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).IsNullable;
end;

{**
  Indicates whether values in the designated column are signed numbers.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsSigned(Column: Integer): Boolean;
begin
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).IsSigned;
end;

{**
  Indicates the designated column's normal maximum width in characters.
  @param column the first column is 1, the second is 2, ...
  @return the normal maximum number of characters allowed as the width
    of the designated column
}
function TZAbstractResultSetMetadata.GetColumnDisplaySize(
  Column: Integer): Integer;
begin
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).GetColumnDisplaySize;
end;

{**
  Gets the designated column's suggested title for use in printouts and
  displays.
  @param column the first column is 1, the second is 2, ...
  @return the suggested column title
}
function TZAbstractResultSetMetadata.GetColumnLabel(Column: Integer): string;
var
  I, J, N: Integer;
  ColumnName: string;
begin
  { Prepare unique column labels. }
  if FColumnsLabels = nil then
  begin
    FColumnsLabels := TStringList.Create;
    for I := 0 to FColumnsInfo.Count - 1 do
    begin
      N := 0;
      ColumnName := (FColumnsInfo[I] as IZColumnInfo).GetColumnLabel;
      for J := 0 to I - 1 do
      begin
        if (FColumnsInfo[J] as IZColumnInfo).GetColumnLabel = ColumnName then
          Inc(N);
      end;
      if ColumnName = '' then
        ColumnName := 'Column';
      if N > 0 then
        ColumnName := ColumnName + '_' + IntToStr(N);
      FColumnsLabels.Add(ColumnName);
    end;
  end;

  Result := ColumnsLabels[Column - 1];
end;

{**
  Get the designated column's name.
  @param column the first column is 1, the second is 2, ...
  @return column name
}
function TZAbstractResultSetMetadata.GetColumnName(
  Column: Integer): string;
begin
  if not Loaded then LoadColumns;
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).GetColumnName;
end;

{**
  Get the designated column's table's schema.
  @param column the first column is 1, the second is 2, ...
  @return schema name or "" if not applicable
}
function TZAbstractResultSetMetadata.GetSchemaName(
  Column: Integer): string;
begin
  if not Loaded then LoadColumns;
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).GetSchemaName;
end;

{**
  Get the designated column's number of decimal digits.
  @param column the first column is 1, the second is 2, ...
  @return precision
}
function TZAbstractResultSetMetadata.GetPrecision(Column: Integer): Integer;
begin
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).GetPrecision;
end;

{**
  Gets the designated column's number of digits to right of the decimal point.
  @param column the first column is 1, the second is 2, ...
  @return scale
}
function TZAbstractResultSetMetadata.GetScale(Column: Integer): Integer;
begin
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).GetScale;
end;

{**
  Gets the designated column's table name.
  @param column the first column is 1, the second is 2, ...
  @return table name or "" if not applicable
}
function TZAbstractResultSetMetadata.GetTableName(Column: Integer): string;
begin
  if not Loaded then LoadColumns;
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).GetTableName;
end;

{**
  Gets the designated column's table's catalog name.
  @param column the first column is 1, the second is 2, ...
  @return column name or "" if not applicable
}
function TZAbstractResultSetMetadata.GetCatalogName(Column: Integer): string;
begin
  if not Loaded then LoadColumns;
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).GetCatalogName;
end;

{**
  Retrieves the designated column's SQL type.
  @param column the first column is 1, the second is 2, ...
  @return SQL type from java.sql.Types
}
function TZAbstractResultSetMetadata.GetColumnType(Column: Integer): TZSQLType;
begin
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).GetColumnType;
end;

{**
  Retrieves the designated column's database-specific type name.

  @param column the first column is 1, the second is 2, ...
  @return type name used by the database. If the column type is
    a user-defined type, then a fully-qualified type name is returned.
}
function TZAbstractResultSetMetadata.GetColumnTypeName(Column: Integer): string;
begin
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).GetColumnTypeName;
end;

{**
  Indicates whether the designated column is definitely not writable.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsReadOnly(Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).IsReadOnly;
end;

{**
  Indicates whether it is possible for a write on the designated column to succeed.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsWritable(Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).IsWritable;
end;

{**
  Indicates whether a write on the designated column will definitely succeed.
  @param column the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsDefinitelyWritable(
  Column: Integer): Boolean;
begin
  if not Loaded then LoadColumns;
  Result := (FColumnsInfo[Column - 1] as IZColumnInfo).IsDefinitelyWritable;
end;

{**
  Gets a table description result set.
  @param TableRef a table reference object.
  @return a result set with table columns from database metadata.
}
function TZAbstractResultSetMetadata.GetTableColumns(
  TableRef: IZTableReference): IZResultSet;
var
  TableKey: IZAnyValue;
begin
  TableKey := TZAnyValue.Create(TableRef.GetFullName);
  if FTableColumns.Get(TableKey) = nil then
  begin
    Result := Metadata.GetColumns(TableRef.GetCatalog,
      TableRef.GetCatalog, TableRef.GetTable, '');
    FTableColumns.Put(TableKey, Result);
  end else
    Result := FTableColumns.Get(TableKey) as IZResultSet;
end;

{**
  Clears specified column information.
  @param ColumnInfo a column information object.
}
procedure TZAbstractResultSetMetadata.ClearColumn(ColumnInfo: IZColumnInfo);
begin
  ColumnInfo.SetReadOnly(True);
  ColumnInfo.SetWritable(False);
  ColumnInfo.SetDefinitelyWritable(False);
  ColumnInfo.SetCatalogName('');
  ColumnInfo.SetSchemaName('');
  ColumnInfo.SetTableName('');
  ColumnInfo.SetColumnName('');
end;

{**
  Reads a column information from table metadata.
  @param FieldName a name of the field.
  @param TableRef a table reference object.
  @param ColumnInfo a column information object.
}
procedure TZAbstractResultSetMetadata.ReadColumnByName(FieldName: string;
  TableRef: IZTableReference; ColumnInfo: IZColumnInfo);
var
  TableColumns: IZResultSet;
begin
  TableColumns := GetTableColumns(TableRef);
  { Checks for unexisted table. }
  if not Assigned(TableColumns) then
    Exit;

  { Locates a column row. }
  TableColumns.BeforeFirst;
  while TableColumns.Next do
  begin
    if TableColumns.GetString(4) = FieldName then
      Break;
  end;
  if TableColumns.IsAfterLast then
    Exit;

  { Reads a column information. }
  ColumnInfo.SetColumnType(TZSQLType(TableColumns.GetInt(5)));
  ColumnInfo.SetReadOnly(False);
  ColumnInfo.SetWritable(True);
  ColumnInfo.SetDefinitelyWritable(True);
  ColumnInfo.SetNullable(TZColumnNullableType(TableColumns.GetInt(11)));
  ColumnInfo.SetCatalogName(TableColumns.GetString(1));
  ColumnInfo.SetSchemaName(TableColumns.GetString(2));
  ColumnInfo.SetTableName(TableColumns.GetString(3));
  ColumnInfo.SetColumnName(FieldName);
  ColumnInfo.SetCaseSensitive(
    IdentifierConvertor.IsCaseSensitive(FieldName));
end;

{**
  Reads a column information from table metadata.
  @param FieldRef a field reference object.
  @param ColumnInfo a column information object.
}
procedure TZAbstractResultSetMetadata.ReadColumnByRef(
  FieldRef: IZFieldReference; ColumnInfo: IZColumnInfo);
begin
  ClearColumn(ColumnInfo);
  { Checks for uncompleted field reference. }
  if not Assigned(FieldRef) or not Assigned(FieldRef.GetTableReference) then
    Exit;
  if not FieldRef.IsField then
    Exit;

  ReadColumnByName(FieldRef.GetField, FieldRef.GetTableReference, ColumnInfo);
end;

{**
  Initializes on single column of the result set.
  @param ColumnInfo a column information object to be initialized.
  @param SelectSchema a schema of the select statement.
}
procedure TZAbstractResultSetMetadata.LoadColumn(
  ColumnInfo: IZColumnInfo; SelectSchema: IZSelectSchema);
var
  I: Integer;
  FieldRef: IZFieldReference;
  TableRef: IZTableReference;
begin
  { Initializes single columns with specified table. }
  FieldRef := SelectSchema.FindFieldByShortName(ColumnInfo.GetColumnLabel);
  ReadColumnByRef(FieldRef, ColumnInfo);
  if ColumnInfo.GetColumnName <> '' then
    Exit;

  { Initializes single columns without specified table. }
  I := 0;
  while (ColumnInfo.GetColumnName = '')
    and (I < SelectSchema.GetTables.Count) do
  begin
    TableRef := SelectSchema.GetTables[I] as IZTableReference;
    if Assigned(FieldRef) then
      ReadColumnByName(FieldRef.GetField, TableRef, ColumnInfo)
    else
      ReadColumnByName(ColumnInfo.GetColumnLabel, TableRef, ColumnInfo);
    Inc(I);
  end;
end;

{**
  Initializes columns with additional data.
}
procedure TZAbstractResultSetMetadata.LoadColumns;
var
  I: Integer;
  Tokenizer: IZTokenizer;
  StatementAnalyser: IZStatementAnalyser;
  SelectSchema: IZSelectSchema;
begin
  { Parses the Select statement and retrieves a schema object. }
  Tokenizer := (Metadata as IZExtraDatabaseMetadata).GetTokenizer;
  StatementAnalyser := (Metadata as IZExtraDatabaseMetadata).GetStatementAnalyser;
  SelectSchema := StatementAnalyser.ExtractSelectSchemaFromBuffer(Tokenizer, SQL);
  if not Assigned(SelectSchema) then
  begin
    Loaded := True;
    Exit;
  end;
  SelectSchema.LinkReferences(IdentifierConvertor);

  { Initializes all columns. }
  for I := 0 to FColumnsInfo.Count - 1 do
    LoadColumn(FColumnsInfo[I] as IZColumnInfo, SelectSchema);

  Loaded := True;
end;

end.

