{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                Generic Cached Resolver                  }
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

unit ZDbcGenericResolver;

interface

uses
  Classes, SysUtils, ZClasses, ZDbcIntfs, ZDbcResultSet, ZCollections,
  ZDbcCache, ZDbcCachedResultSet, ZCompatibility, ZSelectSchema;

type
  {** Represents interface to resolver parameters. }
  IZResolverParameter = interface (IZInterface)
    ['{30D196EA-31DC-4CEC-A8A3-5E358B2E2F78}']

    function GetColumnIndex: Integer;
    function GetColumnName: string;
    function IsNewValue: Boolean;
  end;

  {** Implements a resolver parameter object. }
  TZResolverParameter = class (TZAbstractObject, IZResolverParameter)
  private
    FColumnIndex: Integer;
    FColumnName: string;
    FNewValue: Boolean;
  public
    constructor Create(ColumnIndex: Integer; ColumnName: string;
      NewValue: Boolean);

    function GetColumnIndex: Integer;
    function GetColumnName: string;
    function IsNewValue: Boolean;
  end;

  {** Represents a generic cached resolver. }
  IZGenericCachedResolver = interface (IZCachedResolver)
    ['{B92042BE-05FF-43BF-9673-212BE0087743}']

    function GetInsertSQL: string;
    function GetUpdateSQL: string;
    function GetDeleteSQL: string;

    function GetInsertParams: IZCollection;
    function GetUpdateParams: IZCollection;
    function GetDeleteParams: IZCollection;
  end;

  {**
    Implements a generic cached resolver object which generates
    DML SQL statements and posts resultset updates to database.
  }
  TZGenericCachedResolver = class (TInterfacedObject, IZCachedResolver,
    IZGenericCachedResolver)
  private
    FConnection: IZConnection;
    FMetadata: IZResultSetMetadata;
    FDatabaseMetadata: IZDatabaseMetadata;
    FInitialized: Boolean;
    FTableName: string;
    FInsertSQL: string;
    FDeleteSQL: string;
    FUpdateSQL: string;
    FInsertStatement: IZPreparedStatement;
    FDeleteStatement: IZPreparedStatement;
    FUpdateStatement: IZPreparedStatement;
    FInsertParams: IZCollection;
    FDeleteParams: IZCollection;
    FUpdateParams: IZCollection;
    FIdentifierConvertor: IZIdentifierConvertor;
  protected
    function ComposeFullTableName(Catalog, Schema, Table: string): string;
    function DefineTableName: string;
    function GetDataColumns: IZCollection;
    function GetKeyColumns: IZCollection;
    function FormWhereClause(Columns: IZCollection): string;
    procedure InitializeStatements;
    procedure FillStatement(Statement: IZPreparedStatement;
      Params: IZCollection; OldRowAccessor, NewRowAccessor: TZRowAccessor);
    procedure PostUpdates(Sender: IZCachedResultSet;
      UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor);

    property Connection: IZConnection read FConnection write FConnection;
    property Metadata: IZResultSetMetadata read FMetadata write FMetadata;
    property DatabaseMetadata: IZDatabaseMetadata read FDatabaseMetadata
      write FDatabaseMetadata;
    property Initialized: Boolean read FInitialized write FInitialized;
    property TableName: string read FTableName write FTableName;
    property InsertSQL: string read FInsertSQL write FInsertSQL;
    property DeleteSQL: string read FDeleteSQL write FDeleteSQL;
    property UpdateSQL: string read FUpdateSQL write FUpdateSQL;
    property InsertStatement: IZPreparedStatement read FInsertStatement
      write FInsertStatement;
    property DeleteStatement: IZPreparedStatement read FDeleteStatement
      write FDeleteStatement;
    property UpdateStatement: IZPreparedStatement read FUpdateStatement
      write FUpdateStatement;
    property InsertParams: IZCollection read FInsertParams write FInsertParams;
    property DeleteParams: IZCollection read FDeleteParams write FDeleteParams;
    property UpdateParams: IZCollection read FUpdateParams write FUpdateParams;
    property IdentifierConvertor: IZIdentifierConvertor
      read FIdentifierConvertor write FIdentifierConvertor;
  public
    constructor Create(Statement: IZStatement; Metadata: IZResultSetMetadata);
    destructor Destroy; override;

    function GetInsertSQL: string;
    function GetUpdateSQL: string;
    function GetDeleteSQL: string;

    function GetInsertParams: IZCollection;
    function GetUpdateParams: IZCollection;
    function GetDeleteParams: IZCollection;
  end;

implementation

uses ZDbcMetadata;

{ TZResolverParameter }

{**
  Constructs this resolver parameter and assignes the main properties.
  @param ColumnIndex a result set column index.
  @param ColumnName a result set column name.
  @param NewValue <code>True</code> for new value and <code>False</code>
    for an old one.
}
constructor TZResolverParameter.Create(ColumnIndex: Integer;
  ColumnName: string; NewValue: Boolean);
begin
  FColumnIndex := ColumnIndex;
  FColumnName := ColumnName;
  FNewValue := NewValue;
end;

{**
  Gets a resultset column index.
  @return a result set column index.
}
function TZResolverParameter.GetColumnIndex: Integer;
begin
  Result := FColumnIndex;
end;

{**
  Gets a resultset column name.
  @return a resultset column name.
}
function TZResolverParameter.GetColumnName: string;
begin
  Result := FColumnName;
end;

{**
  Checks is a new value required.
  @return <code>True</code> is required a new value.
}
function TZResolverParameter.IsNewValue: Boolean;
begin
  Result := FNewValue;
end;

{ TZGenericCachedResolver }

{**
  Creates a cached resolver and assignes the main properties.
  @param ResultSet a related ResultSet object.
}
constructor TZGenericCachedResolver.Create(Statement: IZStatement;
  Metadata: IZResultSetMetadata);
begin
  FConnection := Statement.GetConnection;
  FMetadata := Metadata;
  FDatabaseMetadata := Statement.GetConnection.GetMetadata;
  FInitialized := False;
  FInsertParams := TZCollection.Create;
  FDeleteParams := TZCollection.Create;
  FUpdateParams := TZCollection.Create;
  FIdentifierConvertor := TZDefaultIdentifierConvertor.Create(
    FDatabaseMetadata);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZGenericCachedResolver.Destroy;
begin
  FMetadata := nil;
  FDatabaseMetadata := nil;
  FInsertParams := nil;
  FDeleteParams := nil;
  FUpdateParams := nil;

  inherited Destroy;
end;

{**
  Composes a fully quilified table name.
  @param Catalog a table catalog name.
  @param Schema a table schema name.
  @param Table a table name.
  @return a fully qualified table name.
}
function TZGenericCachedResolver.ComposeFullTableName(Catalog, Schema,
  Table: string): string;
begin
  if Table <> '' then
  begin
    Result := IdentifierConvertor.Quote(Table);
    if Schema <> '' then
      Result := IdentifierConvertor.Quote(Schema) + '.' + Result;
    if Catalog <> '' then
      Result := IdentifierConvertor.Quote(Catalog) + '.' + Result;
  end else
    Result := '';
end;

{**
  Defines a table name from the select statement.
}
function TZGenericCachedResolver.DefineTableName: string;
var
  I: Integer;
  Temp: string;
begin
  Result := '';
  for I := 1 to Metadata.GetColumnCount do
  begin
    Temp := ComposeFullTableName(Metadata.GetCatalogName(I),
      Metadata.GetSchemaName(I), Metadata.GetTableName(I));
    if (Result = '') and (Temp <> '') then
      Result := Temp
    else if (Result <> '') and (Temp <> '') and (Temp <> Result) then
    begin
      raise EZSQLException.Create(
        'Can''t update a complex query with more then one table.');
    end;
  end;
  if Result = '' then
    raise EZSQLException.Create('Can''t update this query type.');
end;

{**
  Gets a collection of data columns for INSERT or UPDATE DML statements.
  @return a collection of data columns.
}
function TZGenericCachedResolver.GetDataColumns: IZCollection;
var
  I: Integer;
begin
  Result := TZCollection.Create;
  for I := 1 to Metadata.GetColumnCount do
  begin
    if (Metadata.GetTableName(I) <> '')
      and (Metadata.GetColumnName(I) <> '') then
    begin
      Result.Add(TZResolverParameter.Create(I,
        Metadata.GetColumnName(I), True));
    end;
  end;
end;

{**
  Gets a collection of key columns for DELETE or UPDATE DML statements.
  @return a collection of key columns.
}
function TZGenericCachedResolver.GetKeyColumns: IZCollection;
var
  I: Integer;
  Found: Boolean;
  ColumnName: string;
  Catalog, Schema, Table: string;
  PrimaryKeys: IZResultSet;
begin
  Table := DefineTableName;
  Result := TZCollection.Create;

  { Defines catalog, schema and a table. }
  for I := 1 to Metadata.GetColumnCount do
  begin
    Table := Metadata.GetTableName(I);
    if Table <> '' then
    begin
      Schema := Metadata.GetSchemaName(I);
      Catalog := Metadata.GetCatalogName(I);
      Break;
    end;
  end;

  { Tries to define primary keys. }
  PrimaryKeys := DatabaseMetadata.GetPrimaryKeys(Catalog, Schema, Table);
  while PrimaryKeys.Next do
  begin
    ColumnName := PrimaryKeys.GetString(4);
    Found := False;
    for I := 1 to Metadata.GetColumnCount do
    begin
      if (ColumnName = Metadata.GetColumnName(I))
        and (Table = Metadata.GetTableName(I)) then
      begin
        Found := True;
        Break;
      end;
    end;
    if not Found then
    begin
      Result.Clear;
      Break;
    end;
    Result.Add(TZResolverParameter.Create(I, ColumnName, False));
  end;
  if Result.Count > 0 then
    Exit;

  { Takes a a key all non-blob fields. }
  for I := 1 to Metadata.GetColumnCount do
  begin
    if (Metadata.GetTableName(I) <> '') and (Metadata.GetColumnName(I) <> '')
      and (Metadata.GetColumnType(I) in [stBoolean, stByte, stShort, stInteger,
      stLong, stFloat, stDouble, stBigDecimal, stString, stDate, stTime,
      stTimestamp]) then
    begin
      Result.Add(TZResolverParameter.Create(I,
        Metadata.GetColumnName(I), False));
    end;
  end;
end;

{**
  Forms a where clause for UPDATE or DELETE DML statements.
  @param Columns a collection of key columns.
}
function TZGenericCachedResolver.FormWhereClause(Columns: IZCollection): string;
var
  I: Integer;
  Current: IZResolverParameter;
begin
  Result := '';
  for I := 0 to Columns.Count - 1 do
  begin
    Current := Columns[I] as IZResolverParameter;
    if Result <> '' then
      Result := Result + ' AND ';
    Result := Result + IdentifierConvertor.Quote(
      Current.GetColumnName) + '=?';
  end;
  if Result <> '' then
    Result := 'WHERE ' + Result;
end;

{**
  Initializes all DML statements.
}
procedure TZGenericCachedResolver.InitializeStatements;
var
  I: Integer;
  Temp1, Temp2: string;
  Current: IZResolverParameter;
  WhereClause: string;
  DataParams: IZCollection;
  KeyParams: IZCollection;
begin
  { Defines initial data. }
  TableName := DefineTableName;
  DataParams := GetDataColumns;
  KeyParams := GetKeyColumns;
  WhereClause := FormWhereClause(KeyParams);

  { Forms an insert statement. }
  Temp1 := '';
  Temp2 := '';
  for I := 0 to DataParams.Count - 1 do
  begin
    Current := DataParams[I] as IZResolverParameter;
    if Temp1 <> '' then
    begin
      Temp1 := Temp1 + ',';
      Temp2 := Temp2 + ',';
    end;
    Temp1 := Temp1 + IdentifierConvertor.Quote(Current.GetColumnName);
    Temp2 := Temp2 + '?';
  end;
  InsertSQL := Format('INSERT INTO %s (%s) VALUES (%s)',
    [TableName, Temp1, Temp2]);
  InsertParams.AddAll(DataParams);
  InsertStatement := Connection.PrepareStatement(InsertSQL);

  { Forms a delete statement. }
  DeleteSQL := Format('DELETE FROM %s %s', [TableName, WhereClause]);
  DeleteParams.AddAll(KeyParams);
  DeleteStatement := Connection.PrepareStatement(DeleteSQL);

  { Forms an update statement. }
  Temp1 := '';
  for I := 0 to DataParams.Count - 1 do
  begin
    Current := DataParams[I] as IZResolverParameter;
    if Temp1 <> '' then
      Temp1 := Temp1 + ',';
    Temp1 := Temp1 + IdentifierConvertor.Quote(Current.GetColumnName) + '=?';
  end;
  UpdateSQL := Format('UPDATE %s SET %s %s', [TableName, Temp1, WhereClause]);
  UpdateParams.AddAll(DataParams);
  UpdateParams.AddAll(KeyParams);
  UpdateStatement := Connection.PrepareStatement(UpdateSQL);

  Initialized := True;
end;

{**
  Fills the specified statement with stored or given parameters.
  @param ResultSet a source result set object.
  @param Statement a DBC statement object.
  @param Config an UpdateStatement configuration.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZGenericCachedResolver.FillStatement(Statement: IZPreparedStatement;
  Params: IZCollection; OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  I: Integer;
  ColumnIndex: Integer;
  Current: IZResolverParameter;
  RowAccessor: TZRowAccessor;
  Stream: TStream;
  WasNull: Boolean;
begin
  for I := 0 to Params.Count - 1 do
  begin
    Current := Params[I] as IZResolverParameter;
    if Current.IsNewValue then
      RowAccessor := NewRowAccessor
    else RowAccessor := OldRowAccessor;
    ColumnIndex := Current.GetColumnIndex;

    case Metadata.GetColumnType(ColumnIndex) of
      stBoolean:
        Statement.SetBoolean(I + 1,
          RowAccessor.GetBoolean(ColumnIndex, WasNull));
      stByte:
        Statement.SetByte(I + 1, RowAccessor.GetByte(ColumnIndex, WasNull));
      stShort:
        Statement.SetShort(I + 1, RowAccessor.GetShort(ColumnIndex, WasNull));
      stInteger:
        Statement.SetInt(I + 1, RowAccessor.GetInt(ColumnIndex, WasNull));
      stLong:
        Statement.SetLong(I + 1, RowAccessor.GetLong(ColumnIndex, WasNull));
      stFloat:
        Statement.SetFloat(I + 1, RowAccessor.GetFloat(ColumnIndex, WasNull));
      stDouble:
        Statement.SetDouble(I + 1, RowAccessor.GetDouble(ColumnIndex, WasNull));
      stBigDecimal:
        Statement.SetBigDecimal(I + 1,
          RowAccessor.GetBigDecimal(ColumnIndex, WasNull));
      stString:
        Statement.SetString(I + 1, RowAccessor.GetString(ColumnIndex, WasNull));
      stBytes:
        Statement.SetBytes(I + 1, RowAccessor.GetBytes(ColumnIndex, WasNull));
      stDate:
        Statement.SetDate(I + 1, RowAccessor.GetDate(ColumnIndex, WasNull));
      stTime:
        Statement.SetTime(I + 1, RowAccessor.GetTime(ColumnIndex, WasNull));
      stTimestamp:
        Statement.SetTimestamp(I + 1,
          RowAccessor.GetTimestamp(ColumnIndex, WasNull));
      stAsciiStream:
        begin
          if not RowAccessor.GetBlob(ColumnIndex, WasNull).IsEmpty then
          begin
            Stream := RowAccessor.GetAsciiStream(ColumnIndex, WasNull);
            try
              Statement.SetAsciiStream(I + 1, Stream);
            finally
              if Stream <> nil then
                Stream.Free;
            end;
          end
          else
          begin
            Statement.SetNull(I + 1, Metadata.GetColumnType(ColumnIndex));
          end;
        end;
      stUnicodeStream:
        begin
          if not RowAccessor.GetBlob(ColumnIndex, WasNull).IsEmpty then
          begin
            Stream := RowAccessor.GetUnicodeStream(ColumnIndex, WasNull);
            try
              Statement.SetUnicodeStream(I + 1, Stream);
            finally
              if Stream <> nil then
                Stream.Free;
            end;
          end
          else
          begin
            Statement.SetNull(I + 1, Metadata.GetColumnType(ColumnIndex));
          end;
        end;
      stBinaryStream:
        begin
          if not RowAccessor.GetBlob(ColumnIndex, WasNull).IsEmpty then
          begin
            Stream := RowAccessor.GetBinaryStream(ColumnIndex, WasNull);
            try
              Statement.SetBinaryStream(I + 1, Stream);
            finally
              if Stream <> nil then
                Stream.Free;
            end;
          end
          else
          begin
            Statement.SetNull(I + 1, Metadata.GetColumnType(ColumnIndex));
          end;
        end;
    end;
    if WasNull then
      Statement.SetNull(I + 1, Metadata.GetColumnType(ColumnIndex))
  end;
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZGenericCachedResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  Statement: IZPreparedStatement;
  SQL: string;
  SQLParams: IZCollection;
begin
  if (UpdateType = utDeleted)
    and (OldRowAccessor.RowBuffer.UpdateType = utInserted) then
    Exit;

  case UpdateType of
    utInserted:
      begin
        SQL := GetInsertSQL;
        SQLParams := GetInsertParams;
        Statement := InsertStatement;
      end;
    utDeleted:
      begin
        SQL := GetDeleteSQL;
        SQLParams := GetDeleteParams;
        Statement := DeleteStatement;
      end;
    utModified:
      begin
        SQL := GetUpdateSQL;
        SQLParams := GetUpdateParams;
        Statement := UpdateStatement;
      end;
    utUnmodified:
      Exit;
  end;

  FillStatement(Statement, SQLParams, OldRowAccessor, NewRowAccessor);
  Statement.ExecutePrepared;
end;

{**
  Gets an Insert SQL statement to update the resultset.
  @return an Insert SQL statement.
}
function TZGenericCachedResolver.GetInsertSQL: string;
begin
  if not Initialized then
    InitializeStatements;
  Result := InsertSQL;
end;

{**
  Gets an Update SQL statement to update the resultset.
  @return an Update SQL statement.
}
function TZGenericCachedResolver.GetUpdateSQL: string;
begin
  if not Initialized then
    InitializeStatements;
  Result := UpdateSQL;
end;

{**
  Gets a Delete SQL statement to update the resultset.
  @return a Delete SQL statement.
}
function TZGenericCachedResolver.GetDeleteSQL: string;
begin
  if not Initialized then
    InitializeStatements;
  Result := DeleteSQL;
end;

{**
  Gets Insert SQL statement parameters.
  @return Insert SQL statement parameters.
}
function TZGenericCachedResolver.GetInsertParams: IZCollection;
begin
  if not Initialized then
    InitializeStatements;
  Result := InsertParams;
end;

{**
  Gets Update SQL statement parameters.
  @return Update SQL statement parameters.
}
function TZGenericCachedResolver.GetUpdateParams: IZCollection;
begin
  if not Initialized then
    InitializeStatements;
  Result := UpdateParams;
end;

{**
  Gets Delete SQL statement parameters.
  @return Delete SQL statement parameters.
}
function TZGenericCachedResolver.GetDeleteParams: IZCollection;
begin
  if not Initialized then
    InitializeStatements;
  Result := DeleteParams;
end;

end.

