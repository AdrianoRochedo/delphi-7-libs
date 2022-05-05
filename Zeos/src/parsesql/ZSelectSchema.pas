{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        SQL Select Objects and Assembler classes         }
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

unit ZSelectSchema;

interface

uses ZClasses, ZCollections, ZParseIntfs, ZParseCore, ZGenericSelectParser;

type

  {** Case Sensitive/Unsensitive identificator processor. }
  IZIdentifierConvertor = interface (IZInterface)
    ['{2EB07B9B-1E96-4A42-8084-6F98D9140B27}']

    function IsCaseSensitive(Value: string): Boolean;
    function IsQuoted(Value: string): Boolean;
    function Quote(Value: string): string;
    function ExtractQuote(Value: string): string;
  end;

  {** Represents a reference item type such as Catalog, Table or Field. }
  TZReferenceItemType = (riCatalog, riSchema, riTable, riField, riAlias);

  {** Defines an interface for reference item. }
  IZReferenceItem = interface (IZClonnable)
    ['{F9A68690-ADE1-48AA-B926-F24707BC581E}']
    function GetType: TZReferenceItemType;
    function GetValue: string;
  end;

  {** Implements a reference item assembly. }
  TZReferenceItem = class (TZAbstractObject, IZReferenceItem)
  private
    FType: TZReferenceItemType;
    FValue: string;
  public
    constructor Create(_Type: TZReferenceItemType; Value: string);
    function Clone: IZInterface; override;
    function ToString: string; override;

    function GetType: TZReferenceItemType;
    function GetValue: string;
  end;

  IZTableReference = interface;

  {** Defines an interface to field reference assembly. }
  IZFieldReference = interface (IZClonnable)
    ['{B1F41586-18FA-4D08-9F6D-83EE4AF15B9A}']
    function IsField: Boolean;
    function GetCatalog: string;
    function GetSchema: string;
    function GetTable: string;
    function GetField: string;
    function GetAlias: string;
    function GetTableReference: IZTableReference;
  end;

  {** Implements a field reference assembly. }
  TZFieldReference = class (TZAbstractObject, IZFieldReference)
  private
    FIsField: Boolean;
    FCatalog: string;
    FSchema: string;
    FTable: string;
    FField: string;
    FAlias: string;
    FTableRef: IZTableReference;
  public
    constructor Create(IsField: Boolean; Catalog, Schema, Table,
      Field, Alias: string; TableRef: IZTableReference);
    function Clone: IZInterface; override;
    function ToString: string; override;

    function IsField: Boolean;
    function GetCatalog: string;
    function GetSchema: string;
    function GetTable: string;
    function GetField: string;
    function GetAlias: string;
    function GetTableReference: IZTableReference;
  end;

  {** Defines an interface to table reference assembly. }
  IZTableReference = interface (IZClonnable)
    ['{4A319C60-4E61-4390-AF6E-56AED4C6833B}']
    function GetCatalog: string;
    function GetSchema: string;
    function GetTable: string;
    function GetAlias: string;
    function GetFullName: string;
  end;

  {** Implements a table reference assembly. }
  TZTableReference = class (TZAbstractObject, IZTableReference)
  private
    FCatalog: string;
    FSchema: string;
    FTable: string;
    FAlias: string;
  public
    constructor Create(Catalog, Schema, Table, Alias: string);
    function Clone: IZInterface; override;
    function ToString: string; override;

    function GetCatalog: string;
    function GetSchema: string;
    function GetTable: string;
    function GetAlias: string;
    function GetFullName: string;
  end;

  {** Defines an interface to select assembly. }
  IZSelectSchema = interface (IZClonnable)
    ['{3B892975-57E9-4EB7-8DB1-BDDED91E7FBC}']

    function GetFields: IZCollection;
    function GetTables: IZCollection;
    procedure LinkReferences(Convertor: IZIdentifierConvertor);

    function FindTableByFullName(Catalog, Schema, Table: string):
      IZTableReference;
    function FindTableByShortName(Table: string): IZTableReference;
    function FindFieldByShortName(Field: string): IZFieldReference;
  end;

  {** Implements a select assembly. }
  TZSelectSchema = class (TZAbstractObject, IZSelectSchema)
  private
    FFields: IZCollection;
    FTables: IZCollection;

    procedure ConvertIdentifiers(Convertor: IZIdentifierConvertor);
  public
    constructor Create;
    function Clone: IZInterface; override;
    function ToString: string; override;

    function GetFields: IZCollection;
    function GetTables: IZCollection;
    procedure LinkReferences(Convertor: IZIdentifierConvertor);

    function FindTableByFullName(Catalog, Schema, Table: string):
      IZTableReference;
    function FindTableByShortName(Table: string): IZTableReference;
    function FindFieldByShortName(Field: string): IZFieldReference;
  end;

  {**
    Implements an assembler for catalogs.
  }
  TZCatalogAssembler = class (TZAbstractAssembler)
  public
    procedure WorkOn(Assembly: IZAssembly); override;
  end;

  {**
    Implements an assembler for schemas.
  }
  TZSchemaAssembler = class (TZAbstractAssembler)
  public
    procedure WorkOn(Assembly: IZAssembly); override;
  end;

  {**
    Implements an assembler for tables.
  }
  TZTableAssembler = class (TZAbstractAssembler)
  public
    procedure WorkOn(Assembly: IZAssembly); override;
  end;

  {**
    Implements an assembler for fields.
  }
  TZFieldAssembler = class (TZAbstractAssembler)
  public
    procedure WorkOn(Assembly: IZAssembly); override;
  end;

  {**
    Implements an assembler for field and table aliases.
  }
  TZAliasAssembler = class (TZAbstractAssembler)
  public
    procedure WorkOn(Assembly: IZAssembly); override;
  end;

  {**
    Implements an assembler for field references.
  }
  TZFieldRefAssembler = class (TZAbstractAssembler)
  public
    procedure WorkOn(Assembly: IZAssembly); override;
  end;

  {**
    Implements an assembler for expression references.
  }
  TZExpressionRefAssembler = class (TZAbstractAssembler)
  public
    procedure WorkOn(Assembly: IZAssembly); override;
  end;

  {**
    Implements an assembler for table references.
  }
  TZTableRefAssembler = class (TZAbstractAssembler)
  public
    procedure WorkOn(Assembly: IZAssembly); override;
  end;

  {**
    Implements an assembler for select statement.
  }
  TZSelectAssembler = class (TZAbstractAssembler)
  public
    procedure WorkOn(Assembly: IZAssembly); override;
  end;

implementation

{ TZCatalogAssembler }

{**
  Performs an operations on specific assemblies.
  @param Assembly an assembly object to work on.
}
procedure TZCatalogAssembler.WorkOn(Assembly: IZAssembly);
var
  Temp: string;
begin
  Temp := (Assembly.Pop as IZAnyValue).GetString;
  Assembly.Push(TZReferenceItem.Create(riCatalog, Temp));
end;

{ TZSchemaAssembler }

{**
  Performs an operations on specific assemblies.
  @param Assembly an assembly object to work on.
}
procedure TZSchemaAssembler.WorkOn(Assembly: IZAssembly);
var
  Temp: string;
begin
  Temp := (Assembly.Pop as IZAnyValue).GetString;
  Assembly.Push(TZReferenceItem.Create(riSchema, Temp));
end;

{ TZTableAssembler }

{**
  Performs an operations on specific assemblies.
  @param Assembly an assembly object to work on.
}
procedure TZTableAssembler.WorkOn(Assembly: IZAssembly);
var
  Temp: string;
begin
  Temp := (Assembly.Pop as IZAnyValue).GetString;
  Assembly.Push(TZReferenceItem.Create(riTable, Temp));
end;

{ TZFieldAssembler }

{**
  Performs an operations on specific assemblies.
  @param Assembly an assembly object to work on.
}
procedure TZFieldAssembler.WorkOn(Assembly: IZAssembly);
var
  Temp: string;
begin
  Temp := (Assembly.Pop as IZAnyValue).GetString;
  Assembly.Push(TZReferenceItem.Create(riField, Temp));
end;

{ TZAliasAssembler }

{**
  Performs an operations on specific assemblies.
  @param Assembly an assembly object to work on.
}
procedure TZAliasAssembler.WorkOn(Assembly: IZAssembly);
var
  Temp: string;
begin
  Temp := (Assembly.Pop as IZAnyValue).GetString;
  Assembly.Push(TZReferenceItem.Create(riAlias, Temp));
end;

{ TZFieldRefAssembler }

{**
  Performs an operations on specific assemblies.
  @param Assembly an assembly object to work on.
}
procedure TZFieldRefAssembler.WorkOn(Assembly: IZAssembly);
var
  Temp: IZReferenceItem;
  Catalog, Schema, Table, Field, Alias: string;
begin
  Catalog := '';
  Schema := '';
  Table := '';
  Field := '';
  Alias := '';
  while (Assembly.Stack.Peek <> nil)
    and (Assembly.Stack.Peek.QueryInterface(IZReferenceItem, Temp) = 0) do
  begin
    Assembly.Pop;
    case Temp.GetType of
      riCatalog: Catalog := Temp.GetValue;
      riSchema: Schema := Temp.GetValue;
      riTable: Table := Temp.GetValue;
      riField: Field := Temp.GetValue;
      riAlias: Alias := Temp.GetValue;
    end;
  end;
  Assembly.Push(TZFieldReference.Create(True, Catalog, Schema, Table, Field,
    Alias, nil));
end;

{ TZExpressionRefAssembler }

{**
  Performs an operations on specific assemblies.
  @param Assembly an assembly object to work on.
}
procedure TZExpressionRefAssembler.WorkOn(Assembly: IZAssembly);
var
  Temp: IZReferenceItem;
begin
  if (Assembly.Stack.Peek <> nil)
    and (Assembly.Stack.Peek.QueryInterface(IZReferenceItem, Temp) = 0) then
  begin
    Assembly.Pop;
    Assembly.Push(TZFieldReference.Create(False, '', '', '', '',
      Temp.GetValue, nil));
  end;
end;

{ TZTableRefAssembler }

{**
  Performs an operations on specific assemblies.
  @param Assembly an assembly object to work on.
}
procedure TZTableRefAssembler.WorkOn(Assembly: IZAssembly);
var
  Temp: IZReferenceItem;
  Catalog, Schema, Table, Alias: string;
begin
  Catalog := '';
  Schema := '';
  Table := '';
  Alias := '';
  while (Assembly.Stack.Peek <> nil)
    and (Assembly.Stack.Peek.QueryInterface(IZReferenceItem, Temp) = 0) do
  begin
    Assembly.Pop;
    case Temp.GetType of
      riCatalog: Catalog := Temp.GetValue;
      riSchema: Schema := Temp.GetValue;
      riTable: Table := Temp.GetValue;
      riAlias: Alias := Temp.GetValue;
    end;
  end;
  Assembly.Push(TZTableReference.Create(Catalog, Schema, Table, Alias));
end;

{ TZSelectAssembler }

{**
  Performs an operations on specific assemblies.
  @param Assembly an assembly object to work on.
}
procedure TZSelectAssembler.WorkOn(Assembly: IZAssembly);
var
  Temp: IZObject;
  Schema: IZSelectSchema;
begin
  Schema := TZSelectSchema.Create;
  while Assembly.Stack.Peek <> nil do
  begin
    Temp := Assembly.Pop;
    if Temp.InstanceOf(IZFieldReference) then
      Schema.GetFields.Add(Temp)
    else if Temp.InstanceOf(IZTableReference) then
      Schema.GetTables.Add(Temp);
  end;
  Assembly.Push(Schema);
end;

{ TZReferenceItem }

{**
  Constructs this assembly and assign main properties.
  @param _Type a reference item type.
  @param Value a reference value.
}
constructor TZReferenceItem.Create(_Type: TZReferenceItemType; Value: string);
begin
  FType := _Type;
  FValue := Value;
end;

{**
  Clones an instance of this object.
  @return a clonned instance of this object.
}
function TZReferenceItem.Clone: IZInterface;
begin
  Result := TZReferenceItem.Create(FType, FValue);
end;

{**
  Gets a string representation for this object.
  @return a string representation for this object.
}
function TZReferenceItem.ToString: string;
begin
  case FType of
    riCatalog: Result := 'C:';
    riSchema: Result := 'S:';
    riTable: Result := 'T:';
    riField: Result := 'F:';
    riAlias: Result := 'A:';
  end;
  Result := Result + FValue;
end;

{**
  Gets a reference item type.
  @return a reference item type.
}
function TZReferenceItem.GetType: TZReferenceItemType;
begin
  Result := FType;
end;

{**
  Gets a reference item value.
  @return a reference item value.
}
function TZReferenceItem.GetValue: string;
begin
  Result := FValue;
end;

{ TZFieldReference }

{**
  Constructs this assembly and assignes the main properties.
  @param IsField <code>True</code> for field columns.
  @param Catalog a database catalog name.
  @param Schema a database schema name.
  @param Table a database table name.
  @param Field a table field name.
  @param Alias an alias name.
  @param TableRef a related table reference object.
}
constructor TZFieldReference.Create(IsField: Boolean;
  Catalog, Schema, Table, Field, Alias: string; TableRef: IZTableReference);
begin
  FIsField := IsField;
  FCatalog := Catalog;
  FSchema := Schema;
  FTable := Table;
  FField := Field;
  FAlias := Alias;
  FTableRef := TableRef;
end;

{**
  Clones an instance of this object.
  @return a clonned instance of this object.
}
function TZFieldReference.Clone: IZInterface;
begin
  Result := TZFieldReference.Create(FIsField, FCatalog, FSchema, FTable,
    FField, FAlias, nil);
end;

{**
  Gets a string representation for this object.
  @return a string representation for this object.
}
function TZFieldReference.ToString: string;
begin
  Result := FField;
  if FTable <> '' then
    Result := FTable + '.' + Result;
  if FSchema <> '' then
    Result := FSchema + '.' + Result;
  if FCatalog <> '' then
    Result := FCatalog + '.' + Result;
  if FAlias <> '' then
    Result := Result + '/' + FAlias;
  Result := 'FR:' + Result;
end;

{**
  Gets a field alias name.
  @return a field alias name.
}
function TZFieldReference.GetAlias: string;
begin
  Result := FAlias;
end;

{**
  Gets a table field name.
  @return a table field name.
}
function TZFieldReference.GetField: string;
begin
  Result := FField;
end;

{**
  Checks is this column a field.
  @return <code>True</code> for field and <code>False</code> for expression.
}
function TZFieldReference.IsField: Boolean;
begin
  Result := FIsField;
end;

{**
  Gets a database catalog name.
  @return a database catalog name.
}
function TZFieldReference.GetCatalog: string;
begin
  Result := FCatalog;
end;

{**
  Gets a database schema name.
  @return a database schema name.
}
function TZFieldReference.GetSchema: string;
begin
  Result := FSchema;
end;

{**
  Gets a database table name.
  @return a database table name.
}
function TZFieldReference.GetTable: string;
begin
  Result := FTable;
end;

{**
  Gets a related table reference object.
  @return a related table reference object.
}
function TZFieldReference.GetTableReference: IZTableReference;
begin
  Result := FTableRef;
end;


{ TZTableReference }

{**
  Constructs this assembly and assignes the main properties.
  @param Catalog a database catalog name.
  @param Schema a database schema name.
  @param Table a database table name.
  @param Alias an alias name.
}
constructor TZTableReference.Create(Catalog, Schema, Table, Alias: string);
begin
  FCatalog := Catalog;
  FSchema := Schema;
  FTable := Table;
  FAlias := Alias;
end;

{**
  Clones an instance of this object.
  @return a clonned instance of this object.
}
function TZTableReference.Clone: IZInterface;
begin
  Result := TZTableReference.Create(FCatalog, FSchema, FTable, FAlias);
end;

{**
  Gets a string representation for this object.
  @return a string representation for this object.
}
function TZTableReference.ToString: string;
begin
  Result := FTable;
  if FSchema <> '' then
    Result := FSchema + '.' + Result;
  if FCatalog <> '' then
    Result := FCatalog + '.' + Result;
  if FAlias <> '' then
    Result := Result + '/' + FAlias;
  Result := 'TR:' + Result;
end;

{**
  Gets a table alias name.
  @return a table alias name.
}
function TZTableReference.GetAlias: string;
begin
  Result := FAlias;
end;

{**
  Gets a database catalog name.
  @return a database catalog name.
}
function TZTableReference.GetCatalog: string;
begin
  Result := FCatalog;
end;

{**
  Gets a database schema name.
  @return a database schema name.
}
function TZTableReference.GetSchema: string;
begin
  Result := FSchema;
end;

{**
  Gets a database table name.
  @return a database table name.
}
function TZTableReference.GetTable: string;
begin
  Result := FTable;
end;

{**
  Gets a full database table name.
  @return a full database table name.
}
function TZTableReference.GetFullName: string;
begin
  Result := FTable;
  if FCatalog <> '' then
    Result := FCatalog + '.' + Result;
  if FSchema <> '' then
    Result := FSchema + '.' + Result;
end;

{ TZSelectSchema }

{**
  Constructs this assembly object and assignes the main properties.
}
constructor TZSelectSchema.Create;
begin
  FFields := TZCollection.Create;
  FTables := TZCollection.Create;
end;

{**
  Clones an instance of this object.
  @return a clonned instance of this object.
}
function TZSelectSchema.Clone: IZInterface;
var
  SelectAssembly: TZSelectSchema;
begin
  SelectAssembly := TZSelectSchema.Create;
  SelectAssembly.FFields.AddAll(FFields);
  SelectAssembly.FTables.AddAll(FTables);
  Result := SelectAssembly;
end;

{**
  Gets a string representation for this object.
  @return a string representation for this object.
}
function TZSelectSchema.ToString: string;
begin
  Result := 'SS:' + FFields.ToString + FTables.ToString;
end;

{**
  Gets a list of select fields.
  @return a collection with select fields.
}
function TZSelectSchema.GetFields: IZCollection;
begin
  Result := FFields;
end;

{**
  Gets a lias of select tables.
  @return a collection with select tables.
}
function TZSelectSchema.GetTables: IZCollection;
begin
  Result := FTables;
end;

{**
  Finds a table reference by catalog and table name.
  @param Catalog a database catalog name.
  @param Schema a database schema name.
  @param Table a database table name.
  @return a found table reference object or <code>null</code> otherwise.
}
function TZSelectSchema.FindTableByFullName(Catalog, Schema, Table: string):
  IZTableReference;
var
  I: Integer;
  Current: IZTableReference;
begin
  Result := nil;
  { Looks a table by it's full name. }
  for I := 0 to FTables.Count - 1 do
  begin
    Current := FTables[I] as IZTableReference;
    if (Current.GetSchema = Schema) and (Current.GetTable = Table) then
    begin
      Result := Current;
      Exit;
    end;
  end;

  { Looks a table by it's short name. }
  for I := 0 to FTables.Count - 1 do
  begin
    Current := FTables[I] as IZTableReference;
    if (Current.GetSchema = '') and (Current.GetTable = Table) then
    begin
      Result := Current;
      Exit;
    end;
  end;
end;

{**
  Finds a table reference by table name or table alias.
  @param Table a database table name or alias.
  @return a found table reference object or <code>null</code> otherwise.
}
function TZSelectSchema.FindTableByShortName(Table: string): IZTableReference;
var
  I: Integer;
  Current: IZTableReference;
begin
  Result := nil;
  { Looks a table by it's alias. }
  for I := 0 to FTables.Count - 1 do
  begin
    Current := FTables[I] as IZTableReference;
    if Current.GetAlias = Table then
    begin
      Result := Current;
      Exit;
    end;
  end;

  { Looks a table by it's name. }
  for I := 0 to FTables.Count - 1 do
  begin
    Current := FTables[I] as IZTableReference;
    if Current.GetTable = Table then
    begin
      Result := Current;
      Exit;
    end;
  end;
end;

{**
  Finds a field reference by field name or field alias.
  @param Field a table field name or alias.
  @return a found field reference object or <code>null</code> otherwise.
}
function TZSelectSchema.FindFieldByShortName(Field: string): IZFieldReference;
var
  I: Integer;
  Current: IZFieldReference;
begin
  Result := nil;
  { Looks a field by it's alias. }
  for I := 0 to FFields.Count - 1 do
  begin
    Current := FFields[I] as IZFieldReference;
    if Current.GetAlias = Field then
    begin
      Result := Current;
      Exit;
    end;
  end;

  { Looks a field by it's name. }
  for I := 0 to FFields.Count - 1 do
  begin
    Current := FFields[I] as IZFieldReference;
    if Current.GetField = Field then
    begin
      Result := Current;
      Exit;
    end;
  end;
end;

{**
  Convert all table and field identifiers..
  @param Convertor an identifier convertor.
}
procedure TZSelectSchema.ConvertIdentifiers(Convertor: IZIdentifierConvertor);
var
  I: Integer;
  Temp: IZCollection;
  FieldRef: IZFieldReference;
  TableRef: IZTableReference;
begin
  if Convertor = nil then Exit;

  Temp := TZCollection.Create;
  for I := 0 to FFields.Count - 1 do
  begin
    FieldRef := FFields[I] as IZFieldReference;
    FieldRef := TZFieldReference.Create(FieldRef.IsField,
      Convertor.ExtractQuote(FieldRef.GetCatalog),
      Convertor.ExtractQuote(FieldRef.GetSchema),
      Convertor.ExtractQuote(FieldRef.GetTable),
      Convertor.ExtractQuote(FieldRef.GetField),
      Convertor.ExtractQuote(FieldRef.GetAlias),
      FieldRef.GetTableReference);
    Temp.Add(FieldRef);
  end;
  FFields := Temp;

  Temp := TZCollection.Create;
  for I := 0 to FTables.Count - 1 do
  begin
    TableRef := FTables[I] as IZTableReference;
    TableRef := TZTableReference.Create(
      Convertor.ExtractQuote(TableRef.GetCatalog),
      Convertor.ExtractQuote(TableRef.GetSchema),
      Convertor.ExtractQuote(TableRef.GetTable),
      Convertor.ExtractQuote(TableRef.GetAlias));
    Temp.Add(TableRef);
  end;
  FTables := Temp;
end;

{**
  Links references between fields and tables.
  @param Convertor an identifier convertor.
}
procedure TZSelectSchema.LinkReferences(Convertor: IZIdentifierConvertor);
var
  I, J: Integer;
  Current: IZFieldReference;
  FieldRef: IZFieldReference;
  TableRef: IZTableReference;
  Temp: IZCollection;
begin
  ConvertIdentifiers(Convertor);

  Temp := TZCollection.Create;
  for I := 0 to FFields.Count - 1 do
  begin
    Current := FFields[I] as IZFieldReference;
    if not Current.IsField then
    begin
      Temp.Add(Current);
      Continue;
    end
    else if (Current.GetSchema <> '') and (Current.GetTable <> '') then
    begin
      TableRef := FindTableByFullName(Current.GetCatalog, Current.GetSchema,
        Current.GetTable);
    end
    else if Current.GetTable <> '' then
      TableRef := FindTableByShortName(Current.GetTable)
    else if Current.GetField = '*' then
    begin
      { Add all fields from all tables. }
      for J := 0 to FTables.Count - 1 do
      begin
        TableRef := FTables[J] as IZTableReference;
        FieldRef := TZFieldReference.Create(True, TableRef.GetCatalog,
          TableRef.GetSchema, TableRef.GetTable, '*', '', TableRef);
        Temp.Add(FieldRef);
      end;
      Continue;
    end else
      TableRef := nil;

    if TableRef <> nil then
    begin
      FieldRef := TZFieldReference.Create(True, TableRef.GetCatalog,
        TableRef.GetSchema, TableRef.GetTable, Current.GetField,
        Current.GetAlias, TableRef);
    end
    else
    begin
      FieldRef := TZFieldReference.Create(True, Current.GetCatalog,
        Current.GetSchema, Current.GetTable, Current.GetField,
        Current.GetAlias, TableRef);
    end;
    Temp.Add(FieldRef);
  end;
  FFields := Temp;
end;

end.

