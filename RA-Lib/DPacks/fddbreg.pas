{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1998-2001 Andrei Prygounkov

       description : R&A implemetation of
                     Delphi design-time packages

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm/ralib
************************************************************}

{$INCLUDE RA.INC}

unit fddbreg;

interface

procedure Register;

implementation

uses
  SysUtils, Classes, DsgnIntf, Controls, Forms, DB, DBTables, DBCtrls,
  DBGrids, DBCGrids, Mask, DBConsts, {DBLookup,}
  TypInfo, EditIntf, ExptIntf, ToolIntf, fDataSetEditor,
  fCollectionEditor, fDBGridColumnsEditor,
  packconst;

{$R dbreg.res}


const
  SDAccess         = 'Data Access';
  SDControls       = 'Data Controls';
  SWin31           = 'Win 3.1';


{ TDataSetProperty }

type
  TDataSetProperty = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TDataSetProperty.CheckComponent(const Value: string);
var
  J: Integer;
  Dataset: TDataset;
begin
  Dataset := TDataset(Designer.GetComponent(Value));
  for J := 0 to PropCount - 1 do
    if TDataSource(GetComponent(J)).IsLinkedTo(Dataset) then
      Exit;
  FCheckProc(Value);
end;

procedure TDataSetProperty.GetValues(Proc: TGetStrProc);
begin
  FCheckProc := Proc;
  inherited GetValues(CheckComponent);
end;

{ TDataSourceProperty }

type
  TDataSourceProperty = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure TDataSourceProperty.CheckComponent(const Value: string);
var
  J: Integer;
  DataSource: TDataSource;
begin
  DataSource := TDataSource(Designer.GetComponent(Value));
  for J := 0 to PropCount - 1 do
    if TDataSet(GetComponent(J)).IsLinkedTo(DataSource) then
      Exit;
  FCheckProc(Value);
end;

procedure TDataSourceProperty.GetValues(Proc: TGetStrProc);
begin
  FCheckProc := Proc;
  inherited GetValues(CheckComponent);
end;

{ TDBStringProperty }

type
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ TSessionNameProperty }

type
  TSessionNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TSessionNameProperty.GetValueList(List: TStrings);
begin
  Sessions.GetSessionNames(List);
end;

{ TDatabaseNameProperty }

type
  TDatabaseNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TDatabaseNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TDBDataSet).DBSession.GetDatabaseNames(List);
end;

{ TAliasNameProperty }

type
  TAliasNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TAliasNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TDatabase).Session.GetAliasNames(List);
end;

{ TDriverNameProperty }

type
  TDriverNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TDriverNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TDatabase).Session.GetDriverNames(List);
end;

{ TTableNameProperty }

type
  TTableNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TTableNameProperty.GetValueList(List: TStrings);
const
 {$IFDEF RA_D3}
  Masks: array[TTableType] of string[5] = ('', '*.DB', '*.DBF', '*.TXT');
 {$ELSE}
  Masks: array[TTableType] of string[5] = ('', '*.DB', '*.DBF', '*.TXT', '*.DBF');
 {$ENDIF}
var
  Table: TTable;
begin
  Table := GetComponent(0) as TTable;
  Table.DBSession.GetTableNames(Table.DatabaseName, Masks[Table.TableType],
    Table.TableType = ttDefault, False, List);
end;

{ TIndexNameProperty }

type
  TIndexNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TIndexNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TTable).GetIndexNames(List);
end;

{ TProcedureNameProperty }

type
  TProcedureNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TProcedureNameProperty.GetValueList(List: TStrings);
var
  DBDataSet: TDBDataSet;
begin
  DBDataSet := GetComponent(0) as TDBDataSet;
  DBDataSet.DBSession.GetStoredProcNames(DBDataSet.DatabaseName, List);
end;

{ TIndexFieldNamesProperty }

type
  TIndexFieldNamesProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TIndexFieldNamesProperty.GetValueList(List: TStrings);
var
  I: Integer;
begin
  with GetComponent(0) as TTable do
  begin
    IndexDefs.Update;
    for I := 0 to IndexDefs.Count - 1 do
      with IndexDefs[I] do
        if not (ixExpression in Options) then List.Add(Fields);
  end;
end;

{ TDataFieldProperty }

type
  TDataFieldProperty = class(TDBStringProperty)
  public
    function GetDataSourcePropName: string; virtual;
    procedure GetValueList(List: TStrings); override;
  end;

function TDataFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'DataSource';
end;

procedure TDataFieldProperty.GetValueList(List: TStrings);
var
  Instance: TPersistent;
  PropInfo: PPropInfo;
  DataSource: TDataSource;
begin
  Instance := GetComponent(0);
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, GetDataSourcePropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    DataSource := TObject(GetOrdProp(Instance, PropInfo)) as TDataSource;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
      DataSource.DataSet.GetFieldNames(List);
  end;
end;

{ TLookupSourceProperty }

type
  TLookupSourceProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TLookupSourceProperty.GetValueList(List: TStrings);
begin
  with GetComponent(0) as TField do
    if DataSet <> nil then DataSet.GetFieldNames(List);
end;

{ TLookupDestProperty }

type
  TLookupDestProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TLookupDestProperty.GetValueList(List: TStrings);
begin
  with GetComponent(0) as TField do
    if LookupDataSet <> nil then LookupDataSet.GetFieldNames(List);
end;

{ TListFieldProperty }

type
  TListFieldProperty = class(TDataFieldProperty)
  public
    function GetDataSourcePropName: string; override;
  end;

function TListFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'ListSource';
end;

{ TLookupFieldProperty }

type
  TLookupFieldProperty = class(TDataFieldProperty)
  public
    function GetDataSourcePropName: string; override;
  end;

function TLookupFieldProperty.GetDataSourcePropName: string;
begin
  Result := 'LookupSource';
end;

{ TLookupIndexProperty }

type
  TLookupIndexProperty = class(TLookupFieldProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TLookupIndexProperty.GetValueList(List: TStrings);
var
  Instance: TPersistent;
  PropInfo: PPropInfo;
  DataSource: TDataSource;
begin
  Instance := GetComponent(0);
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, GetDataSourcePropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
  begin
    DataSource := TObject(GetOrdProp(Instance, PropInfo)) as TDataSource;
    if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    begin
      if (DataSource.DataSet is TTable) and
          (TTable(DataSource.DataSet).IndexFieldCount > 0) then
        List.Add(TTable(DataSource.DataSet).IndexFields[0].FieldName)
      else
        DataSource.DataSet.GetFieldNames(List);
    end;
  end;
end;

{ TDBImageEditor }

type
  TDBImageEditor = class(TDefaultEditor)
  public
    procedure Copy; override;
  end;

procedure TDBImageEditor.Copy;
begin
  TDBImage(Component).CopyToClipboard;
end;


{ TDataSetEditor }

type
  TDataSetEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TDataSetEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowDatasetDesigner(Designer, TTable(Component));
  end;
end;

function TDataSetEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SDEDatasetDesigner;
  end;
end;

function TDataSetEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


{ TCollectionProperty }

type
  TCollectionProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure TCollectionProperty.Edit;
begin
  ShowCollectionDesigner(TCollectionEditor, Designer, TCollection(GetOrdValue));
end;

function TCollectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;


{ TDBColumnAttributesProperty }

type
  TDBColumnAttributesProperty = class(TCollectionProperty)
  public
    procedure Edit; override;
  end;

procedure TDBColumnAttributesProperty.Edit;
begin
  ShowCollectionDesigner(TDBGridColumnsEditor, Designer,
    TDBGridColumns(GetOrdValue));
end;

{ TDBGridEditor }
type
  TDBGridEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TDBGridEditor.ExecuteVerb(Index: Integer);
begin
  ShowCollectionDesigner(TDBGridColumnsEditor, Designer,
    TDBGrid(Component).Columns);
end;

function TDBGridEditor.GetVerb(Index: Integer): string;
begin
  Result := SDBGridColEditor;
end;

function TDBGridEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TDBGridColumnFieldNameProperty }

type
  TColumnFieldNameProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TColumnFieldNameProperty.GetValueList(List: TStrings);
var
  Column: DBGrids.TColumn;
begin
  Column := GetComponent(0) as DBGrids.TColumn;
  if Assigned(TDBGrid(Column.Grid).DataSource) and
     Assigned(TDBGrid(Column.Grid).DataSource.DataSet) then
      TDBGrid(Column.Grid).DataSource.DataSet.GetFieldNames(List);
end;




{ Registration }

procedure Register;
begin
  RegisterComponents(sDAccess, [TDataSource, TTable, TQuery,
    TStoredProc, TDatabase, TSession, TBatchMove, TUpdateSQL]);
  RegisterComponents(sDControls, [TDBGrid, TDBNavigator, TDBText,
    TDBEdit, TDBMemo, TDBImage, TDBListBox, TDBComboBox, TDBCheckBox,
    TDBRadioGroup, TDBLookupListBox, TDBLookupComboBox]);
  RegisterComponents(sDControls, [TDBCtrlGrid]);
//  RegisterComponents(sWin31, [TDBLookupList, TDBLookupCombo]);
  RegisterNoIcon([TField]);
  RegisterFields([TStringField, TIntegerField, TSmallintField, TWordField,
    TFloatField, TCurrencyField, TBCDField, TBooleanField, TDateField,
    TVarBytesField, TBytesField, TTimeField, TDateTimeField,
    TBlobField, TMemoField, TGraphicField, TAutoIncField]);
{  RegisterNoIcon([TStringField, TIntegerField, TSmallintField, TWordField,
    TFloatField, TCurrencyField, TBCDField, TBooleanField, TDateField,
    TVarBytesField, TBytesField, TTimeField, TDateTimeField,
    TBlobField, TMemoField, TGraphicField, TAutoIncField]); }
  RegisterPropertyEditor(TypeInfo(TDataSet), TDataSource, 'DataSet', TDataSetProperty);
  RegisterPropertyEditor(TypeInfo(TDataSource), TTable, 'MasterSource', TDataSourceProperty);
  RegisterPropertyEditor(TypeInfo(TDataSource), TQuery, 'DataSource', TDataSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TDatabase, 'AliasName', TAliasNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDatabase, 'DriverName', TDriverNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDatabase, 'SessionName', TSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBDataSet, 'SessionName', TSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBDataSet, 'DatabaseName', TDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(TDataSetUpdateObject), TDataSet, 'UpdateObject', TComponentProperty);
  RegisterPropertyEditor(TypeInfo(string), TTable, 'TableName', TTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TTable, 'IndexName', TIndexNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TTable, 'IndexFieldNames', TIndexFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(string), TField, 'KeyFields', TLookupSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TField, 'LookupKeyFields', TLookupDestProperty);
  RegisterPropertyEditor(TypeInfo(string), TField, 'LookupResultField', TLookupDestProperty);
  RegisterPropertyEditor(TypeInfo(string), TComponent, 'DataField', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBLookupControl, 'KeyField', TListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBLookupControl, 'ListField', TListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TWinControl, 'LookupField', TLookupIndexProperty);
  RegisterPropertyEditor(TypeInfo(string), TWinControl, 'LookupDisplay', TLookupFieldProperty);

  RegisterComponentEditor(TDataset, TDataSetEditor);
  RegisterComponentEditor(TDBGrid, TDBGridEditor);
  RegisterPropertyEditor(TypeInfo(TDBGridColumns), TDBGrid, 'Columns', TDBColumnAttributesProperty);
  RegisterPropertyEditor(TypeInfo(string), TColumn, 'FieldName', TColumnFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(TCheckConstraints), TDataSet, 'Constraints', TCollectionProperty);

{
  RegisterPropertyEditor(TypeInfo(string), TDBEdit, 'EditMask', TMaskProperty);
  RegisterPropertyEditor(TypeInfo(string), TField, 'EditMask', TMaskProperty);

  RegisterPropertyEditor(TypeInfo(string), TTable, 'MasterFields', TFieldLinkProperty);
  RegisterPropertyEditor(TypeInfo(TParams), TQuery, 'Params', TQueryParamsProperty);
  RegisterPropertyEditor(TypeInfo(string), TStoredProc, 'StoredProcName', TProcedureNameProperty);
  RegisterPropertyEditor(TypeInfo(TParams), TStoredProc, 'Params', TStoredParamsProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TTable, 'IndexFiles', TIndexFilesProperty);
  RegisterPropertyEditor(TypeInfo(TDBGridColumns), nil, '', TDBColumnAttributesProperty);
  RegisterComponentEditor(TDataset, TDataSetEditor);
  RegisterComponentEditor(TDatabase, TDatabaseEditor);
  RegisterComponentEditor(TBatchMove, TBatchMoveEditor);
  RegisterComponentEditor(TQuery, TQueryEditor);
  RegisterComponentEditor(TDBImage, TDBImageEditor);
  RegisterComponentEditor(TStoredProc, TStoredProcEditor);
  RegisterComponentEditor(TUpdateSQL, TUpdateSQLEditor);
  DBExpt.Register;
}
end;

end.
