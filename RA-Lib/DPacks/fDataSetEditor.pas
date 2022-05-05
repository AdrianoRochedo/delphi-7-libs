{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 192001 Andrei Prygounkov

       description : DataSet Fields Editor

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit fDataSetEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBCtrls, ExtCtrls, StdCtrls, Db, Menus,
  DsgnWnds, DsgnIntf;

type

  TRADataSetDesigner = class;

  TDataSetEditor = class(TDesignWindow)
    LocalMenu: TPopupMenu;
    AddItem: TMenuItem;
    NewItem: TMenuItem;
    N1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    DeleteItem: TMenuItem;
    SelectAllItem: TMenuItem;
    N2: TMenuItem;
    RetrieveItem: TMenuItem;
    UpdateItem: TMenuItem;
    SaveAttributesAsItem: TMenuItem;
    AssociateItem: TMenuItem;
    Unassociate: TMenuItem;
    DataSource: TDataSource;
    FieldListBox: TListBox;
    Panel1: TPanel;
    DBNavigator: TDBNavigator;
    procedure FormDestroy(Sender: TObject);
    procedure FieldListBoxClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure AddNewItemClick(Sender: TObject);
    procedure DeleteItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SelectAllItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FDataSetDesigner: TRADataSetDesigner;
    procedure UpdateFields;
  public
    procedure FormModified; override;
  end;

  TRADataSetDesigner = class(TDataSetDesigner)
  private
    DataSetEditor: TDataSetEditor;
  public
    destructor Destroy; override;
  end;

 {$IFDEF RA_D3}
  procedure ShowDatasetDesigner(ADesigner: TFormDesigner; ADataSet: TDataSet);
 {$ELSE}
  procedure ShowDatasetDesigner(ADesigner: IFormDesigner; ADataSet: TDataSet);
 {$ENDIF}

implementation

uses packconst, fAddFields, fDefineField, iMTracer;

{$R *.dfm}

 {$IFDEF RA_D5H}
type
  TComponentList = TDesignerSelectionList;
 {$ENDIF}

{$IFDEF RA_D3}
procedure ShowDatasetDesigner(ADesigner: TFormDesigner; ADataSet: TDataSet);
{$ELSE}
procedure ShowDatasetDesigner(ADesigner: IFormDesigner; ADataSet: TDataSet);
{$ENDIF}
var
  DataSetEditor: TDataSetEditor;
begin
  if ADataSet.Designer = nil then
  begin
    DataSetEditor := TDataSetEditor.Create(Application);
    DataSetEditor.Designer := ADesigner;
    DataSetEditor.FDataSetDesigner := TRADataSetDesigner.Create(ADataSet);
    DataSetEditor.FDataSetDesigner.DataSetEditor := DataSetEditor;
    DataSetEditor.DataSource.DataSet := ADataSet;
    DataSetEditor.Caption := ADesigner.GetComponentName(ADesigner.GetRoot) + '.' +
      ADesigner.GetComponentName(ADataSet);
  end
  else
    DataSetEditor := (ADataSet.Designer as TRADataSetDesigner).DataSetEditor;
  DataSetEditor.UpdateFields;
  DataSetEditor.Show;
end;

destructor TRADataSetDesigner.Destroy;
begin
  if Assigned(DataSetEditor) then
  begin
    DataSetEditor.FDataSetDesigner := nil;
    DataSetEditor.Free;
  end;
  inherited Destroy;
end;    { Destroy }

procedure TDatasetEditor.FormDestroy(Sender: TObject);
begin
  if Assigned(FDataSetDesigner) then
  begin
    FDataSetDesigner.DataSetEditor := nil;
    FDataSetDesigner.Free;
  end;
end;

procedure TDatasetEditor.UpdateFields;
var
  i: Integer;
begin
  FieldListBox.Clear;
  if not FDataSetDesigner.DataSet.DefaultFields then
    for i := 0 to FDataSetDesigner.DataSet.FieldCount - 1 do
      FieldListBox.Items.Add(FDataSetDesigner.DataSet.Fields[i].FieldName);
end;    { ReadFields }

procedure TDatasetEditor.FieldListBoxClick(Sender: TObject);
var
  i: integer;
  Selection: TComponentList;
begin
  if FieldListBox.SelCount > 0 then
  begin
    Selection := TComponentList.Create;
    try
      for i := 0 to FieldListBox.Items.Count - 1 do
        if FieldListBox.Selected[i] then
          Selection.Add(FDataSetDesigner.DataSet.Fields[i]);
      Designer.SetSelections(Selection);
    finally
      Selection.Free;
    end;    { try/finally }
  end
  else
    Designer.SelectComponent(FDataSetDesigner.DataSet);
end;

procedure TDatasetEditor.FormActivate(Sender: TObject);
begin
  FieldListBoxClick(nil);
end;

procedure TDatasetEditor.AddNewItemClick(Sender: TObject);
var
  FC: Integer;
begin
  FC := FieldListBox.Items.Count;
  if ((Sender = AddItem) and AddFields(FDataSetDesigner.DataSet)) or
     ((Sender = NewItem) and DefineField(FDataSetDesigner.DataSet)) then
  begin
    UpdateFields;
    for FC := FC to FieldListBox.Items.Count - 1 do
      FieldListBox.Selected[FC] := True;
    FieldListBoxClick(nil);
  end;
end;

procedure TDatasetEditor.DeleteItemClick(Sender: TObject);
var
  i: Integer;
begin
  Designer.SelectComponent(FDataSetDesigner.DataSet);
  for i := FieldListBox.Items.Count - 1 downto 0 do
    if FieldListBox.Selected[i] then
    begin
      FDataSetDesigner.DataSet.Fields[i].Free;
      FieldListBox.Items.Delete(i);
    end;    { for }
  if FieldListBox.Items.Count > 0 then
    FieldListBox.Selected[FieldListBox.Items.Count - 1] := True;
  FieldListBoxClick(nil);
end;

procedure TDatasetEditor.SelectAllItemClick(Sender: TObject);
begin
  FieldListBox.Perform(LB_SELITEMRANGE, 1, High(Word));
end;

procedure TDatasetEditor.FormCreate(Sender: TObject);
begin
  AddItem.Caption := SDEAddItem;
  DeleteItem.Caption := SDEDeleteItem;
  SelectAllItem.Caption := SDESelectAllItem;
  NewItem.Caption := SDENewItem;
end;

procedure TDatasetEditor.FormModified;
var
  i: Integer;
begin
  if not FDataSetDesigner.DataSet.DefaultFields then
    for i := 0 to FDataSetDesigner.DataSet.FieldCount - 1 do
      if FieldListBox.Items[i] <> FDataSetDesigner.DataSet.Fields[i].FieldName then
        FieldListBox.Items[i] := FDataSetDesigner.DataSet.Fields[i].FieldName;
end;

procedure TDataSetEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  { don't uncomment next line ! }
  // Action := caFree;
end;

end.
