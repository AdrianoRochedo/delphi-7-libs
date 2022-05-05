{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1998-2001 Andrei Prygounkov

       R&A implemetation of Delphi design-time packages

       description : Collection Editor

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm/ralib
************************************************************}

{$INCLUDE RA.INC}

unit fCollectionEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, ExtCtrls, RARegAuto,
  DsgnIntf, DsgnWnds, ExptIntf;

type
  TCollectionEditor = class(TDesignWindow)
    Panel3: TPanel;
    Panel1: TPanel;
    Add: TButton;
    Delete: TButton;
    MoveUp: TButton;
    MoveDown: TButton;
    ListView1: TListView;
    ColnEditPopup: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    SelectAll1: TMenuItem;
    N1: TMenuItem;
    ShowButtons1: TMenuItem;
    RegAuto1: TRegAuto;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure MoveUpClick(Sender: TObject);
    procedure MoveDownClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FCollection: TCollection;
   {$IFDEF RA_D3}
    FDesigner: TFormDesigner;
   {$ELSE}
    FDesigner: IFormDesigner;
   {$ENDIF}
    procedure WMGetMinMaxInfo(var M : TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  protected
    procedure SelectCurrent;
    procedure UpdateList;
    procedure UpdateListNames;
  public
    procedure FormModified; override;
    property Collection: TCollection read FCollection;
   {$IFDEF RA_D3}
    property Designer: TFormDesigner read FDesigner;
   {$ELSE}
    property Designer: IFormDesigner read FDesigner;
   {$ENDIF}
  end;

  TCollectionEditorClass = class of TCollectionEditor;

 {$IFDEF RA_D3}
  procedure ShowCollectionDesigner(ACollectionEditorClass: TCollectionEditorClass;
    ADesigner: TFormDesigner; ACollection: TCollection);
 {$ELSE}
  procedure ShowCollectionDesigner(ACollectionEditorClass: TCollectionEditorClass;
    ADesigner: IFormDesigner; ACollection: TCollection);
 {$ENDIF}

implementation

uses packconst;

{$R *.dfm}

type

  THackCollection = class(TCollection)
  end;

 {$IFDEF RA_D5H}
  TComponentList = TDesignerSelectionList;
 {$ENDIF}

{$IFDEF RA_D3}
procedure ShowCollectionDesigner(ACollectionEditorClass: TCollectionEditorClass;
  ADesigner: TFormDesigner; ACollection: TCollection);
{$ELSE}
procedure ShowCollectionDesigner(ACollectionEditorClass: TCollectionEditorClass;
  ADesigner: IFormDesigner; ACollection: TCollection);
{$ENDIF}
var
  i: Integer;
begin
  for i := 0 to Application.ComponentCount - 1 do
    if (Application.Components[i] is ACollectionEditorClass) and
       ((Application.Components[i] as ACollectionEditorClass).Collection =
        ACollection) then
    begin
      (Application.Components[i] as ACollectionEditorClass).Show;
      Exit;
    end;
  with ACollectionEditorClass.Create(Application) do
  begin
    FDesigner := ADesigner;
    FCollection := ACollection;
    Show;
  end;
end;    { ShowCollectionDesigner }

procedure TCollectionEditor.FormShow(Sender: TObject);
begin
  RegAuto1.Load;
  UpdateList;
end;

procedure TCollectionEditor.WMGetMinMaxInfo(var M : TWMGetMinMaxInfo);
begin
  inherited;
  if Visible then
  begin
    M.MinMaxInfo^.ptMinTrackSize.Y := Height - ClientHeight +
      MoveDown.BoundsRect.Bottom;
    M.MinMaxInfo^.ptMinTrackSize.X := Panel1.Width * 2;
  end;
end;

procedure TCollectionEditor.UpdateList;
var
  i: Integer;
begin
  Caption := Format(SCEEditCollection, [Designer.GetComponentName(Designer.GetRoot) + '.' +
    TComponent(THackCollection(Collection).GetOwner).Name + '.' +
    THackCollection(Collection).PropName]);
  if ListView1.Items.Count = Collection.Count then UpdateListNames
  else
  begin
    ListView1.Items.Clear;
    for i := 0 to Collection.Count - 1 do
    begin
      with ListView1.Items.Add do
      begin
        Caption := IntToStr(i) + ' - ' + Collection.Items[i].DisplayName;
      end;
    end;    { for }
  end;
end;    { UpdateList }

procedure TCollectionEditor.UpdateListNames;
var
  i: Integer;
begin
  for i := 0 to Collection.Count - 1 do
    ListView1.Items[i].Caption := IntToStr(i) + ' - ' +
      Collection.Items[i].DisplayName;
end;    { UpdateListNames }

procedure TCollectionEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TCollectionEditor.ListView1Click(Sender: TObject);
begin
  SelectCurrent;
end;

procedure TCollectionEditor.FormActivate(Sender: TObject);
begin
  SelectCurrent;
end;

procedure TCollectionEditor.SelectCurrent;
var
  i: integer;
  Selection: TComponentList;
begin
  if ListView1.Selected <> nil then
  begin
    Selection := TComponentList.Create;
    try
      for i := 0 to ListView1.Items.Count - 1 do
        if ListView1.Items[i].Selected then
          Selection.Add(Collection.Items[i]);
      Designer.SetSelections(Selection);
    finally
      Selection.Free;
    end;    { try/finally }
  end
  else
    Designer.SelectComponent(THackCollection(Collection).GetOwner);
end;    { UpdateCurrent }

procedure TCollectionEditor.AddClick(Sender: TObject);
begin
  Collection.Add;
  UpdateList;
end;

procedure TCollectionEditor.DeleteClick(Sender: TObject);
var
  i: Integer;
begin
  Designer.SelectComponent(THackCollection(Collection).GetOwner);
  for i := ListView1.Items.Count - 1 downto 0 do
    if ListView1.Items[i].Selected then
    begin
      Collection.Items[i].Free;
      UpdateList;
    end;
end;

procedure TCollectionEditor.FormModified;
begin
  UpdateList;
end;

procedure TCollectionEditor.MoveUpClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ListView1.Items.Count - 1 do
    if ListView1.Items[i].Selected then
    begin
      if Collection.Items[i].Index = 0 then Exit;
      Collection.Items[i].Index := Collection.Items[i].Index - 1;
      ListView1.Items[i].Selected := ListView1.Items[i - 1].Selected;
      ListView1.Items[i - 1].Selected := True;
    end;
  UpdateListNames;
end;

procedure TCollectionEditor.MoveDownClick(Sender: TObject);
var
  i: Integer;
begin
  for i := ListView1.Items.Count - 1 downto 0 do
    if ListView1.Items[i].Selected then
    begin
      if Collection.Items[i].Index = ListView1.Items.Count - 1 then Exit;
      Collection.Items[i].Index := Collection.Items[i].Index + 1;
      ListView1.Items[i].Selected := ListView1.Items[i + 1].Selected;
      ListView1.Items[i + 1].Selected := True;
    end;
  UpdateListNames;
end;

procedure TCollectionEditor.FormCreate(Sender: TObject);
begin
  Add.Caption := SCEAdd;
  Delete.Caption := SCEDelete;
  MoveUp.Caption := SCEMoveUp;
  MoveDown.Caption := SCEMoveDown;
  Add1.Caption := SCEAdd;
  Delete1.Caption := SCEDelete;
  MoveUp1.Caption := SCEMoveUp;
  MoveDown1.Caption := SCEMoveDown;
  SelectAll1.Caption := SCESelectAllItem;
  if Assigned(ToolServices) then
    RegAuto1.RegPath := ToolServices.GetBaseRegistryKey +
      '\Property Editors\Collection Editor'
  else 
    RegAuto1.UseReg := False;
end;

procedure TCollectionEditor.FormHide(Sender: TObject);
begin
  RegAuto1.Save;
end;

procedure TCollectionEditor.FormResize(Sender: TObject);
begin
  ListView1.Columns[0].Width := ListView1.ClientWidth;
end;

end.
