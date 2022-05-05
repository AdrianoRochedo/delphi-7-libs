unit Frame_TreeViewObjects;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, ActnList, TreeViewUtils, ImgList, StdCtrls,
  ExtCtrls, OleCtrls, SHDocVw, EmbeddedWB;

type
  TSelectEvent = procedure (Sender, aObject: TObject) of object;

  TTreeViewObjects = class(TFrame)
    Tree: TTreeView;
    Menu_Objects: TPopupMenu;
    ImageList: TImageList;
    Splitter1: TSplitter;
    Browser: TEmbeddedWB;
    procedure TreeDblClick(Sender: TObject);
    procedure TreeEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure TreeEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure Menu_ObjectsPopup(Sender: TObject);
    procedure TreeAddition(Sender: TObject; Node: TTreeNode);
    procedure TreeGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure TreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeEndDrag(Sender, Target: TObject; X, Y: Integer);
  private
    FSelectEvent: TSelectEvent;
    FActions: TActionList;

    function getInterface(Node: TTreeNode): ITreeNode;
    procedure DoSelect(aObject: TObject);

    // Menus
    function CreateMenu(Action: TBasicAction): TMenuItem;
    function CreateMenuItem(Root: TMenuItem; Text: String; Event: TNotifyEvent): TMenuItem; overload;
    function CreateMenuItem(Root: TMenuItem; Action: TBasicAction): TMenuItem; overload;
    function CreateSubMenus(Root: TMenuItem; Action: TBasicAction; CreateItem: boolean): TMenuItem;
  public
    // Somente sera disparado quando o usuario selecionar
    // um no que contenha um objeto que implemente a interface "ITreeNode"
    property OnSelect : TSelectEvent read FSelectEvent write FSelectEvent;
  end;

implementation

{$R *.dfm}

function TTreeViewObjects.getInterface(Node: TTreeNode): ITreeNode;
var o: TObject;
    g: TGUID;
begin
  if Node <> nil then
     begin
     o := TObject(Node.Data);
     if o <> nil then
        begin
        o.GetInterface(ITreeNode, result)
        end
     else
        result := nil;
     end
  else
     result := nil;
end;

procedure TTreeViewObjects.TreeDblClick(Sender: TObject);
var o  : ITreeNode;
    No : TTreeNode;
begin
  o := getInterface(Tree.Selected);
  if (o <> nil) then
     begin
     Tree.Selected.MakeVisible();
     o.ExecuteDefaultAction();
     end;
end;

procedure TTreeViewObjects.DoSelect(aObject: TObject);
begin
  if Assigned(FSelectEvent) then
     FSelectEvent(Tree, aObject);
end;

procedure TTreeViewObjects.TreeEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
var o: ITreeNode;
begin
  o := getInterface(Node);
  if o <> nil then
     AllowEdit := o.CanEdit()
  else
     AllowEdit := false;
end;

procedure TTreeViewObjects.TreeEdited(Sender: TObject; Node: TTreeNode; var S: String);
var o: ITreeNode;
begin
  o := getInterface(Node);
  if o <> nil then
     o.setEditedText(S);
end;

procedure TTreeViewObjects.Menu_ObjectsPopup(Sender: TObject);
var Menu : TMenuItem;
     act : TContainedAction;
       i : Integer;
       o : ITreeNode;
begin
  Menu_Objects.Items.Clear();
  o := getInterface(Tree.Selected);
  if o <> nil then
     begin
     FActions.Free();
     FActions := TActionList.Create(nil);
     o.getActions(FActions);

     for i := 0 to FActions.ActionCount-1 do
       begin
       act := FActions[i];
       Menu := CreateMenuItem(Menu_Objects.Items, act);
       CreateSubMenus(Menu, act, false);
       end;
     end;
end;

function TTreeViewObjects.CreateMenu(Action: TBasicAction): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := TAction(Action).Caption;
  Result.Tag := TAction(Action).Tag;
  Result.Checked := TAction(Action).Checked;
  Result.OnClick := TAction(Action).OnExecute;
end;

function TTreeViewObjects.CreateMenuItem(Root: TMenuItem; Text: String; Event: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := Text;
  Result.OnClick := Event;
  Root.Add(Result);
end;

function TTreeViewObjects.CreateMenuItem(Root: TMenuItem; Action: TBasicAction): TMenuItem;
begin
  Result := CreateMenu(Action);
  Root.Add(Result);
end;

function TTreeViewObjects.CreateSubMenus(Root: TMenuItem; Action: TBasicAction; CreateItem: boolean): TMenuItem;
var i: Integer;
begin
  if CreateItem then
     begin
     Result := CreateMenu(Action);
     Root.Add(Result);
     end
  else
     Result := Root;

  for i := 0 to Action.ComponentCount-1 do
    CreateSubMenus(Result, TAction(Action.Components[i]), true);
end;

procedure TTreeViewObjects.TreeAddition(Sender: TObject; Node: TTreeNode);
var o: ITreeNode;
begin
  o := getInterface(Node);
  if o <> nil then
     begin
     o.setNode(Node);
     Node.Text := o.getNodeText();
     end;
end;

procedure TTreeViewObjects.TreeGetImageIndex(Sender: TObject; Node: TTreeNode);
var o: ITreeNode;
begin
  o := getInterface(Node);
  if o <> nil then
     Node.ImageIndex := o.getImageIndex();
end;

procedure TTreeViewObjects.TreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
var o: ITreeNode;
begin
  o := getInterface(Node);
  if o <> nil then
     Node.SelectedIndex := o.getSelectedImageIndex();
end;

procedure TTreeViewObjects.TreeDeletion(Sender: TObject; Node: TTreeNode);
var o: ITreeNode;
begin
  o := getInterface(Node);
  if o <> nil then
     o.setNode(nil);
end;

procedure TTreeViewObjects.TreeChange(Sender: TObject; Node: TTreeNode);
var o : ITreeNode;
    SL: TStrings;
begin
  SL := TStringList.Create();

  if Node.Data <> nil then
     begin
     o := getInterface(Node);
     if o <> nil then
        begin
        if o.getDescription() <> '' then SL.Text := o.getDescription();
        DoSelect( o.getRef() );
        end
     else
        DoSelect(nil);
     end
  else
     DoSelect(nil);

  Browser.LoadFromStrings(SL);
  SL.Free();
end;

procedure TTreeViewObjects.TreeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var No: TTreeNode;
begin
  Accept := False;

  No := Tree.GetNodeAt(X, Y);
  if No <> nil then
     Accept := (No <> Tree.Selected);

  if (Y < 10) and (No <> nil) then
     Tree.TopItem := No.GetPrevVisible;

  if (Y > Tree.Height - 10) and (No <> nil) then
     Tree.Selected := No.GetNextVisible;
end;

procedure TTreeViewObjects.TreeEndDrag(Sender, Target: TObject; X, Y: Integer);
var No: TTreeNode;
begin
  if Target is TTreeView then
     begin
     No := Tree.GetNodeAt(X, Y);
     if (No <> nil) and (No <> Tree.Selected) then
        begin
        Tree.Selected.MoveTo(No, naAddChild);
        No.AlphaSort();
        end;
     end;
end;

end.
