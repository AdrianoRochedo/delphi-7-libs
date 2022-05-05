{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       components  : TRATreeView, TRAComboBox4Tree
       description : this two components work together

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RATreeView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ComCtrls, StdCtrls;

type
  TRAComboBox4Tree = class;

  TRATreeView = class(TTreeView)
  private
    FTreeComboBox : TRAComboBox4Tree;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure WMSysKeyDown(var Message: TWMKeyDown); message WM_SYSKEYDOWN;
   // procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    procedure Change(Node: TTreeNode); override;
  private
   {Enhanced Drag'n'Drop features}
    procedure WMMouseActivate(var Message : TMessage); message WM_MOUSEACTIVATE;
    procedure WMLButtonDown(var Message : TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message : TWMLButtonUp); message WM_LBUTTONUP;
  end;

  TRAComboBox4Tree = class(TCustomComboBox)
  private
    FComboTreeView : TRATreeView;
    FCanvas : TCanvas;
    FMaxLevel : integer;
    FItemIndex : integer;
    InternalChange : boolean;
    procedure SetRATreeView(AComboTreeView : TRATreeView);
  protected
    procedure TreeChange(Node: TTreeNode);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure DrawItem2(Index: Integer; Rect: TRect; State: TOwnerDrawState; IsEdit : boolean);
    procedure DropDown; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SelItem;
  public
    constructor Create(AOwner : TComponent); override;
    procedure Change; override;
    property Canvas: TCanvas read FCanvas;
  published
    property TreeView : TRATreeView read FComboTreeView write SeTRATreeView;
   // property MaxLevel : integer read FMaxLevel write FMaxLevel;

    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
 {$IFDEF RA_D4H}
  public
    property DockManager;
  published
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
 {$ENDIF RA_D4H}
  end;


implementation

uses RAUtils;


procedure TRATreeView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FTreeComboBox) then
    FTreeComboBox := nil;
end;

procedure TRATreeView.Loaded;
begin
  inherited Loaded;
  if Items.Count > 0 then
    Items[0].Selected := true;
end;

procedure TRATreeView.Change(Node: TTreeNode);
begin
  if Assigned(FTreeComboBox) then FTreeComboBox.TreeChange(Node);
  inherited Change(Node);
end;

procedure TRATreeView.WMSysKeyDown(var Message: TWMKeyDown);
begin
  if Assigned(FTreeComboBox) and
     (Message.CharCode = VK_DOWN) then
  begin
    Message.Result := 0;
    FTreeComboBox.SetFocus;
    PostMessage(FTreeComboBox.Handle, WM_SYSKEYDOWN, TMessage(Message).wParam, TMessage(Message).lParam);
  end else
    inherited;
end;

{************** Enhanced Drag'n'Drop features **************}
procedure TRATreeView.WMMouseActivate(var Message : TMessage);
begin
  if csDesigning in ComponentState then begin inherited; exit; end;
  Message.Result := MA_NOACTIVATE;
end;

procedure TRATreeView.WMLButtonDown(var Message : TWMLButtonDown);
var
  Node : TTreeNode;
  HT : THitTests;
begin
  if csDesigning in ComponentState then begin inherited; exit; end;
  with Message do
  begin
    HT := GetHitTestInfoAt(XPos, YPos);
    if HT - [htOnItem, htOnLabel, htOnIcon] <> HT then
    begin
      Node := GetNodeAt(XPos, YPos);
      if Node <> nil then
      begin
        Node.DropTarget := true;
      end;
    end else
      inherited;
    MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
  end;
end;

procedure TRATreeView.WMLButtonUp(var Message : TWMLButtonUp);
var
  Node : TTreeNode;
  HT : THitTests;
begin
  inherited;
  if csDesigning in ComponentState then exit;
  SetFocus;
  with Message do
  begin
    HT := GetHitTestInfoAt(XPos, YPos);
    if HT - [htOnItem, htOnLabel, htOnIcon] <> HT then
    begin
      Node := GetNodeAt(XPos, YPos);
      if Node <> nil then
        Node.Selected := true;
    end;
  end;
  DropTarget := nil;
end;
{############## Enhanced Drag'n'Drop features ##############}

{############################################################}


constructor TRAComboBox4Tree.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
  FMaxLevel := 0;
//  ItemHeight := 16;
end;

procedure TRAComboBox4Tree.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FComboTreeView) then
    FComboTreeView := nil;
end;

procedure TRAComboBox4Tree.SetRATreeView(AComboTreeView : TRATreeView);
begin
  if FComboTreeView <> nil then
    FComboTreeView.FTreeComboBox := nil;
  FComboTreeView := AComboTreeView;
  FComboTreeView.FTreeComboBox := Self;
  if FComboTreeView <> nil then
    TreeChange(FComboTreeView.Selected);
end;

procedure TRAComboBox4Tree.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  FCanvas := TControlCanvas.Create;
  with Message.DrawItemStruct^ do
  try
   {$IFDEF RA_D5H}
    State := TOwnerDrawState(LongRec(itemState).Lo);
   {$ELSE}
    State := TOwnerDrawState(WordRec(LongRec(itemState).Lo).Lo);
   {$ENDIF RA_D5H}
    FCanvas.Handle := hDC;
    FCanvas.Font := Font;
    FCanvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      FCanvas.Brush.Color := clHighlight;
      FCanvas.Font.Color := clHighlightText
    end;
    if Integer(itemID) >= 0 then
      DrawItem2(itemID, rcItem, State, (ODS_COMBOBOXEDIT and itemState) <> 0)
    else
      FCanvas.FillRect(rcItem);
    FCanvas.Handle := 0;
  finally
    FCanvas.Free;
  end;
end;

procedure TRAComboBox4Tree.DrawItem2(Index: Integer; Rect: TRect; State: TOwnerDrawState; IsEdit : boolean);
var
  X, Off : integer;
  Node : TTreeNode;
  ImageIndex : integer;
  W : integer;
const
  Spacing = 4;
begin
  if not Assigned(FComboTreeView) then exit;
  Canvas.Brush := Brush;
  Canvas.FillRect(Rect);
  if odSelected in State then Canvas.Brush.Color := clHighlight;
  if Assigned(FComboTreeView.Images) then
  begin
    W := FComboTreeView.Images.Width;
    Off := W * 2 div 3;
  end else
  begin
    W := 0;
    Off := 16 * 2 div 3;
  end;
  Node := (Items.Objects[Index] as TTreeNode);
  if IsEdit then X := 0 else X := Node.Level * Off;
  if DroppedDown then
    if FItemIndex = Index then
      ImageIndex := Node.SelectedIndex else
      ImageIndex := Node.ImageIndex
  else
    ImageIndex := Node.SelectedIndex;
  if Assigned(FComboTreeView.Images) then
    FComboTreeView.Images.Draw(Canvas, Rect.Left + 2 + X, Rect.Top, ImageIndex);
  Canvas.TextOut(Rect.Left + 2 + X + W + Spacing, Rect.Top+1, Items[Index]);
  if (X <> 0) and (odFocused in State) then
  begin
    Rect := Bounds(Rect.Left + 2 + X + W + Spacing -1,
                   Rect.Top,
                   Canvas.TextWidth(Items[Index])+2,
                   Canvas.TextHeight(Items[Index])+1);
    DrawFocusRect(Canvas.Handle, Rect);
  end;
end;

procedure TRAComboBox4Tree.DropDown;
begin
  FItemIndex := ItemIndex;
  inherited DropDown;
end;

procedure TRAComboBox4Tree.TreeChange(Node: TTreeNode);
var
  i, F : integer;
  N, N2 : TTreeNode;
begin
  if not Assigned(FComboTreeView) then exit;
  if InternalChange then exit;
 { Getting items from tree }
  Items.Clear;
  if Node = nil then exit;
  N := Node;
  while N.Parent <> nil do
    N := N.Parent;
  for i := 0 to FComboTreeView.Items.Count-1 do
  begin
    N2 := FComboTreeView.Items[i];
    F := Items.Count;
    if (N2.Level = FMaxLevel) or (N2 = Node) then
      while N2 <> nil do
      begin
        if Items.IndexOfObject(N2) = -1 then
          Items.InsertObject(F, N2.Text, N2);
        N2 := N2.Parent;
      end
  end;
  ItemIndex := Items.IndexOfObject(Node);
end;

procedure TRAComboBox4Tree.SelItem;
begin
  if not Assigned(FComboTreeView) then exit;
  if (ItemIndex <> -1) then
    with TTreeNode(Items.Objects[ItemIndex]) do
    begin
      InternalChange := true;
      try
        Selected := true;
        if FComboTreeView.Showing and FComboTreeView.Enabled then
          try
            FComboTreeView.SetFocus;
          except
          end;
      finally
        InternalChange := false;
      end;
    end;
end;

procedure TRAComboBox4Tree.Change;
begin
  if not DroppedDown then SelItem;
end;

procedure TRAComboBox4Tree.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_RETURN : SelItem;
    VK_ESCAPE :
      begin
        if not Assigned(FComboTreeView) then exit;
        try
          FComboTreeView.SetFocus;
        except
        end;
        TreeChange(FComboTreeView.Selected);
      end;
  end;
end;

end.
