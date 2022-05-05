{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       components  : TRACombo
       description : Special combo box

       author      : Roman Tkachev
       e-mail      : whiteman@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RACombo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, Buttons;

type
  TRACombo = class(TCustomPanel)
  private
    FItems     : TStrings;
    FItemIndex : integer;
    FButton    : TSpeedButton;
    FPopupMenu : TPopupMenu;
    FOnChange  : TNotifyEvent;
    procedure SetItems(AItems : TStrings);
    procedure SetItemIndex(AItemIndex : integer);
   {$IFDEF RA_D3H}
    procedure SetFlat(Value: Boolean);
    function GetFlat: Boolean;
   {$ENDIF RA_D3H}
  protected
    procedure OnClick(Sender : TObject);
    procedure ItemsChange(Sender : TObject);
    procedure MenuClick(Sender : TObject);
    procedure ShowMenu(Sender : TObject);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property Items     : TStrings read FItems     write SetItems;
    property ItemIndex : integer  read FItemIndex write SetItemIndex;
    property OnChange  : TNotifyEvent read FOnChange write FOnChange;
   {$IFDEF RA_D3H}
    property Flat: Boolean read GetFlat write SetFlat;
   {$ENDIF RA_D3H}
  published
    property Align;
    property Alignment;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnResize;
    property OnStartDrag;
 {$IFDEF RA_D4H}
  public
    property DockManager;
  published
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property ParentBiDiMode;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
 {$ENDIF RA_D4H}
  end;

implementation

{$R *.RES}

constructor TRACombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelInner  := bvLowered;
  BevelOuter  := bvNone;
  BorderWidth := 1;

  FPopupMenu := TPopupMenu.Create(Self);
  FPopupMenu.OnPopup := ShowMenu;

  Height := 21;
  Width  := 300;
  FButton := TSpeedButton.Create(Self);
  with FButton do begin
    Glyph.LoadFromResourceName(HInstance, 'DOWN');
    Margin  := 3;
    Parent  := Self;
    Top     := 2;
    Left    := 2;
  end;
  FButton.Height  := Height - 4;
  FButton.Width   := Width  - 4;
  FButton.OnClick := OnClick;
 {$IFDEF RA_D3H}
  Flat := True;
 {$ENDIF RA_D3H}

  FItems      := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
  FItemIndex  := -1;
end;

destructor TRACombo.Destroy;
begin
  while FPopupMenu.Items.Count <> 0 do FPopupMenu.Items.Remove(FPopupMenu.Items[0]);
  FItems.Free;
  FButton.Free;
  FPopupMenu.Free;
  inherited Destroy;
end;

procedure TRACombo.OnClick(Sender: TObject);
var
  APoint : TPoint;
begin
  with FButton do APoint := ClientToScreen(Point(0, Height));
  FPopupMenu.Popup(APoint.X, APoint.Y);
end;

procedure TRACombo.SetItems(AItems : TStrings);
begin
  FItems.Assign(AItems);
end;

procedure TRACombo.SetItemIndex(AItemIndex : integer);
begin
  if AItemIndex < -1 then AItemIndex := -1;
  if AItemIndex >= FItems.Count then AItemIndex := FItems.Count - 1;
  if FItemIndex <> AItemIndex then begin
    FItemIndex := AItemIndex;
    if FItemIndex >= 0 then FButton.Caption := FItems[FItemIndex]
                       else FButton.Caption := '';
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

{$IFDEF RA_D3H}
procedure TRACombo.SetFlat(Value: Boolean);
begin
  FButton.Flat := Value;
end;

function TRACombo.GetFlat: Boolean;
begin
  Result := FButton.Flat;
end;
{$ENDIF RA_D3H}

procedure TRACombo.ItemsChange(Sender : TObject);
var
  i       : integer;
  NewItem : TMenuItem;
begin
  while FPopupMenu.Items.Count <> 0 do FPopupMenu.Items.Remove(FPopupMenu.Items[0]);
  for i := 0 to FItems.Count -1 do begin
    NewItem := TMenuItem.Create(Self);
    NewItem.Caption := FItems[i];
    NewItem.RadioItem := true;
    NewItem.Tag     := i;
    NewItem.OnClick := MenuClick;
    FPopupMenu.Items.Add(NewItem);
  end;
  if ItemIndex >= FItems.Count then ItemIndex := FItems.Count -1;
end;

procedure TRACombo.MenuClick(Sender : TObject);
begin
  ItemIndex := TMenuItem(Sender).Tag;
end;

procedure TRACombo.ShowMenu(Sender : TObject);
var
  i : integer;
begin
  for i := 0 to FPopupMenu.Items.Count-1 do
    with FPopupMenu.Items[i] do
      Checked := (Tag = FItemIndex);
end;

procedure TRACombo.WMSize(var Message: TWMSize);
begin
  inherited;
  FButton.Height := Height - 4;
  FButton.Width  := Width  - 4;
end;

end.
