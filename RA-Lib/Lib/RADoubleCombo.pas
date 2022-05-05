{***********************************************************
                R&A Library
       Copyright (C) 192001 Andrei Prygounkov

       components  : TRADoubleCombo
       description : ComboBox with two lists

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RADoubleCombo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, Forms,
  Mask;

const
  WM_UPDATESELPOS = WM_USER + 100; { WM_USER + 1 not working - why ? }

type

  TRADoubleCombo = class;
 { in OnFilter you can specify items that will be displayed in
   Second list and item index for it }
  TOnFilter = procedure(Sender: TRADoubleCombo; FilterItems: TStrings;
    var Index: Integer) of object;
  TComboList = class;
  TComboListClass = class of TComboList;  

  TRADoubleCombo = class(TCustomMaskEdit)
  private
    FPopup: TWinControl;
    FList1: TListBox;
    FList2: TListBox;
    FItems: TStrings;
    FTmpItems: TStrings;
    FFocusedList: TListBox;
    FDropDownCount1: Integer;
    FDropDownCount2: Integer;
    FDropDownWidth: Integer;
    FOnNewItem: TNotifyEvent;
    FOnFilter: TOnFilter;
    procedure SetItems(Value: TStrings);
    function GetListVisible: Boolean;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure CMCancelMode(var Message: TWMCancelMode); message CM_CANCELMODE;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
   // procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMUpdateSelPos(var Message: TMessage); message WM_UPDATESELPOS;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DropDown; dynamic;
    procedure CloseUp(const Accept: Boolean); dynamic;
    procedure UpdateSelPos;
    procedure DoExit; override;
    procedure CheckNewItem; dynamic;
    procedure NewItem; dynamic;
    function GetComboListClass: TComboListClass; dynamic;
    function UnfocusedList: TListBox;
    property FocusedList: TListBox read FFocusedList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TStrings read FItems write SetItems;
    property DropDownCount1: Integer read FDropDownCount1 write FDropDownCount1 default 8;
    property DropDownCount2: Integer read FDropDownCount2 write FDropDownCount2 default 8;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default -1;
    property ListVisible: Boolean read GetListVisible;
    property OnNewItem: TNotifyEvent read FOnNewItem write FOnNewItem;
    property OnFilter: TOnFilter read FOnFilter write FOnFilter;

    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EditMask;
    property Font;
   {$IFDEF RA_D3H}
    property ImeMode;
    property ImeName;
   {$ENDIF RA_D3H}
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TComboList = class(TListBox)
  private
    FRADoubleCombo: TRADoubleCombo;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;


implementation

uses RAUtils;

type

  TDistPopup = class(TWinControl)
  protected
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;


{ TDistPopup }
constructor TDistPopup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Ctl3D := False;
end;

procedure TDistPopup.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;    { CreateWnd }

procedure TDistPopup.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
   // Style := WS_POPUP or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;


{ TComboList }
constructor TComboList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
end;    { Create }

procedure TComboList.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> FRADoubleCombo) and
     (Message.Sender <> FRADoubleCombo.FList1) and
     (Message.Sender <> FRADoubleCombo.FList2) then
    FRADoubleCombo.CloseUp(False);
end;

procedure TComboList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if FRADoubleCombo.FFocusedList <> Self then
  begin
    FRADoubleCombo.FFocusedList := Self;
    CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
    Perform(WM_LBUTTONDOWN, 0, Y shl 16 + X);
  end;
end;

procedure TComboList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FRADoubleCombo.CloseUp(True);
    FRADoubleCombo.SetFocus;
  end;
end;

procedure TComboList.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if FRADoubleCombo.FFocusedList <> Self then
    if odSelected in State then
    begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.Font.Color := clWindowText;
    end;

  Canvas.FillRect(Rect);
  if Index < Items.Count then
    Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]);
end;


{ TRADoubleCombo }
constructor TRADoubleCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropDownCount1 := 8;
  FDropDownCount2 := 8;
  FDropDownWidth := -1;
  FPopup := TDistPopup.Create(Self);
  FPopup.Visible := False;
  FPopup.Parent := Self;
  FList1 := GetComboListClass.Create(Self);
  TComboList(FList1).FRADoubleCombo := Self;
  FList1.Parent := FPopup;
  FList1.ParentCtl3D := False;
  FList2 := GetComboListClass.Create(Self);
  TComboList(FList2).FRADoubleCombo := Self;
  FList2.Parent := FPopup;
  FList2.ParentCtl3D := False;
  FPopup.Visible := False;
  FItems := TStringList.Create;
  FTmpItems := TStringList.Create;
end;    { Create }

destructor TRADoubleCombo.Destroy;
begin
  FItems.Free;
  FTmpItems.Free;
  inherited Destroy;
end;    { Destroy }

procedure TRADoubleCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not FPopup.Visible then
    case Key of
      VK_DOWN:
        DropDown;
      VK_RETURN:
        CheckNewItem;
    end    { case }
  else
    { list visible }
    case Key of
      VK_ESCAPE:
        begin
          CloseUp(False);
          SetFocus;
          SelLength := 0;
          SelStart := Length(Text);
        end;
      VK_RETURN:
        begin
          if ListVisible then
            CloseUp(True) else
            CheckNewItem;
          SetFocus;
          SelLength := 0;
          SelStart := Length(Text);
        end;
      VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
        begin
          SendMessage(FFocusedList.Handle, WM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      VK_TAB:
        begin
          FFocusedList := UnfocusedList;
          UnfocusedList.Perform(WM_KILLFOCUS, 0 ,0);
          CallWindowProc(TComboList(FFocusedList).DefWndProc, FFocusedList.Handle, WM_SETFOCUS, 0, 0);
          Key := 0;
        end;
      else if not (Key in [VK_CONTROL, VK_SHIFT]) then
        PostMessage(Handle, WM_UPDATESELPOS, 0, 0);
    end;    { case }
end;

procedure TRADoubleCombo.DropDown;

  procedure AlignLists;
  var
    H: Integer;
  begin
    if FDropDownWidth = -1 then
      FPopup.Width := Width else
      FPopup.Width := FDropDownWidth;
    FList1.ItemHeight := FList1.Canvas.TextHeight('W');
    FList2.ItemHeight := FList1.ItemHeight;
    H := Min(FList1.ItemHeight * Min(Max(1, FList1.Items.Count), FDropDownCount1),
      Pixels(Self, 250)) + FList1.Height - FList1.ClientHeight;
    FList1.SetBounds(0, 0, FPopup.ClientWidth, H);
   { splitter } //..
    H := Min(FList2.ItemHeight * Min(Max(1, FList1.Items.Count), FDropDownCount2),
      Pixels(Self, 250)) + FList1.Height - FList1.ClientHeight;
    FList2.SetBounds(0, FList1.BoundsRect.Bottom, FList1.Width, H);
  end;    { AlignLists }

var
  P: TPoint;
begin
  (FItems as TStringList).Sort;
  FList1.Items := FItems;
  FList2.Items := FItems;
  P := Parent.ClientToScreen(Point(Left, Top + Height));
  AlignLists;
  FPopup.ClientHeight := FList2.BoundsRect.Bottom;
  P.X := Min(P.X, Screen.Width - Width);
  if P.Y > Screen.Height - FPopup.ClientHeight then
    P.Y := P.Y - Height - FPopup.ClientHeight;

  SetWindowPos(FPopup.Handle, HWND_TOP, P.X, P.Y, 0, 0,
    SWP_NOSIZE or SWP_NOACTIVATE);
  FPopup.Visible := True;
  UpdateSelPos;

  FFocusedList := FList1; { DEBUG !! }

  CallWindowProc(TComboList(FFocusedList).DefWndProc, FFocusedList.Handle, WM_SETFOCUS, 0, 0);
end;

procedure TRADoubleCombo.CloseUp(const Accept: Boolean);
begin
  if not FPopup.Visible then Exit;
  FPopup.Visible := False;
  if Accept and not ReadOnly then
    if FFocusedList.ItemIndex > -1 then
    begin
      Text := FFocusedList.Items[FFocusedList.ItemIndex];
      SelStart := Length(Text);
    end
    else
      CheckNewItem;
end;    { CloseUp }

procedure TRADoubleCombo.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TRADoubleCombo.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  CloseUp(False);
  MouseCapture := False;
end;

procedure TRADoubleCombo.CMCancelMode(var Message: TWMCancelMode);
begin
  CloseUp(False);
end;

procedure TRADoubleCombo.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  inherited;
  if FPopup.Visible and (Message.CharCode in
    [VK_UP, VK_DOWN, VK_RETURN, VK_ESCAPE, VK_TAB]) then Message.Result := 1;
end;

{
procedure TRADoubleCombo.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
  if FPopup.Visible then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;
}

procedure TRADoubleCombo.WMUpdateSelPos(var Message: TMessage);
begin
  UpdateSelPos;
end;

function TRADoubleCombo.UnfocusedList: TListBox;
begin
  if FFocusedList = FList1 then
    Result := FList2 else
    Result := FList1;
end;

procedure TRADoubleCombo.UpdateSelPos;
var
  S: string;
  i, Ind: Integer;
  Identical: Boolean;
begin
  S := ANSIUpperCase(Trim(Text));

 { Up list }
  Ind := -1;
  for i := 0 to FItems.Count - 1 do
    if ANSIStrLIComp(PChar(S), PChar(Trim(FItems[i])), Length(S)) = 0 then
    begin
      Ind := i;
      Break;
    end;
  FList1.ItemIndex := Ind;

 { down list }
  Ind := 0;  
  if Assigned(FOnFilter) then
    FOnFilter(Self, FTmpItems, Ind)
  else
  begin
    if S <> '' then
    begin
      FTmpItems.Clear;
      for i := 0 to FItems.Count - 1 do
        if Pos(S, ANSIUpperCase(FItems[i])) > 0 then
          FTmpItems.Add(FItems[i]);
    end
    else
      FTmpItems.Assign(FItems);
  end;
  Identical := FTmpItems.Count = FList2.Items.Count;
  if Identical then
    for i := 0 to FTmpItems.Count - 1 do
      if FTmpItems[i] <> FList2.Items[i] then
      begin
        Identical := False;
        Break;
      end;
  if not Identical then
  begin
    FList2.Items := FTmpItems;
    FList2.ItemIndex := Ind;
  end;
end;

procedure TRADoubleCombo.DoExit;
begin
  inherited DoExit;
 // CheckNewItem;
end;

procedure TRADoubleCombo.CheckNewItem;
begin
 if (Trim(Text) <> '') and
    (FItems.IndexOf(Trim(Text)) = -1) then
    NewItem;
end;

procedure TRADoubleCombo.NewItem;
begin
  if Assigned(FOnNewItem) then FOnNewItem(Self);
end;

function TRADoubleCombo.GetListVisible: Boolean;
begin
  Result := FPopup.Visible;
end;

function TRADoubleCombo.GetComboListClass: TComboListClass;
begin
  Result := TComboList;
end;

end.
