{***********************************************************
                R&A Library
              R&A Form Designer
       Copyright (C) 1999-2000 R&A

       form        : TRAPropertyInspector
       description : object inspector

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAFDPropertyInspector;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ComCtrls, StdCtrls, Grids, ExtCtrls, RARegAuto, Buttons,
  DsgnIntf, TypInfo, RAFDCompat;

type
  TRAPropertyInspector = class(TForm)
    TabControl: TTabControl;
    ComponentList: TComboBox;
    LocalPopupMenu: TPopupMenu;
    dNextPage: TMenuItem;
    dPrevPage: TMenuItem;
    dNextPageAlt: TMenuItem;
    dPrevPageAlt: TMenuItem;
    LocalMenuItem: TMenuItem;
    Revert: TMenuItem;
    Expand: TMenuItem;
    Collapse: TMenuItem;
    Stayontop: TMenuItem;
    Hide1: TMenuItem;
    Help1: TMenuItem;
    pnlSelection: TPanel;
    bElipsise: TSpeedButton;
    bArrow: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ComponentListClick(Sender: TObject);
    procedure pnlSelectionClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
  private
    PropList: TListBox;
   {$IFDEF RA_D3}
    Designer: TFormDesigner;
   {$ELSE}
    Designer: IFormDesigner;
   {$ENDIF}
    Selection: TComponentList;
    DesignWindow: TCustomForm;
    Props: TStrings;
    Level: Integer;
    PropIndex: Integer;
    procedure ReadProps;
    procedure GetPropEditProc(Prop: TPropertyEditor);
   // procedure WMWindowPosChanging(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGING;
    procedure WMSizing(var Message: TMessage); message WM_SIZING;
    function RoundHeight(const AHeight: Integer): integer;
    procedure Clear;
    procedure ClearProps;
    procedure UpdateComponentList;
  public
    {$IFDEF RA_D3}
    procedure UpdatePropList(ADesigner: TFormDesigner; ADesignWindow: TCustomForm;
      ASelection: TComponentList);
    procedure UpdatePropValues(ADesigner: TFormDesigner; ADesignWindow: TCustomForm;
      ASelection: TComponentList);
    {$ELSE}
    procedure UpdatePropList(ADesigner: IFormDesigner; ADesignWindow: TCustomForm;
      ASelection: TComponentList);
    procedure UpdatePropValues(ADesigner: IFormDesigner; ADesignWindow: TCustomForm;
      ASelection: TComponentList);
    {$ENDIF}
    procedure RestoreDesktop(raProject: TRegAuto);
    procedure WriteDesktop(raProject: TRegAuto);
  end;

  TPropertyInspector = class(TRAPropertyInspector);

  procedure Create;
  procedure Free;

var
  PropertyInspector: TPropertyInspector;

implementation

uses RAUtils, RAFD, RAFDDesktop, RAFDStringsEditor, RAFDAppBuilder;

{$R *.dfm}

type

  TListGlyph = (lgElipsise, lgArrow);

  TPropListBox = class;
  TPropSelection = class;

  TProp = class
  private
    Editor: TPropertyEditor;
    Level: Integer;
    Expanded: Boolean;
    constructor Create(AEditor: TPropertyEditor; const ALevel: Integer);
    destructor Destroy; override;
  end;

  TListButton = class(TSpeedButton)
  private
    PropList: TPropListBox;
  protected
    procedure Click; override;
    procedure SetGlyph(AGlyph: TListGlyph);
  end;

  TValueListBox = class(TListBox)
  private
    PropList: TPropListBox;
   // procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  TPropEdit = class(TCustomEdit)
  private
    Selection: TPropSelection;
    PropList: TPropListBox;
    Editor: TPropertyEditor;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPropSelection = class(TCustomControl)
  private
    PropList: TPropListBox;
    PropEdit: TPropEdit;
    PropButton: TListButton;
    ValueList: TValueListBox;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    function Editor: TPropertyEditor;
    procedure DropDown;
    procedure CloseUp(const Accept: Boolean);
    procedure GetStrProc(const S: string);
  protected
   // procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPropListBox = class(TListBox)
  private
    FSplitter: Integer;
    SplitterMoved: Boolean;
    TmpSplitter: Integer;
    BottomLineBitmap: TBitmap;
    Selection: TPropSelection;
    Props: TStrings; { pointer to TRAPropertyInspector.Props }
    LBClickTime: longword;
    function PtInSplitter(const X, Y: Integer): boolean;
    procedure DrawSplitter;
    procedure UpdateSelection;
    procedure SetSplitter(Value: Integer);
    procedure DrawPropName(Canvas: TCanvas; Rect: TRect; const Index: Integer);
    function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
  protected
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DrawItem;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Splitter: Integer read FSplitter write SetSplitter;
  end;


procedure Create;
begin
  if PropertyInspector = nil then
    PropertyInspector := TPropertyInspector.Create(Application);
end;    { Create }

procedure Free;
begin
  PropertyInspector.Free;
  PropertyInspector := nil;
end;    { Free }

{ TProp }
constructor TProp.Create(AEditor: TPropertyEditor; const ALevel: Integer);
begin
  Editor := AEditor;
  Level := ALevel;
end;    { Create }

destructor TProp.Destroy;
begin
  Editor.Free;
  inherited Destroy;
end;    { Destroy }


{ TListButton }

procedure TListButton.Click;
var
  PA: TPropertyAttributes;
begin
  if PropList.Selection.ValueList.Visible then
    PropList.Selection.CloseUp(False)
  else
  if PropList.Selection.Editor <> nil then
  begin
    PA := PropList.Selection.Editor.GetAttributes;
    if paValueList in PA then
      PropList.Selection.DropDown
    else if paDialog in PA then
      PropList.Selection.Editor.Edit
  end;
end;    { Click }

procedure TListButton.SetGlyph(AGlyph: TListGlyph);
begin
  if AGlyph = lgElipsise then
    Glyph := (PropList.Owner as TRAPropertyInspector).bElipsise.Glyph
  else if AGlyph = lgArrow then
    Glyph := (PropList.Owner as TRAPropertyInspector).bArrow.Glyph;
end;    { SetGlyph }

{ TPropEdit }
constructor TPropEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
end;    { Create }

procedure TPropEdit.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if GetTickCount - PropList.LBClickTime <= GetDoubleClickTime then
    DblClick;
end;    { WMLButtonDown }

procedure TPropEdit.DblClick;
begin
  if Selection.Editor <> nil then
    Selection.Editor.Edit;
end;    { DblClick }

procedure TPropEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of    { }
    VK_RETURN:
			if Selection.Editor <> nil then
			begin
        try
  				Selection.Editor.SetValue(Text);
        except
          Text := Editor.GetValue;
          Modified := False;
          SelectAll;
          raise;
        end;
        SelectAll;
        Modified := False;
        Key := 0;
			end;
    VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR:
      begin
        PostMessage(PropList.Handle, WM_KEYDOWN, Key, 0);
        Key := 0;
      end;
    VK_ADD, VK_SUBTRACT:
      PropList.Selection.DblClick;
    VK_ESCAPE:
      begin
        Text := Editor.GetValue;
        Modified := False;
        SelectAll;
      end;
    else
  end;    { case }
  inherited KeyDown(Key, Shift);
end;    { KeyDown }

{ TValueListBox }

procedure TValueListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Params.ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW;
  end;
end;

procedure TValueListBox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;    { CreateWnd }

procedure TValueListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PropList.Selection.CloseUp(True);
end;    { MouseUp }

procedure TValueListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  ItemIndex := ItemAtPos(Point(X, Y), True);
end;    { MouseMove }

{
procedure TValueListBox.CMCancelMode(var Message: TCMCancelMode);
begin
  PropList.Selection.CloseUp(False);
end;
}

procedure TValueListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        PropList.Selection.CloseUp(True);
      end;
    VK_ESCAPE:
      begin
        PropList.Selection.CloseUp(False);
      end;
    else
      inherited KeyDown(Key, Shift);
  end;
end;    { KeyDown }


{ TPropSelection }
constructor TPropSelection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
 // ControlStyle := [];
  PropList := Owner as TPropListBox;
  Name := 'Selection';
  SetBounds(0, -1000, Width, 18);
  Parent := PropList;
  ParentColor := True;
  PropEdit := TPropEdit.Create(Self);
  PropEdit.Name := 'EditControl';
  PropEdit.Parent := Self;
  PropEdit.Selection := Self;
  PropEdit.PropList := PropList;
  PropButton := TListButton.Create(Self);
  PropButton.Visible := False;
  PropButton.Parent := Self;
  PropButton.PropList := PropList;
  PropButton.Caption := '';
  ValueList := TValueListBox.Create(Self);
  ValueList.Visible := False;
  ValueList.PropList := PropList;
  ValueList.Parent := Self;
end;    { Create }

{
procedure TPropSelection.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
  end;
end;
}

procedure TPropSelection.WMLButtonDown(var Message: TWMLButtonDown);
var
  P: TPoint;
  Dbl: Boolean;
begin
  inherited;
  Dbl := GetTickCount - PropList.LBClickTime <= GetDoubleClickTime;
  P := PropList.ScreenToClient(ClientToScreen(
    Point(Message.XPos, Message.YPos)));
  SendMessage(PropList.Handle, Message.Msg, Message.Keys, P.Y shl 16 + P.X);
  if Dbl then DblClick;
  PropEdit.SelectAll;
end;    { WMLButtonDown }

procedure TPropSelection.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
	GetCursorPos(P);
	P := ScreenToClient(P);
	if PtInRect(PropEdit.BoundsRect, P) then
    Message.Result := 0
  else
  begin
  	if PropList.PtInSplitter(P.X, P.Y) then
  		SetCursor(Screen.Cursors[crHSplit]) else
  		SetCursor(Screen.Cursors[crArrow]);
    Message.Result := 1;
  end;
end;    { WMSetCursor }

procedure TPropSelection.Paint;
var
  R: TRect;
begin
  inherited Paint;
	R := ClientRect;
	InflateRect(R, 1, 0);
	DrawEdge(Canvas.Handle, R, EDGE_SUNKEN, BF_TOP or BF_LEFT);
	DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_BOTTOM);

	R := ClientRect; R.Top := R.Top + 2;
  Canvas.Brush.Color := Color;
  PropList.DrawPropName(Canvas, R, PropList.ItemIndex);
 { Splitter }
  R := Bounds(PropList.FSplitter - 1, 2, 2, Height - 3);
  DrawEdge(Canvas.Handle, R, EDGE_ETCHED, BF_LEFT);

	R := ClientRect; R.Left := PropList.FSplitter + 1;
  inc(R.Top, 2);
  Canvas.Brush.Color := clWindow;
  Canvas.FillRect(R);
end;    { Paint }

function TPropSelection.Editor: TPropertyEditor;
begin
  Result := TProp(PropList.Props.Objects[PropList.ItemIndex]).Editor;
end;    { Editor }

procedure TPropSelection.DblClick;
var
  Ind: Integer;
  i: Integer;
  Lev: Integer;
begin
	Ind := PropList.ItemIndex;
	Lev := TProp(PropList.Props.Objects[Ind]).Level;
  if paSubProperties in Editor.GetAttributes then
  begin
    if TProp(PropList.Props.Objects[Ind]).Expanded then
    begin
  		i := Ind + 1;
      while i < PropList.Props.Count - 1 do
  		begin
        if TProp(PropList.Props.Objects[i]).Level <= Lev then Break;
        PropList.Props.Objects[i].Free;
        PropList.Props.Delete(i);
  			PropList.Items.Delete(i);
  		//	PropList.Invalidate;
  		end;    { for }
    end
    else
    begin
  		with (PropList.Owner as TRAPropertyInspector) do
      begin
        PropIndex := Ind + 1;
        Level := Lev + 1;
    		Editor.GetProperties(GetPropEditProc);
      end;
  		for i := Ind + 1 to Ind + PropList.Props.Count - PropList.Items.Count do
  			PropList.Items.Insert(i, PropList.Props[i]);
    end;
    with TProp(PropList.Props.Objects[Ind]) do Expanded := not Expanded;
  end;
end;    { DblClick }

procedure TPropSelection.DropDown;
const
  DropItemCount = 8;
var
  i: integer;
  MaxWidth: integer;
begin
  if Editor = nil then Exit;
  ValueList.Items.Clear;
  Editor.GetValues(GetStrProc);
//  if ValueList.Items.Count = 0 then Exit;
  if paSortList in Editor.GetAttributes then
    ValueList.Sorted := True; 
  ValueList.Top := ClientOrigin.Y + Height - 1;
  MaxWidth := 0;
  for i := 0 to ValueList.Items.Count - 1 do
    MaxWidth := Max(MaxWidth, Canvas.TextWidth(ValueList.Items[i]) + 8);
  if ValueList.Items.Count > DropItemCount then
    inc(MaxWidth, GetSystemMetrics(SM_CXVSCROLL));
  ValueList.Width := Max(ClientWidth - PropList.Splitter + 1, MaxWidth);
//  ValueList.Left := ClientOrigin.X + PropList.Splitter - 1;
  ValueList.Left := ClientOrigin.X + ClientWidth - ValueList.Width;
  ValueList.Height := Min(ValueList.ItemHeight * Max(1, ValueList.Items.Count)
    + 2, ValueList.ItemHeight * DropItemCount + 2);
  ValueList.Ctl3D := False;
  ValueList.HandleNeeded;
	SetWindowPos(ValueList.Handle, HWND_TOP, ValueList.Left, ValueList.Top,
		ValueList.Width, ValueList.Height,
		SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  ValueList.Visible := True;
//  ValueList.MouseCapture := True;
  PropList.SetFocus;
end;    { DropDown }

procedure TPropSelection.CloseUp(const Accept: Boolean);
begin
  ValueList.MouseCapture := False;
	SetWindowPos(ValueList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
		SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
  ValueList.Visible := False;
  if Accept and (ValueList.ItemIndex > -1) then
    Editor.SetValue(ValueList.Items[ValueList.ItemIndex]);
end;    { CloseUp }

procedure TPropSelection.GetStrProc(const S: string);
begin
  ValueList.Items.Add(S);
end;    { GetStrProc }


{ TPropListBox }
constructor TPropListBox.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  TControlCanvas(Canvas).Control := Self;
  Style := lbOwnerDrawFixed;
  ItemHeight := (Owner as TForm).Canvas.TextHeight('W') +
    Pixels(Owner as TForm, 3);
  IntegralHeight := True;
  ExtendedSelect := False;
  ParentColor := True;
  FSplitter := Width div 2;
  BottomLineBitmap := TBitmap.Create;
  BottomLineBitmap.Width := 8;
  BottomLineBitmap.Height := 8;
  Selection := TPropSelection.Create(Self);
end;    { Create }

destructor TPropListBox.Destroy;
begin
  BottomLineBitmap.Free;
	inherited Destroy;
end;    { Destroy }

procedure TPropListBox.Click;
begin
  inherited Click;
  UpdateSelection;
{  if TControl(Owner).Visible and Selection.PropEdit.Showing then
    Selection.PropEdit.SetFocus; }
end;    { Click }

procedure TPropListBox.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateSelection;
end;    { WMSize }

procedure TPropListBox.UpdateSelection;
var
  R: TRect;
  T: string;
  PA: TPropertyAttributes;
  Item: Integer;
  Foc: Boolean;
begin
  Foc := Selection.PropEdit.Focused;
  Item := ItemIndex;
  R := Bounds(0, (Item - TopIndex) * ItemHeight - 2, ClientWidth, ItemHeight + 2);
  with Selection.BoundsRect do
  if (Left <> R.Left) or (Right <> R.Right) or
     (Top <> R.Top) or (Bottom <> R.Bottom) or
     (Splitter + 2 <> Selection.PropEdit.Left) or
     (Selection.PropEdit.Modified) or
     (Selection.PropEdit.Editor = nil) then
  begin
    Selection.Visible := False;
  	Selection.BoundsRect := R;
    with Selection do
    begin
      if PropEdit.Modified and (PropEdit.Editor <> nil) then
        PropEdit.Editor.SetValue(PropEdit.Text);
    	if (Item > -1) and (Item < Props.Count) then
      begin
    		PropEdit.Editor := Editor;
        T := PropEdit.Editor.GetValue;
      end else
      begin
       { error ? }
    		PropEdit.Editor := nil;
        T := '';
      end;
      if PropEdit.Editor <> nil then
      begin
        PA := Editor.GetAttributes;
        PropButton.Visible := (paDialog in PA) or (paValueList in PA);
        PropEdit.ReadOnly := paReadOnly in PA;
        if paValueList in PA then
          PropButton.SetGlyph(lgArrow)
        else if paDialog in PA then
          PropButton.SetGlyph(lgElipsise)
      end else
      begin
        PropButton.Visible := False;
        PropEdit.ReadOnly := True;
      end;
      if PropButton.Visible then
      begin
        PropButton.SetBounds(3 + Self.ClientWidth - Height, 2, Pixels(Self, 15),
          Pixels(Self, 15));
       { PropButton.SetBounds(5 + Self.ClientWidth - Height, 0, Pixels(Self, 15),
          Pixels(Self, 15)); }
        PropEdit.SetBounds(Splitter + 2, 3, Self.ClientWidth - Splitter -
          4 - Height, Height - 3);
      end else
        PropEdit.SetBounds(Splitter + 2, 3, Self.ClientWidth - Splitter - 4,
          Height - 3);
			PropEdit.Text := T;
			PropEdit.Modified := False;
			PropEdit.SelectAll;
    end;
    Selection.Visible := True;
    Selection.Refresh;
    if Foc then Selection.PropEdit.SetFocus;
  end;

{  R := ClientRect;
  ValidateRect(Handle, @R); }
end;    { UpdateSelection }

procedure TPropListBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  R: TRect;
begin
  Canvas.Brush.Color := Color;
  R := ClientRect;
  R.Top := (Items.Count - TopIndex) * ItemHeight;
  if R.Top < R.Bottom then
    FillRect(Message.DC, R, Canvas.Brush.Handle);
 // Message.Result := 1;
 // inherited;
end;    { WMEraseBkgnd }

procedure TPropListBox.CMColorChanged(var Message: TMessage);
var
  x, y: Integer;
begin
  inherited;
 { create mask for bottom line }
  for x := 0 to 7 do
    for y := 0 to 7 do
      if x mod 2 = 0 then
        BottomLineBitmap.Canvas.Pixels[x, y] := clGray else
        BottomLineBitmap.Canvas.Pixels[x, y] := Color;
end;    { CMColorChanged }

procedure TPropListBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  if SplitterMoved then Exit;
  with Message.DrawItemStruct^ do
  begin
   {$IFDEF RA_D5H}
    State := TOwnerDrawState(LongRec(itemState).Lo);
   {$ELSE}
    State := TOwnerDrawState(WordRec(LongRec(itemState).Lo).Lo);
   {$ENDIF}
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText
    end;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State){ else
      Canvas.FillRect(rcItem)};
    Canvas.Handle := 0;
  end;
end;    { CNDrawItem }

procedure TPropListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  R: TRect;
  PropValue: string;
  Prop: TProp;
  Ind: Integer;
begin
 // if (index > Props.Count - 1) or (index < 0) then Exit;
  if (odFocused in State) or (odSelected in State) then Exit;
  Ind := ItemIndex;
  if index = Ind then Exit;

  Prop := TProp(Props.Objects[index]);
	Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  R := Rect;
 // Canvas.FillRect(R);


 { restore bottom line of previous item }
// 	Canvas.Brush.Bitmap := BottomLineBitmap;
//	R := Rect; R.Top := R.Top - 1; R.Bottom := R.Top + 1;
//	Canvas.FillRect(R);

 { prop name }
  DrawPropName(Canvas, Rect, Index);
 { prop value }
	R := Rect; R.Top := R.Top + 1;
  R.Left := FSplitter + 2;
	R.Bottom := R.Bottom - 1;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
	Canvas.Font.Color := clNavy;
  if Prop.Editor.AllEqual then
  	PropValue := Prop.Editor.GetValue else
    PropValue := '';
	DrawText(Canvas.Handle, PChar(PropValue), -1, R, DT_NOPREFIX);
 { fill right }
  R := Rect; dec(R.Bottom);
  if index = ind - 1 then dec(R.Bottom);
  R.Left := FSplitter + 2 + Canvas.TextWidth(PropValue);
  if R.Left <= R.Right then Canvas.FillRect(R);
 { fill frame }
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := Color;
  Canvas.MoveTo(FSplitter + 1, R.Bottom - 1);
  Canvas.LineTo(FSplitter + 1, R.Top);
  Canvas.LineTo(Rect.Right, R.Top);
  Canvas.MoveTo(FSplitter + 1, R.Bottom - 1);
  Canvas.LineTo(Rect.Right, R.Bottom - 1);
 { bottom line }
  if (Index <> Ind - 1) or (Ind < 0) then
  begin
   	Canvas.Brush.Bitmap := BottomLineBitmap;
  	R := Rect; R.Top := R.Bottom - 1;
  	Canvas.FillRect(R);
  end;
 { splitter }
  R := Bounds(FSplitter - 1, Rect.Top, 2, Rect.Bottom - Rect.Top);
  if Index = Ind - 1 then dec(R.Bottom, 2);
  DrawEdge(Canvas.Handle, R, EDGE_ETCHED, BF_LEFT);
end;    { DrawItem }

procedure TPropListBox.DrawPropName(Canvas: TCanvas; Rect: TRect;
  const Index: Integer);
var
  PropName: string;
  R: TRect;
  Prop: TProp;
  W, L: Integer;
  S: string;
  Ind: Integer;
begin
 // if (index > Props.Count - 1) or (index < 0) then Exit;
  Rect.Right := FSplitter -1;
  Ind := ItemIndex;

  Prop := TProp(Props.Objects[index]);
  W := Canvas.TextWidth('+');
  L := (Prop.Level + 1) * W + Pixels(Self, 2);

 { fill left }
	Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  R := Rect; R.Right := L; dec(R.Bottom);
  if index = ind - 1 then dec(R.Bottom);
	Canvas.FillRect(R);

 { plus sign }
  if paSubProperties in Prop.Editor.GetAttributes then
  begin
    R := Classes.Rect(Pixels(Self, 2), Rect.Top + 1, L, Rect.Bottom);
    Canvas.Font.Color := clBlack;
    if Prop.Expanded then S := '-' else S := '+';
    inc(R.Left, Prop.Level * W);
    DrawText(Canvas.Handle, PChar(S), -1, R, 0);
  end;

 { fill top and bottom }
  R := Rect; dec(R.Bottom);
  if index = ind - 1 then dec(R.Bottom);
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := Color;
  Canvas.MoveTo(L, R.Bottom - 1);
  Canvas.LineTo(FSplitter - 1, R.Bottom - 1);
  Canvas.MoveTo(L, R.Top);
  Canvas.LineTo(FSplitter - 1, R.Top);

	PropName := Props[index];
 { prop name }
  R := Classes.Rect(L, Rect.Top + 1, FSplitter - 1, Rect.Bottom);
	Canvas.Font.Color := clBlack;
	DrawText(Canvas.Handle, PChar(PropName), -1, R, 0);

 { fill right }
  R := Rect; dec(R.Bottom);
  if index = ind - 1 then dec(R.Bottom);
  R.Left := L + Canvas.TextWidth(PropName);
  if R.Left <= R.Right then Canvas.FillRect(R);
end;    { DrawPropName }

function TPropListBox.PtInSplitter(const X, Y: Integer): boolean;
begin
  Result := (X > FSplitter - 3) and (X < FSplitter + 3);
end;    { PtInSplitter }

{ TCustomListBox.ItemAtPos return valid value only for items,
  that is currently visible, what's why we override it }
function TPropListBox.ItemAtPos(Pos: TPoint; Existing: Boolean): Integer;
var
  Count: Integer;
  ItemRect: TRect;
begin
 // Result := LongRec(SendMessage(Handle, LB_ITEMFROMPOINT, 0, Pos.Y shl 16 + Pos.X)).Lo;
	Result := 0;
	Count := Items.Count;
	while Result < Count do
	begin
		Perform(LB_GETITEMRECT, Result, Longint(@ItemRect));
		if PtInRect(ItemRect, Pos) then Exit;
		Inc(Result);
	end;
	if not Existing then Exit;
  Result := -1;
end;    { ItemAtPos }

procedure TPropListBox.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
	GetCursorPos(P);
	P := ScreenToClient(P);
	if PtInSplitter(P.X, P.Y) then
		SetCursor(Screen.Cursors[crHSplit]) else
		SetCursor(Screen.Cursors[crArrow]);
end;    { WMSetCursor }

procedure TPropListBox.WMLButtonDown(var Message: TWMLButtonDown);
begin
  LBClickTime := GetTickCount;
	SendCancelMode(Self);
	MouseCapture := True;
	MouseDown(mbLeft, [], Message.XPos, Message.YPos);
end;    { WMLButtonDown }

procedure TPropListBox.DrawSplitter;
var
  DC: HDC;
begin
  DC := GetDC(Handle);
  try
    PatBlt(DC, TmpSplitter - 1, 0, 1, ClientHeight, PATINVERT);
  finally { wrap up }
    ReleaseDC(Handle, DC);
  end;    { try/finally }
end;    { DrawFSplitter }

procedure TPropListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    if PtInSplitter(X, Y) then
    begin
    	SplitterMoved := True;
    	TmpSplitter := FSplitter;
    	DrawSplitter;
    end else
    begin
      ItemIndex := ItemAtPos(Point(X, Y), True);
      UpdateSelection;
      Selection.PropEdit.SetFocus;
    end
  else
    inherited MouseDown(Button, Shift, X, Y);
end;    { MouseDown }

procedure TPropListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Item: Integer;
begin
  if MouseCapture then
    if SplitterMoved then
    begin
      DrawSplitter;
      TmpSplitter := Max(Pixels(Self, 30), Min(ClientWidth - Pixels(Self, 45), X));
      DrawSplitter;
    end
  else
  begin
    Item := ItemAtPos(Point(X, Y), True);
    if (Item <> ItemIndex) and (Item > -1) then
    begin
      ItemIndex := Item;
      UpdateSelection;
      if Selection.PropEdit.Showing then
        Selection.PropEdit.SetFocus;
     // Refresh;
    end;
  end;
end;    { MouseMove }

procedure TPropListBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if SplitterMoved then
  begin
    DrawSplitter;
    if FSplitter <> X then
    begin
    	FSplitter := Max(Pixels(Self, 30), Min(ClientWidth - Pixels(Self, 45), X));
    	Invalidate;
    end;
    SplitterMoved := False;
    UpdateSelection;
  end;
	MouseCapture := False;
  if Selection.PropEdit.Showing then
    Selection.PropEdit.SetFocus;
end;    { MouseUp }

procedure TPropListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_UP, VK_DOWN, VK_NEXT, VK_PRIOR, VK_RETURN, VK_ESCAPE]) and
     Selection.ValueList.Visible then
  begin
    PostMessage(Selection.ValueList.Handle, WM_KEYDOWN, Key, 0);
    Key := 0;
  end
  else
    inherited KeyDown(Key, Shift);
end;    { KeyDown }

procedure TPropListBox.SetSplitter(Value: Integer);
var
  S1: integer;
begin
	S1 := Max(Pixels(Self, 30), Min(ClientWidth - Pixels(Self, 45), Value));
	if S1 <> FSplitter then
	begin
		FSplitter := S1;
		Invalidate;
	end;
end;    { SetSplitter }

procedure TPropListBox.CMCancelMode(var Message: TCMCancelMode);
begin
  with Message do
    if {(Sender <> Self) and} (Sender <> Selection.ValueList) and
       (Sender <> Selection.PropButton) then
      Selection.CloseUp(False);
end;    { CMCancelMode }

procedure TPropListBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Selection.CloseUp(False);
end;    { WMKillFocus }


{ TRAPropertyInspector }
procedure TRAPropertyInspector.FormCreate(Sender: TObject);
begin
  Name := 'PropertyInspector';
  Caption := ResStr(deObjectInspector, Caption);
  TabControl.Tabs[0] := ResStr(deProperties, TabControl.Tabs[0]);
  TabControl.Tabs[1] := ResStr(deEvents, TabControl.Tabs[1]);
  Props := TStringList.Create;
  PropList := TPropListBox.Create(Self);
  PropList.Name := 'PropList';
  PropList.Left := 4;
  PropList.Top := Pixels(Self, 24);
  PropList.Parent := TabControl;
  TPropListBox(PropList).Props := Props;
{  TPropListBox(PropList).BottomLineBitmap.Assign(
    BottomBitmap.Picture.Bitmap);
  TPropListBox(PropList).BottomLineBitmap.Mask(clWhite); }
end;    { FormCreate }

procedure TRAPropertyInspector.FormDestroy(Sender: TObject);
begin
  ClearProps;
  Props.Free;
end;    { FormDestroy }

procedure TRAPropertyInspector.FormResize(Sender: TObject);
begin
	ComponentList.Width := ClientWidth - ComponentList.Left * 2;
	pnlSelection.Width := ComponentList.Width - Pixels(Self, 21);
	TabControl.Width := ClientWidth - TabControl.Left * 2;
	PropList.Width := TabControl.Width - PropList.Left * 2;
	PropList.Height := ClientHeight - TabControl.Top - PropList.Top -
		Pixels(Self, 4);
	TabControl.Height := TabControl.Top + PropList.Height + Pixels(Self, 3);
	PostMessage(ComponentList.Handle, CB_SETDROPPEDWIDTH, ComponentList.Width -
		Pixels(Self, 4), 0);
	with TPropListBox(PropList) do Splitter := Splitter;
end;   { FormResize }

procedure TRAPropertyInspector.WMSizing(var Message: TMessage);
begin
  if Message.WParam in [WMSZ_BOTTOM, WMSZ_BOTTOMLEFT, WMSZ_BOTTOMRIGHT] then
    with PRect(Message.LParam)^ do Bottom := Top + RoundHeight(Bottom - Top)
  else if Message.WParam in [WMSZ_TOP, WMSZ_TOPLEFT, WMSZ_TOPRIGHT] then
    with PRect(Message.LParam)^ do Top := Bottom - RoundHeight(Bottom - Top);
  Message.Result := 1;
end;    { WMSizing }

function TRAPropertyInspector.RoundHeight(const AHeight: Integer): integer;
var
  PropListBoxHeight: Integer;
  Border: integer;
begin
  PropListBoxHeight := AHeight - (Height - ClientHeight) - TabControl.Top -
    PropList.Top - Pixels(Self, 3) - ComponentList.Top;
  Border := 4;
  PropListBoxHeight := (PropListBoxHeight - Border) div PropList.ItemHeight
    * PropList.ItemHeight + Border;
  Result := PropListBoxHeight +
    (Height - ClientHeight) + TabControl.Top +
    PropList.Top + Pixels(Self, 3) + ComponentList.Top;
  if AHeight - Result > PropList.ItemHeight div 2 then
    inc(Result, PropList.ItemHeight);
end;    { RoundHeight }

procedure TRAPropertyInspector.pnlSelectionClick(Sender: TObject);
begin
  ComponentList.DroppedDown := True;
end;    { pnlSelectionClick }

procedure TRAPropertyInspector.ComponentListClick(Sender: TObject);
var
  Comp: TComponent;
begin
  if Designer = nil then Exit;
  Comp := Designer.Form.FindComponent(SubStr(
    ComponentList.Items[ComponentList.ItemIndex], 0, ':'));
  if Comp = nil then
    Comp := Designer.Form;
  Designer.SelectComponent(Comp);
  PropList.SetFocus;
end;   { ComponentListClick }

{$IFDEF RA_D3}
procedure TRAPropertyInspector.UpdatePropList(ADesigner: TFormDesigner;
  ADesignWindow: TCustomForm; ASelection: TComponentList);
{$ELSE}
procedure TRAPropertyInspector.UpdatePropList(ADesigner: IFormDesigner;
  ADesignWindow: TCustomForm; ASelection: TComponentList);
{$ENDIF}
begin
  Designer := ADesigner;
  DesignWindow := ADesignWindow;
  Selection := ASelection;
  if not Visible then Exit;
  if Designer = nil then
  begin
    Clear;
    Exit;
  end;
  UpdateComponentList;
	with TPropListBox(PropList).Selection do
		if PropEdit.Editor <> nil then
		begin
			if PropEdit.Modified then
				PropEdit.Editor.SetValue(PropEdit.Text);
			PropEdit.Editor := nil;
		end;
  ReadProps;
end;    { UpdatePropList }

{$IFDEF RA_D3}
procedure TRAPropertyInspector.UpdatePropValues(ADesigner: TFormDesigner;
  ADesignWindow: TCustomForm;	ASelection: TComponentList);
{$ELSE}
procedure TRAPropertyInspector.UpdatePropValues(ADesigner: IFormDesigner;
  ADesignWindow: TCustomForm;	ASelection: TComponentList);
{$ENDIF}
begin
  Designer := ADesigner;
  DesignWindow := ADesignWindow;
  Selection := ASelection;
  if not Visible then Exit;
  TPropListBox(PropList).Selection.PropEdit.Editor := nil;
  TPropListBox(PropList).UpdateSelection;
  UpdateComponentList;
  PropList.Invalidate;
end;    { UpdatePropValues }

procedure TRAPropertyInspector.UpdateComponentList;
var
  i: Integer;
begin
	ComponentList.Clear;
  ComponentList.Items.Add(Designer.Form.Name +
    ': T' + Designer.Form.Name);
  for i := 0 to Designer.Form.ComponentCount - 1 do    { Iterate }
    ComponentList.Items.Add(Designer.Form.Components[i].Name +
      ': ' + Designer.Form.Components[i].ClassName);
  ComponentList.Sorted := True;
  if (Selection.Count = 1) and (Selection[0] <> Designer.Form) then
    pnlSelection.Caption := Designer.GetObjectName(Selection[0]) + ': ' +
      Selection[0].ClassName
  else if (Selection.Count = 0) or (Selection[0] = Designer.Form) then
    pnlSelection.Caption := Designer.Form.Name + ': T' + Designer.Form.Name
  else
  { multiple select }
    pnlSelection.Caption := ''; //'multiple selected';
  ComponentList.ItemIndex := ComponentList.Items.IndexOf(pnlSelection.Caption);
end;    { UpdateComponentList }

procedure TRAPropertyInspector.ReadProps;
var
  N: string;
  PropName: string;
  i: integer;
  PropListItemIndex: Integer;
begin
  if Selection.Count = 0 then Selection.Add(DesignWindow);
  if (PropList.ItemIndex <= PropList.Items.Count - 1) and
     (PropList.ItemIndex > 0) then
    PropName := TProp(Props.Objects[PropList.ItemIndex]).Editor.GetName
  else
    PropName :='';

  ClearProps;
  PropListItemIndex := -1;
  Level := 0;
  PropIndex := 0;

  if TabControl.TabIndex = 0 then
    GetComponentProperties(Selection, [tkInteger .. tkClass, tkWChar .. tkWString],
      Designer, GetPropEditProc)
  else
    GetComponentProperties(Selection, [tkMethod], Designer, GetPropEditProc);

	for i := 0 to Props.Count - 1 do    { Iterate }
	begin
		N := TProp(Props.Objects[i]).Editor.GetName;
		if (PropName = N) or
       ((TabControl.TabIndex = 0) and (PropName = '') and (N = 'Name')) then
		begin
			PropListItemIndex := i;
			Break;
		end;    { for }
	end;

	PropList.Items.BeginUpdate;
	try
	 // TPropListBox(PropList).Selection.Visible := False;
		PropList.Items := Props;
		PropList.ItemIndex := PropListItemIndex;
		TPropListBox(PropList).Click;
		TPropListBox(PropList).Selection.Visible := True;
	finally { wrap up }
		PropList.Items.EndUpdate;
	end;    { try/finally }
	with TPropListBox(PropList) do Splitter := Splitter;
end;    { ReadProps }

procedure TRAPropertyInspector.GetPropEditProc(Prop: TPropertyEditor);
begin
  Props.InsertObject(PropIndex, Prop.GetName, TProp.Create(Prop, Level));
  inc(PropIndex);
end;    { GetPropEditProc }

procedure TRAPropertyInspector.ClearProps;
var
  i: integer;
begin
  for i := 0 to Props.Count - 1 do    { Iterate }
    Props.Objects[i].Free;
  Props.Clear;
end;    { ClearProps }

procedure TRAPropertyInspector.Clear;
begin
	PropList.Clear;
	ClearProps;
	ComponentList.Clear;
	TPropListBox(PropList).Click;
	pnlSelection.Caption := '';
	Exit;
end;    { Clear }

procedure TRAPropertyInspector.TabControlChange(Sender: TObject);
begin
  UpdatePropList(Designer, DesignWindow, Selection);
end;    { TabControlChange }

procedure TRAPropertyInspector.RestoreDesktop(raProject: TRegAuto);
var
  AHeight: Integer;
begin
  FDDesktop.RestoreWindowPos(raProject, 'PropertyInspector', Self);
  AHeight := RoundHeight(Height);
  if AHeight = Height then
    FormResize(nil)
  else
    Height := AHeight;
  TPropListBox(PropList).Splitter := raProject.ReadInteger('PropertyInspector',
    'SplitPos', TPropListBox(PropList).Splitter);
end;

procedure TRAPropertyInspector.WriteDesktop(raProject: TRegAuto);
begin
  FDDesktop.WriteWindowPos(raProject, 'PropertyInspector', Self);
  raProject.WriteInteger('PropertyInspector', 'SplitPos',
    TPropListBox(PropList).Splitter);
end;


initialization
 { Standard property editors }
  RegisterPropertyEditor(TypeInfo(TColor), TObject, '', TColorProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TObject, '', TCaptionProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TObject, '', TComponentProperty);
  RegisterPropertyEditor(TypeInfo(TComponentName), TObject, '', TComponentNameProperty);
  RegisterPropertyEditor(TypeInfo(TCursor), TObject, '', TCursorProperty);
  RegisterPropertyEditor(TypeInfo(TDateTime), TObject, '', TDateTimeProperty);
  RegisterPropertyEditor(TypeInfo(TFont), TObject, '', TFontProperty);
  RegisterPropertyEditor(TypeInfo(TModalResult), TObject, '', TModalResultProperty);
  RegisterPropertyEditor(TypeInfo(TShortCut), TObject, '', TShortCutProperty);
  RegisterPropertyEditor(TypeInfo(TTabOrder), TObject, '', TTabOrderProperty);

 { Don't register TBoolProperty - it don't work }
//  RegisterPropertyEditor(TypeInfo(Boolean), TObject, '', TBoolProperty);

  RegisterPropertyEditor(TypeInfo(TStrings), TObject, '', TStringsProperty);

{  RegisterPropertyEditor(TypeInfo(TDate), TObject, '', TDateProperty);
  RegisterPropertyEditor(TypeInfo(TTime), TObject, '', TTimeProperty); }
finalization
  Free;
end.
