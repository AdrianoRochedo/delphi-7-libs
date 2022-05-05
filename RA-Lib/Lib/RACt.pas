{***********************************************************
                R&A Library
       Copyright (C) 1996-2000 R&A

       components  : ...
       description : Small components
       required    : TRegAuto

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

//{$DEFINE TCheckBox95} // - uncomment this line, if you have TCheckBox95 component

unit RACt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, RARegAuto, Grids, ComCtrls
{$IFDEF TCheckBox95}
  , EnhButtn {TCheckBox95}
{$ENDIF TCheckBox95}
  ;

type

  TRegAutoGrid = class(TComponent)
  private
    FRegAuto : TRegAuto;
    procedure SetRegAuto(Value : TRegAuto);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RegAutoNotify(Sender : TObject; Operation : TRegAutoOperation);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property RegAuto : TRegAuto read FRegAuto write SetRegAuto;
  end;


  TRAStatusBar = class(TStatusBar)
  protected
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
  public
    constructor Create(AOwner : TComponent); override;
  end;


 { TRACheckBox - чекбокс специально дл€ использовани€
    в окне ListFilter моих проектов }

 {$IFDEF TCheckBox95}
  TRACheckBox = class(TCheckBox95)
 {$ELSE}
  TRACheckBox = class(TCheckBox)
 {$ENDIF TCheckBox95}
  private
    FAllowGrayed : boolean;
    FCanvas : TCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMControlListChange(var Message: TCMControlListChange); message CM_CONTROLLISTCHANGE;
  protected
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property AllowGrayed : boolean read FAllowGrayed write FAllowGrayed;
  end;

  TRAhtListBox = class(TCustomListBox)
  private
    FHideSel: Boolean;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetHideSel(Value: Boolean);
    function GetPlainItems(index: integer): string;
  protected
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property PlainItems [index: Integer] : string read GetPlainItems;
  published
    property HideSel: Boolean read FHideSel write SetHideSel;

    property Align;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
  //  property IntegralHeight;
  //  property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
  //  property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
  //  property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  //  property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
 {$IFDEF RA_D3H}
    property ImeMode;
    property ImeName;
 {$ENDIF RA_D3H}
 {$IFDEF RA_D4H}
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
 {$ENDIF RA_D4H}
  end;

  TRAhtComboBox = class(TCustomComboBox)
  private
    FHideSel: Boolean;
    FDropWidth : integer;
    procedure SetHideSel(Value: Boolean);
    function GetPlainItems(index: integer): string;
    procedure SetDropWidth(ADropWidth : integer);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    property PlainItems [index: Integer] : string read GetPlainItems;
  published
    property HideSel: Boolean read FHideSel write SetHideSel;
    property DropWidth : integer read FDropWidth write SetDropWidth;
  published
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
  //  property ItemHeight;
    property Items;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
  //  property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  //  property OnMeasureItem;
    property OnStartDrag;
 {$IFDEF RA_D3H}
    property ImeMode;
    property ImeName;
 {$ENDIF RA_D3H}
 {$IFDEF RA_D4H}
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
 {$ENDIF RA_D4H}
  end;

  TRAhtLabel = class(TCustomLabel)
  private
   {$IFNDEF RA_D4H}
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
   {$ENDIF}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure AdjustBounds; {$IFDEF RA_D4H} override; {$ENDIF}
    procedure SetAutoSize(Value: Boolean); override;
    procedure Paint; override;
    procedure Loaded; override;
  published
    property Align;
    property Alignment;
    property AutoSize;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
   // property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
   // property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
   {$IFDEF RA_D3H}
    property Layout;
   {$ENDIF RA_D3H}
   {$IFDEF RA_D4H}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
   {$ENDIF RA_D4H}
  end;

  TRAWaitPanel = class(TCustomControl)
  private
    FTimer: TTimer;
    FSecondColor: TColor;
    FFirstColor: TColor;
    D: Boolean;
    W: Integer;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetColor(const Index: Integer; const Value: TColor);
    procedure TimerTick(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Active: Boolean read GetActive write SetActive;
    property FirstColor: TColor index 0 read FFirstColor write SetColor;
    property SecondColor: TColor index 1 read FSecondColor write SetColor;
  public
   {$IFDEF RA_D4H}
    property DockManager;
   {$ENDIF RA_D4H}
  published
    property Align;
    property Caption;
    property Color;
    property Ctl3D;
    property DragMode;
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
    property Visible;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
   {$IFDEF RA_D4H}
    property AutoSize;
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property DockSite;
    property DragKind;
    property DragCursor;
    property ParentBiDiMode;
    property UseDockManager;
    property OnCanResize;
    property OnClick;
    property OnDockDrop;
    property OnDockOver;
    property OnConstrainedResize;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnResize;
    property OnStartDrag;
    property OnUnDock;
   {$ENDIF RA_D4H}
  end;


  {—охран€ют и восстанавливают ширины колонок Grid}
  procedure RegAutoSaveGrid(Grid : TCustomGrid; RegAuto : TRegAuto;
    Section : string);
  procedure RegAutoRestoreGrid(Grid : TCustomGrid; RegAuto : TRegAuto;
    Section : string);

  {—охран€ют и восстанавливают ширины колонок ListView}
  procedure RegAutoSaveListView(ListView : TCustomListView;
    RegAuto : TRegAuto; Section : string);
  procedure RegAutoRestoreListView(ListView : TCustomListView;
    RegAuto : TRegAuto; Section : string);

implementation

uses RAUtils, RADsgnIntf;

procedure RegAutoSaveGrid(Grid : TCustomGrid; RegAuto : TRegAuto;
  Section : string);
var
  i : integer;
begin
  for i := 0 to TDrawGrid(Grid).ColCount-1 do
    RegAuto.WriteInteger(Section, 'ColumnWidth'+IntToStr(i),
      TDrawGrid(Grid).ColWidths[i]);
end;

procedure RegAutoRestoreGrid(Grid : TCustomGrid; RegAuto : TRegAuto; Section : string);
var
  i : integer;
begin
  for i := 0 to TDrawGrid(Grid).ColCount-1 do
    TDrawGrid(Grid).ColWidths[i] :=
      RegAuto.ReadInteger(Section, 'ColumnWidth'+IntToStr(i),
        TDrawGrid(Grid).ColWidths[i]);
end;

procedure RegAutoSaveListView(ListView : TCustomListView; RegAuto : TRegAuto;
  Section : string);
var
  i : integer;
begin
  for i := 0 to TListView(ListView).Columns.Count-1 do
    RegAuto.WriteInteger(Section, 'ColumnWidth'+IntToStr(i),
      TListView(ListView).Columns[i].Width);
end;

procedure RegAutoRestoreListView(ListView : TCustomListView; RegAuto : TRegAuto;
  Section : string);
var
  i : integer;
begin
  for i := 0 to TListView(ListView).Columns.Count-1 do
    TListView(ListView).Columns[i].Width :=
      RegAuto.ReadInteger(Section, 'ColumnWidth'+IntToStr(i),
        TListView(ListView).Columns[i].Width);
end;


{ TRegAutoGrid }

constructor TRegAutoGrid.Create(AOwner: TComponent);
var
  i : integer;
begin
  inherited Create(AOwner);
  for i := 0 to Owner.ComponentCount-1 do
    if Owner.Components[i] is TRegAuto then begin
      RegAuto := Owner.Components[i] as TRegAuto;
      break;
    end;
end;

procedure TRegAutoGrid.Loaded;
begin
  inherited Loaded;
end;

procedure TRegAutoGrid.SetRegAuto(Value : TRegAuto);
begin
  if Value <> FRegAuto then
  begin
    if FRegAuto <> nil then FRegAuto.RemoveNotify(RegAutoNotify);
    FRegAuto := Value;
    if FRegAuto <> nil then FRegAuto.AddNotify(RegAutoNotify);
  end;
end;

procedure TRegAutoGrid.RegAutoNotify(Sender : TObject; Operation : TRegAutoOperation);
var
  i, j : integer;
begin
  inherited Loaded;
  for j := 0 to Owner.ComponentCount-1 do
    if Owner.Components[j] is TCustomGrid then
      with TDrawGrid(Owner.Components[j] as TCustomGrid) do
        for i := 0 to ColCount-1 do
          case Operation of
            roBeforeLoad : ColWidths[i] := RegAuto.ReadInteger(Owner.Name+'\'+Name, 'ColumnWidth'+IntToStr(i), ColWidths[i]);
            roBeforeSave : RegAuto.WriteInteger(Owner.Name+'\'+Name, 'ColumnWidth'+IntToStr(i), ColWidths[i]);
          end;
end;

procedure TRegAutoGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FRegAuto) and (Operation = opRemove) then
    RegAuto := nil;
end;


{ TRAStatusBar }

constructor TRAStatusBar.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TRAStatusBar.WMSize(var Message: TWMSize);
begin
  inherited;
  Realign;
end;

procedure TRAStatusBar.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  Canvas.Brush.Color:=Color;
  RecreateWnd;
end;

{ TRACheckBox }

constructor TRACheckBox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  if csDesigning in ComponentState then
  begin
    FCanvas := TControlCanvas.Create;
    TControlCanvas(FCanvas).Control := Self;
  end;
end;

destructor TRACheckBox.Destroy;
begin
  if csDesigning in ComponentState then
    FCanvas.Free;
  inherited Destroy;
end;

procedure TRACheckBox.CMControlListChange(var Message: TCMControlListChange);
begin
  inherited;
  if  Message.Inserting then
    Message.Control.Enabled := State = cbChecked;
end;

procedure TRACheckBox.Click;
var
  i : integer;
begin
  inherited Click;
  for i := 0 to ControlCount - 1 do
    TControl(Controls[i]).Enabled := State = cbChecked;
end;

procedure TRACheckBox.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  if (Button = mbRight) and FAllowGrayed then
    if State = cbGrayed then
      State := cbUnchecked else
      State := cbGrayed
end;

procedure TRACheckBox.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    FCanvas.Handle := Message.DC;
    DrawDesignFrame(FCanvas, ClientRect);
    FCanvas.Handle := 0;
  end;
end;


{ TRAhtListBox }

constructor TRAhtListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := lbOwnerDrawFixed;
end;    { Create }

procedure TRAhtListBox.DrawItem(Index: Integer; Rect: TRect;
        State: TOwnerDrawState);
begin
  ItemHtDraw(Canvas, Rect, State, Items[index], FHideSel);
end;

procedure TRAhtListBox.CMFontChanged(var Message: TMessage);
begin
  Canvas.Font := Font;
  ItemHeight := Canvas.TextHeight('W');
end;

procedure TRAhtListBox.SetHideSel(Value: Boolean);
begin
  FHideSel := Value;
  Invalidate;
end;

function TRAhtListBox.GetPlainItems(index: integer): string;
begin
  Result := ItemHtPlain(Items[index]);
end;


{ TRAhtComboBox }

constructor TRAhtComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
end;    { Create }

procedure TRAhtComboBox.DrawItem(Index: Integer; Rect: TRect;
        State: TOwnerDrawState);
begin
  ItemHtDraw(Canvas, Rect, State, Items[index], FHideSel);
end;

procedure TRAhtComboBox.SetHideSel(Value: Boolean);
begin
  FHideSel := Value;
  Invalidate;
end;

function TRAhtComboBox.GetPlainItems(index: integer): string;
begin
  Result := ItemHtPlain(Items[index]);
end;

procedure TRAhtComboBox.CreateWnd;
var
  Tmp : integer;
begin
  inherited CreateWnd;
  if DropWidth = 0 then
    DropWidth := Width
  else
  begin
    Tmp := DropWidth;
    DropWidth := 0;
    DropWidth := Tmp;
  end;
end;

procedure TRAhtComboBox.SetDropWidth(ADropWidth : integer);
begin
  if FDropWidth <> ADropWidth then begin
    FDropWidth := ADropWidth;
    Perform(CB_SETDROPPEDWIDTH, FDropWidth, 0);
  end;
end;


{ TRAhtLabel }

{$IFNDEF RA_D4H}
procedure TRAhtLabel.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
  AdjustBounds;
end;
{$ENDIF RA_D4H}

procedure TRAhtLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TRAhtLabel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TRAhtLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  i: integer;
  DC: HDC;
  X: Integer;
  Rect: TRect;
  Ss: TStrings;
  MaxWidth: Integer;
begin
  if not (csReading in ComponentState) and AutoSize then
  begin
    Rect := ClientRect;
    MaxWidth := 0;
    DC := GetDC(0);
    try
      Canvas.Handle := DC;
      Canvas.Font := Font;
      Ss := TStringList.Create;
      try
        Ss.Text := Caption;
        Rect.Bottom := Canvas.TextHeight('W') * Ss.Count;
        for i := 0 to Ss.Count - 1 do
        begin
          MaxWidth := Max(MaxWidth, ItemHtWidth(Canvas, Bounds(0, 0, 0, 0), [],
            Ss[i], False));
        end;    { for }
      finally
        Ss.Free;
      end;    { try/finally }
    finally
      Canvas.Handle := 0;
      ReleaseDC(0, DC);
    end;    { try/finally }
    Rect.Right := Rect.Left + MaxWidth;
    X := Left;
    if Alignment = taRightJustify then Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right, Rect.Bottom);
  end;
end;

procedure TRAhtLabel.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then
  begin
    inherited SetAutoSize(Value);
    AdjustBounds;
  end;
end;

procedure TRAhtLabel.Paint;
var
  S: string;
  H, W, i: Integer;
  Rect: TRect;
  Ss: TStrings;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
  if Transparent then
    Canvas.Brush.Style := bsClear else
    Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  H := Canvas.TextHeight('W');
  Ss := TStringList.Create;
  Ss.Text := Caption;
  try
    for i := 0 to Ss.Count - 1 do    { Iterate }
    begin
      S := Ss[i];
      Rect := ClientRect;
     {$IFDEF RA_D3H}
      case Layout of    { }
        tlTop:
          inc(Rect.Top, H * i);
        tlBottom:
          Rect.Top := Rect.Bottom - (Ss.Count - i) * H;
        tlCenter:
          Rect.Top := (Rect.Bottom - Rect.Top - Ss.Count * H) div 2 + H * i;
      end;    { case }
     {$ELSE}
      inc(Rect.Top, H * i);
     {$ENDIF RA_D3H}
      case Alignment of    { }
        taLeftJustify: {nothing};
        taRightJustify:
          begin
            W := ItemHtWidth(Canvas, Rect, [], S, False);
            Rect.Left := Rect.Right - W;
          end;
        taCenter:
          begin
            W := ItemHtWidth(Canvas, Rect, [], S, False);
            Rect.Left := Rect.Left + (Rect.Right - Rect.Left - W) div 2;
          end;
      end;    { case }
      ItemHtDraw(Canvas, Rect, [], S, False);
    end;
  finally
    Ss.Free;
  end;
end;



{ TRAWaitProgress }

constructor TRAWaitPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := TimerTick;
  FFirstColor := clBtnFace;
  FSecondColor := clBtnShadow;
end;

function TRAWaitPanel.GetActive: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TRAWaitPanel.SetActive(const Value: Boolean);
begin
  FTimer.Enabled := Value;
  Invalidate;
end;

procedure TRAWaitPanel.SetColor(const Index: Integer; const Value: TColor);
begin
  if Index = 0 then
    FFirstColor := Value
  else if Index = 1 then
    FSecondColor := Value;
  Invalidate;
end;

procedure TRAWaitPanel.Paint;
var
  R1, R2, R: TRect;
begin
  if (csDesigning in ComponentState) then
    DrawDesignFrame(Canvas, ClientRect);
  if Active then
  begin
    if D then
      R := Bounds(ClientWidth - W, 0, W, ClientHeight)
    else
      R := Bounds(0, 0, W, ClientHeight);
    R2 := ClientRect;
    SubtractRect(R1, R2, R);
    Canvas.Brush.Color := Color;
    Canvas.FillRect(R1);
    Canvas.Brush.Color := FSecondColor;
    Canvas.FillRect(R);
  end
  else
    inherited Paint;
end;

procedure TRAWaitPanel.TimerTick(Sender: TObject);
begin
  if W = ClientWidth then
  begin
    W := 0;
    D := not D;
  end
  else
    inc(W);
  Refresh;
end;

end.
