{***********************************************************
                R&A Library
       Copyright (C) 1998, 2001 Andrei Prygounkov

       component   : TRAScrollMax
       description : scrollable panels

       authors     : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAScrollMax;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs,
  ExtCtrls, StdCtrls, Controls, RAScrollBar, Buttons, RAButtons;


 { TRAScrollMax }

const
  CM_PARENTBEVELEDCHANGED       = WM_User + 1;
  CM_PARENTBUTTONFONTCHANGED    = WM_User + 2;
  CM_PARENTBUTTONVISIBLECHANGED = WM_User + 3;

type

  TOnCanExpand = procedure(Sender : TObject; var CanExpand : boolean) of object;
  TOnCanCollapse = procedure(Sender : TObject; var CanCollapse : boolean) of object;

  TRAScrollMax = class;

  TRAScrollMaxBand = class(TCustomControl)
  private
    FData : Pointer;
    FExpandedHeight : integer;
    FButton : TSpeedButton;
    FExpanded : boolean;
    FOrder : integer;
    FBeveled : boolean;
    FBorderWidth : integer;
    FParentBeveled : boolean;
    FParentButtonFont : boolean;
    FParentButtonVisible : boolean;

    FOnResize: TNotifyEvent;
    FOnExpand: TNotifyEvent;
    FOnCollapse: TNotifyEvent;
    FOnCanCollapse : TOnCanCollapse;
    FOnCanExpand : TOnCanExpand;


    procedure ButtonClick(Sender : TObject);
    procedure SetExpanded(const Value: boolean);
    procedure SetExpandedHeight(const Value: integer);
    function GetOrder: integer;
    procedure SetOrder(const Value: integer);
    procedure SetParentBeveled(const Value: boolean);
    procedure SetButtonFont(Value: TFont);
    function GetButtonFont: TFont;
    procedure SetBeveled(const Value : boolean);
    procedure SetBorderWidth(const Value: integer);
    function IsBeveledStored : boolean;
    procedure SetParentButtonFont(const Value: Boolean);
    function IsButtonFontStored: Boolean;
    function GetButtonVisible: boolean;
    procedure SetButtonVisible(const Value: boolean);
    function IsButtonVisibleStored : boolean;
    procedure SetParentButtonVisible(const Value: boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMParentBeveledChanged(var Message: TMessage); message CM_PARENTBEVELEDCHANGED;
    procedure CMParentButtonFontChanged(var Message: TMessage); message CM_PARENTBUTTONFONTCHANGED;
    procedure CMParentButtonVisibleChanged(var Message: TMessage); message CM_PARENTBUTTONVISIBLECHANGED;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure SetZOrder(TopMost: Boolean); override;
    function RAScrollMax : TRAScrollMax;
    procedure UpdateSize(ATop : integer);
   {$IFDEF RA_D2}
    procedure RequestAlign;
   {$ENDIF RA_D2}
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function CollapsedHeight : integer;
    procedure ChangeScale(M, D: Integer); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Data : Pointer read FData write FData;
  published
    property Expanded : boolean read FExpanded write SetExpanded default true;
    property Caption;
    property ExpandedHeight : integer read FExpandedHeight write SetExpandedHeight;
    property Order : integer read GetOrder write SetOrder stored false;
    property ButtonVisible : boolean read GetButtonVisible write SetButtonVisible stored IsButtonVisibleStored;
    property ButtonFont : TFont read GetButtonFont write SetButtonFont stored IsButtonFontStored;
    property Beveled : boolean read FBeveled write SetBeveled default true;
    property BorderWidth : integer read FBorderWidth write SetBorderWidth default 0;
    property ParentBeveled : boolean read FParentBeveled write SetParentBeveled stored IsBeveledStored;
    property ParentButtonVisible : boolean read FParentButtonVisible write SetParentButtonVisible default true;
    property ParentButtonFont : boolean read FParentButtonFont write SetParentButtonFont default true;

    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnExpand : TNotifyEvent read FOnExpand write FOnExpand;
    property OnCollapse : TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnCanExpand : TOnCanExpand read FOnCanExpand write FOnCanExpand;
    property OnCanCollapse : TOnCanCollapse read FOnCanCollapse write FOnCanCollapse;

    property Left   stored false;
    property Top    stored false;
    property Width  stored false;
    property Height stored false;

    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag; 
   {$IFDEF RA_D4H}
    property BiDiMode;
    property ParentBiDiMode;
   {$ENDIF RA_D4H}
  end;

  TRAScrollMaxBands = class(TCustomControl)
  private
    Scrolling : boolean;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure ScrollControls(const DeltaY : integer);
    procedure Paint; override;
  end;

  TRAScrollMax = class(TCustomPanel)
  private
    pnlEdit    : TRAScrollMaxBands;
    ScrollBar  : TRAScrollBar;
    FScrollPos : integer;
    Yy         : integer;
    FButtonFont : TFont;
    FOnScroll: TNotifyEvent;
    FBeveled : boolean;
    FButtonVisible : boolean;
    FAutoHeight : boolean;
    FExpandedHeight : integer;
    FOneExpanded : boolean;

    procedure Correct;
    procedure CorrectHeight;
    procedure BandMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    procedure BandMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    procedure BandMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    procedure ScrollBarScroll(Sender : TObject);

    procedure SetButtonFont(Value: TFont);
    procedure ButtonFontChanged(Sender: TObject);
    function GetBand(index: integer): TRAScrollMaxBand;
    function GetBandCount: integer;
    procedure SetScrollPos(const Value : integer);
    procedure SetButtonVisible(const Value: boolean);
    procedure SetBeveled(const Value: boolean);
    procedure SetAutoHeight(const Value: boolean);
    procedure SetExpandedHeight(const Value : integer);
    function GetScrollBarWidth : cardinal;
    procedure SetScrollBarWidth(const Value : cardinal);
    procedure SetOneExpanded(const Value: boolean);
  protected
    procedure Loaded; override;
   {$IFDEF RA_D2}
    procedure GetChildren(Proc: TGetChildProc); override;
   {$ELSE}
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
   {$ENDIF  RA_D2}
    function GetChildParent: TComponent; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Resize; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure ScrollInView(AControl : TControl);
    procedure MouseControls(AControls : array of TControl);
    procedure MouseClasses(AControlClasses : array of TControlClass);
    function AllCollapsed : boolean;
    function AllExpanded : boolean;
    procedure AddBand(Band: TRAScrollMaxBand);

    property BandCount : integer read GetBandCount;
    property Bands[index : integer] : TRAScrollMaxBand read GetBand;
  published
    property ScrollPos : integer read FScrollPos write SetScrollPos default 0;
    property BorderWidth default 3;
    property Beveled : boolean read FBeveled write SetBeveled default true;
    property ButtonFont : TFont read FButtonFont write SetButtonFont;
    property ButtonVisible : boolean read FButtonVisible write SetButtonVisible default true;
    property AutoHeight : boolean read FAutoHeight write SetAutoHeight;
    property ExpandedHeight : integer read FExpandedHeight write SetExpandedHeight default -1;
    property ScrollBarWidth : cardinal read GetScrollBarWidth write SetScrollBarWidth default 7;
    property OneExpanded : boolean read FOneExpanded write SetOneExpanded default false;
    property OnScroll : TNotifyEvent read FOnScroll write FOnScroll;

    property Align;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
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
    property OnResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
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

  ERAScrollMaxError = class(Exception);

var
  crRAHand : integer;
  crRAHandMove : integer;

implementation

uses RAUtils, RADsgnIntf;

{ Cursors resources }
{$R *.RES}


{********************** TRAScrollMax **********************}

type

  TBandBtn = class(TRANoFrameButton)
  private
   {$IFDEF RA_D3H}
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
   {$ENDIF RA_D3H}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  end;

{$IFDEF RA_D3H}
procedure TBandBtn.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  Message.Result := 1;
end;
{$ENDIF RA_D3H}

procedure TBandBtn.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if Parent <> nil then
    with Parent as TRAScrollMaxBand do
    begin
      FParentButtonFont := false;
      Canvas.Font := Self.Font;
      FButton.Height := Canvas.TextHeight('W') + 4;
      Invalidate;
    end;
end;

{ ****** TRAScrollMaxBand ****** }

constructor TRAScrollMaxBand.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csSetCaption, csAcceptsControls];
  Height := 50;
  FExpandedHeight := 50;
  ParentColor := true;
  FParentButtonFont := true;
  FParentButtonVisible := true;
  FParentBeveled := true;
  FButton := TBandBtn.Create(Self);
  with FButton as TBandBtn do begin
    SetDesigning(false);
    Parent := Self;
    Top     := 2;
    Left    := 4;
    Cursor := crArrow;
    OnClick := ButtonClick;
    Margin := 4;
    Spacing := -1;
    NoBorder := false;
    ParentColor := true;
  end;
  Expanded := true;
end;

destructor TRAScrollMaxBand.Destroy;
begin
  inherited Destroy;
end;

procedure TRAScrollMaxBand.Loaded;
begin
  inherited Loaded;
  Perform(CM_PARENTBEVELEDCHANGED, 0, 0);
  Perform(CM_PARENTBUTTONVISIBLECHANGED, 0, 0);
  Perform(CM_PARENTBUTTONFONTCHANGED, 0, 0);
end;

procedure TRAScrollMaxBand.WMSize(var Message: TWMSize);
begin
  if FExpanded then ExpandedHeight := Height;
  inherited;
  if Assigned(FOnResize) then FOnResize(Self);
  if Parent <> nil then RAScrollMax.CorrectHeight;
end;

procedure TRAScrollMaxBand.CMTextChanged(var Message: TMessage);
begin
  FButton.Caption := Caption;
end;

procedure TRAScrollMaxBand.SetExpanded(const Value: boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value;
    if FExpanded then
      FButton.Glyph.LoadFromResourceName(hInstance, 'RASCROLLMAXBTNMINUS')
    else
      FButton.Glyph.LoadFromResourceName(hInstance, 'RASCROLLMAXBTNPLUS');
    if FExpanded and Assigned(FOnExpand) then FOnExpand(Self);
    if not FExpanded and Assigned(FOnCollapse) then FOnCollapse(Self);
    RequestAlign;
    if Parent <> nil then RAScrollMax.CorrectHeight;
   { if not (csLoading in ComponentState) and (RAScrollMax <> nil) then
      DesignerModified(RAScrollMax); }
  end;
end;

procedure TRAScrollMaxBand.SetExpandedHeight(const Value: integer);
begin
  if FExpandedHeight <> Value then
  begin
    FExpandedHeight := Value;
    if FExpanded then
      Height := FExpandedHeight;
     // RequestAlign - called from SetHeight
  end;
end;

// <<< alterado por Adriano
function TRAScrollMaxBand.GetOrder: integer;
var
  i : integer;
begin
  Result := -1;
  if Parent <> nil then
  begin
    for i := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[i] = Self then
      begin
        FOrder := i;
        Result := i;
        break;
      end;
  end;
end;

procedure TRAScrollMaxBand.SetOrder(const Value: integer);
begin
  if FOrder <> Value then
  begin
    if Parent <> nil then
      TRAScrollMaxBands(Parent).SetChildOrder(Self, Value);
    FOrder := GetOrder;
    RequestAlign;
  end;
end;

function TRAScrollMaxBand.GetButtonFont: TFont;
begin
  Result := FButton.Font;
end;

procedure TRAScrollMaxBand.SetButtonFont(Value: TFont);
begin
  FButton.Font := Value;
end;

procedure TRAScrollMaxBand.SetParentButtonFont(const Value: Boolean);
begin
  if FParentButtonFont <> Value then
  begin
    FParentButtonFont := Value;
    if Parent <> nil then Perform(CM_PARENTBUTTONFONTCHANGED, 0, 0);
  end;
end;

procedure TRAScrollMaxBand.CMParentButtonFontChanged(var Message: TMessage);
begin
  if FParentButtonFont then
  begin
    if RAScrollMax <> nil then
      SetButtonFont(RAScrollMax.FButtonFont);
    FParentButtonFont := true;
  end;
end;

function TRAScrollMaxBand.IsButtonFontStored: Boolean;
begin
  Result := not ParentButtonFont;
end;

function TRAScrollMaxBand.GetButtonVisible: boolean;
begin
  Result := FButton.Visible;
end;

procedure TRAScrollMaxBand.SetButtonVisible(const Value: boolean);
begin
  if FButton.Visible <> Value then
  begin
    FParentButtonVisible := false;
    FButton.Visible := Value;
    UpdateSize(Top);
    Invalidate;
  end;
end;

function TRAScrollMaxBand.IsButtonVisibleStored : boolean;
begin
  Result := not ParentButtonVisible;
end;

procedure TRAScrollMaxBand.SetParentButtonVisible(const Value: boolean);
begin
  if FParentButtonVisible <> Value then
  begin
    FParentButtonVisible := Value;
    if Parent <> nil then Perform(CM_PARENTBUTTONVISIBLECHANGED, 0, 0);
  end;
end;

procedure TRAScrollMaxBand.CMParentButtonVisibleChanged(var Message: TMessage);
begin
  if FParentButtonVisible then
  begin
    if RAScrollMax <> nil then
      SetButtonVisible(RAScrollMax.FButtonVisible);
    FParentButtonVisible := true;
  end;
end;

procedure TRAScrollMaxBand.SetBeveled(const Value: boolean);
begin
  if FBeveled <> Value then
  begin
    FParentBeveled := false;
    FBeveled := Value;
    UpdateSize(Top);
    Invalidate;
  end;
end;

function TRAScrollMaxBand.IsBeveledStored : boolean;
begin
  Result := not ParentBeveled;
end;

procedure TRAScrollMaxBand.SetParentBeveled(const Value: boolean);
begin
  if FParentBeveled <> Value then
  begin
    FParentBeveled := Value;
    if Parent <> nil then Perform(CM_PARENTBEVELEDCHANGED, 0, 0);
  end;
end;

procedure TRAScrollMaxBand.CMParentBeveledChanged(var Message: TMessage);
begin
  if FParentBeveled then
  begin
    if RAScrollMax <> nil then
      SetBeveled(RAScrollMax.FBeveled);
    FParentBeveled := true;
  end;
end;

procedure TRAScrollMaxBand.ButtonClick(Sender : TObject);
var
  E : boolean;
begin
  E := true;
  if FExpanded then
    if Assigned(FOnCanCollapse) then FOnCanCollapse(Self, E) else
  else
    if Assigned(FOnCanExpand) then FOnCanExpand(Self, E);
  if E then
    Expanded := not FExpanded;
  DesignerModified(Self);
end;

procedure TRAScrollMaxBand.SetParent(AParent: TWinControl);
begin
  if not ((AParent is TRAScrollMaxBands) or (AParent = nil)) then
    raise ERAScrollMaxError.Create('TRAScrollMaxBand can be putted only into TRAScrollMax component');
  inherited SetParent(AParent);
  if not (csLoading in ComponentState) then
  begin
    Perform(CM_PARENTBEVELEDCHANGED, 0, 0);
    Perform(CM_PARENTBUTTONVISIBLECHANGED, 0, 0);
    Perform(CM_PARENTBUTTONFONTCHANGED, 0, 0);
  end;
end;

procedure TRAScrollMaxBand.SetZOrder(TopMost: Boolean);
begin
  inherited SetZOrder(TopMost);
  RequestAlign;
end;

procedure TRAScrollMaxBand.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  RAScrollMax.BandMouseDown(Self, Button, Shift, X, Y);
end;

procedure TRAScrollMaxBand.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseMove(Shift, X, Y);
  RAScrollMax.BandMouseMove(Self, Shift, X, Y);
end;

procedure TRAScrollMaxBand.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  RAScrollMax.BandMouseUp(Self, Button, Shift, X, Y);
end;

function TRAScrollMaxBand.RAScrollMax : TRAScrollMax;
begin
  if (Parent <> nil) and ((Parent as TRAScrollMaxBands).Parent <> nil) then
    Result := (Parent as TRAScrollMaxBands).Parent as TRAScrollMax
  else
    Result := nil;
end;

function TRAScrollMaxBand.CollapsedHeight : integer;
begin
  Result := FButton.BoundsRect.Bottom + FButton.Top;
end;

procedure TRAScrollMaxBand.UpdateSize(ATop : integer);
var
  W, H : integer;
begin
  if FExpanded then H := FExpandedHeight
  else H := CollapsedHeight;
  if RAScrollMax <> nil then
    W := Parent.Width - RAScrollMax.BorderWidth else
    W := Width;
  SetBounds(0, ATop, W, H);
  if FBeveled then FButton.Left := 16
  else FButton.Left := 4;
  FButton.Width := Width - FButton.Left * 2;
end;

{$IFDEF RA_D2}
procedure TRAScrollMaxBand.RequestAlign;
begin
  if Parent <> nil then Left := Left - 1;
end;
{$ENDIF RA_D2}

procedure TRAScrollMaxBand.Paint;
var
  R : TRect;
const
  Ex : array[boolean] of integer = (BF_TOP, BF_RECT);
begin
  inherited Paint;
  if Canvas.Handle <> 0 then
  begin
    if (csDesigning in ComponentState) then
      DrawDesignFrame(Canvas, ClientRect);
    if FBeveled then
    begin
      R.Left := 1;
      if ButtonVisible then
        R.Top := FButton.Top + FButton.Height div 2 else
        R.Top := 1;
      R.Right := Width - R.Left;
      R.Bottom := Height - 1;
      DrawEdge(Canvas.Handle, R, EDGE_ETCHED, Ex[FExpanded]);
      if ButtonVisible then
      begin
        Canvas.Brush.Color := Color;
        Canvas.Brush.Style := bsSolid;
        Canvas.FillRect(Bounds(FButton.Left - 2, R.Top, FButton.Width + 4, 2));
      end;
    end;
  end;
end;

procedure TRAScrollMaxBand.AlignControls(AControl: TControl; var Rect: TRect);
var
  BevelSize: Integer;
begin
  BevelSize := FBorderWidth;
  if FBeveled then inc(BevelSize, 3);
  InflateRect(Rect, -BevelSize, -BevelSize);
  if ButtonVisible then begin
    inc(Rect.Top, FButton.Height);
    if FButton.Top > FBorderWidth then inc(Rect.Top, FButton.Top);
  end;
  inherited AlignControls(AControl, Rect);
end;

procedure TRAScrollMaxBand.SetBorderWidth(const Value: integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Realign;
  end;
end;

procedure TRAScrollMaxBand.ChangeScale(M, D: Integer); 
begin
  inherited ChangeScale(M, D);
  ExpandedHeight := FExpandedHeight * M div D;
end;
{ ******* TRAScrollMaxBands ******* }

procedure TRAScrollMaxBands.AlignControls(AControl: TControl; var Rect: TRect);

  procedure AdjustBottom;
  begin
    if (Controls[ControlCount -1].BoundsRect.Bottom < Height) and
       (Controls[0].Top < 0) then
    begin
      if Height - (Controls[ControlCount -1].BoundsRect.Bottom - Controls[0].Top) > 0 then
        ScrollControls(-Controls[0].Top)
      else
        ScrollControls(Height - Controls[ControlCount -1].BoundsRect.Bottom);
    end;
  end;

  procedure AdjustBand;
  var
    Band : TRAScrollMaxBand;
  begin
    Band := AControl as TRAScrollMaxBand;
    if (Band <> nil) and Band.FExpanded and
       (Band.BoundsRect.Bottom > Height) and
       (Band.Top > 0) and
       not (csLoading in Band.ComponentState) then
    begin
      ScrollControls(Height - Band.BoundsRect.Bottom);
    end;
  end;

  procedure SetCursor;
  var
    i : integer;
    Cursor : TCursor;
  begin
    if (Controls[ControlCount -1].BoundsRect.Bottom > ClientHeight) or
       (Controls[0].Top < 0) then
      Cursor := crRAHand else
      Cursor := crDefault;
    for i := 0 to ControlCount -1 do
      Controls[i].Cursor := Cursor;
  end;

var
  i : integer;
  RAScrollMax : TRAScrollMax;
  T : integer;
  SMax, SPage, SPos : integer;
begin
  if Scrolling then exit;
  if (Parent <> nil) and (csLoading in Parent.ComponentState) then exit;
  RAScrollMax := Parent as TRAScrollMax;
  if (AControl <> nil) and
     (AControl as TRAScrollMaxBand).FExpanded and
     RAScrollMax.FOneExpanded then
    for i := 0 to ControlCount -1 do
      if not (Controls[i] is TRAScrollMaxBand) then
        raise ERAScrollMaxError.Create('TRAScrollMax can contains only TRAScrollMaxBand components')
      else if Controls[i] <> AControl then
        (Controls[i] as TRAScrollMaxBand).Expanded := false;
  SPos := RAScrollMax.FScrollPos;
  if ControlCount > 0 then
  begin
    for i := 0 to ControlCount -1 do
    begin
      if not (Controls[i] is TRAScrollMaxBand) then
        raise ERAScrollMaxError.Create('TRAScrollMax can contains only TRAScrollMaxBand components');
      if i > 0 then
        T := Controls[i - 1].BoundsRect.Bottom else
        T := -RAScrollMax.FScrollPos;
      (Controls[i] as TRAScrollMaxBand).UpdateSize(T);
    end;
    AdjustBottom;
    AdjustBand;
    SMax := Controls[ControlCount-1].BoundsRect.Bottom - Controls[0].Top;
    SPos := -Controls[0].Top;
    RAScrollMax.FScrollPos := SPos;
    SetCursor;
  end else
    SMax := Height;
  SPage := Height;
  RAScrollMax.ScrollBar.SetParams(0, SMax, SPage, SPos);
end;

procedure TRAScrollMaxBands.ScrollControls(const DeltaY : integer);
begin
  Scrolling := true;
  try
    ScrollBy(0, DeltaY);
  finally
    Scrolling := false;
  end;
end;

procedure TRAScrollMaxBands.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  if (Message.Sender <> nil) and
     ContainsControl(Message.Sender) and
     (Parent <> nil) then
    (Parent as TRAScrollMax).ScrollInView(Message.Sender);
end;

procedure TRAScrollMaxBands.Paint;
var
  R : TRect;
begin
  inherited Paint;
  if (csDesigning in ComponentState) and
     (ControlCount = 0) and
     (Canvas.Handle <> 0) then
  begin
    R := ClientRect;
    Canvas.Font.Color := clAppWorkSpace;
    DrawText(Canvas.Handle, 'Right click and choose "Add band"',
       -1, R, DT_WORDBREAK {or DT_CENTER or DT_VCENTER});
  end;
end;


{ ******* TRAScrollMax ******* }

constructor TRAScrollMax.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls];
  Caption := '';
  Width := 250;
  Height := 150;
  BorderWidth := 3;
  FExpandedHeight := -1;
  FButtonFont := TFont.Create;
  FButtonFont.Name := 'Small Fonts';
  FButtonFont.Size := 7;
  FButtonFont.OnChange := ButtonFontChanged;
  FButtonVisible := true;
  FBeveled := true;
  ParentColor := true;
  pnlEdit := TRAScrollMaxBands.Create(Self);
  with pnlEdit do begin
    ParentColor := true;
    Align       := alClient;
    Parent      := Self;
    ControlStyle := ControlStyle + [csAcceptsControls];
    ParentColor := true;
  end;
  ScrollBar := TRAScrollBar.Create(Self);
  with ScrollBar do begin
    Inclusive := true;
    Parent := Self;
    Width  := 7;
    Align  := alRight;
    Max    := pnlEdit.Height;
    Page   := Self.Height;
    OnScroll := ScrollBarScroll;
    ParentColor := true;
   {$IFDEF RA_D3H}
    DesignInteractive := true;
   {$ENDIF RA_D3H}
  end;
end;

destructor TRAScrollMax.Destroy;
begin
  FButtonFont.Free;
  inherited Destroy;
end;

procedure TRAScrollMax.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN;
    ExStyle := ExStyle or WS_EX_CONTROLPARENT;
  end;
end;

procedure TRAScrollMax.Loaded;
begin
  inherited Loaded;
  Resize;
  pnlEdit.Realign;
end;

procedure TRAScrollMax.SetButtonFont(Value: TFont);
begin
  FButtonFont.Assign(Value);
end;

procedure TRAScrollMax.SetButtonVisible(const Value: boolean);
begin
  if FButtonVisible <> Value then
  begin
    FButtonVisible := Value;
    pnlEdit.NotifyControls(CM_PARENTBUTTONVISIBLECHANGED);
  end;
end;

procedure TRAScrollMax.SetBeveled(const Value: boolean);
begin
  if FBeveled <> Value then
  begin
    FBeveled := Value;
    pnlEdit.NotifyControls(CM_PARENTBEVELEDCHANGED);
  end;
end;

procedure TRAScrollMax.ButtonFontChanged(Sender: TObject);
begin
  pnlEdit.NotifyControls(CM_PARENTBUTTONFONTCHANGED);
end;

procedure TRAScrollMax.MouseControls(AControls : array of TControl);
var
  i : integer;
begin
  for i := Low(AControls) to High(AControls) do
  begin
    TRAScrollMax(AControls[i]).OnMouseDown := BandMouseDown;
    TRAScrollMax(AControls[i]).OnMouseMove := BandMouseMove;
    TRAScrollMax(AControls[i]).OnMouseUp   := BandMouseUp  ;
  end;
end;

procedure TRAScrollMax.MouseClasses(AControlClasses : array of TControlClass);
var
  i, iB, iC : integer;
begin
  for i := Low(AControlClasses) to High(AControlClasses) do
    for iB := 0 to BandCount - 1 do
      for iC := 0 to Bands[iB].ControlCount -1 do
        if Bands[iB].Controls[iC] is AControlClasses[i] then
        begin
          TRAScrollMax(Bands[iB].Controls[iC]).OnMouseDown := BandMouseDown;
          TRAScrollMax(Bands[iB].Controls[iC]).OnMouseMove := BandMouseMove;
          TRAScrollMax(Bands[iB].Controls[iC]).OnMouseUp   := BandMouseUp  ;
        end;
end;

procedure TRAScrollMax.Correct;
var
  Sm : integer;
  CH : integer;
begin
  if BandCount > 0 then
  begin
    Sm := 0;
    CH := pnlEdit.Height;
    if (Bands[BandCount -1].BoundsRect.Bottom < CH) and (Bands[0].Top < 0) then
      Sm := (CH - Bands[BandCount -1].BoundsRect.Bottom);
    if Bands[0].Top + Sm > 0 then Sm := -Bands[0].Top;
    if Sm <> 0 then begin
      pnlEdit.ScrollControls(Sm);
      ScrollBar.Pos := -Bands[0].Top;
      FScrollPos := ScrollBar.Pos;
    end;
  end;
end;

procedure TRAScrollMax.BandMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  CH : integer;
begin
  if (Button = mbLeft) and (BandCount > 0) then
  begin
    Yy := (Sender as TControl).ClientToScreen(Point(0, Y)).Y;
    CH := pnlEdit.Height;
    if (Bands[BandCount -1].BoundsRect.Bottom > CH) or
       (Bands[0].Top < 0) then
      Screen.Cursor := crRAHandMove else
      Screen.Cursor := crDefault;
  end;
end;

procedure TRAScrollMax.BandMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
var
  Sm : integer;
  CH : integer;
begin
  if (ssLeft in Shift) and (BandCount > 0) then
  begin
    Y := (Sender as TControl).ClientToScreen(Point(0, Y)).Y;
    CH := pnlEdit.Height;
    if not (Sender = ScrollBar.Scroller) then
      Sm := Y-Yy else
      Sm := Yy-Y;
    if {Up} Sm < 0 then begin
      if not (Bands[BandCount -1].BoundsRect.Bottom > CH) then Sm := 0
      else if (Bands[BandCount -1].BoundsRect.Bottom + Sm < CH) then
        Sm := CH - Bands[BandCount -1].BoundsRect.Bottom;
    end else if{Down} Sm > 0 then begin
      if not (Bands[0].Top < 0) then Sm := 0
      else if (Bands[0].Top + Sm > 0) then Sm := -Bands[0].Top;
    end;
    if Sm <> 0 then begin
      pnlEdit.ScrollControls(Sm);
      ScrollBar.Pos := -Bands[0].Top;
      FScrollPos := ScrollBar.Pos;
    end;
    Yy := Y;
    Correct;
  end;
end;

procedure TRAScrollMax.BandMouseUp(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  Screen.Cursor := crDefault;
end;

function TRAScrollMax.GetBand(index: integer): TRAScrollMaxBand;
begin
  Result := TRAScrollMaxBand(pnlEdit.Controls[index]);
end;

function TRAScrollMax.GetBandCount: integer;
begin
  Result := pnlEdit.ControlCount;
end;


{$IFDEF RA_D2}
procedure TRAScrollMax.GetChildren(Proc: TGetChildProc);
begin
  pnlEdit.GetChildren(Proc);
end;
{$ELSE}
procedure TRAScrollMax.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  pnlEdit.GetChildren(Proc, Root);
end;
{$ENDIF  RA_D2}

function TRAScrollMax.GetChildParent: TComponent;
begin
  Result := pnlEdit;
end;

procedure TRAScrollMax.SetScrollPos(const Value : integer);
begin
  if FScrollPos <> Value then
  begin
    FScrollPos := Value;
    if not (csLoading in ComponentState) then
    begin
      if FScrollPos > ScrollBar.Max - ScrollBar.Page then
        FScrollPos := ScrollBar.Max - ScrollBar.Page;
      if FScrollPos < 0 then FScrollPos := 0;
      DesignerModified(Self);
      pnlEdit.Realign;
    end;
  end;
end;

procedure TRAScrollMax.ScrollBarScroll(Sender : TObject);
begin
  ScrollPos := ScrollBar.Pos;
  if Assigned(FOnScroll) then FOnScroll(Self);
end;

procedure TRAScrollMax.ScrollInView(AControl : TControl);
var
  i : integer;
  Band : TRAScrollMaxBand;
  Rect : TRect;
begin
  Band := nil;
  for i := 0 to pnlEdit.ControlCount - 1 do
    if (pnlEdit.Controls[i] as TRAScrollMaxBand).ContainsControl(AControl) then
    begin
      Band := pnlEdit.Controls[i] as TRAScrollMaxBand;
      break;
    end;
  if Band = nil then raise ERAScrollMaxError.CreateFmt('Control %s not a child of %s', [AControl.Name, Parent.Name]);
  Band.Expanded := true;
  Rect := AControl.ClientRect;
  Dec(Rect.Top, BevelWidth + BorderWidth + 4);
  Inc(Rect.Bottom, BevelWidth + BorderWidth + 4);
  Rect.TopLeft := ScreenToClient(AControl.ClientToScreen(Rect.TopLeft));
  Rect.BottomRight := ScreenToClient(AControl.ClientToScreen(Rect.BottomRight));
  if Rect.Top < 0 then
    ScrollPos := ScrollPos + Rect.Top
  else if Rect.Bottom > ClientHeight then
  begin
    if Rect.Bottom - Rect.Top > ClientHeight then
      Rect.Bottom := Rect.Top + ClientHeight;
    ScrollPos := ScrollPos + Rect.Bottom - ClientHeight;
  end;
end;

procedure TRAScrollMax.SetAutoHeight(const Value: boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    if FAutoHeight then CorrectHeight;
  end;
end;

procedure TRAScrollMax.SetExpandedHeight(const Value : integer);
begin
  if FExpandedHeight <> Value then
  begin
    FExpandedHeight := Value;
    if FAutoHeight then CorrectHeight;
  end;
end;

procedure TRAScrollMax.Resize;
begin
  inherited Resize;
  if FAutoHeight and (BandCount > 0) and
    not AllCollapsed and (FExpandedHeight > -1) then
    FExpandedHeight := Height;
  if FAutoHeight then CorrectHeight;
end;

procedure TRAScrollMax.CorrectHeight;
var
  i, H : integer;
  Band : TRAScrollMaxBand;
begin
  if not FAutoHeight or (BandCount = 0) then exit;
  if AllCollapsed then
  begin
    H := 0;
    for i := 0 to BandCount - 1 do
      inc(H, Bands[i].Height);
    ClientHeight := H + 2 * PanelBorder(Self);
  end else
    if FExpandedHeight <> -1 then
      Height := FExpandedHeight
    else
    begin
      H := 0;
      Band := nil;
      for i := 0 to BandCount - 1 do
        if Bands[i].Height > H then
        begin
          Band := Bands[i];
          H := Band.Height;
        end;
      H := 0;
      for i := 0 to BandCount - 1 do
        if Bands[i] = Band then
          inc(H, Bands[i].Height)
        else
          inc(H, Bands[i].CollapsedHeight);
      ClientHeight := H + 2 * PanelBorder(Self);
    end;
end;

function TRAScrollMax.AllCollapsed : boolean;
var
  i : integer;
begin
  Result := false;
  for i := 0 to BandCount - 1 do
    if Bands[i].Expanded then exit;
  Result := true;
end;

function TRAScrollMax.AllExpanded : boolean;
var
  i : integer;
begin
  Result := false;
  for i := 0 to BandCount - 1 do
    if not Bands[i].Expanded then exit;
  Result := true;
end;

procedure TRAScrollMax.AddBand(Band: TRAScrollMaxBand);
begin
  Band.Parent := GetChildParent as TWinControl;
end;

function TRAScrollMax.GetScrollBarWidth : cardinal;
begin
  Result := ScrollBar.Width;
end;

procedure TRAScrollMax.SetScrollBarWidth(const Value : cardinal);
begin
  if Value >= 4 then
    ScrollBar.Width := Value;
end;

procedure TRAScrollMax.SetOneExpanded(const Value: boolean);
begin
  if FOneExpanded <> Value then
  begin
    FOneExpanded := Value;
    { .. }
  end;
end;

initialization
  crRAHand := DefineCursor('RAHAND');
  crRAHandMove := DefineCursor('RAHANDMOVE');
end.
