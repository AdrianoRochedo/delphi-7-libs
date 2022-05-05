{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       component   : TRAScrollBar95
       description : ScrollBar with Page property

       author      : Andrei Prygounkov
       e-mail      : black@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAScrollBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Graphics, StdCtrls, ExtCtrls;

type

  TRAScrollBar95 = class(TScrollBar)
  protected
    FPage : integer;
    procedure SetPage(lPage : integer);
    procedure SetPos(lPos : integer);
    function GetPos : integer; {не работает}
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Pos : integer read GetPos write SetPos;
  published
    property Page : integer read FPage write SetPage;
  end;

  TRAControlScrollBar95 = class
  private
    FKind: TScrollBarKind;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FSmallChange: TScrollBarInc;
    FLargeChange: TScrollBarInc;
    FPage : integer;
    FHandle : hWnd;
    FOnScroll: TScrollEvent;
   // FVisible : boolean;
    procedure SetParam(index, Value: Integer);
   // procedure SetVisible(Value : boolean);
   // procedure SetLargeChange(Value: TScrollBarInc);
  protected
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer); dynamic;
  public
    constructor Create;
    procedure SetParams(AMin, AMax, APosition, APage : integer);
    procedure DoScroll(var Message: TWMScroll);

    property Kind: TScrollBarKind read FKind write FKind default sbHorizontal;
    property SmallChange: TScrollBarInc read FSmallChange write FSmallChange default 1;
    property LargeChange: TScrollBarInc read FLargeChange write FLargeChange default 1;
    property Min  : Integer index 0 read FMin write SetParam default 0;
    property Max  : Integer index 1 read FMax write SetParam default 100;
    property Position : Integer index 2 read FPosition write SetParam default 0;
    property Page : integer index 3 read FPage write SetParam;
    property Handle : hWnd read FHandle write FHandle;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
   // property Visible : boolean read FVisible write SetVisible;
  end;

  TRAScrollBar = class(TCustomPanel)
  private
    FMin, FMax : integer;
    FPos  : integer;
    FPage : integer;
    Scroll : TPanel;
    FDesignInteractive : boolean;
    FInclusive : boolean;
    FOnChange : TNotifyEvent;
    FOnScroll : TNotifyEvent;
    procedure SetParam(index, Value : integer);
    procedure SetInclusive(Value : boolean);
  protected
    procedure CreateWnd; override;
    procedure SetTrackBar;
    procedure Loaded; override;
    procedure Resize; override;
  public
    constructor Create(AOwner : TComponent); override;
    procedure SetParams(const AMin, AMax, APage, APos : integer);
    property Pos : integer index 3 read FPos write SetParam;
    property DesignInteractive : boolean read FDesignInteractive write FDesignInteractive;
    property Scroller : TPanel read Scroll;
  published
    property Color;
    property Align;
    property Min  : integer index 0 read FMin write SetParam;
    property Max  : integer index 1 read FMax write SetParam;
    property Page : integer index 2 read FPage write SetParam;
    property Position : integer index 3 read FPos write SetParam;
    property Inclusive : boolean read FInclusive write SetInclusive;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnScroll : TNotifyEvent read FOnScroll write FOnScroll;
  end;

implementation

uses Consts, RACnst;

constructor TRAScrollBar95.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPage := 1;
end;

procedure TRAScrollBar95.SetPage(lPage : integer);
var
  SCROLLINFO : TSCROLLINFO;
begin
 FPage := lPage;
 if HandleAllocated then begin
    with SCROLLINFO do begin
      cbSize := sizeof(TSCROLLINFO);
      fMask  := SIF_PAGE;
      nPage  := FPage;
    end;
    SetScrollInfo(
      Handle,         // handle of window with scroll bar
      SB_CTL ,        // scroll bar flag
      SCROLLINFO,     // pointer to structure with scroll parameters
      true            // redraw flag
    );
  end;
end;

procedure TRAScrollBar95.SetPos(lPos : integer);
var
  SCROLLINFO : TSCROLLINFO;
begin
 if HandleAllocated then begin
    with SCROLLINFO do begin
      cbSize := sizeof(TSCROLLINFO);
      fMask  := SIF_POS;
      nPos := lPos;
    end;
    SetScrollInfo(
      Handle,         // handle of window with scroll bar
      SB_CTL ,        // scroll bar flag
      SCROLLINFO,     // pointer to structure with scroll parameters
      true            // redraw flag
    );
  end;
 Position := lPos;
end;

function TRAScrollBar95.GetPos : integer;
begin
  Result := Position;
end;

procedure TRAScrollBar95.CreateWnd;
begin
  inherited CreateWnd;
  SetPage(FPage);
end;

{************************* TRAControlScrollBar95 ****************************}
constructor TRAControlScrollBar95.Create;
begin
  FPage := 1;
  FSmallChange := 1;
  FLargeChange := 1;
end;

const
  SBKIND : array[TScrollBarKind] of integer = (SB_HORZ, SB_VERT);

procedure TRAControlScrollBar95.SetParams(AMin, AMax, APosition, APage : integer);
var
  SCROLLINFO : TSCROLLINFO;
begin
  if AMax < AMin then
    raise EInvalidOperation.Create(SScrollBarRange);
  if APosition < AMin then APosition := AMin;
  if APosition > AMax then APosition := AMax;
  if Handle > 0 then begin
    with SCROLLINFO do begin
      cbSize := sizeof(TSCROLLINFO);
      fMask := SIF_DISABLENOSCROLL;
      if (AMin >= 0) or (AMax >= 0) then fMask := fMask or SIF_RANGE;
      if APosition >= 0 then fMask := fMask or SIF_POS;
      if APage >= 0 then fMask := fMask or SIF_PAGE;
      nPos := APosition;
      nMin := AMin;
      nMax := AMax;
      nPage := APage;
    end;
    SetScrollInfo(
      Handle,         // handle of window with scroll bar
      SBKIND[Kind] ,  // scroll bar flag
      SCROLLINFO,     // pointer to structure with scroll parameters
      true            // redraw flag
    );
  end;
end;

procedure TRAControlScrollBar95.SetParam(index, Value: Integer);
begin
  case index of
    0 : FMin := Value;
    1 : FMax := Value;
    2 : FPosition := Value;
    3 : FPage := Value;
  end;
  if FMax < FMin then
    raise EInvalidOperation.Create(SScrollBarRange);
  if FPosition < FMin then FPosition := FMin;
  if FPosition > FMax then FPosition := FMax;
  SetParams(FMin, FMax, FPosition, FPage);
end;

{
procedure TRAControlScrollBar95.SetVisible(Value : boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if Handle <> 0 then

  end;
end;
}

procedure TRAControlScrollBar95.DoScroll(var Message: TWMScroll);
var
  ScrollPos: Integer;
  NewPos: Longint;
  ScrollInfo: TScrollInfo;
begin
  with Message do
  begin
    NewPos := FPosition;
    case TScrollCode(ScrollCode) of
      scLineUp:
        Dec(NewPos, FSmallChange);
      scLineDown:
        Inc(NewPos, FSmallChange);
      scPageUp:
        Dec(NewPos, FLargeChange);
      scPageDown:
        Inc(NewPos, FLargeChange);
      scPosition, scTrack:
        with ScrollInfo do
        begin
          cbSize := SizeOf(ScrollInfo);
          fMask := SIF_ALL;
          GetScrollInfo(Handle, SBKIND[Kind], ScrollInfo);
          NewPos := nTrackPos;
        end;
      scTop:
        NewPos := FMin;
      scBottom:
        NewPos := FMax;
    end;
    if NewPos < FMin then NewPos := FMin;
    if NewPos > FMax then NewPos := FMax;
    ScrollPos := NewPos;
    Scroll(TScrollCode(ScrollCode), ScrollPos);
  end;
  Position := ScrollPos;
end;

procedure TRAControlScrollBar95.Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then FOnScroll(Self, ScrollCode, ScrollPos);
end;
{######################### TRAControlScrollBar95 #########################}



{************************* TRAControlScrollBar95 *************************}

type

  TScroller = class(TPanel)
  private
    Yy : integer;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
  protected
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
  end;

procedure TScroller.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  if (Button = mbLeft) then
    Yy := Y;
end;

procedure TScroller.MouseMove(Shift : TShiftState; X, Y : Integer);
var
  Sm, T, OldPos : integer;
begin
  if Shift = [ssLeft] then
  begin
    Sm := Yy - Y;
    T := Top;
    if (Sm <> 0) then
    begin
      with Parent as TRAScrollBar do
      begin
        OldPos := Pos;
        Pos := Pos - Round(Sm * (FMax - FMin + 1) / ClientHeight);
        if (Pos <> OldPos) and Assigned(FOnScroll) then
          FOnScroll(Parent);
      end;
    end;
    Yy := Y - Top + T;
  end;
end;

procedure TScroller.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  with (Owner as TRAScrollBar) do
    Message.Result := integer(FDesignInteractive and (FPage <> FMax - FMin + 1)) ;
end;

constructor TRAScrollBar.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvLowered;
  Color := clAppWorkSpace;
  Caption := '';
  ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls];
  Scroll := TScroller.Create(Self);
  Scroll.Parent := Self;
  Scroll.Caption := '';
  Scroll.ControlStyle := ControlStyle - [csSetCaption, csAcceptsControls];
  FMax := 100;
  FPage := 10;
  Width := 20;
  Height := 100;
end;

procedure TRAScrollBar.Loaded;
begin
  inherited Loaded;
  Resize;
end;

procedure TRAScrollBar.Resize;
begin
  inherited Resize;
  with Scroll do begin
    Top := BevelWidth;
    Left := BevelWidth;
    Width := Self.Width - 2*BevelWidth;
  end;
  SetTrackBar;
end;

procedure TRAScrollBar.SetTrackBar;
var
  CH, H, T : integer;
  L, FP, P, P1 : integer;
begin
 {перед изменением кода обязательно сделайте копию!}
  if FMin > FMax then FMin := FMax;
  if FPage > FMax-FMin+1 then FPage := FMax-FMin+1;
  if FInclusive then P := FPage else P := 0;
  P1 := FPage - P;
  if FPos > FMax-P then FPos := FMax-P;
  if FPos < FMin then FPos := FMin;
  L := FMax - FMin +1;
  CH := Height - 2*BevelWidth;
  H := Trunc(CH * FPage / L) +1;
  FP := Trunc((FPos-FMin) / L * (L - P1)) +1;
  T := Round(CH * FP / L);
  if H < 7 then H := 7;
  if H > CH then H := CH;
  if T < BevelWidth then T := BevelWidth;
  if T + H > Height - BevelWidth then
    T := Height - BevelWidth - H;
  if FPos = FMax - P then T := Height - BevelWidth - H;

  with Scroll do
    SetBounds(Left, T, Width, H);
end;

procedure TRAScrollBar.SetParam(index, Value : integer);
begin
  case index of
    0 : FMin  := Value;
    1 : FMax  := Value;
    2 : FPage := Value;
    3 : FPos  := Value;
  end;
  SetParams(FMin, FMax, FPage, FPos);
end;

procedure TRAScrollBar.SetParams(const AMin, AMax, APage, APos : integer);
begin
  FMin  := AMin ;
  FMax  := AMax ;
  FPage := APage;
  FPos  := APos ;
  if Assigned(FOnChange) then FOnChange(Self);
  SetTrackBar;
end;

procedure TRAScrollBar.SetInclusive(Value : boolean);
begin
  FInclusive := Value;
  SetTrackBar;
end;

procedure TRAScrollBar.CreateWnd;
begin
  inherited CreateWnd;
  SetTrackBar;
end;


end.
