{***********************************************************
                R&A Library
       Copyright (C) 1996-98 R&A

       component   : TRAHint
       description : Custom activated hint

       authors     : Andrei Prygounkov, Roman Tkachev
       e-mail      : black@infa.ru, whiteman@infa.ru
       www         : http://www.infa.ru/black/ralib.htm
************************************************************}

{$INCLUDE RA.INC}

unit RAHint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, RACt;

type

  TRAHint = class(TComponent)
  protected
    R, Area : TRect;
    Txt : string;
    State : (tmBeginShow, tmShowing, tmStopped);
    HintWindow : THintWindow;
    timerHint : TTimer;
    FAutoHide : boolean;
    procedure timerHintTimer(Sender: TObject);
  public
    constructor Create(lOwner : TComponent); override;
    destructor Destroy; override;
    procedure ActivateHint(lArea : TRect; lTxt : string);
    procedure CancelHint;
  published
    property AutoHide : boolean read FAutoHide write FAutoHide default true;
  end;

  TRAhtHintWindow = class(THintWindow)
  private
    htLabel: TRAhtLabel;
  protected
    procedure Paint; override;
  public
   {$IFNDEF RA_D3H}
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
   {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; {$IFDEF RA_D3H} override; {$ENDIF RA_D3H}
  end;

  procedure RegisterHtHints;

implementation

uses RAUtils;


constructor TRAHint.Create(lOwner : TComponent);
begin
  inherited Create(lOwner);
  timerHint := TTimer.Create(self);
  timerHint.Enabled  := false;
  timerHint.Interval := 50;
  timerHint.OnTimer  := timerHintTimer;
  HintWindow := THintWindowClass.Create(Self);
  FAutoHide := true;
end;

destructor TRAHint.Destroy;
begin
  timerHint.Free;
  HintWindow.Free;
  inherited Destroy;
end;

procedure TRAHint.ActivateHint(lArea : TRect; lTxt : string);
var
  P : TPoint;
begin
  GetCursorPos(P);
  Area := lArea;
  if lTxt = '' then
  begin
    CancelHint;
    exit;
  end else
    Txt := lTxt;
  if not PtInRect(Area, P) then
  begin
    if IsWindowVisible(HintWindow.Handle) then
      ShowWindow(HintWindow.Handle, SW_HIDE);
    exit;
  end;
  if HintWindow.Caption <> Txt then
  begin
   {$IFDEF RA_D2}
    R := Rect(0, 0, Screen.Width, 0);
    DrawText(HintWindow.Canvas.Handle, PChar(Txt), -1, R, DT_CALCRECT or DT_LEFT or
      DT_WORDBREAK or DT_NOPREFIX);
    Inc(R.Right, 6);
    Inc(R.Bottom, 2);
   {$ELSE}
    R := HintWindow.CalcHintRect(Screen.Width, Txt, nil);
   {$ENDIF RA_D2}
    R.Top  := P.Y + 20;
    R.Left := P.X;
    inc(R.Bottom, R.Top);
    inc(R.Right, R.Left);
    State := tmBeginShow;
    timerHint.Enabled := true;
  end;
end;

procedure TRAHint.timerHintTimer(Sender: TObject);
var
  P : TPoint;
  bPoint, bDelay : boolean;
  Delay : integer;
  HintPause : integer;
const
  FDelay : integer = 0;
begin
  HintWindow.Color := Application.HintColor;
  Delay := FDelay * integer(timerHint.Interval);
  case State of
    tmBeginShow :
      begin
        GetCursorPos(P);
        bPoint := not PtInRect(Area, P);
        if bPoint then
        begin
          State := tmStopped;
          exit;
        end;
        if IsWindowVisible(HintWindow.Handle) then
          HintPause := Application.HintShortPause else
          HintPause := Application.HintPause;
        if Delay >= HintPause then
        begin
          HintWindow.ActivateHint(R, Txt);
          FDelay := 0;
          State := tmShowing;
        end else
          inc(FDelay);
      end;{tmBeginShow}
    tmShowing :
      begin
        GetCursorPos(P);
        bDelay := FAutoHide and (Delay > Application.HintHidePause);
        bPoint := not PtInRect(Area, P);
        if bPoint or bDelay then
        begin
          if IsWindowVisible(HintWindow.Handle) then
            ShowWindow(HintWindow.Handle, SW_HIDE);
          FDelay := 0;
          if bPoint then HintWindow.Caption := 'Это hint';
          State := tmStopped;
        end else
          inc(FDelay);
      end;{tmShowing}
    tmStopped:
      begin
        FDelay := 0;
        GetCursorPos(P);
        bPoint := not PtInRect(Area, P);
        if IsWindowVisible(HintWindow.Handle) then
          ShowWindow(HintWindow.Handle, SW_HIDE);
        if bPoint then
        begin
          HintWindow.Caption := 'Это hint';
          timerHint.Enabled := false;
        end;
      end;{tmStopped}
  end;{case}
end;

procedure TRAHint.CancelHint;
begin
  if IsWindowVisible(HintWindow.Handle) then
    ShowWindow(HintWindow.Handle, SW_HIDE);
  HintWindow.Caption := '';
end;


{ Color hints }

constructor TRAhtHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  htLabel := TRAhtLabel.Create(Self);
  htLabel.Parent := Self;
  htLabel.SetBounds(2, 2, 0, 0);
end;

procedure TRAhtHintWindow.Paint;
begin
end;

{$IFNDEF RA_D3H}
procedure TRAhtHintWindow.ActivateHint(Rect: TRect; const AHint: string);
var
  R: TRect;
begin
  R := CalcHintRect(Screen.Width, AHint, nil);
  inherited ActivateHint(Bounds(Rect.Left, Rect.Top, R.Right, R.Bottom + 4),
    AHint);
end;
{$ENDIF}

function TRAhtHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string;
	AData: Pointer): TRect;
begin
  htLabel.Caption := AHint;
  Result := Bounds(0, 0, htLabel.Width + 6, htLabel.Height + 2);
  if Application.HintHidePause > 0 then
    Application.HintHidePause := Max(2500 {default},
      Length(ItemHtPlain(AHint)) *
      (1000 div 20) { 20 symbols per second } );
end;


procedure RegisterHtHints;
begin
  if Application.ShowHint then
  begin
    Application.ShowHint := False;
    HintWindowClass := TRAhtHintWindow;
    Application.ShowHint := True;
  end
  else
    HintWindowClass := TRAhtHintWindow;
end;

end.
