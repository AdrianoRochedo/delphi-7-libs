unit Rochedo.Simulators.Shapes;

interface
uses Windows, Messages, SysUtils, Classes, Controls, Forms, Menus,
     Graphics, StdCtrls, ExtCtrls,
     Rochedo.Simulators.Components;

Type

  TdrBaseShape = class(TGraphicControl, IComponent)
  private
    FObject              : IVisualObject;
    FStopBlink           : Boolean;
    FBlinkTimes          : Integer;
    FBlinkCount          : Integer;
    FSaveColor           : TColor;
    FTimer               : TTimer;
    F3D                  : Boolean;
    FSelected            : Boolean;
    FOnChange            : TNotifyEvent;
    FOnDblClick          : TNotifyEvent;
    FOnBeforeDestruction : TNotifyEvent;

    // IComponent interface ---------------------------------

    procedure setMouseDownEvent(Event: TMouseEvent);
    procedure setMouseMoveEvent(Event: TMouseMoveEvent);
    procedure setMouseUpEvent(Event: TMouseEvent);
    procedure setClickEvent(Event: TNotifyEvent);
    procedure setDblClickEvent(Event: TNotifyEvent);

    procedure setBounds(const Rect: TRect);
     function getBounds(): TRect;
    procedure setPos(Pos: TPoint);
     function getPos(): TPoint;

    procedure Dispose();
    procedure setParent(Parent: TWinControl);
    procedure setObject(Obj: IVisualObject);
     function getObject(): IVisualObject;
    procedure setHint(const s: string);
    procedure setColor(const Color: TColor);
     function getColor(): TColor;
     function getInstance(): TObject;

    // IComponent interface ---------------------------------

     function getCenter(): TPoint;
    procedure set3D(Value: Boolean);
    procedure setSelected(Value: Boolean);
    procedure wmLButtonDlbClk (var Msg: TWMLButtonDblClk); message wm_LButtonDblClk;
    procedure timerTick(Sender: TObject);
    procedure setCenter(const Value: TPoint);
    destructor destroy; override;
  protected
    procedure beforeDestruction(); override;
  public
    NotPaint: Boolean;
    Tag_1: Longint;
    Tag_2: Longint;

    constructor Create(AOwner: TComponent); override;

    procedure Paint(); override;
    procedure Blink(Times: Integer);
    procedure StopBlink();

    property Center: TPoint read GetCenter write SetCenter;
    property Canvas;
  published
    procedure StyleChanged(Sender: TObject);

    property ThreeD: Boolean read F3D write Set3D Default False;
    property Selected: Boolean read FSelected write setSelected;

    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClick;

    // Disparado quando um duplo-Click ocorre
    property OnDblClick : TNotifyEvent read FOnDblClick write FOnDblClick;

    // Disparado quando mudancas visuais ocorrem
    property OnChange : TNotifyEvent read FOnChange write FOnChange;

    // Disparado antes da destruicao da instancia
    property OnBeforeDestruction : TNotifyEvent read FOnBeforeDestruction write FOnBeforeDestruction;
  end;

  {---------------------------------------------------}

  TdrRectangle = Class(TdrBaseShape)
  Private
    FSquare  : Boolean;
    FRound   : Boolean;
    procedure  SetSquare(Value: Boolean);
    procedure   SetRound(Value: Boolean);
  Public
    procedure Paint(); override;
  Published
    property Square: Boolean read FSquare Write SetSquare Default False;
    property  Round: Boolean read  FRound Write  SetRound Default False;
  End;

  {----------------------------------------------------}

  TdrFlag = Class(TdrBaseShape)
  private
    FTransparent: boolean;
    FBackgroundColor: TColor;
  Public
    constructor Create(AOwner: TComponent); override;
    procedure Paint(); override;

    property Transparent : boolean read FTransparent write FTransparent;
    property BackgroundColor : TColor read FBackgroundColor write FBackgroundColor;
  End;

  {----------------------------------------------------}

  TdrEllipse = Class(TdrBaseShape)
  Private
    FRound : Boolean;
    procedure SetRound(Value: Boolean);
  Public
    procedure Paint; override;
  Published
    property Round: Boolean read  FRound Write  SetRound Default False;
  End;

  TdrEllipseBitmap = Class(TdrEllipse)
  private
    FBitmap: TBitmap;
  Public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; Override;
    property Bitmap: TBitmap read FBitmap;
  end;

  TTriangle = Record
                 P1: TPoint;
                 P2: TPoint;
                 P3: TPoint;
               End;

  TdrTriangle = class(TdrBaseShape)
  Private
    FPoints: TTriangle;
    procedure SetPoints(const Value: TTriangle);
  Public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property  Points: TTriangle write SetPoints;
  End;

  {-------------------------------------------}

  TArrowDirection = (adUp, adDown, adLeft, adRight);

  TdrArrow = class(TdrBaseShape)
  private
    FDirection: TArrowDirection;
    FArrowHeight: Integer;
    FFilled: Boolean;
    FTransparent: boolean;
    FBackgroundColor: TColor;
    procedure SetDirection (Value: TArrowDirection);
    procedure SetArrowHeight (Value: Integer);
    procedure SetFilled (Value: Boolean);
    procedure WMLButtonDlbClk (var Msg: TWMLButtonDblClk); message wm_LButtonDblClk;
  public
    constructor Create (AOwner: TComponent); override;
    procedure  Paint; override;
  published
    property Transparent : boolean read FTransparent write FTransparent;
    property BackgroundColor : TColor read FBackgroundColor write FBackgroundColor;
    property Direction: TArrowDirection read FDirection write SetDirection default adRight;
    property ArrowHeight: Integer read FArrowHeight write SetArrowHeight default 10;
    property Filled: Boolean  read FFilled write SetFilled default True;
    property OnClick;
  end;

  {-------------------------------------------}

  TdrBitmap = Class(TdrBaseShape)
  Private
    FBitmap: TBitmap;
    FDrawFrame: Boolean;
  Public
    constructor Create(aOwner: TComponent; const BitmapName: String);
    destructor Destroy; override;
    procedure Paint; override;
    property Bitmap: TBitmap read FBitmap;
    property DrawFrame: Boolean read FDrawFrame write FDrawFrame;
  End;

implementation
uses WinUtils;

procedure BevelLine(Canvas: TCanvas; C: TColor; X1, Y1, X2, Y2: Integer);
begin
  with Canvas do
    begin
    Pen.Color := C;
    MoveTo(X1, Y1);
    LineTo(X2, Y2);
    end;
end;

{ TdrBaseShape }

constructor TdrBaseShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 45;
  Height := 45;
  F3D := True;
  ShowHint := True;
  Visible := True;
  FSelected := False;
  FStopBlink := True;
  Color := clBlue;
  Canvas.Brush.Color := Color;
end;

procedure TdrBaseShape.WMLButtonDlbClk (var Msg: TWMLButtonDblClk);
begin
  inherited;
  if Assigned(FOnDblClick) then FOnDblClick(self);
end;

Function TdrBaseShape.GetCenter: TPoint;
Begin
  Result.y := Top  + Height div 2;
  Result.x := Left + Width  div 2;
End;

procedure TdrBaseShape.SetCenter(const Value: TPoint);
begin
  inherited setBounds(Value.X - Width div 2, Value.Y - Height div 2, Width, Height);
end;

procedure TdrBaseShape.Set3D(Value: Boolean);
Begin
  If Value <> F3D Then
     Begin
     F3D := Value;
     Invalidate();
     if Assigned(FOnChange) then FOnChange(Self);
     End;
End;

procedure TdrBaseShape.SetSelected(Value: Boolean);
begin
  if Value <> FSelected then
     begin
     FSelected := Value;
     if FSelected then
        Canvas.Pen.Mode := pmNOTCOPY
     else
        Canvas.Pen.Mode := pmCOPY;

     Invalidate;
     end;
end;

procedure TdrBaseShape.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;

{-------------------------------------------------------------------------}

procedure TdrRectangle.SetSquare(Value: Boolean);
Begin
  If Value <> FSquare Then
     Begin
     FSquare := Value;
     Invalidate;
     if Assigned(FOnChange) then FOnChange(Self);
     End;
End;

procedure TdrRectangle.SetRound(Value: Boolean);
Begin
  If Value <> FRound Then
     Begin
     FRound := Value;
     Invalidate;
     if Assigned(FOnChange) then FOnChange(Self);
     End;
End;

procedure TdrRectangle.Paint();
var
  X, Y, W, H, S: Integer;
  r: TRect;
begin
  If NotPaint or
     not Visible or
     (Parent = nil) Then Exit;

  inherited;

  with Canvas do
    begin
    Brush.Color := Color;

    X := Pen.Width div 2;
    Y := X;
    W := Width - Pen.Width + 1;
    H := Height - Pen.Width + 1;
    if W < H then S := W else S := H;

    if FSquare then
       begin
       Inc(X, (W - S) div 2);
       Inc(Y, (H - S) div 2);
       W := S;
       H := S;
       end;

    If FRound Then
       RoundRect(X, Y, X + W, Y + H, S div 4, S div 4)
    Else
       If F3D Then
          Begin
          r := Rect(X, Y, X + W, Y + H);
          Frame3D(Canvas, r, clBtnHighlight, clBtnShadow, 2);
          FillRect(r);
          End
       Else
          Rectangle(X, Y, X + W, Y + H);

    end; {With Canvas}
end; {Paint}

{------------------------------------------------------------}

procedure TdrEllipse.SetRound(Value: Boolean);
Begin
  If Value <> FRound Then
     Begin
     FRound := Value;
     Invalidate;
     if Assigned(FOnChange) then FOnChange(Self);
     End;
End;

procedure TdrEllipse.Paint;
var
  X, Y, W, H, S: Integer;
begin
  If NotPaint or
     not Visible or
     (Parent = nil) Then Exit;

  inherited;

  with Canvas do
    begin
    Brush.Color := Color;

    X := Pen.Width div 2;
    Y := X;
    W := Width - Pen.Width + 1;
    H := Height - Pen.Width + 1;
    if W < H then S := W else S := H;

    if FRound then
       begin
       Inc(X, (W - S) div 2);
       Inc(Y, (H - S) div 2);
       W := S;
       H := S;
       end;

    If F3D Then
       Begin
       pen.color := color;
       Ellipse(1,1,w-3,h-3);

       pen.color := clBtnHighlight; {canto de cima}
       Arc(0,0,w-2,h-2, w div 5 * 4,h div 5,w div 5,h div 5 * 4);
       Arc(1,1,w-3,h-3, w div 5 * 4,h div 5,w div 5,h div 5 * 4);

       pen.color := clBtnShadow; {canto de baixo}
       Arc(0,0,w-2,h-2, w div 5,h div 5 * 4,w div 5 * 4,h div 5);
       Arc(1,1,w-3,h-3,w div 5,h div 5 * 4,w div 5 * 4,h div 5);
       End
    Else
       Ellipse(X, Y, X + W, Y + H);
    end; {With Canvas}
end; {Paint}

{--------------------------------------------------------------}

constructor TdrArrow.Create (AOwner: TComponent);
begin
  inherited Create (AOwner);
  FTransparent := true;
  FBackgroundColor := clBtnFace;
  FDirection := adRight;
  Width := 50;
  Height := 20;
  FArrowHeight := 10;
  FFilled := True;
end;

procedure TdrArrow.SetDirection (Value: TArrowDirection);
begin
  if FDirection <> Value then
    begin
    FDirection := Value;
    Invalidate;
    end;
end;

procedure TdrArrow.SetArrowHeight (Value: Integer);
begin
  if FArrowHeight <> Value then
  begin
    FArrowHeight := Value;
    Invalidate;
  end;
end;

procedure TdrArrow.SetFilled (Value: Boolean);
begin
  if FFilled <> Value then
  begin
    FFilled := Value;
    Invalidate;
  end;
end;

procedure TdrArrow.Paint;
var
  X, Y, W, H: integer;
  r: TRect;
  XCenter, YCenter: Integer;
  ArrowPoints: array [0..3] of TPoint;
begin
  If NotPaint or
     not Visible or
     (Parent = nil) Then Exit;

  inherited Paint();

  if not FTransparent then
     begin
     X := Canvas.Pen.Width div 2;
     Y := X;
     W := Width - Canvas.Pen.Width + 1;
     H := Height - Canvas.Pen.Width + 1;

     r := Rect(X, Y, X + W, Y + H);
     Canvas.Brush.Color := FBackgroundColor;
     Canvas.fillRect(r);
     end;

  YCenter := (Height - 1) div 2;
  XCenter := (Width  - 1) div 2;

  Canvas.Brush.Color := Color;

  {draw a line and compute the triangle
  for the arrow point}
  case FDirection of
    adUp:
      begin
      BevelLine(Canvas, clBtnHighlight, XCenter,   Height-1, XCenter  , FArrowHeight);
      BevelLine(Canvas, clBtnShadow   , XCenter+1, Height-1, XCenter+1, FArrowHeight);
      end;

    adDown:
      begin
      BevelLine(Canvas, clBtnHighlight, XCenter,   0, XCenter  , Height - 1 - FArrowHeight);
      BevelLine(Canvas, clBtnShadow   , XCenter+1, 0, XCenter+1, Height - 1 - FArrowHeight);
      end;

    adRight:
      begin
      BevelLine(Canvas, clBtnHighlight, Width - 1, YCenter  , FArrowHeight, YCenter);
      BevelLine(Canvas, clBtnShadow   , Width - 1, YCenter+1, FArrowHeight, YCenter+1);
      end;

    adLeft:
      begin
      BevelLine(Canvas, clBtnHighlight, 0, YCenter  , Width - 1 - FArrowHeight, YCenter);
      BevelLine(Canvas, clBtnShadow   , 0, YCenter+1, Width - 1 - FArrowHeight, YCenter+1);
      end;
    end;

  Canvas.Pen.Color := Color;

  case FDirection of
    adUp:
      begin
      ArrowPoints [0] := Point (0, FArrowHeight);
      ArrowPoints [1] := Point (XCenter, 0);
      ArrowPoints [2] := Point (Width-1, FArrowHeight);
      ArrowPoints [3] := Point (0, FArrowHeight);
      end;

    adDown:
      begin
      ArrowPoints [0] := Point (XCenter, Height - 1);
      ArrowPoints [1] := Point (0, Height - 1 - FArrowHeight);
      ArrowPoints [2] := Point (Width - 1, Height - 1 - FArrowHeight);
      ArrowPoints [3] := Point (XCenter, Height - 1);
      end;

    adLeft:
      begin
      ArrowPoints [0] := Point (FArrowHeight, Height - 1);
      ArrowPoints [1] := Point (0, YCenter);
      ArrowPoints [2] := Point (FArrowHeight, 0);
      ArrowPoints [3] := Point (FArrowHeight, Height - 1);
      end;

    adRight:
      begin
      ArrowPoints [0] := Point (Width - 1 - FArrowHeight, Height - 1);
      ArrowPoints [1] := Point (Width - 1 - FArrowHeight, 0);
      ArrowPoints [2] := Point (Width - 1, YCenter);
      ArrowPoints [3] := Point (Width - 1 - FArrowHeight, Height - 1);
      end;
    end;

  {draw the arrow point, eventually filling it}
  if FFilled then
     Canvas.Polygon (ArrowPoints)
  else
     Canvas.PolyLine (ArrowPoints);

  If F3D Then
     case FDirection of
       adUp:
         begin
         BevelLine(Canvas, clBtnHighlight, 0, FArrowHeight, XCenter, 0);
         Canvas.Pen.Color := clBtnShadow;
         Canvas.LineTo(Width-1, FArrowHeight+1);
         Canvas.LineTo(1, FArrowHeight+1);
         end;

       adDown:
         begin
         BevelLine(Canvas, clBtnHighlight, 0, FArrowHeight-1, Width, FArrowHeight-1);
         Canvas.Pen.Color := clBtnShadow;
         Canvas.LineTo(XCenter, Height-1);
         Canvas.LineTo(0, FArrowHeight);
         end;

       adLeft:
         begin
         BevelLine(Canvas, clBtnHighlight, FArrowHeight, Height - 1, 0, YCenter);
         Canvas.Pen.Color := clBtnShadow;
         Canvas.LineTo(FArrowHeight, 0);
         Canvas.LineTo(FArrowHeight, Height - 1);
         end;

       adRight:
         begin
         BevelLine(Canvas, clBtnHighlight, Width - 1 - FArrowHeight, Height - 1,Width - 1 - FArrowHeight, 0);
         Canvas.Pen.Color := clBtnShadow;
         Canvas.LineTo(Width - 1, YCenter);
         Canvas.LineTo(Width - 1 - FArrowHeight, Height - 1);
         end;
       end;
end;

procedure TdrArrow.WMLButtonDlbClk (var Msg: TWMLButtonDblClk);
var
  ArrowPoints: array [0..2] of TPoint;
  XCenter, YCenter: Integer;
  HRegion: HRgn;
begin
  YCenter := (Height - 1) div 2;
  XCenter := (Width - 1) div 2;
  case FDirection of
    adUp:
      begin
      ArrowPoints [0] :=
        Point (0, FArrowHeight);
      ArrowPoints [1] :=
        Point (XCenter, 0);
      ArrowPoints [2] :=
        Point (Width-1, FArrowHeight);
      end;

    adDown:
      begin
      ArrowPoints [0] := Point (
        XCenter, Height - 1);
      ArrowPoints [1] := Point (
        0, Height - 1 - FArrowHeight);
      ArrowPoints [2] := Point (
        Width - 1, Height - 1 - FArrowHeight);
      end;

    adLeft:
      begin
      ArrowPoints [0] :=
        Point (FArrowHeight, Height - 1);
      ArrowPoints [1] :=
        Point (0, YCenter);
      ArrowPoints [2] :=
        Point (FArrowHeight, 0);
      end;

    adRight:
      begin
      ArrowPoints [0] := Point (
        Width - 1 - FArrowHeight, Height - 1);
      ArrowPoints [1] := Point (
        Width - 1 - FArrowHeight, 0);
      ArrowPoints [2] := Point (
        Width - 1, YCenter);
      end;
    end;

  {check is the click took place
  in the arrow-point region}
  HRegion := CreatePolygonRgn (ArrowPoints, 3, WINDING);
  if PtInRegion (HRegion, Msg.XPos, Msg.YPos) then
     if Assigned (FOnDblClick) then FOnDblClick (self);
end;

procedure TdrBaseShape.BeforeDestruction();
begin
  inherited;
  if Assigned(FOnBeforeDestruction) then
     FOnBeforeDestruction(self);
end;

function TdrBaseShape.getBounds(): TRect;
begin
  result := self.BoundsRect;
end;

function TdrBaseShape.getInstance(): TObject;
begin
  result := self;
end;

procedure TdrBaseShape.setBounds(const Rect: TRect);
begin
  self.BoundsRect := Rect;
end;

procedure TdrBaseShape.setClickEvent(Event: TNotifyEvent);
begin
  self.OnClick := Event;
end;

procedure TdrBaseShape.setColor(const Color: TColor);
begin
  self.Color := Color;
  self.Paint();
end;

procedure TdrBaseShape.setDblClickEvent(Event: TNotifyEvent);
begin
  self.OnDblClick := Event;
end;

procedure TdrBaseShape.setHint(const s: string);
begin
  self.Hint := s;
end;

procedure TdrBaseShape.setMouseDownEvent(Event: TMouseEvent);
begin
  self.OnMouseDown := Event;
end;

procedure TdrBaseShape.setMouseMoveEvent(Event: TMouseMoveEvent);
begin
  self.OnMouseMove := Event;
end;

procedure TdrBaseShape.setMouseUpEvent(Event: TMouseEvent);
begin
  self.OnMouseUp := Event;
end;

procedure TdrBaseShape.setParent(Parent: TWinControl);
begin
  self.Parent := Parent;
end;

procedure TdrBaseShape.setPos(Pos: TPoint);
begin
  self.SetCenter(Pos);
end;

function TdrBaseShape.getPos(): TPoint;
begin
  result := self.Center;
end;

procedure TdrBaseShape.setObject(Obj: IVisualObject);
begin
  FObject := Obj;
end;

procedure TdrBaseShape.Dispose();
begin
  Free();
end;

function TdrBaseShape.getObject(): IVisualObject;
begin
  result := FObject;
end;

function TdrBaseShape.getColor(): TColor;
begin
  result := Color;
end;

{ TdrTriangle }

constructor TdrTriangle.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FPoints.P1.x := -1; // indica que o desenho do triangulo é feito automaticamente
end;

procedure TdrTriangle.Paint();

  procedure Desenha(p1, p2, p3: TPoint; _3D: Boolean);
  var C: TPoint;
  begin
    With Canvas Do
      begin
      Brush.Color := Color;
      pen.Width := 1;
      if _3D then pen.Color := clBtnHighlight else pen.Color := clGreen;
      MoveTo(p1.x, p1.y);
      LineTo(p2.x, p2.y);

      if _3D then
         begin
         pen.Width := 2;
         pen.Color := clBtnShadow;
         end
      else
         begin
         pen.Width := 1;
         pen.Color := clGreen;
         end;
      LineTo(p3.x, p3.y);

      pen.Width := 1;
      if _3D then pen.Color := clBtnHighlight else pen.Color := clGreen;
      LineTo(p1.x, p1.y);
      end;
  end;

begin
  If NotPaint or not Visible Then Exit;

  inherited;

  with Canvas, FPoints do
    begin
    Brush.Color := Color;

    if p1.x < 0 then
       Desenha(
         Point(width div 2, 0),
         Point(width, height-2),
         Point(0, height-2),
         False // 3D
         )
    else
       Desenha(p1, p2, p3, False);

    FloodFill(width div 2, height-4, clGreen, fsBorder);

    if p1.x < 0 then
       Desenha(
         Point(width div 2, 0),
         Point(width, height-2),
         Point(0, height-2),
         True // 3D
         )
    else
       Desenha(p1, p2, p3, True);
    end; {With Canvas}
end; {Paint}

procedure TdrTriangle.SetPoints(const Value: TTriangle);
begin
  FPoints := Value;
end;

{ TdrBitmap }

constructor TdrBitmap.Create(aOwner: TComponent; const BitmapName: String);
begin
  inherited Create(aOwner);
  FDrawFrame := False;
  FBitmap := TBitmap.Create();
  if BitmapName <> '' then
     FBitmap.LoadFromResourceName(HInstance, BitmapName);
end;

destructor TdrBitmap.Destroy;
begin
  FBitmap.Free;
  Inherited Destroy;
end;

procedure TdrBitmap.Paint();
var R: TRect;
begin
  If NotPaint or
     not Visible or
     (Parent = nil) Then Exit;

  inherited Paint();

  R := self.ClientRect;
  Canvas.Brush.Color := FBitmap.Canvas.Pixels[0, 0];
  Canvas.FillRect(R);
  Canvas.Draw(1, 1, FBitmap);
  if FDrawFrame then
     Frame3D(Canvas, R, clWhite, clbtnShadow, 1);
end;

{ TdrEllipseBitmap }

constructor TdrEllipseBitmap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmap := TBitmap.Create;
end;

destructor TdrEllipseBitmap.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TdrEllipseBitmap.Paint();
begin
  inherited Paint;

  If NotPaint or
     not Visible or
     (FBitmap = nil) or
     (Parent = nil) Then Exit;

  Canvas.Draw(width div 2 - FBitmap.Width div 2,
              height div 2 - FBitmap.Height div 2,
              FBitmap);
end;

procedure TdrBaseShape.Paint();
begin
  inherited;
end;

procedure TdrBaseShape.TimerTick(Sender: TObject);
begin
  inc(FBlinkCount);
  
  if FStopBlink or (FBlinkCount = FBlinkTimes) then
     begin
     FreeAndNil(FTimer);
     Canvas.Brush.Color := FSaveColor;
     end
  else
     if Canvas.Brush.Color <> clRed then
        Canvas.Brush.Color := clRed
     else
        Canvas.Brush.Color := clBlue;

  Paint();
end;

procedure TdrBaseShape.Blink(Times: Integer);
var i: Integer;
begin
  if FTimer = nil then
     begin
     FStopBlink := False;
     FBlinkCount := 0;
     FBlinkTimes := Times * 2;
     FSaveColor := Canvas.Brush.Color;
     FTimer := TTimer.Create(nil);
     FTimer.Interval := 400;
     FTimer.OnTimer := TimerTick;
     FTimer.Enabled := True;
     end;
end;

procedure TdrBaseShape.StopBlink();
begin
  FStopBlink := True;
end;

destructor TdrBaseShape.Destroy;
begin
  if FTimer <> nil then FTimer.Free;
  pointer(FObject) := nil;
  inherited Destroy();
end;

{ TdrFlag }

constructor TdrFlag.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clBLUE;
  FTransparent := true;
  FBackgroundColor := clBtnFace;
end;

procedure TdrFlag.Paint();
var
  X, Y, W, H: Integer;
  r: TRect;
begin
  If NotPaint or
     not Visible or
     (Parent = nil) Then Exit;

  inherited Paint();

  with Canvas do
    begin
    X := Pen.Width div 2;
    Y := X;
    W := Width - Pen.Width + 1;
    H := Height - Pen.Width + 1;

    r := Rect(X, Y, X + W, Y + H);

    if F3D then
       Frame3D(Canvas, r, clBtnHighlight, clBtnShadow, 2);

    if not FTransparent then
       begin
       Brush.Color := FbackgroundColor;
       fillRect(r);
       end;

    Pen.Color := clBlack;
    MoveTo(X + 3, Y + H - 3);
    LineTo(X + 3, 3);
    LineTo(X + W - 3, Y + (H div 4));
    LineTo(X + 3, Y + (H div 2) + 2);
    if Color = clBLACK then Brush.Color := Color + 01 else Brush.Color := Color;
    FloodFill(X + 4, Y + (H div 4), clBLACK, fsBORDER);
    end; {With Canvas}
end; {Paint}

end.
