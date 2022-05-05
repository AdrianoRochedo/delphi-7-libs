unit Rochedo.PanZoom;

{
 This unit contains the TPanZoom panel and its supporting classes.

 Copyright (c) 1997-1999 Dave Shapiro, Professional Software, Inc.
 Use and modify freely.

 Updated by Adriano Rochedo Conceicao in 2004.
}

interface

uses
  Windows, Types, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  PPoint = ^TPoint;
  PBooleanPoint = ^TBooleanPoint;
  TBooleanPoint = record
    X, Y: Boolean;
  end;
  PDoublePoint = ^TDoublePoint;
  TDoublePoint = record
    X, Y: Double;
  end;

function MakePoint(const X, Y: Integer): TPoint;
function MakeBooleanPoint(const X, Y: Boolean): TBooleanPoint;
function MakeDoublePoint(const X, Y: Double): TDoublePoint;

const
  DefBandWidth = 1;

type
  TRubberBandStopEvent = procedure(Sender: TObject; ARect: TRect) of object;

  TPsRubberBand = class(TComponent)
  private
  protected
    FCanvas: TCanvas;
    FProportional: Boolean;
    FPropWidth: Integer;
    FPropHeight: Integer;
    FOnStop: TRubberBandStopEvent;
    FOnCancel: TNotifyEvent;
    FHasStarted: Boolean;
    FBandWidth: Integer;
    Brush, OldBrush: TBrush;
    Band: TRect;
    procedure Paint;
    procedure UpdateBand(const X, Y: Integer);
    function GetCorrectBand: TRect;
    procedure SetBandWidth(const NewBandWidth: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start(const X, Y: Integer);
    procedure Move(const X, Y: Integer);
    procedure Stop(const X, Y: Integer);
    procedure Cancel;
    procedure SetProportional(const APropWidth, APropHeight: Integer);
    procedure SetNonProportional;
    property Canvas: TCanvas read FCanvas write FCanvas;
    property Proportional: Boolean read FProportional;
    property HasStarted: Boolean read FHasStarted;
  published
    property OnStop: TRubberBandStopEvent read FOnStop write FOnStop;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property BandWidth: Integer read FBandWidth write SetBandWidth
                                default DefBandWidth;
  end;

type
  TScaleSystem = class(TPersistent)
  private
    { Given. }
    FDeviceP1: TPoint;    { Absolute device value for first scale point. }
    FSkewP: TPoint;       { Absolute device value for skew point. }
    FDeviceP2Prime: TPoint; { Absolute device value for second scale point. }
    FActualP1: TDoublePoint; { Real-world value of DeviceP1, as given by user. }
    FActualP2: TDoublePoint; { Real-world value of DeviceP2Prime, as given by user. }
    FIsLogarithmicP: TBooleanPoint;  { Whether each axis is logarithmic. }
    { Derived. }
    FCosTheta: Double;  { Cosine of skew angle. }
    FSinTheta: Double;  { Sine of skew angle. }
    FDeviceP2: TPoint; { Absolute device value for second scale point, }
                              { de-rotated by skew angle. }
    FDeltaDeviceP: TDoublePoint;  { DeviceP2 - DeviceP1. }
    FUnitsPerPixelP: TDoublePoint; { Units per device pixel. }
    FXIsBackward: Boolean; { Whether or not the X axis goes more negative to the right. }
    FLogRatioP: TDoublePoint; { Pre-calculated for doing log axes quickly. }
  protected
  public
    WindowOriginP: TPoint;       { Zooming stuff. }
    WindowExtentP: TPoint;       {       |        }
    ViewportOriginP: TPoint;     {       |        }
    ViewportExtentP: TPoint;     {       |        }
    constructor Create;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure OneToOne;
    procedure Define(const ADeviceP1, ASkewP, ADeviceP2Prime: TPoint;
                     const AActualP1, AActualP2: TDoublePoint;
                     const AIsLogarithmicP: TBooleanPoint);
    function DeviceToAbsolute(const ADeviceP: TPoint): TPoint;
    function AbsoluteToDevice(const AAbsoluteP: TPoint): TPoint;
    function DeviceToMaster(const ADeviceP: TPoint): TPoint;
    function MasterToDevice(const AMasterP: TPoint): TPoint;
    function DeviceToActual(const ADeviceP: TPoint): TDoublePoint;
    function ActualToDevice(const AActualP: TDoublePoint): TPoint;
    function ActualToMaster(const AActualP: TDoublePoint): TPoint;
    function Transform(const P: TPoint;
                       const Dest: TScaleSystem): TPoint;

    property DeviceP1: TPoint read FDeviceP1;
    property SkewP: TPoint read FSkewP;
    property DeviceP2Prime: TPoint read FDeviceP2Prime;
    property ActualP1: TDoublePoint read FActualP1;
    property ActualP2: TDoublePoint read FActualP2;
    property IsLogarithmicP: TBooleanPoint read FIsLogarithmicP;
  end;

const
  DefSizeHandleWidth = 6;
  DefMovable = True;
  DefSizable = True;

type
  TSizeHandleWidth = 0..MaxInt;

  TResizablePanel = class(TPanel)
  private
  protected
    FMovable: Boolean;
    FSizable: Boolean;
    FSizeHandleWidth: TSizeHandleWidth;
    procedure WMNCHitTest(var Mess: TWMNCHitTest); message WM_NCHITTEST;
    function GetTotalBorderWidth: Integer; virtual;
    function GetUsableWidth: Integer; virtual;
    function GetUsableHeight: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Movable: Boolean read FMovable write FMovable default DefMovable;
    property Sizable: Boolean read FSizable write FSizable default DefSizable;
    property SizeHandleWidth: TSizeHandleWidth read FSizeHandleWidth
                                               write FSizeHandleWidth
                                               default DefSizeHandleWidth;
    property TotalBorderWidth: Integer read GetTotalBorderWidth;
    property UsableWidth: Integer read GetUsableWidth;
    property UsableHeight: Integer read GetUsableHeight;
  end;

type
  TMouseEnterPaintBox = class(TPaintBox)
  protected
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  published
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter
                                        write FOnMouseEnter;
                                        
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave
                                        write FOnMouseLeave;

    property OnDblClick;
  end;

const
  DefSmallChange = 2;
  DefLargeChange = 10;
  DefPaintBoxCursor = crCross;
  DefZoomCursor = crCross;
  DefUseFastPainting = False;
  DefUseSpanningCursor = False;

type
  TPageSizeScrollBar = TScrollBar;

  TScalePoint = record
    DeviceP: TPoint;
    ActualP: TDoublePoint;
  end;

  TDefinableScalePoint = record
    DeviceP: TPoint;
    ActualP: TDoublePoint;
    Defined: Boolean;
  end;

  TPanZoomMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
                                     DeviceX, DeviceY: Integer;
                                     ActualX, ActualY: Double) of object;

  TPanZoomMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; DeviceX, DeviceY: Integer;
                          ActualX, ActualY: Double) of object;

  TBitmapPaintEvent = procedure(Sender: TObject; BitmapCanvas: TCanvas) of object;

  TPanZoomPanel = class(TResizablePanel)
  private
    ZoomCompleted: Boolean;
  protected
    OriginalScaleSystem, CurrentScaleSystem: TScaleSystem;
    PrevX: Integer;
    PrevY: Integer;
    OldMovable: Boolean;
    OldCursor: TCursor;
    FirstPoint: Boolean;
    FScaleDefined: Boolean;
    FBitmap: TBitmap;
    FOriginalBitmap: TBitmap;
    FPaintBox: TMouseEnterPaintBox;
    FRubberBand: TPsRubberBand;
    FHorzScrollBar, FVertScrollBar: TPageSizeScrollBar;
    FZoomingIn: Boolean;
    FProportionalZoom: Boolean;
    FZoomCursor: TCursor;
    FPainting: Boolean;
    PenDown: Boolean;
    FUseFastPainting: Boolean;
    FFirstScalePoint: TDefinableScalePoint;
    FSkewPoint: TDefinableScalePoint;
    FSecondScalePoint: TDefinableScalePoint;
    FShowScalePoints: Boolean;
    FUseSpanningCursor: Boolean;
    FUseSpanningCursorOverridden: Boolean;
    FOnResize: TNotifyEvent;
    FOnPaintBoxMouseDown: TPanZoomMouseEvent;
    FOnPaintBoxMouseMove: TPanZoomMouseMoveEvent;
    FOnPaintBoxMouseUp: TPanZoomMouseEvent;
    FOnPaintBoxMouseEnter: TNotifyEvent;
    FOnPaintBoxMouseLeave: TNotifyEvent;
    FOnPaintBoxClick: TNotifyEvent;
    FOnPaintBoxDblClick: TNotifyEvent;
    FOnPaintBoxPaint: TNotifyEvent;
    FOnZoomInBegin: TNotifyEvent;
    FOnZoomInEnd: TNotifyEvent;
    FOnZoomInCancel: TNotifyEvent;
    FOnZoomOut: TNotifyEvent;
    FOnZoomIn: TNotifyEvent;
    FOnPaintBegin: TNotifyEvent;
    FOnPaintEnd: TNotifyEvent;
    FOnScaleDefined: TNotifyEvent;
    FOnScaleUndefined: TNotifyEvent;
    FOnBitmapPaint: TBitmapPaintEvent;

    procedure ClearPrevCoords;
    procedure OverrideUseSpanningCursor;
    procedure UnOverrideUseSpanningCursor;
    procedure PaintPrevSpanningCursor;
    procedure PaintCurrSpanningCursor;
    procedure PaintSpanningCursor(const X, Y: Integer);
    procedure ReconfigureCurrentSystem;
    procedure SetupScrollBars;
    function PaintBitmap: Boolean;
    procedure PaintBoxPaint(Sender: TObject); virtual;

    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
                                X, Y: Integer);

    procedure RubberBandStop(Sender: TObject; ARect: TRect);

    procedure HorzScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
                                  var ScrollPos: Integer);

    procedure VertScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
                                  var ScrollPos: Integer);

    procedure DoResize;
    procedure SetBitmap(const NewBitmap: TBitmap);
    procedure Paint; override;
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
                              Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseEnter(Sender: TObject);
    procedure PaintBoxMouseLeave(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure PaintBoxDblClick(Sender: TObject);
    procedure SetZoomingIn(const NewZoomingIn: Boolean);
    procedure SetProportionalZoom(const NewProportionalZoom: Boolean);
    function SetPaintBoxTransformation: Boolean;
    procedure UnsetPaintBoxTransformation;
    procedure SetPainting(const NewPainting: Boolean);
    procedure CreateAndSetPaintingCursor;
    function GetPenWidth: Integer;
    procedure SetPenWidth(const NewPenWidth: Integer);
    function GetRubberBandWidth: Integer;
    procedure SetRubberBandWidth(const NewRubberBandWidth: Integer);
    procedure Resize; override;
    function GetPaintBoxCursor: TCursor;
    procedure SetPaintBoxCursor(const NewPaintBoxCursor: TCursor);
    function GetPaintBoxCanvas: TCanvas;
    procedure SetUseFastPainting(const NewUseFastPainting: Boolean);
    procedure ConvertIfFastPainting;
    procedure Loaded; override;
    procedure SetSmallChange(const NewSmallChange: Integer);
    procedure SetLargeChange(const NewLargeChange: Integer);
    function GetSmallChange: Integer;
    function GetLargeChange: Integer;
    procedure SetShowScalePoints(const NewShowScalePoints: Boolean);
    procedure PaintScalePoints;
    procedure DoPaintScalePoints;
    procedure SetUseSpanningCursor(const NewUseSpanningCursor: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearScalePoints();
    procedure ZoomOut(const Factor: Double);
    procedure ZoomIn(const Factor: Double);
    procedure OneToOne();
    procedure Fit();
    procedure SizeToImage();
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure DefineScale(const XIsLogarithmic, YIsLogarithmic: Boolean);
    procedure DefineScaleNoSkew(const XIsLogarithmic, YIsLogarithmic: Boolean);
    procedure UndefineScale();
    procedure ClearBitmap();
    procedure SetFirstScalePoint(const NewFirstDevice: TPoint; const NewFirstActual: TDoublePoint);
    procedure UnsetFirstScalePoint();
    procedure SetSkewPoint(const NewSkewPoint: TPoint);
    procedure UnsetSkewPoint();
    procedure SetSecondScalePoint(const NewSecondDevice: TPoint; const NewSecondActual: TDoublePoint);
    procedure UnsetSecondScalePoint();

    function ActualToDevice(const ActualP: TDoublePoint): TPoint;
    function DeviceToActual(const DeviceP: TPoint): TDoublePoint;
    function MaxDeviceP(): TPoint;

    property Bitmap                  : TBitmap       read FBitmap                   write SetBitmap;
    property PaintBoxCanvas          : TCanvas       read GetPaintBoxCanvas;
    property Painting                : Boolean       read FPainting                 write SetPainting;
    property PenWidth                : Integer       read GetPenWidth               write SetPenWidth;
    property FirstScalePointDevice   : TPoint        read FFirstScalePoint.DeviceP;
    property FirstScalePointActual   : TDoublePoint  read FFirstScalePoint.ActualP;
    property FirstScalePointDefined  : Boolean       read FFirstScalePoint.Defined;
    property SecondScalePointActual  : TDoublePoint  read FSecondScalePoint.ActualP;
    property SecondScalePointDefined : Boolean       read FSecondScalePoint.Defined;
    property SecondScalePointDevice  : TPoint        read FSecondScalePoint.DeviceP;
    property ScaleDefined            : Boolean       read FScaleDefined;
    property SkewPointDefined        : Boolean       read FSkewPoint.Defined;
    property SkewPointDevice         : TPoint        read FSkewPoint.DeviceP;
    property ZoomingIn               : Boolean       read FZoomingIn                write SetZoomingIn;
    property ScaleSystem             : TScaleSystem  read CurrentScaleSystem;
  published
    property LargeChange       : Integer read GetLargeChange     write SetLargeChange       default DefLargeChange;
    property PaintBoxCursor    : TCursor read GetPaintBoxCursor  write SetPaintBoxCursor    default DefPaintBoxCursor;
    property ProportionalZoom  : Boolean read FProportionalZoom  write SetProportionalZoom;
    property RubberBandWidth   : Integer read GetRubberBandWidth write SetRubberBandWidth;
    property ShowScalePoints   : Boolean read FShowScalePoints   write SetShowScalePoints;
    property SmallChange       : Integer read GetSmallChange     write SetSmallChange       default DefSmallChange;
    property UseFastPainting   : Boolean read FUseFastPainting   write SetUseFastPainting   default DefUseFastPainting;
    property UseSpanningCursor : Boolean read FUseSpanningCursor write SetUseSpanningCursor default DefUseSpanningCursor;
    property ZoomCursor        : TCursor read FZoomCursor        write FZoomCursor          default DefZoomCursor;

    // Events ...

    property OnClick          : TNotifyEvent           read FOnPaintBoxClick      write FOnPaintBoxClick;
    property OnDblClick       : TNotifyEvent           read FOnPaintBoxDblClick   write FOnPaintBoxDblClick;
    property OnMouseDown      : TPanZoomMouseEvent     read FOnPaintBoxMouseDown  write FOnPaintBoxMouseDown;
    property OnMouseEnter     : TNotifyEvent           read FOnPaintBoxMouseEnter write FOnPaintBoxMouseEnter;
    property OnMouseLeave     : TNotifyEvent           read FOnPaintBoxMouseLeave write FOnPaintBoxMouseLeave;
    property OnMouseMove      : TPanZoomMouseMoveEvent read FOnPaintBoxMouseMove  write FOnPaintBoxMouseMove;
    property OnMouseUp        : TPanZoomMouseEvent     read FOnPaintBoxMouseUp    write FOnPaintBoxMouseUp;
    property OnPaintBegin     : TNotifyEvent           read FOnPaintBegin         write FOnPaintBegin;
    property OnPaintBoxPaint  : TNotifyEvent           read FOnPaintBoxPaint      write FOnPaintBoxPaint;
    property OnBitmapPaint    : TBitmapPaintEvent      read FOnBitmapPaint        write FOnBitmapPaint;
    property OnPaintEnd       : TNotifyEvent           read FOnPaintEnd           write FOnPaintEnd;
    property OnScaleDefined   : TNotifyEvent           read FOnScaleDefined       write FOnScaleDefined;
    property OnScaleUndefined : TNotifyEvent           read FOnScaleUndefined     write FOnScaleUndefined;
    property OnZoomInBegin    : TNotifyEvent           read FOnZoomInBegin        write FOnZoomInBegin;
    property OnZoomInCancel   : TNotifyEvent           read FOnZoomInCancel       write FOnZoomInCancel;
    property OnZoomInEnd      : TNotifyEvent           read FOnZoomInEnd          write FOnZoomInEnd;
    property OnZoomOut        : TNotifyEvent           read FOnZoomOut            write FOnZoomOut;
    property OnZoomIn         : TNotifyEvent           read FOnZoomIn             write FOnZoomIn;
  end;

procedure Register;

implementation

const
  crPainting = 5;
  UndefinedPoint: TPoint = (X: 0; Y: 0);
  UndefinedDoublePoint: TDoublePoint = (X: 0.0; Y: 0.0);

var
  (*$IFDEF Win32*)
    OsVersionInfo: TOsVersionInfo;
  (*$ENDIF*)
  RunningNT: Boolean;
  (*$IFDEF PreDelphi3*)
    StupidHandle: HDC;
  (*$ENDIF*)

procedure Register;
begin
  RegisterComponents('AD-2', [TPsRubberBand, TResizablePanel, TPanZoomPanel]);
end;

function MakePoint(const X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function MakeBooleanPoint(const X, Y: Boolean): TBooleanPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function MakeDoublePoint(const X, Y: Double): TDoublePoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure SetPen(const ACanvas: TCanvas; const AColor: TColor;
                 const AMode: TPenMode; const AStyle: TPenStyle;
                 const AWidth: Integer);
begin
  with ACanvas.Pen do begin
    Color := AColor;
    Mode := AMode;
    Style := AStyle;
    Width := AWidth;
  end;
end;

procedure SetBrush(const ACanvas: TCanvas; const AColor: TColor;
                   const AStyle: TBrushStyle);
begin
  with ACanvas.Brush do begin
    Color := AColor;
    Style := AStyle;
  end;
end;

procedure SetPenAndBrush(const ACanvas: TCanvas; const APenColor: TColor;
                         const AMode: TPenMode; const APenStyle: TPenStyle;
                         const AWidth: Integer; const ABrushColor: TColor;
                         const ABrushStyle: TBrushStyle);
begin
  SetPen(ACanvas, APenColor, AMode, APenStyle, AWidth);
  SetBrush(ACanvas, ABrushColor, ABrushStyle);
end;


{ -----------------------------------------------------------------------------}


constructor TScaleSystem.Create;
begin
  inherited;
  OneToOne;
end;

destructor TScaleSystem.Destroy;
begin
  inherited;
end;

procedure TScaleSystem.AssignTo(Dest: TPersistent);
begin
  if Dest is TScaleSystem then with TScaleSystem(Dest) do begin
    FDeviceP1 := Self.FDeviceP1;
    FSkewP := Self.FSkewP;
    FDeviceP2Prime := Self.FDeviceP2Prime;
    FActualP1 := Self.FActualP1;
    FActualP2 := Self.FActualP2;
    FIsLogarithmicP := Self.FIsLogarithmicP;
    FCosTheta := Self.FCosTheta;
    FSinTheta := Self.FSinTheta;
    FDeviceP2 := Self.FDeviceP2;
    FDeltaDeviceP := Self.FDeltaDeviceP;
    FUnitsPerPixelP := Self.FUnitsPerPixelP;
    FXIsBackward := Self.FXIsBackward;
    FLogRatioP := Self.FLogRatioP;
    WindowOriginP := Self.WindowOriginP;
    WindowExtentP := Self.WindowExtentP;
    ViewportOriginP := Self.ViewportOriginP;
    ViewportExtentP := Self.ViewportExtentP;
  end
  else begin
    inherited;
  end;
end;

procedure TScaleSystem.OneToOne;
begin
  WindowOriginP.X := 0;
  WindowOriginP.Y := 0;
  WindowExtentP.X := 1;
  WindowExtentP.Y := 1;
  ViewportOriginP.X := 0;
  ViewportOriginP.Y := 0;
  ViewportExtentP.X := 1;
  ViewportExtentP.Y := 1;
end;

procedure TScaleSystem.Define(const ADeviceP1, ASkewP, ADeviceP2Prime:
                                    TPoint;
                              const AActualP1, AActualP2: TDoublePoint;
                              const AIsLogarithmicP: TBooleanPoint);
var
  Theta: Double;
begin
  FDeviceP1 := ADeviceP1;
  FSkewP := ASkewP;
  FDeviceP2Prime := ADeviceP2Prime;
  FActualP1 := AActualP1;
  FActualP2 := AActualP2;
  FIsLogarithmicP := AIsLogarithmicP;
  Theta := ArcTan((FSkewP.Y - FDeviceP1.Y) / (FSkewP.X - FDeviceP1.X));
  FCosTheta := Cos(Theta);
  FSinTheta := Sin(Theta);
  FDeviceP2.X := Round( FDeviceP1.X + (FDeviceP2Prime.X - FDeviceP1.X) *
                 FCosTheta + (FDeviceP2Prime.Y - FDeviceP1.Y) * FSinTheta );
  FDeviceP2.Y := Round( FDeviceP1.Y + (FDeviceP2Prime.Y - FDeviceP1.Y) *
                 FCosTheta - (FDeviceP2Prime.X - FDeviceP1.X) * FSinTheta );
  FDeltaDeviceP.X := FDeviceP2.X - FDeviceP1.X;
  FDeltaDeviceP.Y := FDeviceP2.Y - FDeviceP1.Y;
  FUnitsPerPixelP.X := (FActualP2.X - FActualP1.X) / (FDeviceP2.X - FDeviceP1.X);
  FUnitsPerPixelP.Y := (FActualP2.Y - FActualP1.Y) / (FDeviceP2.Y - FDeviceP1.Y);
  FXIsBackward := FUnitsPerPixelP.X < 0;
  if FIsLogarithmicP.X then FLogRatioP.X := Ln(FActualP2.X / FActualP1.X);
  if FIsLogarithmicP.Y then FLogRatioP.Y := Ln(FActualP2.Y / FActualP1.Y);
end;

function TScaleSystem.DeviceToAbsolute(const ADeviceP: TPoint): TPoint;
begin
  Result := MakePoint(ADeviceP.X - ViewportOriginP.X +
                             MulDiv(WindowOriginP.X, ViewportExtentP.X,
                                    WindowExtentP.X),
                             ADeviceP.Y - ViewportOriginP.Y +
                             MulDiv(WindowOriginP.Y, ViewportExtentP.Y,
                                    WindowExtentP.Y)
                            );
end;

function TScaleSystem.AbsoluteToDevice(const AAbsoluteP: TPoint): TPoint;
begin
  Result := MakePoint(ViewportOriginP.X + AAbsoluteP.X -
                             MulDiv(WindowOriginP.X, ViewportExtentP.X,
                                    WindowExtentP.X),
                             ViewportOriginP.Y + AAbsoluteP.Y -
                             MulDiv(WindowOriginP.Y, ViewportExtentP.Y,
                                    WindowExtentP.Y));
end;

function TScaleSystem.DeviceToMaster(const ADeviceP: TPoint): TPoint;
begin
  Result := MakePoint(WindowOriginP.X +
                             MulDiv(ADeviceP.X - ViewportOriginP.X,
                                    WindowExtentP.X, ViewportExtentP.X),
                             WindowOriginP.Y +
                             MulDiv(ADeviceP.Y - ViewportOriginP.Y,
                                    WindowExtentP.Y, ViewportExtentP.Y));
end;

function TScaleSystem.MasterToDevice(const AMasterP: TPoint): TPoint;
begin
  Result := MakePoint(ViewportOriginP.X +
                             MulDiv(AMasterP.X - WindowOriginP.X,
                                    ViewportExtentP.X, WindowExtentP.X),
                             ViewportOriginP.Y +
                             MulDiv(AMasterP.Y - WindowOriginP.Y,
                                    ViewportExtentP.Y, WindowExtentP.X));
end;

function TScaleSystem.DeviceToActual(const ADeviceP: TPoint): TDoublePoint;
var
  AbsoluteP: TPoint;
  PDiffPixels, PPrimePixels: TDoublePoint;
begin
  AbsoluteP := DeviceToAbsolute(ADeviceP);
  PDiffPixels.X := AbsoluteP.X - FDeviceP1.X;
  PDiffPixels.Y := AbsoluteP.Y - FDeviceP1.Y;
  PPrimePixels.X := PDiffPixels.X * FCosTheta + PDiffPixels.Y * FSinTheta;
  PPrimePixels.Y := PDiffPixels.Y * FCosTheta - PDiffPixels.X * FSinTheta;
  if FIsLogarithmicP.X then
    Result.X := FActualP1.X * Exp(PPrimePixels.X / FDeltaDeviceP.X * FLogRatioP.X)
  else
    Result.X := FActualP1.X + FUnitsPerPixelP.X * PPrimePixels.X;
  if FIsLogarithmicP.Y then
    Result.Y := FActualP1.Y * Exp(PPrimePixels.Y / FDeltaDeviceP.Y * FLogRatioP.Y)
  else
    Result.Y := FActualP1.Y + FUnitsPerPixelP.Y * PPrimePixels.Y;
end;

function TScaleSystem.ActualToDevice(const AActualP: TDoublePoint): TPoint;
var
  PPrimePixels: TDoublePoint;
  AbsoluteP: TPoint;
begin
  if FIsLogarithmicP.X then
    PPrimePixels.X := Ln(AActualP.X / FActualP1.X) / FLogRatioP.X * FDeltaDeviceP.X
  else
    PPrimePixels.X := (AActualP.X - FActualP1.X) / FUnitsPerPixelP.X;
  if FIsLogarithmicP.Y then
    PPrimePixels.Y := Ln(AActualP.Y / FActualP1.Y) / FLogRatioP.Y * FDeltaDeviceP.Y
  else
    PPrimePixels.Y := (AActualP.Y - FActualP1.Y) / FUnitsPerPixelP.Y;
  AbsoluteP.X := Round(PPrimePixels.X * FCosTheta - PPrimePixels.Y * FSinTheta) +
                 FDeviceP1.X;
  AbsoluteP.Y := Round(PPrimePixels.Y * FCosTheta + PPrimePixels.X * FSinTheta) +
                 FDeviceP1.Y;
  Result := AbsoluteToDevice(AbsoluteP);
end;

function TScaleSystem.ActualToMaster(const AActualP: TDoublePoint): TPoint;
begin
  Result := DeviceToMaster(ActualToDevice(AActualP));
end;

function TScaleSystem.Transform(const P: TPoint;
                                const Dest: TScaleSystem): TPoint;
begin
  Result := MakePoint(MulDiv(P.X,
                                    MulDiv(Self.WindowExtentP.X,
                                           Dest.ViewportExtentP.X,
                                           Self.ViewportExtentP.X),
                                    Dest.WindowExtentP.X),
                             MulDiv(P.Y,
                                    MulDiv(Self.WindowExtentP.Y,
                                           Dest.ViewportExtentP.Y,
                                           Self.ViewportExtentP.Y),
                                    Dest.WindowExtentP.Y));
end;


{ -----------------------------------------------------------------------------}


procedure TMouseEnterPaintBox.CMMouseEnter(var Msg: TMessage);
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TMouseEnterPaintBox.CMMouseLeave(var Msg: TMessage);
begin
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;


{ -----------------------------------------------------------------------------}


constructor TPanZoomPanel.Create(AOwner: TComponent);
begin
  inherited;
  FShowScalePoints := False;
  CurrentScaleSystem := TScaleSystem.Create;
  OriginalScaleSystem := TScaleSystem.Create;
  ClearScalePoints;
  FZoomCursor := DefZoomCursor;
  FUseFastPainting := DefUseFastPainting;
  FUseSpanningCursor := DefUseSpanningCursor;
  FUseSpanningCursorOverridden := False;
  FHorzScrollBar := TPageSizeScrollBar.Create(Self);
  with FHorzScrollBar do begin
    Parent := Self;
    Kind := sbHorizontal;
    OnScroll := HorzScrollBarScroll;
    TabStop := False;
  end;
  FVertScrollBar := TPageSizeScrollBar.Create(Self);
  with FVertScrollBar do begin
    Parent := Self;
    Kind := sbVertical;
    OnScroll := VertScrollBarScroll;
    TabStop := False;
  end;
  SetSmallChange(DefSmallChange);
  SetLargeChange(DefLargeChange);
  FPaintBox := TMouseEnterPaintBox.Create(Self);
  with FPaintBox do begin
    Parent := Self;
    ControlStyle := ControlStyle + [csOpaque];
    Align := alClient;
    OnPaint := PaintBoxPaint;
    OnMouseMove := PaintBoxMouseMove;
    OnMouseDown := PaintBoxMouseDown;
    OnMouseUp := PaintBoxMouseUp;
    OnMouseEnter := PaintBoxMouseEnter;
    OnMouseLeave := PaintBoxMouseLeave;
    OnClick := PaintBoxClick;
    OnDblClick := PaintBoxDblClick;
  end;
  SetPaintBoxCursor(DefPaintBoxCursor);
  ClearPrevCoords;
  FRubberBand := TPsRubberBand.Create(Self);
  with FRubberBand do begin
    Canvas := FPaintBox.Canvas;
    OnStop := Self.RubberBandStop;
  end;
  ZoomCompleted := False;
end;

destructor TPanZoomPanel.Destroy;
begin
  ClearBitmap;
  FRubberBand.Free;
  FPaintBox.Free;
  FVertScrollBar.Free;
  FHorzScrollBar.Free;
  OriginalScaleSystem.Free;
  CurrentScaleSystem.Free;
  inherited;
end;

procedure TPanZoomPanel.Loaded;
begin
  inherited;
end;

procedure TPanZoomPanel.SetSmallChange(const NewSmallChange: Integer);
begin
  FHorzScrollBar.SmallChange := NewSmallChange;
  FVertScrollBar.SmallChange := NewSmallChange;
end;

procedure TPanZoomPanel.SetLargeChange(const NewLargeChange: Integer);
begin
  FHorzScrollBar.LargeChange := NewLargeChange;
  FVertScrollBar.LargeChange := NewLargeChange;
end;

function TPanZoomPanel.GetSmallChange: Integer;
begin
  Result := FHorzScrollBar.SmallChange;
end;

function TPanZoomPanel.GetLargeChange: Integer;
begin
  Result := FHorzScrollBar.LargeChange;
end;

procedure TPanZoomPanel.SetFirstScalePoint(const NewFirstDevice: TPoint;
                                           const NewFirstActual: TDoublePoint);
begin
  FFirstScalePoint.DeviceP := CurrentScaleSystem.DeviceToAbsolute(NewFirstDevice);
  FFirstScalePoint.ActualP := NewFirstActual;
  FFirstScalePoint.Defined := True;
  DoPaintScalePoints;
end;

procedure TPanZoomPanel.UnsetFirstScalePoint;
begin
  FFirstScalePoint.Defined := False;
  FPaintBox.Invalidate;
end;

procedure TPanZoomPanel.SetSkewPoint(const NewSkewPoint: TPoint);
begin
  FSkewPoint.DeviceP := CurrentScaleSystem.DeviceToAbsolute(NewSkewPoint);
  FSkewPoint.Defined := True;
  DoPaintScalePoints;
end;

procedure TPanZoomPanel.UnsetSkewPoint;
begin
  FSkewPoint.Defined := False;
  FPaintBox.Invalidate;
end;

procedure TPanZoomPanel.SetSecondScalePoint(const NewSecondDevice: TPoint;
                                            const NewSecondActual: TDoublePoint);
begin
  FSecondScalePoint.DeviceP := CurrentScaleSystem.DeviceToAbsolute(NewSecondDevice);
  FSecondScalePoint.ActualP := NewSecondActual;
  FSecondScalePoint.Defined := True;
  DoPaintScalePoints;
end;

procedure TPanZoomPanel.UnsetSecondScalePoint;
begin
  FSecondScalePoint.Defined := False;
  FPaintBox.Invalidate;
end;

procedure TPanZoomPanel.PaintScalePoints;
const
  ScalePointColor = clLime;

  procedure PaintScalePoint(const P: TPoint);
  const
    ScalePointRadius = 4;
  begin
    FPaintBox.Canvas.Ellipse(P.X - ScalePointRadius, P.Y - ScalePointRadius,
                             P.X + ScalePointRadius, P.Y + ScalePointRadius);
  end;

begin
  with FPaintBox.Canvas do
    begin
    Brush.Color := ScalePointColor;
    Pen.Color := ScalePointColor;
    end;

  if FFirstScalePoint.Defined then
     PaintScalePoint(CurrentScaleSystem.AbsoluteToDevice (FFirstScalePoint.DeviceP));

  if FSkewPoint.Defined then
     PaintScalePoint(CurrentScaleSystem.AbsoluteToDevice(FSkewPoint.DeviceP));

  if FSecondScalePoint.Defined then
    PaintScalePoint(CurrentScaleSystem.AbsoluteToDevice(FSecondScalePoint.DeviceP));
end;

procedure TPanZoomPanel.DoPaintScalePoints;
begin
  if FShowScalePoints then PaintScalePoints;
end;

procedure TPanZoomPanel.SetUseSpanningCursor(const NewUseSpanningCursor:
                                                   Boolean);
begin
  if FUseSpanningCursor <> NewUseSpanningCursor then
    FUseSpanningCursor := NewUseSpanningCursor;
end;

procedure TPanZoomPanel.SetShowScalePoints(const NewShowScalePoints: Boolean);
begin
  if FShowScalePoints = NewShowScalePoints then Exit;
  FShowScalePoints := NewShowScalePoints;
  FPaintBox.Invalidate;
end;

procedure TPanZoomPanel.ClearBitmap;
begin
  FBitmap.Free;
  FBitmap := nil;
  FOriginalBitmap.Free;
  FOriginalBitmap := nil;
  FPaintBox.Hide;
  FPaintBox.Update;
  UndefineScale;
  DoResize;
  FPaintBox.Show;
  Invalidate;
end;

procedure TPanZoomPanel.SetBitmap(const NewBitmap: TBitmap);
begin
  ClearBitmap();
  FBitmap := TBitmap.Create();
  FBitmap.Assign(NewBitmap);
  FOriginalBitmap := TBitmap.Create();
  FOriginalBitmap.Assign(NewBitmap);
  ConvertIfFastPainting();
  OneToOne();
  DoResize();
end;

procedure TPanZoomPanel.Paint;
begin
  inherited;
end;

procedure TPanZoomPanel.Resize;
begin
  inherited;
  DoResize;
  if Assigned(FOnResize) then FOnResize(Self);
end;

function TPanZoomPanel.GetPaintBoxCursor: TCursor;
begin
  Result := FPaintBox.Cursor;
end;

procedure TPanZoomPanel.SetPaintBoxCursor(const NewPaintBoxCursor: TCursor);
begin
  FPaintBox.Cursor := NewPaintBoxCursor;
end;

function TPanZoomPanel.GetPaintBoxCanvas: TCanvas;
begin
  Result := FPaintBox.Canvas;
end;

procedure TPanZoomPanel.ConvertIfFastPainting;
begin
  if Assigned(FBitmap) and FUseFastPainting then begin
    (*$IFDEF PreDelphi3*)
      StupidHandle := FBitmap.Canvas.Handle;
    (*$ELSE*)
      FBitmap.PixelFormat := pfDevice;
    (*$ENDIF*)
  end;
end;

procedure TPanZoomPanel.SetUseFastPainting(const NewUseFastPainting: Boolean);
begin
  if FUseFastPainting <> NewUseFastPainting then begin
    FUseFastPainting := NewUseFastPainting;
    ConvertIfFastPainting;
  end;
end;

procedure TPanZoomPanel.ClearScalePoints();
begin
  FFirstScalePoint.Defined := False;
  FSkewPoint.Defined := False;
  FSecondScalePoint.Defined := False;
end;

procedure TPanZoomPanel.ZoomIn(const Factor: Double);
begin
  if (FZoomingIn or FPainting) or (Factor = 0) then Exit;
  with CurrentScaleSystem.WindowExtentP do
    begin
    X := Trunc(X / Factor);
    Y := Trunc(Y / Factor);
    end;
  DoResize();
  if FUseSpanningCursor then FPaintBox.Update();
  if Assigned(FOnZoomIn) then FOnZoomIn(Self);
end;

procedure TPanZoomPanel.ZoomOut(const Factor: Double);
begin
  if FZoomingIn or FPainting then Exit;
  with CurrentScaleSystem.WindowExtentP do begin
    X := Trunc(X * Factor);
    Y := Trunc(Y * Factor);
  end;
  DoResize();
  if FUseSpanningCursor then FPaintBox.Update();
  if Assigned(FOnZoomOut) then FOnZoomOut(Self);
end;

procedure TPanZoomPanel.SetZoomingIn(const NewZoomingIn: Boolean);
begin
  if FPainting or (FZoomingIn = NewZoomingIn) then Exit;
  FZoomingIn := NewZoomingIn;
  if FZoomingIn then begin
    OldMovable := Movable;
    Movable := False;
    OldCursor := FPaintBox.Cursor;
    OverrideUseSpanningCursor;
    FPaintBox.Cursor := FZoomCursor;
    if FProportionalZoom then
      FRubberBand.SetProportional(UsableWidth, UsableHeight)
    else
      FRubberBand.SetNonProportional;
    if Assigned(FOnZoomInBegin) then FOnZoomInBegin(Self);
  end
  else begin
    Movable := OldMovable;
    UnOverrideUseSpanningCursor;
    FPaintBox.Cursor := OldCursor;
    {
     The ZoomCompleted flag is only set when a zoom was done successfully,
     specifically in the OnRubberBand event.
    }
    if ZoomCompleted then begin
      if Assigned(FOnZoomInEnd) then FOnZoomInEnd(Self);
    end
    else begin
      FRubberBand.Cancel;
      if Assigned(FOnZoomInCancel) then FOnZoomInCancel(Self);
    end;
    ZoomCompleted := False;
  end;
end;

procedure TPanZoomPanel.SetProportionalZoom(const NewProportionalZoom: Boolean);
begin
  if not FZoomingIn then FProportionalZoom := NewProportionalZoom;
end;

procedure TPanZoomPanel.SetPainting(const NewPainting: Boolean);
begin
  if FZoomingIn or (FPainting = NewPainting) then Exit;
  FPainting := NewPainting;
  PenDown := False;
  if FPainting then begin
    OldMovable := Movable;
    Movable := False;
    OldCursor := FPaintBox.Cursor;
    FirstPoint := True;
    OverrideUseSpanningCursor;
    CreateAndSetPaintingCursor;
    if Assigned(FOnPaintBegin) then FOnPaintBegin(Self);
  end
  else begin
    Movable := OldMovable;
    UnOverrideUseSpanningCursor;
    FPaintBox.Cursor := OldCursor;
    if Assigned(FOnPaintEnd) then FOnPaintEnd(Self);
  end;
end;

procedure TPanZoomPanel.ClearPrevCoords;
begin
  PrevX := -1;
  PrevY := -1;
  FirstPoint := FPainting;
end;

procedure TPanZoomPanel.OverrideUseSpanningCursor;
begin
  if not FUseSpanningCursorOverridden then begin
    PaintCurrSpanningCursor;
    FUseSpanningCursorOverridden := True;
  end;
end;

procedure TPanZoomPanel.UnOverrideUseSpanningCursor;
begin
  if FUseSpanningCursorOverridden then begin
    FUseSpanningCursorOverridden := False;
    PaintCurrSpanningCursor;
  end;
end;

procedure TPanZoomPanel.PaintPrevSpanningCursor;
begin
  if (PrevX > -1) and (PrevY > -1) then PaintSpanningCursor(PrevX, PrevY);
end;

procedure TPanZoomPanel.PaintCurrSpanningCursor;
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := FPaintBox.ScreenToClient(P);
  if PtInRect(FPaintBox.ClientRect, P) then PaintSpanningCursor(P.X, P.Y);
end;

procedure TPanZoomPanel.PaintSpanningCursor(const X, Y: Integer);
var
  H: HDC;
begin
  if FUseSpanningCursor and not FUseSpanningCursorOverridden then begin
    with FPaintBox.Canvas do begin
      Brush.Style := bsSolid;
      Brush.Color := clWhite;
      H := Handle;
    end;
    Windows.DrawFocusRect(H, Rect(0, Y, FPaintBox.Width, Y));
    Windows.DrawFocusRect(H, Rect(X, 0, X, FPaintBox.Height));
  end;
end;

procedure TPanZoomPanel.ReconfigureCurrentSystem;

  function OToC(const OriginalP: TPoint): TPoint;
  begin
    Result := OriginalScaleSystem.Transform(OriginalP, CurrentScaleSystem);
  end;

begin
  if not ScaleDefined then Exit;
  CurrentScaleSystem.Define(OToC(OriginalScaleSystem.DeviceP1),
                            OToC(OriginalScaleSystem.SkewP),
                            OToC(OriginalScaleSystem.DeviceP2Prime),
                            OriginalScaleSystem.ActualP1,
                            OriginalScaleSystem.ActualP2,
                            OriginalScaleSystem.IsLogarithmicP);
end;

procedure TPanZoomPanel.LoadFromStream(Stream: TStream);
var
  SD: Boolean;
  LogP: TBooleanPoint;
  WO, WE, VO, VE: TPoint;
begin
  Stream.Read(SD, SizeOf(SD));
  if SD then begin
    {
     Viewport and window stuff from OriginalScaleSystem. Put it into
     CurrentScaleSystem for the DefineScale procedure. We'll restore the
     actual saved CurrentScaleSystem afterwards
     }
    Stream.Read(CurrentScaleSystem.WindowOriginP,
                SizeOf(CurrentScaleSystem.WindowOriginP));
    Stream.Read(CurrentScaleSystem.WindowExtentP,
                SizeOf(CurrentScaleSystem.WindowExtentP));
    Stream.Read(CurrentScaleSystem.ViewportOriginP,
                SizeOf(CurrentScaleSystem.ViewportOriginP));
    Stream.Read(CurrentScaleSystem.ViewportExtentP,
                SizeOf(CurrentScaleSystem.ViewportExtentP));
    Stream.Read(LogP, SizeOf(OriginalScaleSystem.IsLogarithmicP));
    { This is the actual saved CurrentScaleSystem. }
    Stream.Read(WO, SizeOf(CurrentScaleSystem.WindowOriginP));
    Stream.Read(WE, SizeOf(CurrentScaleSystem.WindowExtentP));
    Stream.Read(VO, SizeOf(CurrentScaleSystem.ViewportOriginP));
    Stream.Read(VE, SizeOf(CurrentScaleSystem.ViewportExtentP));
    { Scale points. }
    {
     Note that this circumvents the SetFirstScalePoint method, because
     that method calls DeviceToAbsolute on us, and we're actually reading
     in the absolute coordinates, calculated from the original DefineScale.
    }
    Stream.Read(FFirstScalePoint.DeviceP, SizeOf(FFirstScalePoint.DeviceP));
    Stream.Read(FFirstScalePoint.ActualP, SizeOf(FFirstScalePoint.ActualP));
    FFirstScalePoint.Defined := True;
    Stream.Read(FSkewPoint.DeviceP, SizeOf(FSkewPoint.DeviceP));
    FSkewPoint.Defined := True;
    Stream.Read(FSecondScalePoint.DeviceP, SizeOf(FSecondScalePoint.DeviceP));
    Stream.Read(FSecondScalePoint.ActualP, SizeOf(FSecondScalePoint.ActualP));
    FSecondScalePoint.Defined := True;
    DefineScale(LogP.X, LogP.Y);
    { Now restore the actual saved CurrentScaleSystem. }
    with CurrentScaleSystem do begin
      WindowOriginP := WO;
      WindowExtentP := WE;
      ViewportOriginP := VO;
      ViewportExtentP := VE;
    end;
  end;
  DoResize;
end;

procedure TPanZoomPanel.SaveToStream(Stream: TStream);
begin
  Stream.Write(ScaleDefined, SizeOf(ScaleDefined));
  if ScaleDefined then begin
    { Viewport and window stuff from OriginalScaleSystem. }
    Stream.Write(OriginalScaleSystem.WindowOriginP,
                 SizeOf(OriginalScaleSystem.WindowOriginP));
    Stream.Write(OriginalScaleSystem.WindowExtentP,
                 SizeOf(OriginalScaleSystem.WindowExtentP));
    Stream.Write(OriginalScaleSystem.ViewportOriginP,
                 SizeOf(OriginalScaleSystem.ViewportOriginP));
    Stream.Write(OriginalScaleSystem.ViewportExtentP,
                 SizeOf(OriginalScaleSystem.ViewportExtentP));
    Stream.Write(OriginalScaleSystem.IsLogarithmicP,
                 SizeOf(OriginalScaleSystem.IsLogarithmicP));
    { Viewport and window stuff from CurrentScaleSystem. }
    Stream.Write(CurrentScaleSystem.WindowOriginP,
                 SizeOf(CurrentScaleSystem.WindowOriginP));
    Stream.Write(CurrentScaleSystem.WindowExtentP,
                 SizeOf(CurrentScaleSystem.WindowExtentP));
    Stream.Write(CurrentScaleSystem.ViewportOriginP,
                 SizeOf(CurrentScaleSystem.ViewportOriginP));
    Stream.Write(CurrentScaleSystem.ViewportExtentP,
                 SizeOf(CurrentScaleSystem.ViewportExtentP));
    { Scale points. }
    Stream.Write(FirstScalePointDevice, SizeOf(FirstScalePointDevice));
    Stream.Write(FirstScalePointActual, SizeOf(FirstScalePointActual));
    Stream.Write(SkewPointDevice, SizeOf(SkewPointDevice));
    Stream.Write(SecondScalePointDevice, SizeOf(SecondScalePointDevice));
    Stream.Write(SecondScalePointActual, SizeOf(SecondScalePointActual));
  end;
end;

procedure TPanZoomPanel.DefineScale(const XIsLogarithmic, YIsLogarithmic:
                                          Boolean);
begin
  if not FFirstScalePoint.Defined or not FSkewPoint.Defined or
     not FSecondScalePoint.Defined then
  begin
    raise Exception.Create('Can''t define scale: Scale points not completely ' +
                           'defined.');
  end;
  with OriginalScaleSystem do begin
    WindowOriginP := CurrentScaleSystem.WindowOriginP;
    WindowExtentP := CurrentScaleSystem.WindowExtentP;
    ViewportOriginP := CurrentScaleSystem.ViewportOriginP;
    ViewportExtentP := CurrentScaleSystem.ViewportExtentP;
  end;
  OriginalScaleSystem.Define(FFirstScalePoint.DeviceP,
                             FSkewPoint.DeviceP,
                             FSecondScalePoint.DeviceP,
                             FFirstScalePoint.ActualP,
                             FSecondScalePoint.ActualP,
                             MakeBooleanPoint(XIsLogarithmic, YIsLogarithmic));
  CurrentScaleSystem.Assign(OriginalScaleSystem);
  FScaleDefined := True;
  if Assigned(FOnScaleDefined) and not (csDestroying in ComponentState) then
  begin
    FOnScaleDefined(Self);
  end;
end;

procedure TPanZoomPanel.DefineScaleNoSkew(const XIsLogarithmic,
                                                YIsLogarithmic: Boolean);
begin
  SetSkewPoint(MakePoint(FSecondScalePoint.DeviceP.X -
                                FFirstScalePoint.DeviceP.X,
                                FFirstScalePoint.DeviceP.Y));
  DefineScale(XIsLogarithmic, YIsLogarithmic);
end;

procedure TPanZoomPanel.UndefineScale;
begin
  if FScaleDefined then begin
    ClearScalePoints;
    FScaleDefined := False;
    if Assigned(FOnScaleUndefined) and not (csDestroying in ComponentState) then
      FOnScaleUndefined(Self);
  end;
end;

procedure TPanZoomPanel.OneToOne();
begin
  with CurrentScaleSystem do begin
    WindowOriginP.X := 0;
    WindowOriginP.Y := 0;
    WindowExtentP.X := FBitmap.Width;
    WindowExtentP.Y := FBitmap.Height;
    ViewportOriginP := WindowOriginP;
    ViewportExtentP := WindowExtentP;
  end;
  ReconfigureCurrentSystem;
  DoResize;
end;

procedure TPanZoomPanel.SizeToImage;
var
  Borders: Integer;
  MaxWidth, MaxHeight: Integer;
begin
  with CurrentScaleSystem do begin
    MaxWidth := MulDiv(FBitmap.Width, ViewportExtentP.X, WindowExtentP.X);
    MaxHeight := MulDiv(FBitmap.Height, ViewportExtentP.Y, WindowExtentP.Y);
  end;
  Borders := TotalBorderWidth shl 1;
  SetBounds(Left, Top, MaxWidth + Borders, MaxHeight + Borders);
end;

function TPanZoomPanel.SetPaintBoxTransformation(): Boolean;
begin
  with FPaintBox, CurrentScaleSystem do begin
    SetMapMode(Canvas.Handle, MM_ANISOTROPIC);
    Result := SetViewportOrgEx(Canvas.Handle, ViewportOriginP.X,
                               ViewportOriginP.Y, nil) and
              SetViewportExtEx(Canvas.Handle, ViewportExtentP.X,
                               ViewportExtentP.Y, nil) and
              SetWindowOrgEx(Canvas.Handle, WindowOriginP.X,
                             WindowOriginP.Y, nil) and
              SetWindowExtEx(Canvas.Handle, WindowExtentP.X,
                             WindowExtentP.Y, nil);
  end;
end;

procedure TPanZoomPanel.UnsetPaintBoxTransformation;
begin
  with FPaintBox.Canvas do begin
    SetViewportOrgEx(Handle, 0, 0, nil);
    SetViewportExtEx(Handle, 1, 1, nil);
    SetWindowOrgEx(Handle, 0, 0, nil);
    SetWindowExtEx(Handle, 1, 1, nil);
  end;
end;

function TPanZoomPanel.PaintBitmap(): Boolean;
begin
  with FPaintBox, CurrentScaleSystem do
    begin
    Result := SetPaintBoxTransformation();
    if Result then
       begin
       if Assigned(FOnBitmapPaint) then
          begin
          FBitmap.Assign(FOriginalBitmap);
          FOnBitmapPaint(self, FBitmap.Canvas);
          end;

       Canvas.Draw(MulDiv(Left, WindowExtentP.X, ViewportExtentP.X),
                   MulDiv(Top, WindowExtentP.Y, ViewportExtentP.Y),
                   FBitmap);

       UnsetPaintBoxTransformation();
       end;
    end;
end;

procedure TPanZoomPanel.PaintBoxPaint(Sender: TObject);
begin
  if Assigned(FBitmap) then
     if not PaintBitmap() then OneToOne();

  DoPaintScalePoints();
  if Assigned(FOnPaintBoxPaint) then FOnPaintBoxPaint(Self);
end;

procedure TPanZoomPanel.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
                                          Shift: TShiftState; X, Y: Integer);
var
  ActualP: TDoublePoint;
begin
  if FZoomingIn and not FRubberBand.HasStarted then FRubberBand.Start(X, Y);
  if Assigned(FOnPaintBoxMouseDown) then begin
    ActualP := DeviceToActual(MakePoint(X, Y));
    FOnPaintBoxMouseDown(Self, Button, Shift, X, Y, ActualP.X, ActualP.Y);
  end;
end;

procedure TPanZoomPanel.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
                                        Shift: TShiftState; X, Y: Integer);
var
  ActualP: TDoublePoint;
begin
  if FZoomingIn and FRubberBand.HasStarted then FRubberBand.Stop(X, Y);
  if Assigned(FOnPaintBoxMouseUp) then begin
    ActualP := DeviceToActual(MakePoint(X, Y));
    FOnPaintBoxMouseUp(Self, Button, Shift, X, Y, ActualP.X, ActualP.Y);
  end;
end;

procedure TPanZoomPanel.PaintBoxMouseEnter(Sender: TObject);
begin
  if Assigned(FOnPaintBoxMouseEnter) then FOnPaintBoxMouseEnter(Self);
end;

procedure TPanZoomPanel.PaintBoxMouseLeave(Sender: TObject);
begin
  if FUseSpanningCursor then PaintPrevSpanningCursor;
  ClearPrevCoords;
  if Assigned(FOnPaintBoxMouseLeave) then FOnPaintBoxMouseLeave(Self);
end;

procedure TPanZoomPanel.PaintBoxClick(Sender: TObject);
begin
  if FPainting then begin
    PenDown := not PenDown;
    FirstPoint := PenDown;
  end;
  if Assigned(FOnPaintBoxClick) then FOnPaintBoxClick(Sender);
end;

procedure TPanZoomPanel.PaintBoxDblClick(Sender: TObject);
begin
  if not FPainting then
     if Assigned(FOnPaintBoxDblClick) then FOnPaintBoxDblClick(Sender);
end;

function TPanZoomPanel.DeviceToActual(const DeviceP: TPoint): TDoublePoint;
begin
  if ScaleDefined then
    Result := CurrentScaleSystem.DeviceToActual(DeviceP)
  else
    Result := UndefinedDoublePoint;
end;

function TPanZoomPanel.ActualToDevice(const ActualP: TDoublePoint): TPoint;
begin
  if ScaleDefined then
    Result := CurrentScaleSystem.ActualToDevice(ActualP)
  else
    Result := UndefinedPoint;
end;

procedure TPanZoomPanel.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
                                          X, Y: Integer);
var
  MasterP: TPoint;
  ActualP: TDoublePoint;
  (*$IFDEF PreDelphi3*)
    Rgn: HRGN;
    Overshoot: Integer;
    L: Integer;
    R: Integer;
    T: Integer;
    B: Integer;
  (*$ENDIF*)
begin
  if FZoomingIn then FRubberBand.Move(X, Y);
  if FPainting and PenDown then begin
    MasterP := CurrentScaleSystem.DeviceToMaster(MakePoint(X, Y));
    (*$IFDEF PreDelphi3*)
      if X < PrevX then begin
        L := X;
        R := PrevX;
      end
      else begin
        L := PrevX;
        R := X;
      end;
      if Y < PrevY then begin
        T := Y;
        B := PrevY;
      end
      else begin
        T := PrevY;
        B := Y;
      end;
    (*$ENDIF*)
    with FPaintBox, CurrentScaleSystem do begin
      if FirstPoint then begin
        Windows.MoveToEx(FBitmap.Canvas.Handle, MasterP.X, MasterP.Y, nil);
        FirstPoint := False;
      end
      else begin
        (*$IFDEF PreDelphi3*)
          Overshoot := 1 + MulDiv(Canvas.Pen.Width, ViewportExtentP.X,
                                  WindowExtentP.X);
        (*$ENDIF*)
        Windows.LineTo(FBitmap.Canvas.Handle, MasterP.X, MasterP.Y);
        (*$IFDEF PreDelphi3*)
          Rgn := CreateRectRgn(L - Overshoot + Left, T - Overshoot + Top,
                               R + Overshoot + Left, B + Overshoot + Top);
          SelectClipRgn(Canvas.Handle, Rgn);
        (*$ENDIF*)
        PaintBitmap;
        (*$IFDEF PreDelphi3*)
          DeleteObject(Rgn);
        (*$ENDIF*)
      end;
    end;
  end;
  if FUseSpanningCursor then begin
    PaintPrevSpanningCursor;
    PaintSpanningCursor(X, Y);
  end;
  PrevX := X;
  PrevY := Y;
  if Assigned(FOnPaintBoxMouseMove) then begin
    ActualP := DeviceToActual(MakePoint(X, Y));
    FOnPaintBoxMouseMove(Self, Shift, X, Y, ActualP.X, ActualP.Y);
  end;
end; { FPaintBoxMouseMove }

function TPanZoomPanel.MaxDeviceP: TPoint;
begin
  with CurrentScaleSystem do begin
    Result := MakePoint(MulDiv(FPaintBox.Width, ViewportExtentP.X,
                                      WindowExtentP.X),
                               MulDiv(FPaintBox.Height, ViewportExtentP.Y,
                                      WindowExtentP.Y));
  end;
end;
 
procedure TPanZoomPanel.SetupScrollBars;
var
  MaxWidth, MaxHeight: Integer;
  WasVisibleHorz, WasVisibleVert: Boolean;
  SPos, SPageSize: Integer;

  procedure CheckHorzScrollBar;
  var
    OldHeight: Integer;
  begin
    with FHorzScrollBar do begin
      Visible := FPaintBox.Width < MaxWidth;
      if Visible then begin
        {
         The Align property changed from D2 to D3. Starting in D3, a control's
         dimensions would be actively changed to fit better. So, if the bevel
         of the panel is 3, the corresponding dimension of the control (for
         the vertical scroll bar, the width; for the horizontal, the height)
         would be shrunk that much. To get around this, we have to store the
         original dimension.
        }
        OldHeight := Height;
        Align := alBottom;
        Height := OldHeight;
      end;
    end;
  end;

  procedure CheckVertScrollBar;
  var
    OldWidth: Integer;
  begin
    with FVertScrollBar do begin
      Visible := FPaintBox.Height < MaxHeight;
      if Visible then begin
        {
         The Align property changed from D2 to D3. Starting in D3, a control's
         dimensions would be actively changed to fit better. So, if the bevel
         of the panel is 3, the corresponding dimension of the control (for
         the vertical scroll bar, the width; for the horizontal, the height)
         would be shrunk that much. To get around this, we have to store the
         original dimension.
        }
        OldWidth := Width;
        Align := alRight;
        Width := OldWidth;
      end;
    end;
  end;

begin
  with CurrentScaleSystem do begin
    MaxWidth := MulDiv(FBitmap.Width, ViewportExtentP.X, WindowExtentP.X);
    MaxHeight := MulDiv(FBitmap.Height, ViewportExtentP.Y, WindowExtentP.Y);
  end;
  WasVisibleHorz := FHorzScrollBar.Visible;
  WasVisibleVert := FVertScrollBar.Visible;
  FPaintBox.Align := alClient;
  CheckHorzScrollBar;
  CheckVertScrollBar;
  {
   If the horizontal scroll bar didn't need to appear but the vertical one
   did need to appear, the visibility of the vertical one might cause the
   horizontal one to need to appear (because FPaintBox.Width changed). So
   check again.
  }
  if not FHorzScrollBar.Visible then CheckHorzScrollBar;
  with FPaintBox do begin
    Align := alNone;
    if not FHorzScrollBar.Visible then Width := MaxWidth;
    if not FVertScrollBar.Visible then Height := MaxHeight;
  end;
  with FHorzScrollBar do begin
    if Visible then begin
      Align := alNone;
      if FVertScrollBar.Visible then Width := FVertScrollBar.Left - Left;
      with CurrentScaleSystem do begin
        SPageSize := 1 + MulDiv(FPaintBox.Width, WindowExtentP.X,
                                ViewportExtentP.X);
      end;
      if WasVisibleHorz then begin
        SPos := Position;
        if SPos > Succ(Max - SPageSize) then SPos := Succ(Max - SPageSize);
      end
      else begin
        SPos := 0;
      end;
      SetParams(SPos, Min, FBitmap.Width);
      PageSize := SPageSize;
      OnScroll(Self, scTrack, SPos);
    end
    else begin
      CurrentScaleSystem.WindowOriginP.X := 0;
    end;
  end;
  with FVertScrollBar do begin
    if Visible then begin
      Align := alNone;
      if FHorzScrollBar.Visible then Height := FHorzScrollBar.Top - Top;
      with CurrentScaleSystem do begin
        SPageSize := 1 + MulDiv(FPaintBox.Height, WindowExtentP.Y,
                                ViewportExtentP.Y);
      end;
      if WasVisibleVert then begin
        SPos := Position;
        if SPos > Succ(Max - SPageSize) then SPos := Succ(Max - SPageSize);
      end
      else begin
        SPos := 0;
      end;
      SetParams(SPos, Min, FBitmap.Height);
      PageSize := SPageSize;
      OnScroll(Self, scTrack, SPos);
    end
    else begin
      CurrentScaleSystem.WindowOriginP.Y := 0;
    end;
  end;
  ReconfigureCurrentSystem;
end;

procedure TPanZoomPanel.DoResize;
begin
  if Assigned(FBitmap) then begin
    SetupScrollBars;
  end
  else begin
    FHorzScrollBar.Visible := False;
    FVertScrollBar.Visible := False;
  end;
  FPaintBox.Invalidate;
end;

const
  InvalidScrollCodes = [scEndScroll, scPosition];

procedure TPanZoomPanel.HorzScrollBarScroll(Sender: TObject;
                                            ScrollCode: TScrollCode;
                                            var ScrollPos: Integer);
begin
  CurrentScaleSystem.WindowOriginP.X := ScrollPos;
  if not (ScrollCode in InvalidScrollCodes) then FPaintBox.Invalidate;
end;

procedure TPanZoomPanel.VertScrollBarScroll(Sender: TObject;
                                            ScrollCode: TScrollCode;
                                            var ScrollPos: Integer);
begin
  CurrentScaleSystem.WindowOriginP.Y := ScrollPos;
  if not (ScrollCode in InvalidScrollCodes) then FPaintBox.Invalidate;
end;

procedure TPanZoomPanel.RubberBandStop(Sender: TObject; ARect: TRect);
var
  F1, F2: TPoint;
  WasVisibleHorz, WasVisibleVert: Boolean;
begin
  with ARect do if (Right = Left) or (Bottom = Top) then begin
    FRubberBand.Cancel;
    Exit;
  end;

  with CurrentScaleSystem, ARect do begin
    F1 := DeviceToMaster(MakePoint(Left, Top));
    F2 := DeviceToMaster(MakePoint(Right, Bottom));
  end;

  WasVisibleHorz := FHorzScrollBar.Visible;
  WasVisibleVert := FVertScrollBar.Visible;

  with CurrentScaleSystem do begin
    WindowOriginP := F1;
    WindowExtentP := MakePoint(F2.X - F1.X, F2.Y - F1.Y);
    ViewportExtentP.X := Self.UsableWidth;
    ViewportExtentP.Y := Self.UsableHeight;
    if FVertScrollBar.Visible then Dec(ViewportExtentP.X, FVertScrollBar.Width);
    if FHorzScrollBar.Visible then Dec(ViewportExtentP.Y, FHorzScrollBar.Height);
  end;

  DoResize;

  with CurrentScaleSystem do begin
    if FHorzScrollBar.Visible and not WasVisibleHorz then
      Dec(ViewportExtentP.Y, FHorzScrollBar.Height);
    if FVertScrollBar.Visible and not WasVisibleVert then
      Dec(ViewportExtentP.X, FVertScrollBar.Width);
  end;

  if (FHorzScrollBar.Visible and not WasVisibleHorz) or
     (FVertScrollBar.Visible and not WasVisibleVert) then
  begin
    DoResize;
  end;

  FHorzScrollBar.Position := F1.X;
  FVertScrollBar.Position := F1.Y;
  HorzScrollBarScroll(Self, scPosition, Integer(F1.X));
  VertScrollBarScroll(Self, scPosition, Integer(F1.Y));
  FPaintBox.Invalidate;
  FRubberBand.Cancel;
  if FUseSpanningCursor then FPaintBox.Update;
  {
   So the property setting routine knows which event to call, namely
   OnCancelZoomIn or OnEndZoomIn.
  }
  ZoomCompleted := True;
  SetZoomingIn(False);
end;

procedure TPanZoomPanel.CreateAndSetPaintingCursor;
const
  CursorDim = 32;
  PaintingCursorHandle: HCursor = 0;
  (*$IFDEF Ver80*)
    CLR_INVALID = $FFFFFFFF;
  (*$ENDIF*)
var
  X, Y, Z: Integer;
  BitColor: TColorRef;
  ByteBMP, ByteMSK: Byte;
  Bitmap, Mask: array [0..127] of Byte;
  CursorBitmap: TBitmap;
  PWidth: Integer;
  CursorPos: TPoint;
  ActualGray, ActualYellow: TColorRef; {TColor;}
begin
  if not RunningNT then begin
    if (GetSystemMetrics(SM_CXCURSOR) <> CursorDim) or
       (GetSystemMetrics(SM_CYCURSOR) <> CursorDim) then
    begin
      Exit;
    end;
  end;
  with CurrentScaleSystem do begin
    PWidth := 1 + MulDiv(FBitmap.Canvas.Pen.Width, ViewportExtentP.X,
                         WindowExtentP.X);
  end;
  Screen.Cursors[crPainting] := 0;
  if PaintingCursorHandle <> 0 then DestroyCursor(PaintingCursorHandle);
  CursorBitmap := TBitmap.Create;
  with CursorBitmap do begin
    Width := CursorDim;
    Height := Width;
    with Canvas do begin
      Pixels[0, 0] := clBlack;
      ActualGray := Pixels[0, 0];
      Pixels[0, 0] := clYellow;
      ActualYellow := Pixels[0, 0];
    end;
    { Make bitmap entirely gray, i.e. transparent. }
    SetPenAndBrush(Canvas, ActualGray, pmCopy, psSolid, 1, ActualGray, bsSolid);
    Canvas.Rectangle(0, 0, Width, Height);
    { Now draw a square to show the pen size. }
    SetPenAndBrush(Canvas, ActualYellow, pmCopy, psSolid, 1, ActualYellow,
                   bsClear);
    Canvas.Rectangle((Width - PWidth) div 2, (Height - PWidth) div 2,
                     (Width + PWidth) div 2, (Height + PWidth) div 2);
  end;

  for Y := 0 to 31 {CursorBitmap.Height - 1} do begin
    for X := 0 to 3 {CursorBitmap.Width div 8 - 1} do begin
      ByteBMP := 0;
      ByteMSK := 0;
      for Z := 0 to 7 do begin
        BitColor := GetPixel(CursorBitmap.Canvas.Handle, (X * 8) + Z, Y);
        if BitColor <> CLR_INVALID then begin
          ByteBMP := ByteBMP shl 1 or 1;
          ByteMSK := ByteMSK shl 1 or Byte(BitColor = ActualYellow);
        end;
      end;
      Bitmap[Y * 4 + X] := ByteBMP;
      Mask[Y * 4 + X] := ByteMSK;
    end;
  end;

  PaintingCursorHandle := CreateCursor(HInstance, CursorBitmap.Width div 2,
                                       CursorBitmap.Height div 2,
                                       CursorBitmap.Width, CursorBitmap.Height,
                                       @Bitmap, @Mask);

  CursorBitmap.Free;

  Screen.Cursors[crPainting] := PaintingCursorHandle;

  FPaintBox.Cursor := crPainting;
  { This forces a redraw of the cursor and its new size. }
  (*$IFDEF Win32*) if (*$ENDIF*)
  GetCursorPos(CursorPos)
  (*$IFDEF Win32*) then (*$ELSE*) ; (*$ENDIF*)
  SetCursorPos(CursorPos.X, CursorPos.Y);
end; { CreateAndSetPaintingCursor }

function TPanZoomPanel.GetPenWidth: Integer;
begin
  Result := FBitmap.Canvas.Pen.Width;
end;

procedure TPanZoomPanel.SetPenWidth(const NewPenWidth: Integer);
begin
  if Assigned(FBitmap) then begin
    FBitmap.Canvas.Pen.Width := NewPenWidth;
    CreateAndSetPaintingCursor;
  end;
end;

function TPanZoomPanel.GetRubberBandWidth: Integer;
begin
  Result := FRubberBand.BandWidth;
end;

procedure TPanZoomPanel.SetRubberBandWidth(const NewRubberBandWidth: Integer);
begin
  FRubberBand.BandWidth := NewRubberBandWidth;
end;


{ -----------------------------------------------------------------------------}


constructor TPsRubberBand.Create(AOwner: TComponent);
begin
  inherited;
  FHasStarted := False;
  FBandWidth := DefBandWidth;
  Brush := TBrush.Create;
  with Brush do begin
    Color := clWhite;
    Style := bsSolid;
  end;
end;

destructor TPsRubberBand.Destroy;
begin
  Brush.Free;
  inherited;
end;

procedure TPsRubberBand.SetProportional(const APropWidth, APropHeight: Integer);
begin
  FProportional := True;
  FPropWidth := APropWidth;
  FPropHeight := APropHeight;
end;

procedure TPsRubberBand.SetNonProportional;
begin
  FProportional := False;
end;

procedure TPsRubberBand.Start(const X, Y: Integer);
begin
  if FHasStarted then Exit;
  Band := Rect(X, Y, X, Y);
  FHasStarted := True;
end;

procedure TPsRubberBand.Move(const X, Y: Integer);
begin
  if not FHasStarted then Exit;
  OldBrush := FCanvas.Brush;
  FCanvas.Brush := Brush;
  Paint;
  UpdateBand(X, Y);
  Paint;
  FCanvas.Brush := OldBrush;
end;

procedure TPsRubberBand.Cancel;
begin
  if not FHasStarted then Exit;
  OldBrush := FCanvas.Brush;
  FCanvas.Brush := Brush;
  Paint;
  FCanvas.Brush := OldBrush;
  FHasStarted := False;
  if Assigned(FOnCancel) then FOnCancel(Self);
end;

procedure TPsRubberBand.Stop(const X, Y: Integer);
begin
  if not FHasStarted then Exit;
  OldBrush := FCanvas.Brush;
  FCanvas.Brush := Brush;
  Paint;
  FCanvas.Brush := OldBrush;
  UpdateBand(X, Y);
  FHasStarted := False;
  if Assigned(FOnStop) then FOnStop(Self, GetCorrectBand);
end;

function TPsRubberBand.GetCorrectBand: TRect;
begin
  with Band do begin
    if Left > Right then begin
      Result.Left := Right;
      Result.Right := Left;
    end
    else begin
      Result.Left := Left;
      Result.Right := Right;
    end;
    if Top > Bottom then begin
      Result.Top := Bottom;
      Result.Bottom := Top;
    end
    else begin
      Result.Top := Top;
      Result.Bottom := Bottom;
    end;
  end;
end;

procedure TPsRubberBand.SetBandWidth(const NewBandWidth: Integer);
begin
  if FHasStarted or (NewBandWidth < 1) then Exit;
  FBandWidth := NewBandWidth;
end;

procedure TPsRubberBand.Paint;
var
  I: Integer;
  CorrectBand: TRect;
begin
  CorrectBand := GetCorrectBand;
  for I := 1 to FBandWidth do begin
    Windows.DrawFocusRect(FCanvas.Handle, CorrectBand);
    with CorrectBand do begin
      Dec(Left);
      Dec(Top);
      Inc(Right);
      Inc(Bottom);
    end;
  end;
end;

procedure TPsRubberBand.UpdateBand(const X, Y: Integer);
var
  W, H: Integer;
begin
  with Band do begin
    if FProportional then begin
      W := MulDiv(Abs(Y - Top), FPropWidth, FPropHeight);
      H := Abs(X - Left);
      if W < H then begin
        if X < Left then W := -W;
        BottomRight := Point(Left + W, Y);
      end
      else begin
        H := MulDiv(H, FPropHeight, FPropWidth);
        if Y < Top then H := -H;
        BottomRight := Point(X, Top + H);
      end;
    end
    else begin
      BottomRight := Point(X, Y);
    end;
  end;
end;


{ -----------------------------------------------------------------------------}


constructor TResizablePanel.Create(AOwner: TComponent);
begin
  inherited;
  FMovable := DefMovable;
  FSizable := DefSizable;
  FSizeHandleWidth := DefSizeHandleWidth;
end;

destructor TResizablePanel.Destroy;
begin
  inherited;
end;

procedure TResizablePanel.WMNCHitTest(var Mess: TWMNCHitTest);
const
  MovableStatus: array [Boolean] of Integer = (HTCLIENT, HTCAPTION);
var
  P: TPoint;
begin
  if csDesigning in ComponentState then begin
    Parent.Dispatch(Mess);
    Exit;
  end;
  with Mess do begin
    if FSizable and (Align = alNone) then begin
      P := ScreenToClient(Point(XPos, YPos));
      if (P.X < FSizeHandleWidth) and (P.Y < FSizeHandleWidth) then
        Result := HTTOPLEFT
      else if (P.X < FSizeHandleWidth) and (Height - P.Y < FSizeHandleWidth) then
        Result := HTBOTTOMLEFT
      else if P.X < FSizeHandleWidth then
        Result := HTLEFT
      else if (Width - P.X < FSizeHandleWidth) and (P.Y < FSizeHandleWidth) then
        Result := HTTOPRIGHT
      else if (Width - P.X < FSizeHandleWidth) and
              (Height - P.Y < FSizeHandleWidth) then
      begin
        Result := HTBOTTOMRIGHT;
      end
      else if Width - P.X < FSizeHandleWidth then
        Result := HTRIGHT
      else if P.Y < FSizeHandleWidth then
        Result := HTTOP
      else if Height - P.Y < FSizeHandleWidth then
        Result := HTBOTTOM;
    end;
    if Result = 0 then Result := MovableStatus[(Align = alNone) and FMovable];
  end;
end;

function TResizablePanel.GetTotalBorderWidth: Integer;
begin
  Result := Ord(BorderStyle = bsSingle) +
            Ord(BevelOuter <> bvNone) * BevelWidth +
            Ord(BevelInner <> bvNone) * BorderWidth +
            Ord(BevelInner <> bvNone) * BevelWidth;
end;

function TResizablePanel.GetUsableWidth: Integer;
begin
  Result := ClientWidth - TotalBorderWidth shl 1;
end;

function TResizablePanel.GetUsableHeight: Integer;
begin
  Result := ClientHeight - TotalBorderWidth shl 1;
end;

procedure TPanZoomPanel.Fit();
begin
  if FBitmap <> nil then
     begin
     with CurrentScaleSystem do
       begin
       WindowExtentP.X := trunc(FBitmap.Width * (FBitmap.Width / self.Width));
       WindowExtentP.Y := trunc(FBitmap.Height * (FBitmap.Height / self.Height));
       end;
     ReconfigureCurrentSystem();
     DoResize();
     end;
end;

initialization
(*$IFDEF Win32*)
  OsVersionInfo.dwOSVersionInfoSize := SizeOf(OsVersionInfo);
(*$ENDIF*)
  RunningNT := (*$IFDEF Win32*)
                 GetVersionEx(OsVersionInfO) and
                 (OsVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT);
               (*$ELSE*)
                 False;
               (*$ENDIF*)

end.
