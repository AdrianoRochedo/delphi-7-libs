{**********************************************}
{   TeeChart and TeeTree TCanvas3D component   }
{   Copyright (c) 1998 by David Berneda        }
{        All Rights Reserved                   }
{**********************************************}
{$I teedefs.inc}
unit TeCanvas;
{$P-}

{.$DEFINE TEEPERF}               { creates a text file with speed timing values }
{.$DEFINE TEEGRAPHLIST}          { creates a graph list }
{.$DEFINE TEEUSEDRAWDIB}         { uses DIB bitmaps }
{.$DEFINE MONITOR_BUFFERREDRAWS} { displays buffer drawing counters }

interface

Uses {$IFDEF D1}
     WinProcs,WinTypes,
     {$ELSE}
     Windows,   { <--- because BCB3 "Illegal EXTDEF fixup bug }
     {$ENDIF}
     Classes,Controls,Graphics,SysUtils{$IFDEF TEEPERF},MMSystem{$ENDIF}
     {$IFDEF TEEUSEDRAWDIB}
     ,MMSystem
     {$ENDIF}
     ;

Const TeePiStep=(Pi/180.0);

type { Several specialized Pen classes }
     TChartPen=class(TPen)
     private
       FSmallDots : Boolean;
       FVisible   : Boolean;
       Procedure SetVisible(Value:Boolean);
       Procedure SetSmallDots(Value:Boolean);
     public
       Constructor Create(OnChangeEvent:TNotifyEvent);
       Procedure Assign(Source:TPersistent); override;
     published
       property SmallDots:Boolean read FSmallDots write SetSmallDots default False;
       property Visible:Boolean read FVisible write SetVisible default True;
     end;

     TChartHiddenPen=class(TChartPen)
     public
       Constructor Create(OnChangeEvent:TNotifyEvent);
     published
       property Visible default False;
     end;

     TDottedGrayPen=class(TChartPen)
     public
       Constructor Create(OnChangeEvent:TNotifyEvent);
     published
       property Color default clGray;
       property Style default psDot;
     end;

     TDarkGrayPen=class(TChartPen)
     public
       Constructor Create(OnChangeEvent:TNotifyEvent);
     published
       property Color default clDkGray;
     end;

     TChartArrowPen=class(TChartPen)
     public
       Constructor Create(OnChangeEvent:TNotifyEvent);
     published
       property Color default clWhite;
     end;

     TChartAxisPen=class(TChartPen)
     public
       Constructor Create(OnChangeEvent:TNotifyEvent);
     published
       property Width default 2;
     end;

     TTeeView3DScrolled=procedure(IsHoriz:Boolean) of object;
     TTeeView3DChangedZoom=procedure(NewZoom:Integer) of object;

     TView3DOptions = class(TPersistent)
     private
       FElevation   : Integer;
       FHorizOffset : Integer;
       FOrthogonal  : Boolean;
       FPerspective : Integer;
       FRotation    : Integer;
       FTilt        : Integer;
       FVertOffset  : Integer;
       FZoom        : Integer;
       FZoomText    : Boolean;
       FOnScrolled  : TTeeView3DScrolled;
       FOnChangedZoom:TTeeView3DChangedZoom;

       FParent      : TWinControl;
       Procedure SetElevation(Value:Integer);
       Procedure SetPerspective(Value:Integer);
       Procedure SetRotation(Value:Integer);
       Procedure SetTilt(Value:Integer);
       Procedure SetHorizOffset(Value:Integer);
       Procedure SetVertOffset(Value:Integer);
       Procedure SetOrthogonal(Value:Boolean);
       Procedure SetZoom(Value:Integer);
       Procedure SetZoomText(Value:Boolean);
       Procedure SetBooleanProperty(Var Variable:Boolean; Value:Boolean);
       Procedure SetIntegerProperty(Var Variable:Integer; Value:Integer);
     public
       Constructor Create(AParent:TWinControl);
       Procedure Repaint;
       Procedure Assign(Source:TPersistent); override;
       property Parent:TWinControl read FParent write FParent;
       property OnChangedZoom:TTeeView3DChangedZoom read FOnChangedZoom
                                                    write FOnChangedZoom;
       property OnScrolled:TTeeView3DScrolled read FOnScrolled write FOnScrolled;
     published
       property Elevation:Integer read FElevation write SetElevation default 345;
       property HorizOffset:Integer read FHorizOffset write SetHorizOffset default 0;
       property Orthogonal:Boolean read FOrthogonal write SetOrthogonal default True;
       property Perspective:Integer read FPerspective write SetPerspective default 15;
       property Rotation:Integer read FRotation write SetRotation default 345;
       property Tilt:Integer read FTilt write SetTilt default 0;
       property VertOffset:Integer read FVertOffset write SetVertOffset default 0;
       property Zoom:Integer read FZoom write SetZoom default 100;
       property ZoomText:Boolean read FZoomText write SetZoomText default True;
     end;

     TCanvasBackMode  = ( cbmNone,cbmTransparent,cbmOpaque );
     TCanvasTextAlign = Integer;  { TA_LEFT, TA_CENTER, TA_RIGHT }

     TGradientDirection=( gdTopBottom,
                          gdBottomTop,
                          gdLeftRight,
                          gdRightLeft,
                          gdFromCenter,
                          gdFromTopLeft,
                          gdFromBottomLeft );

     TTeeCanvas=class
     private
       FCanvas     : TCanvas;
       FFont       : TFont;
       FPen        : TPen;
       FBrush      : TBrush;
       FMetafiling : Boolean;
     protected
       Procedure SetCanvas(ACanvas:TCanvas);

       function GetBackColor:TColor; virtual; abstract;
       Function GetBackMode:TCanvasBackMode; virtual; abstract;
       Function GetHandle:HDC; virtual; abstract;
       Function GetMonochrome:Boolean; virtual; abstract;
       Function GetTextAlign:TCanvasTextAlign; virtual; abstract;
       Function GetUseBuffer:Boolean; virtual; abstract;

       Procedure SetBackColor(Color:TColor); virtual; abstract;
       Procedure SetBackMode(Mode:TCanvasBackMode); virtual; abstract;
       Procedure SetMonochrome(Value:Boolean); virtual; abstract;
       procedure SetPixel(X, Y: Integer; Value: TColor); virtual; abstract;
       procedure SetTextAlign(Align:TCanvasTextAlign); virtual; abstract;
       Procedure SetUseBuffer(Value:Boolean); virtual; abstract;

     public
       procedure AssignVisiblePen(APen:TChartPen);
       Procedure ResetState;

       { 2d }
       procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); virtual; abstract;
       procedure Draw(X, Y: Integer; Graphic: TGraphic); virtual; abstract;
       procedure Ellipse(X1, Y1, X2, Y2: Integer); virtual; abstract;
       procedure FillRect(const Rect: TRect); virtual; abstract;
       procedure Frame3D( Rect: TRect; TopColor,BottomColor: TColor; Width: Integer); virtual; abstract;
       procedure LineTo(X,Y:Integer); virtual; abstract;
       procedure MoveTo(X,Y:Integer); virtual; abstract;
       procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); virtual; abstract;
       procedure Rectangle(X0,Y0,X1,Y1:Integer); virtual; abstract;
       procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer); virtual; abstract;
       procedure StretchDraw(const Rect: TRect; Graphic: TGraphic); virtual; abstract;
       Procedure TextOut(X,Y:Integer; const Text:String); virtual; abstract;
       Function TextWidth(Const St:String):Integer; virtual;
       Function TextHeight(Const St:String):Integer; virtual;
       Function FontHeight:Integer;

       { 2d extra }
       procedure ClipRectangle(Const Rect:TRect); virtual; abstract;
       Procedure DoHorizLine(X0,X1,Y:Integer); virtual; abstract;
       Procedure DoRectangle(Const Rect:TRect); virtual; abstract;
       Procedure DoVertLine(X,Y0,Y1:Integer); virtual; abstract;
       procedure EraseBackground(const Rect: TRect); virtual; abstract;
       Procedure GradientFill( Const Rect:TRect;
                               StartColor,EndColor:TColor;
                               Direction:TGradientDirection); virtual; abstract;
       Procedure Invalidate; virtual; abstract;
       Procedure Line(X0,Y0,X1,Y1:Integer); virtual; abstract;
       Procedure Polygon(const Points: array of TPoint); virtual; abstract;
       procedure RotateLabel(x,y:Integer; Const St:String; RotDegree:Integer); virtual; abstract;
       procedure UnClipRectangle; virtual; abstract;

     { properties }
       property BackColor:TColor read GetBackColor write SetBackColor;
       property BackMode:TCanvasBackMode read GetBackMode write SetBackMode;
       property Brush:TBrush read FBrush;
       property Font:TFont read FFont;
       property Handle:HDC read GetHandle;
       property Metafiling:Boolean read FMetafiling write FMetafiling;
       property Monochrome:Boolean read GetMonochrome write SetMonochrome;
       property Pen:TPen read FPen;
       property Pixels[X, Y: Integer]: TColor write SetPixel;
       property ReferenceCanvas:TCanvas read FCanvas write SetCanvas;
       property TextAlign:TCanvasTextAlign read GetTextAlign write SetTextAlign;
       property UseBuffer:Boolean read GetUseBuffer write SetUseBuffer;
     end;

     { 3d }
     TPoint3D  = record x,y,z:Integer; end;
     TFourPoints = Array[0..3] of TPoint;

     TCanvas3D=class(TTeeCanvas)
     private
       F3DOptions    : TView3DOptions;
       FIsOrthogonal : Boolean;
     protected
       procedure SetPixel3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; Value: TColor); virtual; abstract;
       Function GetSupportsFullRotation:Boolean; virtual; abstract;
       Function GetSupports3DText:Boolean; virtual; abstract;
     public
     { 3d }
       Procedure Calculate2DPosition(Var x,y:Integer; z:Integer); virtual; abstract;
       Function Calculate3DPosition(x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}):TPoint; virtual; abstract;

       Function InitWindow( DestCanvas:TCanvas;
                            A3DOptions:TView3DOptions;
                            ABackColor:TColor;
                            Is3D:Boolean;
                            Const UserRect:TRect):TRect; virtual; abstract;

       Procedure Assign(Source:TCanvas3D); virtual;

       Procedure Projection(MaxDepth:Integer; const Bounds,Rect:TRect); virtual; abstract;
       Procedure ShowImage(DestCanvas,DefaultCanvas:TCanvas; Const UserRect:TRect); virtual; abstract;
       Function ReDrawBitmap:Boolean; virtual; abstract;

       Procedure Arrow( Filled:Boolean;
                        Const FromPoint,ToPoint:TPoint;
                        ArrowWidth,ArrowHeight,Z:Integer); virtual; abstract;
       procedure ClipCube(Const Rect:TRect; MinZ,MaxZ:Integer); virtual; abstract;
       Procedure Cube(Left,Right,Top,Bottom,Z0,Z1:Integer; DarkSides:Boolean); virtual; abstract;
       procedure Cylinder(Vertical:Boolean; Left,Top,Right,Bottom,Z0,Z1:Integer; Dark3D:Boolean); virtual; abstract;
       Procedure HorizLine3D(Left,Right,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); virtual; abstract;
       Procedure VertLine3D(X,Top,Bottom,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); virtual; abstract;
       Procedure ZLine3D(X,Y,Z0,Z1:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); virtual; abstract;
       procedure EllipseWithZ(X1, Y1, X2, Y2, Z: {$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); virtual; abstract;
       procedure FrontPlaneBegin; virtual; abstract;
       procedure FrontPlaneEnd; virtual; abstract;
       Procedure LineWithZ(X0,Y0,X1,Y1,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); virtual; abstract;
       procedure MoveTo3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); virtual; abstract;
       procedure LineTo3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); virtual; abstract;
       procedure Pie3D( XCenter,YCenter,XRadius,YRadius,Z0,Z1:Integer;
                        Const StartAngle,EndAngle:Double;
                        DarkSides,DrawSides:Boolean); virtual; abstract;
       procedure Plane3D(Const A,B:TPoint; Z0,Z1:Integer); virtual; abstract;
       procedure PlaneWithZ(P1,P2,P3,P4:TPoint; Z:Integer); virtual; abstract;
       procedure PlaneFour3D(Points:TFourPoints; Z0,Z1:Integer); virtual; abstract;
       procedure Pyramid(Vertical:Boolean; Left,Top,Right,Bottom,z0,z1:Integer; DarkSides:Boolean); virtual; abstract;
       Procedure RectangleWithZ(Const Rect:TRect; Z:Integer); virtual; abstract;
       Procedure RectangleY(Left,Top,Right,Z0,Z1:Integer); virtual; abstract;
       Procedure RectangleZ(Left,Top,Bottom,Z0,Z1:Integer); virtual; abstract;
       procedure RotateLabel3D( x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
                                Const St:String; RotDegree:Integer); virtual; abstract;
       Procedure TextOut3D(x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; const Text:String); virtual; abstract;
       procedure TriangleWithZ(Const P1,P2,P3:TPoint; Z:Integer); virtual; abstract;

       property Pixels3D[X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}]:TColor write SetPixel3D;
       property Supports3DText:Boolean read GetSupports3DText;
       property SupportsFullRotation:Boolean read GetSupportsFullRotation;
       property View3DOptions:TView3DOptions read F3DOptions write F3DOptions;
     end;

     TTeeCanvas3D=class(TCanvas3D)
     private
       FXCenter        : Longint;
       FYCenter        : Longint;
       FZCenter        : Longint;
       FXCenterOffset  : Longint;
       FYCenterOffset  : Longint;

       s2              : Extended;
       c2s1            : Extended;
       c2s3            : Double;
       c2c3            : Double;
       c2c1            : Double;
       tempXX          : Double;
       tempYX          : Double;
       tempXZ          : Double;
       tempYZ          : Double;

       FWas3D          : Boolean;
       FIs3D           : Boolean;

       FBitmap         : TBitmap;
       FBufferedDisplay: Boolean;
       FMonochrome     : Boolean;
       FDirty          : Boolean;

       FBounds         : TRect;

       IZoomText       : Boolean;
       IZoomFactor     : Double;
       IPerspec        : Double;

       Procedure TransferBitmap(ALeft,ATop:Integer; ACanvas:TCanvas);

     protected

       { 2d }
       function GetBackColor:TColor; override;
       Function GetBackMode:TCanvasBackMode; override;
       Function GetHandle:HDC; override;
       Function GetMonochrome:Boolean; override;
       Function GetSupports3DText:Boolean; override;
       Function GetSupportsFullRotation:Boolean; override;
       Function GetTextAlign:TCanvasTextAlign; override;
       Function GetUseBuffer:Boolean; override;

       Procedure SetBackColor(Color:TColor); override;
       Procedure SetBackMode(Mode:TCanvasBackMode); override;
       Procedure SetMonochrome(Value:Boolean); override;
       procedure SetPixel(X, Y: Integer; Value: TColor); override;
       procedure SetTextAlign(Align:TCanvasTextAlign); override;
       Procedure SetUseBuffer(Value:Boolean); override;

       { 3d private }
       Procedure Calc3DTPoint(Var P:TPoint; z:Integer);
       Function Calc3DTPoint3D(Const P:TPoint3D):TPoint;
       Procedure Calc3DPoint(Var P:TPoint; x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});

       { 3d }
       procedure SetPixel3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; Value: TColor); override;
       Procedure Calc3DPos(var x,y:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; z:Integer);
     public
       { almost public... }
       Procedure Calculate2DPosition(Var x,y:Integer; z:Integer); override;
       Function Calculate3DPosition(x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}):TPoint; override;

       { public }
       Constructor Create;
       Destructor Destroy; override;

       Function InitWindow( DestCanvas:TCanvas;
                            A3DOptions:TView3DOptions;
                            ABackColor:TColor;
                            Is3D:Boolean;
                            Const UserRect:TRect):TRect; override;
       Function ReDrawBitmap:Boolean; override;
       Procedure ShowImage(DestCanvas,DefaultCanvas:TCanvas; Const UserRect:TRect); override;

       procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); override;
       procedure Draw(X, Y: Integer; Graphic: TGraphic); override;
       procedure Ellipse(X1, Y1, X2, Y2: Integer); override;
       procedure EraseBackground(const Rect: TRect); override;
       procedure FillRect(const Rect: TRect); override;
       procedure Frame3D( Rect: TRect; TopColor,BottomColor: TColor;
                          Width: Integer); override;
       procedure LineTo(X,Y:Integer); override;
       procedure MoveTo(X,Y:Integer); override;
       procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); override;
       procedure Rectangle(X0,Y0,X1,Y1:Integer); override;
       procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer); override;
       procedure StretchDraw(const Rect: TRect; Graphic: TGraphic); override;
       Procedure TextOut(X,Y:Integer; const Text:String); override;

       { 2d extra }
       procedure ClipRectangle(Const Rect:TRect); override;
       procedure ClipCube(Const Rect:TRect; MinZ,MaxZ:Integer); override;
       Procedure DoRectangle(Const Rect:TRect); override;
       Procedure DoHorizLine(X0,X1,Y:Integer); override;
       Procedure DoVertLine(X,Y0,Y1:Integer); override;
       Procedure GradientFill( Const Rect:TRect;
                               StartColor,EndColor:TColor;
                               Direction:TGradientDirection); override;
       Procedure Invalidate; override;
       Procedure Line(X0,Y0,X1,Y1:Integer); override;
       Procedure Polygon(const Points: array of TPoint); override;
       procedure RotateLabel(x,y:Integer; Const St:String; RotDegree:Integer); override;
       procedure RotateLabel3D( x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
                                Const St:String; RotDegree:Integer); override;
       procedure UnClipRectangle; override;

       property XCenter:Longint read FXCenter write FXCenter;
       property YCenter:Longint read FYCenter write FYCenter;

       { 3d }
       Procedure Projection(MaxDepth:Integer; const Bounds,Rect:TRect); override;

       Procedure Arrow( Filled:Boolean;
                        Const FromPoint,ToPoint:TPoint;
                        ArrowWidth,ArrowHeight,Z:Integer); override;
       Procedure Cube(Left,Right,Top,Bottom,Z0,Z1:Integer; DarkSides:Boolean); override;
       procedure Cylinder(Vertical:Boolean; Left,Top,Right,Bottom,Z0,Z1:Integer; Dark3D:Boolean); override;
       procedure EllipseWithZ(X1, Y1, X2, Y2, Z: {$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;
       Procedure RectangleZ(Left,Top,Bottom,Z0,Z1:Integer); override;
       Procedure RectangleY(Left,Top,Right,Z0,Z1:Integer); override;
       procedure FrontPlaneBegin; override;
       procedure FrontPlaneEnd; override;
       Procedure HorizLine3D(Left,Right,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;
       procedure LineTo3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;
       Procedure LineWithZ(X0,Y0,X1,Y1,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;
       procedure MoveTo3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;
       procedure Pie3D( XCenter,YCenter,XRadius,YRadius,Z0,Z1:Integer;
                        Const StartAngle,EndAngle:Double;
                        DarkSides,DrawSides:Boolean); override;
       procedure Plane3D(Const A,B:TPoint; Z0,Z1:Integer); override;
       procedure PlaneWithZ(P1,P2,P3,P4:TPoint; Z:Integer); override;
       procedure PlaneFour3D(Points:TFourPoints; Z0,Z1:Integer); override;
       procedure Pyramid(Vertical:Boolean; Left,Top,Right,Bottom,z0,z1:Integer; DarkSides:Boolean); override;
       Procedure RectangleWithZ(Const Rect:TRect; Z:Integer); override;
       Procedure TextOut3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; const Text:String); override;
       procedure TriangleWithZ(Const P1,P2,P3:TPoint; Z:Integer); override;
       Procedure VertLine3D(X,Top,Bottom,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;
       Procedure ZLine3D(X,Y,Z0,Z1:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}); override;

       property Bounds:TRect read FBounds;
     end;

Function ApplyDark(Color:TColor; HowMuch:Byte):TColor;
Function ApplyBright(Color:TColor; HowMuch:Byte):TColor;

Function Point3D(x,y,z:Integer):TPoint3D;

procedure SinCos(Const Angle:Extended; var ResultSin, ResultCos: Extended);
function ArcTan2(Y, X: Extended): Extended;

Procedure SwapLongint(Var a,b:Longint);  { exchanges a and b }
Procedure SwapDouble(Var a,b:Double);    { exchanges a and b }
Procedure SwapInteger(Var a,b:Integer);  { exchanges a and b }

Function MaxDouble(Const a,b:Double):Double; { returns max (a or b) }
Function MinDouble(Const a,b:Double):Double; { returns min (a or b) }

Function MaxLong(a,b:Longint):Longint; { returns max (a or b) }
Function MinLong(a,b:Longint):Longint; { returns min (a or b) }

Procedure RectSize(Const R:TRect; Var RectWidth,RectHeight:Longint);
Procedure RectCenter(Const R:TRect; Var XCenter,YCenter:Longint);
Procedure ClipCanvas(ACanvas:TCanvas; Const Rect:TRect);
Procedure UnClipCanvas(ACanvas:TCanvas);

Const TeeCharForHeight     = 'W';  { <-- this is used to calculate Text Height }
      DarkerColorQuantity  : Byte=128; { <-- for dark 3D sides }
      DarkColorQuantity    : Byte=64;
      TeeGradientPrecision : Integer = 2;   { how many pixels precision, ( 1=best) }

{$IFDEF TEETRIAL}
Procedure TeeTrial(ComponentState:TComponentState);
Type TTeeTrialShowAbout=Procedure(ShowOrderForm:Boolean);
Var TeeTrialShowAboutBox:TTeeTrialShowAbout{$IFNDEF D1}=nil{$ENDIF};
{$ENDIF}

{$IFDEF TEEGRAPHLIST}
Procedure GraphListPlay(ACanvas:TCanvas);
{$ENDIF}

implementation

{$IFDEF TEETRIAL}
Uses TeeAbout;
{$ENDIF}

{$IFDEF MONITOR_BUFFERREDRAWS}
Const RedrawCountBuffer:Integer=0;
{$ENDIF}

{$IFDEF TEETRIAL}
Const TeeTreeMargin:Boolean=True;
{$ENDIF}

type PPoints = ^TPoints;
     TPoints = array[0..0] of TPoint;

{$IFDEF TEEPERF}
var F:TextFile;
    StartTime:Longint;

Procedure Log(Const S:String);
var t:Integer;
begin
  t:=TimeGetTime;
  WriteLn(f,IntToStr(t-StartTime)+#9+S);
  StartTime:=t;
end;
{$ENDIF}

{$IFDEF TEEGRAPHLIST}
Var GraphList:TList;

const TGL_SETBKMODE=0;
      TGL_SETBKCOLOR=1;
      TGL_TEXTOUT=2;
      TGL_Rectangle=3;
      TGL_RoundRectangle=4;
      TGL_SetTextAlign=5;
      TGL_MoveTo=6;
      TGL_LineTo=7;
      TGL_Poly=8;

type TGraphListItem=class
       Id:Integer;
       Value:Integer;
     end;

     TGraphListTextOut=class(TGraphListItem)
       x,y:Integer;
       Text:String;
     end;

     TGraphListRect=class(TGraphListItem)
       Rect:TRect;
     end;

     TGraphListRoundRect=class(TGraphListRect)
       X3,Y3:Integer;
     end;

     TGraphListMoveTo=class(TGraphListItem)
       X,Y:Integer;
     end;

     TGraphListLineTo=class(TGraphListMoveTo)
     end;

     TGraphListPoly=class(TGraphListItem)
       Num:Integer;
       Points:PPoints;
     end;

Procedure GraphListInit;
begin
  if GraphList=nil then GraphList:=TList.Create;
  GraphList.Clear;
end;

Procedure GraphListDone;
var t:Integer;
begin
  for t:=0 to GraphList.Count-1 do
      TGraphListItem(GraphList[t]).Free;
  GraphList.Free;
end;

Procedure GraphListAdd(Value:Pointer);
begin
  GraphList.Add(Value);
end;

Procedure GraphListAddInteger(ID:Integer; Value:Integer);
Var R:TGraphListItem;
begin
  R:=TGraphListItem.Create;
  R.Id:=Id;
  R.Value:=Value;
  GraphList.Add(R);
end;

Procedure GraphListAddRect(Const Rect:TRect);
Var R:TGraphListRect;
begin
  R:=TGraphListRect.Create;
  R.Id:=TGL_Rectangle;
  R.Rect:=Rect;
  GraphListAdd(R);
end;

Procedure GraphListAddPoly(P:PPoints; N:Integer);
Var R:TGraphListPoly;
begin
  R:=TGraphListPoly.Create;
  R.Id:=TGL_Poly;
  R.Num:=N;
  R.Points:=P;
  GraphListAdd(R);
end;

Procedure GraphListAddTextOut(x,y:Integer; Const Text:String);
Var R:TGraphListTextOut;
begin
  R:=TGraphListTextOut.Create;
  R.Id:=TGL_TEXTOUT;
  R.X:=X;
  R.Y:=Y;
  R.Text:=Text;
  GraphListAdd(R);
end;

Procedure GraphListAddMoveTo(x,y:Integer);
Var R:TGraphListMoveTo;
begin
  R:=TGraphListMoveTo.Create;
  R.Id:=TGL_MoveTo;
  R.X:=X;
  R.Y:=Y;
  GraphListAdd(R);
end;

Procedure GraphListAddLineTo(x,y:Integer);
Var R:TGraphListLineTo;
begin
  R:=TGraphListLineTo.Create;
  R.Id:=TGL_LineTo;
  R.X:=X;
  R.Y:=Y;
  GraphListAdd(R);
end;

Procedure GraphListPlay(ACanvas:TCanvas);
var dc:HDC;
    t:Integer;
begin
  DC:=ACanvas.Handle;
  for t:=0 to GraphList.Count-1 do
  With TGraphListItem(GraphList[t]) do
  Case Id of
      TGL_SETBKMODE  : SetBkMode(DC,Value);
      TGL_SETBKCOLOR : SetBkColor(DC,Value);
      TGL_TEXTOUT    :
      With TGraphListTextOut(GraphList[t]) do
           Windows.TextOut(dc,x,y,@Text[1],Length(Text));

      TGL_Rectangle:
         With TGraphListRect(GraphList[t]).Rect do
              Windows.Rectangle(dc,Left,Top,Right,Bottom);

      TGL_RoundRectangle :
         With TGraphListRoundRect(GraphList[t]) do
         With Rect do Windows.RoundRect(dc,Left,Top,Right,Bottom,x3,y3);
      TGL_SetTextAlign : SetTextAlign(DC,Value);
      TGL_MoveTo:
         With TGraphListMoveTo(GraphList[t]) do
              Windows.MoveToEx(dc, x,y,nil);
      TGL_LineTo:
         With TGraphListLineTo(GraphList[t]) do
              Windows.LineTo(dc, x,y);
      TGL_Poly:
         With TGraphListPoly(GraphList[t]) do
              Windows.Polygon(dc,Points,Num);

  end;
end;
{$ENDIF}

{$IFDEF D1}
procedure SinCos(Const Angle:Extended; var ResultSin, ResultCos: Extended);
begin
  ResultSin:=Sin(Angle);
  ResultCos:=Cos(Angle);
end;
{$ELSE}
procedure SinCos(Const Angle: Extended; var ResultSin, ResultCos: Extended);
asm
  FLD     Angle
  FSINCOS
  FSTP    tbyte ptr [edx]    { Cos }
  FSTP    tbyte ptr [eax]    { Sin }
  FWAIT
end;
{$ENDIF}

{$IFDEF D1}
function ArcTan2(Y, X: Extended): Extended;
Const HalfPi=Pi*0.5;
begin
  if x=0 then
  begin
     if y<0 then result:=-HalfPi
            else result:=HalfPi;
  end
  else
  begin
    result:=ArcTan(y/x);
    if x<0 then
       if y<0 then result:=result-pi
              else result:=result+pi;
  end;
end;
{$ELSE}
function ArcTan2(Y, X: Extended): Extended;
asm
  FLD     Y
  FLD     X
  FPATAN
  FWAIT
end;
{$ENDIF}

Function Point3D(x,y,z:Integer):TPoint3D;
begin
  result.x:=x;
  result.y:=y;
  result.z:=z;
end;

Procedure RectSize(Const R:TRect; Var RectWidth,RectHeight:Longint);
begin
  With R do
  begin
    RectWidth :=Right-Left;
    RectHeight:=Bottom-Top;
  end;
end;

Procedure RectCenter(Const R:TRect; Var XCenter,YCenter:Longint);
begin
  With R do
  begin
    XCenter:=(Left+Right) div 2;
    YCenter:=(Top+Bottom) div 2;
  end;
end;

{ TChartPen = TPen+Visible property }
Constructor TChartPen.Create(OnChangeEvent:TNotifyEvent);
begin
  inherited Create;
  FSmallDots:=False;
  FVisible:=True;
  OnChange:=OnChangeEvent;
end;

Procedure TChartPen.Assign(Source:TPersistent);
begin
  if Source is TChartPen then
  begin
    FVisible:=TChartPen(Source).FVisible;
    FSmallDots:=TChartPen(Source).FSmallDots;
  end;
  inherited Assign(Source);
end;

Procedure TChartPen.SetVisible(Value:Boolean);
Begin
  if FVisible<>Value then
  begin
    FVisible:=Value;
    Changed;
  end;
end;

Procedure TChartPen.SetSmallDots(Value:Boolean);
begin
  if FSmallDots<>Value then
  begin
    FSmallDots:=Value;
    Changed;
  end;
end;

{ TChartHiddenPen }
Constructor TChartHiddenPen.Create(OnChangeEvent:TNotifyEvent);
Begin
  inherited Create(OnChangeEvent);
  Visible:=False;
end;

{ TDottedGrayPen }
Constructor TDottedGrayPen.Create(OnChangeEvent:TNotifyEvent);
Begin
  inherited Create(OnChangeEvent);
  Color:=clGray;
  Style:=psDot;
end;

{ TDarkGrayPen }
Constructor TDarkGrayPen.Create(OnChangeEvent:TNotifyEvent);
Begin
  inherited Create(OnChangeEvent);
  Color:=clDkGray;
end;

{ TChartAxisPen }
Constructor TChartAxisPen.Create(OnChangeEvent:TNotifyEvent);
Begin
  inherited Create(OnChangeEvent);
  Width:=2;
end;

{ TChartArrowPen }
Constructor TChartArrowPen.Create(OnChangeEvent:TNotifyEvent);
Begin
  inherited Create(OnChangeEvent);
  Color:=clWhite;
end;

{ TView3DOptions }
Constructor TView3DOptions.Create(AParent:TWinControl);
begin
  inherited Create;
  FParent      :=AParent;
  FOrthogonal  :=True;
  FZoom        :=100; { % }
  FZoomText    :=True;
  FRotation    :=345;
  FElevation   :=345;
  FPerspective :=15; { % }
end;

Procedure TView3DOptions.Repaint;
begin
  FParent.Invalidate;
end;

Procedure TView3DOptions.SetIntegerProperty(Var Variable:Integer; Value:Integer);
begin
  if Variable<>Value then
  begin
    Variable:=Value;
    Repaint;
  end;
end;

Procedure TView3DOptions.SetBooleanProperty(Var Variable:Boolean; Value:Boolean);
begin
  if Variable<>Value then
  begin
    Variable:=Value;
    Repaint;
  end;
end;

Procedure TView3DOptions.SetElevation(Value:Integer);
begin
  SetIntegerProperty(FElevation,Value);
end;

Procedure TView3DOptions.SetPerspective(Value:Integer);
begin
  SetIntegerProperty(FPerspective,Value);
end;

Procedure TView3DOptions.SetRotation(Value:Integer);
begin
  SetIntegerProperty(FRotation,Value);
end;

Procedure TView3DOptions.SetTilt(Value:Integer);
begin
  SetIntegerProperty(FTilt,Value);
end;

Procedure TView3DOptions.SetHorizOffset(Value:Integer);
begin
  if FHorizOffset<>Value then
  begin
    FHorizOffset:=Value;
    Repaint;
    if Assigned(FOnScrolled) then FOnScrolled(True);
  end;
end;

Procedure TView3DOptions.SetVertOffset(Value:Integer);
begin
  if FVertOffset<>Value then
  begin
    FVertOffset:=Value;
    Repaint;
    if Assigned(FOnScrolled) then FOnScrolled(False);
  end;
end;

Procedure TView3DOptions.SetOrthogonal(Value:Boolean);
begin
  SetBooleanProperty(FOrthogonal,Value);
end;

Procedure TView3DOptions.SetZoom(Value:Integer);
begin
  if FZoom<>Value then
  begin
    if Assigned(FOnChangedZoom) then FOnChangedZoom(Value);
    FZoom:=Value;
    Repaint;
  end;
end;

Procedure TView3DOptions.SetZoomText(Value:Boolean);
begin
  SetBooleanProperty(FZoomText,Value);
end;

Procedure TView3DOptions.Assign(Source:TPersistent);
begin
  if Source is TView3DOptions then
  With TView3DOptions(Source) do
  begin
    Self.FElevation   :=FElevation;
    Self.FHorizOffset :=FHorizOffset;
    Self.FOrthogonal  :=FOrthogonal;
    Self.FPerspective :=FPerspective;
    Self.FRotation    :=FRotation;
    Self.FTilt        :=FTilt;
    Self.FVertOffset  :=FVertOffset;
    Self.FZoom        :=FZoom;
    Self.FZoomText    :=FZoomText;
  end;
end;

{ TTeeCanvas }
Procedure TTeeCanvas.SetCanvas(ACanvas:TCanvas);
begin
  FCanvas := ACanvas;
  FPen    := FCanvas.Pen;
  FFont   := FCanvas.Font;
  FBrush  := FCanvas.Brush;
end;

Procedure TTeeCanvas.ResetState;
begin
  With FPen do
  begin
    Color:=clBlack;
    Width:=1;
    Style:=psSolid;
  end;
  With FBrush do
  begin
    Color:=clWhite;
    Style:=bsSolid;
  end;
  With FFont do
  begin
    Color:=clBlack;
    Size:=10;
  end;
  BackColor:=clWhite;
  BackMode:=cbmTransparent;
end;

procedure TTeeCanvas.AssignVisiblePen(APen:TChartPen);
Var LBrush:TLogBrush;
begin
  if APen.Visible then
  begin
    {$IFNDEF D1}    { only valid in Windows-NT ... }
    if APen.SmallDots and (APen.Width=1) and
       (Win32Platform=VER_PLATFORM_WIN32_NT) then
    begin
      LBrush.lbStyle:=bs_Solid;
      LBrush.lbColor:=ColorToRGB(APen.Color);
      FPen.Handle:=ExtCreatePen( PS_COSMETIC or PS_ALTERNATE,
                                 1,LBrush,0,nil);
    end
    else
    {$ENDIF}
      FPen.Assign(APen);
  end
  else
    FPen.Style:=psClear;
end;

Function TTeeCanvas.TextWidth(Const St:String):Integer;
var Extent:TSize;
    {$IFDEF D1}
    tmpFont,tmpDC:THandle;
    {$ENDIF}
begin
{$IFDEF D1}
  if Metafiling then
  Begin
    tmpDC  :=CreateCompatibleDC(0);
    tmpFont:=SelectObject(tmpDC, FFont.Handle);
    if GetTextExtentPoint(tmpDC, @St[1], Length(St), Extent) then
       result := Extent.cX + 1
    else
       result := 0;
    SelectObject(tmpDC,tmpFont);
    DeleteDC(tmpDC);
  end
  else
{$ENDIF}
  begin
    ReferenceCanvas.Font.Assign(FFont);
    if GetTextExtentPoint( ReferenceCanvas.Handle,
                           {$IFNDEF D1}
                           PChar(St)
                           {$ELSE}
                           @St[1]
                           {$ENDIF}, Length(St), Extent) then
       result:=Extent.cX
    else
       result:=0;
  end;
end;

Function TTeeCanvas.TextHeight(Const St:String):Integer;
var Extent:TSize;
    {$IFDEF D1}
    tmpDC,tmpFont : THandle;
    {$ENDIF}
Begin
{$IFDEF D1}
  if Metafiling then
  Begin
    tmpDC  :=CreateCompatibleDC(0);
    tmpFont:=SelectObject(tmpDC, FFont.Handle);
    if GetTextExtentPoint(tmpDC, @St[1], Length(St), Extent) then
       result := Extent.cY
    else
       result := 0;
    SelectObject(tmpDC,tmpFont);
    DeleteDC(tmpDC);
  end
  else
{$ENDIF}
  begin
    ReferenceCanvas.Font.Assign(FFont);
    if GetTextExtentPoint( ReferenceCanvas.Handle,
                           {$IFNDEF D1}
                           PChar(St)
                           {$ELSE}
                           @St[1]
                           {$ENDIF}, Length(St), Extent) then
       result:=Extent.cY
    else
       result:=0;
  end;
end;

Function TTeeCanvas.FontHeight:Integer;
begin
  result:=TextHeight(TeeCharForHeight);
end;

{ TCanvas3D }
Procedure TCanvas3D.Assign(Source:TCanvas3D);
begin
  Monochrome:=Source.Monochrome;
end;

{ TTeeCanvas3D }
Constructor TTeeCanvas3D.Create;
begin
  inherited Create;
  IZoomText:=True;
  FMetafiling:=False;
  FCanvas:=nil;
  F3DOptions:=nil;
  FBitmap:=nil;
  FIs3D:=False;
  FBufferedDisplay:=True;
  FDirty:=True;
  {$IFDEF TEEPERF}
  AssignFile(f,'c:\log.txt');
  Rewrite(f);
  StartTime:=TimeGetTime;
  Log('create');
  {$ENDIF}
end;

Destructor TTeeCanvas3D.Destroy;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap:=nil;
  end;
  {$IFDEF TEEPERF}
  CloseFile(f);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListDone;
  {$ENDIF}
  inherited Destroy;
end;

Function TTeeCanvas3D.GetBackMode:TCanvasBackMode;
begin
  {$IFDEF TEEPERF}
  Log('GetBackMode');
  {$ENDIF}
  result:=TCanvasBackMode(GetBkMode(FCanvas.Handle));
end;

Procedure TTeeCanvas3D.SetBackMode(Mode:TCanvasBackMode); { Opaque, Transparent }
begin
  {$IFDEF TEEPERF}
  Log('SetBackMode');
  {$ENDIF}
  SetBkMode(FCanvas.Handle,Ord(Mode));
  {$IFDEF TEEGRAPHLIST}
  GraphListAddInteger(TGL_SETBKMODE,Ord(Mode));;
  {$ENDIF}
end;

Procedure TTeeCanvas3D.SetBackColor(Color:TColor);
begin
  {$IFDEF TEEPERF}
  Log('SetBackColor');
  {$ENDIF}
  SetBkColor(FCanvas.Handle,Color);
  {$IFDEF TEEGRAPHLIST}
  GraphListAddInteger(TGL_SETBKCOLOR,Color);;
  {$ENDIF}
end;

function TTeeCanvas3D.GetBackColor:TColor;
begin
  {$IFDEF TEEPERF}
  Log('GetBackColor');
  {$ENDIF}
  result:=GetBkColor(FCanvas.Handle);
end;

Procedure TTeeCanvas3D.TextOut(X,Y:Integer; const Text:String);
begin
  {$IFDEF TEEPERF}
  Log('TextOut');
  {$ENDIF}
  FCanvas.TextOut(X,Y,Text);
  {$IFDEF TEEGRAPHLIST}
  GraphListAddTextOut(x,y,Text);
  {$ENDIF}
end;

procedure TTeeCanvas3D.Rectangle(X0,Y0,X1,Y1:Integer);
begin
  {$IFDEF TEEPERF}
  Log('Rectangle');
  {$ENDIF}
  FCanvas.Rectangle(X0,Y0,X1,Y1);
  {$IFDEF TEEGRAPHLIST}
  GraphListAddRect(Rect(X0,Y0,X1,Y1));
  {$ENDIF}
end;

procedure TTeeCanvas3D.RoundRect(X1,Y1,X2,Y2,X3,Y3:Integer);
{$IFDEF TEEGRAPHLIST}
Var R:TGraphListRoundRect;
{$ENDIF}
begin
  {$IFDEF TEEPERF}
  Log('RoundRect');
  {$ENDIF}
  FCanvas.RoundRect(X1,Y1,X2,Y2,X3,Y3);
  {$IFDEF TEEGRAPHLIST}
  R:=TGraphListRoundRect.Create;
  R.Id:=TGL_RoundRectangle;
  R.Rect:=Rect(X1,Y1,X2,Y2);
  R.X3:=X3;
  R.Y3:=Y3;
  GraphListAdd(R);
  {$ENDIF}
end;

procedure TTeeCanvas3D.SetTextAlign(Align:TCanvasTextAlign);
begin
  {$IFDEF TEEPERF}
  Log('SetTextAlign');
  {$ENDIF}
  {$IFDEF D1}
  WinProcs.SetTextAlign(FCanvas.Handle,Ord(Align));
  {$ELSE}
  Windows.SetTextAlign(FCanvas.Handle,Ord(Align));
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddInteger(TGL_SetTextAlign,Ord(Align));
  {$ENDIF}
end;

procedure TTeeCanvas3D.MoveTo(X,Y:Integer);
begin
  {$IFDEF TEEPERF}
  Log('MoveTo');
  {$ENDIF}
  {$IFDEF D1}
  WinProcs.MoveToEx(FCanvas.Handle, X, Y, nil);
  {$ELSE}
  Windows.MoveToEx(FCanvas.Handle, X, Y, nil);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddMoveTo(x,y);
  {$ENDIF}
end;

procedure TTeeCanvas3D.LineTo(X,Y:Integer);
begin
  {$IFDEF TEEPERF}
  Log('LineTo');
  {$ENDIF}
  {$IFDEF D1}
  WinProcs.LineTo(FCanvas.Handle, X, Y);
  {$ELSE}
  Windows.LineTo(FCanvas.Handle, X, Y);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddLineTo(x,y);
  {$ENDIF}
end;

Procedure TTeeCanvas3D.DoRectangle(Const Rect:TRect);
begin
  {$IFDEF TEEPERF}
  Log('DoRectangle');
  {$ENDIF}
  With Rect do FCanvas.Rectangle(Left,Top,Right,Bottom);
  {$IFDEF TEEGRAPHLIST}
  GraphListAddRect(Rect);
  {$ENDIF}
end;

{ 3D Canvas }
procedure TTeeCanvas3D.PlaneWithZ(P1,P2,P3,P4:TPoint; Z:Integer);
Var Points:TFourPoints;
begin
  {$IFDEF TEEPERF}
  Log('PlaneWithZ');
  {$ENDIF}
  Calc3DTPoint(P1,Z);
  Calc3DTPoint(P2,Z);
  Calc3DTPoint(P3,Z);
  Calc3DTPoint(P4,Z);
  Points[0]:=P1;
  Points[1]:=P2;
  Points[2]:=P3;
  Points[3]:=P4;
  {$IFDEF D1}
  WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ELSE}
  Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddPoly(@Points,4);
  {$ENDIF}
end;

Procedure TTeeCanvas3D.Calc3DTPoint(Var P:TPoint; z:Integer);
begin
  {$IFDEF TEEPERF}
  Log('Calc3DTPoint');
  {$ENDIF}
  Calc3DPos(P.X,P.Y,Z);
end;

Function TTeeCanvas3D.Calc3DTPoint3D(Const P:TPoint3D):TPoint;
begin
  {$IFDEF TEEPERF}
  Log('Calc3DTPoint3D');
  {$ENDIF}
  Calc3DPoint(result,P.X,P.Y,P.Z);
end;

Function TTeeCanvas3D.Calculate3DPosition(x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}):TPoint;
begin
  Calc3DPos(x,y,z);
  result.x:=x;
  result.y:=y;
end;

Procedure TTeeCanvas3D.Calc3DPoint( Var P:TPoint;
                                    x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
begin
  {$IFDEF TEEPERF}
  Log('Calc3DPoint');
  {$ENDIF}
  Calc3DPos(x,y,z);
  P.x:=x;
  P.y:=y;
end;

Procedure TTeeCanvas3D.Calculate2DPosition(Var x,y:Integer; z:Integer);
var x1:Integer;
    tmp:Double;
begin
  if IZoomFactor<>0 then
  begin
    tmp:=1.0/IZoomFactor;
    if FIsOrthogonal then
    begin
      x:=Round((x-FXCenterOffset)*tmp)-z+FXCenter;
      y:=Round((y-FYCenterOffset)*tmp)+z+FYCenter;
    end
    else
    if FIs3D and (tempXX<>0) and (c2c3<>0)  then
    begin
      x1:=x;
      z:=z-FZCenter;
      x:=Round((((x1-FXCenterOffset)*tmp)-(z*tempXZ)-
                 (y -FYCenter)*c2s3)   / tempXX) + FXCenter;
      y:=Round((((y -FYCenterOffset)*tmp)-(z*tempYZ)-
                 (x1-FXCenter)*tempYX) / c2c3)   + FYCenter;
    end;
  end;
end;

Procedure TTeeCanvas3D.Calc3DPos(Var x,y:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; z:Integer);
var x1 : Integer;
    tmp: Double;
    y1 : Integer;
begin
  {$IFDEF TEEPERF}
  Log('Calc3DPos');
  {$ENDIF}
  if FIsOrthogonal then
  begin
    x:=Round( IZoomFactor*(x-FXCenter+z) )+FXCenterOffset;
    y:=Round( IZoomFactor*(y-FYCenter-z) )+FYCenterOffset;
  end
  else
  if FIs3D then
  begin
    Dec(z,FZCenter);
    x1:=x-FXCenter;
    y1:=y-FYCenter;
    if IPerspec>0 then
       tmp:=IZoomFactor*(1-((x1*c2s1 -y1*s2 + z*c2c1)*IPerspec))
    else
       tmp:=IZoomFactor;
    x:=Round((x1*tempXX + y1*c2s3 + z*tempXZ)*tmp)+FXCenterOffset;
    y:=Round((x1*tempYX + y1*c2c3 + z*tempYZ)*tmp)+FYCenterOffset;
  end;
end;

Function TTeeCanvas3D.GetHandle:HDC;
begin
  {$IFDEF TEEPERF}
  Log('GetHandle');
  {$ENDIF}
  result:=FCanvas.Handle;
end;

{  NONANTIALIASED_QUALITY = 3;
  ANTIALIASED_QUALITY = 4;}

Procedure TTeeCanvas3D.TextOut3D(x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; const Text:String);
var tmp            : Integer;
    OldSize        : Integer;
    tmpSizeChanged : Boolean;
    DC             : HDC;
    LogRec         : TLogFont;
    NewFont        : HFont;
    OldFont        : HFont;
begin
  {$IFDEF TEEPERF}
  Log('TextOut3D');
  {$ENDIF}

  Calc3DPos(x,y,z);
  if IZoomText then
  begin
    tmpSizeChanged:=False;
    DC:=0;
    OldFont:=0;
    if IZoomFactor<>1 then
    With FFont do
    begin
      OldSize:=Size;
      tmp:=MaxLong(1,Round(IZoomFactor*OldSize));
      if OldSize<>tmp then
      begin
        DC:=FCanvas.Handle;
        GetObject(FFont.Handle, SizeOf(LogRec), @LogRec);
        LogRec.lfHeight:= -MulDiv( tmp,FFont.PixelsPerInch,72);
        NewFont:=CreateFontIndirect(LogRec);
        OldFont:=SelectObject(DC,NewFont);
        tmpSizeChanged:=True;
      end;
    end;
    FCanvas.TextOut(X,Y,Text);
    if tmpSizeChanged then DeleteObject(SelectObject(DC,OldFont));
  end
  else FCanvas.TextOut(X,Y,Text);
  {$IFDEF TEEGRAPHLIST}
  GraphListAddTextOut(x,y,Text);
  {$ENDIF}
end;

procedure TTeeCanvas3D.MoveTo3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
begin
  {$IFDEF TEEPERF}
  Log('MoveTo3D');
  {$ENDIF}
  Calc3DPos(x,y,z);
  {$IFDEF D1}
  WinProcs.MoveToEx(FCanvas.Handle, X, Y, nil);
  {$ELSE}
  Windows.MoveToEx(FCanvas.Handle, X, Y, nil);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddMoveTo(x,y);
  {$ENDIF}
end;

procedure TTeeCanvas3D.LineTo3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
begin
  {$IFDEF TEEPERF}
  Log('LineTo3D');
  {$ENDIF}
  Calc3DPos(x,y,z);
  {$IFDEF D1}
  WinProcs.LineTo(FCanvas.Handle, X, Y);
  {$ELSE}
  Windows.LineTo(FCanvas.Handle, X, Y);
  {$ENDIF}
  FCanvas.LineTo(X,Y);
  {$IFDEF TEEGRAPHLIST}
  GraphListAddLineTo(x,y);
  {$ENDIF}
end;

Procedure TTeeCanvas3D.RectangleWithZ(Const Rect:TRect; Z:Integer);
var Points : TFourPoints;
begin
  {$IFDEF TEEPERF}
  Log('RectangleWithZ');
  {$ENDIF}
  With Rect do
  begin
    Calc3DPoint(Points[0],Left,Top,Z);
    Calc3DPoint(Points[1],Right,Top,Z);
    Calc3DPoint(Points[2],Right,Bottom,Z);
    Calc3DPoint(Points[3],Left,Bottom,Z);
  end;
  {$IFDEF D1}
  WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ELSE}
  Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddPoly(@Points,4);
  {$ENDIF}
end;

Procedure TTeeCanvas3D.DoHorizLine(X0,X1,Y:Integer);
var DC : HDC;
begin
  {$IFDEF TEEPERF}
  Log('DoHorizLine');
  {$ENDIF}
  DC:=FCanvas.Handle;
  {$IFDEF D1}
  WinProcs.MoveToEx(DC,X0,Y,nil);
  WinProcs.LineTo(DC,X1,Y);
  {$ELSE}
  Windows.MoveToEx(DC,X0,Y,nil);
  Windows.LineTo(DC,X1,Y);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddMoveTo(x0,y);
  GraphListAddLineTo(x1,y);
  {$ENDIF}
end;

Procedure TTeeCanvas3D.DoVertLine(X,Y0,Y1:Integer);
var DC : HDC;
begin
  {$IFDEF TEEPERF}
  Log('DoVertLine');
  {$ENDIF}
  DC:=FCanvas.Handle;
  MoveToEx(DC,X,Y0,nil);
  {$IFDEF D1}
  WinProcs.LineTo(DC,X,Y1);
  {$ELSE}
  Windows.LineTo(DC,X,Y1);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddMoveTo(x,y0);
  GraphListAddLineTo(x,y1);
  {$ENDIF}
end;

Procedure ClipCanvas(ACanvas:TCanvas; Const Rect:TRect);
Var P      : Array[0..1] of TPoint;
    Region : HRgn;
    DC     : HDC;
begin
  with Rect do
  begin
    p[0]:=TopLeft;
    p[1]:=BottomRight;
  end;
  DC:=ACanvas.Handle;
  LPToDP(DC,P,2);
  Region:=CreateRectRgn(P[0].X,P[0].Y,P[1].X,P[1].Y);
  SelectClipRgn(DC,Region);
  DeleteObject(Region);
end;

procedure TTeeCanvas3D.ClipRectangle(Const Rect:TRect);
begin
  {$IFDEF TEEPERF}
  Log('ClipRectangle');
  {$ENDIF}
  ClipCanvas(FCanvas,Rect);
end;

procedure TTeeCanvas3D.ClipCube(Const Rect:TRect; MinZ,MaxZ:Integer);
Var P      : Array[0..5] of TPoint;
    Region : HRgn;
    DC     : HDC;
    tmpR   : TRect;
    pa     : TPoint;
    pb     : TPoint;
begin
  {$IFDEF TEEPERF}
  Log('ClipCube');
  {$ENDIF}
  if FIs3D then
  With Rect do
  begin
    Calc3DPoint(p[0],Left,Bottom,MinZ);
    Calc3DPoint(p[1],Left,Top,MinZ);

    Calc3DPoint(pa,Left,Top,MaxZ);
    Calc3DPoint(pb,Right,Top,MinZ);

    if pb.Y<pa.Y then p[2]:=pb else p[2]:=pa;

    Calc3DPoint(p[3],Right,Top,MaxZ);

    Calc3DPoint(pa,Right,Bottom,MaxZ);
    Calc3DPoint(pb,Right,Top,MinZ);
    if pb.x>pa.x then p[4]:=pb else p[4]:=pa;

    Calc3DPoint(p[5],Right,Bottom,MinZ);
    DC:=FCanvas.Handle;
    LPToDP(DC,P,6);
    Region:=CreatePolygonRgn(P,6,ALTERNATE);
    SelectClipRgn(DC,Region);
    DeleteObject(Region);
  end
  else
  begin
    tmpR:=Rect;
    Inc(tmpR.Left);
    Inc(tmpR.Top);
    Dec(tmpR.Bottom);
    ClipRectangle(tmpR);
  end;
end;

Procedure UnClipCanvas(ACanvas:TCanvas);
{$IFDEF D1}
Var Region : HRgn;
    DC     : HDC;
{$ENDIF}
begin
  {$IFDEF D1}
  DC:=ACanvas.Handle;
  Region:=CreateRectRgn( 0,0,
                         GetDeviceCaps(DC,HORZRES),
                         GetDeviceCaps(DC,VERTRES) );
  SelectClipRgn(DC,Region);
  DeleteObject(Region);
  {$ELSE}
  SelectClipRgn(ACanvas.Handle,0);
  {$ENDIF}
end;

procedure TTeeCanvas3D.UnClipRectangle;
begin
  {$IFDEF TEEPERF}
  Log('UnClipRectangle');
  {$ENDIF}
  UnClipCanvas(FCanvas);
end;

Procedure TTeeCanvas3D.Projection(MaxDepth:Integer; const Bounds,Rect:TRect);
begin
  {$IFDEF TEEPERF}
  Log('Projection');
  {$ENDIF}
  RectCenter(Rect,FXCenter,FYCenter);
  FZCenter      :=MaxDepth div 2;
  FXCenterOffset:=FXCenter;
  FYCenterOffset:=FYCenter;
  if Assigned(F3DOptions) then
  With F3DOptions do
  begin
    Inc(FXCenterOffset,HorizOffset);
    Inc(FYCenterOffset,VertOffset);
    if Perspective>0 then IPerspec:=Perspective/35000.0;
  end;
end;

Function TTeeCanvas3D.InitWindow( DestCanvas:TCanvas;
                                  A3DOptions:TView3DOptions;
                                  ABackColor:TColor;
                                  Is3D:Boolean;
                                  Const UserRect:TRect):TRect;
Procedure CalcTrigValues;
Var c1:Extended;
    c2:Extended;
    c3:Extended;
    s1:Extended;
    s3:Extended;
    rx:Double;
    ry:Double;
    rz:Double;
begin
  {$IFDEF TEEPERF}
  Log('CalcTrigValues');
  {$ENDIF}
  if not FIsOrthogonal then
  begin
    if Assigned(F3DOptions) then
    With F3DOptions do
    begin
      rx:=Rotation;
      ry:=Elevation;
      rz:=Tilt;
    end
    else
    begin
      rx:=0;
      ry:=0;
      rz:=0;
    end;

    IPerspec:=0;

    SinCos(rx*TeePiStep,s1,c1);
    SinCos(ry*TeePiStep,s2,c2);
    SinCos(rz*TeePiStep,s3,c3);

    c2s3:=c2*s3;
    c2c3:=MaxDouble(1E-5,c2*c3);

    tempXX:=MaxDouble(1E-5, s1*s2*s3 + c1*c3 );
    tempYX:=( c3*s1*s2 - c1*s3 );

    tempXZ:=( c1*s2*s3 - c3*s1 );
    tempYZ:=( c1*c3*s2 + s1*s3 );

    c2s1:=c2*s1;
    c2c1:=c1*c2;
  end;
end;

var tmpH:Longint;
    tmpW:Longint;
    tmpCanvas:TCanvas;
begin
  {$IFDEF TEEPERF}
  Log('InitWindow');
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListInit;
  {$ENDIF}
  FBounds:=UserRect;
  F3DOptions:=A3DOptions;

  FIs3D:=Is3D;
  FIsOrthogonal:=False;
  IZoomFactor:=1;
  if FIs3D then
  begin
    if Assigned(F3DOptions) then
    begin
      FIsOrthogonal:=F3DOptions.Orthogonal;
      IZoomFactor:=0.01*F3DOptions.Zoom;
      IZoomText:=F3DOptions.ZoomText;
    end;
    CalcTrigValues;
  end;

  if FBufferedDisplay then
  begin
    RectSize(UserRect,tmpW,tmpH);
    if Assigned(FBitmap) and
       ((FBitmap.Width<>tmpW) or (FBitmap.Height<>tmpH)) then
    begin
      FBitmap.Free;
      FBitmap:=nil;
    end;
    if not Assigned(FBitmap) then
    begin
      FBitmap:=TBitMap.Create;
      FBitmap.Monochrome:=FMonochrome;
      FBitmap.Width :=tmpW;
      FBitmap.Height:=tmpH;
    end;
    tmpCanvas:=FBitmap.Canvas;
    tmpCanvas.OnChange:=nil;
    tmpCanvas.OnChanging:=nil;
    SetCanvas(tmpCanvas);
    result:=Rect(0,0,tmpW,tmpH);
  end
  else
  begin
    SetCanvas(DestCanvas);
    result:=UserRect;
  end;
end;

{$IFDEF TEEUSEDRAWDIB}
type HDRAWDIB=Longint;

Const vfw='msvfw32.dll';
      DDF_JUSTDRAWIT=$80;
      DDF_DONTDRAW=$10;
      DDF_HURRYUP=$800;

function DrawDibOpen:HDrawDib; external vfw name 'DrawDibOpen';
function DrawDibClose(hdd:HDrawDib):Bool; external vfw name 'DrawDibClose';
function DrawDibDraw(hdd:HDRAWDIB; dc:HDC; x,y,dx,dy:Integer;
                                   bpi:PBITMAPINFOHEADER;
                                   Bits:Pointer;
                                   xs,ys,dxs,dys:Integer;
                                   Flags:UINT):Bool;
                                   external vfw name 'DrawDibDraw';

var hdd:HDRAWDIB;
{$ENDIF}


Procedure TTeeCanvas3D.TransferBitmap(ALeft,ATop:Integer; ACanvas:TCanvas);
{$IFDEF TEEUSEDRAWDIB}
var ColorBitsSize,ColorInfoSize: DWORD;
    ColorInfo, ColorBits: Pointer;
{$ENDIF}
begin
  {$IFDEF TEEPERF}
  Log('TransferBitmap');
  {$ENDIF}
{$IFDEF MONITOR_BUFFERREDRAWS}
  Inc(RedrawCountBuffer);
  FBitmap.Canvas.TextOut(0,20,IntToStr(RedrawCountBuffer));
{$ENDIF}
{$IFDEF TEEUSEDRAWDIB}
  GetDIBSizes(FBitmap.Handle, ColorInfoSize, ColorBitsSize);
  GetMem(ColorInfo,ColorInfoSize);
  GetMem(ColorBits,ColorBitsSize);
  GetDIB(FBitmap.Handle, 0, ColorInfo^, ColorBits^);
  DrawDibDraw(hdd,ACanvas.Handle,ALeft,ATop,FBitmap.Width,
          FBitmap.Height,ColorInfo,ColorBits,0,0,FBitmap.Width,FBitmap.Height,
          0);
  FreeMem(ColorInfo,ColorInfoSize);
  FreeMem(ColorBits,ColorBitsSize);
{$ELSE}
  BitBlt( ACanvas.Handle,ALeft,ATop,
          FBitmap.Width,
          FBitmap.Height,
          FBitmap.Canvas.Handle,0,0,SRCCOPY);
{$ENDIF}
end;

Function TTeeCanvas3D.ReDrawBitmap:Boolean;
begin
  {$IFDEF TEEPERF}
  Log('RedrawBitmap');
  {$ENDIF}
  result:=not FDirty;
  if result then TransferBitmap(0,0,FCanvas);
end;

Procedure TTeeCanvas3D.ShowImage(DestCanvas,DefaultCanvas:TCanvas; Const UserRect:TRect);
begin
  {$IFDEF TEEPERF}
  Log('ShowImage');
  {$ENDIF}
  if FBufferedDisplay then
  begin
    With UserRect do TransferBitmap(Left,Top,DestCanvas);
    FDirty:=False;
  end;
  SetCanvas(DefaultCanvas);
end;

procedure TTeeCanvas3D.StretchDraw(const Rect: TRect; Graphic: TGraphic);
begin
  {$IFDEF TEEPERF}
  Log('StretchDraw');
  {$ENDIF}
  FCanvas.StretchDraw(Rect,Graphic);
end;

procedure TTeeCanvas3D.Draw(X, Y: Integer; Graphic: TGraphic);
begin
  {$IFDEF TEEPERF}
  Log('Draw');
  {$ENDIF}
  FCanvas.Draw(X,Y,Graphic);
end;

Procedure TTeeCanvas3D.GradientFill( Const Rect : TRect;
                                     StartColor : TColor;
                                     EndColor   : TColor;
                                     Direction  : TGradientDirection);

Var Trgb     : Array[0..2] of Integer;
    Drgb     : Array[0..2] of Integer;
    tmpBrush : HBRUSH;
    DC       : HDC;
    OldColor : TColor;

  Procedure CalcBrushColor(Index,Range:Integer);
  var tmp:TColor;
  begin
    tmp:=RGB( Trgb[0] + MulDiv(Index,Drgb[0],Range),
                                Trgb[1] + MulDiv(Index,Drgb[1],Range),
                                Trgb[2] + MulDiv(Index,Drgb[2],Range));
    if tmp<>OldColor then
    begin
      if tmpBrush<>0 then DeleteObject(SelectObject(DC,tmpBrush));
      tmpBrush:=SelectObject(DC,CreateSolidBrush(tmp));
      OldColor:=tmp;
    end;
  end;

Var Size     : Longint;
    Steps    : Longint;
    tmpRect  : TRect;
    SizeX    : Longint;
    SizeY    : Longint;
    XCenter,
    YCenter,
    tmp1,
    tmp2,
    P0,
    P1,
    P2,
    P3       : Integer;


  Procedure CleanBrushes;
  begin
    if tmpBrush<>0 then DeleteObject(SelectObject(DC,tmpBrush));
  end;

  Procedure RectGradient(Horizontal:Boolean);
  var t : Integer;
  begin
    Steps:=Size;
    if Steps>256 then Steps:=256;

    DC:=FCanvas.Handle;

    With Rect do
    begin
      if Horizontal then P3:=Bottom-Top
                    else P3:=Right-Left;
      P1:=MulDiv(0,Size,Steps);
      OldColor:=-1;
      for t:=0 to Steps-1 do
      Begin
        CalcBrushColor(t,Pred(Steps));
        P2:=MulDiv(t+1,Size,Steps);
        if Horizontal then
        Begin
          P0:=Right-P1;
          PatBlt(DC,P0,Top,Right-P2-P0,P3,PATCOPY);
        end
        Else
        Begin
          P0:=Bottom-P1;
          PatBlt(DC,Left,P0,P3,Bottom-P2-P0,PATCOPY);
        end;
        P1:=P2;
      end;
    end;
  end;

var tmpLeft : Integer;
    tmpTop  : Integer;
    tmp3    : Integer;
    tmpDiagonal: Integer;
    FromTop : Boolean;
Begin
  {$IFDEF TEEPERF}
  Log('GradientFill');
  {$ENDIF}
  tmpRect:=Rect;
  With tmpRect do
  begin
    if Right<Left then SwapInteger(Left,Right);
    if Bottom<Top then SwapInteger(Top,Bottom);
  end;

  Trgb[0]:=GetRValue(StartColor);
  Trgb[1]:=GetGValue(StartColor);
  Trgb[2]:=GetBValue(StartColor);
  Drgb[0]:=GetRValue(EndColor)-Trgb[0];
  Drgb[1]:=GetGValue(EndColor)-Trgb[1];
  Drgb[2]:=GetBValue(EndColor)-Trgb[2];

  P0:=0;
  P1:=0;
  P2:=0;
  P3:=0;

  tmpBrush:=0;
  OldColor:=-1;
  with tmpRect do
  Case Direction of
    gdLeftRight,
    gdRightLeft: begin
                   Size:=Right-Left;
                   P1:=Top;
                   P3:=Bottom-Top;
                   RectGradient(True);
                 end;
    gdTopBottom,
    gdBottomTop: begin
                   Size:=Bottom-Top;
                   P0:=Left;
                   P2:=Right-Left;
                   RectGradient(False);
                 end;
  else
  begin
    RectSize(tmpRect,SizeX,SizeY);
    Case Direction of
    gdFromTopLeft,
    gdFromBottomLeft:
      begin
        FromTop:=Direction=gdFromTopLeft;
        if FromTop then P1:=Top else P1:=Bottom;
        P0:=P1;
        tmpDiagonal:=Round(Sqrt(Sqr(SizeX)+Sqr(SizeY)));
        DC:=FCanvas.Handle;
        tmp1:=0;
        tmp2:=0;
        Repeat
          CalcBrushColor(tmp2,tmpDiagonal);
          PatBlt(DC,Left,P0,tmp2+1,1,PATCOPY);
          PatBlt(DC,Left+tmp2,P0,1,P1-P0,PATCOPY);
          if tmp1<SizeY then
          begin
            Inc(tmp1);
            if FromTop then Inc(P0) else Dec(P0);
          end;
          if tmp2<SizeX then Inc(tmp2);
        Until (tmp1>=SizeY) and (tmp2>=SizeX);
      end;
    gdFromCenter:
      begin
        XCenter:=SizeX shr 1;
        YCenter:=SizeY shr 1;
        tmp1:=0;
        tmp2:=0;
        tmp3:=XCenter+YCenter;
        DC:=FCanvas.Handle;
        Repeat
          CalcBrushColor((tmp1+tmp2), tmp3);
          P0:=SizeY-(2*tmp1);
          P1:=SizeX-(2*tmp2);
          tmpLeft:=Left+tmp2;
          tmpTop:=Top+tmp1;
          PatBlt(DC,tmpLeft,tmpTop,TeeGradientPrecision,P0,PATCOPY);
          PatBlt(DC,Right-tmp2-1,tmpTop,TeeGradientPrecision,P0,PATCOPY);
          PatBlt(DC,tmpLeft,tmpTop,P1,TeeGradientPrecision,PATCOPY);
          PatBlt(DC,tmpLeft,Bottom-tmp1-TeeGradientPrecision,
                    P1,TeeGradientPrecision,PATCOPY);
          if tmp1<YCenter then Inc(tmp1,TeeGradientPrecision);
          if tmp2<XCenter then Inc(tmp2,TeeGradientPrecision);
        Until (tmp1>=YCenter) and (tmp2>=XCenter);
      end;
    end;
  end;
  end;
  CleanBrushes;
end;

procedure TTeeCanvas3D.EraseBackground(const Rect: TRect);
begin
  {$IFDEF TEEPERF}
  Log('EraseBackGround');
  {$ENDIF}
  {$IFDEF D1}
  WinProcs.FillRect(FCanvas.Handle, Rect, FBrush.Handle);
  {$ELSE}
  Windows.FillRect(FCanvas.Handle, Rect, FBrush.Handle);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddRect(Rect);
  {$ENDIF}
end;

procedure TTeeCanvas3D.FillRect(const Rect: TRect);
begin
  {$IFDEF TEEPERF}
  Log('FillRect');
  {$ENDIF}
  {$IFDEF D1}
  WinProcs.FillRect(FCanvas.Handle, Rect, FBrush.Handle);
  {$ELSE}
  Windows.FillRect(FCanvas.Handle, Rect, FBrush.Handle);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddRect(Rect);
  {$ENDIF}
end;

procedure TTeeCanvas3D.Frame3D( Rect: TRect; TopColor,BottomColor: TColor;
                                  Width: Integer);
var TopRight   : TPoint;
    BottomLeft : TPoint;
begin
  {$IFDEF TEEPERF}
  Log('Frame3D');
  {$ENDIF}
  FPen.Width := 1;
  Dec(Rect.Bottom);
  Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    with Rect do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      FPen.Color := TopColor;
      FCanvas.PolyLine([BottomLeft, TopLeft, TopRight]);
      FPen.Color := BottomColor;
      Dec(BottomLeft.X);
      FCanvas.PolyLine([TopRight, BottomRight, BottomLeft]);
    end;
    InflateRect(Rect, -1, -1);
  end;
end;

Function ApplyDark(Color:TColor; HowMuch:Byte):TColor;
Var r : Byte;
    g : Byte;
    b : Byte;
Begin
  {$IFDEF TEEPERF}
  Log('ApplyDark');
  {$ENDIF}
  Color:=ColorToRGB(Color);

  r:=Lo(Color);
  g:=Hi(Color);
  b:=Lo(Color shr 16);
{  r:=GetRValue(Color);
  g:=GetGValue(Color);
  b:=GetBValue(Color);}

  if r>HowMuch then r:=r-HowMuch else r:=0;
  if g>HowMuch then g:=g-HowMuch else g:=0;
  if b>HowMuch then b:=b-HowMuch else b:=0;
  result:=RGB(r,g,b);
end;

Function ApplyBright(Color:TColor; HowMuch:Byte):TColor;
Var r : Byte;
    g : Byte;
    b : Byte;
Begin
  {$IFDEF TEEPERF}
  Log('ApplyBright');
  {$ENDIF}
  Color:=ColorToRGB(Color);

  r:=Lo(Color);
  g:=Hi(Color);
  b:=Lo(Color shr 16);
{  r:=GetRValue(Color);
  g:=GetGValue(Color);
  b:=GetBValue(Color);}

  if (r+HowMuch)<256 then r:=r+HowMuch else r:=255;
  if (g+HowMuch)<256 then g:=g+HowMuch else g:=255;
  if (b+HowMuch)<256 then b:=b+HowMuch else b:=255;
  result:=RGB(r,g,b);
end;

Procedure TTeeCanvas3D.Cube(Left,Right,Top,Bottom,Z0,Z1:Integer; DarkSides:Boolean);
Var OldColor : TColor;
    tmpSolid : Boolean;

  Procedure Dark(Quantity:Byte);
  begin
    if tmpSolid then FBrush.Color:=ApplyDark(OldColor,Quantity)
                else BackColor:=ApplyDark(OldColor,Quantity);
  end;

var P0,P1,P2,P3 : TPoint;
    Points      : TFourPoints;
begin
  {$IFDEF TEEPERF}
  Log('Cube');
  {$ENDIF}
  Calc3DPoint(P0,Left,Top,z0);
  Calc3DPoint(P1,Right,Top,z0);
  Calc3DPoint(P2,Right,Bottom,z0);
  Calc3DPoint(P3,Right,Top,z1);

  tmpSolid:=FBrush.Style=bsSolid;
  if tmpSolid then OldColor:=FBrush.Color
              else OldColor:=BackColor;

  Points[0]:=P0;
  Points[1]:=P1;
  Points[2]:=P2;

  Calc3DPoint(Points[3],Left,Bottom,z0);

  {$IFDEF D1}
  WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ELSE}
  Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ENDIF}

  Points[0]:=P1;
  Points[1]:=P3;
  Calc3DPoint(Points[2],Right,Bottom,z1);
  Points[3]:=P2;

  if points[2].x>p2.x then
  begin
    if DarkSides then Dark(DarkerColorQuantity);
    {$IFDEF D1}
    WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
    {$ELSE}
    Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
    {$ENDIF}
  end
  else
  begin
    Points[0]:=P0;
    Calc3DPoint(Points[1],Left,Top,z1);
    Calc3DPoint(Points[2],Left,Bottom,z1);
    Calc3DPoint(Points[3],Left,Bottom,z0);

    if DarkSides then Dark(DarkerColorQuantity);
    {$IFDEF D1}
    WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
    {$ELSE}
    Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
    {$ENDIF}
  end;

  Points[0]:=P0;
  Points[1]:=P1;
  Points[2]:=P3;
  Calc3DPoint(Points[3],Left,Top,z1);

  if p1.y>points[3].y then
  begin
    if DarkSides then Dark(DarkColorQuantity);
    {$IFDEF D1}
    WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
    {$ELSE}
    Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
    {$ENDIF}
  end;
end;

Procedure TTeeCanvas3D.RectangleZ(Left,Top,Bottom,Z0,Z1:Integer);
var Points : TFourPoints;
begin
  {$IFDEF TEEPERF}
  Log('RectangleZ');
  {$ENDIF}
  Calc3DPoint(Points[0],Left,Top,Z0);
  Calc3DPoint(Points[1],Left,Top,Z1);
  Calc3DPoint(Points[2],Left,Bottom,Z1);
  Calc3DPoint(Points[3],Left,Bottom,Z0);
  {$IFDEF D1}
  WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ELSE}
  Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddPoly(@Points,4);
  {$ENDIF}
end;

Procedure TTeeCanvas3D.RectangleY(Left,Top,Right,Z0,Z1:Integer);
var Points : TFourPoints;
begin
  {$IFDEF TEEPERF}
  Log('RectangleY');
  {$ENDIF}
  Calc3DPoint(Points[0],Left,Top,Z0);
  Calc3DPoint(Points[1],Right,Top,Z0);
  Calc3DPoint(Points[2],Right,Top,Z1);
  Calc3DPoint(Points[3],Left,Top,Z1);
  {$IFDEF D1}
  WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ELSE}
  Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddPoly(@Points,4);
  {$ENDIF}
end;

procedure TTeeCanvas3D.FrontPlaneBegin;
begin
  {$IFDEF TEEPERF}
  Log('FrontPlaneBegin');
  {$ENDIF}
  FWas3D:=FIs3D;
  FIs3D:=False;
end;

procedure TTeeCanvas3D.FrontPlaneEnd;
begin
  {$IFDEF TEEPERF}
  Log('FrontPlaneEnd');
  {$ENDIF}
  FIs3D:=FWas3D;
end;

Procedure TTeeCanvas3D.Invalidate;
begin
  {$IFDEF TEEPERF}
  Log('Invalidate');
  {$ENDIF}
  FDirty:=True;
end;

procedure TTeeCanvas3D.RotateLabel3D(x,y,z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; Const St:String; RotDegree:Integer);
begin
  {$IFDEF TEEPERF}
  Log('RotateLabel3D');
  {$ENDIF}
  Calc3DPos(x,y,z);
  RotateLabel(x,y,St,RotDegree);
end;

procedure TTeeCanvas3D.RotateLabel(x,y:Integer; Const St:String; RotDegree:Integer);
var OldFont: HFONT;
    NewFont: HFONT;
    LogRec : TLOGFONT;
    DC     : HDC;
begin
  {$IFDEF TEEPERF}
  Log('RotateLabel');
  {$ENDIF}
  if RotDegree>360 then RotDegree:=RotDegree-360;
  FBrush.Style := bsClear;
  DC:=FCanvas.Handle;
  GetObject(FFont.Handle, SizeOf(LogRec), @LogRec);
  LogRec.lfEscapement   := RotDegree*10;
  LogRec.lfOrientation  := RotDegree*10; { <-- fix, was zero }
  LogRec.lfOutPrecision := OUT_TT_ONLY_PRECIS;
  if IZoomText then
  if IZoomFactor<>1 then
     LogRec.lfHeight    := -MulDiv( MaxLong(1,Round(IZoomFactor*FFont.Size)),
                            FFont.PixelsPerInch, 72);
  NewFont := CreateFontIndirect(LogRec);
  OldFont := SelectObject(DC,NewFont);
  {$IFDEF D1}
  WinProcs.TextOut(DC,X,Y,@St[1],Length(St));
  {$ELSE}
  Windows.TextOut(DC,X,Y,@St[1],Length(St));
  {$ENDIF}
  DeleteObject(SelectObject(DC,OldFont));
  {$IFDEF TEEGRAPHLIST}
  GraphListAddTextOut(x,y,St);
  {$ENDIF}
end;

Procedure TTeeCanvas3D.Line(X0,Y0,X1,Y1:Integer);
var DC : HDC;
begin
  {$IFDEF TEEPERF}
  Log('Line');
  {$ENDIF}
  DC:=FCanvas.Handle;
  {$IFDEF D1}
  WinProcs.MoveToEx(DC,X0,Y0,nil);
  WinProcs.LineTo(DC,X1,Y1);
  {$ELSE}
  Windows.MoveToEx(DC,X0,Y0,nil);
  Windows.LineTo(DC,X1,Y1);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddMoveTo(x0,y0);
  GraphListAddLineTo(x1,y1);
  {$ENDIF}
end;

Procedure TTeeCanvas3D.LineWithZ(X0,Y0,X1,Y1,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
var DC : HDC;
begin
  {$IFDEF TEEPERF}
  Log('LineWithZ');
  {$ENDIF}
  Calc3DPos(x0,y0,z);
  Calc3DPos(x1,y1,z);
  DC:=FCanvas.Handle;
  {$IFDEF D1}
  WinProcs.MoveToEx(DC,X0,Y0,nil);
  WinProcs.LineTo(DC,X1,Y1);
  {$ELSE}
  Windows.MoveToEx(DC,X0,Y0,nil);
  Windows.LineTo(DC,X1,Y1);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddMoveTo(x0,y0);
  GraphListAddLineTo(x1,y1);
  {$ENDIF}
end;

Procedure TTeeCanvas3D.Polygon(const Points: array of TPoint);
Begin
  {$IFDEF TEEPERF}
  Log('Polygon');
  {$ENDIF}
  {$IFDEF D1}
  WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, High(Points) + 1);
  {$ELSE}
  Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, High(Points) + 1);
  {$ENDIF}
  {$IFDEF TEEGRAPHLIST}
  GraphListAddPoly(@Points,High(Points)+1);
  {$ENDIF}
end;

procedure TTeeCanvas3D.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  {$IFDEF TEEPERF}
  Log('Ellipse');
  {$ENDIF}
  FCanvas.Ellipse(X1,Y1,X2,Y2);
end;

procedure TTeeCanvas3D.EllipseWithZ(X1, Y1, X2, Y2, Z: {$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
Const PiStep=Pi*0.1;
      NumCirclePoints=32;
Var P       : Array[0..NumCirclePoints-1] of TPoint;
    Points  : Array[0..2] of TPoint;
    PCenter : TPoint;
    t       : Integer;
    XRadius : Integer;
    YRadius : Integer;
    tmpSin  : Extended;
    tmpCos  : Extended;
    Old     : TPenStyle;
    DC      : HDC;
begin
  {$IFDEF TEEPERF}
  Log('EllipseWithZ');
  {$ENDIF}
  if FIsOrthogonal then
  begin
    Calc3DPos(X1,Y1,Z);
    Calc3DPos(X2,Y2,Z);
    FCanvas.Ellipse(X1,Y1,X2,Y2);
  end
  else
  if FIs3D then
  begin
    PCenter.X:=(X2+X1) div 2;
    PCenter.Y:=(Y2+Y1) div 2;
    XRadius:=(X2-X1) div 2;
    YRadius:=(Y2-Y1) div 2;
    Calc3DPoint(P[0],PCenter.X,Y2,Z);
    for t:=1 to NumCirclePoints-1 do
    begin
      SinCos(t*piStep,tmpSin,tmpCos);
      Calc3DPoint(P[t],PCenter.X+Trunc(XRadius*tmpSin),PCenter.Y+Trunc(YRadius*tmpCos),Z);
    end;
    if FBrush.Style<>bsClear then
    begin
      Old:=FPen.Style;
      FPen.Style:=psClear;
      Calc3DTPoint(PCenter,{PCenter.X,PCenter.Y,}Z);
      Points[0]:=PCenter;
      Points[1]:=P[0];
      Points[2]:=P[1];
      DC:=Handle;
      {$IFDEF D1}
      WinProcs.Polygon(DC, PPoints(@Points)^, 3);
      {$ELSE}
      Windows.Polygon(DC, PPoints(@Points)^, 3);
      {$ENDIF}
      Points[1]:=P[1];
      for t:=2 to NumCirclePoints-1 do
      begin
        Points[2]:=P[t];
        {$IFDEF D1}
        WinProcs.Polygon(DC, PPoints(@Points)^, 3);
        {$ELSE}
        Windows.Polygon(DC, PPoints(@Points)^, 3);
        {$ENDIF}
        Points[1]:=P[t];
      end;
      FPen.Style:=Old;
    end;
    if FPen.Style<>psClear then FCanvas.PolyLine(P);
  end
  else FCanvas.Ellipse(X1,Y1,X2+1,Y2+1);
end;

procedure TTeeCanvas3D.SetPixel(X, Y: Integer; Value: TColor);
begin
  {$IFDEF TEEPERF}
  Log('SetPixel');
  {$ENDIF}
  FCanvas.Pixels[X,Y]:=Value;
end;

procedure TTeeCanvas3D.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  {$IFDEF TEEPERF}
  Log('Arc');
  {$ENDIF}
  FCanvas.Arc(X1,Y1,X2,Y2,X3,Y3,X4,Y4);
end;

procedure TTeeCanvas3D.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  {$IFDEF TEEPERF}
  Log('Pie');
  {$ENDIF}
  FCanvas.Pie(X1,Y1,X2,Y2,X3,Y3,X4,Y4);
end;

procedure TTeeCanvas3D.Pie3D( XCenter,YCenter,XRadius,YRadius,Z0,Z1:Integer;
                              Const StartAngle,EndAngle:Double;
                              DarkSides,DrawSides:Boolean);
Var OldColor : TColor;

  Procedure Dark(Quantity:Byte);
  begin
    if FBrush.Style=bsSolid then
       FBrush.Color:=ApplyDark(OldColor,Quantity)
    else
       BackColor:=ApplyDark(OldColor,Quantity);
  end;

Const MaxCircleSteps=32;
var Points      : Array[0..MaxCircleSteps] of TPoint;
    Points3D    : Array[1..2*MaxCircleSteps] of TPoint;
    Start3D,
    End3D,
    CircleSteps : Integer;

  Procedure Draw3DPie;
  var t,tt:Integer;
  begin
    if DarkSides then Dark(32);
    if (Start3D=1) and (End3D=CircleSteps) then
    begin
      for t:=1 to CircleSteps do Points3D[t]:=Points[t];
      tt:=CircleSteps;
    end
    else
    begin
      tt:=0;
      for t:=Start3D to End3D do
      begin
        inc(tt);
        Points3D[tt]:=Points[t];
        Points3D[End3D-Start3D+1+tt]:=Points3D[2*CircleSteps-End3D+tt];
      end;
    end;
    {$IFDEF D1}
    WinProcs.Polygon(FCanvas.Handle, PPoints(@Points3D)^, 2*tt);
    {$ELSE}
    Windows.Polygon(FCanvas.Handle, PPoints(@Points3D)^, 2*tt);
    {$ENDIF}
  end;

Var tmpAngle,
    tmpSin,
    tmpCos  : Extended;
    Step    : Double;
    Started,
    Ended   : Boolean;
    t,
    tt,
    tmpX,
    tmpY    : Integer;
begin
  {$IFDEF TEEPERF}
  Log('Pie3D');
  {$ENDIF}
  CircleSteps:=32;
  Calc3DPoint(Points[0],XCenter,YCenter,Z1);
  Step:=(EndAngle-StartAngle)/(CircleSteps-1);
  tmpAngle:=StartAngle;
  for t:=1 to CircleSteps do
  begin
    SinCos(tmpAngle,tmpSin,tmpCos);
    tmpX:=XCenter+Round(XRadius*tmpCos);
    tmpY:=YCenter-Round(YRadius*tmpSin);
    Calc3DPoint(Points[t],tmpX,tmpY,Z1);
    Calc3DPoint(Points3D[2*CircleSteps+1-t],tmpX,tmpY,Z0);
    tmpAngle:=tmpAngle+Step;
  end;

  if FBrush.Style=bsSolid then OldColor:=FBrush.Color
                          else OldColor:=BackColor;

  { side }
  if DrawSides then
  begin
    if Points[CircleSteps].X<XCenter then
    begin
      Points3D[1]:=Points[0];
      Points3D[2]:=Points[CircleSteps];
      Points3D[3]:=Points3D[CircleSteps+1];
      Calc3DPoint(Points3D[4],XCenter,YCenter,Z0);
      if DarkSides then Dark(32);
      {$IFDEF D1}
      WinProcs.Polygon(FCanvas.Handle, PPoints(@Points3D)^, 4);
      {$ELSE}
      Windows.Polygon(FCanvas.Handle, PPoints(@Points3D)^, 4);
      {$ENDIF}
    end;

    { side }
    if Points[1].X>XCenter then
    begin
      Points3D[1]:=Points[0];
      Points3D[2]:=Points[1];
      Points3D[3]:=Points3D[2*CircleSteps];
      Calc3DPoint(Points3D[4],XCenter,YCenter,Z0);
      if DarkSides then Dark(32);
      {$IFDEF D1}
      WinProcs.Polygon(FCanvas.Handle, PPoints(@Points3D)^, 4);
      {$ELSE}
      Windows.Polygon(FCanvas.Handle, PPoints(@Points3D)^, 4);
      {$ENDIF}
    end;
  end;

  { 2d pie }
  if FBrush.Style=bsSolid then FBrush.Color:=OldColor
                          else BackColor:=OldColor;
  {$IFDEF D1}
  WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, CircleSteps+1);
  {$ELSE}
  Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, CircleSteps+1);
  {$ENDIF}

  { 3d pie }
  Ended:=False;
  Start3D:=0;
  End3D:=0;
  for t:=2 to CircleSteps do
  begin
    if Points[t].X>Points[t-1].X then
    begin
      Start3D:=t-1;
      Started:=True;
      for tt:=t+1 to CircleSteps-1 do
      if Points[tt+1].X<Points[tt].X then
      begin
        End3D:=tt;
        Ended:=True;
        Break;
      end;
      if (not Ended) and (Points[CircleSteps].X>=Points[CircleSteps-1].X) then
      begin
        End3D:=CircleSteps;
        Ended:=True;
      end;
      if Started and Ended then Draw3DPie;
      if End3D<>CircleSteps then
      if Points[CircleSteps].X>Points[CircleSteps-1].X then
      begin
        End3D:=CircleSteps;
        tt:=CircleSteps-1;
        While (Points[tt].X>Points[tt-1].X) do
        begin
          Dec(tt);
          if tt=1 then break;
        end;
        if tt>1 then
        begin
          Start3D:=tt;
          Draw3DPie;
        end;
      end;
      Break;
    end;
  end;
end;

procedure TTeeCanvas3D.Plane3D(Const A,B:TPoint; Z0,Z1:Integer);
var Points : TFourPoints;
begin
  {$IFDEF TEEPERF}
  Log('Plane3D');
  {$ENDIF}
  Calc3DPoint(Points[0],A.X,A.Y,Z0);
  Calc3DPoint(Points[1],B.X,B.Y,Z0);
  Calc3DPoint(Points[2],B.X,B.Y,Z1);
  Calc3DPoint(Points[3],A.X,A.Y,Z1);
  {$IFDEF D1}
  WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ELSE}
  Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ENDIF}
end;

procedure TTeeCanvas3D.Cylinder(Vertical:Boolean; Left,Top,Right,Bottom,Z0,Z1:Integer; Dark3D:Boolean);
Const NumCylinderSides=16;
      Step=2.0*pi/NumCylinderSides;
      StepColor=256 div NumCylinderSides;

Var OldColor : TColor;

  Procedure Dark(Quantity:Byte);
  begin
    if FBrush.Style=bsSolid then FBrush.Color:=ApplyDark(OldColor,Quantity)
                            else BackColor:=ApplyDark(OldColor,Quantity)
  end;

var tmpSize,
    tmpMid,
    tmpMidZ,
    Radius,
    ZRadius,
    t,
    NumSide :Longint;
    Poly    :Array[1..NumCylinderSides] of TPoint3D;
    tmpPoly :Array[1..NumCylinderSides] of TPoint;
    tmpSin,
    tmpCos  :Extended;
Begin
  {$IFDEF TEEPERF}
  Log('Cylinder');
  {$ENDIF}
  if FBrush.Style=bsSolid then OldColor:=FBrush.Color
                          else OldColor:=BackColor;
  ZRadius:=(Z1-Z0) shr 1;
  tmpMidZ:=(Z1+Z0) shr 1;
  if Vertical then
  begin
    Radius:=(Right-Left) shr 1;
    tmpMid:=(Right+Left) shr 1;
    for t:=1 to NumCylinderSides do
    begin
      SinCos((t-4)*Step,tmpSin,tmpCos);
      Poly[t].X:=tmpMid+Round(tmpSin*Radius);
      if Top<Bottom then Poly[t].Y:=Top
                    else Poly[t].Y:=Bottom;
      Poly[t].Z:=tmpMidZ-Round(tmpCos*ZRadius);
    end;

    tmpSize:=Abs(Bottom-Top);

    tmpPoly[1]:=Calc3DTPoint3D(Poly[1]);
    With Poly[1] do Calc3DPoint(tmpPoly[2],x,y+tmpSize,z);
    NumSide:=0;
    for t:=2 to NumCylinderSides do
    begin
      tmpPoly[4]:=Calc3DTPoint3D(Poly[t]);
      With Poly[t] do Calc3DPoint(tmpPoly[3],x,y+tmpSize,z);

      if tmpPoly[4].x>tmpPoly[1].x then
      begin
        if Dark3D then Dark(StepColor*NumSide);
        {$IFDEF D1}
        WinProcs.Polygon(FCanvas.Handle, PPoints(@tmpPoly)^, 4);
        {$ELSE}
        Windows.Polygon(FCanvas.Handle, PPoints(@tmpPoly)^, 4);
        {$ENDIF}
        Inc(NumSide);
      end;

      tmpPoly[1]:=tmpPoly[4];
      tmpPoly[2]:=tmpPoly[3];
    end;
  end
  else
  begin
    Radius:=(Bottom-Top) shr 1;
    tmpMid:=(Bottom+Top) shr 1;
    for t:=1 to NumCylinderSides do
    begin
      SinCos((t+5)*Step,tmpSin,tmpCos);
      if Left<Right then Poly[t].X:=Right
                    else Poly[t].X:=Left;
      Poly[t].Y:=tmpMid+Round(tmpSin*Radius);
      Poly[t].Z:=tmpMidZ-Round(tmpCos*ZRadius);
    end;

    tmpSize:=Abs(Right-Left);

    tmpPoly[1]:=Calc3DTPoint3D(Poly[1]);
    With Poly[1] do Calc3DPoint(tmpPoly[2],x-tmpSize,y,z);
    NumSide:=0;
    for t:=2 to NumCylinderSides-2 do
    begin
      tmpPoly[4]:=Calc3DTPoint3D(Poly[t]);
      With Poly[t] do Calc3DPoint(tmpPoly[3],x-tmpSize,y,z);

      if tmpPoly[4].y>tmpPoly[1].y then
      begin
        if Dark3D then Dark(StepColor*NumSide);
        {$IFDEF D1}
        WinProcs.Polygon(FCanvas.Handle, PPoints(@tmpPoly)^, 4);
        {$ELSE}
        Windows.Polygon(FCanvas.Handle, PPoints(@tmpPoly)^, 4);
        {$ENDIF}
        Inc(NumSide);
      end;

      tmpPoly[1]:=tmpPoly[4];
      tmpPoly[2]:=tmpPoly[3];
    end;
  end;
  for t:=1 to NumCylinderSides do tmpPoly[t]:=Calc3DTPoint3D(Poly[t]);
  if Dark3D then Dark(DarkColorQuantity);
  {$IFDEF D1}
  WinProcs.Polygon(FCanvas.Handle, PPoints(@tmpPoly)^, NumCylinderSides);
  {$ELSE}
  Windows.Polygon(FCanvas.Handle, PPoints(@tmpPoly)^, NumCylinderSides);
  {$ENDIF}
end;

procedure TTeeCanvas3D.Pyramid(Vertical:Boolean; Left,Top,Right,Bottom,z0,z1:Integer; DarkSides:Boolean);
Var OldColor:TColor;

  Procedure Dark(Quantity:Byte);
  begin
    if FBrush.Style=bsSolid then FBrush.Color:=ApplyDark(OldColor,Quantity)
                            else BackColor:=ApplyDark(OldColor,Quantity)
  end;

Var P0,P1,P2,P3,PTop:TPoint;
begin
  {$IFDEF TEEPERF}
  Log('Pyramid');
  {$ENDIF}
  if FBrush.Style=bsSolid then OldColor:=FBrush.Color
                          else OldColor:=BackColor;
  if Vertical then
  begin
    if Top<>Bottom then
    Begin
      Calc3DPoint(P0,Left,Bottom,Z0);
      Calc3DPoint(P1,Right,Bottom,Z0);
      Calc3DPoint(PTop,(Left+Right) div 2,Top,(Z0+Z1) div Longint(2));
      Polygon([P0,PTop,P1]);
      if Top<Bottom then
      begin
        Calc3DPoint(P2,Left,Bottom,Z1);
        if P2.Y<PTop.Y then Polygon([ P0,PTop,P2] );
      end;
      if DarkSides then Dark(DarkerColorQuantity);
      Calc3DPoint(P3,Right,Bottom,Z1);
      Polygon([ P1,PTop,P3 ] );
      if (Top<Bottom) and (P2.Y<PTop.Y) then
      begin
        Calc3DPoint(P2,Left,Bottom,Z1);
        Polygon([ PTop,P2,P3 ] );
      end;
    end;
    if Top>=Bottom then
    begin
      if DarkSides then Dark(DarkColorQuantity);
      RectangleY(Left,Bottom,Right,Z0,Z1);
    end;
  end
  else
  begin
    if Left<>Right then
    Begin
      Calc3DPoint(P0,Left,Top,Z0);
      Calc3DPoint(P1,Left,Bottom,Z0);
      Calc3DPoint(PTop,Right,(Top+Bottom) shr 1,(Z0+Z1) shr 1);
      Polygon([P0,PTop,P1]);
      if DarkSides then Dark(DarkColorQuantity);
      Calc3DPoint(P2,Left,Top,Z1);
      Polygon([ P0,PTop,P2 ] );
    end;
    if Left>=Right then
    begin
      if DarkSides then Dark(DarkerColorQuantity);
      RectangleZ(Left,Top,Bottom,Z0,Z1);
    end;
  end;
end;

procedure TTeeCanvas3D.PlaneFour3D(Points:TFourPoints; Z0,Z1:Integer);
begin
  {$IFDEF TEEPERF}
  Log('PlaneFour3D');
  {$ENDIF}
  Calc3DTPoint(Points[0],z0);
  Calc3DTPoint(Points[1],z0);
  Calc3DTPoint(Points[2],z1);
  Calc3DTPoint(Points[3],z1);
  {$IFDEF D1}
  WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ELSE}
  Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, 4);
  {$ENDIF}
end;

procedure TTeeCanvas3D.SetPixel3D(X,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF}; Value: TColor);
var DC:HDC;
begin
  {$IFDEF TEEPERF}
  Log('SetPixel3D');
  {$ENDIF}
  Calc3DPos(X,Y,Z);
  if FPen.Width=1 then FCanvas.Pixels[X,Y]:=Value
  else
  begin { simulate a big dot pixel }
    DC:=FCanvas.Handle;
    {$IFDEF D1}
    WinProcs.MoveToEx(DC, X, Y, nil);
    WinProcs.LineTo(DC,X,Y);
    {$ELSE}
    Windows.MoveToEx(DC, X, Y, nil);
    Windows.LineTo(DC,X,Y);
    {$ENDIF}
  end;
end;

Function TTeeCanvas3D.GetSupports3DText:Boolean;
begin
  {$IFDEF TEEPERF}
  Log('GetSupports3DText');
  {$ENDIF}
  result:=False;
end;

Function TTeeCanvas3D.GetSupportsFullRotation:Boolean;
begin
  {$IFDEF TEEPERF}
  Log('GetSupportsFullRotation');
  {$ENDIF}
  result:=False;
end;

Function TTeeCanvas3D.GetTextAlign:TCanvasTextAlign;
begin
  {$IFDEF TEEPERF}
  Log('GetTextAlign');
  {$ENDIF}
  {$IFDEF D1}
  result:=WinProcs.GetTextAlign(FCanvas.Handle);
  {$ELSE}
  result:=Windows.GetTextAlign(FCanvas.Handle);
  {$ENDIF}
end;

Function TTeeCanvas3D.GetUseBuffer:Boolean;
begin
  {$IFDEF TEEPERF}
  Log('GetUseBuffer');
  {$ENDIF}
  result:=FBufferedDisplay;
end;

Procedure TTeeCanvas3D.SetUseBuffer(Value:Boolean);
begin
  {$IFDEF TEEPERF}
  Log('SetUseBuffer');
  {$ENDIF}
  FBufferedDisplay:=Value;
  if (not FBufferedDisplay) and Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap:=nil;
    FDirty:=True;
  end;
end;

Function TTeeCanvas3D.GetMonochrome:Boolean;
begin
  {$IFDEF TEEPERF}
  Log('GetMonochrome');
  {$ENDIF}
  result:=FMonochrome;
end;

Procedure TTeeCanvas3D.SetMonochrome(Value:Boolean);
begin
  {$IFDEF TEEPERF}
  Log('SetMonochrome');
  {$ENDIF}
  if FMonochrome<>Value then
  begin
    FMonochrome:=Value;
    FBitmap.Free;
    FBitmap:=nil;
    FDirty:=True;
    if Assigned(F3DOptions) then F3DOptions.Repaint;
  end;
end;

Procedure TTeeCanvas3D.HorizLine3D(Left,Right,Y,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
var DC   : HDC;
    tmpY :{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
begin
  {$IFDEF TEEPERF}
  Log('HorizLine3D');
  {$ENDIF}
  tmpY:=Y;
  Calc3DPos(Left,tmpY,Z);
  DC:=FCanvas.Handle;
  {$IFDEF D1}
  WinProcs.MoveToEx(DC,Left,tmpY,nil);
  {$ELSE}
  Windows.MoveToEx(DC,Left,tmpY,nil);
  {$ENDIF}
  tmpY:=Y;
  Calc3DPos(Right,tmpY,Z);
  {$IFDEF D1}
  WinProcs.LineTo(DC,Right,tmpY);
  {$ELSE}
  Windows.LineTo(DC,Right,tmpY);
  {$ENDIF}
end;

Procedure TTeeCanvas3D.VertLine3D(X,Top,Bottom,Z:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
var DC   : HDC;
    tmpX :{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
begin
  {$IFDEF TEEPERF}
  Log('VertLine3D');
  {$ENDIF}
  tmpX:=X;
  Calc3DPos(tmpX,Top,Z);
  DC:=FCanvas.Handle;
  {$IFDEF D1}
  WinProcs.MoveToEx(DC,tmpX,Top,nil);
  {$ELSE}
  Windows.MoveToEx(DC,tmpX,Top,nil);
  {$ENDIF}
  tmpX:=X;
  Calc3DPos(tmpX,Bottom,Z);
  {$IFDEF D1}
  WinProcs.LineTo(DC,tmpX,Bottom);
  {$ELSE}
  Windows.LineTo(DC,tmpX,Bottom);
  {$ENDIF}
end;

Procedure TTeeCanvas3D.ZLine3D(X,Y,Z0,Z1:{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF});
var DC   : HDC;
    tmpX,
    tmpY :{$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
begin
  {$IFDEF TEEPERF}
  Log('ZLine3D');
  {$ENDIF}
  tmpX:=X;
  tmpY:=Y;
  Calc3DPos(tmpX,tmpY,Z0);
  DC:=FCanvas.Handle;
  {$IFDEF D1}
  WinProcs.MoveToEx(DC,tmpX,tmpY,nil);
  {$ELSE}
  Windows.MoveToEx(DC,tmpX,tmpY,nil);
  {$ENDIF}
  tmpX:=X;
  tmpY:=Y;
  Calc3DPos(tmpX,tmpY,Z1);
  {$IFDEF D1}
  WinProcs.LineTo(DC,tmpX,tmpY);
  {$ELSE}
  Windows.LineTo(DC,tmpX,tmpY);
  {$ENDIF}
end;

procedure TTeeCanvas3D.TriangleWithZ(Const P1,P2,P3:TPoint; Z:Integer);
var Points : Array[0..2] of TPoint;
begin
  {$IFDEF TEEPERF}
  Log('TriangleWithZ');
  {$ENDIF}
  Calc3DPoint(Points[0],P1.X,P1.Y,Z);
  Calc3DPoint(Points[1],P2.X,P2.Y,Z);
  Calc3DPoint(Points[2],P3.X,P3.Y,Z);
  {$IFDEF D1}
  WinProcs.Polygon(FCanvas.Handle, PPoints(@Points)^, 3);
  {$ELSE}
  Windows.Polygon(FCanvas.Handle, PPoints(@Points)^, 3);
  {$ENDIF}
end;

Procedure TTeeCanvas3D.Arrow( Filled:Boolean;
                              Const FromPoint,ToPoint:TPoint;
                              ArrowWidth,ArrowHeight,Z:Integer);
Var x    : Double;
    y    : Double;
    SinA : Double;
    CosA : Double;

    Function CalcArrowPoint:TPoint;
    Begin
      result.X:=Round( x*CosA + y*SinA);
      result.Y:=Round(-x*SinA + y*CosA);
      Calc3DTPoint(result,Z);
    end;

Var tmpHoriz  : Longint;
    tmpVert   : Longint;
    dx        : Longint;
    dy        : Longint;
    tmpHoriz4 : Double;
    xb        : Double;
    yb        : Double;
    l         : Double;

   { These are the Arrows points coordinates }
    To3D,pc,pd,pe,pf,pg,ph:TPoint;

    (*           pc
                   |\
    ph           pf| \
      |------------   \ ToPoint
 From |------------   /
    pg           pe| /
                   |/
                 pd
    *)
begin
  {$IFDEF TEEPERF}
  Log('Arrow');
  {$ENDIF}
  dx := ToPoint.x-FromPoint.x;
  dy := FromPoint.y-ToPoint.y;
  l  := Sqrt(1.0*dx*dx+1.0*dy*dy);
  if l>0 then  { if at least one pixel... }
  Begin
    tmpHoriz:=ArrowWidth;
    tmpVert :=MinLong(Round(l),ArrowHeight);
    SinA:= dy / l;
    CosA:= dx / l;
    xb:= ToPoint.x*CosA - ToPoint.y*SinA;
    yb:= ToPoint.x*SinA + ToPoint.y*CosA;
    x := xb - tmpVert;
    y := yb - tmpHoriz/2;
    pc:=CalcArrowPoint;
    y := yb + tmpHoriz/2;
    pd:=CalcArrowPoint;
    if Filled then
    Begin
      tmpHoriz4:=tmpHoriz/4;
      y := yb - tmpHoriz4;
      pe:=CalcArrowPoint;
      y := yb + tmpHoriz4;
      pf:=CalcArrowPoint;
      x := FromPoint.x*cosa - FromPoint.y*sina;
      y := yb - tmpHoriz4;
      pg:=CalcArrowPoint;
      y := yb + tmpHoriz4;
      ph:=CalcArrowPoint;
      To3D:=ToPoint;
      Calc3DTPoint(To3D,Z);
      Polygon([ ph,pg,pe,pc,To3D,pd,pf ]);
    end
    else
    begin
      MoveTo3D(FromPoint.x,FromPoint.y,z);
      LineTo3D(ToPoint.x,ToPoint.y,z);
      LineTo3D(pd.x,pd.y,z);
      MoveTo3D(ToPoint.x,ToPoint.y,z);
      LineTo3D(pc.x,pc.y,z);
    end;
  end;
end;

{ Util functions }
Function MaxDouble(Const a,b:Double):Double;
begin
  if a>b then result:=a else result:=b;
end;

Function MinDouble(Const a,b:Double):Double;
begin
  if a<b then result:=a else result:=b;
end;

Function MaxLong(a,b:Longint):Longint;
begin
  if a>b then result:=a else result:=b;
end;

Function MinLong(a,b:Longint):Longint;
begin
  if a>b then result:=b else result:=a;
end;

Procedure SwapLongint(Var a,b:Longint);
var tmp : Longint;
Begin
  Tmp:=a;  a:=b;  b:=Tmp;
End;

Procedure SwapDouble(Var a,b:Double);
var tmp : Double;
begin
  tmp:=a;  a:=b;  b:=tmp;
end;

Procedure SwapInteger(Var a,b:Integer);
var tmp : Integer;
Begin
  tmp:=a; a:=b; b:=tmp;
end;

{$IFDEF TEETRIAL}
Procedure TeeTrial(ComponentState:TComponentState);
begin
  if TeeTreeMargin and (not (csDesigning in ComponentState)) then
  begin
    TeeTrialShowAboutBox(True);
    TeeTreeMargin:=False;
  end;
end;
{$ENDIF}

{$IFDEF TEEUSEDRAWDIB}
initialization
  hdd:=DrawDibOpen;
finalization
  DrawDibClose(hdd);
{$ENDIF}
end.
