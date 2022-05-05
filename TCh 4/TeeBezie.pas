{**********************************************}
{   TBezierSeries Component                    }
{   Copyright (c) 1996-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit TeeBezie;

{ This Series component is derived from PointLine Series.
  It draws PolyBezier curves using every 3 points in the Series.
  The first point in the Series determines the origin.

  The LinePen property controls the Bezier curve color, width and style.
  The inherited Pointer property is used to draw the control points. }
interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, TeeProcs,
  Teengine, Chart, Series, TeCanvas;

Const MaxBezierPoints=8000;  { <-- maximum points per Series }

type
  TBezierSeries = class(TCustomSeries)
  private
    { Private declarations }
    FNumBezierPoints:Integer;
    Procedure SetBezierPoints(Value:Integer);
  protected
    { Protected declarations }
    Procedure SetSeriesColor(AColor:TColor); override;
    procedure DrawAllValues; override;
  public
    { Public declarations }
    Constructor Create(AOwner:TComponent); override;
    Procedure PrepareForGallery(IsEnabled:Boolean); override;
  published
    { Published declarations }
    property LinePen;
    property NumBezierPoints:Integer read FNumBezierPoints
                                     write SetBezierPoints default 32;
    property Pointer;
    property XValues;
    property YValues;
   { events }
    property OnClickPointer;
  end;

implementation

Uses TeeConst, TeeProco;

Constructor TBezierSeries.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FNumBezierPoints:=32;
end;

Procedure TBezierSeries.SetBezierPoints(Value:Integer);
begin
  if Value<2 then Raise ChartException.Create(TeeMsg_LowBezierPoints);
  SetIntegerProperty(FNumBezierPoints,Value);
end;

procedure TBezierSeries.DrawAllValues;
type PBezierPoints = ^TBezierPoints;
     TBezierPoints = array[0..0] of TPoint;
     TBezierMaxPoints=Array[0..MaxBezierPoints-1] of TPoint;
     PBezierMaxPoints=^TBezierMaxPoints;
var tmpPoints : PBezierMaxPoints;
    t         : Integer;
    tt        : Integer;
    tmpCount  : Integer;
    tmpColor  : TColor;
    mu        : Double;
    mum1      : Double;
    mum12     : Double;
    mu2       : Double;
    P         : TPoint;
    P1        : TPoint;
    P2        : TPoint;
    P3        : TPoint;
Begin
  New(tmpPoints);
  { Calculate XY coordinates... }
  tmpCount:=MinLong(MaxBezierPoints,Count);
  for t:=0 to tmpCount-1 do
  begin
    tmpPoints^[t].X:=CalcXPos(t);
    tmpPoints^[t].Y:=CalcYPos(t);
  end;
  { Draw bezier line... }
  With ParentChart,Canvas do
  begin
    AssignVisiblePen(LinePen);
    Brush.Style:=bsClear;
    if View3D then
    begin
      MoveTo3D(tmpPoints^[0].X,tmpPoints^[0].Y,StartZ);
      for t:=1 to tmpCount div 3 do
      begin
        P1:=tmpPoints^[3*t-3];
        P2:=tmpPoints^[3*t-2];
        P3:=tmpPoints^[3*t-1];
        for tt:=1 to FNumBezierPoints do
        begin
          mu:=tt/FNumBezierPoints;
          mu2:=Sqr(mu);
          mum1:=1-mu;
          mum12:=Sqr(mum1);
          p.x:=Round(p1.x * mum12 + 2*p2.x*mum1*mu + p3.x*mu2);
          p.y:=Round(p1.y * mum12 + 2*p2.y*mum1*mu + p3.y*mu2);
          LineTo3D(P.X,P.Y,StartZ);
        end;
      end;
    end
    else
    begin
      MoveTo(tmpPoints^[0].X,tmpPoints^[0].Y);
      {$IFNDEF D1}
      PolyBezierTo(Handle,PBezierPoints(@tmpPoints)^,(3*(tmpCount div 3)));
      {$ELSE}
      for t:=1 to tmpCount div 3 do
      begin
        P1:=tmpPoints^[3*t-3];
        P2:=tmpPoints^[3*t-2];
        P3:=tmpPoints^[3*t-1];
        for tt:=1 to FNumBezierPoints do
        begin
          mu:=tt/FNumBezierPoints;
          mu2:=Sqr(mu);
          mum1:=1-mu;
          mum12:=Sqr(mum1);
          p.x:=Round(p1.x * mum12 + 2*p2.x*mum1*mu + p3.x*mu2);
          p.y:=Round(p1.y * mum12 + 2*p2.y*mum1*mu + p3.y*mu2);
          LineTo(P.X,P.Y);
        end;
      end;
      {$ENDIF}
    end;
  end;
  { Draw pointers... }
  if Pointer.Visible then
  for t:=0 to tmpCount-1 do
  begin
    tmpColor:=ValueColor[t];
    With Pointer do
    begin
      PrepareCanvas(tmpColor);
      Draw(tmpPoints^[t].X,tmpPoints^[t].Y,tmpColor,Style);
    end;
  end;
  Dispose(tmpPoints);
End;

Procedure TBezierSeries.SetSeriesColor(AColor:TColor);
begin
  inherited SetSeriesColor(AColor);
  LinePen.Color:=AColor;
end;

Procedure TBezierSeries.PrepareForGallery(IsEnabled:Boolean);
Begin
  inherited PrepareForGallery(IsEnabled);
  FillSampleValues(3);
  ColorEachPoint:=IsEnabled;
  Pointer.Draw3D:=False;
end;

{ Un-register the Series }
Procedure TeeBezierExitProc; far;
begin
  UnRegisterTeeSeries([TBezierSeries]);
end;

{ Register the Series at Chart gallery }
initialization
  RegisterTeeSeries( TBezierSeries, TeeMsg_GalleryBezier, TeeMsg_GalleryExtended, 1 );
{$IFDEF D1}
  AddExitProc(TeeBezierExitProc);
{$ELSE}
finalization
  TeeBezierExitProc;
{$ENDIF}
end.
