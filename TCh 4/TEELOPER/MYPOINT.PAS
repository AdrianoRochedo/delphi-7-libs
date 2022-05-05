{**********************************************}
{   TMyPointSeries                             }
{   Copyright (c) 1997 by David Berneda        }
{      Series Developer Kit Example            }
{**********************************************}
unit MyPoint;

interface

Uses SysUtils,Classes,Graphics,Teengine,Chart,Series;

{ This sample Series derives from TPointSeries.
  It shows how to override the DrawValue method, which is called
  every time every point in the Series should be displayed.

  In this sample, one horizontal line and one vertical line are
  drawn from the axis to every point.
  A new TPen property is published to control the lines attributes.
}

Type TMyPointSeries=class(TPointSeries)
     private
       FLinesPen:TChartPen;
       procedure SetLinesPen(Value:TChartPen);
     protected
       procedure DrawValue(ValueIndex:Longint); override;
     public
       Constructor Create(AOwner:TComponent); override;
       Destructor Destroy; override;
     published
       property LinesPen:TChartPen read FLinesPen write SetLinesPen;
     end;

implementation

Uses WinTypes,WinProcs;

{ overrided constructor to change default pointer style and 3D }
Constructor TMyPointSeries.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Pointer.Draw3D:=False;
  Pointer.Style:=psDiamond;
  FLinesPen:=CreateChartPen;  { <-- create new pen property }
  FLinesPen.Color:=clRed;     { <-- set default color to Red }
end;

{ overrided DrawValue to draw additional lines for each point }
procedure TMyPointSeries.DrawValue(ValueIndex:Longint);
var tmpX,tmpY:Longint;
begin
  With ParentChart,Canvas do
  begin
    { calculate X and Y screen positions in pixels }
    tmpX:=CalcXPos(ValueIndex);
    tmpY:=CalcYPos(ValueIndex);

    { change brush and pen attributes }
    Brush.Style:=bsClear;
    SetBkMode(Canvas.Handle,Transparent);
    Pen.Assign(FLinesPen);

    { draw the horizontal and vertical lines }
    MoveTo(ChartRect.Left,tmpY);
    LineTo(tmpX,tmpY);
    LineTo(tmpX,ChartRect.Bottom);
  end;

  { draw the point }
  inherited DrawValue(ValueIndex);
end;

procedure TMyPointSeries.SetLinesPen(Value:TChartPen);
begin
  FLinesPen.Assign(Value);   { <-- set new property values }
end;

Destructor TMyPointSeries.Destroy;
begin
  FLinesPen.Free;   { <-- remember to destroy private properties }
  inherited Destroy;
end;

{ Series/Functions Registration/Un-Registration }

Procedure MyPointExitProc; far;
begin
  UnRegisterTeeSeries([TMyPointSeries]);
end;

initialization
  RegisterTeeSeries( TMyPointSeries, 'MyPoint', 'Samples', 1 );
{$IFNDEF WIN32}
  AddExitProc(MyPointExitProc);
{$ENDIF}
{$IFDEF WIN32}
finalization
  MyPointExitProc;
{$ENDIF}
end.
