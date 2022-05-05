{**********************************************}
{   TWindRoseSeries                            }
{   Copyright (c) 1998 by David Berneda        }
{      Series Developer Kit Example            }
{**********************************************}
{$I teedefs.inc}
unit TeeRose;

interface

Uses WinTypes,WinProcs,SysUtils,Classes,Graphics,Teengine,Chart,
     TeCanvas, Series, TeePolar, ExtCtrls;

{  This unit contains two Series components:

   TWindRoseSeries --  A Polar Series displaying Wind directions.
   TClockSeries    --  A Polar Series showing a watch.
}

Type TWindRoseSeries=class(TCustomPolarSeries)
     protected
       Function GetCircleLabel(Const AngleOrIndex:Double):String; override;
     public
       Constructor Create(AOwner: TComponent); override;
       Procedure PrepareForGallery(IsEnabled:Boolean); override;
     published
       property AngleIncrement;
       property AngleValues;
       property Brush;
       property CircleBackColor;
       property CircleLabels default True;
       property CircleLabelsFont;
       property CircleLabelsRotated;
       property CirclePen;
       property CloseCircle;
       property Pen;
       property Pointer;
       property RadiusIncrement;
       property RadiusValues;
       property RotationAngle default 90;
     end;

     TClockSeries=class;

     TClockSeriesStyle=(cssDecimal,cssRoman);

     TClockSeriesGetTimeEvent=procedure(Sender:TClockSeries; Var ATime:TDateTime) of object;

     { This series draws a "Clock" using a Polar series }
     TClockSeries=class(TCustomPolarSeries)
     private
       FOnGetTime  : TClockSeriesGetTimeEvent;
       FPenHours   : TChartPen;
       FPenMinutes : TChartPen;
       FPenSeconds : TChartPen;
       FStyle      : TClockSeriesStyle;

       ITimer      : TTimer;
       Procedure SetPenHours(Value:TChartPen);
       Procedure SetPenMinutes(Value:TChartPen);
       Procedure SetPenSeconds(Value:TChartPen);
       Procedure SetStyle(Value:TClockSeriesStyle);
       Procedure OnTimerExpired(Sender:TObject);
     protected
       Procedure DrawAllValues; override;
       Function GetCircleLabel(Const AngleOrIndex:Double):String; override;
       Procedure SetParentChart(Value:TCustomAxisPanel); override;
     public
       Constructor Create(AOwner: TComponent); override;
       Destructor Destroy; override;

       Procedure Assign(Source:TPersistent); override;
       Function NumSampleValues:Longint; override;
       Procedure PrepareForGallery(IsEnabled:Boolean); override;
     published
       property Brush;
       property CircleBackColor;
       property Circled default True;
       property CircleLabels default True;
       property CircleLabelsFont;
       property CircleLabelsRotated;
       property CirclePen;
       property PenHours:TChartPen read FPenHours write SetPenHours;
       property PenMinutes:TChartPen read FPenMinutes write SetPenMinutes;
       property PenSeconds:TChartPen read FPenSeconds write SetPenSeconds;
       property RotationAngle default 90;
       property Style:TClockSeriesStyle read FStyle write SetStyle
                                        default cssRoman;
       { events }
       property OnGetTime  : TClockSeriesGetTimeEvent read FOnGetTime
                                                      write FOnGetTime;
     end;

implementation

Uses TeeProco;  { <-- needed because the "Samples" constant }

{ TWindRoseSeries }
Constructor TWindRoseSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CircleLabels:=True;
  RotationAngle:=90;
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
end;

Procedure TWindRoseSeries.PrepareForGallery(IsEnabled:Boolean);
begin
  inherited PrepareForGallery(IsEnabled);
  CircleLabelsFont.Size:=6;
  Pointer.HorizSize:=2;
  Pointer.VertSize:=2;
end;

{ Return the string corresponding to the "Angle" degree parameter }
Function TWindRoseSeries.GetCircleLabel(Const AngleOrIndex:Double):String;
begin
  Case Round(AngleOrIndex) of
      0:  result:='N';
     45:  result:='NW';
     90:  result:='W';
    135:  result:='SW';
    180:  result:='S';
    225:  result:='SE';
    270:  result:='E';
    315:  result:='NE';
  else result:='';
  end;
end;

{ TClockSeries }
Constructor TClockSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Pointer.Visible:=False;
  ShowInLegend:=False;
  FStyle:=cssRoman;
  Brush.Style:=bsSolid;
  CircleLabels:=True;
  RotationAngle:=90;
  Circled:=True;
  Add(0 {$IFNDEF D5},'',clTeeColor{$ENDIF});
  FPenHours:=CreateChartPen;
  FPenMinutes:=CreateChartPen;
  FPenSeconds:=CreateChartPen;
  ITimer:=TTimer.Create(Self);
  With ITimer do
  begin
    Interval:=1000;
    Enabled:=True;
    OnTimer:=OnTimerExpired;
  end;
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
end;

Destructor TClockSeries.Destroy;
begin
  FPenHours.Free;
  FPenMinutes.Free;
  FPenSeconds.Free;
  ITimer.Free;
  inherited Destroy;
end;

Procedure TClockSeries.OnTimerExpired(Sender:TObject);
begin
  ITimer.Enabled:=False;
  Repaint;
  ITimer.Enabled:=True;
end;

Procedure TClockSeries.Assign(Source:TPersistent);
begin
  if Source is TClockSeries then
  With TClockSeries(Source) do
  begin
    Self.FPenHours.Assign(FPenHours);
    Self.FPenMinutes.Assign(FPenMinutes);
    Self.FPenSeconds.Assign(FPenSeconds);
    Self.FStyle:=FStyle;
  end;
  inherited Assign(Source);
end;

Const PiDegree=Pi/180.0;

Procedure TClockSeries.DrawAllValues;
Var X  : {$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};
    Y  : {$IFDEF D2C1}Longint{$ELSE}Integer{$ENDIF};

    Procedure CalcPos(Const AAngle,ASize:Double);
    begin
      AngleToPos(AAngle*PiDegree,ASize*XRadius/2.0,ASize*YRadius/2.0,X,Y);
    end;

Var H  : Word;
    M  : Word;
    S  : Word;
    Ms : Word;
    tmpColor:TColor;
    tmp : TDateTime;
begin
  With ParentChart.Canvas do
  begin
    tmp:=Now;
    if Assigned(FOnGetTime) then FOnGetTime(Self,tmp);
    DecodeTime(tmp,H,M,S,Ms);
    Brush.Assign(Self.Brush);

    if FPenHours.Visible then
    begin
      Pen.Assign(FPenHours);
      CalcPos(360.0-(360.0*(60.0*H+M)/(12.0*60.0)),1.3);
      Arrow(True,Point(CircleXCenter,CircleYCenter),Point(X,Y),14,20,EndZ);
    end;

    if FPenMinutes.Visible then
    begin
      Pen.Assign(FPenMinutes);
      CalcPos(360.0-(360.0*M/60.0),1.7);
      Arrow(True,Point(CircleXCenter,CircleYCenter),Point(X,Y),10,16,EndZ);
    end;

    CalcPos(360.0-(360.0*S/60.0),1.8);
    if FPenSeconds.Visible then
    begin
      Pen.Assign(FPenSeconds);
      MoveTo3D(CircleXCenter,CircleYCenter,EndZ);
      LineTo3D(X,Y,EndZ);
    end;

    With Pointer do
    if Visible then
    begin
      tmpColor:=Brush.Color;
      PrepareCanvas(tmpColor);
      Draw(X,Y,tmpColor,Style);
    end;
  end;
end;

Procedure TClockSeries.SetParentChart(Value:TCustomAxisPanel);
begin
  inherited SetParentChart(Value);
  if ParentChart<>nil then
  With ParentChart do
  begin
    LeftAxis.Visible:=False;
    TopAxis.Visible:=False;
    RightAxis.Visible:=False;
    BottomAxis.Visible:=False;
    AngleIncrement:=30;
  end;
end;

Procedure TClockSeries.PrepareForGallery(IsEnabled:Boolean);
begin
  inherited PrepareForGallery(IsEnabled);
  CircleLabelsFont.Size:=6;
  Pointer.HorizSize:=2;
  Pointer.VertSize:=2;
end;

{ Return the string corresponding to the "Angle" degree parameter }
Function TClockSeries.GetCircleLabel(Const AngleOrIndex:Double):String;
Const RomanNumber:Array[1..12] of String=
  ('I','II','III','IV','V','VI','VII','VIII','IX','X','XI','XII');
var tmpAngleIndex:Integer;
begin
  tmpAngleIndex:=Round((360.0-AngleOrIndex)/30.0);
  if FStyle=cssDecimal then result:=IntToStr(tmpAngleIndex)
                       else result:=RomanNumber[tmpAngleIndex];
end;

Procedure TClockSeries.SetPenHours(Value:TChartPen);
begin
  FPenHours.Assign(Value);
end;

Procedure TClockSeries.SetPenMinutes(Value:TChartPen);
begin
  FPenMinutes.Assign(Value);
end;

Procedure TClockSeries.SetPenSeconds(Value:TChartPen);
begin
  FPenSeconds.Assign(Value);
end;

Procedure TClockSeries.SetStyle(Value:TClockSeriesStyle);
begin
  if FStyle<>Value then
  begin
    FStyle:=Value;
    Repaint;
  end;
end;

Function TClockSeries.NumSampleValues:Longint;
begin
  result:=1;
end;

{ Series/Functions Registration/Un-Registration }
Procedure WindRoseExitProc; far;
begin
  UnRegisterTeeSeries([ TWindRoseSeries, TClockSeries ]);
end;

initialization
  RegisterTeeSeries( TWindRoseSeries, 'Wind Rose', TeeMsg_GallerySamples, 1 );
  RegisterTeeSeries( TClockSeries, 'Clock', TeeMsg_GallerySamples, 1 );
{$IFDEF D1}
  AddExitProc(WindRoseExitProc);
{$ELSE}
finalization
  WindRoseExitProc;
{$ENDIF}
end.
