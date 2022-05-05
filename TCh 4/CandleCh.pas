{**********************************************}
{   TCandleSeries (derived from OHLCSeries)    }
{   Copyright (c) 1995-1996 by David Berneda   }
{**********************************************}
{$I teedefs.inc}
unit CandleCh;

interface

{ This unit shows how a new Chart Series component can be easily created.
  TCandleSeries derives from TOHLCSeries (Open, High, Low & Close).
  See OHLChart.pas for TOHLCSeries source code.

  TCandleSeries overrides the TChartSeries.DrawValue method to paint its
  points in a financial used fashion.

  TVolumeSeries overrides the TChartSeries.DrawValue method to paint its
  points like thin vertical bars.
}
Uses WinTypes,WinProcs,Classes,Graphics,Chart,Series,OHLChart,Teengine,
     TeCanvas;

Const DefCandleWidth = 9;  { 4 + 1 + 4 }

Type TCandleStyle=(csCandleStick,csCandleBar);

     { TCandleSeries }
     TCandleSeries=class(TOHLCSeries)
     private
       FCandleWidth    : Integer;
       FCandleStyle    : TCandleStyle;
       FUpCloseColor   : TColor;
       FDownCloseColor : TColor;
       FShowOpenTick   : Boolean;
       FShowCloseTick  : Boolean;
     protected
       procedure DrawValue(ValueIndex:Longint); override;
       Procedure SetShowOpenTick(Value:Boolean);
       Procedure SetShowCloseTick(Value:Boolean);
       Procedure SetUpColor(Value:TColor);
       Procedure SetDownColor(Value:TColor);
       Procedure SetCandleWidth(Value:Integer);
       Procedure SetCandleStyle(Value:TCandleStyle);
       Function GetDraw3D:Boolean;
       procedure SetDraw3D(Value:Boolean);
       Function GetDark3D:Boolean;
       procedure SetDark3D(Value:Boolean);
       Function GetPen:TPen;
       procedure SetPen(Value:TPen);
     public
       Constructor Create(AOwner: TComponent); override;
       Procedure PrepareForGallery(IsEnabled:Boolean); override;
       Procedure Assign(Source:TPersistent); override;
       Function AddCandle( Const ADate:TDateTime;
                           Const AOpen,AHigh,ALow,AClose:Double):Longint;
       Function  GetEditorClass:String; override;
    published
       property CandleStyle:TCandleStyle read FCandleStyle write SetCandleStyle
                                         default csCandleStick;
       property CandleWidth:Integer read FCandleWidth write SetCandleWidth
                                    default DefCandleWidth;
       property Draw3D:Boolean read GetDraw3D write SetDraw3D default False;
       property Dark3D:Boolean read GetDark3D write SetDark3D default True;
       property DownCloseColor:TColor read FDownCloseColor write SetDownColor
                                      default clRed;
       property ShowCloseTick:Boolean read FShowCloseTick write SetShowCloseTick
                                      default True;
       property ShowOpenTick:Boolean read FShowOpenTick write SetShowOpenTick
                                     default True;
       property UpCloseColor:TColor read FUpCloseColor write SetUpColor
                                    default clWhite;
       property Pen:TPen read GetPen write SetPen;
     end;

     { Used in financial charts for Volume quantities (or OpenInterest) }
     { Overrides FillSampleValues to create random POSITIVE values }
     { Overrides DrawValue to paint a thin vertical bar }
     { Declares VolumeValues (same like YValues) }
     TVolumeSeries=class(TCustomSeries)
     protected
       Function GetVolumeValues:TChartValueList;
       Procedure SetVolumeValues(Value:TChartValueList);
       procedure DrawValue(ValueIndex:Longint); override;
     public
       Constructor Create(AOwner: TComponent); override;
       Function GetEditorClass:String; override;
       Function NumSampleValues:Longint; override;
       Procedure FillSampleValues(NumValues:Longint); override;
       Procedure PrepareForGallery(IsEnabled:Boolean); override;
     published
       property LinePen;
       property VolumeValues:TChartValueList read GetVolumeValues write SetVolumeValues;
       property XValues;
     end;

implementation

Uses SysUtils,TeeProCo,TeeConst;

{ TCandleSeries }
Constructor TCandleSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FUpCloseColor  :=clWhite;
  FDownCloseColor:=clRed;
  FCandleWidth   :=DefCandleWidth;
  FCandleStyle   :=csCandleStick;
  FShowOpenTick  :=True;
  FShowCloseTick :=True;
  Pointer.Draw3D :=False;
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
end;

Procedure TCandleSeries.SetShowOpenTick(Value:Boolean);
Begin
  SetBooleanProperty(FShowOpenTick,Value);
End;

Procedure TCandleSeries.SetShowCloseTick(Value:Boolean);
Begin
  SetBooleanProperty(FShowCloseTick,Value);
End;

procedure TCandleSeries.DrawValue(ValueIndex:Longint);
var yOpen  : Longint;
    yClose : Longint;
    yHigh  : Longint;
    yLow   : Longint;

    Function CalcCandleColor:TColor;
    Begin
      With ParentChart.Canvas do
      begin
        if yOpen>yClose then result:=FUpCloseColor   else
        if yOpen<yClose then result:=FDownCloseColor else
        Begin
          { color algorithm when open is equal to close }
          if ValueIndex=0 then
             result:=FUpCloseColor  { <-- first point }
          else
          if CloseValues.Value[ValueIndex-1]>CloseValues.Value[ValueIndex] then
             result:=FDownCloseColor
          else
          if CloseValues.Value[ValueIndex-1]<CloseValues.Value[ValueIndex] then
             result:=FUpCloseColor
          else
             result:=ValueColor[ValueIndex-1];
        end;
      end;
    end;

Var x             : Integer;
    tmpLeftWidth  : Integer;
    tmpRightWidth : Integer;
    tmpTop        : LongInt;
    tmpBottom     : LongInt;
Begin
  Pointer.PrepareCanvas(clTeeColor); { Pointer Pen and Brush styles }
  With ParentChart, Canvas do
  Begin
    X:=CalcXPosValue(DateValues.Value[ValueIndex]); { The horizontal position }

    { Vertical positions of Open, High, Low & Close values for this point }
    YOpen :=CalcYPosValue(OpenValues.Value[ValueIndex]);
    YHigh :=CalcYPosValue(HighValues.Value[ValueIndex]);
    YLow  :=CalcYPosValue(LowValues.Value[ValueIndex]);
    YClose:=CalcYPosValue(CloseValues.Value[ValueIndex]);

    tmpLeftWidth:=FCandleWidth div 2; { calc half Candle Width }
    tmpRightWidth:=FCandleWidth-tmpLeftWidth;

    if FCandleStyle=csCandleStick then
    Begin { draw Candle Stick }

      if View3D and Pointer.Draw3D then
      begin
        tmpTop:=yClose;
        tmpBottom:=yOpen;
        if tmpTop>tmpBottom then SwapLongint(tmpTop,tmpBottom);
        { Draw Candle Vertical Line from bottom to Low }
        VertLine3D(x,tmpBottom,yLow,MiddleZ);
        { Draw 3D Candle }
        Brush.Color:=CalcCandleColor;
        if yOpen=yClose then Pen.Color:=CalcCandleColor;
        Cube( x-tmpLeftWidth,x+tmpRightWidth,tmpTop,tmpBottom,
              StartZ,EndZ,Pointer.Dark3D);
        { Draw Candle Vertical Line from Top to High }
        VertLine3D(x,tmpTop,yHigh,MiddleZ);
      end
      else
      begin
        { Draw Candle Vertical Line from High to Low }
        VertLine3D(x,yLow,yHigh,MiddleZ);
        { remember that Y coordinates are inverted }
        if yOpen=yClose then
        begin
          Pen.Color:=CalcCandleColor;
          HorizLine3D(x-tmpLeftWidth,x+tmpRightWidth,yOpen,MiddleZ);
        end
        else
        begin
          Brush.Color:=CalcCandleColor;
          RectangleWithZ(Rect(x-tmpLeftWidth,yOpen,x+tmpRightWidth,yClose),MiddleZ);
        end;
      end;
    end
    else
    Begin { draw Candle bar }
      Pen.Color:=CalcCandleColor;
      { Draw Candle Vertical Line from High to Low }
      VertLine3D(x,yLow,yHigh,MiddleZ);
      if FShowOpenTick then HorizLine3D(x,x-tmpLeftWidth,yOpen,MiddleZ);
      if FShowCloseTick then HorizLine3D(x,x+tmpRightWidth,yClose,MiddleZ);
    end;
  end;
end;

Procedure TCandleSeries.SetUpColor(Value:TColor);
Begin
  SetColorProperty(FUpCloseColor,Value);
end;

Procedure TCandleSeries.SetDownColor(Value:TColor);
Begin
  SetColorProperty(FDownCloseColor,Value);
end;

Procedure TCandleSeries.SetCandleWidth(Value:Integer);
Begin
  SetIntegerProperty(FCandleWidth, Value);
end;

Procedure TCandleSeries.SetCandleStyle(Value: TCandleStyle);
Begin
  if FCandleStyle<>Value then
  begin
    FCandleStyle:=Value;
    Repaint;
  end;
end;

Function TCandleSeries.GetEditorClass:String;
Begin
  result:='TCandleEditor';  { <-- do not translate }
End;

Procedure TCandleSeries.PrepareForGallery(IsEnabled:Boolean);
Begin
  inherited PrepareForGallery(IsEnabled);
  FillSampleValues(4);
  ColorEachPoint:=True;
  if IsEnabled then
     UpCloseColor:=clBlue
  else
  begin
    UpCloseColor:=clSilver;
    DownCloseColor:=clSilver;
    Pointer.Pen.Color:=clGray;
  end;
  Pointer.Pen.Width:=2;
  CandleWidth:=12;
end;

Procedure TCandleSeries.Assign(Source:TPersistent);
begin
  if Source is TCandleSeries then
  With TCandleSeries(Source) do
  begin
    Self.FCandleWidth   :=FCandleWidth;
    Self.FCandleStyle   :=FCandleStyle;
    Self.FUpCloseColor  :=FUpCloseColor;
    Self.FDownCloseColor:=FDownCloseColor;
    Self.FShowOpenTick  :=FShowOpenTick;
    Self.FShowCloseTick :=FShowCloseTick;
  end;
  inherited Assign(Source);
end;

Function TCandleSeries.GetDraw3D:Boolean;
begin
  result := Pointer.Draw3D;
end;

procedure TCandleSeries.SetDraw3D(Value:Boolean);
begin
  Pointer.Draw3D := Value;
end;

Function TCandleSeries.GetDark3D:Boolean;
begin
  result := Pointer.Dark3D;
end;

procedure TCandleSeries.SetDark3D(Value:Boolean);
begin
  Pointer.Dark3D:=Value;
end;

Function TCandleSeries.GetPen:TPen;
begin
  result:=Pointer.Pen;
end;

procedure TCandleSeries.SetPen(Value:TPen);
begin
  Pointer.Pen.Assign(Value);
end;

Function TCandleSeries.AddCandle(Const ADate: TDateTime;
                                 Const AOpen, AHigh, ALow, AClose: Double):Longint;
begin
   result:=AddOHLC( ADate, AOpen, AHigh, ALow, AClose);
end;

{ TVolumeSeries }
Constructor TVolumeSeries.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DrawArea:=False;
  Pointer.Visible:=False;
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
end;

Function TVolumeSeries.GetVolumeValues:TChartValueList;
Begin
  result:=YValues;
end;

Procedure TVolumeSeries.SetVolumeValues(Value:TChartValueList);
Begin
  SetYValues(Value);
end;

procedure TVolumeSeries.DrawValue(ValueIndex:Longint);
var tmpY : Integer;
Begin
  With ParentChart,Canvas do
  Begin
    Pen.Assign(LinePen);
    Pen.Color:=ValueColor[ValueIndex];  { <-- assign point color }
    { moves to x,y coordinates and draws a vertical bar to top or bottom,
      depending on the vertical Axis.Inverted property }
    BackMode:=cbmTransparent;
    With GetVertAxis do
      if Inverted then tmpY:=IStartPos else tmpY:=IEndPos;
    VertLine3D(CalcXPos(ValueIndex),CalcYPos(ValueIndex),tmpY,MiddleZ);
  end;
end;

Function TVolumeSeries.NumSampleValues:Longint;
Begin
  result:=40; { same TOHLCSeries }
end;

Procedure TVolumeSeries.FillSampleValues(NumValues:Longint);
Var tmpX,
    StepX,
    tmpY,
    MinY,
    DifY  : Double; { temporal variables }
    t     : Longint;
Begin
  Clear; { delete all points }
  CalcRandomBounds(NumValues,tmpX,StepX,tmpY,MinY,DifY);  { get random limits }
  tmpY:=Random(Round(DifY) div 15);
  for t:=1 to NumValues do
  Begin
    tmpY:=Random(Round(DifY) div 15);
    AddXY(tmpX,tmpY{$IFNDEF D5},'', clTeeColor{$ENDIF});
    tmpX:=tmpX+StepX;
  end;
  RefreshSeries;
End;

Procedure TVolumeSeries.PrepareForGallery(IsEnabled:Boolean);
begin
  inherited PrepareForGallery(IsEnabled);
  FillSampleValues(26);
  Pointer.InflateMargins:=True;
end;

Function TVolumeSeries.GetEditorClass:String;
Begin
  result:='TVolumeSeriesEditor';
End;

Procedure TeeCandleExitProc; far;
begin
  UnRegisterTeeSeries([TCandleSeries,TVolumeSeries]);
end;

initialization
  RegisterTeeSeries( TCandleSeries, TeeMsg_GalleryCandle,
                     TeeMsg_GalleryExtended, 1);
  RegisterTeeSeries( TVolumeSeries, TeeMsg_GalleryVolume,
                     TeeMsg_GalleryExtended, 1);
{$IFDEF D1}
  AddExitProc(TeeCandleExitProc);
{$ENDIF}
{$IFNDEF D1}
finalization
  TeeCandleExitProc;
{$ENDIF}
end.
