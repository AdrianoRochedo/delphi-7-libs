{**********************************************}
{   TBubbleSeries (derived from TPointSeries)  }
{   Copyright (c) 1995-1996 by David Berneda   }
{**********************************************}
unit BubbleCh;

interface

{ TBubbleSeries derives from standard TPointSeries.
  Each point in the series is drawn like a Bubble.
  Each point has a Radius value that's used to draw the Bubble with its
  corresponding screen size.

  Inherits all functionality from TPointSeries and
  its ancestor (TCustomSeries).
}
Uses WinTypes,Classes,Graphics,Chart,Series,TeEngine; { <-- needed units }

Type
  TBubbleSeries=class(TPointSeries)
  private
    FRadiusValues : TChartValueList; { <-- Bubble's radius storage }
    FSquared      : Boolean;
    Procedure SetSquared(Value:Boolean);
    Procedure SetRadiusValues(Value:TChartValueList);
    Function ApplyRadius( Const Value:Double;
                          AList:TChartValueList;
                          Increment:Boolean):Double;
  protected
    procedure DrawValue(ValueIndex:Longint); override; { <-- main draw method }
  public
    Constructor Create(AOwner: TComponent); override;
    Function AddBubble(Const AX,AY,ARadius:Double
      {$IFNDEF D5};
      Const AXLabel:String; AColor:TColor
      {$ENDIF}):Longint;
    Procedure Assign(Source:TPersistent); override;
    Procedure DrawLegendShape(ValueIndex:Longint; Const Rect:TRect); override;
    Procedure FillSampleValues(NumValues:Longint); override; { <-- to add random radius values }
    Function IsValidSourceOf(Value:TChartSeries):Boolean; override;
    Function MaxYValue:Double; override;  { <-- adds radius }
    Function MinYValue:Double; override;  { <-- substracts radius }
  published
    property ColorEachPoint default True;
    property RadiusValues:TChartValueList read FRadiusValues write SetRadiusValues;
    property Squared:Boolean read FSquared write SetSquared default True;
  end;

implementation

Uses SysUtils,TeeProcs,TeeConst,TeCanvas;

{ TBubbleSeries }
Constructor TBubbleSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FRadiusValues:=TChartValueList.Create(Self,TeeMsg_ValuesBubbleRadius); { <-- radius storage }
  With Pointer do
  begin
    InflateMargins:=False;
    Style:=psCircle; { <-- a Bubble is a circle (by default) }
    AllowChangeSize:=False;
  end;
  Marks.Frame.Visible:=False;
  Marks.Transparent:=True;
  FSquared:=True;
  ColorEachPoint:=True;
end;

Procedure TBubbleSeries.SetSquared(Value:Boolean);
Begin
  SetBooleanProperty(FSquared,Value);
end;

Procedure TBubbleSeries.SetRadiusValues(Value:TChartValueList);
Begin
  SetChartValueList(FRadiusValues,Value); { standard method }
end;

{ Helper method, special to Bubble series }
Function TBubbleSeries.AddBubble( Const AX,AY,ARadius:Double
                                  {$IFNDEF D5};
                                  Const AXLabel:String; AColor:TColor
                                  {$ENDIF}):Longint;

Begin
  result:=AddXY(AX,AY {$IFNDEF D5},AXLabel,AColor {$ENDIF} ); { standard add X,Y }

  FRadiusValues.TempValue:=ARadius;
  AddValue(result);
end;

Procedure TBubbleSeries.FillSampleValues(NumValues:Longint);
Var t     : Longint;
    tmpX  : Double;
    tmpY  : Double;
    StepX : Double;
    MinY  : Double;
    DifY  : Double;
Begin
  Clear;
  CalcRandomBounds(NumValues,tmpX,StepX,tmpY,MinY,DifY);
  for t:=1 to NumValues do { some sample values to see something in design mode }
  Begin
    AddBubble( tmpX,                { X }
               Random(Round(DifY)), { Y }
               (DifY/10.0)+Round(DifY/(10+Random(15)))  { <- Radius }
               {$IFNDEF D5},'', clTeeColor{$ENDIF});
    tmpX:=tmpX+StepX;
  end;
  RefreshSeries;
end;

procedure TBubbleSeries.DrawValue(ValueIndex:Longint);
Var tmpSize : Longint;
Begin
  { This overrided method is the main paint for bubble points.
    The bubble effect is achieved by changing the Pointer.Size based
    on the corresponding Radius value for each point in the series.

    We dont use Pointer.Size:=... because that will force a repaint
    while we are painting !! giving recursive endlessly repaints !!!
  }
  tmpSize:=CalcYSizeValue(FRadiusValues.Value[ValueIndex]);
  if FSquared then
     Pointer.ChangeHorizSize(tmpSize)
  else
     Pointer.ChangeHorizSize(CalcXSizeValue(FRadiusValues.Value[ValueIndex]));
  Pointer.ChangeVertSize(tmpSize);
  DrawPointer(CalcXPos(ValueIndex),CalcYPos(ValueIndex),ValueColor[ValueIndex],ValueIndex);
  { dont call inherited }
end;

Function TBubbleSeries.ApplyRadius( Const Value:Double;
                                    AList:TChartValueList;
                                    Increment:Boolean):Double;
var t:Longint;
begin
  result:=Value;
  for t:=0 to Count-1 do
  if Increment then
     result:=MaxDouble(result,AList[t]+FRadiusValues.Value[t])
  else
     result:=MinDouble(result,AList[t]-FRadiusValues.Value[t]);
end;

Function TBubbleSeries.MaxYValue:Double;
Begin
  result:=ApplyRadius(inherited MaxYValue,YValues,True);
end;

Function TBubbleSeries.MinYValue:Double;
Begin
  result:=ApplyRadius(inherited MinYValue,YValues,False);
end;

Procedure TBubbleSeries.Assign(Source:TPersistent);
begin
  if Source is TBubbleSeries then
     FSquared:=TBubbleSeries(Source).FSquared;
  inherited Assign(Source);
end;

Function TBubbleSeries.IsValidSourceOf(Value:TChartSeries):Boolean;
begin
  result:=Value is TBubbleSeries; { Only Bubbles can be assigned to Bubbles }
end;

Procedure TBubbleSeries.DrawLegendShape(ValueIndex:Longint; Const Rect:TRect);
var tmp : Longint;
begin
  tmp:=MinLong((Rect.Right-Rect.Left),(Rect.Bottom-Rect.Top));
  Pointer.ChangeHorizSize(tmp);
  Pointer.ChangeVertSize(tmp);
  inherited DrawLegendShape(ValueIndex,Rect);
end;

initialization
  RegisterTeeSeries( TBubbleSeries, TeeMsg_GalleryBubble,
                     TeeMsg_GalleryStandard,2);
end.
