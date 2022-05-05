{**********************************************}
{   TArrowSeries (derived from TPointSeries)   }
{   Copyright (c) 1995-1996 by David Berneda   }
{**********************************************}
unit ArrowCha;

interface

{ This unit shows how a new Chart Series component can be easily created.
  TArrowSeries derives from standard TPointSeries.
  Each point in the series is drawn like an Arrow.
  Each arrow has initial and end X,Y values that are used to draw the
  Arrow with its corresponding screen size.

  Inherits all functionality from TPointSeries and
  its ancestor (TCustomSeries).
}
Uses WinTypes,Classes,Graphics,Chart,Series,TeEngine; { <-- needed units }

Type
  TArrowSeries=class(TPointSeries)
  private
    FEndXValues : TChartValueList;
    FEndYValues : TChartValueList; { <-- Arrows end X,Y values }
    Procedure SetEndXValues(Value:TChartValueList);
    Procedure SetEndYValues(Value:TChartValueList);
    Function GetArrowHeight:Integer;
    procedure SetArrowHeight(Value:Integer);
    Function GetArrowWidth:Integer;
    procedure SetArrowWidth(Value:Integer);
    Function GetStartXValues:TChartValueList;
    Procedure SetStartXValues(Value:TChartValueList);
    Function GetStartYValues:TChartValueList;
    Procedure SetStartYValues(Value:TChartValueList);
  protected
    procedure DrawValue(ValueIndex:Longint); override; { <-- main draw method }
  public
    Constructor Create(AOwner: TComponent); override;
    Function AddArrow(Const X0,Y0,X1,Y1:Double
                      {$IFNDEF D5};
                      Const ALabel:String; AColor:TColor
                      {$ENDIF}):Longint;
    Procedure FillSampleValues(NumValues:Longint); override; { <-- to add random arrow values }
    Function GetEditorClass:String; override;
    Function IsValidSourceOf(Value:TChartSeries):Boolean; override;
    Function MaxXValue:Double; override;
    Function MinXValue:Double; override;
    Function MaxYValue:Double; override;
    Function MinYValue:Double; override;
    Procedure PrepareForGallery(IsEnabled:Boolean); override;
  published
    property ArrowHeight:Integer read GetArrowHeight write SetArrowHeight stored False;
    property ArrowWidth:Integer read GetArrowWidth write SetArrowWidth stored False;
    property EndXValues:TChartValueList read FEndXValues write SetEndXValues;
    property EndYValues:TChartValueList read FEndYValues write SetEndYValues;
    property StartXValues:TChartValueList read GetStartXValues write SetStartXValues;
    property StartYValues:TChartValueList read GetStartYValues write SetStartYValues;
  end;

implementation

Uses SysUtils,TeeProcs,TeeConst,TeCanvas;

{ TArrowSeries }
Constructor TArrowSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  CalcVisiblePoints:=False;
  XValues.DateTime:=True;
  FEndXValues :=TChartValueList.Create(Self,TeeMsg_ValuesArrowEndX);
  FEndXValues.DateTime:=True;
  FEndYValues :=TChartValueList.Create(Self,TeeMsg_ValuesArrowEndY);
  Pointer.InflateMargins:=False;
  Marks.Frame.Visible:=False;
  Marks.Transparent:=True;
end;

Procedure TArrowSeries.SetEndXValues(Value:TChartValueList);
Begin
  SetChartValueList(FEndXValues,Value); { standard method }
end;

Procedure TArrowSeries.SetEndYValues(Value:TChartValueList);
Begin
  SetChartValueList(FEndYValues,Value); { standard method }
end;

{ Helper method, special to Arrow series }
Function TArrowSeries.AddArrow( Const X0,Y0,X1,Y1:Double
                                {$IFNDEF D5};
                                Const ALabel:String; AColor:TColor
                                {$ENDIF}):Longint;
Begin
  result:=AddXY(X0,Y0{$IFNDEF D5},ALabel,AColor{$ENDIF}); { standard add X,Y }
  FEndXValues.TempValue:=X1;
  FEndYValues.TempValue:=Y1;
  AddValue(result);
end;

Procedure TArrowSeries.FillSampleValues(NumValues:Longint);
Var tmpDifX : Longint;
    tmpDifY : Longint;
    t       : Longint;
    tmpX0   : Double;
    tmpY0   : Double;
    tmpX    : Double;
    tmpY    : Double;
    StepX   : Double;
    MinY    : Double;
    DifY    : Double;
Begin
  Clear;
  CalcRandomBounds(NumValues,tmpX,StepX,tmpY,MinY,DifY);
  tmpDifY:=Round(DifY);
  for t:=1 to NumValues do { some sample values to see something in design mode }
  Begin
    tmpDifX :=Round(StepX*NumValues);
    tmpX0   :=tmpX+Random(tmpDifX);
    tmpY0   :=MinY+Random(tmpDifY);
    AddArrow( tmpX0,tmpY0,
              tmpX0+Random(tmpDifX),   { X1 }
              tmpY0+Random(tmpDifY)   { Y1 }
              {$IFNDEF D5},'', clTeeColor{$ENDIF});
  end;
  RefreshSeries;
end;

Function TArrowSeries.GetArrowWidth:Integer;
Begin
  result:=Pointer.HorizSize;
end;

Function TArrowSeries.GetArrowHeight:Integer;
Begin
  result:=Pointer.VertSize;
end;

procedure TArrowSeries.SetArrowWidth(Value:Integer);
Begin
  Pointer.HorizSize:=Value;
End;

procedure TArrowSeries.SetArrowHeight(Value:Integer);
Begin
  Pointer.VertSize:=Value;
End;

procedure TArrowSeries.DrawValue(ValueIndex:Longint);
Var p0       : TPoint;
    p1       : TPoint;
    tmpColor : TColor;
Begin
       { This overrided method is the main paint for Arrow points. }
  P0.x:=CalcXPos(ValueIndex);
  P0.y:=CalcYPos(ValueIndex);
  P1.x:=CalcXPosValue(FEndXValues.Value[ValueIndex]);
  P1.y:=CalcYPosValue(FEndYValues.Value[ValueIndex]);
  tmpColor:=ValueColor[ValueIndex];
  With ParentChart do
  begin
    if View3D then Pointer.PrepareCanvas(tmpColor)
    else
    With Canvas do
    begin
      Pen.Assign(Pointer.Pen);
      Pen.Color:=tmpColor;
    end;
    Canvas.Arrow(View3D,P0,P1,ArrowWidth,ArrowHeight,MiddleZ);
  end;
end;

Function TArrowSeries.MaxXValue:Double;
Begin
  result:=MaxDouble(inherited MaxXValue,FEndXValues.MaxValue);
end;

Function TArrowSeries.MinXValue:Double;
Begin
  result:=MinDouble(inherited MinXValue,FEndXValues.MinValue);
end;

Function TArrowSeries.MaxYValue:Double;
Begin
  result:=MaxDouble(inherited MaxYValue,FEndYValues.MaxValue);
end;

Function TArrowSeries.MinYValue:Double;
Begin
  result:=MinDouble(inherited MinYValue,FEndYValues.MinValue);
end;

Function TArrowSeries.GetEditorClass:String;
Begin
  result:='TArrowSeriesEditor'; { <-- dont translate ! }
end;

Function TArrowSeries.GetStartXValues:TChartValueList;
Begin
  result:=XValues;
End;

Procedure TArrowSeries.SetStartXValues(Value:TChartValueList);
Begin
  SetXValues(Value);
End;

Function TArrowSeries.GetStartYValues:TChartValueList;
Begin
  result:=YValues;
End;

Procedure TArrowSeries.SetStartYValues(Value:TChartValueList);
Begin
  SetYValues(Value);
End;

Procedure TArrowSeries.PrepareForGallery(IsEnabled:Boolean);
Begin
  inherited PrepareForGallery(IsEnabled);
  FillSampleValues(3);
  ArrowWidth :=12;
  ArrowHeight:=12;
  if not IsEnabled then SeriesColor:=clGray;
end;

Function TArrowSeries.IsValidSourceOf(Value:TChartSeries):Boolean;
begin
  result:=Value is TArrowSeries;
end;

initialization
  RegisterTeeSeries(TArrowSeries,TeeMsg_GalleryArrow,TeeMsg_GalleryStandard,2);
end.
