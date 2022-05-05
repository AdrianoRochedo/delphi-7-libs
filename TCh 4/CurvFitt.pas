{****************************************************}
{   TCurveFittingFunction                            }
{   Copyright (c) 1995-1998 by David Berneda         }
{****************************************************}
{$I teedefs.inc}
unit CurvFitt;

interface

{ This unit shows how a new Chart Series component can be easily created.
  TCustomFittingFunction derives from standard TTeeFunction.

  TCurveFittingFunction and TTrendFunction both derive from
  TCustomFittingFunction.

  Based on a Polynomial degree value (# of polynomy items), a curve
  fitting is calculated for each X,Y pair value to determine the new
  Y position for each source X value.
}

Uses Classes, TeePoly, StatChar, Teengine;

Type
  TTypeFitting=( cfPolynomial
                 {$IFDEF TEEOCX}
                 ,cfLogarithmic
                 ,cfExponential
                 {$ENDIF}
                );

  TCustomFittingFunction=class(TTeeFunction)
  private
    FPolyDegree     : Integer; { <-- between 1 and 20 }
    FTypeFitting    : TTypeFitting;
    FFirstPoint     : Longint;
    FLastPoint      : Longint;
    FFirstCalcPoint : Longint;
    FLastCalcPoint  : Longint;
    { internal }
    IAnswerVector   : TDegreeVector;
    IMinYValue      : Double;
    Procedure SetPolyDegree(Value:Integer);
    Procedure SetTypeFitting(Value:TTypeFitting);
    Procedure SetFirstPoint(Value:Longint);
    Procedure SetLastPoint(Value:Longint);
    Procedure SetFirstCalcPoint(Value:Longint);
    Procedure SetLastCalcPoint(Value:Longint);
  protected
    Function GetAnswerVector(Index:Integer):Double;
    Procedure SetLongintProperty(Var Variable:Longint; Value:Longint);
    procedure AddFittedPoints(Source:TChartSeries); virtual;
  public
    Constructor Create(AOwner: TComponent); override;
    procedure AddPoints(Source:TChartSeries); override;
    { new }
    Function GetCurveYValue(Source:TChartSeries; Const X:Double):Double;
    property AnswerVector[Index:Integer]:Double read GetAnswerVector;
    property PolyDegree:Integer read FPolyDegree write SetPolyDegree default 5;
    property TypeFitting:TTypeFitting read FTypeFitting write SetTypeFitting default cfPolynomial;
    property FirstPoint:Longint read FFirstPoint write SetFirstPoint default -1;
    property LastPoint:Longint read FLastPoint write SetLastPoint default -1;
    property FirstCalcPoint:Longint read FFirstCalcPoint write SetFirstCalcPoint default -1;
    property LastCalcPoint:Longint read FLastCalcPoint write SetLastCalcPoint default -1;
  end;

  TCurveFittingFunction=class(TCustomFittingFunction)
  published
    property PolyDegree;
    property TypeFitting;
    property FirstPoint;
    property LastPoint;
    property FirstCalcPoint;
    property LastCalcPoint;
  end;

  TTrendFunction=class(TTeeFunction)
  protected
    Procedure CalculatePeriod( Source:TChartSeries;
                               Const tmpX:Double;
                               FirstIndex,LastIndex:Longint); override;
    Procedure CalculateAllPoints( Source:TChartSeries;
                                  NotMandatorySource:TChartValueList); override;
  public
    Function Calculate(SourceSeries:TChartSeries; First,Last:Longint):Double; override;
    Function CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;  override;
    Procedure CalculateTrend( Var m,b:Double; Source:TChartSeries;
                              FirstIndex,LastIndex:Longint);
  end;

implementation

Uses SysUtils,TeeProCo,Chart,TeeProcs,TeeConst,TeCanvas;

{ TCurveFittingFunction }
Constructor TCustomFittingFunction.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  CanUsePeriod:=False;
  InternalSetPeriod(1);
  FPolyDegree:=5;
  FTypeFitting:=cfPolynomial;
  FFirstPoint:=-1;
  FLastPoint:=-1;
  FFirstCalcPoint:=-1;
  FLastCalcPoint:=-1;
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
end;

Procedure TCustomFittingFunction.SetLongintProperty(Var Variable:Longint; Value:Longint);
Begin
  if Variable<>Value then
  Begin
    Variable:=Value;
    Recalculate;
  end;
end;

Procedure TCustomFittingFunction.SetFirstPoint(Value:Longint);
Begin
  SetLongintProperty(FFirstPoint,Value);
End;

Procedure TCustomFittingFunction.SetLastPoint(Value:Longint);
Begin
  SetLongintProperty(FLastPoint,Value);
End;

Procedure TCustomFittingFunction.SetFirstCalcPoint(Value:Longint);
Begin
  SetLongintProperty(FFirstCalcPoint,Value);
End;

Procedure TCustomFittingFunction.SetLastCalcPoint(Value:Longint);
Begin
  SetLongintProperty(FLastCalcPoint,Value);
End;

Procedure TCustomFittingFunction.SetTypeFitting(Value:TTypeFitting);
Begin
  if FTypeFitting<>Value then
  Begin
    FTypeFitting:=Value;
    Recalculate;
  end;
end;

Procedure TCustomFittingFunction.SetPolyDegree(Value:Integer);
Begin
  if FPolyDegree<>Value then
  begin
    if (Value<1) or (Value>20) then
       Raise Exception.Create(TeeMsg_PolyDegreeRange);
    FPolyDegree:=Value;
    Recalculate;
  end;
end;

Function TCustomFittingFunction.GetAnswerVector(Index:Integer):Double;
Begin
  if (Index<1) or (Index>FPolyDegree) then
     Raise Exception.CreateFmt(TeeMsg_AnswerVectorIndex,[FPolyDegree]);
  result:=IAnswerVector[Index];
End;

procedure TCustomFittingFunction.AddFittedPoints(Source:TChartSeries);
Var tmpX         : Double;
    tmpMinXValue : Double;
    t            : Longint;
    tmpStart     : Longint;
    tmpEnd       : Longint;
    AList        : TChartValueList;
begin
  AList:=ValueList(Source);
  With Source do
  begin
    tmpMinXValue:=XValues.MinValue;
    IMinYValue:=AList.MinValue;
    if FFirstPoint=-1 then tmpStart:=0
                      else tmpStart:=FFirstPoint;
    if FLastPoint=-1 then tmpEnd:=Count-1
                     else tmpEnd:=FLastPoint;
    for t:=tmpStart to tmpEnd do  { 1 to 1 relationship between source and self }
    begin
      tmpX:=XValue[t];
      ParentSeries.AddXY( tmpX, CalcFitting( FPolyDegree,
                                             IAnswerVector,
                                             tmpX-tmpMinXValue)+IMinYValue
                                             {$IFNDEF D5},'', clTeeColor{$ENDIF});
    end;
  end;
end;

procedure TCustomFittingFunction.AddPoints(Source:TChartSeries);
var t            : Longint;
    tmpStart     : Longint;
    tmpEnd       : Longint;
    tmpCount     : Longint;
    tmpPos       : Longint;
    IXVector     : PVector;
    IYVector     : PVector;
    tmpMinXValue : Double;
    AList        : TChartValueList;
Begin
  ParentSeries.Clear;
  With Source do
  if Count>=FPolyDegree then
  begin
    AList:=ValueList(Source);
    New(IXVector);
    try
      New(IYVector);
      try
        tmpMinXValue:=XValues.MinValue;
        IMinYValue:=AList.MinValue;
        if FFirstCalcPoint=-1 then tmpStart:=0
                              else tmpStart:=MaxLong(0,FFirstCalcPoint);
        if FLastCalcPoint=-1 then tmpEnd:=Count-1
                             else tmpEnd:=MinLong(Count-1,FLastCalcPoint);
        tmpCount:=(tmpEnd-tmpStart+1);
        for t:=1 to tmpCount do
        Begin
          tmpPos:=t+tmpStart-1;
          IXVector^[t]:=New(PFloat);
          PFloat(IXVector^[t])^:=XValue[tmpPos]-tmpMinXValue;
          IYVector^[t]:=New(PFloat);
          PFloat(IYVector^[t])^:=AList.Value[tmpPos]-IMinYValue;
        end;
        try
          PolyFitting(tmpCount,FPolyDegree,IXVector,IYVector,IAnswerVector);
          AddFittedPoints(Source);
        finally
          for t:=1 to tmpCount do
          begin
            Dispose(PFloat(IXVector^[t]));
            Dispose(PFloat(IYVector^[t]));
          end;
        end;
      finally
        Dispose(IYVector);
      end;
    finally
      Dispose(IXVector);
    end;
  end;
end;

{ calculates and returns the Y value corresponding to a X value }
Function TCustomFittingFunction.GetCurveYValue(Source:TChartSeries; Const X:Double):Double;
Begin
  result:=CalcFitting(FPolyDegree,IAnswerVector,X-Source.XValues.MinValue)+IMinYValue;
end;

{ TTrendFunction }
Function TTrendFunction.Calculate(SourceSeries:TChartSeries; First,Last:Longint):Double;
begin
  result:=0;
end;

Function TTrendFunction.CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;
begin
  result:=0;
end;

Procedure TTrendFunction.CalculateTrend(Var m,b:Double; Source:TChartSeries; FirstIndex,LastIndex:Longint);
var n       : Integer;
    t       : Integer;
    x       : Double;
    y       : Double;
    Divisor : Double;
    SumX    : Double;
    SumXY   : Double;
    SumY    : Double;
    SumX2   : Double;
    NotMandatory:TChartValueList;
begin
  if FirstIndex=TeeAllValues then n:=Source.Count
                             else n:=LastIndex-FirstIndex+1;
  if n>1 then
  With Source do
  begin
    if YMandatory then NotMandatory:=XValues
                  else NotMandatory:=YValues;
    if FirstIndex=TeeAllValues then
    begin
      SumX:=NotMandatory.Total;
      SumY:=ValueList(Source).Total;
    end
    else
    begin
      SumX:=0;
      SumY:=0;
    end;
    SumX2:=0;
    SumXY:=0;
    With ValueList(Source) do
    for t:=FirstIndex to LastIndex do
    begin
      x:=NotMandatory[t];
      y:=Value[t];
      SumXY:=SumXY+x*y;
      SumX2:=SumX2+Sqr(x);
      if FirstIndex<>TeeAllValues then
      begin
        SumX:=SumX+x;
        SumY:=SumY+y;
      end;
    end;
    Divisor:=n*SumX2-Sqr(SumX);
    if Divisor<>0 then
    begin
      m:=( (n*SumXY) - (SumX*SumY) ) / Divisor;
      b:=( (SumY*SumX2) - (SumX*SumXY) ) / Divisor;
    end
    else
    begin
      m:=1;
      b:=0;
    end;
  end;
end;

Procedure TTrendFunction.CalculatePeriod( Source:TChartSeries;
                                          Const tmpX:Double;
                                          FirstIndex,LastIndex:Longint);
Var m : Double;
    b : Double;

  Procedure AddPoint(Const Value:Double);
  begin
    ParentSeries.AddXY( Value, m*Value+b {$IFNDEF D5},'', clTeeColor{$ENDIF} );
  end;

Var n:Integer;
begin
  if FirstIndex=TeeAllValues then n:=Source.Count
                             else n:=LastIndex-FirstIndex+1;
  if n>1 then { minimum 2 points to calculate a trend }
  begin
    CalculateTrend(m,b,Source,FirstIndex,LastIndex);
    With Source do
    if YMandatory then
    begin
      AddPoint(XValues.Value[FirstIndex]);
      AddPoint(XValues.Value[LastIndex]);
    end
    else
    begin
      AddPoint(YValues.Value[FirstIndex]);
      AddPoint(YValues.Value[LastIndex]);
    end;
  end;
end;

Procedure TTrendFunction.CalculateAllPoints( Source:TChartSeries;
                                             NotMandatorySource:TChartValueList);
begin
  CalculatePeriod(Source,0,0,Source.Count-1);
end;

Procedure TeeCurvFittExitProc(); far;
begin
  UnRegisterTeeFunctions([ TCurveFittingFunction, TTrendFunction ]);
end;

initialization
  RegisterTeeBasicFunction( TCurveFittingFunction, TeeMsg_FunctionCurveFitting );
  RegisterTeeFunction( TTrendFunction, TeeMsg_FunctionTrend, TeeMsg_GalleryFunctions, 1 );
finalization
  TeeCurvFittExitProc();
end.
