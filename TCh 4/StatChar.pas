{*************************************}
{    TeeChart  Functions              }
{ Copyright (c) 1996 by David Berneda }
{    All Rights Reserved              }
{*************************************}
{$I teedefs.inc}
unit StatChar;

interface

Uses Classes, TeEngine;

Type { Abstract MovingTeeFunction for oscillators }
     TMovingTeeFunction=class(TTeeFunction)
     public
       Constructor Create(AOwner:TComponent); override;
     published
       property Period;
     end;

     { Moving Average }
     TMovingAverageFunction=class(TMovingTeeFunction)
     private
       FWeighted:Boolean;
       Procedure SetWeighted(Value:Boolean);
     public
       Function Calculate( Series:TChartSeries;
                           FirstIndex,LastIndex:Longint):Double; override;
       property Weighted:Boolean read FWeighted write SetWeighted default False;
     end;

     { RSI }
     TRSIFunction = class(TMovingTeeFunction)
     public
       Function Calculate( Series:TChartSeries;
                           FirstIndex,LastIndex:Longint):Double; override;
     end;

     { Exponential Average }
     TExpAverageFunction = class(TMovingTeeFunction)
     private
       FWeight:Double;
     protected
       Procedure SetWeight(Const Value:Double);
     public
       Constructor Create(AOwner:TComponent); override;
       Function Calculate( Series:TChartSeries;
                           FirstIndex,LastIndex:Longint):Double; override;
     published
       property Weight:Double read FWeight write SetWeight;
     end;

     { Momemtum }
     TMomentumFunction=class(TMovingTeeFunction)
     public
       Function Calculate( Series:TChartSeries;
                           FirstIndex,LastIndex:Longint):Double; override;
     end;

     { StdDeviation }
     TStdDeviationFunction=class(TTeeFunction)
     private
       FComplete : Boolean;
       ISum      : Double;
       ISum2     : Double;
       INumPoints: LongInt;
       Procedure SetComplete(Value:Boolean);
       Function CalculateDeviation:Double;
       Procedure Accumulate(Const Value:Double);
     public
       Function Calculate(SourceSeries:TChartSeries; FirstIndex,LastIndex:Longint):Double; override;
       Function CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;  override;
     published
       property Complete:Boolean read FComplete write SetComplete default False;
     end;

implementation

Uses SysUtils,TeeProcs,TeeConst,TeeProCo,Chart,TeCanvas;

{ MovingTeeFunction }
Constructor TMovingTeeFunction.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  MovingFunction:=True;
  InternalSetPeriod(1);
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
end;

{ MovingAverage }
Procedure TMovingAverageFunction.SetWeighted(Value:Boolean);
begin
  if FWeighted<>Value then
  begin
    FWeighted:=Value;
    Recalculate;
  end;
end;

Function TMovingAverageFunction.Calculate( Series:TChartSeries;
                                           FirstIndex,LastIndex:Longint):Double;
var t         : Longint;
    tmpSumX   : Double;
    tmpYValue : Double;
    tmpXValue : Double;
    tmpVList  : TChartValueList;
begin
  result:=0;
  tmpSumX:=0;
  tmpVList:=ValueList(Series);
  for t:=FirstIndex to LastIndex do
  begin
    tmpYValue:=tmpVList.Value[t];
    if FWeighted then
    Begin
      tmpXValue:=Series.XValue[t];
      result:=result+tmpYValue*tmpXValue;
      tmpSumX:=tmpSumX+tmpXValue;
    end
    else result:=result+tmpYValue;
  end;
  if FWeighted then
  begin
    if tmpSumX<>0 then result:=result/tmpSumX else result:=0;
  end
  else result:=result/(LastIndex-FirstIndex+1);
end;

{ R.S.I. }
Function TRSIFunction.Calculate( Series:TChartSeries;
                                 FirstIndex,LastIndex:Longint):Double;
var NumPoints : Longint;
    t         : Longint;
    tmpClose  : Double;
    Ups       : Double;
    Downs     : Double;
    Opens     : TChartValueList;
    Closes    : TChartValueList;
Begin
  With Series do
  Begin
    Opens :=GetYValueList('OPEN');  
    Closes:=GetYValueList('CLOSE');
    Ups:=0;
    Downs:=0;
    for t:=FirstIndex to LastIndex do
    Begin
      tmpClose:=Closes[t];
      if Opens[t]>tmpClose then Downs:=Downs+tmpClose
                           else Ups  :=Ups  +tmpClose;
    end;
    NumPoints:=(LastIndex-FirstIndex)+1;
    Downs:=Downs/NumPoints;
    Ups  :=Ups  /NumPoints;
    if Downs<>0 then
    Begin
      result:=100.0 - ( 100.0 / ( 1.0+Abs(Ups/Downs) ) );
      if result<0   then result:=0 else
      if result>100 then result:=100;
    end
    else result:=0;
  end;
end;

{ Exponential Average }
Constructor TExpAverageFunction.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FWeight:=0.2;
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
End;

Procedure TExpAverageFunction.SetWeight(Const Value:Double);
Begin
  if (Value<0) or (Value>1) then
     raise Exception.Create(TeeMsg_ExpAverageWeight);
  if FWeight<>Value then
  begin
    FWeight:=Value;
    Recalculate;
  end;
End;

Function TExpAverageFunction.Calculate( Series:TChartSeries;
                                        FirstIndex,LastIndex:Longint):Double;
Begin
  With ValueList(Series) do
  begin
    result:=Value[LastIndex];
    if LastIndex>0 then result:=Value[LastIndex-1]*(1.0-FWeight)+result*FWeight;
  end;
end;

{ Momentum }
Function TMomentumFunction.Calculate( Series:TChartSeries;
                                      FirstIndex,LastIndex:Longint):Double;
Begin
  if FirstIndex=TeeAllValues then
  begin
    FirstIndex:=0;
    LastIndex:=Series.Count-1;
  end;
  With ValueList(Series) do result:=Value[LastIndex]-Value[FirstIndex];
End;

{ StdDeviation }
Function TStdDeviationFunction.CalculateDeviation:Double;
Var Divisor:Double;
begin
  if Complete then Divisor:=Sqr(INumPoints)
              else Divisor:=INumPoints*(INumPoints-1);
  result:=Sqrt( ((INumPoints*ISum2) - Sqr(ISum)) / Divisor );
end;

Procedure TStdDeviationFunction.Accumulate(Const Value:Double);
begin
  ISum:=ISum+Value;
  ISum2:=ISum2+Sqr(Value);
end;

Function TStdDeviationFunction.Calculate(SourceSeries:TChartSeries; FirstIndex,LastIndex:Longint):Double;
var t:LongInt;
begin
  if FirstIndex=TeeAllValues then
  begin
    FirstIndex:=0;
    INumPoints:=SourceSeries.Count;
    LastIndex:=INumPoints-1;
  end
  else INumPoints:=LastIndex-FirstIndex+1;
  if INumPoints>1 then
  begin
    ISum2:=0;
    ISum:=0;
    With ValueList(SourceSeries) do
    for t:=FirstIndex to LastIndex do Accumulate(Value[t]);
    result:=CalculateDeviation;
  end
  else result:=0;
end;

Function TStdDeviationFunction.CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;
var t:Integer;
begin
  if SourceSeriesList.Count>0 then
  begin
    INumPoints:=0;
    ISum2:=0;
    ISum:=0;
    for t:=0 to SourceSeriesList.Count-1 do
    begin
      With ValueList(TChartSeries(SourceSeriesList[t])) do
      if Count>ValueIndex then
      begin
        Accumulate(Value[ValueIndex]);
        Inc(INumPoints);
      end;
    end;
    if INumPoints>1 then result:=CalculateDeviation
                    else result:=0;
  end
  else result:=0;
end;

Procedure TStdDeviationFunction.SetComplete(Value:Boolean);
begin
  if FComplete<>Value then
  begin
    FComplete:=Value;
    Recalculate;
  end;
end;

Procedure TeeStatsExitProc; far;
begin
  UnRegisterTeeFunctions([ TMovingAverageFunction,
                           TRSIFunction,
                           TExpAverageFunction,
                           TMomentumFunction,
                           TStdDeviationFunction ]);
end;

initialization
  RegisterTeeBasicFunction( TMovingAverageFunction, TeeMsg_FunctionMovingAverage );
  RegisterTeeBasicFunction( TExpAverageFunction,    TeeMsg_FunctionExpAverage );
  RegisterTeeBasicFunction( TRSIFunction,           TeeMsg_FunctionRSI );
  RegisterTeeBasicFunction( TMomentumFunction,      TeeMsg_FunctionMomentum );
  RegisterTeeBasicFunction( TStdDeviationFunction,  TeeMsg_FunctionStdDeviation );
{$IFDEF D1}
  AddExitProc(TeeStatsExitProc);
{$ENDIF}
{$IFNDEF D1}
finalization
  TeeStatsExitProc;
{$ENDIF}
end.
