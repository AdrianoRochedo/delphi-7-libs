{*************************************}
{    TeeChart Functions               }
{ Copyright (c) 1996 by David Berneda }
{    All Rights Reserved              }
{*************************************}
{$I teedefs.inc}
unit TeeFunci;

interface

Uses Classes,TeEngine;

Type TBasicTeeFunction=class(TTeeFunction)
     end;

     TAddTeeFunction=class(TTeeFunction)
     public
       Function Calculate(SourceSeries:TChartSeries; FirstIndex,LastIndex:Longint):Double; override;
       Function CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;  override;
     end;

     TManySeriesTeeFunction=class(TTeeFunction)
     protected
       Function CalculateValue(Const AResult,AValue:Double):Double; virtual; abstract;
     public
       Function CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;  override;
     end;

     TSubtractTeeFunction=class(TManySeriesTeeFunction)
     protected
       Function CalculateValue(Const AResult,AValue:Double):Double; override;
     end;

     TMultiplyTeeFunction=class(TManySeriesTeeFunction)
     protected
       Function CalculateValue(Const AResult,AValue:Double):Double; override;
     end;

     TDivideTeeFunction=class(TManySeriesTeeFunction)
     protected
       Function CalculateValue(Const AResult,AValue:Double):Double; override;
     end;

     THighTeeFunction=class(TTeeFunction)
     public
       Function Calculate(SourceSeries:TChartSeries; FirstIndex,LastIndex:Longint):Double; override;
       Function CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;  override;
     end;

     TLowTeeFunction=class(TTeeFunction)
     public
       Function Calculate(SourceSeries:TChartSeries; FirstIndex,LastIndex:Longint):Double; override;
       Function CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;  override;
     end;

     TAverageTeeFunction=class(TTeeFunction)
     public
       Function Calculate(SourceSeries:TChartSeries; FirstIndex,LastIndex:Longint):Double; override;
       Function CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;  override;
     end;

implementation

Uses SysUtils,TeeProcs,TeeConst;

{ Add }
Function TAddTeeFunction.Calculate(SourceSeries:TChartSeries; FirstIndex,LastIndex:Longint):Double;
var t:Longint;
begin
  With ValueList(SourceSeries) do
  if FirstIndex=TeeAllValues then result:=Total
  else
  begin
    result:=0;
    for t:=FirstIndex to LastIndex do result:=result+Value[t];
  end;
end;

Function TAddTeeFunction.CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;
var t:Longint;
    tmpList:TChartValueList;
begin
  result:=0;
  for t:=0 to SourceSeriesList.Count-1 do
  begin
    tmpList:=ValueList(TChartSeries(SourceSeriesList[t]));
    if tmpList.Count>ValueIndex then result:=result+tmpList[ValueIndex];
  end;
end;

{ ManySeriesFunction }
Function TManySeriesTeeFunction.CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;
var tmpList  : TChartValueList;
    tmpFirst : Boolean;
    t        : Longint;
begin
  tmpFirst:=True;
  result:=0;
  for t:=0 to SourceSeriesList.Count-1 do
  begin
    tmpList:=ValueList(TChartSeries(SourceSeriesList[t]));
    if tmpList.Count>ValueIndex then
    begin
      if tmpFirst then
      begin
        result:=tmpList[ValueIndex];
        tmpFirst:=False;
      end
      else result:=CalculateValue(result,tmpList[ValueIndex]);
    end
  end
end;

{ Subtract }
Function TSubtractTeeFunction.CalculateValue(Const AResult,AValue:Double):Double;
begin
  result:=AResult-AValue;
end;

{ Multiply }
Function TMultiplyTeeFunction.CalculateValue(Const AResult,AValue:Double):Double;
begin
  result:=AResult*AValue;
end;

{ Divide }
Function TDivideTeeFunction.CalculateValue(Const AResult,AValue:Double):Double;
begin
  if AValue=0 then result:=AResult
              else result:=AResult/AValue;
end;

{ High }
Function THighTeeFunction.Calculate(SourceSeries:TChartSeries; FirstIndex,LastIndex:Longint):Double;
var t:Longint;
    tmp:Double;
begin
  With ValueList(SourceSeries) do
  if FirstIndex=TeeAllValues then result:=MaxValue
  else
  begin
    result:=0;
    for t:=FirstIndex to LastIndex do
    begin
      tmp:=Value[t];
      if t=FirstIndex then result:=tmp else
      if tmp>result then result:=tmp;
    end;
  end;
end;

Function THighTeeFunction.CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;
var t:Longint;
    tmp:Double;
    tmpList:TChartValueList;
begin
  result:=0;
  for t:=0 to SourceSeriesList.Count-1 do
  begin
    tmpList:=ValueList(TChartSeries(SourceSeriesList[t]));
    if tmpList.Count>ValueIndex then
    begin
      tmp:=tmpList[ValueIndex];
      if (t=0) or (tmp>result) then result:=tmp;
    end;
  end;
end;

{ Low }
Function TLowTeeFunction.Calculate(SourceSeries:TChartSeries; FirstIndex,LastIndex:Longint):Double;
var t:Longint;
    tmp:Double;
begin
  With ValueList(SourceSeries) do
  if FirstIndex=TeeAllValues then result:=MinValue
  else
  begin
    result:=0;
    for t:=FirstIndex to LastIndex do
    begin
      tmp:=Value[t];
      if t=FirstIndex then result:=tmp else
      if tmp<result then result:=tmp;
    end;
  end;
end;

Function TLowTeeFunction.CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;
var t:Longint;
    tmp:Double;
    tmpList:TChartValueList;
begin
  result:=0;
  for t:=0 to SourceSeriesList.Count-1 do
  begin
    tmpList:=ValueList(TChartSeries(SourceSeriesList[t]));
    if tmpList.Count>ValueIndex then
    begin
      tmp:=tmpList[ValueIndex];
      if (t=0) or (tmp<result) then result:=tmp;
    end;
  end;
end;

{ Average }
Function TAverageTeeFunction.Calculate(SourceSeries:TChartSeries; FirstIndex,LastIndex:Longint):Double;
var t:Longint;
begin
  if FirstIndex=TeeAllValues then
  With SourceSeries do
  begin
    if Count>0 then result:=ValueList(SourceSeries).Total/Count
               else result:=0;
  end
  else
  begin
    result:=0;
    With ValueList(SourceSeries) do
    for t:=FirstIndex to LastIndex do result:=result+Value[t];
    result:=result/(LastIndex-FirstIndex+1);
  end;
end;

Function TAverageTeeFunction.CalculateMany(SourceSeriesList:TList; ValueIndex:Longint):Double;
var t,Counter:Longint;
    tmpList:TChartValueList;
begin
  result:=0;
  if SourceSeriesList.Count>0 then
  begin
    Counter:=0;
    for t:=0 to SourceSeriesList.Count-1 do
    begin
      tmpList:=ValueList(TChartSeries(SourceSeriesList[t]));
      if tmpList.Count>ValueIndex then
      begin
        Inc(Counter);
        result:=result+tmpList[ValueIndex];
      end;
    end;
    if Counter>0 then result:=result/Counter;
  end;
end;

end.
