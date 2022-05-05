{**********************************************}
{   TCountTeeFunction Component                }
{   Copyright (c) 1996-1998 by David Berneda   }
{**********************************************}
{$I teedefs.inc}
unit TeeCount;

interface

Uses Classes,Graphics,Teengine,Chart,Series;

Type TCountTeeFunction=class(TTeeFunction)
     public
       Function Calculate(Series:TChartSeries; First,Last:Longint):Double; override;
       Function CalculateMany(SeriesList:TList; ValueIndex:Longint):Double;  override;
     end;

implementation

Uses SysUtils,TeeConst,TeeProco;

Function TCountTeeFunction.Calculate(Series:TChartSeries; First,Last:Longint):Double;
begin
  if First=TeeAllValues then result:=ValueList(Series).Count
                        else result:=Last-First+1;
end;

Function TCountTeeFunction.CalculateMany(SeriesList:TList; ValueIndex:Longint):Double;
var t:Longint;
begin
  result:=0;
  for t:=0 to SeriesList.Count-1 do
    if ValueList(TChartSeries(SeriesList[t])).Count>ValueIndex then result:=result+1;
end;

{ Series/Functions Registration }
Procedure TeeCountExitProc; far;
begin
  UnRegisterTeeFunctions([TCountTeeFunction]);
end;

initialization
  RegisterTeeBasicFunction(TCountTeeFunction, TeeMsg_FunctionCount);
{$IFDEF D1}
  AddExitProc(TeeCountExitProc);
{$ENDIF}
{$IFNDEF D1}
finalization
  TeeCountExitProc;
{$ENDIF}
end.
