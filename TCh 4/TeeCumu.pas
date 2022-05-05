{**********************************************}
{   TCumulative Function Component             }
{   Copyright (c) 1997-98 by David Berneda     }
{**********************************************}
{$I teedefs.inc}
unit teeCumu;

interface

{ The TCumulative function sums the Series values
  starting from the first point.

  Example:

  Given these values               :      1 2 3

  The TCumulative function returns :      1 3 6

                                     ( 1=1, 1+2=3 and 1+2+3=6 )
}

uses
  WinTypes,WinProcs, Messages, SysUtils, Classes, Graphics, TeEngine, Chart;

type
  TCumulative = class(TTeeFunction)
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent); override;
    Function Calculate(Series:TChartSeries; First,Last:Longint):Double; override;
    Function CalculateMany(SeriesList:TList; ValueIndex:Longint):Double;  override;
  published
    { Published declarations }
    property Period;
  end;

implementation

Uses TeeProco,TeeConst;  { <-- needed only for the "Functions" constant }

Constructor TCumulative.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  InternalSetPeriod(1);
end;

Function TCumulative.Calculate(Series:TChartSeries; First,Last:Longint):Double;
begin
  if First>0 then result:=ParentSeries.MandatoryValueList.Last
             else result:=0;
  if First>=0 then
     result:=result+ValueList(Series).Value[First];
end;

{ Returns the sum( ) of the current ValueIndex point of all Series PLUS the
  accumulated calculation of the previous ValueIndex point.
}
Function TCumulative.CalculateMany(SeriesList:TList; ValueIndex:Longint):Double;
var t:Longint;
    tmpList:TChartValueList;
begin
  if ValueIndex=0 then result:=0
                  else result:=ParentSeries.MandatoryValueList[ValueIndex-1];
  for t:=0 to SeriesList.Count-1 do
  begin
    tmpList:=ValueList(TChartSeries(SeriesList[t]));
    if tmpList.Count>ValueIndex then result:=result+tmpList[ValueIndex];
  end;
end;

{ Series/Functions Registration }
Procedure TeeCumuExitProc; far;
begin
  UnRegisterTeeFunctions([TCumulative]);
end;

initialization
  RegisterTeeFunction( TCumulative, TeeMsg_FunctionCumulative, TeeMsg_GalleryFunctions, 2 );
{$IFDEF D1}
  AddExitProc(TeeCumuExitProc);
{$ELSE}
finalization
  TeeCumuExitProc;
{$ENDIF}
end.
