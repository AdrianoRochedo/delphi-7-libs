{**********************************************}
{   TCountTeeFunction Component                }
{   Copyright (c) 1996 by David Berneda        }
{      Series Developer Kit Example            }
{**********************************************}
unit TeeCount;

interface

Uses Classes,Graphics,Teengine,Chart,Series;

Type TCountTeeFunction=class(TTeeFunction)
     public
       Function Calculate(Series:TChartSeries; First,Last:Longint):Double; override;
       Function CalculateMany(SeriesList:TList; ValueIndex:Longint):Double;  override;
     end;

implementation

Uses SysUtils;

Function TCountTeeFunction.Calculate(Series:TChartSeries; First,Last:Longint):Double;
begin
  if First=TeeAllValues then result:=Series.MandatoryValueList.Count
                        else result:=Last-First+1;
end;

Function TCountTeeFunction.CalculateMany(SeriesList:TList; ValueIndex:Longint):Double;
var t:Longint;
    tmpList:TChartValueList;
begin
  result:=0;
  for t:=0 to SeriesList.Count-1 do
  begin
    tmpList:=TChartSeries(SeriesList[t]).MandatoryValueList;
    if tmpList.Count>ValueIndex then result:=result+1;
  end;
end;

{ Series/Functions Registration }
Procedure TeeCountExitProc; far;
begin
  UnRegisterTeeFunctions([TCountTeeFunction]);
end;

initialization
  RegisterTeeFunction( TCountTeeFunction, 'Count', 'Samples', 2 );
{$IFNDEF WIN32}
  AddExitProc(TeeCountExitProc);
{$ENDIF}
{$IFDEF WIN32}
finalization
  TeeCountExitProc;
{$ENDIF}
end.
