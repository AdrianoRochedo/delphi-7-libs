{*********************************************}
{  TeeChart Grid functions                    }
{  Copyright (c) 1997 by David Berneda        }
{  All rights reserved                        }
{*********************************************}
unit Teegrid;

interface

Uses Grids,Chart;

{ This procedure fills the AGrid parameter with the AChart Series
  values and labels. }

type ChartGridOptions=(cgLabels,cgColors,cgXValues);
     TChartGridOptions=set of ChartGridOptions;

     TTeeGridColumnValues=Record
      ColLabels,
      ColColors,
      ColXValues,
      FirstColSeries:Integer;
     end;

Procedure TeeFillGrid( AGrid:TStringGrid;
                       AChart:TCustomChart;
                       Const AOptions:TChartGridOptions;
                       Var ColumnValues:TTeeGridColumnValues);

{ This procedure modifies a Series point in (ARow,ACol) with "value" }
Procedure TeeModifyGrid( AChart:TCustomChart;
                         ACol,ARow:Longint;
                         Const Value:String;
                         Const ColumnValues:TTeeGridColumnValues );
implementation

Uses TeeProcs,Teengine,SysUtils,Graphics;

Procedure TeeFillGrid( AGrid:TStringGrid;
                       AChart:TCustomChart;
                       Const AOptions:TChartGridOptions;
                       Var ColumnValues:TTeeGridColumnValues);

  Function GetMaxPoints:Integer;
  var t:Integer;
  begin
    result:=0;
    for t:=0 to AChart.SeriesCount-1 do
      result:=Max(result,AChart[t].Count);
  end;

Var ACol,ARow,ColSeries:Integer;
    ASeries:TChartSeries;
    tmp:TChartValueList;
    tmpColumnValues:TTeeGridColumnValues;
begin
  With AGrid do
  begin
    Cells[0,0]:='#';
    ACol:=1;
    With ColumnValues do
    begin
      ColLabels:=-1;
      ColColors:=-1;
      ColXValues:=-1;
      if cgLabels in AOptions then
      begin
        Cells[ACol,0]:='Label';
        ColLabels:=ACol;
        Inc(ACol);
      end;
      if cgColors in AOptions then
      begin
        Cells[ACol,0]:='Color';
        ColColors:=ACol;
        Inc(ACol);
      end;
      if cgXValues in AOptions then
      begin
        Cells[ACol,0]:='X';
        ColXValues:=ACol;
        Inc(ACol);
      end;
      FirstColSeries:=ACol;
    end;
    tmpColumnValues:=ColumnValues;
    ColCount:=ACol+AChart.SeriesCount;
    RowCount:=1+GetMaxPoints;
    for ACol:=0 to AChart.SeriesCount-1 do
    begin
      ASeries:=AChart[ACol];
      ColSeries:=tmpColumnValues.FirstColSeries+ACol;
      Cells[ColSeries,0]:=ASeries.Name;
      for ARow:=1 to ASeries.Count do
      begin
        Cells[0,ARow]:=IntToStr(ARow-1);   { point number }
        With tmpColumnValues do
        begin
          if ColLabels<>-1 then
             Cells[ColLabels,ARow]:=ASeries.XLabel[ARow-1];  { point label }
          if ColColors<>-1 then
             Cells[ColColors,ARow]:=IntToStr(ASeries.ValueColor[ARow-1]);  { point label }
          if ColXValues<>-1 then
          begin
            if ASeries.YMandatory then tmp:=ASeries.XValues
                                  else tmp:=ASeries.YValues;
            Cells[ColXValues,ARow]:=FloatToStr(tmp[ARow-1]);
          end;
        end;
        Cells[ColSeries,ARow]:=FloatToStr(ASeries.MandatoryValueList[ARow-1]);
      end;
      With tmpColumnValues do
      begin
        ColLabels:=-1;
        ColColors:=-1;
        ColXValues:=-1;
      end;
    end;
  end;
end;

Procedure TeeModifyGrid( AChart:TCustomChart;
                         ACol,ARow:Longint;
                         Const Value:String;
                         Const ColumnValues:TTeeGridColumnValues );
Var tmpColor:TColor;
begin
  if ARow=0 then exit;
  With ColumnValues do
  if ACol=ColLabels then
  begin
    if AChart.SeriesCount>0 then
       AChart[0].XLabel[ARow-1]:=Value;
  end
  else
  if ACol=ColColors then
  begin
    if AChart.SeriesCount>0 then
    begin
      if Value='' then tmpColor:=clTeeColor
                  else tmpColor:=StrToInt(Value);
      AChart[0].ValueColor[ARow-1]:=tmpColor;
    end;
  end
  else
  if ACol=ColXValues then
  begin
    if AChart.SeriesCount>0 then
      if (Value<>'') and (Value<>'-') then AChart[0].XValue[ARow-1]:=StrToFloat(Value);
  end
  else
  begin
    if AChart.SeriesCount>ACol-FirstColSeries then
    begin
      With AChart[ACol-FirstColSeries] do
      begin
        While Count<ARow do AddNull('');
        if Value='' then ValueColor[ARow-1]:=clNone
        else
        begin
          MandatoryValueList[ARow-1]:=StrToFloat(Value);
          if ValueColor[ARow-1]=clNone then
             ValueColor[ARow-1]:=clTeeColor;
        end;
      end;
    end;
  end;
  AChart.Repaint;
end;

end.
