{**********************************************}
{   TeeChart Cross-Tab routines                }
{   Copyright (c) 1996-97 by David Berneda     }
{**********************************************}
unit teeCross;

interface

uses DB,Chart,Teengine,TeeProcs;

type TGroupFormula=(gfCount,gfSum);


{ This procedure will create an array of Chart Series.

  The array is determined by the "AGroupField" parameter.

  The "ASeries" parameter will be used to duplicate it many times,
  one for each "group".

  Example of use:
  ---------------

  Imagine you have a Table1 component with "product sales".

  In this Table1 component you have the following fields:

  Product    ( Cars, Bikes, Trucks... )
  Country    ( USA, UK, Germany, Australia... )
  Amount     ( $1234... )

  Now we want to create a Chart consisting of one Bar Series for
  each "Product", showing the sum of "Amount" for each "Country".

  So, our "GroupField" is "Product", our "LabelField" is "Country" and
  our "ValueField" is "Amount".

  The code is:

   ...
   begin
     FillDataSet( Table1, BarSeries1, "Product", "Country", "Amount", gfSum );
   end;


  After calling this procedure, the Chart will own several Series, one for
  each "Product".
  You can access and modify these Series as usually, like for example
  changing the Series Color, Title, etc.

}
Procedure FillDataSet( ADataSet:TDataSet;
                       ASeries:TChartSeries;
                       Const AGroupField,ALabelField,AValueField:String;
                       GroupFormula:TGroupFormula);

implementation



Procedure FillDataSet( ADataSet:TDataSet;
                       ASeries:TChartSeries;
                       Const AGroupField,ALabelField,AValueField:String;
                       GroupFormula:TGroupFormula);

   Function LocateSeries(Const ATitle:String):TChartSeries;
   var t:Integer;
   begin
     With ASeries.ParentChart do
     for t:=0 to SeriesCount-1 do
     if Series[t].Title=ATitle then
     begin
       result:=Series[t];
       exit;
     end;
     result:=nil;
   end;

   Function LocateLabel(tmpSeries:TChartSeries; Const ALabel:String):Integer;
   var t:Integer;
   begin
     With tmpSeries do
     for t:=0 to Count-1 do
     if XLabel[t]=ALabel then
     begin
       result:=t;
       exit;
     end;
     result:=-1;
   end;

var tmpGroup:String;
    tmpSeries:TChartSeries;
    tmpLabel:String;
    tmpValue:Double;
    t,tt,
    tmpPoint:Integer;
begin
  With ASeries.ParentChart do
  begin
    While SeriesCount>1 do
          if Series[SeriesCount-1]<>ASeries then Series[SeriesCount-1].Free;
    ASeries.Clear;
    ASeries.Title:='';
    With ADataSet do
    begin
      DisableControls;
      try
        First;
        While not eof do
        begin
          tmpGroup:=FieldByName(AGroupField).AsString;
          tmpSeries:=LocateSeries(tmpGroup);
          if tmpSeries=nil then
          begin
            if ASeries.Title='' then
               tmpSeries:=ASeries
            else
            begin
              tmpSeries:=CloneChartSeries(ASeries);
              tmpSeries.SeriesColor:=GetDefaultColor(SeriesCount);
            end;
            tmpSeries.Title:=tmpGroup;
          end;
          tmpLabel:=FieldByName(ALabelField).AsString;

          tmpValue:=FieldByName(AValueField).AsFloat;
          if GroupFormula=gfCount then tmpValue:=1;

          tmpPoint:=LocateLabel(tmpSeries,tmpLabel);
          if tmpPoint=-1 then
          begin
            tmpSeries.Add(tmpValue,tmpLabel,clTeeColor);
            for t:=0 to SeriesCount-1 do
            if Series[t]<>tmpSeries then
               if tmpSeries.Count>Series[t].Count then
               for tt:=1 to (tmpSeries.Count-Series[t].Count) do
                   Series[t].Add(0,tmpLabel,clTeeColor);
          end
          else
          begin
            With tmpSeries.MandatoryValueList do
            case GroupFormula of
              gfCount,
              gfSum: Value[tmpPoint]:=Value[tmpPoint]+tmpValue;
            end;
          end;
          Next;
        end;
      finally
        EnableControls;
      end;
    end;
  end;
end;

end.
 