{**********************************************}
{  TCustomDBChart (or derived) Editor Dialog   }
{  Copyright (c) 1995-1998 by David Berneda    }
{  All Rights Reserved                         }
{**********************************************}
{$I teedefs.inc}
unit DBEditCh;

interface

Uses {$IFDEF D1}IEdit16{$ELSE}IEditCha,IEdiSeri;{$ENDIF}

{$IFNDEF D1}
Var InternalOnGetDesignerNames:TOnGetDesignerNamesEvent;
{$ENDIF}

implementation

uses
  WinTypes,WinProcs, Messages, SysUtils, Classes, Graphics, Controls,
  Dialogs, DB, DBTables, StdCtrls, Teengine, TeeConst, Chart, DBChart,
  TeeProcs;

{$IFNDEF D1}
Function OnDBChartGetSourceStyle(ASeries:TChartSeries):TTeeDataSourceStyle;
begin
  result:=dsStyle_NoData;
  if Assigned(ASeries.DataSource) then
     if ASeries.DataSource is TDataSet then result:=dsStyle_Dataset
                                       else
     if ASeries.DataSource is TDataSource then result:=dsStyle_Datasource;
end;
{$ENDIF}

Procedure OnDBChartFillFields( AForm:{$IFDEF D1}TChartEditForm{$ELSE}TFormTeeSeries{$ENDIF}); far;
Var tmpList : TStrings;
    tmp     : TComponent;

  Procedure AddField(Const tmpSt:String; tmpType:TFieldType);
  Var tt:Longint;
  begin
    With AForm do
    Case tmpType of
      ftSmallint,
      ftInteger,
      ftWord,
      ftFloat,
      ftCurrency,
      ftBCD,
      ftDate,
      ftTime,
      ftDateTime: begin
                    tmpList.Add(tmpSt);
                    if tmp is TDataSet then
                    for tt:=0 to TheSeries.ValuesLists.Count-1 do
                        GetSourceCombo(tt).Items.Add(tmpSt);
                  end;
      ftString: if tmp is TDataSet then tmpList.Add(tmpSt);
    end;
  end;

Var t       : Longint;
    tmpData : TDataSet;
    tmpSt   : String;
    tmpField: String;
Begin
  With AForm do
  begin
    tmp:=GetSelectedSource;
    if Assigned(TheSeries) and
       Assigned(tmp) then
    begin
      tmpData:=nil;
      if (tmp is TDataSet) then
      begin
        tmpData:=TDataSet(tmp);
        tmpList:=CBLabelsField.Items;
      end
      {$IFNDEF D1}
      else
      if (tmp is TDataSource) then
      begin
        tmpData:=TDataSource(tmp).DataSet;
        tmpList:=LBAvailFields.Items;
      end
      {$ENDIF};
      tmpList.Clear;
      if Assigned(tmpData) then
      With tmpData do
      begin
        if FieldCount>0 then
           for t:=0 to FieldCount-1 do
               AddField(Fields[t].FieldName,Fields[t].DataType)
        else
        begin
          FieldDefs.Update;
          for t:=0 to FieldDefs.Count-1 do
              AddField(FieldDefs[t].Name,FieldDefs[t].DataType);
        end;
      end;
      {$IFNDEF D1}
      if (tmp is TDataSource) then
      begin
        tmpSt:=TheSeries.MandatoryValueList.ValueSource;
        LBSelFields.Clear;
        for t:=1 to TeeNumFields(tmpSt) do
        begin
          tmpField:=TeeExtractField(tmpSt,t);
          LBSelFields.Items.Add(tmpField);
          With LBAvailFields.Items do Delete(IndexOf(tmpField));
        end;
      end;
      {$ENDIF}
    end;
  end;
end;

Procedure OnDBChartIsDateTimeSource( AComponent:TComponent;
                                     Const ValueSource:String;
                                     Var IsDateTime:Boolean); far;
var tmpField:TField;
begin
  IsDateTime:=False;
  if AComponent is TDataSet then
  begin
    tmpField:=TDataSet(AComponent).FindField(ValueSource);
    if Assigned(tmpField) then
    Case tmpField.DataType of
      ftDate,ftTime,ftDateTime: IsDateTime:=True;
    end;
  end;
end;

Function OnDBChartIsValidComponentSource( AComponent:TComponent
                                          {$IFNDEF D1}
                                          ; DataSourceStyle:TTeeDataSourceStyle
                                          {$ENDIF}):Boolean; far;
begin
  result:={$IFNDEF D1}((DataSourceStyle=dsStyle_DataSet) and{$ENDIF} (AComponent is TDataSet)
{$IFNDEF D1}
   )
    or
    ((DataSourceStyle=dsStyle_DataSource) and (AComponent is TDataSource))
{$ENDIF}
    ;
end;

Procedure OnCreateEditDBChart(AForm:{$IFDEF D1}TChartEditForm{$ELSE}TFormTeeSeries{$ENDIF}; AChart:TCustomChart); far;
var tmp:Integer;
begin
  With AForm do
  begin
    if AChart is TCustomDBChart then
    begin
      With CBDataSourceStyle.Items do
      begin
        if IndexOf(TeeMsg_DataSet)=-1 then Add(TeeMsg_Dataset);
        {$IFNDEF D1}
        if IndexOf(TeeMsg_SingleRecord)=-1 then Add(TeeMsg_SingleRecord);
        {$ENDIF}
      end;
      OnChartFillFields:=OnDBChartFillFields;
      {$IFNDEF D1}
      OnChartGetSourceStyle:=OnDBChartGetSourceStyle;
      {$ENDIF}
      OnChartIsValidComponentSource:=OnDBChartIsValidComponentSource;
      OnChartIsDateTimeSource:=OnDBChartIsDateTimeSource;
      {$IFNDEF D1}
      if Assigned(InternalOnGetDesignerNames) then
         OnGetDesignerNames:=InternalOnGetDesignerNames;
      {$ENDIF}
    end
    else
    begin
      With CBDataSourceStyle.Items do
      begin
        tmp:=IndexOf(TeeMsg_DataSet);
        if tmp<>-1 then Delete(tmp);
        tmp:=IndexOf(TeeMsg_SingleRecord);
        if tmp<>-1 then Delete(tmp);
      end;
      OnChartFillFields:=nil;
      {$IFNDEF D1}
      OnChartGetSourceStyle:=nil;
      {$ENDIF}
      OnChartIsValidComponentSource:=nil;
      OnChartIsDateTimeSource:=nil;
      {$IFNDEF D1}
      OnGetDesignerNames:=nil;
      {$ENDIF}
    end;
  end;
end;

initialization
{$IFNDEF D1}
  InternalOnGetDesignerNames:=nil;
{$ENDIF}
  InternalOnCreateEditSeries:=OnCreateEditDBChart;
end.
