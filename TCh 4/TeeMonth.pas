{****************************************}
{       TeeChart Charting Library        }
{ Copyright (c) 1996-98 by David Berneda }
{          All Rights Reserved           }
{****************************************}
{$I teedefs.inc}
unit TeeMonth;

interface

Uses Teengine,DB,
     {$IFNDEF D3}
     DBTables,
     {$ENDIF}
     TeeProcs;

{ This unit fills a Series with points.

  The points are obtained calculating a COUNT 
  or SUM of a Field in a Database Table or Query.

  The COUNT or SUM is performed BY MONTH or BY WEEK
  over a DateTime field in the database.

  The parameters are as follows:

  ASeries          The Series to add the points.
  ATable           The Database to scan.
  DateFieldName    The name of the DateTime field in the table.
  ValueFieldName   The name of the field in the table to SUM. (if Count = False)
  DatePeriod       It can be:  dtOneMonth or dtOneWeek
  Count            Is TRUE if we want to COUNT records. FALSE if we want to SUM.

}
procedure DBMonthlySeries( ASeries:TChartSeries;
                           ATable:TDataSet;
                           Const DateFieldName,
                                 ValueFieldName:String;
                           DatePeriod:TDateTimeStep;
                           Count:Boolean);

{ Returns the number of the week, from 1 to 52.
  It also returns the Year, which can be the same year, the
  year before or the year after, depending on which day of the week
  the year starts.
  The "FirstDay" constant sets weeks to start at Monday.
}
Function DateToWeek(Const ADate:TDateTime; Var Year:Word):Integer;

Type
  TFilterRecordProc=procedure (Sender:TDataSet; Var Accept:Boolean) of object;

procedure DBMonthlySeriesFilter( ASeries:TChartSeries;
                                 ATable:TDataSet;
                                 Const DateFieldName,
                                       ValueFieldName:String;
                                       DatePeriod:TDateTimeStep;
                                       Count:Boolean;
                                 FilterRecord:TFilterRecordProc);

implementation

Uses SysUtils,TeeProco,TeCanvas;

{ returns the DateTime of the January 1st of the Year }
function YearBegin(Const ADate:TDateTime):TDateTime;
var Year,Month,Day:Word;
begin
  DecodeDate(ADate,Year,Month,Day);
  result:=EncodeDate(Year,1,1);
end;

{ returns the Day number inside the Year }
function YearDay(Const ADate:TDateTime):Longint;
begin
  result:=1+Trunc(ADate-YearBegin(ADate));
end;

{ returns the DateTime of December 31st of the Year }
function YearEnd(Const ADate:TDateTime):TDateTime;
var Year,Month,Day:Word;
begin
  DecodeDate(ADate,Year,Month,Day);
  result:=EncodeDate(Year,12,31);
end;

{ Returns the number of the week, from 1 to 52.
  It also returns the Year, which can be the same year, the
  year before or the year after, depending on which day of the week
  the year starts.
  The "FirstDay" constant sets weeks to start at Monday.
}
Function DateToWeek(Const ADate:TDateTime; Var Year:Word):Integer;
Const FirstDay=0; { Monday }
Var d,m,y,j,j0,j1,week:Word;
begin
  DecodeDate(ADate,y,m,d);
  If (m < 3) then
    j := 1461*(y-1) DIV 4 + (153*(m+9)+2) DIV 5 + d
  Else
    j := 1461*y DIV 4 + (153*(m-3)+2) DIV 5 + d;

  j0:=1461*(y-1) DIV 4 + 310;
  j0:=j0-(j0-firstday) MOD 7;

  If (j<j0) then
  begin
    j0 := 1461*(y-2) DIV 4 + 310;
    j0 := j0 - (j0-firstday) MOD 7;
    week := 1 + (j-j0) DIV 7;
    year := y-1;
  end
  else
  begin
    j1 := 1461*y DIV 4 + 310;
    j1 := j1 - (j1-firstday) MOD 7;
    If j<j1 then
    begin
      week := 1 + (j-j0) DIV 7;
      year := y;
    end
    Else
    begin
      week := 1;
      year := y+1;
    End;
  End;
  result:=week;
End;

procedure DBMonthlySeriesFilter( ASeries:TChartSeries;
                                 ATable:TDataSet;
                                 Const DateFieldName,
                                       ValueFieldName:String;
                                       DatePeriod:TDateTimeStep;
                                       Count:Boolean;
                                 FilterRecord:TFilterRecordProc);

Var tmpXPos:TDateTime;
    tmpWeek,
    tmp:Integer;
    Hour,Minute,Second,MSecond,Year,Month,Day:Word;
    tmpQuantity:Double;
    tmpLabel:String;
    DateField:TDateTimeField;
    ValueField:TFloatField;
    tmpAccept:Boolean;
begin
  {$IFDEF TEETRIAL}
  TeeTrial(ASeries.ComponentState);
  {$ENDIF}
  With ATable do
  begin
    DateField:=FieldByName(DateFieldName) as TDateTimeField;
    if ValueFieldName='' then ValueField:=nil
                         else ValueField:=FieldByName(ValueFieldName) as TFloatField;
    ASeries.Clear;
    DisableControls;
    First;
    While not eof do
    begin
      tmpAccept:=False;
      if Assigned(FilterRecord) then
         FilterRecord(ATable,tmpAccept)
      else
         tmpAccept:=True;
      if tmpAccept then
      begin
        tmpXPos:=0;
        tmpWeek:=0;
        DecodeDate(DateField.Value,Year,Month,Day);
        Case DatePeriod of
          dtOneHour:  begin
                        DecodeTime(DateField.Value,Hour,Minute,Second,MSecond);
                        tmpXPos:=Round(DateField.Value)+Hour/24.0;
                      end;
          dtOneDay:   tmpXPos:=Round(DateField.Value);
          dtOneMonth: tmpXPos:=EncodeDate(Year,Month,1);
          dtOneWeek : begin
                        tmpWeek:=DateToWeek(DateField.Value,Year);
                        tmpXPos:=tmpWeek+Year*52;
                      end;
        end;
        if Count then tmpQuantity:=1
                 else tmpQuantity:=ValueField.Value;
        tmp:=ASeries.XValues.Locate(tmpXPos);
        if tmp=-1 then
        begin
          Case DatePeriod of
            dtOneHour : tmpLabel:=FormatDateTime('dd hh:mm',tmpXPos);
            dtOneDay  : tmpLabel:=FormatDateTime('dd/MMM',tmpXPos);
            dtOneMonth: tmpLabel:=FormatDateTime('MMM/yy',tmpXPos);
            dtOneWeek : tmpLabel:=IntToStr(tmpWeek)+'/'+IntToStr(Year);
          end;
          ASeries.AddXY(tmpXPos,tmpQuantity,tmpLabel{$IFNDEF D5},clTeeColor{$ENDIF});
        end
        else
        With ASeries do YValues[tmp]:=YValues[tmp]+tmpQuantity;
      end;
      Next;
    end;
    EnableControls;
  end;
end;

{ clears all points in ASeries and adds new points }
procedure DBMonthlySeries( ASeries:TChartSeries;
                           ATable:TDataSet;
                           Const DateFieldName,
                                 ValueFieldName:String;
                           DatePeriod:TDateTimeStep;
                           Count:Boolean);
begin
  DBMonthlySeriesFilter( ASeries,ATable,DateFieldName,ValueFieldName,DatePeriod,Count,nil);
end;

end.
