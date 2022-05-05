{**********************************************}
{   TOHLCSeries (derived from TCustomSeries)   }
{   Copyright (c) 1995-1996 by David Berneda   }
{**********************************************}
unit OHLChart;

interface

Uses Classes,Graphics,Chart,Series,Teengine;

{ This Unit shows an example of a derived Chart Series.
  Used in financial applications, OHLC stands for Open,High,Low & Close.
  These are the prices a particular financial product has in a given time
  period.

  TOHLCSeries extends the basic TCustomSeries adding new lists for
  High, Low & Close prices, and preserving the default Y values for Open
  prices.

  Overrides the basic list functions (Add, Clear & Delete) plus the
  FillSampleValues method, used in design mode to show some fictional
  values to the user.

  Publishes the High,Low & Close values lists and "overrides" the XValues
  property to be DateValues and the YValues to be OpenValues.
}
Type

  TOHLCSeries = class(TCustomSeries)
     private { assumed YValues = OpenValues }
       FHighValues  : TChartValueList;
       FLowValues   : TChartValueList;
       FCloseValues : TChartValueList;
     protected
       Function GetDateValues:TChartValueList;
       Procedure SetDateValues(Value:TChartValueList);

       Function GetOpenValues:TChartValueList;
       Procedure SetOpenValues(Value:TChartValueList);
       Procedure SetHighValues(Value:TChartValueList);
       Procedure SetLowValues(Value:TChartValueList);
       Procedure SetCloseValues(Value:TChartValueList);
     public
       Constructor Create(AOwner: TComponent); override;
       Function AddOHLC( Const ADate:TDateTime;
                         Const AOpen,AHigh,ALow,AClose:Double):Longint;
       Procedure GetRandomOHLC(Var AOpen,AClose,AHigh,ALow:Double; YRange:Double);
       Procedure FillSampleValues(NumValues:Longint); override;
       Function IsValidSourceOf(Value:TChartSeries):Boolean; override;
       Function MaxYValue:Double; override;
       Function MinYValue:Double; override;
       Function NumSampleValues:Longint; override;
     published
       property CloseValues:TChartValueList read FCloseValues write SetCloseValues;
       property DateValues:TChartValueList read GetDateValues write SetDateValues;
       property HighValues:TChartValueList read FHighValues write SetHighValues;
       property LowValues:TChartValueList read FLowValues write SetLowValues;
       property OpenValues:TChartValueList read GetOpenValues write SetOpenValues;
     end;

implementation

Uses SysUtils, TeCanvas ,TeeProco;

Constructor TOHLCSeries.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  XValues.DateTime:=True;
  XValues.Name:=TeeMsg_ValuesDate;
  YValues.Name:=TeeMsg_ValuesOpen;
  FHighValues :=TChartValueList.Create(Self,TeeMsg_ValuesHigh);
  FLowValues  :=TChartValueList.Create(Self,TeeMsg_ValuesLow);
  FCloseValues:=TChartValueList.Create(Self,TeeMsg_ValuesClose);
  {$IFDEF TEETRIAL}
  TeeTrial(ComponentState);
  {$ENDIF}
end;

Function TOHLCSeries.GetDateValues:TChartValueList;
Begin
  result:=XValues; { overrides the default XValues }
end;

Procedure TOHLCSeries.SetDateValues(Value:TChartValueList);
begin
  SetXValues(Value); { overrides the default XValues }
end;

Function TOHLCSeries.GetOpenValues:TChartValueList;
Begin
  result:=YValues; { overrides the default YValues }
end;

Procedure TOHLCSeries.SetOpenValues(Value:TChartValueList);
begin
  SetYValues(Value); { overrides the default YValues }
end;

Procedure TOHLCSeries.SetHighValues(Value:TChartValueList);
Begin
  SetChartValueList(FHighValues,Value); { standard method }
end;

Procedure TOHLCSeries.SetLowValues(Value:TChartValueList);
Begin
  SetChartValueList(FLowValues,Value); { standard method }
end;

Procedure TOHLCSeries.SetCloseValues(Value:TChartValueList);
Begin
  SetChartValueList(FCloseValues,Value); { standard method }
end;

Function TOHLCSeries.AddOHLC( Const ADate:TDateTime;
                             Const AOpen,AHigh,ALow,AClose:Double):Longint;
Begin
  FHighValues.TempValue:=AHigh;
  FLowValues.TempValue:=ALow;
  FCloseValues.TempValue:=AClose;
  result:=AddXY(ADate,AOpen{$IFNDEF D5},'', clTeeColor{$ENDIF}); { standard add X,Y }
  AddValue(result);
end;

Function TOHLCSeries.MaxYValue:Double;
Begin
  result:=inherited MaxYValue;
  result:=MaxDouble(result,FHighValues.MaxValue);
  result:=MaxDouble(result,FLowValues.MaxValue);
  result:=MaxDouble(result,FCloseValues.MaxValue);
End;

Function TOHLCSeries.MinYValue:Double;
Begin
  result:=inherited MinYValue;
  result:=MinDouble(result,FHighValues.MinValue);
  result:=MinDouble(result,FLowValues.MinValue);
  result:=MinDouble(result,FCloseValues.MinValue);
End;

Procedure TOHLCSeries.GetRandomOHLC(Var AOpen,AClose,AHigh,ALow:Double; YRange:Double);
var tmpY     : Longint;
    tmpFixed : Double;
Begin
  tmpY:=Abs(Round(YRange/400.0));
  AClose:=AOpen+Random(Round(YRange/25.0))-(YRange/50.0); { imagine a close price... }
  { and imagine the high and low session price }
  tmpFixed:=3*Round(Abs(AClose-AOpen)/10.0);
  if AClose>AOpen then
  Begin
    AHigh:=AClose+tmpFixed+Random(tmpY);
    ALow:=AOpen-tmpFixed-Random(tmpY);
  end
  else
  begin
    AHigh:=AOpen+tmpFixed+Random(tmpY);
    ALow:=AClose-tmpFixed-Random(tmpY);
  end;
end;

Procedure TOHLCSeries.FillSampleValues(NumValues:Longint);
Var t      : Longint;
    tmpX,
    StepX,
    tmpY,
    MinY,
    DifY   : Double;
    AOpen,
    AHigh,
    ALow,
    AClose : Double;
Begin
  Clear;
  CalcRandomBounds(NumValues,tmpX,StepX,tmpY,MinY,DifY);
  AOpen:=MinY+Random(Round(DifY)); { starting open price }
  for t:=1 to NumValues do
  Begin
    { Generate random figures }
    GetRandomOHLC(AOpen,AClose,AHigh,ALow,DifY);
    { call the standard add method }
    AddOHLC(tmpX,AOpen,AHigh,ALow,AClose);
    tmpX:=tmpX+StepX;  { <-- next point X value }
    { tomorrow, the market will open at today's close plus/minus something }
    AOpen:=AClose+Random(10)-5;
  end;
  RefreshSeries;
end;

Function TOHLCSeries.NumSampleValues:Longint;
begin
  result:=40;
end;

Function TOHLCSeries.IsValidSourceOf(Value:TChartSeries):Boolean;
begin
  result:=Value is TOHLCSeries;
end;

end.
