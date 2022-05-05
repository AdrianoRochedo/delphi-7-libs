unit TeeChart_Intervals;

{**************************************************}
{   TInterval_Series (derived from TCustomSeries)  }
{   Copyright (c) Adriano Rochedo                  }
{**************************************************}

interface
uses Classes, Types, Contnrs, Graphics, Math, Chart, TeEngine, Series;

Type
  TInterval_Series = class(TCustomSeries)
  private
    FLow_Values  : TChartValueList;  // x1
    FHigh_Values : TChartValueList;  // x2
  protected
    Procedure SetHighValues(Value: TChartValueList);
    Procedure SetLowValues(Value: TChartValueList);
    procedure DrawValue(ValueIndex: Longint); override;
  public
    Constructor Create(AOwner: TComponent); override;

    Function IsValidSourceOf(Value: TChartSeries):Boolean; override;
    Function MaxXValue: Double; override;
    Function MinXValue: Double; override;
    Function MaxYValue: Double; override;
    Function MinYValue: Double; override;

    Function Add(const Title: String;
                 const Low, High: Double;
                 const Color: TColor = clTeeColor): Longint; reintroduce;

    property Low_Values  : TChartValueList read FLow_Values  write SetLowValues;
    property High_Values : TChartValueList read FHigh_Values write SetHighValues;
  end;

implementation
uses TeCanvas;

{ TInterval_Series }

Constructor TInterval_Series.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);

  self.ColorEachPoint := True;

  XValues.Name := 'Low';
  XValues.Order := loNone;

  FLow_Values  := XValues;
  FHigh_Values := TChartValueList.Create(Self, 'High');
end;

Procedure TInterval_Series.SetHighValues(Value: TChartValueList);
Begin
  SetChartValueList(FHigh_Values, Value); { standard method }
end;

Procedure TInterval_Series.SetLowValues(Value: TChartValueList);
Begin
  SetChartValueList(FLow_Values, Value); { standard method }
end;

Function TInterval_Series.Add(const Title: String;
                              const Low, High: Double;
                              const Color: TColor): Longint;
Begin
  Result := AddX(Low, Title, Color);
  FHigh_Values.TempValue := High;
  AddValue(Result);
end;

Function TInterval_Series.MaxXValue: Double;
Begin
  result := inherited MaxXValue;
  result := MaxDouble(result, FHigh_Values.MaxValue);
  result := MaxDouble(result, FLow_Values.MaxValue);
End;

Function TInterval_Series.MinXValue: Double;
Begin
  result := inherited MinXValue;
  result := MinDouble(result, FHigh_Values.MinValue);
  result := MinDouble(result, FLow_Values.MinValue);
End;

Function TInterval_Series.IsValidSourceOf(Value:TChartSeries):Boolean;
begin
  Result := Value is TInterval_Series;
end;

procedure TInterval_Series.DrawValue(ValueIndex: Integer);
var xHigh : Longint;
    xLow  : Longint;
    y     : Integer;
Begin
  Pointer.PrepareCanvas(clTeeColor); { Pointer Pen and Brush styles }
  With ParentChart, Canvas do
    begin
    y     := CalcYPosValue(ValueIndex);
    xLow  := CalcXPosValue(Low_Values.Value[ValueIndex]);
    xHigh := CalcXPosValue(High_Values.Value[ValueIndex]);

    Pen.Color := ValueColor[ValueIndex];
    Pen.Width := 2;

    // Desenha uma reta entre Low e High
    HorizLine3D(xLow, xHigh, y, MiddleZ);

    Pen.Width := 1;

    // Desenha os tracinhos nas pontas da reta
    VertLine3D(xLow, y-3, y+4, MiddleZ);
    VertLine3D(xHigh, y-3, y+4, MiddleZ);
    end; // with
end;

function TInterval_Series.MaxYValue: Double;
begin
  result := inherited MaxYValue;
  result := MaxDouble(result, Count-1);
end;

function TInterval_Series.MinYValue: Double;
begin
  result := inherited MinYValue;
  result := MinDouble(result, 0);
end;

end.
