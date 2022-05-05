unit TeeChart_BoxPlot;

{**********************************************}
{   TBoxPlotSeries (derived from TCustomSeries)}
{   Copyright (c) Adriano Rochedo              }
{**********************************************}

interface
uses Classes, Types, Contnrs, Graphics, Math, Chart, TeEngine, Series, CandleCh, BubbleCh;

Const
  DefBoxWidth = 8;  { 3 + 2 + 3 }

Type
  TGenericBoxPlot_Series = class(TCustomSeries)
  private
    FHigh_Values : TChartValueList;
    FLow_Values  : TChartValueList;
    FQ1_Values   : TChartValueList;
    FQ3_Values   : TChartValueList;
  protected
    Function  GetQ1Values: TChartValueList;
    Procedure SetQ1Values(Value: TChartValueList);
    Procedure SetQ3Values(Value: TChartValueList);
    Procedure SetHighValues(Value: TChartValueList);
    Procedure SetLowValues(Value: TChartValueList);
  public
    Constructor Create(AOwner: TComponent); override;

    Function IsValidSourceOf(Value: TChartSeries):Boolean; override;
    Function MaxYValue: Double; override;
    Function MinYValue: Double; override;

    Function AddBox(const Title: String;
                    const X: Double;
                    const Q1, Q3, High, Low: Double;
                    const Color: TColor = clTeeColor): Longint;

    property Q1_Values   : TChartValueList read FQ1_Values   write SetQ1Values;
    property Q3_Values   : TChartValueList read FQ3_Values   write SetQ3Values;
    property High_Values : TChartValueList read FHigh_Values write SetHighValues;
    property Low_Values  : TChartValueList read FLow_Values  write SetLowValues;
  end;

  TExtremesPoints = class
  private
    FPoints: TDoubleDynArray;
  public
    constructor Create(const Points: TDoubleDynArray);
    Function MaxValue: Double;
    Function MinValue: Double;
    property Points : TDoubleDynArray read FPoints;
  end;

  TBoxPlotExtremeList = class(TObjectList)
  private
    FSerie : TChartSeries;
    FName  : String;
    function getExtremes(i: Integer): TExtremesPoints;
  public
    constructor Create(AOwner: TChartSeries; const aName: String);
    procedure Add(const Points: TDoubleDynArray);

    Function MaxValue: Double;
    Function MinValue: Double;

    property Extremes[i: Integer] : TExtremesPoints read getExtremes;
    property Serie : TChartSeries read FSerie;
    property Name  : String read FName;
  end;

  TBoxPlotWithMinMax_Series = class(TGenericBoxPlot_Series)
  private
    FMin_Values: TChartValueList;
    FMax_Values: TChartValueList;
    FMed_Values: TChartValueList;
    FBoxWidth  : Integer;
    FExt_Values: TBoxPlotExtremeList;

    procedure SetMaxValues(const Value: TChartValueList);
    procedure SetMinValues(const Value: TChartValueList);
    procedure SetMedValues(const Value: TChartValueList);
  protected
    procedure DrawValue(ValueIndex:Longint); override;
    Procedure SetBoxWidth(Value:Integer);
    Function  GetDraw3D: Boolean;
    procedure SetDraw3D(Value: Boolean);
    Function  GetDark3D: Boolean;
    procedure SetDark3D(Value: Boolean);
    Function  GetPen: TPen;
    procedure SetPen(Value: TPen);
  public
    Constructor Create(AOwner: TComponent);
    Destructor Destroy; override;

    Procedure Assign(Source:TPersistent); override;
    Function IsValidSourceOf(Value: TChartSeries):Boolean; override;
    Function MaxYValue: Double; override;
    Function MinYValue: Double; override;
    Procedure ClearLists; override;

    Function AddBox(const Title: String;
                    const X: Double;
                    const Q1, Q3, High, Low, Min, Max, Median: Double;
                    const Extremes: TDoubleDynArray;
                    const Color: TColor = clTeeColor): Longint;

    property Min_Values    : TChartValueList read FMin_Values write SetMinValues;
    property Max_Values    : TChartValueList read FMax_Values write SetMaxValues;
    property Median_Values : TChartValueList read FMed_Values write SetMedValues;

    // Pontos Extremos: Poderá existir um conjunto de pontos para cada caixa
    property Extremes_Values : TBoxPlotExtremeList read FExt_Values;

    property BoxWidth : Integer   read FBoxWidth write SetBoxWidth default DefBoxWidth;
    property Draw3D   : Boolean   read GetDraw3D write SetDraw3D   default False;
    property Dark3D   : Boolean   read GetDark3D write SetDark3D   default True;
    property Pen      : TPen      read GetPen    write SetPen;
  end;

implementation
Uses SysUtils, TeCanvas;

{ TBoxPlotExtremeList }

procedure TBoxPlotExtremeList.Add(const Points: TDoubleDynArray);
begin
  inherited Add(TExtremesPoints.Create(Points));
end;

constructor TBoxPlotExtremeList.Create(aOwner: TChartSeries; const aName: String);
begin
  inherited Create(True);
  FSerie := AOwner;
  FName := aName;
end;

function TBoxPlotExtremeList.getExtremes(i: Integer): TExtremesPoints;
begin
  Result := TExtremesPoints(Items[i]);
end;

function TBoxPlotExtremeList.MaxValue: Double;
var i: Integer;
    x: Double;
begin
  if Count = 0 then
     begin
     Result := Math.MinDouble;
     Exit;
     end;

  Result := getExtremes(0).MaxValue;
  for i := 1 to Count-1 do
    begin
    x := getExtremes(i).MaxValue;
    if x > Result then Result := x;
    end;
end;

function TBoxPlotExtremeList.MinValue: Double;
var i: Integer;
    x: Double;
begin
  if Count = 0 then
     begin
     Result := Math.MaxDouble;
     Exit;
     end;

  Result := getExtremes(0).MinValue;
  for i := 1 to Count-1 do
    begin
    x := getExtremes(i).MinValue;
    if x < Result then Result := x;
    end;
end;    

{ TGenericBoxPlot_Series }

Constructor TGenericBoxPlot_Series.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);

  XValues.DateTime := false;
  XValues.Name := '';

  YValues.Name := 'Q1';
  FQ1_Values   := YValues;
  FQ3_Values   := TChartValueList.Create(Self, 'Q3');
  FHigh_Values := TChartValueList.Create(Self, 'High');
  FLow_Values  := TChartValueList.Create(Self, 'Low');
end;

Function TGenericBoxPlot_Series.GetQ1Values: TChartValueList;
Begin
  Result := YValues; { overrides the default YValues }
end;

Procedure TGenericBoxPlot_Series.SetQ1Values(Value: TChartValueList);
begin
  SetYValues(Value); { overrides the default YValues }
end;

Procedure TGenericBoxPlot_Series.SetHighValues(Value: TChartValueList);
Begin
  SetChartValueList(FHigh_Values, Value); { standard method }
end;

Procedure TGenericBoxPlot_Series.SetLowValues(Value: TChartValueList);
Begin
  SetChartValueList(FLow_Values, Value); { standard method }
end;

procedure TGenericBoxPlot_Series.SetQ3Values(Value: TChartValueList);
begin
  SetChartValueList(FQ3_Values, Value); { standard method }
end;

Function TGenericBoxPlot_Series.AddBox(const Title: String;
                                       const X: Double;
                                       const Q1, Q3, High, Low: Double;
                                       const Color: TColor): Longint;
Begin
  Result := AddXY(X, Q1, Title, Color); 
  FQ3_Values.TempValue := Q3;
  FHigh_Values.TempValue := High;
  FLow_Values.TempValue := Low;
  AddValue(Result);
end;

Function TGenericBoxPlot_Series.MaxYValue: Double;
Begin
  result := inherited MaxYValue;
  result := MaxDouble(result, FQ3_Values.MaxValue);
  result := MaxDouble(result, FHigh_Values.MaxValue);
  result := MaxDouble(result, FLow_Values.MaxValue);
End;

Function TGenericBoxPlot_Series.MinYValue: Double;
Begin
  result := inherited MinYValue;
  result := MinDouble(result, FQ3_Values.MinValue);
  result := MinDouble(result, FHigh_Values.MinValue);
  result := MinDouble(result, FLow_Values.MinValue);
End;

Function TGenericBoxPlot_Series.IsValidSourceOf(Value:TChartSeries):Boolean;
begin
  Result := Value is TGenericBoxPlot_Series;
end;

{ TBoxPlotWithMinMax_Series }

constructor TBoxPlotWithMinMax_Series.Create(AOwner: TComponent);
begin
  inherited;
  FBoxWidth   := DefBoxWidth;
  FMin_Values := TChartValueList.Create(Self, 'Min');
  FMax_Values := TChartValueList.Create(Self, 'Max');
  FMed_Values := TChartValueList.Create(Self, 'Med');
  FExt_Values := TBoxPlotExtremeList.Create(Self, 'Extremos');
end;

function TBoxPlotWithMinMax_Series.AddBox(const Title: String;
                                          const X: Double;
                                          const Q1, Q3, High, Low, Min, Max, Median: Double;
                                          const Extremes: TDoubleDynArray;
                                          const Color: TColor): Longint;
var H, L: Double;
begin
  // Não podemos chamar "inherited AddBox" por causa de "AddValue" que insere os
  // valores temporarios nas listas de pontos.

  Result := AddXY(X, Q1, Title, Color);
  FQ3_Values.TempValue := Q3;

  H := High;
  if H > Max then H := Max;
  FHigh_Values.TempValue := H;

  L := Low;
  if L < Min then L := Min;
  FLow_Values.TempValue := L;

  FMin_Values.TempValue := Min;
  FMax_Values.TempValue := Max;
  FMed_Values.TempValue := Median;

  FExt_Values.Add(Extremes);

  AddValue(Result);
end;

function TBoxPlotWithMinMax_Series.IsValidSourceOf(Value: TChartSeries): Boolean;
begin
   Result := Value is TBoxPlotWithMinMax_Series;
end;

function TBoxPlotWithMinMax_Series.MaxYValue: Double;
begin
  result := inherited MaxYValue;
  result := MaxDouble(result, FMin_Values.MaxValue);
  result := MaxDouble(result, FMax_Values.MaxValue);
  result := MaxDouble(result, FMed_Values.MaxValue);
  result := MaxDouble(result, FExt_Values.MaxValue);
end;

function TBoxPlotWithMinMax_Series.MinYValue: Double;
begin
  result := inherited MinYValue;
  result := MinDouble(result, FMin_Values.MinValue);
  result := MinDouble(result, FMax_Values.MinValue);
  result := MinDouble(result, FMed_Values.MinValue);
  result := MinDouble(result, FExt_Values.MinValue);
end;

procedure TBoxPlotWithMinMax_Series.SetMaxValues(const Value: TChartValueList);
begin
  SetChartValueList(FMax_Values, Value);
end;

procedure TBoxPlotWithMinMax_Series.SetMinValues(const Value: TChartValueList);
begin
  SetChartValueList(FMin_Values, Value);
end;

procedure TBoxPlotWithMinMax_Series.Assign(Source: TPersistent);
begin
  if Source is TBoxPlotWithMinMax_Series then
     With TCandleSeries(Source) do
       begin
       Self.FBoxWidth := FBoxWidth;
       end;
  inherited Assign(Source);
end;

procedure TBoxPlotWithMinMax_Series.DrawValue(ValueIndex: Integer);
var yQ1           : Longint;
    yQ3           : Longint;
    yHigh         : Longint;
    yLow          : Longint;
    yMin          : Longint;
    yMax          : Longint;
    yMed          : Longint;
    x             : Integer;
    y             : Integer;
    i             : Integer;
    tmpLeftWidth  : Integer;
    tmpRightWidth : Integer;
    p             : TDoubleDynArray;

    function CalcColor: TColor;
    begin
      Result := clRed;
    end;

Begin
  Pointer.PrepareCanvas(clTeeColor); { Pointer Pen and Brush styles }
  With ParentChart, Canvas do
    begin
    x     := CalcXPosValue(XValue[ValueIndex]);
    yQ1   := CalcYPosValue(Q1_Values.Value[ValueIndex]);
    yQ3   := CalcYPosValue(Q3_Values.Value[ValueIndex]);
    yHigh := CalcYPosValue(High_Values.Value[ValueIndex]);
    yLow  := CalcYPosValue(Low_Values.Value[ValueIndex]);
    yMin  := CalcYPosValue(Min_Values.Value[ValueIndex]);
    yMax  := CalcYPosValue(Max_Values.Value[ValueIndex]);
    yMed  := CalcYPosValue(Median_Values.Value[ValueIndex]);

    tmpLeftWidth := FBoxWidth div 2;
    tmpRightWidth := FBoxWidth - tmpLeftWidth;

    // Desenha uma reta entre High e Low
    Pen.Width := 2;
    VertLine3D(x, yHigh, yLow, MiddleZ);

    Pen.Width := 1;
    Brush.Color := ValueColor[ValueIndex];
    if View3D and Pointer.Draw3D then
       begin
       // desenha um cubo entre Q3 e Q1
       Cube(x-tmpLeftWidth, x+tmpRightWidth, yQ3, YQ1, StartZ, EndZ, Pointer.Dark3D);

       Pen.Width := 2;

       // Desenha um tracinho em High
       Plane3D(Point(x-tmpLeftWidth+2, yHigh), Point(x+tmpRightWidth-2, yHigh), StartZ, EndZ);

       // Desenha um tracinho em Low
       Plane3D(Point(x-tmpLeftWidth+2, yLow), Point(x+tmpRightWidth-2, yLow), StartZ, EndZ);

       // Desenha a mediana
       Pen.Width := 2;
       Plane3D(Point(x-tmpLeftWidth, yMed), Point(x+tmpRightWidth, yMed), StartZ, EndZ);

       Pen.Width := 1;

       // Desenha os Extremos
       Brush.Color := clRED;
       p := FExt_Values.Extremes[ValueIndex].Points;
       for i := 0 to High(p) do
         begin
         y := CalcYPosValue(p[i]);
         EllipseWithZ(X-3, y-3, X+3, y+3, MiddleZ);
         end;

       // Desenha os pontos Max e Min
       Brush.Color := clYellow;
       EllipseWithZ(x-4, yMax-4, x+4, yMax+4, MiddleZ);
       EllipseWithZ(x-4, yMin-4, x+4, yMin+4, MiddleZ);
       end
    else
       begin
       // desenha um retângulo entre Q3 e Q1
       RectangleWithZ(Rect(x-tmpLeftWidth, yQ3, x+tmpRightWidth, yQ1), MiddleZ);

       Pen.Width := 2;

       // Desenha um tracinho em High
       HorizLine3D(x-tmpLeftWidth-MiddleZ+2, x+tmpRightWidth-MiddleZ-4, yHigh, MiddleZ);

       // Desenha um tracinho em Low
       HorizLine3D(x-tmpLeftWidth-MiddleZ+2, x+tmpRightWidth-MiddleZ-4, yLow, MiddleZ);

       // Desenha a mediana
       Pen.Width := 2;
       HorizLine3D(x-tmpLeftWidth-MiddleZ, x+tmpRightWidth-MiddleZ, yMed, MiddleZ);

       Pen.Width := 1;

       // Desenha os Extremos
       Brush.Color := clRED;
       p := FExt_Values.Extremes[ValueIndex].Points;
       for i := 0 to High(p) do
         begin
         y := CalcYPosValue(p[i]);
         Ellipse(X-3+MiddleZ, y-3, X+3+MiddleZ, y+3);
         end;

       // Desenha os pontos Max e Min
       Brush.Color := clYellow;
       Ellipse(X-4+MiddleZ, yMax-4, X+4+MiddleZ, yMax+4);
       Ellipse(X-4+MiddleZ, yMin-4, X+4+MiddleZ, yMin+4);
       end;
    end; // with
end;

function TBoxPlotWithMinMax_Series.GetDark3D: Boolean;
begin
  result := Pointer.Dark3D;
end;

function TBoxPlotWithMinMax_Series.GetDraw3D: Boolean;
begin
  result := Pointer.Draw3D;
end;

function TBoxPlotWithMinMax_Series.GetPen: TPen;
begin
  result := Pointer.Pen;
end;

procedure TBoxPlotWithMinMax_Series.SetBoxWidth(Value: Integer);
begin
  SetIntegerProperty(FBoxWidth, Value);
end;

procedure TBoxPlotWithMinMax_Series.SetDark3D(Value: Boolean);
begin
  Pointer.Dark3D := Value;
end;

procedure TBoxPlotWithMinMax_Series.SetDraw3D(Value: Boolean);
begin
  Pointer.Draw3D := Value;
end;

procedure TBoxPlotWithMinMax_Series.SetPen(Value: TPen);
begin
  Pointer.Pen.Assign(Value);
end;

procedure TBoxPlotWithMinMax_Series.SetMedValues(const Value: TChartValueList);
begin
  SetChartValueList(FMed_Values, Value);
end;

destructor TBoxPlotWithMinMax_Series.Destroy;
begin
  inherited;
  FExt_Values.Free;
end;

procedure TBoxPlotWithMinMax_Series.ClearLists;
begin
  inherited;
  FExt_Values.Clear;
end;

{ TExtremesPoints }

constructor TExtremesPoints.Create(const Points: TDoubleDynArray);
begin
  inherited Create;
  FPoints := Points;
end;

function TExtremesPoints.MaxValue: Double;
begin
  if Length(FPoints) > 0 then
     Result := Math.MaxValue(FPoints)
  else
     Result := Math.MinDouble;
end;

function TExtremesPoints.MinValue: Double;
begin
  if Length(FPoints) > 0 then
     Result := Math.MinValue(FPoints)
  else
     Result := Math.MaxDouble;
end;

end.
