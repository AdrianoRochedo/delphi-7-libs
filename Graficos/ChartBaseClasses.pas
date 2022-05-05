unit ChartBaseClasses;

interface
uses Forms, Graphics, Classes, SysUtils, Windows, SysUtilsEx,
     CandleCh, BubbleCh, OHLChart, TeeProcs, TeEngine, Chart, Series;

type
  TfoChartList = Class;

  TSeriesList = class
  private
    FChart: TChart;
    function getSerie(i: Integer): TChartSeries;
    function getCount: Integer;
  public
    // Barras --------------------------------------------------------------------------------------

    function AddBarSerie
      (const aTitle: String; aColor: TColor; aType, aSubType: Byte): TBarSeries; overload;

    function AddBarSerie
      (const aTitle: String; aColor: TColor; aType, aSubType: Byte; Values: IArray): TBarSeries; overload;

    function AddBarSerie
      (const aTitle: String; aColor: TColor; aType, aSubType: Byte; Values: IArray; StartIndex, EndIndex: Integer): TBarSeries; overload;

    // Linhas --------------------------------------------------------------------------------------

    function AddLineSerie(const aTitle: String; aColor: TColor): TLineSeries; overload;

    function AddLineSerie
      (const aTitle: String; aColor: TColor; Values: IArray): TLineSeries; overload;

    function AddLineSerie
      (const aTitle: String; aColor: TColor; Values: IArray; StartIndex, EndIndex: Integer): TLineSeries; overload;

    // Pontos --------------------------------------------------------------------------------------

    function AddPointSerie(const aTitle: String; aColor: TColor): TPointSeries; overload;

    function AddPointSerie
      (const aTitle: String; aColor: TColor; Values: IArray): TPointSeries; overload;

    function AddPointSerie
      (const aTitle: String; aColor: TColor; x, y: IArray): TPointSeries; overload;

    function AddPointSerie
      (const aTitle: String; aColor: TColor; Values: IArray; StartIndex, EndIndex: Integer): TPointSeries; overload;

    // Pizza ---------------------------------------------------------------------------------------

    Function AddPieSerie(const aTitle: String): TPieSeries;

    // Vela ----------------------------------------------------------------------------------------

    Function AddCandleSerie(const aTitle: String; aColor: TColor; aType: Byte): TCandleSeries;

    // Bolhas --------------------------------------------------------------------------------------

    Function AddBubbleSerie(const aTitle: String; aColor: TColor; aType: Byte): TBubbleSeries;

    // Outras --------------------------------------------------------------------------------------

    function Add45DegreeLine(const aTitle: String; aColor: TColor): TFastLineSeries;
    function AddTendencyLine(const aTitle: String; aColor: TColor; Values: TChartSeries): TFastLineSeries;

    // Propriedades --------------------------------------------------------------------------------

    // Retorna uma série
    property Item[i: Integer] : TChartSeries read getSerie; default;

    // Retorna o número de séries existentes
    property Count : Integer read getCount;

    // Formulario a que esta instancia pertence
    property Chart : TChart read FChart write FChart;
  end;

  TfoChartList = class
  private
    FList: TList;
    FFS: TFormStyle;
    function getChartForm(i: Integer): TForm;
    procedure setFormStyle(const Value: TFormStyle);
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Show();
    procedure Clear();
    procedure Add(ChartForm: TForm);
    procedure Remove(ChartForm: TForm);

    property Item[i: Integer]: TForm read getChartForm; default;
    property FormStyle: TFormStyle read FFS write setFormStyle;
  end;

implementation
uses teeStore,
     CurvFitt,
     wsGLib,
     Math,
     Form_BaseChart,
     DialogoBase_OpcoesSeries,
     Dialogo_OpcoesBarSeries,
     Dialogo_OpcoesGeraisGrafico,
     Dialogo_OpcoesSerieLinhas,
     Dialogo_OpcoesSeriePontos,
     Dialogo_OpcoesSeriePizza,
     Dialogo_OpcoesSerieVelas;

{ TfoChartList }

procedure TfoChartList.Add(ChartForm: TForm);
begin
  TfoBaseChart(ChartForm).List := Self;
  ChartForm.FormStyle := FFS;
  FList.Add(ChartForm);
end;

constructor TfoChartList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FFS := fsNormal;
end;

destructor TfoChartList.Destroy();
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
    TfoBaseChart(FList[i]).ReleaseFormOnClose := True;

  Clear;
  FList.Free;

  inherited Destroy;
end;

function TfoChartList.getChartForm(i: Integer): TForm;
begin
  try
    Result := TForm(FList[i]);
  except
    Raise Exception.CreateFmt('Índice do Gráfico não Existe [%d]', [i]);
  end;
end;

procedure TfoChartList.Clear;
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
    if TfoBaseChart(FList[i]).ReleaseFormOnClose then
       begin
       TfoBaseChart(FList[i]).Free;
       FList[i] := nil;
       end;
  FList.Pack;
end;

procedure TfoChartList.Show;
var i, x, y, dy, ch, cw: Integer;
begin
  y  := 5;
  x  := y;
  dy := GetSystemMetrics(SM_CYCAPTION);
  ch := Application.MainForm.ClientHeight;
  cw := Application.MainForm.ClientWidth;

  for i := 0 to FList.Count-1 do
    begin
    TfoBaseChart(FList[i]).Top  := x;
    TfoBaseChart(FList[i]).Left := x;
    TfoBaseChart(FList[i]).Show(FFS);
    if x + TfoBaseChart(FList[i]).Height < ch then
       inc(x, dy)
    else
       begin
       if y + TfoBaseChart(FList[i]).Width < cw then inc(y, dy) else y := 0;
       x := y;
       end;
    end;
end;

procedure TfoChartList.Remove(ChartForm: TForm);
begin
  FList.Remove(ChartForm);
end;

procedure TfoChartList.setFormStyle(const Value: TFormStyle);
var i: Integer;
begin
  for i := 0 to FList.Count-1 do
    TfoBaseChart(FList[i]).FormStyle := Value;

  FFS := Value;
end;

{ TSeriesList }

function TSeriesList.AddBarSerie(const aTitle: String;
         aColor: TColor; aType, aSubType: Byte): TBarSeries;
begin
  Result               := TBarSeries.Create(Chart);
  Result.Active        := True;
  Result.SeriesColor   := aColor;
  Result.ParentChart   := Chart;
  Result.Marks.Visible := False;
  Result.Title         := aTitle;
  Result.BarStyle      := TBarStyle(aType);
  Result.MultiBar      := TMultiBar(aSubType);
end;

function TSeriesList.AddBarSerie(const aTitle: String; aColor: TColor;
  aType, aSubType: Byte; Values: IArray): TBarSeries;
begin
  Result := AddBarSerie(
    aTitle, aColor, aType, aSubType, Values, Values.Low(), Values.High());
end;

function TSeriesList.AddBarSerie(const aTitle: String; aColor: TColor;
                        aType, aSubType: Byte; Values: IArray; StartIndex, EndIndex: Integer): TBarSeries;
var i: Integer;
    x: Double;
begin
  Result := AddBarSerie(aTitle, aColor, aType, aSubType);
  if EndIndex <= Values.High() then
     for i := StartIndex to EndIndex do
       if Values.IsMissValue(i, x) then
          Result.AddNull
       else
          Result.Add(x, '', clTeeColor);
end;

function TSeriesList.AddLineSerie(const aTitle: String; aColor: TColor; Values: IArray): TLineSeries;
begin
  Result := AddLineSerie(aTitle, aColor, Values, Values.Low(), Values.High());
end;

function TSeriesList.AddLineSerie(const aTitle: String; aColor: TColor;
                        Values: IArray; StartIndex, EndIndex: Integer): TLineSeries;
var i: Integer;
    x: Double;
begin
  Result := AddLineSerie(aTitle, aColor);
  if EndIndex <= Values.High() then
     for i := StartIndex to EndIndex do
       if Values.IsMissValue(i, x) then
          Result.AddNull()
       else
          Result.Add(x, '', clTeeColor);
end;

function TSeriesList.AddLineSerie(const aTitle: String; aColor: TColor): TLineSeries;
begin
  Result               := TLineSeries.Create(Chart);
  Result.Active        := True;
  Result.SeriesColor   := aColor;
  Result.ParentChart   := Chart;
  Result.Marks.Visible := False;
  Result.Title         := aTitle;
end;

function TSeriesList.Add45DegreeLine(const aTitle: String; aColor: TColor): TFastLineSeries;
var x: Real;
begin
  Result               := TFastLineSeries.Create(Chart);
  Result.SeriesColor   := aColor;
  Result.ParentChart   := Chart;
  Result.Marks.Visible := False;
  Result.Title         := aTitle;

  x := Max(Chart.MaxXValue(Chart.BottomAxis),
           Chart.MaxYValue(Chart.LeftAxis));

  Result.AddXY(0, 0, '', clteeColor);
  Result.AddXY(x, x, '', clteeColor);
end;

function TSeriesList.AddPointSerie(const aTitle: String; aColor: TColor): TPointSeries;
begin
  Result                   := TPointSeries.Create(Chart);
  Result.Active            := True;
  Result.SeriesColor       := aColor;
  Result.ParentChart       := Chart;
  Result.Marks.Visible     := False;
  Result.Title             := aTitle;
  Result.Pointer.Style     := psCircle;
  Result.Pointer.HorizSize := 3;
  Result.Pointer.VertSize  := 3;
end;

function TSeriesList.AddPointSerie(const aTitle: String; aColor: TColor; Values: IArray): TPointSeries;
begin
  Result := AddPointSerie(aTitle, aColor, Values, Values.Low(), Values.High());
end;

function TSeriesList.AddPointSerie(const aTitle: String; aColor: TColor; x, y: IArray): TPointSeries;
var i: Integer;
    v1, v2: Double;
    b1, b2: boolean;
begin
  if x.Low() <> y.Low() then
     raise Exception.Create('Os limites inferiores das séries x e y precisam ser iguais');

  if x.High() <> y.High() then
     raise Exception.Create('Os limites superiores das séries x e y precisam ser iguais');

  Result := AddPointSerie(aTitle, aColor);
  for i := x.Low() to x.High do
    begin
    b1 := x.IsMissValue(i, v1);
    b2 := y.IsMissValue(i, v2);
    if b1 or b2 then
       Result.AddNull()
    else
       Result.AddXY(v1, v2, '', clTeeColor);
    end;
End;

function TSeriesList.AddPointSerie(const aTitle: String; aColor: TColor;
                        Values: IArray; StartIndex, EndIndex: Integer): TPointSeries;
var i: Integer;
    x: Double;
begin
  Result := AddPointSerie(aTitle, aColor);
  if EndIndex <= Values.High() then
     for i := StartIndex to EndIndex do
       if Values.IsMissValue(i, x) then
          Result.AddNull()
       else
          Result.Add(x, '', clTeeColor);
End;

Function TSeriesList.AddPieSerie(const aTitle: String): TPieSeries;
Begin
  Result              := TPieSeries.Create(Chart);
  Result.Active       := True;
  Result.ParentChart  := Chart;
  Result.Title        := aTitle;
End;

Function TSeriesList.AddCandleSerie(const aTitle: String; aColor: TColor; aType: Byte): TCandleSeries;
Begin
  Result              := TCandleSeries.Create(Chart);
  Result.Active       := True;
  Result.SeriesColor  := aColor;
  Result.ParentChart  := Chart;
  Result.CandleStyle  := TCandleStyle(aType); //TCandleStyle=(csCandleStick,csCandleBar,csCandleMed);
  Result.Title        := aTitle;
End;

Function TSeriesList.AddBubbleSerie(const aTitle: String; aColor: TColor; aType: Byte): TBubbleSeries;
Begin
{TSeriesPointerStyle=( psRectangle,psCircle,psTriangle,psDownTriangle,
                        psCross,psDiagCross,psStar,psDiamond,psSmallDot );}
end;

function TSeriesList.AddTendencyLine(const aTitle: String; aColor: TColor; Values: TChartSeries): TFastLineSeries;
var T: TTrendFunction;
begin
  Result              := TFastLineSeries.Create(Chart);
  Result.SeriesColor  := aColor;
  Result.ParentChart  := Chart;
  Result.Title        := aTitle;

  T := TTrendFunction.Create(Chart);
  T.ParentSeries := Result;
  T.Period := Values.Count;
  T.AddPoints(Values);
end;

function TSeriesList.getSerie(i: Integer): TChartSeries;
begin
  Result := Chart.Series[i];
end;

function TSeriesList.getCount: Integer;
begin
  Result := Chart.SeriesCount;
end;

end.
