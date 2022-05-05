unit Lib_Chart;

interface
uses psBase;

  procedure API(Lib: TLib);

implementation
uses Classes,
     SysUtils,                         
     Forms,
     ChartBaseClasses,
     Form_Chart,
     Graphics,
     Chart,
     TeEngine,
     Series,
     wsVec;

const
  cCatChart = 'Gráficos (TChart)';

type
  Tps_Chart = class(TpsClass)
    procedure AddMethods; override;
    class procedure amSetView3D(Const Func_Name: String; Stack: TexeStack);
    class procedure amTitle(Const Func_Name: String; Stack: TexeStack);
    class procedure am_BottonAxis(Const Func_Name: String; Stack: TexeStack);
    class procedure am_LeftAxis(Const Func_Name: String; Stack: TexeStack);
    class procedure am_RightAxis(Const Func_Name: String; Stack: TexeStack);
    class procedure am_TopAxis(Const Func_Name: String; Stack: TexeStack);
  end;

  Tps_ChartAxis = class(TpsClass)
    procedure AddMethods; override;
    class procedure am_setTitle(Const Func_Name: String; Stack: TexeStack);
    class procedure am_setLogarithmic(Const Func_Name: String; Stack: TexeStack);
    class procedure am_setMaximum(Const Func_Name: String; Stack: TexeStack);
    class procedure am_setMinimum(Const Func_Name: String; Stack: TexeStack);
  end;

  Tps_ChartForm = class(TpsClass)
  public
    function CreateObject(Stack: TexeStack): TObject; override;
    procedure AddMethods; override;

    class procedure amShow (const Func_Name: String; Stack: TexeStack);
    class procedure amGetSeries (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetChart  (Const Func_Name: String; Stack: TexeStack);
    class procedure am_getCaption(Const Func_Name: String; Stack: TexeStack);
    class procedure am_setBounds(Const Func_Name: String; Stack: TexeStack);
    class procedure am_setCaption(Const Func_Name: String; Stack: TexeStack);
  end;

  Tps_Series = class(TpsClass)
  public
    procedure AddMethods; override;

    // Linhas
    class procedure amAddLineSerie   (Const Func_Name: String; Stack: TexeStack);
    class procedure amAddLineSerieEx (const Func_Name: String; Stack: TexeStack);

    // Pontos
    class procedure amAddPointSerie   (Const Func_Name: String; Stack: TexeStack);
    class procedure amAddPointSerieEx (const Func_Name: String; Stack: TexeStack);

    // Pizza
    class procedure amAddPieSerie (Const Func_Name: String; Stack: TexeStack);

    // Barras
    class procedure amAddBarSerie (Const Func_Name: String; Stack: TexeStack);
  end;

  Tps_Serie = class(TpsClass)
  public
    procedure AddMethods; override;
    class procedure amAdd   (Const Func_Name: String; Stack: TexeStack);
    class procedure amAddEx (Const Func_Name: String; Stack: TexeStack);
    class procedure amAddXY (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetXLabel (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetXLabel (Const Func_Name: String; Stack: TexeStack);
    class procedure am_AddNull(Const Func_Name: String; Stack: TexeStack);
    class procedure am_Count(Const Func_Name: String; Stack: TexeStack);
    class procedure am_Delete(Const Func_Name: String; Stack: TexeStack);
    class procedure am_setValueFormat(Const Func_Name: String; Stack: TexeStack);
    class procedure am_setTitle(Const Func_Name: String; Stack: TexeStack);
    class procedure am_ShowInLegend(Const Func_Name: String; Stack: TexeStack);
  end;

  Tps_BarSerie = class(Tps_Serie)
    procedure AddMethods; override;
    class procedure amSetBarWidthPercent (Const Func_Name: String; Stack: TexeStack);
  end;

  Tps_LineSerie = class(Tps_Serie)
    procedure AddMethods; override;
  end;

  Tps_PointSerie = class(Tps_Serie)
    procedure AddMethods; override;
  end;

  Tps_PieSerie = class(Tps_Serie)
    procedure AddMethods; override;
  end;

{ ------------------- Ponto de Entrada ------------------ }

procedure API(Lib: TLib);
begin
  Tps_Chart.Create(TChart,
                   nil,
                   'Encapsula um objeto do tipo TChart',
                   cCatChart,
                   [], [], [],
                   False,
                   Lib.Classes);

  Tps_ChartAxis.Create(TChartAxis,
                       nil,
                       'Representa um eixo do gráfico',
                       cCatChart,
                       [], [], [],
                       False,
                       Lib.Classes);

  Tps_ChartForm.Create(TfoChart,
                       nil,
                      'Encapsula uma janela contendo um gráfico',
                      cCatChart,
                      [], [], [],
                      True,
                      Lib.Classes);

  Tps_Series.Create(TSeriesList,
                    nil,
                    'Gerencia as séries de um gráfico',
                    cCatChart,
                    [], [], [],
                    False,
                    Lib.Classes);

  Tps_Serie.Create(TChartSeries,
                    nil,
                    'Representa os dados de uma série',
                    cCatChart,
                    [], [], [],
                    False,
                    Lib.Classes);

  Tps_BarSerie.Create(TBarSeries,
                      TChartSeries,
                      'Encapsula uma Serie do tipo "Barras"',
                      cCatChart,
                      [], [], [],
                      False,
                      Lib.Classes);

  Tps_LineSerie.Create(TLineSeries,
                       TChartSeries,
                       'Encapsula uma Serie do tipo "Linhas"',
                       cCatChart,
                       [], [], [],
                       False,
                       Lib.Classes);

  Tps_PointSerie.Create(TPointSeries,
                        TChartSeries,
                        'Encapsula uma Serie do tipo "Pontos"',
                        cCatChart,
                        [], [], [],
                        False,
                        Lib.Classes);

  Tps_PieSerie.Create(TPieSeries,
                      TChartSeries,
                      'Encapsula uma Serie do tipo "Pizza"',
                      cCatChart,
                      [], [], [],
                      False,
                      Lib.Classes);
end;

{ Tps_Chart }

class procedure Tps_Chart.amSetView3D(Const Func_Name: String; Stack: TexeStack);
begin
  TChart(Stack.AsObject(2)).View3D := Stack.AsBoolean(1);
end;

class procedure Tps_Chart.amTitle(Const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(TChart(Stack.AsObject(1)).Title.Text);
end;

class procedure Tps_Chart.am_BottonAxis(Const Func_Name: String; Stack: TexeStack);
var o: TChart;
begin
  o := TChart(Stack.AsObject(1));
  Stack.PushObject(o.BottomAxis)
end;

class procedure Tps_Chart.am_LeftAxis(Const Func_Name: String; Stack: TexeStack);
var o: TChart;
begin
  o := TChart(Stack.AsObject(1));
  Stack.PushObject(o.LeftAxis)
end;

class procedure Tps_Chart.am_RightAxis(Const Func_Name: String; Stack: TexeStack);
var o: TChart;
begin
  o := TChart(Stack.AsObject(1));
  Stack.PushObject(o.RightAxis)
end;

class procedure Tps_Chart.am_TopAxis(Const Func_Name: String; Stack: TexeStack);
var o: TChart;
begin
  o := TChart(Stack.AsObject(1));
  Stack.PushObject(o.TopAxis)
end;

procedure Tps_Chart.AddMethods;
begin
  with Procs do
    begin
    Add('SetView3D',
        'Mostra o gráfico em 2D ou 3D',
        '',
        [pvtBoolean],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetView3D);
    end;

  with Functions do
    begin
    Add('Title',
        'Retorna uma referência a propriedade "Title" no qual é um objeto do tipo TStringList',
        '',
        [],
        [],
        [],
        pvtObject,
        TStringList,
        amTitle);

    Add('BottonAxis',
        'Eixo de baixo',
        '',
        [],
        [],
        [],
        pvtObject,
        TChartAxis,
        am_BottonAxis);

    Add('LeftAxis',
        'Eixo Esquerdo',
        '',
        [],
        [],
        [],
        pvtObject,
        TChartAxis,
        am_LeftAxis);

    Add('RightAxis',
        '',
        '',
        [],
        [],
        [],
        pvtObject,
        TChartAxis,
        am_RightAxis);

    Add('TopAxis',
        'Eixo do Topo',
        '',
        [],
        [],
        [],
        pvtObject,
        TChartAxis,
        am_TopAxis);
    end;
end;

{ Tps_ChartAxis }

class procedure Tps_ChartAxis.am_setTitle(Const Func_Name: String; Stack: TexeStack);
var o: TChartAxis;
begin
  o := TChartAxis(Stack.AsObject(2));
  o.Title.Caption := Stack.AsString(1);
end;

class procedure Tps_ChartAxis.am_setLogarithmic(Const Func_Name: String; Stack: TexeStack);
var o: TChartAxis;
begin
  o := TChartAxis(Stack.AsObject(2));
  o.Logarithmic := Stack.AsBoolean(1);
end;

class procedure Tps_ChartAxis.am_setMaximum(Const Func_Name: String; Stack: TexeStack);
var o: TChartAxis;
begin
  o := TChartAxis(Stack.AsObject(2));
  o.Maximum := Stack.AsFloat(1);
end;

class procedure Tps_ChartAxis.am_setMinimum(Const Func_Name: String; Stack: TexeStack);
var o: TChartAxis;
begin
  o := TChartAxis(Stack.AsObject(2));
  o.Minimum := Stack.AsFloat(1);
end;

procedure Tps_ChartAxis.AddMethods;
begin
  with Procs do
    begin
    Add('setTitle',
        'Título do Eixo',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_setTitle);

    Add('setLogarithmic',
        'Informa se a escala dos valores mostrados no eixo é logarítmica',
        '',
        [pvtBoolean],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_setLogarithmic);

    Add('setMaximum',
        'Informa o valor máximo para o eixo',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_setMaximum);

    Add('setMinimum',
        'Informa o valor mínimo do eixo',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_setMinimum);
    end;

  with Functions do
    begin
    end;
end;

{ Tps_ChartForm }

procedure Tps_ChartForm.AddMethods;
begin
  with Procs do
    begin
    Add('Show',
        'Mostra o Gráfico na tela',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amShow);

    Add('setBounds',
        'Estabelece a posição e o tamanho da janela',
        '',
        [pvtInteger, pvtInteger, pvtInteger, pvtInteger],
        [nil, nil, nil, nil],
        [False, False, False, False],
        pvtNull,
        TObject,
        am_setBounds);

    Add('setCaption',
        'Estabelece o título da janela',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_setCaption);
    end;

  with Functions do
    begin
    Add('Chart',
        'Obtem uma referência ao objeto "Chart" (TeeChart)',
        '',
        [],
        [],
        [],
        pvtObject,
        TChart,
        amGetChart);

    Add('Series',
        'Obtem uma referência as séries do gráfico',
        '',
        [],
        [],
        [],
        pvtObject,
        TSeriesList,
        amGetSeries);

    Add('getCaption',
        'Retorna o título da janela',
        '',
        [],
        [],
        [],
        pvtString,
        TObject,
        am_getCaption);
    end;
end;

class procedure Tps_ChartForm.amGetChart(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    TfoChart(Stack.AsObject(1)).Chart);
end;

class procedure Tps_ChartForm.amGetSeries(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    TfoChart(Stack.AsObject(1)).Series);
end;

class procedure Tps_ChartForm.amShow(const Func_Name: String; Stack: TexeStack);
begin
  TfoChart(Stack.AsObject(1)).Show(fsNormal);
end;

{ Tps_Series }

procedure Tps_Series.AddMethods;
begin
  with Procs do
    begin
    end;

  with Functions do
    begin
    Add('AddBarSerie',
        'Adiciona uma série de Barras em um gráfico'#13 +
        'Parâmetros: Titulo, Cor, Tipo, Multi-Tipo',
        '',
        [pvtString, pvtInteger, pvtInteger, pvtInteger],
        [nil      , nil       , nil       , nil       ],
        [False    , False     , False     , False     ],
        pvtObject,
        TBarSeries,
        amAddBarSerie);

    Add('AddLineSerie',
        'Adiciona uma série tipo "Linhas" em um gráfico'#13 +
        'Parâmetros: Título, Cor',
        '',
        [pvtString, pvtInteger],
        [nil      , nil       ],
        [false    , false     ],
        pvtObject,
        TLineSeries,
        amAddLineSerie);

    Add('AddLineSerieEx',
        'Adiciona uma série tipo "Linhas" em um gráfico'#13 +
        'Parâmetros: Título, Cor, Vetor com os dados',
        '',
        [pvtString, pvtInteger, pvtObject],
        [nil      , nil       , TwsVec   ],
        [false    , false     , true     ],
        pvtObject,
        TLineSeries,
        amAddLineSerieEx);

    Add('AddPointSerie',
        'Adiciona uma série tipo "Pontos" em um gráfico'#13 +
        'Parâmetros: Título, Cor',
        '',
        [pvtString, pvtInteger],
        [nil      , nil       ],
        [false    , false     ],
        pvtObject,
        TPointSeries,
        amAddPointSerie);

    Add('AddPointSerieEx',
        'Adiciona uma série tipo "Pontos" em um gráfico'#13 +
        'Parâmetros: Título, Cor, Vetor com os dados',
        '',
        [pvtString, pvtInteger, pvtObject],
        [nil      , nil       , TwsVec   ],
        [false    , false     , true     ],
        pvtObject,
        TPointSeries,
        amAddPointSerieEx);

    Add('AddPieSerie',
        'Adiciona uma série tipo "Pizza" em um gráfico'#13 +
        'Parâmetros: Título',
        '',
        [pvtString],
        [nil      ],
        [false    ],
        pvtObject,
        TPieSeries,
        amAddPieSerie);
    end;
end;

class procedure Tps_Series.amAddBarSerie(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    TSeriesList(Stack.AsObject(5)).AddBarSerie(
      Stack.AsString(1),          // Titulo
      TColor(Stack.AsInteger(2)), // Cor
      Stack.AsInteger(3),         // Tipo
      Stack.AsInteger(4))         // Multi-Tipo
    );
end;

class procedure Tps_Series.amAddLineSerie(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    TSeriesList(Stack.AsObject(3)).AddLineSerie(
      Stack.AsString(1),
      TColor(Stack.AsInteger(2))
      )
    );
end;

class procedure Tps_Series.amAddLineSerieEx(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    TSeriesList(Stack.AsObject(4)).AddLineSerie(
      Stack.AsString(1),
      TColor(Stack.AsInteger(2)),
      TwsVec(Stack.AsObject(3))
      )
    );
end;

class procedure Tps_ChartForm.am_getCaption(Const Func_Name: String; Stack: TexeStack);
var o: TfoChart;
begin
  o := TfoChart(Stack.getSelf);
  Stack.PushString(o.Caption);
end;

class procedure Tps_ChartForm.am_setBounds(Const Func_Name: String; Stack: TexeStack);
var o: TfoChart;
begin
  o := TfoChart(Stack.getSelf());
  o.setBounds(Stack.AsInteger(1),
              Stack.AsInteger(2),
              Stack.AsInteger(3),
              Stack.AsInteger(4));
end;

class procedure Tps_ChartForm.am_setCaption(Const Func_Name: String; Stack: TexeStack);
var o: TfoChart;
begin
  o := TfoChart(Stack.getSelf());
  o.Caption := Stack.AsString(1);
end;

function Tps_ChartForm.CreateObject(Stack: TexeStack): TObject;
begin
  Result := TfoChart.Create;
end;

class procedure Tps_Series.amAddPieSerie(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    TSeriesList( Stack.getSelf() ).AddPieSerie( Stack.AsString(1) ));
end;

class procedure Tps_Series.amAddPointSerie(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    TSeriesList(Stack.getSelf()).AddPointSerie(
      Stack.AsString(1),
      TColor(Stack.AsInteger(2))
      )
    );
end;

class procedure Tps_Series.amAddPointSerieEx(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    TSeriesList(Stack.getSelf()).AddPointSerie(
      Stack.AsString(1),
      TColor(Stack.AsInteger(2)),
      TwsVec(Stack.AsObject(3))
      )
    );
end;

{ Tps_Serie }

class procedure Tps_Serie.am_AddNull(Const Func_Name: String; Stack: TexeStack);
var o: TChartSeries;
begin
  o := TChartSeries(Stack.AsObject(2));
  Stack.PushInteger(o.AddNull(Stack.AsString(1)));
end;

class procedure Tps_Serie.am_Count(Const Func_Name: String; Stack: TexeStack);
var o: TChartSeries;
begin
  o := TChartSeries(Stack.getSelf);
  Stack.PushInteger(o.Count);
end;

class procedure Tps_Serie.am_Delete(Const Func_Name: String; Stack: TexeStack);
var o: TChartSeries;
begin
  o := TChartSeries(Stack.getSelf);
  o.Delete(Stack.AsInteger(1));
end;

class procedure Tps_Serie.am_setValueFormat(Const Func_Name: String; Stack: TexeStack);
var o: TChartSeries;
begin
  o := TChartSeries(Stack.getSelf);
  o.ValueFormat := Stack.AsString(1);
end;

class procedure Tps_Serie.am_setTitle(Const Func_Name: String; Stack: TexeStack);
var o: TChartSeries;
begin
  o := TChartSeries(Stack.getSelf);
  o.Title := Stack.AsString(1);
end;

class procedure Tps_Serie.am_ShowInLegend(Const Func_Name: String; Stack: TexeStack);
var o: TChartSeries;
begin
  o := TChartSeries(Stack.getSelf);
  o.ShowInLegend := Stack.AsBoolean(1);
end;

class procedure Tps_Serie.amAdd(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(
    TChartSeries(Stack.AsObject(2)).Add(Stack.AsFloat(1))
    );
end;

class procedure Tps_Serie.amAddEx(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(
    TChartSeries(Stack.AsObject(4)).Add(Stack.AsFloat(1), // valor
    Stack.AsString(2),                                    // título
    Stack.AsInteger(3))                                   // cor
    );
end;

procedure Tps_Serie.AddMethods;
begin
  with Procs do
    begin
    Add('SetXLabel',
        'Estabelece o título de um ponto dado seu índice.',
        '',
        [pvtInteger, pvtString],
        [nil       , nil],
        [false     , false],
        pvtNull,
        TObject,
        amSetXLabel);

    Add('setValueFormat',
        'Estabelece o formato dos valores',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_setValueFormat);

    Add('setTitle',
        '',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_setTitle);

    Add('ShowInLegend',
        'Estabelece se a série deverá ou não ser mostrada na legenda',
        '',
        [pvtBoolean],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_ShowInLegend);

    Add('Delete',
        'Remove um ponto da série',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        am_Delete);
    end;

  with Functions do
    begin
    Add('Add',
        'Adiciona um valor retornando o índice deste ponto na série.',
        '',
        [pvtReal],
        [nil    ],
        [false  ],
        pvtInteger,
        TObject,
        amAdd);

    Add('AddEx',
        'Adiciona um valor retornando o índice deste ponto na série.',
        'Parâmetros: Valor, Título e Cor',
        [pvtReal, pvtString, pvtInteger],
        [nil    , nil      , nil       ],
        [false  , false    , false     ],
        pvtInteger,
        TObject,
        amAddEx);

    Add('AddXY',
        'Adiciona um ponto (X, Y) retornando o índice deste ponto na série.',
        'Parâmetros: Valor de X, Valor de Y, Título e Cor',
        [pvtReal, pvtReal, pvtString, pvtInteger],
        [nil    , nil    , nil      , nil       ],
        [false  , false  , false    , false     ],
        pvtInteger,
        TObject,
        amAddXY);

    Add('GetXLabel',
        'Retorna o título de um ponto dado seu índice.',
        '',
        [pvtInteger],
        [nil    ],
        [false  ],
        pvtString,
        TObject,
        amGetXLabel);

    Add('AddNull',
        'Adiciona um valor nulo',
        '',
        [pvtString],
        [nil],
        [False],
        pvtInteger,
        TObject,
        am_AddNull);

    Add('Count',
        'Retorna o número de pontos',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        am_Count);
    end;
end;

class procedure Tps_Serie.amAddXY(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(
    TChartSeries(Stack.AsObject(5)).AddXY(Stack.AsFloat(1),      // X
                                          Stack.AsFloat(2),      // Y
                                          Stack.AsString(3),     // Label
                                          Stack.AsInteger(4)));  // Cor
end;

class procedure Tps_Serie.amGetXLabel(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(
    TChartSeries(Stack.getSelf()).XLabel[Stack.AsInteger(1)]);
end;

class procedure Tps_Serie.amSetXLabel(const Func_Name: String; Stack: TexeStack);
begin
  TChartSeries(Stack.getSelf()).XLabel[Stack.AsInteger(1)] := Stack.AsString(2);
end;

{ Tps_BarSerie }

class procedure Tps_BarSerie.amSetBarWidthPercent(const Func_Name: String; Stack: TexeStack);
begin
  TBarSeries(Stack.AsObject(2)).BarWidthPercent := Stack.AsInteger(1);
end;

procedure Tps_BarSerie.AddMethods;
begin
  with Procs do
    begin
    Add('SetBarWidthPercent',
        'Configura a largura das barras',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetBarWidthPercent);
    end;

  with Functions do
    begin
    end;
end;

{ Tps_LineSerie }

procedure Tps_LineSerie.AddMethods;
begin
  // ....
end;

{ Tps_PointSerie }

procedure Tps_PointSerie.AddMethods;
begin
  // ...
end;

{ Tps_PieSerie }

procedure Tps_PieSerie.AddMethods;
begin
  // ...
end;

end.
