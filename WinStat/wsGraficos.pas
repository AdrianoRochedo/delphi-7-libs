unit wsGraficos;

{
  FINALIDADES: Plotagem de gr�ficos que se utilizam da Classe TwsDataSet
  DATA DE CRIA��O: .......................................... 01/07/1998
  AUTOR: ..................................... Adriano Rochedo Concei��o
  AUTOR: ...................................... Anderson Priebe Ferrugem
  AUTOR: ...................................... Jos� Claudio Vahl J�nior
  �LTIMA ALTERA��O .......................................... 04/01/2000

  ROTINAS: .............................................................

  OBS.: Todas as func�es tem um par�metro chamado Funcs (TFunctions) que
  indica a lista de fun��es matem�ticas que ser�o calculadas

    NOME: ............................................... ws_ScatterPlot
    PAR�METROS: ... (1) Conjunto de dados a ser utilizado - [TwsDataSet]
            (2) Nome da vari�vel/coluna independente (eixo X) - [String]
            (3) Nome das vari�veis dependentes (eixo Y) - [TStrings]


  NOME: .................................................. ws_DesignPlot
  AUTOR: ...................................... Anderson Priebe Ferrugem
    PAR�METROS: ... (1) Conjunto de dados a ser utilizado - [TwsDataSet]
            (2) Nome da vari�vel/coluna independente (eixo X) - [String]
                (3) Nome das vari�veis dependentes (eixo Y) - [TStrings]
                         (4) Lista de fun��es matem�ticas - [TFunctions]


   NOME: ............................................. ws_HistoGramPlot
   AUTOR: ...................................... Anderson Priebe Ferrugem
    PAR�METROS: ... (1) Conjunto de dados a ser utilizado - [TwsDataSet]
            (2) Nome da vari�vel/coluna independente (eixo X) - [String]




    COMENT�RIOS: .......................................................
      Na vers�o de 16 bits (Delphi I), podemos plotar no m�ximo 16.000
      pontos para cada s�rie (dados e fun��es).
      N�o h� verifica��o dos tipos de vari�veis do Conjunto de Dados.
      O eixo X poder� ser igual a alguma vari�vel do eixo Y.
      Tem que existir pelo menos uma vari�vel X e uma Y.
      O 4. par�metro � opcional, quando queremos isto usamos (NIL).


   NOME: ................................................ ws_VectorsPlot
   AUTOR: .................................... Adriano Rochedo Concei��o
   PAR�METROS: ........... (x) Array de vetores constituindo as abcissas
               .......... (y) Array de vetores constituindo as ordenadas
               (Tipo) Array de vetores constituindo o tipo de cada s�rie

Todas as rotinas foram modificadas por Jos� Claudio Vahl J�nior
}

interface
uses Forms,
     Contnrs,
     Classes,
     Chart,
     Form_Chart,
     wsMatrix,
     wsVec,
     wsAvaliadorDeExpressoes,
     wsBXML_Output;

Type
  TwsSerieType = (stLine, stArea, stPoint, stBar, stPie,
                  stFastLine, stArrows, stGant, stBubble, stCandle);

  // Armazena janelas que cont�m gr�ficos
  // Por default, quando inst�ncias desta classe s�o destru�das, os objetos contidos
  // por ela N�O s�o destru�dos automaticamente.
  TwsGraphics = class
  private
    FList: TObjectList;
    FActiveChart: TChart;
    FActiveForm: TfoChart;
    function getChart(i: Integer): TChart;
    function getForm(i: Integer): TfoChart;
    function getCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Cria um novo gr�fico e retorna seu �ndice
    // Seta a propriedade Active para este novo gr�fico
    function New(const Caption, Title: String): Integer;

    // Adiciona um gr�fico e retorna sua posi��o na lista
    function Add(Grafico: TfoChart): Integer;

    // Retorna o formul�rio que cont�m o gr�fico
    property Form[i: Integer] : TfoChart read getForm;

    // Retorna diretamente o gr�fico
    property Chart[i: Integer] : TChart read getChart;

    // Retorna o formul�rio correntemente selecionado que cont�m o gr�fico
    property ActiveForm: TfoChart read FActiveForm;

    // Retorna diretamente o gr�fico correntemente selecionado
    property ActiveChart: TChart read FActiveChart;

    // Retorna o n�mero de gr�ficos contidos na lista
    property Count : Integer read getCount;
  end;

  {Gr�fico de fun��es}
  Function ws_FunctionPlot(
       Funcs : TFunctions    {Lista de express�es que ser�o calculadas}
    ): TfoChart;             {Janela que possui o gr�fico}

  {Dispers�o de pontos bivariado}
  Function ws_ScatterPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String;      {1 coluna}
     Y       : TStrings;    {n Colunas}
     Title   : string = ''
  ): TfoChart; overload;  {Janela que possui o gr�fico}

  {Dispers�o de pontos univariado}
  Function ws_ScatterPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Y       : TStrings;    {n Colunas, n�o existe ordenada}
     Title   : string = ''
  ): TfoChart; overload;    {Janela que possui o gr�fico}

  Function ws_ScatterPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String;      {1 coluna}
     Y       : TStrings;    {n Colunas}
     Funcs   : TFunctions;   {Lista de express�es que ser�o calculadas}
     Title   : string = 'Gr�fico de Dispers�o de Pontos Incluindo Fun��es'
  ): TfoChart; overload;  {Janela que possui o gr�fico}

  {Dispers�o de pontos com reta de tend�ncia}
  Function ws_ScatterPlotTendency(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String;      {1 coluna}
     Y       : TStrings;    {n Colunas}
     x1, y1  : Double;      {indica o primeiro ponto de uma reta}
     x2, y2  : Double;      {indica o segundo ponto de uma reta}
     Funcs   : TFunctions;   {Lista de express�es que ser�o calculadas}
     Title   : string = 'Dispers�o de Pontos'
  ): TfoChart;            {Janela que possui o gr�fico}

  Function ws_ScatterPlotIndex(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String       {1 coluna}
  ): TfoChart;            {Janela que possui o gr�fico}

  Function ws_DesignPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const Y : String;      {1 coluna}
     Factors : TStrings     {n Colunas}
  ): TfoChart;            {Janela que possui o gr�fico}

  Function ws_HistogramPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String ;      {1 coluna}
     Colunas : TStrings      {n Colunas}
  ): TfoChart;            {Janela que possui o gr�fico}

  Function ws_InteractionPlot(
     DS       : TwsDataSet;  {Origem dos dados}
     Const Y  : String;      {1 coluna, que sera plotado o eixo Y}
     Factors  : TStrings;    {n Colunas}
     out Means: TwsDataSet  // conjunto de dados com as medias
  ): TfoChart;            {Janela que possui o gr�fico}

Function ws_BoxPlot(
     Frames      : TwsDataSets;      // Frames com os dados
     Variaveis   : TStrings;         // Vari�veis para construcao das caixas
     out StatList: array of TwsGeneral
     ): TwsGraphics;                 // Lista com os gr�ficos criados

  Function ws_Lines(
    DSets   : TwsDataSets; {Origem dos Dados}
    Const X : String;     {1 coluna}
    Colunas : TStrings    {n Colunas}
  ): TfoChart;           {Janela que possui o grafico}

  Function ws_ColVert(DS: TwsDataSet; Colunas : TStrings; const Names: String=''): TfoChart;

  Function ws_PizzaPlot(DS: TwsDataSet; const Coluna: String; const Names: string=''): TfoChart;

  Function ws_VectorsPlot(
    const x        : array of TwsVec;     // vetores das abcissas
    const y        : array of TwsVec;     // vetores das ordenadas
    const Tipo     : array of TwsSerieType; // tipo de cada s�rie
    var   Erro     : Integer              // 0 - Sem Erros, > 0 Algum Erro
  ): TfoChart;                          // Resultado: Janela que possui o gr�fico

Function ws_MatrixPlot(
  x       : TwsVec;
  Y       : TwsGeneral;
  GType   : TwsSerieType;
  var Erro: Integer;
  w: TwsVec=nil;
  z: TwsVec=nil
  ): TfoChart;

Function ws_MatrixPlotTendency(
  x       : TwsVec;
  Y       : TwsGeneral;
  GType   : TwsSerieType;
  x1, y1  : Double;      {indica o primeiro ponto de uma reta}
  x2, y2  : Double;      {indica o segundo ponto de uma reta}
  var Erro: Integer;
  w: TwsVec=nil;
  z: TwsVec=nil
  ): TfoChart;


  // Gr�fico de quantis
  function GraphQuantil(DS: TwsDataSet; const YStr: String): TfoChart;
  // Gr�fico de simetria
  function SymmetryPlot(DS: TwsDataSet; const YStr: String): TfoChart;
  // Gr�fico quantil-quantil
  function QQGraph(DS: TwsDataSet; const XStr, YStr: String): TfoChart;

implementation
uses teengine,
     Controls,
     SysUtils,
     Graphics,
     Lists,
     Series,
     TeeChart_BoxPlot,
     wsGLib,
     wsConstTypes;

  {-------------------------  rotinas auxiliares  -------------------------}

Function SelectColor(i: Word; BlackWhite: Boolean = False): TColor;
var xx: Integer;
begin
  if BlackWhite then xx := 14 else xx := 12;
  inc(i, xx);
  case (i mod xx) of
    00: Result := clRed;
    01: Result := clGreen;
    02: Result := clBlue;
    03: Result := clMaroon;
    04: Result := clNavy;
    05: Result := clTeal; 
    06: Result := clFuchsia;
    07: Result := clYellow;
    08: Result := clAqua;
    09: Result := clPurple;
    10: Result := clOlive;
    11: Result := clLime;
    12: Result := clBlack;
    13: Result := clWhite;
    end;
end;
//function GetCaption(DS: TwsDataSet): String;
function GetCaption(DS: TwsDataSet): String;
Begin
  Result := DS.Name;
  If DS.MLab <> '' Then Result := Result + ' (' + DS.MLab + ')';
End;

function sAjusta(const x: double): String;
begin
  if x > 1 then
    Result := FloatToStrF(x, ffFixed, 3, 1)
  else
    Result := FloatToStrF(x, ffGeneral, 5, 3)
end;

{ TwsGraphics }

constructor TwsGraphics.Create;
begin
  inherited Create;
  FList := TObjectList.Create(False);
end;

destructor TwsGraphics.Destroy;
begin
  FList.Free;
  inherited;
end;

function TwsGraphics.Add(Grafico: TfoChart): Integer;
begin
  Result := FList.Add(Grafico);
  FActiveForm := Grafico;
end;

function TwsGraphics.getChart(i: Integer): TChart;
begin
  Result := getForm(i).Chart;
  FActiveChart := Result;
end;

function TwsGraphics.getForm(i: Integer): TfoChart;
begin
  Result := TfoChart(FList[i]);
  FActiveForm := Result;
end;

function TwsGraphics.New(const Caption, Title: String): Integer;
begin
  FActiveForm := TfoChart.Create(Caption);
  FActiveChart := FActiveForm.Chart;
  FActiveChart.Title.Text.Clear;
  FActiveChart.Title.Text.Add(Title);
  Result := Add(FActiveForm);
end;

function TwsGraphics.getCount: Integer;
begin
  Result := FList.Count;
end;

  {---------------------------  ws_FunctionPlot  -------------------------}

Function ws_FunctionPlot(
     Funcs : TFunctions      {Lista de express�es que ser�o calculadas}
  ): TfoChart;             {Janela que possui o gr�fico}
var i      : Integer;
    XValue : Double;        {Valor atual do eixo X}
    YValue : Double;        {Valor atual do eixo Y}
    L      : TLineSeries;  //vari�vel que cont�m a s�rie
Begin
  Result:= TfoChart.Create('Gr�fico de Fun��es');
  Result.Chart.View3D := False;
  If Funcs <> Nil Then
     For i := 0 to Funcs.Count - 1 do
       Begin
       L := Result.Series.AddLineSerie(Funcs.Func[i].Eval.Expression, selectColor(i));
       XValue := Funcs.Func[i].Li;

       While XValue < Funcs.Func[i].LS Do
         Begin
         if not IsMissValue(Funcs.GetValue(XValue, i)) then
           Begin
           YValue:= Funcs.GetValue(XValue, i);
           L.AddXY(XValue, YValue, '', clTeeColor);
           End;
         XValue := XValue + Funcs.Func[i].Incr;
         End;

       L.ShowInLegend:= not(Funcs.Count = 1); //mostra na legenda somente quando tem mais de uma s�rie
       End; {for Funcs.count}

  Result.Caption:= 'Fun��es definidas pelo usu�rio';
  Result.Chart.Title.Text.Clear;
  for i:=0 to Funcs.Count-1 do
    Result.Chart.Title.Text.Add(Funcs.Func[i].Eval.Expression);
End;

Function ws_ScatterPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String;      {1 coluna}
     Y       : TStrings;     {n Colunas}
     Title   : string = ''
  ): TfoChart;            {Janela que possui o gr�fico}
var i, j      : Integer;
    Ind       : TwsLIVec;      {�ndice das colunas a serem plotadas}
    XValue    : Double;        {Valor atual do eixo X}
    YValue    : Double;        {Valor atual do eixo Y}
    bAux      : Boolean;       {Vari�vel auxiliar para tratar valor perdido}
    xCol      : Word;          {�ndice da vari�vel independente}
    VRet      : Double;        {recebe o valor da celula no teste de valor perdido}
    L         : TPointSeries;  //vari�vel que cont�m a s�rie
Begin
  {Cria o janela do Gr�fico}
  Result:= TfoChart.Create(GetCaption(DS));
  Result.Chart.View3D := False;
  Try
    Ind:= DS.IndexColsFromStrings(Y);
    Screen.Cursor := crHourGlass;
    Try
      xCol:= DS.Struct.IndexOf(X);
      {Plota as colunas}
      For i:= 0 to Y.Count - 1 do
        Begin
        L:= Result.Series.AddPointSerie(Y[i], selectColor(i)); //Adiciona as S�ries

        // Insere as observacoes que nao possuem valores perdidos
        For j:= 1 to DS.NRows do
          Begin
          bAux:= True;
          if not DS.IsMissValue(j, Ind[i+1], VRet) then
            YValue:= VRet
          else
            bAux:= False;
          if not DS.IsMissValue(j, xCol, VRet) then
            XValue:= VRet
          else
            bAux:= False;

          if bAux then L.AddXY(XValue, YValue, ''{sAjusta(XValue)}, clTeeColor);
          End;

        L.ShowInLegend:= not (Y.Count = 1); //mostra na legenda somente quando tem mais de uma s�rie
        L.Pointer.HorizSize:= 3; L.Pointer.VertSize:= 3;
        End;

      Result.Chart.Title.Text.Clear;
      Result.Chart.Title.Text.Add(DS.MLab);

      if Y.Count = 1 then
        Result.Chart.LeftAxis.Title.Caption := Y[0] //StrListToStr(TStringList(Y));
      else
        Result.Chart.LeftAxis.Title.Caption := 'Y';

      Result.Chart.BottomAxis.Title.Caption := X;

    Finally
      Screen.Cursor := crDefault;
      Ind.Free;
    End;

  Except
    Result.Free();
    Result := nil;
    Raise;
  End;
End; {ScatterPlot bivariado }

Function ws_ScatterPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Y       : TStrings;     {n Colunas}
     Title   : string = ''
  ): TfoChart;            {Janela que possui o gr�fico}
var i, j      : Integer;
    Ind       : TwsLIVec;      {�ndice das colunas a serem plotadas}
    YValue    : Double;        {Valor atual do eixo Y}
    bAux      : Boolean;       {Vari�vel auxiliar para tratar valor perdido}
    VRet      : Double;        {recebe o valor da celula no teste de valor perdido}
    L         : TPointSeries;  //vari�vel que cont�m a s�rie
Begin
  {Cria o janela do Gr�fico}
  Result:= TfoChart.Create(GetCaption(DS));
  Result.Chart.View3D := False;
  Try
    Ind:= DS.IndexColsFromStrings(Y);
    Screen.Cursor := crHourGlass;
    Try
      {Plota as colunas}
      For i:= 0 to Y.Count - 1 do
        Begin
        Randomize;
        L:= Result.Series.AddPointSerie(Y[i], selectColor(i)); //Adiciona as S�ries

        // Insere as observacoes que nao possuem valores perdidos
        For j:= 1 to DS.NRows do
          Begin
          bAux:= True;
          if not DS.IsMissValue(j, Ind[i+1], VRet) then
             YValue:= VRet
          else
             bAux:= False;
          if bAux then L.AddXY(YValue, Random, '', clTeeColor);
          End;

        L.ShowInLegend:= not (Y.Count = 1); //mostra na legenda somente quando tem mais de uma s�rie
        L.Pointer.HorizSize:= 3; L.Pointer.VertSize:= 3;
        End;

      // esconder o eixo Y

      Result.Chart.Title.Text.Clear;
      Result.Chart.Title.Text.Add(DS.MLab);
      if (Y.Count = 1) then
        Result.Chart.BottomAxis.Title.Caption := Y[0]
      else
        Result.Chart.BottomAxis.Title.Caption := 'Valores das Vari�veis';

    Finally
      Screen.Cursor := crDefault;
      Ind.Free;
    End;

  Except
    Result.Free();
    Result := nil;
    Raise;
  End;
End; {ws_ScatterPlot univariado}

Function ws_ScatterPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String;      {1 coluna}
     Y       : TStrings;    {n Colunas}
     Funcs   : TFunctions;   {Lista de express�es que ser�o calculadas}
     Title   : string = 'Gr�fico de Dispers�o de Pontos Incluindo Fun��es'
  ): TfoChart;            {Janela que possui o gr�fico}
var i, j      : Integer;
    Ind       : TwsLIVec;      {�ndice das colunas a serem plotadas}
    XValue    : Double;        {Valor atual do eixo X}
    YValue    : Double;        {Valor atual do eixo Y}
    bAux      : Boolean;       {Vari�vel auxiliar para tratar valor perdido}
    xCol      : Word;          {�ndice da vari�vel independente}
    VRet      : Double; //variavel que recebe o valor da celula no teste de valor perdido
    L1        : TLineSeries;  //vari�vel que cont�m a s�rie
    L2        : TPointSeries;  //vari�vel que cont�m a s�rie
Begin
  {Cria o janela do Gr�fico}
  Result:= TfoChart.Create(GetCaption(DS));
  Result.Chart.View3D := False;
  Try
    Ind := DS.IndexColsFromStrings(Y);
    Screen.Cursor := crHourGlass;
    Try
      xCol := DS.Struct.IndexOf(X);

      {Plota as colunas}
      For i := 0 to Y.Count - 1 do
        Begin
        L2:= Result.Series.AddPointSerie(Y[i], selectColor(i));
        For j := 1 to DS.NRows do
          Begin
          bAux:= True;
          if not DS.IsMissValue(j, Ind[i+1], VRet) then
            YValue:= VRet
          else
            bAux:= False;
          if not DS.IsMissValue(j, xCol, VRet) then
            XValue:= VRet
          else
            bAux:= False;

          if bAux then L2.AddXY(XValue, YValue, ''{sAjusta(XValue)}, clTeeColor);
          End;
        L2.ShowInLegend:= not (Y.Count = 1); //mostra na legenda somente quando tem mais de uma s�rie
        End;

      {Plota as fun��es se definidas}
      If Funcs <> Nil Then
         For i:= 0 to Funcs.Count - 1 do
           Begin
           L1:= Result.Series.AddLineSerie(Funcs.Func[i].Eval.Expression, selectColor(i + Y.Count));
           XValue := Funcs.Func[i].Li;

           While XValue < Funcs.Func[i].LS Do
             Begin
             YValue:= Funcs.GetValue(XValue, i);
             L1.AddXY(XValue, YValue, ''{sAjusta(XValue)}, clTeeColor);
             XValue:= XValue + Funcs.Func[i].Incr;
             End;
           End;

      Result.Caption:= 'Fun��es definidas pelo usu�rio';
      Result.Chart.Title.Text.Clear;
      Result.Chart.Title.Text.Add(DS.MLab);
      Result.Chart.BottomAxis.Title.Caption:= X;
      Result.Chart.Title.Text.Add(Title);
    Finally
      Screen.Cursor := crDefault;
      Ind.Free;
    End;

  Except
    Result.Free;
    Result := nil;
    Raise;
  End;
End; {DSScatterPlot}


{---------------------------  ws_ScatterPlotTendency  -------------------------}

{Dispers�o de pontos com reta de tend�ncia}
  Function ws_ScatterPlotTendency(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String;      {1 coluna}
     Y       : TStrings;    {n Colunas}
     x1, y1  : Double;      {indica o primeiro ponto de uma reta}
     x2, y2  : Double;      {indica o segundo ponto de uma reta}
     Funcs   : TFunctions;   {Lista de express�es que ser�o calculadas}
     Title   : String = 'Dispers�o de Pontos'
  ): TfoChart;            {Janela que possui o gr�fico}

var L : TLineSeries;  //vari�vel que cont�m a s�rie
begin
  Result:= ws_ScatterPlot(DS, X, Y, Title);
  L:= Result.Series.AddLineSerie('Tend�ncia', clRed);
  L.ShowInLegend:= False;
  L.AddXY(x1, y1, '', clTeeColor);
  L.AddXY(x2, y2, '', clTeeColor);
end;

{---------------------------  ws_DesignPlot  -------------------------}
                              {delineamento}
Function ws_DesignPlot(DS      : TwsDataSet;  {Origem dos dados}
                       Const Y : String;      {1 coluna}
                       Factors : TStrings     {n Colunas}
                      ): TfoChart;          {Janela que possui o gr�fico}

var i,j,YCol : Integer;
    IndFactor: TwsLIVec;      //�ndice das colunas dos fatores
    YMAX,                     //Valor Maximo de Y
    YMED,                     //Valor Medio  de Y
    YMIN     : double;         //Valor Minimo de Y
    Coluna   : TwsFactor;     //Coluna atual que esta sendo utilizada
    Means    : TwsGeneral;    //Matriz que retorna os valores MAX, MIN, MED,SQD, Repeti��es
    LL       : TLineSeries;  //vari�vel que cont�m as s�ries de pontos
    LP       : TPointSeries;  //vari�vel que cont�m as s�ries de pontos
Begin
  {Pega o indice da coluna Y}
  YCol := DS.Struct.IndexOf(Y);

  {IndFactor recebe os indices dos fatores}
  IndFactor := DS.IndexColsFromStrings(Factors);

  if (IndFactor<>nil) and (YCol<>-1) then
    begin
    //Calcula as estatisticas
    Means:=DS.MeanFactor(IndFactor, YCol, YMED, YMIN, YMAX);
    IndFactor.Free;
    {Cria o janela do Gr�fico}
    Result := TfoChart.Create(GetCaption(DS));
    LL:= Result.Series.AddLineSerie('M�dia Geral', clTeeColor);
    LL.AddXY(1, YMED, '', clTeeColor);
    LL.AddXY(Factors.Count, YMED, '', clTeeColor);

    For i:= 1 to Factors.Count  do
      begin
      Coluna:= TwsFactor(DS.Struct.ColbyName(Factors[i-1]));
      LP:= Result.Series.AddPointSerie(Coluna.Name, selectColor(i));
      LP.Pointer.Style:= TSeriesPointerStyle(psDownTriangle);
      For j:= 1 to Coluna.LevelNames.Count Do {Varre os niveis do fator }
        LP.AddXY(i,Means[i,j],'',clTeeColor);
      end; {For i:= 1 to NumFactors}
    Means.Free;

    Result.Chart.View3D:=False;
    Result.Chart.Title.Text.Clear;
    Result.Chart.Title.Text.Add(DS.MLab);
    Result.Chart.LeftAxis.Title.Caption:= Y;
    Result.Chart.BottomAxis.Title.Caption:= 'Fatores';;
    Result.Chart.ZoomPercent(85);
    end;
End;
  {fim}


{---------------------------  ws_HistoGramPlot  -------------------------}
                                {Histograma}
Function ws_HistoGramPlot(
     DS      : TwsDataSet;  {Origem dos dados}
     Const X : String ;     {coluna a ser plotada}
     Colunas : TStrings     {n Colunas}
  ): TfoChart;            {Janela que possui o gr�fico}
var i,j,cY,xCol : Integer;  {xCol- �ndice da vari�vel independente}
    YValue      : Double;        {Valor atual do eixo Y}
    L           : TBarSeries;  //vari�vel que cont�m a s�rie
    st          : String;
    TabNum      : Boolean;
Begin
  {Cria o janela do Gr�fico}
  Result := TfoChart.Create('Histograma: ' + GetCaption(DS));
  xCol := DS.Struct.IndexOf(X);
  TabNum:=(DS.Struct.Col[1] is TwsQuantitative) or (DS.Struct.Col[1] is TwsNumeric);
  Try
    Screen.Cursor := crHourGlass;

    {Plota as colunas}
    For i := 0 to Colunas.Count-1 do
      Begin
      L := Result.Series.AddBarSerie(Colunas[i], clTeeColor, ord(bsRectangle), ord(mbSide));
      L.Marks.Visible := true;
      L.Marks.Style := smsValue;
      L.BarStyle := bsRectangle;  // ***********

      L.BarWidthPercent:=95;

      cY := DS.Struct.IndexOf(Colunas[i]);
      For j := 1 to DS.NRows do
        Begin
        if TabNum then
          st := IntToStr(j)
        else
          st := TwsFactor(DS.Struct.Col[xCol]).LevelNames[DS.AsInteger[j, xCol]];
        if not DS.IsMissValue(j, cY, YValue) then
           L.AddXY(j, YValue, st, clTeeColor);
        End;
      End;

    //mostra na legenda somente quando tem mais de uma s�rie
    L.ShowInLegend:=(Colunas.Count>1);
    if Colunas.Count=1 then
      Result.Chart.LeftAxis.Title.Caption :=Colunas[0];
    Result.Chart.BottomAxis.Title.Caption :='Classes';
    Result.Chart.View3D:= False;
    Result.Chart.Title.Text.Add('Histograma para Feq��ncia Relativa '+Colunas[0]);
    Result.Chart.Title.Text.Add(DS.MLab);
  Finally
    Screen.Cursor := crDefault;
  End;
End; {ws_HistoGramPlot}


{---------------------------  ws_InteractionPlot  -------------------------}
                                  {Intera��o}
Function ws_InteractionPlot(
     DS       : TwsDataSet;  // Origem dos dados
     Const Y  : String;      // 1 coluna, que sera plotado o eixo Y
     Factors  : TStrings;    // 2 Colunas
     out Means: TwsDataSet   // conjunto de dados com as medias
  ): TfoChart;            {Janela que possui o gr�fico}

{ Objetivo
    Obtem o grafico da interacao entre os dois fatores especificados
  Parametros
    DS: Conjunto de dados com os valores
    Y: Nome da vari�vel resposta relacionada aos fatores
    Factors: Nomes dos dois fatores para o gr�fico da intera��o
}
var
  YCol,                          // Indice da variavel numerica
  NumLin,i,j      : Integer;
  Factor,Asc,RVar : TwsLIVec;
  Vec             : TwsVec;
  C               : TwsDataSetCol;
  FacA,FacB       : TwsFactor;
  L               : TLineSeries; //vari�vel que cont�m a s�rie
Begin
  // Obtem o indice da coluna de Y
  YCol := DS.Struct.IndexOf(Y);
  if YCol<>-1 then                        // Variavel Y existe no conjunto ?
    begin
    Factor:= TwsLIVec.Create(2);          // Indices dos fatores
    Factor[1]:= DS.Struct.IndexOf(Factors[0]);
    FacA:=TwsFactor(DS.Struct.ColByName(Factors[0]));
    Factor[2]:= DS.Struct.IndexOf(Factors[1]);;
    FacB:=TwsFactor(DS.Struct.ColByName(Factors[1]));

    Asc:= TwsLIVec.Create(2);
    Asc[1]:= 1;                           // Ordenacao em ordem ascendente
    Asc[2]:= 1;

    DS.SortRows(Factor,Asc);              // Ordena para calcular as medias das combinacoes
    Asc.Free;

    RVar := TwsLIVec.Create(1);
    RVar[1] := YCol;
                                          // Fatores + medias + repet
    Means := TwsDataSet.Create('Med_Comb');
    Means.MLab:='M�dias de combina��es de n�veis para '+Y;

    C:=TwsQualitative.Create(Factors[0],FacA.Lab);        { 1 }
    With TwsQualitative(C) Do
      for i:=0 to FacA.Levels-1 do
        AddLevel(FacA.LevelNames[i]);
    Means.Struct.AddColEx(C);

    C:=TwsQualitative.Create(Factors[1],FacB.Lab);        { 2 }
    With TWSQualitative(C) Do
      for i:=0 to FacB.Levels-1 do
        AddLevel(FacB.LevelNames[i]);
    Means.Struct.AddColEx(C);

    C:=TwsNumeric.Create('Medias','M�dias de '+Y);        { 3 }
    Means.Struct.AddColEx(C);

    C:=TwsNumeric.Create('Repet','N�mero de repeti��es'); { 4 }
    Means.Struct.AddColEx(C);

    NumLin:=1;
    repeat
      Vec := DS.SortKeyMeans(Factor,RVar,NumLin);
      if Vec <> nil then Means.MAdd(Vec);
    until Vec = nil;

    if (FacA.Levels*FacB.Levels <> Means.NRows) then
      begin
      Means.Free;
      Result:=nil;
      Raise Exception.Create('Todas as caselas necessitam ter observa��es');
      end;

    {Cria o janela do Gr�fico}
    Result := TfoChart.Create(GetCaption(DS));
    NumLin:=0;
    For i:= 1 to FacA.Levels do  {Cria uma serie para cada nivel do fator A}
      Begin
      L:= Result.Series.AddLineSerie(FacA.LevelNames[i-1], selectColor(i-1));
      L.Pointer.Style:= TSeriesPointerStyle(psStar);
      For j:= 1 to FacB.Levels do
        Begin
        Inc(NumLin);
        L.AddXY(Means[NumLin,2],Means[NumLin,3],FacB.LevelNames[j-1],clTeeColor);
        End;
      End;

    with Result do
      begin
      Result.Chart.View3D:=False;
      Chart.Title.Text.Clear;
      Chart.BottomAxis.Title.Caption:= Factors[1];
      Chart.LeftAxis.Title.Caption:= 'M�dias de '+Y;
      end;
    end;
    RVar.Free
End; {ws_InteractionPlot}

// Resultar� em tantos gr�ficos quanto o n�mero de Vari�veis para an�lise
Function ws_BoxPlot(
     Frames      : TwsDataSets;         // Lista com os conjuntjos de dados
     Variaveis   : TStrings;            // Variaveis para construcao do grafico
     out StatList: array of TwsGeneral  // Lista com as matrizes de estatisticas
     ): TwsGraphics;                  {Lista com os gr�ficos criados}

        { C�digo das estat�sticas
      Codigo             Estatistica          Posicao
           0:    0   0%  Minimo                     1
           4:  .25  25%      Q1                     2
           5:  .50  50%  Mediana                    3
           6:  .75  75%      Q3                     4
           7:    1 100%  Maximo                     5
           9:            QAmplit                    6
          13:            Cerca Inferior             7
          14:            Cerca Superior             8
          15:            val < cerca inf            9
          16:            val > cerca sup           10
          18:            Valor adjacente inferior  11
          19:            Valor adjacente superior  12
        }

var i, j, k,nBox  : Integer;      // contadores
    DSet          : TwsGeneral;   // matriz que armazena as estat�sticas calculadas
    DS            : TwsDataSet;   // recebe um elemento dos frames por vez
    IndCol        : TwsLIVec;     // �ndice das Variaveis a serem plotadas
    Stat          : TwsLIVec;     // Vetor com as estatiscas que deverao ser calculadas para o DataSet
    AOpen, AClose,
    AHigh, ALow,
    Max,Min,Med   : Double;
    Extremos      : TDoubleList;
    LV            : TBoxPlotWithMinMax_Series; //vari�vel tempor�ria que cont�m a s�rie de velas
Begin
  {Cria o janela do Gr�fico}
  Result := TwsGraphics.Create;
  Try
    Screen.Cursor:= crHourGlass;
    Try
      {Indices das estatisticas que ser�o calculadas}
      Stat := TwsLIVec.CreateFrom([0,4,5,6,7,9,13,14,15,16,18,19]);

      // Vari�vel que conter� os valores dos pontos extremos
      Extremos := TDoubleList.Create;

      // Cria o gr�fico
      Result.New('', '');
      Result.ActiveChart.View3D := False;

      IndCol := Frames[0].IndexColsFromStrings(Variaveis);

      // Existir�o tantas s�ries quanto vari�veis
      for k := 0 to Variaveis.Count-1 do
        begin
        // Cria a s�rie BoxPlot
        LV := TBoxPlotWithMinMax_Series.Create(nil);
        LV.ParentChart := Result.ActiveChart;
        LV.Title := Variaveis[k];
        end;
      nBox:=0;
      // Para cada conjunto de dados
      for k := 0 to Frames.Count-1 do
        Begin
        DS := Frames[k];
        DSet := DS.MatOrderStat(IndCol,Stat); StatList[k]:=DSet;
        // Para cada variavel
        For i := 1 to DSet.NRows do   // cada linha de DSet � uma vari�vel
          begin
          AOpen   := DSet[i, 4]; {Q3}
          AClose  := DSet[i, 2]; {Q1}
          Max     := DSet[i, 5]; {Max}
          Min     := DSet[i, 1]; {Min}
          Med     := DSet[i, 3]; {Mediana}
          ALow    := DSet[i,11]; {valor adjacente inferior}
          AHigh   := DSet[i,12]; {valor adjacente superior}

          Extremos.Clear;

          if DSet[i,9] > 0 then //se existem valores abaixo da cerca inferior
             for j:= 1 to DS.NRows do
               if DS[j,IndCol[i]] < ALow then
                  Extremos.Add(DS[j, IndCol[i]]);

          if DSet[i,10] > 0 then //se exitem valores acima da cerca superior
             for j:= 1 to DS.NRows do
               if DS[j,IndCol[i]] > AHigh then
                  Extremos.Add(DS[j,IndCol[i]]);

          Inc(nBox);
          LV := TBoxPlotWithMinMax_Series(Result.ActiveChart.Series[i-1]);
          LV.AddBox(DS.Group,nBox,aClose,aOpen,aHigh,aLow,Min,Max,Med,Extremos.ToArray);
          end; // for i
        end; // for kk

      // Centraliza um pouco mais os pontos se tenho mais de uma caixa por gr�fico
      // Nunca use este m�todo antes de ter adicionado os pontos !!
      Result.ActiveChart.ZoomPercent(85);
      if Frames.Count=1 then
        Result.ActiveChart.BottomAxis.LabelStyle:=talNone;
      for i:=0 to Result.ActiveChart.SeriesCount-1 do
        if nBox < 11 then
          TBoxPlotWithMinMax_Series(Result.ActiveChart.Series[i]).BoxWidth := 24;
      Result.ActiveChart.Legend.Visible:=True;
      if DSet.nRows=1 then
        begin
        Result.ActiveChart.Legend.Visible:=False;
        Result.ActiveChart.LeftAxis.Title.Caption:= DSet.RowName[1];
        end;

    Finally
      Stat.Free;
      Extremos.Free;
      IndCol.Free;
    End;
  Finally
    Screen.Cursor := crDefault;
  End;
End; {ws_BoxPlot}

{---------------------------  ws_ScatterPlotIndex  -------------------------}

Function ws_ScatterPlotIndex(
  DS      : TwsDataSet;  {Origem dos dados}
  Const X : String       {1 coluna}
): TfoChart;           {Janela que possui o gr�fico}

var i         : Integer;
    XValue    : Double;        {Valor atual do eixo X}
    YValue    : Double;        {Valor atual do eixo Y}
    bAux      : Boolean;       {Vari�vel auxiliar para tratar valor perdido}
    xCol      : Word;          {�ndice da vari�vel independente}
    VRet      : Double;        {recebe o valor da celula no teste de valor perdido}
    L         : TPointSeries;  //vari�vel que cont�m a s�rie
Begin
  {Testo se as colunas existem, simplesmente fa�o uma refer�ncia}
  DS.Struct.ColByName(X);

  {Cria o janela do Gr�fico}
  Result := TfoChart.Create(GetCaption(DS));
  Result.Chart.View3D:= False;
  Try
    {Adiciona a s�rie - coluna}
    L:= Result.Series.AddPointSerie(X, clRed);
    Screen.Cursor:= crHourGlass;
    Try
      xCol:= DS.Struct.IndexOf(X);

      {Plota a Vari�vel}
      For i:= 1 to DS.NRows do
        Begin
        bAux:= True;
        if not DS.IsMissValue(i, xCol, VRet) then
          YValue := VRet
        else
          bAux:= False;

        XValue:= i;
        if bAux then L.AddXY(XValue, YValue, ''{sAjusta(XValue)}, clTeeColor);
        End;
      L.ShowInLegend:= False;

    with Result do
      begin
      Result.Chart.View3D:=False;
      Chart.Title.Text.Clear;
      Chart.Title.Text.Add(DS.MLab);
      Chart.BottomAxis.Title.Caption:= '�ndice';
      Chart.LeftAxis.Title.Caption:= X;
      end;

    Finally
      Screen.Cursor := crDefault;
    End;

  Except
    Result.Free;
    Result:= nil;
    Raise;
  End;
end;

 {---------------------------  ws_Lines  -------------------------}

Function ws_Lines(
   DSets   : TwsDataSets; {Origem dos Dados}
   Const X : String;      {1 coluna}
   Colunas : TStrings     {n Colunas}
): TfoChart;              {Janela que possui o grafico}
Var i,j,k  : integer;     {Contadoras}
    XValue : Double;      {Valor atual do eixo X}
    YValue : Double;      {Valor atual do eixo Y}
    ind    : TwsLIVec;    {�ndice das Colunas a serem plotadas}
    bAux   : Boolean;     {Vari�vel auxiliar para tratar valor perdido}
    xCol   : Word;        {Coluna que est� sendo plotada}
    VRet   : Double;      //variavel que recebe o valor da celula no teste de valor perdido
    L      : TLineSeries; //vari�vel que cont�m a s�rie
    DS     : TwsDataSet;
    st     : string;
Begin
  Result:= TfoChart.Create(GetCaption(DSets[0]));
  Try
    Screen.Cursor:= crHourGlass;
    ind:= DSets[0].IndexColsFromStrings(Colunas);
    xCol:= DSets[0].Struct.IndexOf(X);
    for k:=0 to DSets.Count-1 do
      begin
      DS:=DSets[k];
      For i:= 0 to Colunas.Count - 1 do
        Begin
        st:=DS.MLab+'.'+Colunas[i];
        Delete(st,1,7);
        L:= Result.Series.AddLineSerie(st, gCores[k+i]);

        For j:= 1 to DS.NRows do
          Begin
          bAux:= True;
          //para n�o plotar o ponto que � valor perdido com o valor anterior
          if not DS.IsMissValue(j, Ind[i+1], VRet) then
            YValue:= VRet
          else
            bAux:= False;
          if not DS.IsMissValue(j, xCol, VRet) then
            XValue:= VRet
          else
            bAux:= False;

          if bAux then L.AddXY(XValue, YValue, ''{sAjusta(XValue)}, clTeeColor);
          End;
        L.ShowInLegend:= not(Colunas.Count = 1);
        End;
      end;
    Result.Chart.View3D:=False;
//    Result.Chart.Title.Text.Clear;
//    Result.Chart.Title.Text.Add(DS.MLab);
    if Colunas.Count=1 then
      Result.Chart.LeftAxis.Title.Caption:= Colunas[0]
    else
      Result.Chart.LeftAxis.Title.Caption:= 'Y';;
    Result.Chart.BottomAxis.Title.Caption:= X;
  Finally
    Screen.Cursor := crDefault;
  End;
End;

 {---------------------------  ws_ColVert  -------------------------}

Function ws_ColVert(DS: TwsDataSet; Colunas : TStrings; const Names: String=''): TfoChart;
{ Objetivo
    Constr�i gr�ficos de colunas verticais
  Par�metros
    DS: Conjunto de dados
    Colunas: Lista das vari�veis. Os ret�ngulos para as diferentes vari�veis ser�o colocados
      lado a lado
    Names: Coluna (do tipo fator) onde estar�o os nomes que identificar�o cada coluna. Se n�o
      for alterado o valor default (vazio), cada coluna ser� identificada com o �ndice apenas.
}
Var
  i,j,                     // Contadoras
  nCol,                    // coluna com os nomes
  xCol   : Integer;        // coluna com os valores
  XValue,                  // Valor atual do eixo X
  YValue : Double;         // Valor atual do eixo Y
  st     : string;         // Nome para a coluna
  L      : TBarSeries;     //vari�vel que cont�m a s�rie
Begin
  Result:= TfoChart.Create(GetCaption(DS));
  Try
    Screen.Cursor:= crHourGlass;
    if Names<>'' then
      nCol:=DS.Struct.IndexOf(Names)
    else
      nCol:=-1;
    For i:= 0 to Colunas.Count - 1 do
      Begin
      //Tipo �: TMultiBar=(mbNone,mbSide,mbStacked,mbStacked100)
      //Estilo de barra TBarStyle=(bsRectangle,bsPyramid,bsInvPyramid,bsCilinder,bsEllipse,bsArrow,bsRectGradient);
      L:= Result.Series.AddBarSerie(Colunas[i], clTeeColor, 6, 1);

      xCol:= DS.Struct.IndexOf(Colunas[i]);
      For j:= 1 to DS.NRows do
        Begin
        XValue:= j;
        if nCol=-1 then
          if DS.ColIdentName<>'' then
            st:=DS.RowName[j]
          else
            st:=IntToStr(j)
        else
          st:=TwsFactor(DS.Struct.Col[nCol]).LevelNames[Trunc(DS[j,nCol])];
        if not DS.IsMissValue(j,xCol,YValue) then
          L.AddXY(XValue,YValue,st,clTeeColor);
        End;
      L.ShowInLegend:= (Colunas.Count > 1); //mostra na legenda somente quando tem mais de uma s�rie
      L.BarWidthPercent:= 93; //98
      End;
    Result.Chart.Title.Text.Clear;
    Result.Chart.View3D:= False;
    Result.Chart.Title.Text.Add(DS.MLab);
    if Colunas.Count=1 then
      Result.Chart.LeftAxis.Title.Caption:= Colunas[0]
    else
      Result.Chart.LeftAxis.Title.Caption:= 'Y';
  Finally
  Screen.Cursor := crDefault;
  End;
End;

 {---------------------------  ws_PizzaPlot  -------------------------}

Function ws_PizzaPlot(DS: TwsDataSet; const Coluna: String; const Names: string=''): TfoChart;
{ Objetivo
    Constr�i gr�ficos de setores (pizzas)
  Par�metros
    DS: Conjunto de dados
    Coluna: Nome da vari�vel que forncer� os valores
    Names: Coluna (do tipo fator) onde estar�o os nomes que identificar�o cada setor. Se n�o
      for alterado o valor default (vazio), cada setor ser� identificada com o �ndice apenas.
}
Var
  j        : integer;    // Contadoras
  YValue   : Double;     // Valor atual do eixo Y
  xCol,nCol: Integer;    // Coluna que est� sendo plotada
  L        : TPieSeries; // Vari�vel que cont�m a s�rie
  st       : String;
Begin
  Result:= TfoChart.Create(GetCaption(DS));
  Try
    Screen.Cursor:= crHourGlass;
    L:= Result.Series.AddPieSerie(Coluna);
    xCol:=DS.Struct.IndexOf(Coluna);
    if Names<>'' then
      nCol:=DS.Struct.IndexOf(Names)
    else
      nCol:=-1;
    For j:= 1 to DS.NRows do
      Begin
      if nCol=-1 then
        if DS.ColIdentName<>'' then
          st:=DS.RowName[j]
        else
          st:=IntToStr(j)
      else
        st:=TwsFactor(DS.Struct.Col[nCol]).LevelNames[Trunc(DS[j,nCol])];
      if not DS.IsMissValue(j,xCol,YValue) then
        L.Add(YValue,st,clTeeColor);
      End;
    L.ShowInLegend:= True;
    Result.Chart.Title.Text.Clear;
    Result.Chart.Title.Text.Add('Gr�fico de Pizza para '+Coluna);
  Finally
    Screen.Cursor := crDefault;
  End;
End;

 {---------------------------  ws_VectorsPlot  -------------------------}

Function ws_VectorsPlot(
  const x        : array of TwsVec;       // vetores das abcissas
  const y        : array of TwsVec;       // vetores das ordenadas
  const Tipo     : array of TwsSerieType; // tipo de cada s�rie
  var   Erro     : Integer                // 0 - Sem Erros, > 0 Algum Erro
  ): TfoChart;                          // Resultado: Janela que possui o gr�fico
{  Objetivo
     Faz o gr�fico de dispers�o bivariado entre os vetores que comp�em os arrays x e y
   Par�metros
     x: array [0..Length(x)] com os endere�os dos vetores para o eixo das abscissas
     y: array [0..Length(y)] com os endere�os dos vetores para o eixo das ordenadas
     Tipo: array [0..Length(Tipo)] com os tipos de pontos. Os tipos v�lidos s�o:
       stLine: para gr�fico de linhas entre x[i] x y[i]
       stPoint: para gr�fico de pontos entre x[i] x y[i]
       stBar: para gr�fico de barras entre x[i] x y[i]
       stPie: para gr�fico de pizza entre x[i] x y[i]
     Erro: retorna c�digo de erro
       0: processamento Ok
       1: N�o h� correspond�ncia na quantidade de vetores em x e y
       2: Os pares de vetores x[i] e y[i] devem ter a mesma dimens�o
   Resultado
     Janela gr�fica
   Observa��es
     pode gerar excess�o no processamento das janelas gr�ficas       
}

var
  i,j : Integer;
  LX  : TChartSeries;                     // vari�vel que cont�m a s�rie
Begin
  Erro := 0;

  if (Length(x) <> Length(y)) or (Length(x) <> Length(Tipo)) then
     begin
     Erro := 1;
     exit; // N�o h� correspond�ncia na quantidade dos par�metros
     end;

  for i := 0 to High(x) do
    if x[i].Len <> y[i].Len then
       begin
       Erro := 2;
       exit; // N�o h� correspond�ncia no n�mero de elementos de cada par de vetores
       end;

  {Cria o janela do Gr�fico}
  Result:= TfoChart.Create('Gr�ficos de Vetores');
  Try
    Screen.Cursor := crHourGlass;
    Try
      for i:= 0 to High(Tipo) do
        Begin
        case Tipo[i] of
          stLine     : LX:= Result.Series.AddLineSerie(y[i].Name, clTeeColor);
          stArea     : Raise Exception.Create('N�o Implementado para �rea!');
          stPoint    : LX:= Result.Series.AddPointSerie(y[i].Name, clTeeColor);
          stBar      : LX:= Result.Series.AddBarSerie(y[i].Name, clTeeColor, 1, 0);
          stPie      : LX:= Result.Series.AddPieSerie(y[i].Name);
          stFastLine : Raise Exception.Create('N�o Implementado para fastline!');
          stArrows   : Raise Exception.Create('N�o Implementado para setas!');
          stGant     : Raise Exception.Create('N�o Implementado para gant!');
          stBubble   : Raise Exception.Create('N�o Implementado para bolhas!');
          stCandle   : Raise Exception.Create('N�o Implementado para candle!');
          End; //case Tipo[i]

        { Insere os valores na serie }
        For j:= 1 to x[i].Len do
          if not (IsMissValue(y[i].Data[j]) or IsMissValue(x[i].Data[j])) then
             LX.AddXY(x[i].Data[j],y[i].Data[j], ''{sAjusta(XValue)}, clTeeColor);
        LX.ShowInLegend := True;
        End; //for i:=1 to High(Tipo)

      Result.Chart.View3D:= False;
      Result.Chart.Title.Text.Clear;
    Except
      Result.Free;
      Result := nil;
      Erro := 3;
    End;
  Finally
    Screen.Cursor := crDefault;
  End;
end;

 {---------------------------  ws_MatrixPlot  -------------------------}

Function ws_MatrixPlot(x        : TwsVec;
                       Y        : TwsGeneral;
                       GType    : TwsSerieType;
                       var Erro : Integer;
                       w        : TwsVec = nil;
                       z        : TwsVec = nil): TfoChart;
{ Objetivo
    Constroi gr�fico de pontos entre um conjunto de vetores e as linhas da matriz
  Par�metros
    x    : Vetor das abcissas, �nico para todas as linhas de Y
    Y    : Matriz geral onde cada linha (ordenadas) � uma s�ries de pontos
    GType: Tipo comum para todas as s�ries.
    Erro : retorna c�digo de erro
      0: processamento Ok
      2: x e as linhas de Y n�o t�m o mesmo n�mero de elementos
      3: w e z n�o t�m o mesmo n�mero de elementos. N�o impede que o gr�fico entre x
        e as linhas de Y seja constru�do.
      4: Erro na constru��o do gr�fico
    w, z: vetores para constru��o de s�ries adicionais, se apontarem para algum endere�o
      v�lido. Necessitam ter o mesmo n�mero de elementos mas podem ter dimens�o distinta
      de x e das linhas de Y
  Retorno: janela gr�fica
}
var
  i,j : Integer;
  LX  : TChartSeries; //vari�vel que cont�m a s�rie
Begin
  Erro := 0;
  if x.Len <> Y.NCols then
    begin
    Erro := 2;
    exit; // N�o h� correspond�ncia no n�mero de elementos de cada par de vetores
    end;
  {Cria o janela do Gr�fico}
  Result:= TfoChart.Create('Gr�fico de Dispers�o');
  Try
    Screen.Cursor := crHourGlass;
    Try
      for i:= 1 to Y.NRows do
        Begin
        case GType of
          stLine     : LX:= Result.Series.AddLineSerie(Y.RowName[i],clTeeColor);
          stArea     : Raise Exception.Create('N�o Implementado para �rea!');
          stPoint    : LX:= Result.Series.AddPointSerie(Y.RowName[i],gCores[i-1]{clTeeColor});
          stBar      : begin
                       LX:= Result.Series.AddBarSerie(Y.RowName[i],clTeeColor,0,1);
                       TBarSeries(LX).BarWidthPercent:=95;
                       end;
          stPie      : LX:= Result.Series.AddPieSerie(Y.RowName[i]);
          stFastLine : Raise Exception.Create('N�o Implementado para fastline!');
          stArrows   : Raise Exception.Create('N�o Implementado para setas!');
          stGant     : Raise Exception.Create('N�o Implementado para gant!');
          stBubble   : Raise Exception.Create('N�o Implementado para bolhas!');
          stCandle   : Raise Exception.Create('N�o Implementado para candle!');
          End; //case Tipo
        {Plota os Vetores}
        For j:= 1 to Y.NCols do
          if not (IsMissValue(Y[i,j]) or IsMissValue(x[j])) then
             LX.AddXY(x[j],Y[i,j],'',clTeeColor);
        LX.ShowInLegend := Y.RowName[i]<>'';
        End; //for i:=1 to Y.NRows
        // Inclui series adicionais, se for o caso
        if (w<>nil) and (z<>nil) then
          if w.Len = z.Len then
            begin
            LX:= Result.Series.AddPointSerie(z.Name,clTeeColor);
            For j:= 1 to w.Len do
              if not (IsMissValue(w[j]) or IsMissValue(z[j])) then
                 LX.AddXY(w[j],z[j],'',clTeeColor);
            LX.ShowInLegend := True;
            end
          else
            Erro:=3;
      Result.Chart.View3D:= False;
      Result.Chart.Title.Text.Clear;
    Except
      Result.Free;
      Erro:=4;
      Result := nil;
    End;
  Finally
    Screen.Cursor := crDefault;
  End;
end;

Function ws_MatrixPlotTendency(
  x       : TwsVec;
  Y       : TwsGeneral;
  GType   : TwsSerieType;
  x1, y1  : Double;      {indica o primeiro ponto de uma reta}
  x2, y2  : Double;      {indica o segundo ponto de uma reta}
  var Erro: Integer;
  w       : TwsVec=nil;
  z       : TwsVec=nil
  ): TfoChart;
var
  L : TLineSeries;  //vari�vel que cont�m a s�rie
begin
  Result:=ws_MatrixPlot(x,Y,GType,Erro,w,z);
  L:= Result.Series.AddLineSerie('Formato nulo', clTeeColor);
//  L.ShowInLegend:= False;
  L.AddXY(x1, y1, '', clTeeColor);
  L.AddXY(x2, y2, '', clTeeColor)
end; // MatrixPlotTendency

function GraphQuantil(DS: TwsDataSet; const YStr: String): TfoChart;
{ Objetivo
    Construir gr�fico dos quantis observados (v�riavel com valores ordenados) contra
    as respectivas propor��es de dados que esses valores representam.
  Par�metros
    DS  : Conjunto de dados
    YStr: Vari�vel para obten��o dos quantis observados
}
var
  pData,vData: TwsVec;
  k,n        : Integer;
  Graf_Erro  : Integer;
begin
  vData := DS.CopyCol(DS.Struct.IndexOf(YStr));
  n := vData.Len;
  pData := TwsDFVec.Create(n);
  vData.QuickSort(True); // Esta ordem deve ser ascendente
  try
    for k := 1 to n do
      pData[k] := (k-0.5)/n; // como valor correspondente para o eixo X
    pData.Name:=YStr;
    Result := ws_VectorsPlot([pData],[vData],[stPoint],Graf_Erro);
    if Graf_Erro = 0 then
       begin
       Result.Chart.LeftAxis.Title.Caption:= YStr;
       Result.Chart.BottomAxis.Title.Caption:= 'Quantis';
       Result.Chart.Title.Text.Add('Gr�fico de Quantis');
       end
    else
       raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo ' + intToStr(Graf_Erro));
  finally
    pData.Free; vData.Free;
  end;
end; { GraphQuantil }

function SymmetryPlot(DS: TwsDataSet; const YStr: String): TfoChart;
{ Objetivo
    Construir gr�fico para aux�lio na detec��o de distribui��es assim�tricas
  Par�metros
    DS         : Conjunto de dados
    YStr       : Coluna para a qual se deseja fazer o gr�fico.
}
var
  vData,xData,yData: TwsVec;
  Md               : Double;
  k,n,n1,j         : Integer;
  Graf_Erro        : Integer;
begin
    vData := DS.CopyCol(DS.Struct.IndexOf(YStr));  // Obtem os valores das colunas
    vData.QuickSort(True);
    n := vData.Len;
    if Odd(n) then
      begin                       // Obtem a mediana
      k := (n+1) div 2;
      Md := vData[k]
      end
    else
      begin
      k := n div 2;
      Md := (vData[k]+vData[k+1])/2
      end;
    xData := TwsDFVec.Create(k);
    yData := TwsDFVec.Create(k);
    n1 := n+1;
    try
      for j := 1 to k do
        begin
        xData[j] := Md - vData[j];       // para o eixo X.
        yData[j] := vData[n1-j]-Md;      // para o eixo Y
        end;
      xData.Name:= 'Simetria';
      yData.Name:= 'Diferen�as';
      Result := ws_VectorsPlot([xData,xData],[xData,yData],[stPoint,stPoint],Graf_Erro);
      if Graf_Erro = 0 then
         begin
         Result.Chart.LeftAxis.Title.Caption:= 'Mediana - Y';
         Result.Chart.BottomAxis.Title.Caption:= 'Y - Mediana';
         Result.Chart.Title.Text.Add('Gr�fico de Simetria para '+YStr)
         end
      else
         raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo '+intToStr(Graf_Erro));
    finally
      vData.Free;
      xData.Free;
      yData.Free;
    end;
end; { SymmetryPlot }

function QQGraph(DS: TwsDataSet; const XStr, YStr: String): TfoChart;
{ Objetivo
    Construir gr�fico dos quantis observados de uma vari�vel contra quantis observados de
    outra.
  Par�metros
    DS  : Conjunto de dados
    XStr: Nome da vari�vel X
    YStr: Nome da vari�vel Y
}
var
  vXData,vYData: TwsVec;
  Graf_Erro    : Integer;
begin
  vXData := DS.CopyCol(DS.Struct.IndexOf(XStr));// Obtem, a coluna para a variavel X
  vXData.QuickSort(True);                       // Em ordem ascendente
  vYData := DS.CopyCol(DS.Struct.IndexOf(YStr));// Obtem, a coluna para a variavel Y
  vYData.QuickSort(True);                       // Em ordem ascendente
  vXData.Name:='X=Y';
  vYData.Name:=YStr;
  Result := ws_VectorsPlot([vXData,vXData],[vXData,vYData],[stPoint,stPoint],Graf_Erro);
  if Graf_Erro=0 then
     begin
     Result.Chart.Title.Text.Add('Gr�fico de Quantis Emp�ricos');
     Result.Chart.LeftAxis.Title.Caption:= XStr;
     Result.Chart.BottomAxis.Title.Caption:= YStr;
     end;
  vXData.Free;
  vYData.Free
end; { QQGraph }


end.
