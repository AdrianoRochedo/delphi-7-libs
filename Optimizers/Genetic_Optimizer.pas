unit Genetic_Optimizer;

  {TODO 1 -cProgramacao: Tolerancia de parada}

interface
uses Math,
     Classes,
     Contnrs,
     Dialogs,
     SysUtils,
     SysUtilsEx,
     WinUtils,
     MSXML4,
     XML_Utils,
     Optimizer_Interfaces,
     Optimizer_Base,
     Optimizer_Form_ParsManager,
     Optimizer_Form_ParsManager_Genetic;

const
  GO_Evolution = 1; // indica que uma evolução ocorreu
  GO_Complex   = 2; // indica que a evolução de um complexo ocorreu

type
  TGens = TFloatArray;

  // Representa um individuo em uma populacao (um ponto amostral)
  TIndividual = {private} class
  private
    Gens: TGens;
    Aptness: real;
    Prob: real;
    constructor Create(Pars: TParameters);
    procedure toXML(x: TXML_Writer);
    procedure LoadFromXML(node: IXMLDomNode);
  end;

  // Representa um conjunto de individuos
  TIndividualList = {private} class
  private
    FList: TObjectList;
    destructor Destroy(); override;
    function getItem(i: integer): TIndividual;
    function getCount: integer;
    procedure setItem(i: integer; const Value: TIndividual);
    function getLast: TIndividual;
  public
    // Cria uma instancia de individuos
    constructor Create(const OwnIndividuals: boolean);

    // Ordena a populacao em ordem crescente de Aptidoes
    procedure Sort();

    // Adiciona um individuo ao conjunto
    procedure Add(Individual: TIndividual);

    // Esvazia a lista
    procedure Clear();

    // Troca um elemento pelo outro
    procedure Exchange(i1, i2: integer);

    // Retorna o numero de individuos da populacao
    property Count: integer read getCount;

    // Retorna o Last individuo
    property Last : TIndividual read getLast;

    // Retorna um individuo do conjunto
    property Item[i: integer] : TIndividual  read getItem
                                            write setItem;
                                                  default;
  end;

  // Representa um agrupamento de individuos
  TComplex = TIndividualList;

  // Representa um conjunto de complexos
  TComplexList = array of TComplex;

  // Representa todos os individuos do sistema
  TPopulation = {private} class(TIndividualList)
  private
    constructor Create(Count: Integer; Pars: TParameters);
    function Mean(): real;
    function DSP(aMean: real = -1): real;
    procedure SaveToFile(const Filename: string; Evolution: integer);
    procedure LoadFromFile(const Filename: string; out Evolution: integer);
  end;

  // Mecanismo de otimizacao pelo metodo genetico
  {TODO 1 -cPascalScript: Propriedades para controle do salvamento da pop.}
  TGeneticOptimizer = class(TOptimizer)
  private
    // Sub-Objetos
    FPop         : TPopulation;
    FComplexList : TComplexList;

    // Campos internos
    FEvolutionCount          : integer;
    FMaxGenerations          : integer;
    FComplexCount            : integer;
    FPopEvolutionCount       : integer;
    FIndividualsByComplex    : Integer;
    FPopFilename             : string;

    function getPopulationCount(): integer;
    function CalculateOFValue(Gens: TGens): real;
    procedure CalculateProbs(var Complex: TComplex);
    procedure GroupPopulation();
    procedure DoEvolution();

    procedure updateEvolutionCount();
    procedure updateComplexEvolutionCount(value: integer);
    procedure updateComplexCount(value: integer);
    procedure updateEstatistics();

    procedure Start(); override;
    procedure Finish(); override;
    procedure Execute(); override;

    function CreateParsManager(): TfoParsManager; override;
  public
    constructor Create();

    // Retorna a aptidao de um individuo
    function getAptness(individualIndex: integer): real;

    // Retorna o valor do Gen de um individuo
    function getGen(individualIndex, genIndex: integer): real;

    // Leitura e salvamento dos indivíduos da população
    procedure SavePopulation(const Filename: string);
    procedure LoadPopulation(const Filename: string);

    // Calcula a media da populacao
    // Nesta classe "aptnessIndex" é ignorado
    function getPopulationMean(const aptnessIndex: integer): real;

    // Calcula o desvio-padrao da populacao
    // Nesta classe "aptnessIndex" é ignorado
    function getPopulationDSP(const aptnessIndex: integer): real;

    // Retorna o numero de indivíduos da populacao
    property PopulationCount: integer read getPopulationCount;

    // Indica o numero de evolucoes que ja ocorreram (Gerações)
    property EvolutionCount: integer read FEvolutionCount;

    // Indica o numero de complexos do sistema
    property ComplexCount: integer read FComplexCount write FComplexCount;

    // Indica o numero de evolucoes que os complexos sofrerao
    property PopEvolutionCount: integer read FPopEvolutionCount
                                       write FPopEvolutionCount;

    // Nome do arquivo onde a população de uma determinada epoca devera
    // ser salva, a populacao somente sera salva se existir um nome de
    // arquivo valido.
    property PopFilename : string read FPopFilename write FPopFilename;
  end;

implementation

{ TIndividual }

constructor TIndividual.Create(Pars: TParameters);
var i: Integer;
begin
  inherited Create();

  if Pars <> nil then
     begin
     // Aloca os gens
     setLength(Gens, Pars.Count);

     // Gera um Valor relativo para cada gen (parametro)
     for i := 0 to High(Gens) do
       Gens[i] := Pars[i].Min + System.Random * (Pars[i].Max - Pars[i].Min);
     end;   
end;

procedure TIndividual.LoadFromXML(node: IXMLDomNode);
begin
  Aptness := toFloat(node.childNodes.item[0].Text);
  Prob := toFloat(node.childNodes.item[1].Text);
  Gens := XML_Utils.LoadFloatArray(node.childNodes.item[2]);
end;

procedure TIndividual.toXML(x: TXML_Writer);
begin
  x.beginTag('individual');
    x.beginIdent();
    x.Write('aptness', Aptness);
    x.Write('prob', Prob);
    x.Write('gens', Gens);
    x.endIdent();
  x.endTag('individual');
end;

{ TIndividualList }

constructor TIndividualList.Create(const OwnIndividuals: boolean);
begin
  inherited Create();
  FList := TObjectList.Create(OwnIndividuals);
end;

destructor TIndividualList.Destroy();
begin
  FList.Free();
  inherited;
end;

procedure TIndividualList.Add(Individual: TIndividual);
begin
  FList.Add(Individual);
end;

function TIndividualList.getItem(i: integer): TIndividual;
begin
  result := TIndividual(FList[i]);
end;

procedure TIndividualList.setItem(i: integer; const Value: TIndividual);
begin
  FList[i] := Value;
end;

function TIndividualList.getCount(): integer;
begin
  Result := FList.Count;
end;

function CompareFunc(Item1, Item2: Pointer): integer;
var x: real;
begin
  x := TIndividual(Item1).Aptness - TIndividual(Item2).Aptness;
  if x < 0.0 then result := -1 else
  if x > 0.0 then result := +1
  else
     result := 0;
end;

procedure TIndividualList.Sort();
begin
  FList.Sort(CompareFunc);
end;

procedure TIndividualList.Clear();
begin
  FList.Clear();
end;

function TIndividualList.getLast(): TIndividual;
begin
  result := TIndividual (FList.Last);
end;

procedure TIndividualList.Exchange(i1, i2: integer);
begin
  FList.Exchange(i1, i2);
end;

{ TPopulation }

constructor TPopulation.Create(Count: Integer; Pars: TParameters);
var i: Integer;
begin
  inherited Create(true);
  for i := 1 to Count do
    FList.Add(TIndividual.Create(Pars));
end;

function TPopulation.DSP(aMean: real = -1): real;
var i: Integer;
begin
  if aMean = -1 then
     aMean := self.Mean();

  result := 0;

  for i := 0 to getCount()-1 do
    result := result + ( SQR( getItem(i).Aptness - aMean ));

  result := Sqrt(result / (getCount() - 1));
end;

procedure TPopulation.LoadFromFile(const Filename: string; out Evolution: integer);

    procedure LoadIndividual(node: IXMLDomNode);
    var x: TIndividual;
    begin
      x := TIndividual.Create(nil);
      x.LoadFromXML(node);
      Add(x);
    end;

var doc: IXMLDOMDocument;
      i: integer;
      k: integer;
begin
  Evolution := 0;

  doc := OpenXMLDocument(Filename);
  if (doc.documentElement <> nil) and
     (doc.documentElement.tagName = 'population') then
     try
       SaveDecimalSeparator();
       StartWait();

       // out
       Evolution := toInt(doc.documentElement.attributes.item[0].text);

       k := doc.documentElement.childNodes.length;
       if k = getCount() then
          begin
          Clear();
          for i := 0 to k-1 do
            LoadIndividual(doc.documentElement.childNodes.item[i]);
          end
       else
          Dialogs.MessageDLG(Format(
            'O número de indivíduos da nova população (%d) é incompatível'#13 +
            'com o número de indivíduos da população atual (%d).'#13 +
            'A nova população não será lida.', [ k, getCount() ]),
             mtWarning, [mbOk], 0);

     finally
       StopWait();
       RestoreDecimalSeparator();
     end
  else
     Dialogs.MessageDLG(Format(
       'Arquivo <%s>'#13 +
       'não é um arquivo válido de indivíduos'#13 +
       'A nova população não será lida.', [Filename]),
        mtWarning, [mbOk], 0);
end;

procedure TPopulation.SaveToFile(const Filename: string; Evolution: integer);
var i: Integer;
    x: TXML_Writer;
    F: TStrings;
begin
  F := TStringList.Create();
  x := TXML_Writer.Create(F);
  try
    SaveDecimalSeparator();
    x.WriteHeader('TGeneticOptimizer.Population.SaveToFile', []);
    x.beginTag('population', ['Evolution'], [Evolution]);
      x.beginIdent();
      for i := 0 to getCount()-1 do getItem(i).toXML(x);
      x.endIdent();
    x.endTag('population');
    F.SaveToFile(Filename);
  finally
    RestoreDecimalSeparator();
    x.Free();
    F.Free();
  end;
end;

function TPopulation.Mean(): real;
var i: Integer;
begin
  result := 0;

  for i := 0 to getCount()-1 do
    result := result + getItem(i).Aptness;

  result := result / getCount();
end;

{ TGeneticOptimizer }

procedure TGeneticOptimizer.Start();
var i: Integer;
begin
  inherited Start();

  if FErro = '' then
     begin
     // Inicializa a semente do gerador de numeros aleatorios
     System.Randomize();

     // numero de individuos de cada Complex
     FIndividualsByComplex := 2 * FPars.Count + 1;

     // Número de evoluções de cada sub-complexo (gerações)
     FMaxGenerations := FIndividualsByComplex;

     // seta o numero de complexos
     setLength(FComplexList, FComplexCount);
     for i := 0 to FComplexCount-1 do
       FComplexList[i] := TComplex.Create(false);

     // Criacao dos pontos amostrais
     // A populacao ja podera estar criada pela leitura de individuos
     if FPop = nil then
        begin
        // Inicia o contador de Evolucoes da populacao
        FEvolutionCount := 0;

        // Cria a populacao
        FPop := TPopulation.Create(FComplexCount * FIndividualsByComplex, FPars);

        // Avaliacao inicial da populacao
        for i := 0 to FPop.Count-1 do
          begin
          FPop[i].Aptness := CalculateOFValue(FPop[i].Gens);
          if FStop then Exit;
          end;
        end;

     // Calcula a media e o desvio padrao inicial da populacao
     updateEstatistics();
     end;  
end;

procedure TGeneticOptimizer.DoEvolution();

  type
    THipercube = array of record
                            Min: real;
                            Max: real;
                          end;

  // Calcula o Min e o Max Value de cada gen (parametro) para os
  // indivíduos do Complex atual.
  procedure getHipercube(const Complex : TComplex;
                           var HC      : THipercube);
  var i, k: integer;
      Gen: real;
  begin
    // Inicializa cada ponto do Hipercube
    for i := 0 to FPars.Count-1 do
      begin
      HC[i].Max := Math.MinDouble;
      HC[i].Min := Math.MaxDouble;
      end;

    // Calcula o Min e o Max Value de cada gen
    for i := 0 to Complex.Count-1 do
      for k := 0 to High(Complex[i].Gens) do
        begin
        Gen := Complex[i].Gens[k];
        HC[k].Min := Math.Min(HC[k].Min, Gen);
        HC[k].Max := Math.Max(HC[k].Max, Gen);
        end;
  end; // proc CalcularHipercube

  // Selheciona randomicamente n Pais para serem geradores de um novo
  // individuo.
  procedure getParents(var Complex  : TComplex;
                       var Parents  : TIndividualList;
                       var Weak     : TIndividual);

  var iPai, iInd, iPior, iProb: Integer;
      Rand, Mult: real;
      iniProb, fimProb, difProb: real;
  begin
    iPior := -1;
    Parents.Clear();

    // Numero de Parents = Count + 1
    for iPai := 0 to FPars.Count do
      begin
      Rand := System.Random;

      for iInd := 0 to Complex.Count-1 do
        begin
        if iInd = 0 then
           iniProb := 0.0
        else
           iniProb := Complex[iInd-1].Prob;

        fimProb := Complex[iInd].Prob;

        if (Rand > iniProb) and (Rand <= fimProb) then
           begin
           // Seleciona o individuo que sera um dos Parents
           Parents.Add(Complex[iInd]);

           // Seleciona o pior
           if iInd > iPior then
              begin
              iPior := iInd;
              Weak := Complex[iInd];
              end;

           // Calcula a extensao do intervalo de probabilidades onde "Rand" foi
           // encontrado
           difProb := fimProb - iniProb;

           // Recalcula as probabilidades retirando o intervalo acima
           for iProb := iInd to Complex.Count-1 do
             Complex[iProb].Prob := Complex[iProb].Prob - difProb;

           Mult := 1 / Complex.Last.Prob;

           // Redimenciona os intervalos
           for iProb := 0 to Complex.Count-2 do
             Complex[iProb].Prob := Complex[iProb].Prob * Mult;

           // A Prob do Last individuo sempre sera 1
           Complex.Last.Prob := 1.0;

           // Vai escolher outro Pai pois este ja foi encontrado
           break;
           end
        end; // for iInd

      end; // for iPai
  end; // proc getParents

  // Calcula o Value medio dos genes dos Parents sem considerar o pior deles
  procedure CalculateCentroide(const Parents   : TIndividualList;
                               const Weak      : TIndividual;
                                 var Centroide : TFloatArray);
  var iPai, iGen: Integer;
  begin
    Fill(Centroide, 0.0);

    for iPai := 0 to Parents.Count-1 do
      if Parents[iPai] <> Weak then
         for iGen := 0 to FPars.Count-1 do
           Centroide[iGen] := Centroide[iGen] + Parents[iPai].Gens[iGen];

    Divide(Centroide, Parents.Count-1);
  end; // CalculateCentroide

  procedure Calculate_RC(const Centroide   : TFloatArray;
                        const Weak        : TIndividual;
                          var Reflex      : TGens;
                          var Contraction : TGens);
  var iGen: integer;
       dif: real;
  begin
    for iGen := 0 to FPars.Count-1 do
      begin
      dif := Centroide[iGen] - Weak.Gens[iGen];
      Reflex[iGen] := Centroide[iGen] + dif;
      Contraction[iGen] := Centroide[iGen] - dif / 2;
      end;
  end; // Calculate_RC

  procedure Mutation(const HC   : THipercube;
                       var Weak : TIndividual);
  var iGen: Integer;
  begin
    for iGen := 0 to FPars.Count-1 do
      Weak.Gens[iGen] := HC[iGen].Min + System.Random *
                                 (HC[iGen].Max - HC[iGen].Min);

    Weak.Aptness := CalculateOFValue(Weak.Gens);
  end; // Mutation

  function UpdateGens(const Gens : TGens;
                        var Weak : TIndividual): boolean;
  var FO: real;
  begin
    FO := CalculateOFValue(Gens);
    if FO <= Weak.Aptness then
       begin
       result := true;
       Weak.Aptness := FO;
       Weak.Gens := System.Copy(Gens);
       end
    else
       result := false;
  end; // UpdateGens

  // realiza Reflex ou Contraction ou Mutation
  procedure UpdateGensBy_RCM (const Hipercube   : THipercube;
                              const Reflex      : TGens;
                              const Contraction : TGens;
                                var Weak        : TIndividual);

      function ReflexIsValid() : boolean;
      var iGen: Integer;
      begin
        result := true;
        for iGen := 0 to FPars.Count-1 do
          if (Reflex[iGen] <= FPars[iGen].Min) or
             (Reflex[iGen] >= FPars[iGen].Max) then
             begin
             result := false;
             break;
             end;
      end; // ReflexIsValid()

  // RCM
  begin
    if ReflexIsValid() then
       begin
       if not UpdateGens(Reflex, Weak) then
          if not UpdateGens(Contraction, Weak) then
             Mutation(Hipercube, Weak);
       end
    else
       Mutation(Hipercube, Weak);
  end; // RCM

  procedure Rearrange(  var Complex : TComplex;
                       const Weak   : TIndividual );
  begin
    // Reordena por Aptidao
    Complex.Sort();

    // Recalcula as probabilidades
    CalculateProbs(Complex);
  end; // Rearrange

// iXXX: variáveis inteiras
// rXXX: variáveis reais
// vXXX: vetores (arrays)

var iComp      : integer;         // Contador dos Complexos
    iEvolution : integer;         // Contador das Evolucoes
    vComplex   : TComplex;        // Complex Atual
    vHC        : THipercube;      // Hipercube
    vParents   : TIndividualList; // Individuos do Complex selecionados como Parents
    vCentroide : TFloatArray;     // vetor com a média dos gens
    vR         : TGens;           // vetor de Reflex
    vC         : TGens;           // vetor de Contraction
    WI         : TIndividual;     // Pior Individuo do Complex Corrente

    procedure AlocateMem();
    begin
      setLength(vHC        , FPars.Count);
      setLength(vR         , FPars.Count);
      setLength(vC         , FPars.Count);
      setLength(vCentroide , FPars.Count);

      vParents := TIndividualList.Create(false);
    end;

    procedure DealocateMem();
    begin
      vHC := nil;
      vR := nil;
      vC := nil;
      vCentroide := nil;
      vParents.Free();
    end;

// DoEvolution
begin
  AlocateMem();
  for iComp := 0 to FComplexCount-1 do
    begin
    vComplex := FComplexList[iComp];
    getHipercube({const} vComplex, {var} vHC);
    for iEvolution := 1 to FMaxGenerations do
      begin
      getParents({var} vComplex, {var} vParents, {var} WI);
      CalculateCentroide({const} vParents, {const} WI, {var} vCentroide);
      Calculate_RC({const} vCentroide, {const} WI, {var} vR, {var} vC);
      UpdateGensBy_RCM({const} vHC, {const} vR, {const} vC, {var} WI);
      Rearrange({var} vComplex, {const} WI);
      updateComplexEvolutionCount(iEvolution);
      if FStop then Exit;
      end; // for iEvolution
    updateComplexCount(iComp);
    end; // for iComp
end;

// Calcula a prob. acumulada para cada individuo do Complex
procedure TGeneticOptimizer.CalculateProbs(var Complex: TComplex);
var i  : Integer;
    n  : integer;
    pa : real; // Prob acumulada
begin
  pa := 0;
  n  := Complex.Count;

  for i := 0 to n-1 do
    begin
    pa := pa + ( 2 * (n - i) ) /
               ( n * (n + 1) );

    Complex[i].Prob := pa;
    end;

  // Para evitar erros de arredondamento o Last Value recebera 1
  Complex.Last.Prob := 1.0;
end;

procedure TGeneticOptimizer.GroupPopulation();
var k, i : integer;
begin
  for k := 0 to FComplexCount-1 do
    begin
    // Esvazia o Complex
    FComplexList[k].Clear();

    // agrupa os individuos em complexos
    for i := 0 to FIndividualsByComplex-1 do
      FComplexList[k].Add( FPop[i * FComplexCount + k] );

    // Calcula e acumula as probabilidades
    CalculateProbs(FComplexList[k]);
    end;
end;

procedure TGeneticOptimizer.Execute();
begin
  repeat
    updateEvolutionCount();

    FPop.Sort();
    GroupPopulation();
    DoEvolution();

    updateEstatistics();

    // Salva a populacao se "FPopFilename" for um arquivo valido
    SavePopulation(FPopFilename);

  until ( FStop ) or
        ( FEvolutionCount = FPopEvolutionCount );
end;

procedure TGeneticOptimizer.Finish();
var i: Integer;
begin
  inherited Finish();
  for i := 0 to Length(FComplexList)-1 do FComplexList[i].Free();
  FComplexList := nil;
  FPop.Free();
  FPop := nil;
end;

function TGeneticOptimizer.CalculateOFValue(Gens: TGens): real;
var i: integer;
begin
  // Atualiza os valores dos parâmetros para poderem ser acessados
  // via Pascal Script
  for i := 0 to FPars.Count-1 do
    FPars[i].Value := Gens[i];

  // Calcula o valor da funcao objetivo
  FProject.o_CalculateObjetiveFunctions();
  result := self.FOFValues.AsDouble[0];

  // Atualiza os mostradores
  UpdateOFValue(0, result);
  UpdateSimulationCount();
end;

constructor TGeneticOptimizer.Create();
begin
  inherited Create();
  FComplexCount := 3;
  FPopEvolutionCount := 40;
end;

function TGeneticOptimizer.CreateParsManager(): TfoParsManager;
begin
  result := TfoParsManager_Genetic.Create(nil);
end;

procedure TGeneticOptimizer.updateEvolutionCount();
begin
  inc(FEvolutionCount);
  TfoParsManager_Genetic(FParsManager).EvolutionCount := FEvolutionCount;
end;

function TGeneticOptimizer.getAptness(individualIndex: integer): real;
begin
  result := FPop.Item[individualIndex].Aptness;
end;

function TGeneticOptimizer.getGen(individualIndex, genIndex: integer): real;
begin
  result := FPop.Item[individualIndex].Gens[genIndex];
end;

function TGeneticOptimizer.getPopulationCount(): integer;
begin
  result := FPop.Count;
end;

procedure TGeneticOptimizer.updateComplexCount(value: integer);
begin
  TfoParsManager_Genetic(FParsManager).ComplexCount := Value + 1;
  ProcessMessages(GO_Complex);
end;

procedure TGeneticOptimizer.updateComplexEvolutionCount(value: integer);
begin
  TfoParsManager_Genetic(FParsManager).ComplexEvolutionCount := Value;
  ProcessMessages(GO_Evolution);
end;

procedure TGeneticOptimizer.updateEstatistics();
var m: real;
begin
  if Length(F_OF_Viwers) <> FOFValuesCount then
     RedimOF();

  //if F_OF_Viwers[0].Visible then
     begin
     m := FPop.Mean();
     F_OF_Viwers[0].setMean(m);
     F_OF_Viwers[0].setDP(FPop.DSP(m));
     end;
end;

function TGeneticOptimizer.getPopulationDSP(const aptnessIndex: integer): real;
begin
  result := FPop.DSP();
end;

function TGeneticOptimizer.getPopulationMean(const aptnessIndex: integer): real;
begin
  result := FPop.Mean();
end;

procedure TGeneticOptimizer.LoadPopulation(const Filename: string);
begin
  if not FileExists(Filename) then Exit;

  if FPars.Count = 0 then
     raise Exception.Create('Os Parâmetros a serem otimizados devem ser'#13 +
                            'definidos antes da leitura de uma população');

  // numero de individuos de cada Complex
  FIndividualsByComplex := 2 * FPars.Count + 1;

  if FPop = nil then
     FPop := TPopulation.Create(FComplexCount * FIndividualsByComplex, FPars);

  // Quando a populacao for lida, sera feita a verificacao do numero de
  // individuos existentes contra o numero de individuos do arquivo, se forem
  // diferentes, uma mensagem de aviso sera dada e a otimizacao continuara
  // com a populacao original.
  FPop.LoadFromFile(Filename, {out} FEvolutionCount);

  // Atualizacao Visual
  TfoParsManager_Genetic(FParsManager).EvolutionCount := FEvolutionCount;
end;

procedure TGeneticOptimizer.SavePopulation(const Filename: string);
var s: string;
begin
  if (Filename <> '') then
     begin
     s := SysUtils.ExtractFilePath(Filename);
     if SysUtils.DirectoryExists(s) then
        FPop.SaveToFile(Filename, FEvolutionCount);
     end;
end;

end.
