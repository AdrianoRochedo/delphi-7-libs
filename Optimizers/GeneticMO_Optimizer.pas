unit GeneticMO_Optimizer;

  // Procurar por "<<<<" para itens nao terminados
  // Tolerancia para parada

interface
uses Math,
     Classes,
     Contnrs,
     SysUtils,
     SysUtilsEx,
     WinUtils,
     MSXML4,
     XML_Utils,
     Optimizer_Interfaces,
     Optimizer_Base,
     Optimizer_Form_ParsManager;

const
  MO_OFCalc = 1;       // Função Objetivo Calculada
  MO_Evolution = 2;    // Ocorreu uma Evolução

type
  TGens = TFloatArray;

  // Representa um individuo em uma populacao (um ponto amostral)
  TIndividual = {private} class
  private
    Gens: TGens;
    Aptness: TFloatArray;
    Pareto: integer;
    Prob: real;
    Pos: integer; // Posição na População
    constructor Create(Pars: TParameters; AptnessCount: integer; Pos: Integer);
    procedure toXML(x: TXML_Writer);
    procedure LoadFromXML(node: IXMLDomNode);
  end;

  // Representa um conjunto de individuos
  TPopulation = {private} class
  private
    FList: TObjectList;
    destructor Destroy(); override;
    function getItem(i: integer): TIndividual;
    function getCount: integer;
    procedure setItem(i: integer; const Value: TIndividual);
    function getLast: TIndividual;
    procedure SaveToFile(const Filename: string; Evolution, RMax: integer);
    procedure LoadFromFile(const Filename: string; out Evolution, RMax: integer);
  public
    // Cria uma instancia de individuos
    constructor Create(const Count, AptnessCount: Integer; const Pars: TParameters);

    // Calcula a media da populacao
    function Mean(const aptnessIndex: integer): real;

    // Calcula o desvio-padrao da populacao
    function DSP(const aptnessIndex: integer; aMean: real = -1): real;

    // Esvazia a lista
    procedure Clear();

    // Adiciona um individuo ao conjunto
    procedure Add(Individual: TIndividual);

    // Retorna o numero de individuos da populacao
    property Count: integer read getCount;

    // Retorna o Last individuo
    property Last : TIndividual read getLast;

    // Retorna um individuo do conjunto
    property Item[i: integer] : TIndividual  read getItem
                                            write setItem;
                                                  default;
  end;

  TIndividualList = {private} class(TPopulation)
  private
    constructor Create();
    procedure Clear();
  end;

  TGroupsOfIndividuals = array of TIndividualList;

  // Mecanismo de otimizacao pelo metodo genetico
  TGeneticMO_Optimizer = class(TOptimizer)
  private
    // Sub-Objetos
    FPop: TPopulation;

    // Campos internos
    FEvolutionCount    : integer;
    FPopEvolutionCount : integer;
    FPopulationCount   : integer;

    FRMax   : integer;         // rank máximo alcançado em cada evolução, atualizado em "SortByParetoRanking"
    FWorsts : TIndividualList; // Piores indivíduos em cada evolução
    
    FPopFilename: string;      

    procedure CalculateOFValues(Individual: TIndividual);
    procedure DoEvolution();

    // Classifica a população utilizando o método de Ranking de Pareto.
    // Retorna "true" se existe pelo menos um ponto dominado na amostra.
    function SortByParetoRanking(): boolean;

    procedure updateEvolutionCount();
    procedure updateEstatistics();

    procedure Start(); override;
    procedure Finish(); override;
    procedure Execute(); override;

    function CreateParsManager(): TfoParsManager; override;
    function getWorstsCount: integer;
  public
    constructor Create();

    // Leitura e salvamento dos indivíduos da população
    procedure SavePopulation(const Filename: string);
    procedure LoadPopulation(const Filename: string);

    // Retorna a aptidao de um individuo
    function getAptness(const individualIndex, aptnessIndex: integer): real;

    // Retorna o valor do Gen de um individuo
    function getGen(const individualIndex, genIndex: integer): real;

    // Retorna a media das aptidoes da populacao
    function getPopulationMean(const aptnessIndex: integer): real;

    // Retorna o Desvio Padrao das aptidoes da populacao
    function getPopulationDSP(const aptnessIndex: integer): real;

    // Indica o numero de evolucoes que ja ocorreram
    property EvolutionCount: integer read FEvolutionCount;

    // Numero de indivíduos da populacao
    property PopulationCount: integer read FPopulationCount write FPopulationCount;

    // Indica o numero máximo de evolucoes que a populacao podera sofrer
    property PopEvolutionCount: integer read FPopEvolutionCount
                                       write FPopEvolutionCount;

    property RMax : integer read FRMax;

    property WorstsCount: integer read getWorstsCount;

    // Nome do arquivo onde a população de uma determinada epoca devera
    // ser salva, a populacao somente sera salva se existir um nome de
    // arquivo valido.
    property PopFilename : string read FPopFilename write FPopFilename;
  end;

implementation
uses Dialogs,
     Optimizer_Form_ParsManager_Genetic;

{ TIndividual }

constructor TIndividual.Create(Pars: TParameters; AptnessCount: integer; Pos: Integer);
var i: Integer;
begin
  inherited Create();

  self.Pos := Pos;
  setLength(Aptness, AptnessCount);

  if Pars <> nil then
     begin
     setLength(Gens, Pars.Count);

     // Gera o Value relativo para cada gen (parametro)
     for i := 0 to High(Gens) do
       Gens[i] := Pars[i].Min + System.Random * (Pars[i].Max - Pars[i].Min);
     end;  
end;

procedure TIndividual.LoadFromXML(node: IXMLDomNode);
begin
  Aptness := XML_Utils.LoadFloatArray(node.childNodes.item[0]);
  Prob := toFloat(node.childNodes.item[1].Text);
  Gens := XML_Utils.LoadFloatArray(node.childNodes.item[2]);
  Pareto := toInt(node.childNodes.item[3].Text);
  Pos  := toInt(node.childNodes.item[4].Text);
end;

procedure TIndividual.toXML(x: TXML_Writer);
begin
  x.beginTag('individual');
    x.beginIdent();
    x.Write('aptness', Aptness);
    x.Write('prob', Prob);
    x.Write('gens', Gens);
    x.Write('pareto', Pareto);
    x.Write('pos', Pos);
    x.endIdent();
  x.endTag('individual');
end;

{ TIndividualList }

procedure TIndividualList.Clear();
begin
  FList.Clear();
end;

constructor TIndividualList.Create();
begin
  // Não chamar o ancedente
  FList := TObjectList.Create(false);
end;

{ TPopulation }

constructor TPopulation.Create(const Count, AptnessCount: Integer; const Pars: TParameters);
var i: Integer;
begin
  inherited Create();
  FList := TObjectList.Create(true);
  for i := 1 to Count do
    FList.Add(TIndividual.Create(Pars, AptnessCount, i-1));
end;

destructor TPopulation.Destroy();
begin
  FList.Free();
  inherited;
end;

procedure TPopulation.Add(Individual: TIndividual);
begin
  FList.Add(Individual);
end;

function TPopulation.getItem(i: integer): TIndividual;
begin
  result := TIndividual(FList[i]);
end;

procedure TPopulation.setItem(i: integer; const Value: TIndividual);
begin
  FList[i] := Value;
end;

function TPopulation.getCount(): integer;
begin
  Result := FList.Count;
end;

function TPopulation.getLast(): TIndividual;
begin
  result := TIndividual (FList.Last);
end;

function TPopulation.DSP(const aptnessIndex: integer; aMean: real = -1): real;
var i: Integer;
begin
  if aMean = -1 then
     aMean := self.Mean(aptnessIndex);

  result := 0;

  for i := 0 to getCount()-1 do
    result := result + ( SQR( getItem(i).Aptness[aptnessIndex] - aMean ));

  result := Sqrt(result / (getCount() - 1));
end;

function TPopulation.Mean(const aptnessIndex: integer): real;
var i: Integer;
begin
  result := 0;

  for i := 0 to getCount()-1 do
    result := result + getItem(i).Aptness[aptnessIndex];

  result := result / getCount();
end;

procedure TPopulation.LoadFromFile(const Filename: string; out Evolution, RMax: integer);

    procedure LoadIndividual(node: IXMLDomNode);
    var x: TIndividual;
    begin
      x := TIndividual.Create(nil, 0, 0);
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
       RMax := toInt(doc.documentElement.attributes.item[1].text);

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

procedure TPopulation.SaveToFile(const Filename: string; Evolution, RMax: integer);
var i: Integer;
    x: TXML_Writer;
    F: TStrings;
begin
  F := TStringList.Create();
  x := TXML_Writer.Create(F);
  try
    SaveDecimalSeparator();
    x.WriteHeader('TGeneticMOOptimizer.Population.SaveToFile', []);
    x.beginTag('population', ['Evolution', 'RMax'], [Evolution, RMax]);
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

procedure TPopulation.Clear();
begin
  FList.Clear();
end;

{ TGeneticMO_Optimizer }

procedure TGeneticMO_Optimizer.Start();
var i: Integer;
begin
  inherited Start();

  if FErro = '' then
     begin
     // Inicializa a semente do gerador de numeros aleatorios
     System.Randomize();

     // Lista para os piores indivíduos da população
     FWorsts := TIndividualList.Create();

     if FPop = nil then
        begin
        // Inicia o contador de Evolucoes
        FEvolutionCount := 0;

        // Criacao dos pontos amostrais
        FPop := TPopulation.Create(FPopulationCount, self.ObjectivesCount, FPars);

        // Avaliacao inicial da populacao
        for i := 0 to FPop.Count-1 do
          begin
          CalculateOFValues(FPop[i]);
          if FStop then Exit;
          end;
        end;

     // Calcula a media e o desvio padrao inicial da populacao
     updateEstatistics();
     end;
end;

procedure TGeneticMO_Optimizer.DoEvolution();

  type
    THipercube = array of record
                            Min: real;
                            Max: real;
                          end;

   // Marca os Piores indivíduos
   procedure defineWorsts();
   var i: Integer;
   begin
     FWorsts.Clear();
     for i := 0 to FPopulationCount-1 do
       if FPop[i].Pareto = FRMax then
          FWorsts.Add(FPop[i]);
   end;

   // Calcula a probabilidade para cada indivíduo
   procedure calcProbs();
   var x  : real; // Somatório dos Paretos
       pa : real; // Prob acumulada
       i  : integer;
   begin
     x := 0.0;
     for i := 0 to FPopulationCount-1 do
       x := x + FPop[i].Pareto;

     pa := 0.0;
     for i := 0 to FPopulationCount-1 do
       begin
       pa := pa + ( FRMax - FPop[i].Pareto + 1) /
                  ( FPopulationCount * (FRMax + 1) - x );

       FPop[i].Prob := pa;
       end;

     // Para evitar erros de arredondamento o último valor receberá 1
     FPop.Last.Prob := 1.0;
   end;

   // Define os Pais, onde Pais = Lista de Indivíduos e num. Pais = num. Piores
   // Cada lista de pais conterá (nGenes+1) indivíduos sendo que na
   // primeira posição de cada lista estarão os piores e os outros serão
   // escolhidos na população.
   procedure defineGroupsOfParents(var GroupsOfParents: TGroupsOfIndividuals);
   var i: Integer;
       j: integer;
       m: integer;
       x: real;
       p: real;
   begin
     // Dimensiona a lista de pais e já seleciona o primeiro pai de cada uma.
     setLength(GroupsOfParents, FWorsts.Count);
     for i := 0 to FWorsts.Count-1 do
       begin
       GroupsOfParents[i] := TIndividualList.Create();
       GroupsOfParents[i].Add(FWorsts[i]); // Primeiro Pai

       // Define os outros pais
       // Neste algorítmo, os indivíduos de um mesmo grupo podem ser repetidos.
       // <<< Para nao permitir o repeteco de ind. em um mesmo grupo, trocar
       //     o laço j por um "while" e só definir indivíduos diferentes.
       for j := 1 to Parameters.Count do
         begin
         x := System.Random;
         p := 0.0;
         for m := 0 to FPopulationCount-1 do
           begin
           if (x >= p) and (x <= FPop[m].Prob) then // ??? <= Prop
              begin
              // Seleciona um Pai para o grupo i, que pode ser um dos anteriores !
              GroupsOfParents[i].Add(FPop[m]);
              break; // Var escolher outro Pai
              end;
           p := FPop[m].Prob;
           end; // for m
         end; // for j
       end; // for i
   end;

   procedure clearGroupsOfParents(var GroupsOfParents: TGroupsOfIndividuals);
   var i: Integer;
   begin
     for i := 0 to FWorsts.Count-1 do
       GroupsOfParents[i].Free();
   end;

   // Evolui cada um dos grupos
   procedure Evolve(var GroupsOfParents: TGroupsOfIndividuals;
                    var Centroide: TFloatArray;
                    var Reflection: TFloatArray;
                    var Contraction: TFloatArray);

   var i, j, k, m: integer;
       Dominion: integer;
       Rejected: boolean;
       x: real;
       p: TIndividualList; // grupo dos Pais
   begin
     for k := 0 to FWorsts.Count-1 do
       begin
       // Grupo de pais atual
       p := GroupsOfParents[k];

       // Zera o vetor
       Fill(Centroide, 0.0);

       // Calcula o somatório
       for j := 1 to Parameters.Count do
         for i := 0 to Parameters.Count-1 do
           Centroide[i] := Centroide[i] + p.Item[j].Gens[i];

       // Calcula a média
       Divide(Centroide, Parameters.Count);

       // Calcula os vetores de reflexão e contração
       for i := 0 to Parameters.Count-1 do
         begin
         x := Centroide[i] - p.Item[0].Gens[i];
         Reflection[i] := Centroide[i] + x;
         Contraction[i] := Centroide[i] - x / 2;
         end;

       Rejected := false;

       // Verifica se o ponto gerado por Reflexão está dentro da região válida
       for i := 0 to Parameters.Count-1 do
         if (Reflection[i] < Parameters[i].Min) or
            (Reflection[i] > Parameters[i].Max) then
            begin
            Rejected := true;
            break;
            end;

       if not Rejected then
          begin
          // Substitui o pior indivíduo utilizando Reflaxão
          FWorsts[k].Gens := System.Copy(Reflection);
          CalculateOFValues(FWorsts[k]);

          // Verifica se o ponto de reflexão é dominado e somente o aceita
          // se for não dominado em relação aos outros ind. do grupo.
          for j := 1 to Parameters.Count do
            begin
            Dominion := 0;

            for m := 0 to self.ObjectivesCount - 1 do
              if p.Item[j].Aptness[m] < FWorsts[k].Aptness[m] then
                 inc(Dominion);

            if Dominion = self.ObjectivesCount then
               begin
               Rejected := true;
               break;
               end;
            end; // for j (para os outros ind. do grupo)
          end; // if not Rejected

       // Substitui o pior indivíduo utilizando Contração se ele foi
       // rejeitado por Reflexão ou por dominância
       if Rejected then
          begin
          FWorsts[k].Gens := System.Copy(Contraction);
          CalculateOFValues(FWorsts[k]);
          end;
       end; // for k (para cada grupo)
   end;

   procedure AlocateMem(var Centroide: TFloatArray;
                        var Reflection: TFloatArray;
                        var Contraction: TFloatArray);
   begin
     setLength(Centroide, Parameters.Count);
     setLength(Reflection, Parameters.Count);
     setLength(Contraction, Parameters.Count);
   end;

   procedure DealocateMem(var Centroide: TFloatArray;
                          var Reflection: TFloatArray;
                          var Contraction: TFloatArray);
   begin
     Centroide := nil;
     Reflection := nil;
     Contraction := nil;
   end;

var
  vGroupsOfParents : TGroupsOfIndividuals;
  vCentroide       : TFloatArray;
  vReflection      : TFloatArray;
  vContraction     : TFloatArray;
begin
  // Prepara os vetores
  alocateMem(vCentroide, vReflection, vContraction);
  // Marca os Piores indivíduos
  defineWorsts();
  // Calcula a probabilidade para cada indivíduo
  calcProbs();
  // Define os grupos de pais (simplexes)
  defineGroupsOfParents(vGroupsOfParents);
  // Evolui cada um dos grupos
  Evolve(vGroupsOfParents, vCentroide, vReflection, vContraction);
  // Desaloca a memória utilizada no processo
  dealocateMem(vCentroide, vReflection, vContraction);
  // Atualiza os contadores
  updateEvolutionCount();
end;

function TGeneticMO_Optimizer.SortByParetoRanking(): boolean;

  // "j" domina "i" somente se todas as suas FOs forem melhor que as de "i"
  // Complexidade: O³(cubo)
  function IsDominated(i, j: integer): boolean;
  var k: integer;
      ii, ij: TIndividual;
  begin
    ij := FPop[j];
    ii := FPop[i];
    Result := true;
    //for k := 0 to Parameters.Count-1 do
    for k := 0 to self.ObjectivesCount - 1 do
      if ii.Aptness[k] <= ij.Aptness[k] then // Existe uma FO de "j" pior ou igual a de "i"
         begin                               // Portanto "i" é não dominado por "j"
         Result := false;
         break;
         end;
  end;

var i,j    : integer;
    IRD    : boolean;
begin
  FRMax := 1;
  for i := 0 to FPopulationCount-1 do FPop[i].Pareto := 1;

  // Forma os grupos (indivíduos de paretos iguais)
  repeat
    IRD := False;

    for i := 0 to FPopulationCount-1 do
      if FPop[i].Pareto = FRMax then
         for j := 0 to FPopulationCount-1 do
           if (j <> i) and (FPop[j].Pareto = FRMax) then
              if IsDominated(i, j) then
                 begin
                 inc(FPop[i].Pareto);
                 IRD := true;
                 break;
                 end;

    if IRD then inc(FRMax);
    until IRD = False; // Só sairá se não existir mais dominância.

  result := FRMax > 1;
end;

procedure TGeneticMO_Optimizer.Execute();
begin
  repeat
    if SortByParetoRanking() then
       // Se existe pelo menos um ponto dominado na amostra
       begin
       DoEvolution();
       updateEstatistics();

       // Salva a populacao se "FPopFilename" for um arquivo valido
       SavePopulation(FPopFilename);
       end
    else
       // Todos os pontos na amostra se tornam mutualmente não-dominados
       FStop := true;

  until ( FStop ) or
        ( FEvolutionCount = FPopEvolutionCount );
end;

procedure TGeneticMO_Optimizer.Finish();
begin
  inherited Finish();
  FPop.Free();
end;

procedure TGeneticMO_Optimizer.CalculateOFValues(Individual: TIndividual);
var i: integer;
begin
  // Atualiza os valores dos parâmetros para poderem ser acessados
  // via Pascal Script

  FPars.IndividualPos := Individual.Pos;
  for i := 0 to FPars.Count-1 do
    FPars[i].Value := Individual.Gens[i];

  FProject.o_CalculateObjetiveFunctions();

  // Obtem os valores das funcoes objetivos calculados via Script
  for i := 0 to FOFValuesCount-1 do
    begin
    Individual.Aptness[i] := FOFValues.AsDouble[i];
    UpdateOFValue(i, Individual.Aptness[i]);
    end;

  UpdateSimulationCount();
  ProcessMessages(MO_OFCalc);
end;

constructor TGeneticMO_Optimizer.Create();
begin
  inherited Create();
  FPopEvolutionCount := 40;
  FPopulationCount := 20;
end;

function TGeneticMO_Optimizer.CreateParsManager(): TfoParsManager;
begin
  result := TfoParsManager_Genetic.Create(nil);
end;

procedure TGeneticMO_Optimizer.updateEvolutionCount();
begin
  inc(FEvolutionCount);
  TfoParsManager_Genetic(FParsManager).EvolutionCount := FEvolutionCount;
  ProcessMessages(MO_Evolution);
end;

function TGeneticMO_Optimizer.getAptness(const individualIndex, aptnessIndex: integer): real;
begin
  result := FPop.Item[individualIndex].Aptness[aptnessIndex];
end;

function TGeneticMO_Optimizer.getGen(const individualIndex, genIndex: integer): real;
begin
  result := FPop.Item[individualIndex].Gens[genIndex];
end;

procedure TGeneticMO_Optimizer.updateEstatistics();
var m: real;
begin
  //m := FPop.Mean();
  //F_OF_Viwer.setMean(m);
  //F_OF_Viwer.setDP(FPop.DP(m));
end;

function TGeneticMO_Optimizer.getPopulationDSP(const aptnessIndex: integer): real;
begin
  result := FPop.DSP(aptnessIndex);
end;

function TGeneticMO_Optimizer.getPopulationMean(const aptnessIndex: integer): real;
begin
  result := FPop.Mean(aptnessIndex);
end;

procedure TGeneticMO_Optimizer.LoadPopulation(const Filename: string);
begin
  if not FileExists(Filename) then Exit;

  if FPars.Count = 0 then
     raise Exception.Create('Os Parâmetros a serem otimizados devem ser'#13 +
                            'definidos antes da leitura de uma população');

  if FPop = nil then
     FPop := TPopulation.Create(FPopulationCount, self.ObjectivesCount, FPars);

  // Quando a populacao for lida, sera feita a verificacao do numero de
  // individuos existentes contra o numero de individuos do arquivo, se forem
  // diferentes, uma mensagem de aviso sera dada e a otimizacao continuara
  // com a populacao original.
  FPop.LoadFromFile(Filename, {out} FEvolutionCount, {out} FRMax);

  // Atualizacao Visual
  TfoParsManager_Genetic(FParsManager).EvolutionCount := FEvolutionCount;
end;

procedure TGeneticMO_Optimizer.SavePopulation(const Filename: string);
var s: string;
begin
  if (Filename <> '') then
     begin
     s := SysUtils.ExtractFilePath(Filename);
     if SysUtils.DirectoryExists(s) then
        FPop.SaveToFile(Filename, FEvolutionCount, FRMax);
     end;
end;

function TGeneticMO_Optimizer.getWorstsCount: integer;
begin
  result := self.FWorsts.Count;
end;

end.
