unit Optimizer_psLib;

interface
uses psBASE;

  procedure API(Lib: TLib);

implementation
uses sysutils,
     Dialogs,
     Optimizer_Base,
     Rosenbrock_Optimizer,
     Genetic_Optimizer,
     GeneticMO_Optimizer;

const
  cCat_Optimizer = 'Otimizadores';

type
  TpsOpt_Parameter = Class(TpsClass)
  public
    procedure AddMethods; override;

    class procedure amShow          (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetName       (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetName       (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetLink       (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetValue      (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetValue      (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetMin        (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetMin        (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetMax        (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetMax        (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetStep       (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetStep       (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetTolerance  (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetTolerance  (Const Func_Name: String; Stack: TexeStack);
  end;

  TpsOpt_Parameters = Class(TpsClass)
  public
    procedure AddMethods; override;

    class procedure amCount           (Const Func_Name: String; Stack: TexeStack);
    class procedure amParameter       (Const Func_Name: String; Stack: TexeStack);
    class procedure amParamByName     (Const Func_Name: String; Stack: TexeStack);
    class procedure amClear           (Const Func_Name: String; Stack: TexeStack);
    class procedure amCreateParameter (Const Func_Name: String; Stack: TexeStack);
  end;

  TpsOptimizer = Class(TpsClass)
  public
    procedure AddMethods; override;

    // Optimizer
    class procedure amStop                 (Const Func_Name: String; Stack: TexeStack);
    class procedure amShowOFViwer          (Const Func_Name: String; Stack: TexeStack);
    class procedure amShowParsManager      (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetTolerante         (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetTolerante         (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetParameters        (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetClass             (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetOFValue           (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetObjetivesCount    (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetObjetivesCount    (Const Func_Name: String; Stack: TexeStack);

    // Rosenbroke Optimizer
    class procedure amGetIncStep           (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetIncStep           (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetDecStep           (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetDecStep           (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetInitialStep       (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetInitialStep       (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetMaxSimulations    (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetMaxSimulations    (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetMaxTimeSimulation (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetMaxTimeSimulation (Const Func_Name: String; Stack: TexeStack);

    // Genetic Optimizer Mono e Mult-Objective
    class procedure amLoadPopulation       (Const Func_Name: String; Stack: TexeStack);
    class procedure amSavePopulation       (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetPopFilename       (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetPopFilename       (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetComplexCount      (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetPopEvolutionCount (Const Func_Name: String; Stack: TexeStack);
    class procedure amSetPopulationCount   (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetPopulationCount   (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetPopulationDSP     (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetPopulationMean    (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetEvolutionCount    (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetAptness           (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetGen               (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetRMax              (Const Func_Name: String; Stack: TexeStack);
    class procedure amGetWorstsCount       (Const Func_Name: String; Stack: TexeStack);
  end;

{ ------------------- Ponto de Entrada ------------------ }

procedure API(Lib: TLib);
begin
  TpsOpt_Parameter.Create(
      TParameter,
      nil,
      'Representa um parâmetro',
      cCat_Optimizer,
      [], [], [],
      False,
      Lib.Classes);

  TpsOpt_Parameters.Create(
     TParameters,
     nil,
     'Gerencia os parâmetros da Otimização',
     cCat_Optimizer,
     [], [], [],
     False,
     Lib.Classes);

  TpsOptimizer.Create(
     TOptimizer,
     nil,
     'Classe básica para Mecanismos de Otimização',
     cCat_Optimizer,
     [], [], [],
     False,
     Lib.Classes);
end;

{ ------------------- Ponto de Entrada ------------------ }

{ TpsOpt_Parameter }

procedure TpsOpt_Parameter.AddMethods;
begin
  with Procs do
    begin
    Add('Show',
        'Mostra as informações do parâmetro na posição x, y',
        '',
        [pvtInteger, pvtInteger],
        [nil       , nil       ],
        [false     , false     ],
        pvtNull,
        TObject,
        amShow);

    Add('SetLink',
        'Liga um objeto existente a este parâmetro.'#13 +
        'O objeto tem que implementar a interface IOptimizer.'#13 +
        'Estes objetos são: PCs, Reservatórios, Sub-Bacias, Trechos-Dáguas e Demandas'#13 +
        'Parâmetros:'#13 +
        '  - 1. Objeto que implementa a interface IOptimizer'#13 +
        '  - 2. Nome da propriedade do objeto a ser otimizada'#13 +
        '  - 3. Ano, caso a propriedade seja uma tabela indexada por ano'#13 +
        '  - 4. Mes, caso a propriedade seja uma tabela indexada por ano e mes'#13 +
        ''#13 +
        'OBS.: Para as propriedades simples (não tabeladas), os parâmetros Ano e Mes são ignorados.'#13,
        '',
        [pvtObject, pvtString, pvtInteger, pvtInteger],
        [TObject, nil, nil, nil],
        [True, false, false, false],
        pvtNull,
        TObject,
        amSetLink);

    Add('SetName',
        '',
        '',
        [pvtString],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetName);

    Add('SetValue',
        '',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetValue);

    Add('SetMin',
        '',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetMin);

    Add('SetMax',
        '',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetMax);

    Add('SetStep',
        '',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetStep);

    Add('SetTolerance',
        '',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetTolerance);
    end;

  with Functions do
    begin
    Add('GetName',
        '',
        '',
        [],
        [],
        [],
        pvtString,
        TObject,
        amGetName);

    Add('GetValue',
        '',
        '',
        [],
        [],
        [],
        pvtReal,
        TObject,
        amGetValue);

    Add('GetMin',
        '',
        '',
        [],
        [],
        [],
        pvtReal,
        TObject,
        amGetMin);

    Add('GetMax',
        '',
        '',
        [],
        [],
        [],
        pvtReal,
        TObject,
        amGetMax);

    Add('GetStep',
        '',
        '',
        [],
        [],
        [],
        pvtReal,
        TObject,
        amGetStep);

    Add('GetTolerance',
        '',
        '',
        [],
        [],
        [],
        pvtReal,
        TObject,
        amGetTolerance);
    end;
end;

class procedure TpsOpt_Parameter.amGetMax(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(
    TParameter(Stack.AsObject(1)).Max
    );
end;

class procedure TpsOpt_Parameter.amGetMin(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(
    TParameter(Stack.AsObject(1)).Min
    );
end;

class procedure TpsOpt_Parameter.amGetName(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(
    TParameter(Stack.AsObject(1)).Name
    );
end;

class procedure TpsOpt_Parameter.amSetLink(const Func_Name: String; Stack: TexeStack);
begin
  TParameter(Stack.AsObject(5)).SetLink(
    Stack.AsObject(1),   // objeto que implementa a interface IOptimizer
    Stack.AsString(2),   // Nome da propriedade
    Stack.AsInteger(3),  // Ano
    Stack.AsInteger(4)   // Mes
    );
end;

class procedure TpsOpt_Parameter.amGetStep(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(
    TParameter(Stack.AsObject(1)).Step
    );
end;

class procedure TpsOpt_Parameter.amGetTolerance(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(
    TParameter(Stack.AsObject(1)).Tolerance
    );
end;

class procedure TpsOpt_Parameter.amGetValue(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(
    TParameter(Stack.AsObject(1)).Value
    );
end;

class procedure TpsOpt_Parameter.amSetMax(const Func_Name: String; Stack: TexeStack);
begin
  TParameter(Stack.AsObject(2)).Max := Stack.AsFloat(1);
end;

class procedure TpsOpt_Parameter.amSetMin(const Func_Name: String; Stack: TexeStack);
begin
  TParameter(Stack.AsObject(2)).Min := Stack.AsFloat(1);
end;

class procedure TpsOpt_Parameter.amSetName(const Func_Name: String; Stack: TexeStack);
begin
  TParameter(Stack.AsObject(2)).Name := Stack.AsString(1);
end;

class procedure TpsOpt_Parameter.amSetStep(const Func_Name: String; Stack: TexeStack);
begin
  TParameter(Stack.AsObject(2)).Step := Stack.AsFloat(1);
end;

class procedure TpsOpt_Parameter.amSetTolerance(const Func_Name: String; Stack: TexeStack);
begin
  TParameter(Stack.AsObject(2)).Tolerance := Stack.AsFloat(1);
end;

class procedure TpsOpt_Parameter.amSetValue(const Func_Name: String; Stack: TexeStack);
begin
  TParameter(Stack.AsObject(2)).Value := Stack.AsFloat(1);
end;

class procedure TpsOpt_Parameter.amShow(const Func_Name: String; Stack: TexeStack);
begin
  TParameter(Stack.AsObject(3)).Show(Stack.AsInteger(1), Stack.AsInteger(2));
end;

{ TpsOpt_Parameters }

procedure TpsOpt_Parameters.AddMethods;
begin
  with Procs do
    begin
    Add('Clear',
        'Remove da lista todos os parâmetros',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amClear);
    end;

  with Functions do
    begin
    Add('Count',
        'Retorna a quantidade total de parâmetros',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amCount);

    Add('Parameter',
        'Retorna o i-égimo parâmetro',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtObject,
        TParameter,
        amParameter);

    Add('ParamByName',
        'Retorna o parâmetro que possui o nome passado',
        '',
        [pvtString],
        [nil],
        [False],
        pvtObject,
        TParameter,
        amParamByName);

    Add('CreateParameter',
        'Informa a definição de um parâmetro para o Rosenbrock.'#13 +
        'Devolve o parâmetro criado.',
        '',
        [],
        [],
        [],
        pvtObject,
        TParameter,
        amCreateParameter);
    end;
end;

class procedure TpsOpt_Parameters.amClear(const Func_Name: String; Stack: TexeStack);
begin
  TParameters(Stack.AsObject(1)).Clear;
end;

class procedure TpsOpt_Parameters.amCount(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(
    TParameters(Stack.AsObject(1)).Count
    );
end;

class procedure TpsOpt_Parameters.amCreateParameter(const Func_Name: String; Stack: TexeStack);
var Pars: TParameters;
begin
  Pars := TParameters(Stack.AsObject(1));
  Stack.PushObject(Pars.CreateParameter);
end;

class procedure TpsOpt_Parameters.amParamByName(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    TParameters(Stack.AsObject(2)).ParamByName(
      Stack.AsString(1)
      )
    );
end;

class procedure TpsOpt_Parameters.amParameter(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    TParameters(Stack.AsObject(2)).Item[
      Stack.AsInteger(1)]
    );
end;

{ TpsOptimizer }

procedure TpsOptimizer.AddMethods;
begin
  with Procs do
    begin
    Add('Stop',
        'Termina a otimização',
        '',
        [],
        [],
        [],
        pvtNull,
        TObject,
        amStop);

    Add('ShowOFViwer',
        'Mostra a dinâmica da função objetivo em um gráfico'#13 +
        'Parâmetros: Índice da função, Coord. X, Coord. Y',
        '',
        [pvtInteger, pvtInteger, pvtInteger],
        [nil, nil, nil],
        [False, False, False],
        pvtNull,
        TObject,
        amShowOFViwer);

    Add('ShowParsManager',
        'Mostra o gerenciador de parâmetros',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [False, False],
        pvtNull,
        TObject,
        amShowParsManager);        

    Add('SetObjetivesCount',
        'Estabelece o número de funções objetivos a serem otimizadas.'#13 +
        'Nos otimizadores "TRosenbroke" e "TGeneticOptimizer" a chamada deste método não tem função.',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetObjetivesCount);

    Add('SetOFValue',
        'Estabelece o valor de uma função Objetivo.'#13 +
        'O índice da função inicia em 0.'#13 +
        'Parâmetros: Índice da FO (integer) e Valor (real)',
        '',
        [pvtInteger, pvtReal],
        [nil, nil],
        [False, false],
        pvtNull,
        TObject,
        amSetOFValue);

    Add('SetTolerante',
        '(TRosenbroke) Estabelece a tolerância para a variação da Função Objetivo',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetTolerante);

    Add('SetIncStep',
        '(TRosenbroke) Estabelece o Tamanho do Passo de Incremento',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetIncStep);

    Add('SetDecStep',
        '(TRosenbroke) Estabelece o Tamanho do Passo de Decremento',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetDecStep);

    Add('SetInitialStep',
        '(TRosenbroke) Estabelece o Tamanho do Passo Inicial',
        '',
        [pvtBoolean],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetInitialStep);

    Add('SetMaxSimulations',
        '(TRosenbroke) Estabelece o número máximo de simulaçãoes',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetMaxSimulations);

    Add('SetMaxTimeSimulation',
        '(TRosenbroke) Estabelece o tempo máximo de simulação em Mili-Segundos',
        '',
        [pvtReal],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetMaxTimeSimulation);

    Add('SetComplexCount',
        '(TGeneticOptimizer) Estabelece o número de complexos',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetComplexCount);

    Add('SetPopEvolutionCount',
        '(TGeneticOptimizer, TGeneticMO_Optimizer) Estabelece o número de evoluções da População',
        '',
        [pvtInteger],
        [nil],
        [False],
        pvtNull,
        TObject,
        amSetPopEvolutionCount);

    Add('SetPopulationCount',
        '(TGeneticMO_Optimizer) Estabelece o número de indivíduos da população',
        '',
        [pvtInteger],
        [nil],
        [false],
        pvtNull,
        TObject,
        amSetPopulationCount);

    Add('LoadPopulation',
        '(TGeneticOptimizer, TGeneticMO_Optimizer) Le os indivíduos da população',
        '',
        [pvtString],
        [nil],
        [false],
        pvtNull,
        TObject,
        amLoadPopulation);

    Add('SavePopulation',
        '(TGeneticOptimizer, TGeneticMO_Optimizer) Salva os indivíduos da população',
        '',
        [pvtString],
        [nil],
        [false],
        pvtNull,
        TObject,
        amSavePopulation);

    Add('SetPopFilename',
        '(TGeneticOptimizer, TGeneticMO_Optimizer) Estabelece o nome do arquivo onde a população deverá ser salva',
        '',
        [pvtString],
        [nil],
        [false],
        pvtNull,
        TObject,
        amSetPopFilename);
    end;

  with Functions do
    begin
    Add('GetObjetivesCount',
        'Retorna o número de funções objetivos a serem otimizadas.'#13 +
        'Nos otimizadores "TRosenbroke" e "TGeneticOptimizer" a chamada deste método não tem função.',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetObjetivesCount);

    Add('GetTolerante',
        'Retorna a tolerancia da Função Objetivo',
        '',
        [],
        [],
        [],
        pvtReal,
        TObject,
        amGetTolerante);

    Add('GetIncStep',
        'Retorna o passo de incremento',
        '',
        [],
        [],
        [],
        pvtReal,
        TObject,
        amGetIncStep);

    Add('GetDecStep',
        'Retorna o passo de decremento',
        '',
        [],
        [],
        [],
        pvtReal,
        TObject,
        amGetDecStep);

    Add('GetInitialStep',
        'Retorna o passo inicial',
        '',
        [],
        [],
        [],
        pvtBoolean,
        TObject,
        amGetInitialStep);

    Add('GetMaxSimulations',
        'Retorna o número máximo de simulações',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetMaxSimulations);

    Add('GetMaxTimeSimulation',
        'Retorna o tempo máximo de simulação',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetMaxTimeSimulation);

    Add('Parameters',
        'Retorna uma referência aos parâmetros',
        '',
        [],
        [],
        [],
        pvtObject,
        TParameters,
        amGetParameters);

    Add('ClassName',
        'Retorna o nome da Classe',
        '',
        [],
        [],
        [],
        pvtString,
        TObject,
        amGetClass);

    Add('getPopulationCount',
        '(TGeneticOptimizer, TGeneticMO_Optimizer) Retorna o número de indivíduos da população',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetPopulationCount);

    Add('getPopulationDSP',
        '(TGeneticOptimizer, TGeneticMO_Optimizer)'#13 +
        'Retorna o Desvio Padrão dos indivíduos da população.'#13 +
        'Na classe "TGeneticOptimizer" o índice da aptidão não é considerado.'#13 +
        'Parâmetro: Índice da Aptidão',
        '',
        [pvtInteger],
        [nil],
        [false],
        pvtReal,
        TObject,
        amGetPopulationDSP);

    Add('getPopulationMean',
        '(TGeneticOptimizer, TGeneticMO_Optimizer)'#13 +
        'Retorna a Média dos indivíduos da população.'#13 +
        'Na classe "TGeneticOptimizer" o índice da aptidão não é considerado.'#13 +
        'Parâmetro: Índice da Aptidão',
        '',
        [pvtInteger],
        [nil],
        [false],
        pvtReal,
        TObject,
        amGetPopulationMean);

    Add('getAptness',
        '(TGeneticOptimizer, TGeneticMO_Optimizer) Retorna uma das aptidões de um indivíduo dado seu índice.'#13 +
        'Parâmetros: Índice do Individuo, Índice da Aptidão'#13 +
        'O segundo parâmetro é ignorado no "TGeneticOptimizer"',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [false, false],
        pvtReal,
        TObject,
        amGetAptness);

    Add('getGen',
        '(TGeneticOptimizer, TGeneticMO_Optimizer) Retorna o valor do Gen de um indivíduo'#13 +
        'Parâmetros: índice do indivíduo, índice do gen',
        '',
        [pvtInteger, pvtInteger],
        [nil, nil],
        [false, false],
        pvtReal,
        TObject,
        amGetGen);

    Add('getPopFilename',
        '(TGeneticOptimizer, TGeneticMO_Optimizer)'#13 +
        'Retorna o nome corrente do arquivo onde a população está sendo salva.',
        '',
        [],
        [],
        [],
        pvtString,
        TObject,
        amGetPopFilename);

    Add('getEvolutionCount',
        '(TGeneticOptimizer, TGeneticMO_Optimizer)'#13 +
        'Indica o número de evoluções que ja ocorreram na População (Gerações)',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetEvolutionCount);

    Add('getRMax',
        '(TGeneticMO_Optimizer)'#13 +
        'Retorna o rank máximo alcançado em cada evolução.',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetRMax);

    Add('getWorstsCount',
        '(TGeneticMO_Optimizer)'#13 +
        'Retorna o quantidade dos piores (com RMax) indivíduos em cada geração.',
        '',
        [],
        [],
        [],
        pvtInteger,
        TObject,
        amGetWorstsCount);
    end;
end;

class procedure TpsOptimizer.amGetDecStep(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(
    TRosenbrock(Stack.AsObject(1)).DecStep
    );
end;

class procedure TpsOptimizer.amGetIncStep(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(
    TRosenbrock(Stack.AsObject(1)).IncStep
    );
end;

class procedure TpsOptimizer.amGetInitialStep(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushBoolean(
    TRosenbrock(Stack.AsObject(1)).InitialStep
    );
end;

class procedure TpsOptimizer.amGetMaxSimulations(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(
    TRosenbrock(Stack.AsObject(1)).MaxSimulations
    );
end;

class procedure TpsOptimizer.amGetMaxTimeSimulation(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(
    TRosenbrock(Stack.AsObject(1)).MaxTimeSimulation
    );
end;

class procedure TpsOptimizer.amGetParameters(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushObject(
    TOptimizer(Stack.AsObject(1)).Parameters
    );
end;

class procedure TpsOptimizer.amGetTolerante(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushFloat(
    TOptimizer(Stack.AsObject(1)).Tolerance
    );
end;

class procedure TpsOptimizer.amSetComplexCount(const Func_Name: String; Stack: TexeStack);
begin
  TGeneticOptimizer(Stack.getSelf()).ComplexCount := Stack.AsInteger(1);
end;

class procedure TpsOptimizer.amSetPopEvolutionCount(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := Stack.getSelf();
  if o is TGeneticOptimizer then
     TGeneticOptimizer(o).PopEvolutionCount := Stack.AsInteger(1)
  else
     TGeneticMO_Optimizer(o).PopEvolutionCount := Stack.AsInteger(1)
end;

class procedure TpsOptimizer.amSetDecStep(const Func_Name: String; Stack: TexeStack);
begin
  TRosenbrock(Stack.AsObject(2)).DecStep := Stack.AsFloat(1);
end;

class procedure TpsOptimizer.amSetIncStep(const Func_Name: String; Stack: TexeStack);
begin
  TRosenbrock(Stack.AsObject(2)).IncStep := Stack.AsFloat(1);
end;

class procedure TpsOptimizer.amSetInitialStep(const Func_Name: String; Stack: TexeStack);
begin
  TRosenbrock(Stack.AsObject(2)).InitialStep := Stack.AsBoolean(1);
end;

class procedure TpsOptimizer.amSetMaxSimulations(const Func_Name: String; Stack: TexeStack);
begin
  TRosenbrock(Stack.AsObject(2)).MaxSimulations := Stack.AsInteger(1);
end;

class procedure TpsOptimizer.amSetMaxTimeSimulation(const Func_Name: String; Stack: TexeStack);
begin
  TRosenbrock(Stack.AsObject(2)).MaxTimeSimulation := Stack.AsInteger(1);
end;

class procedure TpsOptimizer.amSetTolerante(const Func_Name: String; Stack: TexeStack);
begin
  TOptimizer(Stack.AsObject(2)).Tolerance := Stack.AsFloat(1);
end;

class procedure TpsOptimizer.amShowOFViwer(const Func_Name: String; Stack: TexeStack);
begin
  TOptimizer(Stack.getSelf()).Show_OFViwer(Stack.AsInteger(1), Stack.AsInteger(2), Stack.AsInteger(3));
end;

class procedure TpsOptimizer.amShowParsManager(const Func_Name: String; Stack: TexeStack);
begin
  TOptimizer(Stack.AsObject(3)).Show_ParsManager(Stack.AsInteger(1), Stack.AsInteger(2));
end;

class procedure TpsOptimizer.amStop(const Func_Name: String; Stack: TexeStack);
begin
  TOptimizer(Stack.AsObject(1)).Stop;
end;

class procedure TpsOptimizer.amGetClass(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushString(TOptimizer(Stack.getSelf).ClassName);
end;

class procedure TpsOptimizer.amGetAptness(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := Stack.getSelf();

  if o is TGeneticOptimizer then
     Stack.PushFloat(
       TGeneticOptimizer(o).getAptness(Stack.AsInteger(1))
     )
  else
     Stack.PushFloat(
       TGeneticMO_Optimizer(o).getAptness(Stack.AsInteger(1), Stack.AsInteger(2))
     )
end;

class procedure TpsOptimizer.amGetGen(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := Stack.getSelf();
  if o is TGeneticOptimizer then
     Stack.PushFloat(TGeneticOptimizer(o).getGen(Stack.AsInteger(1), Stack.AsInteger(2)))
  else
     Stack.PushFloat(TGeneticMO_Optimizer(o).getGen(Stack.AsInteger(1), Stack.AsInteger(2)))
end;

class procedure TpsOptimizer.amGetPopulationCount(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := Stack.getSelf();
  if o is TGeneticOptimizer then
     Stack.PushInteger(TGeneticOptimizer(o).PopulationCount)
  else
     Stack.PushInteger(TGeneticMO_Optimizer(o).PopulationCount)
end;

class procedure TpsOptimizer.amSetOFValue(const Func_Name: String; Stack: TexeStack);
begin
  TOptimizer(Stack.getSelf()).setOFValue(Stack.AsInteger(1), Stack.AsFloat(2));
end;

class procedure TpsOptimizer.amSetObjetivesCount(const Func_Name: String; Stack: TexeStack);
begin
  TOptimizer(Stack.getSelf()).ObjectivesCount := Stack.AsInteger(1);
end;

class procedure TpsOptimizer.amGetObjetivesCount(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger(TOptimizer(Stack.getSelf()).ObjectivesCount);
end;

class procedure TpsOptimizer.amSetPopulationCount(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := Stack.getSelf();
  if o is TGeneticMO_Optimizer then
     TGeneticMO_Optimizer(o).PopulationCount := Stack.AsInteger(1);
end;

class procedure TpsOptimizer.amGetPopulationDSP(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := Stack.getSelf();
  if o is TGeneticOptimizer then
     Stack.PushFloat(TGeneticOptimizer(o).getPopulationDSP(Stack.AsInteger(1)))
  else
     Stack.PushFloat(TGeneticMO_Optimizer(o).getPopulationDSP(Stack.AsInteger(1)))
end;

class procedure TpsOptimizer.amGetPopulationMean(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := Stack.getSelf();
  if o is TGeneticOptimizer then
     Stack.PushFloat(TGeneticOptimizer(o).getPopulationMean(Stack.AsInteger(1)))
  else
     Stack.PushFloat(TGeneticMO_Optimizer(o).getPopulationMean(Stack.AsInteger(1)))
end;

class procedure TpsOptimizer.amLoadPopulation(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := Stack.getSelf();
  if o is TGeneticOptimizer then
     TGeneticOptimizer(o).LoadPopulation(Stack.AsString(1))
  else
     TGeneticMO_Optimizer(o).LoadPopulation(Stack.AsString(1))
end;

class procedure TpsOptimizer.amSavePopulation(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := Stack.getSelf();
  if o is TGeneticOptimizer then
     TGeneticOptimizer(o).SavePopulation(Stack.AsString(1))
  else
     TGeneticMO_Optimizer(o).SavePopulation(Stack.AsString(1))
end;

class procedure TpsOptimizer.amGetPopFilename(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := Stack.getSelf();
  if o is TGeneticOptimizer then
     Stack.PushString( TGeneticOptimizer(o).PopFilename )
  else
     Stack.PushString( TGeneticMO_Optimizer(o).PopFilename );
end;

class procedure TpsOptimizer.amSetPopFilename(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := Stack.getSelf();
  if o is TGeneticOptimizer then
     TGeneticOptimizer(o).PopFilename := Stack.AsString(1)
  else
     TGeneticMO_Optimizer(o).PopFilename := Stack.AsString(1)
end;

class procedure TpsOptimizer.amGetEvolutionCount(const Func_Name: String; Stack: TexeStack);
var o: TObject;
begin
  o := Stack.getSelf();
  if o is TGeneticOptimizer then
     Stack.PushInteger( TGeneticOptimizer(o).EvolutionCount )
  else
     Stack.PushInteger( TGeneticMO_Optimizer(o).EvolutionCount )
end;

class procedure TpsOptimizer.amGetRMax(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger( TGeneticMO_Optimizer(Stack.getSelf).RMax );
end;

class procedure TpsOptimizer.amGetWorstsCount(const Func_Name: String; Stack: TexeStack);
begin
  Stack.PushInteger( TGeneticMO_Optimizer(Stack.getSelf).WorstsCount );
end;

end.

