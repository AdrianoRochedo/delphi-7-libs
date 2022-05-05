{.$I wsDEFINE.INC}

unit wsMasterModel;

interface

uses
  SysUtils, Classes, Dialogs, Forms, Edit, SysUtilsEx, Gridx32, wsMatrix, wsGLib,
  wsTabelaDeSimbolos, wsFuncoesDeDataSets, wsVec, wsConstTypes, wsListaDeTermos;

{ 08/01/1998:
  Implementar m�todo < Undo > da classe < TMasterModel >.
  Este m�todo ter� aux�lio do campo interno [FNivel] que conter� a informa��o de
  at� onde a classe chegou.
  A tarefa de < Undo > ser� destruir os objetos criados at� aquele momento e
  arrumar o estado corrente da classe.

  05/10/99
  Correcao bug de GetRespVars
  Inicio Regressao multivariada

  02/12/99
  Adequacao de wsRegressoes generalizando o uso da matriz de produtos cruzados
  para todos os modelos lineares

  23/12/99
  Revisao da matriz de incidencia, para incorporar a ideia a todos os modelos
  de analise da variacao. Foram introduzidos processos de codificacao para os
  varios tipos de modelos

  27/12/99 - Programa compilou e funcionou para varios exemplos

  29/12/99 - Inicio da implantacao da classe para analise de modelos de analise da variacao
    nao balanceados. Adequacao de todos os metodos
  08/01/00 - Funcionamento das somas de quadrados do tipo III

  15/08/00 - Correcao de bugs na definicao de modelos de regressao que possuiam variaveis
             que nao estavam presentes no conjunto de dados
           - Implmentacao da regressao ponderada
           - Organizacao de varios metodos para melhor compartilhamento como eh o caso dos
             metodos para somas de quadrados e produtos
}

Type
  TMeanTest=(mtDuncan, mtTukey, mtSNK, mtDMS, mtModDMS, mtScheffe, mtDunnet, mtModTukey);

  TVarSelect = (Stepwise, AllReg, RForward, Backward);

  PtrArray = ^TPtrArray;
  TPtrArray = Array [0..MaxNumPointer-1] Of Pointer;

  TwsModelType = (mtUnivBalAnova,   // Anova univariada balanceada
                mtUnivBalAncova,    // Ancova univariada balanceada
                mtUnivCompAnova,    // Anova univariada completa
                mtUnivCompAncova,   // Ancova univariada completa
                mtUnivIncompAnova,  // Anova univariada incompleta
                mtUnivIncompAncova, // Ancova univariada incompleta
                mtMultBalAnova,     // Anova multivariada balanceada
                mtMultBalAncova,    // Ancova multivariada balanceada
                mtMultCompAnova,    // Anova multivariada completa
                mtMultCompAncova,   // Ancova multivariada completa
                mtMultIncompAnova,  // Anova multivariada incompleta
                mtMultIncompAncova, // Ancova multivariada incompleta
                mtUnivRegComp,      // Regress�o univariada modelo completo
                mtUnivWReg,         // Regressao univariada ponderada
                mtAllReg,           // Regress�o univariada todas as regress�es
                mtStepwiseSelect,   // Reg. univariada passo a passo, stepwise
                mtForwardSelect,    // Reg. univariada passo a passo, forward
                mtBackwardEliminat, // Reg. univariada passo a passo, backward
                mtMultReg,          // Regress�o multivariada
                mtRegression,       // Analise de regressao
                mtAnova,            // Analise da variacao
                mtNone);

  // Tipos de contrastes
  TMeanContr = (mcBonferroni, mcScheffe, mcF);

  // Tipos de variaveis que serao consideradas
  TwsInternalVarType = (tiRespostas, tiFatores, tiCovarianca, tiNumericas);

  TwsLMManager = class;

  {  Heran�a
      TwsLinearModel --> TObject
   Objetivo
      A classe TwsLinearModel serve de base para implementa��o de todas as demais classes
      para an�lise de modelos lineares. � uma classe abstrata e n�o poder� ser chamada
      diretamente. Dentre outras caracter�sticas, possibilita a defini��o de:
        * F�rmulas estruturais, utilizando operadores estruturais de cruzamento e aninhamento,
          dentre outros.
        * Modelos de regress�o, de varia��o e de covaria��o
        * Vari�veis de grupo
        * Filtros de observa��es
        * Express�es dentro dos modelos
        * An�lise univariada ou multivariada
        * Estruturas completas balanceadas, completas n�o balanceadas e incompletas
  }
  TwsLinearModel = Class
  Private
    FIntercept  : Boolean;        // Intercepto para o modelo
    FXMat       : TwsGeneral;     // Matriz X. Cuidar a destruicao nos descendentes
    FManager    : TwsLMManager;   // Gerenciador de modelos lineares
    FIncMat     : TwsGeneral;     // Matriz de incid�ncia
  Protected
    dsWIndex    : Integer;        // Indice da variavel peso no conjunto de dados
    wInd        : Integer;        // Indice da variavel peso na matriz X
    WVar        : Boolean;        // Existe variavel peso ?
    wtot,                         // Soma dos pesos
    DFRes       : double;   // Graus de liberdade do residuo
    NPar,                         // N�mero de par�metros
    NResp,                        // N�mero de vari�veis respostas
    NPred,                        // N�mero de vari�veis preditoras
    NumObs      : Integer;        // Numero de observacoes

    CoefList    : TList;          // Lista com as sa�das dos coeficientes

    Terms       : TwsTermList;    // Lista de termos criados a partir do modelo definido
    DSToAnalysis: TwsDataset;     // Conjunto de dados em analise
    CovIDX,                       // Indices das covariaveis (num�ricas a direita)
    FacIDX,                       // Indices dos fatores
    NumIDX,                       // Indices das variaveis (todas) num�ricas
    RIDX,                         // Indices das vari�veis respostas
    RightIDX    : TwsLIVec;       // Indices das variaveis a direita

    FXY         : TwsSymmetric;  { Matriz sim�trica que vai armazenar as m�dias na primeira
                                   coluna e e nas demais posicoes os produtos cruzados entre as
                                   variaveis X e Y ajustados para as respectivas m�dias
                                 }
    Toler       : TwsVec;        // Tolerancias para aplicacao do sweep
    ColOp       : TwsLIVec;     { Vetor inicializado com 1 em todas as posicoes. Quando a
                                  coluna i de FXY e operada pelo sweep, a posicao i do ColOp
                                  troca de sinal. � criado e atualizado nos descendentes
                                 }
    procedure Fit; virtual; abstract;  { Proporciona forma de defini��o dos m�todos virtuais
                                         nos descendentes para ajustameto dos diferentes
                                         modelos lineares. Em algumas situacoes o m�todo Fit opera
                                         as colunas da matriz de SQ&P e obt�m quantidades
                                         importantes para o processo de an�lise, em outras opera
                                         diretamante a matriz do modelo
                                        }

    Procedure CrossProduct; virtual;       // Matriz de produtos cruzados

    Procedure WCrossProduct(Col: TwsLIVec); virtual; // Matriz de produtos cruzados ponderados

    procedure GetIncMat; virtual;          // Obtem a matriz de incid�ncia

    Function  XLine(i:Integer): TwsVec; virtual; // Obtem uma linha da matriz do modelo

    procedure GetXMat; virtual;            // Obtem a matriz do modelo
    Destructor Destroy; Override;
  Public
    Constructor Create(Manager: TwsLMManager);
    // Estatisticas multivariadas
//    function MultivarStat(Teta:TwsVec;r,rc,rw:Integer): TwsDataSet;
    procedure Coefficients; virtual; abstract; { Organiza a apresenta��o e executa os testes para os
                                                 coeficientes correspondentes aos termos do modelo }

    Property Intercept: Boolean Read FIntercept Write FIntercept; // Modelo possui intercepto?

    property XMat: TwsGeneral Read FXMat Write FXMat;       // Matriz do modelo

    property IncMat: TwsGeneral Read FIncMat Write FIncMat; // Matriz de incid�ncia

    property Manager: TwsLMManager Read FManager Write FManager;  // Gerenciador de modelos lineares
  End; {TwsLinearModel}

  {---------------------------  Analise da variacao  --------------------------}
  { Heran�a
     TwsAnova --> TwsLinearModel
    Objetivo
      Classe base para defini��o de todas as demais classes para an�lise da varia��o. S�o
      consideradas classes para an�lise da varia��o aquelas destinadas a analisar modelos
      lineares que possuam no lado direito na f�rmula pelo menos um fator. Nesta classe s�o
      definidos m�todos importantes para todas as demais classes: Colunas da matriz X para
      cada termo do modelo, graus de liberdade para cada termo, linhas da matriz X, testes
      de compara��es m�ltiplas, de contrastes e regress�o polinomial

  }
  TwsAnova = Class(TwsLinearModel)
  Private
    FRand         : TBits;         // True na posi��o i se a var. i � aleat�ria
    FIncEnder     : PtrArray;      // Endere�os das matrizes de codificacao
    TermInd,NumInd: TwsLIVec;      // Indices dos termos e numericas em X
    Teta          : TwsGeneral;    // Estimativas dos parametros do modelo
    VTeta         : TwsSymmetric;  // Coeficientes da variancia
    nCov          : Integer;       // Numero de covariaveis presentes no modelo
    // Obtem graus de liberdade e colunas iniciais e finais de cada termo
    Procedure GetDF(out glTrat: double); virtual;
    // Obtem linha da matriz X
    Function  XLine(i:Integer): TwsVec; override;
    // Obtem os nomes das colunas de X
    function  XNameCols: TStrings;
    // Matriz X para este caso particular
    procedure GetXMat; override;
    // Comparacoes entre medias
    Procedure MultRange(Teste, Fator, Grupo, Taxa, Variancia: String);
    // Testes para contrastes especificados
    Procedure Contrasts(Teste, Fator, Grupo, Variancia: String);
    // Analise de regressao polinomial
    Procedure Polinomial(Fator, Grupo, Variancia, DMax: String);
    // Imprime todas as matrizes de contrastes associadas aos diferentes fatores
    procedure PrintCodeMat;
    Destructor  Destroy; Override;
  Public
    Constructor Create(Manager: TwsLMManager);
    // Obtem as medias de minimos quadrados
    procedure LSMeans;
  End;

  {--------------------------  Analise da variacao balanceada  -------------------------}
  { Heran�a
      TwsBAnova --> TwsAnova --> TwsLinearModel
    Objetivo
      An�lise de modelos de an�lise da varia��o de estruturas completas balanceadas.
      Utiliza um algoritmo apropriado para estas situa��es, descrito entre outros em
      Bock (1972). Neste algoritmo obtem-se uma matriz, denominada matriz das estimativas
      ortogonais onde a partir de cada coluna s�o obtidas as somas de quadrados para an�lise
      univariada e o conjunto de colunas gera as somas de quadrados e produtos para a an�lise
      multivariada
  }
  TwsBAnova = Class(TwsAnova)
  Private
    NReps  : Word;        // N�mero de repeti��es
    SSColX : TwsVec;      // Soma dos quadrados das colunas de X
    U      : TwsGeneral;  // Matriz das estimativas ortogonais
    // Matriz de incid�ncia para este caso particular
    Procedure GetIncMat; Override;
    // Obtem matriz das estimativas ortogonais
    procedure GetOrthEstimate;
    // Obtem os coeficientes no modelo ANOVA e respectivos testes
    procedure Coefficients; override;
    Destructor Destroy; Override;
  Public
    Constructor Create(Manager: TwsLMManager);
  End;

  {----------------------  Analise da variacao balanceada univariada  ------------------}

  { Heran�a
      TwsUBAnova --> TwsBAnova --> TwsAnova --> TwsLinearModel
    Objetivo
      An�lise de modelos de an�lise da varia��o univariados com estruturas de dados
      balanceadas.
  }
  TwsUBAnova = Class(TwsBAnova)
  Private
    // Obtem o vetor de SQ para cada termo
    Procedure Fit; Override;
  End;

  {--------------------  Analise da variacao balanceada multivariada  ----------------}

  { Heran�a
      TwsMBAnova --> TwsBAnova --> TwsAnova --> TwsLinearModel
    Objetivo
      An�lise de modelos de an�lise da varia��o multivariados com estruturas de dados
      balanceadas.
  }
  TwsMBAnova = Class(TwsBAnova)
  Private
    ResAdj: TwsSymmetric;   // Matriz com as somas de quadrados e produtos ajustadas
    // Obtem as matrizes de SQ&P para cada termo
    Procedure Fit; Override;
    destructor Destroy; override;
  End;

  { Heran�a
      TwsUBAncova --> TwsMBAnova --> TwsBAnova --> TwsAnova --> TwsLinearModel
    Objetivo
      An�lise de modelos de an�lise da varia��o multivariados com estruturas de dados
      balanceadas.
  }
  TwsUBAncova = class(TwsMBAnova)
  private
    // Obtem as somas de quadrados e produtos ajustados
    procedure Fit; override;
    // Obtem os coeficientes do modelo ANCOVA
    procedure Coefficients; override;
  end;

  {-----------------------  Analise da variacao nao balanceada  ----------------------}
{
 Heran�a
   TwsNBAnova --> TwsAnova --> TwsLinearModel
 Objetivo
   Classe base para an�lise de modelos com estruturas de dados n�o balanceadas
}
  TwsNBAnova = Class(TwsAnova)
  Private
    // Matriz de incid�ncia
    Procedure GetIncMat; Override;
    // Graus de liberdade, colunas da matriz X para cada termo, etc.
    Procedure GetDF(out glTrat: double); override;
    // Matriz de c�digos adequadas para a classe
    function GetCodeMat(C: TwsFactor): TwsGeneral; virtual; abstract;
    // Matriz X para o caso nao balanceado
    procedure GetXMat;
  Public
    Constructor Create(Manager: TwsLMManager);
    // Obtem os coeficientes do modelo ANOVA nao balanceado
    procedure Coefficients; override;
  End;

  {------------  Analise da variacao completa nao balanceada univariada --------------}
{
 Heran�a
   TwsUNBAnova3 --> TwsNBAnova --> TwsAnova --> TwsLinearModel
 Objetivo
   Classe base para an�lise de modelos com estruturas de dados n�o balanceadas mas completas
}
  TwsUNBAnova3 = Class(TwsNBAnova)
  Private
    // Obtem o vetor de SQ Tipo III para cada termo
    procedure Fit; Override;
    // Obtem a matriz de codificacao para as classificacoes nao balanceadas completas
    function GetCodeMat(C: TwsFactor): TwsGeneral; override;
  End;

{
 Heran�a
   TwsUNBAnova1 --> TwsNBAnova --> TwsAnova --> TwsLinearModel
 Objetivo
   Classe base para an�lise de modelos com estruturas de dados incompletas
}
  TwsUNBAnova1 = Class(TwsNBAnova)
  Private
    // Obtem o vetor de SQ Tipo I para cada termo
    procedure Fit; Override;
    // Obtem a matriz de codificacao para as classificacoes nao balanceadas incompletas
    function GetCodeMat(C: TwsFactor): TwsGeneral; override;
  End;
  {---------------------------------------------------------------}

{
  Heran�a
    TwsLMManager --> TObject
  Objetivo
    Esta classe tem uma enorme import�ncia na utiliza��o das classes de an�lise de modelos lineares.
    Ela gerencia a obten��o dos frames de dados, os frames de m�dias, avalia o modelo e gerencia a
    aplica��o da classe mais adequada para a analaise do modelo linear
}
  TwsLMManager = class
  private
    FClassType  : TwsModelType;    // Tipo de modelo em utilizacao

    FModel      : TwsLinearModel;  // Modelo linear em uso
    FFormula,                      // Formula do modelo
    FWeightVar,                    // Vari�vel peso para pondera��o
    FCondition,                    // Condi��o entrada pelo usu�rio
    FGroups     : String;          // Lista dos grupos em formato String
    FTerms      : TwsTermList;     // Lista de Termos

    Graf_Reg    : TForm;           // Gr�ficos da an�lise de regress�o

    FRIdx,                         // �ndices das Respostas
    FFacIdx,                       // �ndices dos Fatores
    FCovIdx,                       // �ndices das Covari�veis
    FRightIdx,                     // Indices das variaveis a direita
    FNumIdx     : TwsLIVec;        // �ndice das Vari�veis num�ricas

    FTV         : TwsTabVar;         // Tabela de simbolos para calculo de expressoes
    FDataSet    : TwsDataSet;      // Dataset escolhido pelo usu�rio

    FUnivariado,                   // Analise univariada ?
    FIntercepto : Boolean;         // Modelo com intercepto ?

    // Obtem o objeto para analise do modelo linear
    Function GetModel: TwsLinearModel;
    // Obtem o dataframe da lista de dataframes
    Function GetMFrame(i: Integer): TwsDataSet;
    // Obtem o numero de conjuntos de dados da lista de dataframes
    Function GetNData: Word;
    // Indices das variaveis respostas
    Function GetIndexResp(SL: TStrings): TwsLIVec;
    // Indices das variaveis por tipo
    Function GetIndexByType(SL: TStrings; T: TwsEnumDataType): TwsLIVec;
    // Indices das variaveis desejadas
    Function GetIndex(SL: TStrings): TwsLIVec;
    // Lista das variaveis respostas
    Function GetRespVars : TStrings;
    // Existe variavel do tipo fator no modelo?
    Function HaveFactor(SL: TStrings): Boolean;
    // Existe variavel numerica no modelo?
    Function HaveNumeric(SL: TStrings): Boolean;
    // Qual a classe mais adequada para analisar o modelo linear?
    function GetClassType(Data: TwsDataSet; VarSelect: Integer = 0): TwsModelType;
    // Obtem o vetor das medias gerais
    function GetGMean: TwsDFVec;
    // Seta o conjunto de dados para analise
    Procedure SetDataSet(Value: TwsDataSet);
    Destructor Destroy; Override;
  public
    Error         : Word;       // 0 - Se nao ocorreu nenhum erro
                                // 1 - Erro na chamada de ModelFrame.

    SSList,                     // Lista com as SQ
    GMeanList,                  // Lista com as m�dias
    DSMeanList: TList;          // Lista com os conjuntos de m�dias

    FListFrame  : TwsDataSets;        // Conjuntos obtidos atraves das vari�veis de grupo

    DataIndex   : Integer;      // Indice do conjunto de dados a analisar

    AnalysisType: TwsModelType; // Qual o tipo de modelo em analise

    Options     : TBits;        // Opcoes gerais estabelecidas na interface

    stClass     : String;       // Descri��o da classe utilizada para analisar
                                // o conjunto de dados
    LResp,                      // Nomes das vari�veis respostas
    LVar        : TStrings;     // Nomes das vari�veis � direita
    State,                      // Estado ou est�gio da opera��o
    ValidObs    : Integer;      // Numero de observacoes validas
    Constructor Create(aDataSet: TwsDataset; Const Model, Group, Condition, WV: String);
    // Libera conteudos de memoria criados nesta fase
    Procedure Undo;
    // Avalia o modelo linear especificado na interface
    Procedure EvalModel;
    // Executa analise de regressao multivariada
    Procedure ExecMReg(C, W: TwsGeneral; Desc: String; Alpha: double= 0.05);
    // Executa analise de regressao univariada
    procedure ExecReg(MSelect: Integer; ialpha, oalpha: double; LinHyp: TwsGeneral);
    // Executa analise da variacao
    procedure ExecAnova(FUser: Boolean);
    {Em funcao da especificacao da interface implementa procedimentos de discriminacao
     da variacao de tratamentos
    }
    Procedure DiscrimVarTreat(mrTab, coTab, poTab: TdrStringAlignGrid);
    // Determina classe para adequada para analise do modelo linear
    procedure SetAnovaClass;
    // Implementa os testes de comparacoes multiplas
    procedure MultRangeTest(Const Test: String; Y: TwsGeneral; YCol: Integer;
                            Level, CVar, df: double);
    // Implementa os testes para contrastes
    procedure ContrastTest(Const Test: String; Y, C: TwsGeneral; YCol: Integer;
                           CVar, df: double);
    // Implementa analise de regressao polinomial
    procedure PolRegTest(Means: TwsGeneral; YCol: Integer; x: TwsVec; CVar,
                         DF: double; DMax: Integer);
    { Como a analise da variacao e realizada em varios estagios, limpa tudo que foi
      definido no primeiro estagio - definicao do modelo e conjunto de dados}
    Procedure ClearStage1;
    { Como a analise da variacao e realizada em varios estagios, limpa tudo que foi
      definido no segundo estagio - quadro da analise da variacao e outras opcoes}
    Procedure ClearStage2;
    { Como a analise da variacao e realizada em varios estagios, limpa tudo que foi
      definido no terceiro estagio - discriminacao da variacao de tratamentos}
    Procedure ClearStage3;
    // Retorna objeto para analise do modelo linear
    Property Model      : TwsLinearModel   Read GetModel;
    // Retorna conjunto de dados
    Property DataSet    : TwsDataSet       Read FDataSet     Write SetDataSet;
    // Retorna tabela de definicao de variaveis
    Property TabVar     : TwsTabVar        Read FTV          Write FTV;
    // True se o modelo e univariado
    Property Univariado : Boolean          Read FUnivariado  Write FUnivariado;
    // True se o modelo possui intercepto
    Property Intercept  : Boolean          Read FIntercepto  Write FIntercepto;
    // Indices das variaveis respostas
    Property RIdx       : TwsLIVec         Read FRIdx        Write FRIdx;
    // Indices dos fatores
    Property FacIdx     : TwsLIVec         Read FFacIdx      Write FFacIdx;
    // Indives das covariaveis
    Property CovIdx     : TwsLIVec         Read FCovIdx      Write FCovIdx;
    // Indices das variaveis a direita
    Property RightIdx   : TwsLIVec         Read FRightIdx    Write FRightIdx;
    // Indices das variaveis numericas
    Property NumIdx     : TwsLIVec         Read FNumIdx      Write FNumIdx;
    // Retorna vetor com media geral das variaveis numericas
    Property GeneralMean: TwsDFVec         Read GetGMean;
    // Formula para o modelo linear
    Property Formula    : String           Read FFormula     Write FFormula;
    // Variavel peso
    Property WeightVar  : String           Read FWeightVar   Write FWeightVar;
    // Condicao estabelecida para selecao de observacoes
    Property Condition  : String           Read FCondition   Write FCondition;
    // Grupos decorrentes da indicacao de variaveis de grupo
    Property Groups     : String           Read FGroups      Write FGroups;
    // Quantos conjuntos de dados possui a lista decorrente da construcao dos grupos
    Property NData      : Word             Read GetNData;
    // Lista de termos do modelo
    Property Terms      : TwsTermlist      Read FTerms       Write FTerms;
    // Retorna Dataframe (medias) desejado
    Property MFrame[I:Integer]: TwsDataSet Read GetMFrame;
  end; // TwsLMManager

function GetSignif(const x: double): Integer;
function StrToMeanTest(Const Test: String): TMeanTest;
function StrToMeanContr(Const Test: String): TMeanContr;

implementation
Uses //progVars,
     wsAvaliadordeModelosLineares,
     wsFuncoesDeProbabilidade,
     wsRegressoes,
     //GraficoDoModeloDeRegressao,
     wsJanelaDeGraficos,
     wsGraficos,
     Graphics,
     //ws_const,
     wsMath,
     Math,
     wsFuncoesDeEscalares,
     wsOutPut,
     drGraficosBase,
     advClasses,
     Chart,
     drGraficos;

{$IFDEF DEBUG}
var debug : TDebug;
{$ENDIF}

{=========================== TLinearModel Class =============================}

Constructor TwsLinearModel.Create(Manager: TwsLMManager);
{ Objetivo
    Participa da criacao da instancia de uma classe de modelos lineares. Classe abstrata.
      * Cria refer�ncias para endere�os importantes presentes no gerenciador de modelos
      * Imprime algumas informa��es gerais sobre a an�lise
  Par�metro
    Manager: Gerenciador de modelos lineares
  Campos modificados
    FManager
    NumObs
    Intercept
    Terms
    CovIDX
    FacIDX
    NumIDX
    RightIDX
    RIDX
    NResp
    CoefList
  M�todos chamados
    Create herdado
}
var
  st: String;
  i: Integer;
Begin
  Inherited Create;

  FManager  := Manager;
  NumObs    := Manager.ValidObs;
  Intercept := Manager.Intercept;
  Terms     := Manager.Terms;
  CovIDX    := Manager.FCovIDX;
  FacIDX    := Manager.FFacIDX;
  NumIDX    := Manager.FNumIDX;
  RightIDX  := Manager.FRightIDX;
  RIDX      := Manager.FRIDX;
  CoefList  := TList.Create;
  NResp     := RIdx.Len;                     // Numero de variaveis resposta no modelo

{<<<<<
  gOutPut.WriteTitle('========== An�lise'+Manager.MFrame[Manager.DataIndex].MLab+' ==========',clBlack);
  gOutPut.WriteTitle('Modelo estrutural: '+Manager.FFormula,clBlack);
}
  st :='Modelo expandido: ';
  for i := 0 To Terms.Count-3 do
    begin
    st := st+Terms[i];
    if i<Terms.Count-3 then
      st := st + ' + ';
    end;
  //gOutPut.WriteTitle(st,clBlack);<<<<<
  st := 'Vari�veis respostas: ';
  for i:=1 to NResp do
    begin
    st := st + Manager.LResp[i-1];
    if i<NResp then
      st := st+', ';
    end;
  //gOutPut.WriteTitle(st,clBlack);<<<<<
  if Manager.FWeightVar <> '' then
    begin
    WVar:=True;
{<<<<<
    gOutPut.WriteTitle('Pondera��o pela vari�vel: '+Manager.FWeightVar,clBlack);
}
    end
  else
    WVar:=False;
{<<<<<
  if FManager.FCondition <> '' then
    gOutPut.WriteTitle('Observa��es inclu�das pela condi��o: '+Manager.FCondition,clBlack);
  gOutPut.WriteLine;
}
End; { TLinearModel.Create }

Destructor TwsLinearModel.Destroy;
{ Objetivo
    Libera conte�dos de mem�ria ocupados por campos do objeto
  Campos liberados
    FXMat - Se n�o houver op��o de salvamento
    FXY
    ColOp
    Toler
    FIncMat
    CoefList
  M�todos chamados
    Detroy herdado
}
var
  i: Integer;
Begin
  ColOp.Free;
  Toler.Free;
  FXY.Free;
  FIncMat.Free;
  if not Manager.Options[ord(cmVar_SaveMatMod)] then FXMat.Free;
  if not Manager.Options[ord(cmVar_SaveCoef)] Then
    for i:=0 to CoefList.Count-1 do
      TwsDataSet(CoefList[i]).Free;
  CoefList.Free;
  Inherited Destroy;
End; { TwsLinearModel.Destroy }

procedure TwsLinearModel.GetIncMat;
{ Objetivo
    Cria matriz de incid�ncia das vari�veis nos termos. Quando o modelo possui fatores, o
    m�todo � redefinido.
  Campos criados ou atualizados
    FIncMat
  M�todos chamados
    Nenhum
}
var
  i,j,nInc: Integer;
  Col     : TwsDataSetCol;
begin
  nInc:=Terms.Count-2;
  FIncMat:=TwsGeneral.Create(RightIdx.Len, nInc);
  FIncMat.MLab:= 'Matriz de incid�ncia das vari�veis nos termos';
  FIncMat.Name:= 'Variaveis';
  For i:=0 To nInc-1 Do
    FIncMat.ColName[i+1] := PegaNome(Terms[i]);

  // Para cada variavel do lado direito do modelo
  For i := 1 To RightIdx.Len Do
    Begin
    Col := DSToAnalysis.Struct.Col[RightIdx[i]]; {Rochedo, 24/07/1998}
    FIncMat.RowName[i] := UpperCase(Col.Name);
    // Variavel num�rica
    For j := 0 to nInc-1 do
      If Pertence(Col.Name, PegaNome(Terms[j])) Then
        FIncMat[i,j+1] := 3
      Else
        FIncMat[i,j+1] := 0
    end;
end; { GetIncMat }

Procedure TwsLinearModel.GetXMat;
{ Objetivo
    Obt�m a matriz do modelo adequada para an�lise de modelos. Incorpora os valores das
    respostas e da vari�vel peso, se ela existir
  Campos modificados
    xMat
  M�todos chamados
    xLine
    WriteTable
    AddDataSet
    SendMessageToObjects
}
var
  xCols: double;
  i,j,k: Integer;
  x,aux: TwsVec;
begin
  i:=Terms.Count-1;                // retira residuo e total, inclui intercepto
  if WVar then
    k:=NResp+1                  // respostas + peso
  else
    k:=NResp;                   // respostas
  xMat:=TwsGeneral.Create(0, k+i);
  xMat.Name := 'Mat_X';
  xMat.MLab := 'Matriz do modelo';
  // variaveis preditoras
  xMat.ColName[1] := 'Itcp';
  for j:=1 to i do
    xMat.ColName[j+1]:=PegaNome(Terms[j-1]);
  // variaveis respostas
  for j:=1 to NResp do
    xMat.ColName[i+j] := DSToAnalysis.ColName[RIdx[j]];
  // Constroi as linhas (incluindo as preditoras, resposta e peso) e as insere na matriz
  For i := 1 to DSToAnalysis.NRows do
    Begin
    aux := xLine(i);                              // constroi linha para as preditoras
    x := TwsDFVec.Create(aux.Len+k);
    for j:=1 to aux.Len do
      x[j]:=aux[j];                               // Copia preditoras na linha de X
    for j:=1 to NResp do
      x[aux.Len+j]:=DSToAnalysis[i,RIdx[j]];      // inclui valores das respostas
    aux.Free;
//    x.Name:='Lx'+IntToStr(i);
    xMat.MAdd(x);                                 // Insere na matriz X
    End; {For i}
  // Se existir variavel peso, completa ultima coluna de X
  if WVar then
    begin
    k:=xMat.NCols;
    xMat.ColName[k]:=Manager.WeightVar;
    wInd:=xMat.NCols;
    for i:=1 to xMat.NRows do
      xMat[i,k]:=DStoAnalysis[i,dsWIndex];           // mais o da variavel peso
    end;
end;

Function TwsLinearModel.xLine(i:Integer): TwsVec;
{ Objetivo
    Constroi uma linha da matriz X correspondente � linha especificada do conjunto de dados
  Par�metros
    i: Corresponde � linha do conjunto de dados e da matriz X
  Observa��es
    Cada linha � constru�da com base na matriz de incid�ncia. Neste caso, considera somente
    valores de vari�veis num�ricas. Se existirem fatores, o m�todo ser� redefinido. Se
    o modelo possui intercepto, a primeira coluna possui todos os valores iguais a 1.
  Campos modificados
    Nenhum
}
Var
  VTemp: TwsVec;
  j    : Integer;

   Function ProcessaTermo(j,k:Integer):TwsVec;
   { Constroi uma parte da linha da matriz X correspondente a linha k de DSToAnalysis e o
     termo j
   }
   Var
     ii,j1: Integer;
     LVec  :TList;
     v     :TwsVec;
   Begin
     LVec:=TList.Create;
     j1 := j+1;

     For ii := 1 To FIncMat.NRows Do
       Begin
       If FIncMat[ii,j1] <> 0 Then
         Begin
         v := TwsDFVec.Create(1);
         v[1] := DSToAnalysis[k,RightIdx[ii]];
         LVec.Add(v);
         End;
       End;      // For ii
       Result := TermKron(LVec);
       LVec.Free;
   End;{ ProcessaTermo }

Begin
  Result := TwsDFVec.Create(1);
  Result[1]:=1;
  For j := 1 To FIncMat.NCols Do
    Begin
    VTemp := ProcessaTermo(j-1,i);
    VecAppend(Result, VTemp, False, VTemp.Len);
    VTemp.Free;
    End;
End; { TwsLinearModel.XLine }

Procedure TwsLinearModel.CrossProduct;
{ Objetivo
    Obt�m a matriz de produtos cruzados ajustados para a m�dia a partir da matriz do
    modelo (xMat).
  Campos modificados
    FXY   : Cont�m a matriz de produtos cruzados ajustados para a m�dia envolvendo as
            colunas cujos �ndices est�o em NumIdx, ou seja, de todas as colunas num�ricas
            do conjunto de dados em an�lise. O elemento (1,1) � invertido para aplica��o
            do sweep.
    NumObs: Cont�m as observa��es v�lidas inclu�das na obten��o da matriz de produtos
            cruzados, ou seja, todas as linhas para as quais nenhuma coluna cujo �ndice
            est� em NumIdx n�o cont�m valores perdidos
  M�todos chamados
    Nenhum
}
var
  i,j,k,i1,
  j1,nvar   : Integer;
  dk        : double;
  aux,x     :TwsVec;
  Idx       : TwsLIVec;
begin
  Idx:=Index(2,xMat.NCols);
  nvar := Idx.Len;
  FXY := TwsSymmetric(Jota(nvar+1, nvar+1, mtSymmetric, 0));

  // Atualiza nomes de linhas e colunas
  for i := 1 to nvar+1 do
    Begin
    FXY.ColName[i] := xMat.ColName[i];
    FXY.RowName[i] := FXY.ColName[i];
    End;

  // Constroi a matriz de somas de quadrados e produtos para os valores de todas as variaveis
  aux := TwsDFVec.Create(nvar+1);
  for k:=1 to xMat.NRows do
    begin                                         // Para cada observacao
    x := xMat.Row[k];
    if not x.LocMiss(Manager.NumIdx) then           // Se nao tem valor perdido
      begin
      Inc(NumObs);                                // Atualiza numero de observacoes validas
      for i:= 1 to nvar do
        begin
        i1 := Idx[i];
        aux[i]:=x[i1];
        i1:=i+1;
        aux[i]:=aux[i]-FXY[i1,1];
        FXY[i1,1]:=FXY[i1,1]+aux[i]/NumObs         // Medias na primeira coluna
        end; { for i }

      for i:=1 to nvar do                          // Atualiza matriz X'X para a linha k
        begin
        i1:=i+1;
        dk:=x[Idx[i]]-FXY[i1,1];
        for j:= 1 to i do
          begin
          j1:=j+1;
          FXY[i1,j1]:=FXY[i1,j1]+aux[j]*dk
          end // for j
        end;  // for i
      end;    // if LocMiss
  end;        // For k

  FXY[1,1]:=1/NumObs;
  Aux.Free;
end; { CrossProduct }

Procedure TwsLinearModel.WCrossProduct(Col: TwsLIVec);
{ Objetivo
    Obt�m a matriz de produtos cruzados ponderados ajustados para a m�dia a partir da matriz
    do modelo (xMat). A coluna wInd de xMat � considerada a coluna dos pesos
  Par�metros
    Col : �ndices das colunas que ser�o consideradas
  Campos modificados
    FXY   : Cont�m a matriz de produtos cruzados ajustados para a m�dia envolvendo as
            colunas cujos �ndices est�o em Col. O elemento (1,1) � invertido para aplica��o do sweep.
    NumObs: Cont�m as observa��es v�lidas inclu�das na obten��o da matriz de produtos
            cruzados, ou seja, todas as linhas para as quais nenhuma coluna cujo �ndice
            est� em NumIdx n�o cont�m valores perdidos
    wTot  : Total da vari�vel peso
  M�todos chamados
    xMat.WAdjCrossProd()
  Observa��es
    Linhas com valores perdidos em qualquer coluna cujo �ndice est� em Col ou cujos pesos s�o nulos
    ou negativos s�o eliminadas.
}
var
  i: Integer;
  M: TwsVec;      // Retorna as m�dias de cada coluna
begin
  // Obtem matriz de produtos cruzados com as medias ponderado
  FXY:=xMat.WAdjCrossProd(Col,M,NumObs,wTot,wInd);
  for i:=1 to Col.Len do
    begin
    FXY.ColName[i]:=xMat.ColName[Col[i]];
    FXY.RowName[i]:=FXY.ColName[i]
    end;
  FXY[1,1]:=1/wTot;
  for i:= 2 to FXY.NRows do
    FXY[i,1]:=M[i];
  M.Free;
end;
(*
function TwsLinearModel.MultivarStat(Teta:TwsVec;r,rc,rw:Integer): TwsDataSet;
{Objetivo
   Obt�m as estatisticas para os quatro testes multivariados: Pillai, Roy, Wilks e
     Hotteling-Lawley. Retorna um conjunto de dados com as estatisticas, valor da
     estatistica F, probabilidades, graus de liberdade para numerador e denominador do
     teste F e tipo de aproximacao obtida.
 Par�metros
   Teta: Vetor com os autovalores
   r   : N�mero de autovalores n�o nulos
   rc  : M�nimo entre o n�mero de combina��es independentes entre os par�metros e o n�mero
         de graus de liberdade da hip�tese
   rw  : M�nimo entre o n�mero de combina��es entre as vari�veis respostas e o n�mero de
         vari�veis respostas
 Campos modificados
   Nenhum
}
var
  i: Integer;
  aux1: double;
  Col :TwsDataSetCol;
  v: TwsDFVec;

  procedure FApprox;
  var
    r1,u,t,wl,m,n: double;
    Err: Word;
  begin
    // Wilks
    rc:=Math.Min(NPred,rc);
    rw:=Math.Min(NResp,rw);
    r1:=DFRes-(rw-rc+1)/2;
    u := Sqr(rw)+Sqr(rc)-5;
    if u > 0 then
      t:=Sqrt((Sqr(rw)*Sqr(rc)-4)/u)
    else
      t:=1;
    u:=(rw*rc-2)/2;
    wl := Power(Result[1,1],1/t);

    Result[1,3]:=rw*rc;                                 // GL Numerador
    Result[1,4]:=r1*t-u;                                // GL Denominador
    Result[1,2]:=((1-wl)/wl)*(Result[1,4]/Result[1,3]); // Estatistica F
                                                        // Probablidade
    Result[1,5]:=FInt(Result[1,2],Result[1,3],Result[1,4],True,Err);
    if Math.Min(rw,rc)<=2 then
      Result[1,6] := 0                                  // Exato
    else
      Result[1,6]:=1;                                   // Aproximado

    m:=0.5*(Abs(rw-rc)-1); n:=0.5*(DFRes-rw-1);
    u:=(2*n+r+1); t:=(2*m+r+1);

    // Pillai
    wl:=Result[2,1];
    Result[2,2]:=(u/t)*(wl/(r-wl));                     // Estatistica F
    Result[2,3]:=r*t;                                   // GL Numerador
    Result[2,4]:=r*u;                                   // GL Denominador
                                                        // Probablidade
    Result[2,5]:=FInt(Result[2,2],Result[2,3],Result[2,4],True,Err);
    Result[2,6]:=1;                                     // Aproximado

    // Hotelling-Lawley
    Result[3,2]:=(2*(r*n+1)*Result[3,1]/(Sqr(r)*t));    // Estatistica F
    Result[3,3]:=r*t;                                   // GL Numerador
    Result[3,4]:=2*(r*n+1);                             // GL Denominador
                                                        // Probabilidade
    Result[3,5]:=FInt(Result[3,2],Result[3,3],Result[3,4],True,Err);
    Result[3,6]:=1;                                     // Aproximada

    // Roy
    t := Math.Max(rw,rc);
    Result[4,2]:=Result[4,1]*(DFRes-t+rc)/t;            // LS para estatistica F
    Result[4,3]:=t;                                     // GL Numerador
    Result[4,4]:=DFRes-t+rc;                            // GL Denominador
                                                        // LI para probabilidade
    Result[4,5]:=FInt(Result[4,2],Result[4,3],Result[4,4],True,Err);
    Result[4,6]:=2;                                     // Limite superior
  end; // FApprox

begin // MultivarStat
  Result :=TwsDataSet.Create('MultEstat',0,0);
  Result.ColIdentName := 'Estat�sticas';
  with Result do
    begin
    MLab:='Estat�sticas Multivariadas';
    Struct.AddColEx(TwsNumeric.Create('Valor','Valor da Estat�stica',12,7));  //1
    Struct.AddColEx(TwsNumeric.Create('F','Estat�stica F',12,7));  //2
    Struct.AddColEx(TwsNumeric.Create('GL_Num','Graus de liberdade do numerador',6,5));//3
    Struct.AddColEx(TwsNumeric.Create('GL_Den','Graus de liberdade do denominador',6,5));//4
    Struct.AddColEx(TwsNumeric.Create('Valor_p','N�vel m�nimo de signific�ncia',11,4)); //5
    Col:=TwsQualitative.Create('Tipo','Tipo de aproxima��o para estat�stica',8);       //6
    With TwsQualitative(Col) Do
      Begin
      AddLevel('Exata');
      AddLevel('Aprox');
      AddLevel('L_Sup');
      End; { With Col }
    Struct.AddColEx(Col);                                                              //7
    end; // with Result

  v:=TwsDFVec.Create(6);
  v.Name:='Wilks';
  aux1:=1;
  for i:=1 to r do
    aux1:=aux1*(1/(1+Teta[i]));
  v[1]:=aux1; // Wilks
  Result.MAdd(v);

  v:=TwsDFVec.Create(6);
  v.Name:='Pillai';
  aux1:=0;
  for i:=1 to r do
    aux1:=aux1+(Teta[i]/(1+Teta[i]));
  v[1]:=aux1; // Pillai
  Result.MAdd(v);

  v:=TwsDFVec.Create(6);
  v.Name:='Hot_Lawley';
  aux1:=0;
  for i:=1 to r do
    aux1:=aux1+Teta[i];
  v[1]:=aux1; // Hotelling-Lawley
  Result.MAdd(v);

  v:=TwsDFVec.Create(6);
  v.Name:='Roy';
  v[1]:=Teta[1]; // Roy
  Result.MAdd(v);

  // Obtencao das aproximacoes
  FApprox
end;
*)
{----------------------- TwsAnova ------------------------}

Constructor TwsAnova.Create(Manager: TwsLMManager);
{ Objetivo
    Participa da cria��o de uma inst�ncia de uma classe de modelos lineares para an�lise
    da varia��o. N�o pode ser criada individualmente.
  Par�metros
    Manager: Gerenciador de modelos lineares
  Campos modificados ou atualizados
    DSToAnalysis
    Terms
    FRand
    nCov
  M�todos chamados
    Create herdado
    GetIncMat
    PrintCodeMat
  Sa�das
    Conjuntos de dados gerados por ModelFrame         (I/S)
    Conjuntos de dados com as m�dias de combina��es   (I/S)
    Matriz de incid�ncia                              (  I)
    Matrizes de codifica��o                           (  I)
}
var i  : Integer;
    Col: TwsDataSetCol;
    S  : TwsSymmetric;
Begin
  Inherited Create(Manager);

  if Manager.CovIdx<>nil then nCov:=Manager.CovIdx.Len else nCov:=0;

  DSToAnalysis := TwsDataSet(Manager.DSMeanList[Manager.DataIndex]);

  // Imprime eou salva o conjunto de dados referente ao modelo estabelecido
{<<<<<
  if Manager.Options[ord(cmVar_PrintDadosMod)] then
    TwsOutPut(gOutPut).WriteTable(TwsDataSet(Manager.FListFrame[Manager.DataIndex]));
}
  if Manager.Options[ord(cmVar_SaveDadosMod)] then
     begin
     Manager.TabVar.AddDataSet(TwsDataSet(Manager.FListFrame[Manager.DataIndex]).Name,
                        TwsDataSet(Manager.FListFrame[Manager.DataIndex]));
     //SendMessageToObjects(WSPM_UPDATE_STATUS, [0]);
     end;

{<<<<<
  if Manager.Options[ord(cmVar_PrintMedias)] then
    TwsOutPut(gOutPut).WriteTable(DSToAnalysis);
}
  if Manager.Options[ord(cmVar_SaveMedias)] then
     begin
     Manager.TabVar.AddDataSet(DSToAnalysis.Name,DSToAnalysis);
     //SendMessageToObjects(WSPM_UPDATE_STATUS, [0]);
     end;

  //SQ ou SQP total
  If Manager.Univariado and (nCov=0) Then
    Terms.Term[Terms.Count-1].SetSSq(TwsVec(Manager.SSList[Manager.DataIndex]).Copy(1,NResp))
  else
    begin
    TwsSymmetric(Manager.SSList[Manager.DataIndex]).Copy(mtSymmetric,TwsMatrix(S));
    Terms.Term[Terms.Count-1].SetSSq(S)
    end;

  GetIncMat;
  // Opcao - Matriz de incidencia
{<<<<<
  if Manager.Options[ord(cmVar_PrintMatInc)] Then
    TwsOutPut(gOutPut).WriteTable(FIncMat);
}
  // Imprime contrastes ou matrizes de codifica��o
  if Manager.Options[ord(cmVar_Contrastes)] then
    begin
{<<<<<
    gOutPut.WriteCenter('Matrizes Para Codifica��o da Matriz X');
}
    PrintCodeMat;
    end;

  FRand := TBits.Create;
  FRand.Size := FIncMat.NRows;

  for i := 1 to FIncMat.NRows do
    begin
    Col := DSToAnalysis.Struct.ColByName(FIncMat.RowName[i]);
    FRand[i-1]:=(Col.ColType<>dtNumeric) and (TwsFactor(Col).LevelType=tfRandom);
    end;
End; { TwsAnova.Create }

procedure TwsAnova.LSMeans;
{ Objetivo
    Obt�m um conjunto de dados com as m�dias ajustadas, erros padr�es e matriz de covari�ncias
    para cada termo do modelo onde foi realizado o teste F. Por enquanto, faz somente para uma
    vari�vel. O conjunto de dados gerado � repassado ao termo.
}
var
  LM,A,B,mK : TwsGeneral;
  LV        : TwsSymmetric;
  i,j,nv,it,
  k,nk      : Integer;
  aux       : double;
  v         : TwsVec;
  Lev,GSize : TwsLIVec;
  T,TRes    : TwsTerm;
  Vbl       : TStrings;
  Col       : TwsDataSetCol;
begin
  TRes:=Terms.Term[Terms.Count-2];   // termo do erro
  // Para cada termo, exceto total, residuo e regressao

  for it:=0 to Terms.Count-4 do
    begin
    Vbl:=Terms.Variables(it); // contem as variaveis envolvidas nos termos
    // Faz para uma variavel somente (por enquanto)
    if Vbl.Count > 1 then
      begin
      Vbl.Free;
      Continue
      end;
    T:=Terms.Term[it];
    if T.FTest then     // se foi realizado o teste F
      begin
      // Obtem a matriz de contrastes para o termo
      k:=FIncMat.IndexByName(Terms[it]);
      j:=0;
      repeat    // obtem a primeira matriz
        Inc(j);
        A:=FIncEnder^[Posi(j,k,FIncMat.NCols)-1]
      until A <> nil;
      A.Copy(mtGeneral, TwsMatrix(mK));
      while j<FIncMat.NRows do        // completa o produto de Kronecker
        begin
        Inc(j);
        A:=FIncEnder^[Posi(j,k,FIncMat.NCols)-1];
        if A <> nil then
          begin
          B:=TwsGeneral(mK.Kronecker(A));
          mK.Free;
          B.Copy(mtGeneral,TwsMatrix(mK));
          B.Free
          end;
        end;

      // Matriz com as medias ajustadas
      LM:=TwsGeneral.Create(mK.NRows,NResp);
      v:=Manager.GeneralMean;
      nv:=T.ICol;
      for i:=1 to mK.NRows do
        for j:=1 to NResp do     // para cada resposta
          begin
          nk:=nCov+j;
          aux:=0;
          for k:=1 to mK.NCols do
            aux:= aux+mK[i,k]*Teta[nv+k,nk];
          LM[i,j]:=v[nk]+aux
          end;

      // Matriz de covariancias das medias
      LV:=TwsSymmetric.Create(mk.NRows);
      v:=TwsDFVec.Create(Trunc(T.DF));
      nv:=T.ICol;
      for i:=1 to mK.NRows do // esgota as linhas mK
        begin
        for j:=1 to Trunc(T.DF) do   // para cada coluna das covariancias
          begin
          aux:=0;
          for k:=1 to mk.NCols do
            aux:=aux+mK[i,k]*VTeta[nv+k,nv+j];
          v[j]:=aux;
          end; // for j
        for j:=1 to i do
          begin
          aux:=0;
          for k:=1 to Trunc(T.DF) do
            aux:=aux+v[k]*mK[j,k];
          LV[i,j]:=aux
          end; // for j
        end; // for i
      v.Free;
      // Constroi o conjunto de dados com os resultados
      nv:=Vbl.Count;
      Lev:=TwsLIVec.Create(nv);
      GSize:=TwsLIVec.Create(nv); // tamanho do grupo para geracao dos indices
      for i:=1 to nv do
        begin
        Col:=CopyDescCol(DSToAnalysis.Struct.ColByName(Vbl[i-1]));
        Lev[i]:=TwsFactor(Col).Levels;
        end;
      GSize[nv]:=1; // ultimo grupo tem tamanho 1
      for i:= nv-1 downto 1 do
        GSize[i]:=GSize[i+1]*Lev[i+1];
      // Constroi conjunto de dados para saida das medias ajustadas
      for nk:=1 to NResp do
        begin
        T.LSMeans:=TwsDataSet.Create('MMQ_'+Manager.LResp[nk-1],0,0);
        T.LSMeans.MLab:='M�dias de m�nimos quadrados para o termo '+Terms[it];
        // tantas colunas quantas sao as variaveis
        for i:=1 to nv do
          begin
          Col:=CopyDescCol(DSToAnalysis.Struct.ColByName(Vbl[i-1]));
          T.LSMeans.Struct.AddColEx(Col);   // Vbl.Count colunas
          end;
        // coluna para a media ajustada
        T.LSMeans.Struct.AddColEx(TwsNumeric.Create('Media','M�dia ajustada (m�nimos quadrados)',12,7));
        // coluna para erro padrao da media ajustada
        T.LSMeans.Struct.AddColEx(TwsNumeric.Create('ErrPadr','Erro padr�o da m�dia ajustada',12,7));
        // tantas colunas quantas sao as covariancias
        for i:=1 to LV.NCols do
          T.LSMeans.Struct.AddColEx(TwsNumeric.Create('Cov_'+IntToStr(i),'Covari�ncias',12,7));
        for i:=1 to mk.NRows do
          begin
          v:=TwsDFVec.Create(nv+2+LV.NCols); // fatores+med ajust+erro pad+covarianc
          for j:=1 to nv do   // nv fatores
            v[j]:=GL1(i,Lev[j],GSize[j])-1;
          v[nv+1]:=LM[i,nk];                                         // media ajustada
          v[nv+2]:=Sqrt((LV[i,i]+1/NumObs)*(TRes.AsSSq[nk]/DFRes));  // erro padrao
          for j:=1 to LV.NCols do                                    // covariancias
            v[nv+j+2]:=(LV[i,j]+1/NumObs)*(TRes.AsSSq[nk]/DFRes);
          T.LSMeans.MAdd(v)
          end;
        //TwsOutPut(gOutPut).WriteTable(T.LSMeans);<<<<<
        end; // for nk
        Vbl.free; GSize.Free; Lev.Free;
      LM.Free; LV.Free;
      end;  // if T.FTest
    end // for trm
end; // LSMeans

Destructor TwsAnova.Destroy;
{ Objetivo
    Libera conte�do ocupado pelos campos da classe
  Campos liberados
    FRand
    FIncMat
    FIncEnder
    CoefList
    Chama liberador herdado
}
Begin
  If Assigned(FRand) Then
     begin
     FRand.Free;
     FRand := nil;
     end;
  If Assigned(FIncEnder) and Assigned(Terms) Then
    Begin
    FreeMem(FIncEnder, SizeOf(Pointer) * RightIdx.Len * (Terms.Count - 2));
    FIncEnder := nil;
    End;
  TermInd.Free;
  NumInd.Free;
  Teta.Free;
  VTeta.Free;
  Inherited Destroy
End; { TwsAnova.Destroy }

Procedure TwsAnova.GetDF(out glTrat: double);
{ Objetivo
    Obt�m, para cada termo do modelo, os graus de liberdade, coluna inicial e final da
    matriz X, coeficientes dos componentes de vari�ncia e auxiliares para determinacao dos
    valores esperados dos quadrados m�dios
  Par�metro
    glTrat: Retorna graus de liberdade de tratamentos
  Campos modificados
    DFRes
    TermList
  M�todos chamados
    Nenhum
}
var T,T1      : TwsTerm;   // Termos
    Niveis    : TwsLIVec;  // Numero de n�veis de cada vari�vel
    k,i,i1,x,
    ic        : Integer;
    glTotal   : double;     // Graus de liberdade total
    Col       : TwsDataSetCol;
Begin
  glTotal := NumObs - 1;
  DFRes := glTotal;
  glTrat := 0;

  Niveis := TwsLIVec.Create(RightIDX.Len);
  For k := 1 to RightIDX.Len do
    Begin
    Col := DSToAnalysis.Struct.ColByName(FIncMat.Row[k].Name);
    If Col.ColType <> dtNumeric Then
       Niveis[k] := TwsFactor(Col).Levels
    Else
       Niveis[k] := 1;   // num�ricas sao consideradas como tendo um nivel
    end;

  ic := 1;
  // Colunas da matriz de incidencia armazenam os termos definidos
  For k := 1 to FIncMat.NCols do
    Begin
    T        := Terms.Term[k-1];
    T.DF     := 1;                    // Graus de liberdade
//    T.nr     := NReps;
    T.ICol   := ic;                   // Coluna inicial em X (ou X'X)
    T.Coef   := 1;                    // Coeficiente do componente de variancia
    T.Effect := TBits.Create;         // Auxiliar na determinacao do valor esperado
    T.Effect.Size := FIncMat.NRows;
    T.Fact   := TBits.Create;         // Auxiliar na determinacao do valor esperado
    T.Fact.Size   := FIncMat.NRows;
    T.CTRL   := TBits.Create;         // Auxiliar na determinacao do valor esperado
    T.CTRL.Size   := FIncMat.NRows;

    For i := 0 to FIncMat.NRows - 1 do
      Begin
      i1 := i+1;
      T.Fact[i] := False;
      T.Effect[i] := False;
      x := Trunc(FincMat[i1,k]);
(*
      If x > 0 Then
         Begin
         T.Fact[i] := True;
         If x = 1 Then T.Effect[i] := True;
         End
      Else
        T.Coef := T.Coef*Niveis[i1];
*)
      T.Ctrl[i] := FRand[i] or T.Effect[i];

      If (x > 0) Then
        begin
        T.Fact[i] := True;
        if x = 1 then
          begin
          T.Effect[i] := True;
          T.DF:=T.DF*(Niveis[i1]-1);
          end
        else  // if x=1
          T.DF := T.DF*Niveis[i1];
        end
      else // if x>0
        begin
        T.Coef := T.Coef*Niveis[i1];
//        T.nr := T.nr*Niveis[i1]         // numero de repeticoes para a combinacao
        end
      End; {For}

    DFRes  := DFRes-T.DF;
    glTrat := glTrat+T.DF;
    T.LCol := T.ICol+Trunc(T.DF)-1;   // Coluna final do termo
    ic     := T.LCol+1;
    End; {For k}

  T1 := Terms.Term[Terms.Count-2];     // Residuo
  T1.DF := DFRes;

  T1 := Terms.Term[Terms.Count-1];     // Total
  T1.DF  := glTotal;
  Niveis.Free;
End;

Function TwsAnova.XLine(i:Integer): TwsVec;
{ Objetivo
    Constroi uma linha da matriz X correspondente � linha especificada do conjunto de dados
  Par�metros
    i: �ndice da linha da matriz e do modelo (e do conjunto de dados relacionado) que ser�
       constru�da.
  Observa��es
    Cada linha � constru�da com base na matriz de incid�ncia tmando valores de uma matriz
    de codifica��o (ou contrastes) se a coluna for do tipo fator ou os pr�prios valores,
    se a coluna for do tipo num�rica. Se o modelo possui intercepto, a primeira coluna
    possui todos os valores iguais a 1.
  Campos modificados
    Nenhum
}
Var
  VTemp: TwsVec;
  j    : Integer;

   Function ProcessaTermo(j,k:Integer):TwsVec;
   { Constroi uma parte da linha da matriz X correspondente a linha k de DSToAnalysis e o
     termo j
   }
   Var
     ii,j1,
     M     :Integer;
     LVec  :TList;
     A     :TwsGeneral;
     v     :TwsVec;
   Begin
     LVec:=TList.Create;
     j1 := j+1;

     For ii := 1 To FIncMat.NRows Do
       Begin
       If FIncMat[ii,j1] <> 0 Then
          Begin
          M := RightIdx[ii];
          Case Trunc(FIncMat[ii,j1]) Of
            1,
            2  :Begin
                A := FIncEnder^[Posi(ii,j,FIncMat.NCols)];
                v := A.Row[DSToAnalysis.AsInteger[k,M]+1];
                LVec.Add(v);
                End;
            3  :Begin
                v := TwsDFVec.Create(1);
                v[1] := DSToAnalysis[k,M];
                LVec.Add(v);
                End;
            End; // Case Trunc(FIncMat[ii,j1])
          End;   // If FIncMat[ii,j1] <> 0
       End;      // For ii
       Result := TermKron(LVec);
       LVec.Free;
   End;{ ProcessaTermo }

Begin
  Result := TwsDFVec.Create(1);
  Result[1]:=1;
  For j := 1 To FIncMat.NCols Do
    Begin
    VTemp := ProcessaTermo(j-1,i);
    VecAppend(Result, VTemp, False, VTemp.Len);
    VTemp.Free;
    End;
End; { TwsAnova.XLine }

procedure TwsAnova.GetXMat;
{ Objetivo
    Obt�m a matriz do modelo para os modelos de covari�ncia balanceados. A matriz
    ter� uma coluna para:
      Intercepto
      Cada termo do modelo, incluindo covari�veis
      respostas em an�lise
   Campos modificados
     xMat
   M�todos chamados
     GetDF
     xLine
   Observa��es
     Dependendo das op��es, a matriz do modelo poder� ser impressa ou salva na tabela de
     s�mbolos
}
var
  xCols    : double;
  i,j,ja,
  k,nw,
  nxi,lsize: Integer;
  x,aux    : TwsVec;
  st       : string;
begin
  GetDF(xCols);
  xMat      := TwsGeneral.Create(0, Trunc(xCols));
  //xMat      := TwsGeneral.Create(0, 0);
  xMat.Name := 'Mat_X';
  xMat.MLab := 'Matriz do modelo';
  // Obtem nomes para as colunas de X de acordo com os termos
  // Se o modelo contem covariaveis, elas estarao incluidas
  xMat.cName.Assign(XNameCols);

  nxi:=nCov;
  // nw=1 se existe variavel peso; 0 cc.
  if wVar then nw:=1 else nw:=0;

  // Modelo pode nao incluir efeito isolado da covariavel.
  //Se as covari�veis nao estiverem presentes, insere-as
  for i:=1 to nCov do
    begin
    st:=DSToAnalysis.ColName[CovIdx[i]];
    if (xMat.CName.IndexOf(st)=-1) then
      xMat.CName.Add(st)   // adiciona as variaveis que ainda nao estao em X
    else
      Dec(nxi);             // nCov sera o num de variaveis que ja estao em X
    end;

  // Insere os nomes das respostas como as ultimas colunas de X
  for j:=1 to NResp do
    xMat.CName.Add(DSToAnalysis.ColName[RIdx[j]]);

  // Insere nome da variavel peso, se existir
  if wVar then
    xMat.CName.Add(Manager.WeightVar);

  // intcp, termos, covariaveis, variavel peso e respostas
  xMat.NCols:=1 + xMat.NCols + nxi + nw + NResp;

  // localiza indices das variaveis numericas em xMat
  NumInd:=TwsLIVec.Create(nCov+NResp+nw);
  ja:=0;
  for i:=1 to nCov do
    begin
    NumInd[i]:=xMat.IndexByName(DSToAnalysis.ColName[CovIdx[i]]);
    Inc(ja)
    end;
  lsize:=ja;
  // indices das repostas
  for i:= 1 to NResp do
    begin
    st:=DSToAnalysis.ColName[RIdx[i]];
    NumInd[lsize+i]:=xMat.IndexByName(st);
    Inc(ja)
    end;
  Inc(lsize,ja);
  // indice da variavel peso
  for i:= 1 to nw do
    NumInd[lsize+i]:=xMat.IndexByName(Manager.WeightVar);
  // Localiza indices das colunas dos termos, excluindo covariaveis
  TermInd:=TwsLIVec.Create(xMat.NCols-1-nCov-NResp-nw);
  j:=1;
  for i:=2 to xMat.NCols do
    if (not NumInd.SeqSearch(i,ja)) then  // se a coluna nao eh numerica
      begin
      TermInd[j]:=i;                      // entao eh coluna que envolve fator
      Inc(j)
      end;

  ja:=XMat.NCols;
  lsize:=ja+NResp+nCov+nw;
  For i := 1 to DSToAnalysis.NRows do
    Begin
    aux := xLine(i); // obtem a linha de X, incluindo covariaveis
    ja:=aux.Len;
    // Para copiar a linha de X, covariaveis e respostas
    x := TwsDFVec.Create(lsize);
    for j:=1 to ja do                  // Linha de X
      x[j]:=aux[j];

    for j:=1 to nxi do              // inclui covariaveis que ainda nao estavam
      x[ja+j]:=DSToAnalysis[i,CovIdx[j]];

    Inc(ja,nxi);
    for j:=1 to NResp do            // inclui as respostas
      x[ja+j]:=DSToAnalysis[i,RIdx[j]];

    Inc(ja,NResp);
    for j:=1 to nw do
      x[ja+j]:=DSToAnalysis[i,dsWIndex]; // inclui variavel peso

    aux.Free;
    xMat.MAdd(x);
    End; {For i}
  // Opcao - Matriz do modelo
{<<<<<
  if Manager.Options[ord(cmVar_PrintMatMod)] then
    TwsOutPut(gOutPut).WriteTable(XMat);
}
  if Manager.Options[ord(cmVar_SaveMatMod)] then
     begin
     Manager.TabVar.AddMatrix(XMat.Name, XMat);
     //SendMessageToObjects(WSPM_UPDATE_STATUS_OBJETOS,[0]);
     end;
end;

Procedure TwsAnova.MultRange(Teste, Fator, Grupo, Taxa, Variancia: String);
{  Objetivo
     Prepara as informa��es necess�rias para que possa ser aplicado um teste de compara��es
     m�ltiplas de m�dias
   Par�metros
     Teste    : Teste desejado. As op��es s�o:
                 'Duncan'     - Teste de Duncan
                 'Tukey'      - Teste de Tukey
                 'SNK'        - Teste de Student-Newman-Kewls
                 'Tukey B'    - Teste que utiliza como valor critico a m�dia entre os
                                valores cr�ticos de Tukey e Newman-Kewls
                 'DMS'        - Teste DMS de Fisher
                 'Scheffe'    - Teste de Scheffe
                 'Bonferroni' - Teste de Bonferroni
     Fator    : Fator cujos n�veis ser�o comparados
     Grupo    : Vari�veis de grupo para o teste.
     Taxa     : Taxa de erro desejada (em percentagem).
     Variancia: Express�o para o c�lculo da vari�ncia da compara��o. Esta express�o ser�
                tratada pelo m�todo TermList.GetVariances.
   M�todos chamados
     TwsDataSet.FacMeans()
     TwsTermList.GetVariances
     TwsLMManager.MultRangeTest()
}
Var
  i,j,k,ki,
  IndFator : Integer;
  DS       : TwsDataSet;
  FMeans   : TwsGeneral;
  G        : TStrings;
  L        : TList;
  Tx       : double;
  V, DF    : TwsVec;
  GCols    : TwsLIVec;
  st,st1   : String;
Begin
  DS := FManager.MFrame[FManager.DataIndex];

  IndFator := DS.Struct.IndexOf(Fator);
{<<<<<
  if DS.Struct.Col[IndFator].ColType = dtQuant then
    gOutPut.Warning('Fator '+Fator+' � quantitativo. Regress�o Polinomial'+
                    ' pode ser uma an�lise mais adequada para este fator');
}
  { Encontrar as colunas das vari�veis de grupo }
  Delete(Taxa, Length(Taxa), 1);
  Tx := StrToFloat(SysUtilsEx.AllTrim(Taxa))/100;
  Terms.GetVariances(Variancia, V, DF);

  st  := 'An�lise do Fator '+Fator;
  st1 :='';
  GCols := nil;

  G := nil;
  StringToStrings(Grupo, G);
  if G <> nil then
     GCols := DS.IndexColsFromStrings(G);

  Try
    dsSort(DS, G, True, '');
    L := dsMark(DS, G);
    For j := 0 to L.Count - 1 do
      begin
      FMeans := DS.FacMeans(IndFator, pwsRecBloco(L[j])^.Inicio, pwsRecBloco(L[j])^.Fim, RIdx);
      { Colocar no r�tulo de FMeans os n�veis fixados, se houver. Utilizar os �ndices
        de colunas de G }
      if G <> nil then
        begin
        st1 := ' Grupo: ';
        for k := 1 to GCols.Len do
          begin
          st1 := st1 + DS.ColName[GCols[k]] + ' = ';
          ki := DS.AsInteger[pwsRecBloco(L[j])^.Inicio, GCols[k]];
          st1 := st1 + TwsFactor(DS.Struct.Col[GCols[k]]).LevelNames[ki];
          if k<GCols.Len then st1:=st1+',';
          end;
        end;
      FMeans.Name:=Fator;
      FMeans.MLab := st+st1;

      For k := 1 to NResp do
        FManager.MultRangeTest(Teste, FMeans, 2*(k-1)+1, Tx, V[k], DF[k]);

      FMeans.Free;
      end; { For j }
{<<<<<
    if gLink then
      gOutPut.WriteLink('Veja mais detalhes sobre Compara��es M�ltiplas',
      ExtractFilePath(Application.ExeName)+'Ajuda\Modelos_Lineares\Comparacoes Multiplas.htm');
}
    BlocosFree(L);
  Finally
    G.Free;
    V.Free;
    DF.Free;
    GCols.Free;
  End;
End;

Procedure TwsAnova.Contrasts(Teste, Fator, Grupo, Variancia: String);
{  Objetivo
     Prepara as informa��es necess�rias para que possa ser aplicado um teste para
     contrastes estabelecidos pelo usu�rio
   Par�metros
     Teste    : Teste desejado. As op��es s�o:
                 'F'          - Teste F
                 'Scheffe'    - Teste de Scheffe
                 'Bonferroni' - Teste de Bonferroni
     Fator    : Fator cujos n�veis ser�o comparados
     Grupo    : Vari�veis de grupo para o teste.
     Taxa     : Taxa de erro desejada (em percentagem).
     Variancia: Express�o para o c�lculo da vari�ncia da compara��o. Esta express�o ser�
                tratada pelo m�todo TermList.GetVariances. Note que aqui deve ser indicada
                somente a combina��o de termos envolvida na vari�ncia do contraste. A
                vari�ncia do contraste ser� calculada internamente, levando em considera��o
                os coeficientes de cada contrste definido
   Campos modificados
     Nenhum
   M�todos chamados
     TwsTermList.GetVariances()
     TwsDataSet.FacMeans()
     TwsLMManager.ContrastTest()
   Observa��o
     Caso seja definido um contrate desconhecido ou se n�o huver contraste definido, ser�
     utilizada a matriz de contrates de Helmert.
}
Var i,j,k,ki : Integer;
    DS       : TwsDataSet;
    Y, FMeans: TwsGeneral;
    G        : TStrings;
    L        : TList;
    V, DF    : TwsVec;
    GCols    : TwsLIVec;
    st,st1   : String;
    IndFator : Integer;
    Col      : TwsDataSetCol;
Begin
  DS := FManager.MFrame[FManager.DataIndex];
  GCols := nil;
  IndFator := DS.Struct.IndexOf(Fator);
  Col := DS.Struct.Col[IndFator];
  G := nil;
  StringToStrings(Grupo, G);
  Terms.GetVariances(Variancia, V, DF);
  st := 'M�dias de ' + Fator;
  st1:='';
  if G <> nil then
     GCols := DS.IndexColsFromStrings(G);

  Try
    dsSort(DS, G, True, '');
    L := dsMark(DS, G);
    For j := 0 to L.Count - 1 do
      begin
      FMeans := DS.FacMeans(IndFator,pwsRecBloco(L[j])^.Inicio,pwsRecBloco(L[j])^.Fim,RIdx);
      if G <> nil then
        begin
        st1 := ' Grupo: ';
        for k := 1 to GCols.Len do
          begin
          st1 := st1 + DS.ColName[GCols[k]] + ' = ';
          ki := DS.AsInteger[pwsRecBloco(L[j])^.Inicio, GCols[k]];
          st1 := st1 + TwsFactor(DS.Struct.Col[GCols[k]]).LevelNames[ki];
          if k<GCols.Len then st1:=st1+',';
          end;
        end;

      FMeans.Name := Fator;
      FMeans.MLab := st+st1;
      // Aceita somente contrastes de Helmert ou do usuario
      case TwsFactor(Col).ContrType of
        ctHelm: Y := Helmert(TwsFactor(Col).LevelNames.Count);
        ctUser: begin
              Y := TwsFactor(Col).Contr;
              if Y = nil then // Se nao existe contraste definido, pega o de Helmert
                Y := Helmert(TwsFactor(Col).LevelNames.Count);
              end;
      else
        Y := Helmert(TwsFactor(Col).LevelNames.Count);
      end; { case }

      For k := 1 to NResp do
        FManager.ContrastTest(Teste, FMeans, Y, 2*(k-1)+1, V[k], DF[k]);

      if TwsFactor(Col).ContrType <> ctUser Then Y.Free;
      FMeans.Free;
      end; { For j }

{<<<<<
    if gLink then
      gOutPut.WriteLink('Veja mais detalhes sobre Contrastes',
      ExtractFilePath(Application.ExeName)+'Ajuda\Modelos_Lineares\Contrastes.htm');
}
    BlocosFree(L);
  Finally
    G.Free;
    V.Free;
    DF.Free;
    GCols.Free;
  End;
end;

Procedure TwsAnova.Polinomial(Fator, Grupo, Variancia, DMax: String);
{  Objetivo
     Prepara as informa��es necess�rias para que possa ser realizada a an�lise de regress�o
     polinomial
   Par�metros
     Fator    : Fator cujos n�veis ser�o analisados
     Grupo    : Vari�veis de grupo para o teste.
     Taxa     : Taxa de erro desejada (em percentagem).
     Vari�ncia: Express�o para o c�lculo da vari�ncia da compara��o. Esta express�o ser�
                tratada pelo m�todo TermList.GetVariances. Note que aqui deve ser indicada
                somente a combina��o de termos envolvida na vari�ncia do contraste. A
                vari�ncia do contraste ser� calculada internamente, levando em considera��o
                os coeficientes de ajustamento do polin�mio.
     DMax     : N�vel m�ximo para o polin�mio. Se n�o for definido ou se for maior que o
                n�mero de graus de liberdade do fator, DMax ser� igual a este.
   Campos modificados
     Nenhum
   M�todos chamados
     TwsTermList.GetVariances()
     TwsDataSet.FacMeans()
     TwsLMManager.PolRegTest()
}
Var i,j,k,ki: Integer;
    DS      : TwsDataSet;
    FMeans  : TwsGeneral;
    G       : TStrings;
    L       : TList;
    V, DF, x: TwsVec;
    GCols   : TwsLIVec;
    st,st1  : String;
    IndFator: Integer;
Begin
  DS := FManager.MFrame[FManager.DataIndex];
  GCols := nil;
  IndFator := DS.Struct.IndexOf(Fator);
  If DS.Struct.Col[IndFator].ColType <> dtQuant Then
     Raise Exception.CreateFmt('Fator < %s > deve ser quantitativo', [Fator]);
  Terms.GetVariances(Variancia, V, DF);

  st := 'M�dias de ' + Fator;
  G := nil;
  StringToStrings(Grupo, G);            // obtem as variaveis de grupo
  if G<>nil then
    GCols := DS.IndexColsFromStrings(G);
  st1:='';
  Try
    dsSort(DS, G, True, '');       // ordena
    L := dsMark(DS, G);            // obtem os indices dos grupos
    For j := 0 to L.Count - 1 do   // para cada grupo
      begin
      // Obtem a matriz de medias
      FMeans := DS.FacMeans(IndFator,pwsRecBloco(L[j])^.Inicio,pwsRecBloco(L[j])^.Fim,RIdx);
      // Se possui variavel de grupo
      if G <> nil then
        begin
        st1:=' Grupo: ';
        for k := 1 to GCols.Len do
          begin
          st1 := st1 + DS.ColName[GCols[k]] + ' = ';
          ki := DS.AsInteger[pwsRecBloco(L[j])^.Inicio, GCols[k]];
          st1 := st1 + TwsFactor(DS.Struct.Col[GCols[k]]).LevelNames[ki];
          if k<GCols.Len then st1:=st1+',';
          end; // for k
        end;
      FMeans.Name := Fator;
      FMeans.MLab := st+st1;
      x := TwsQuantitative(DS.Struct.Col[IndFator]).LevelValues;
      // Faz o teste para cada resposta
      For k := 1 to NResp do
        FManager.PolRegTest(FMeans,2*(k-1)+1,x,V[k],DF[k],StrToInt(SysUtilsEx.AllTrim(DMax)));
      FMeans.Free;
      end; // For j

{<<<<<
    if gLink then
      gOutPut.WriteLink('Veja mais detalhes sobre Regress�o Polinomial',
      ExtractFilePath(Application.ExeName)+'Ajuda\Modelos_Lineares\Regressao Polinomial.htm');
}
    BlocosFree(L);
  Finally
    G.Free;
    V.Free;
    DF.Free;
    GCols.Free;
  End;
End;

procedure TwsAnova.PrintCodeMat;
{ Objetivo
    Imprime a matriz de codifica��o utilizada na constru��o da matriz X
  Campos modificados
    Nenhum
  M�todos chamados
    Nenhum
}
var
  i,n: Integer;
  A: TwsGeneral;
begin
  n:= (Terms.Count-2)*RightIdx.Len;
  for i:=0 to n-1 do
      begin
      A:=FIncEnder^[i];
      if A <> nil then
         //TwsOutPut(gOutPut).WriteTable(A)<<<<<
      end
end;

function TwsAnova.XNameCols: TStrings;
{ Objetivo
    Retorna uma lista com os nomes das colunas da matriz X, levando em considera��o os
    termos envolvidos na defini��o do modelo. A lista gerada dever� substituir o campo
    CName.
  Campos modificados
    Nenhum
  M�todos chamados
    Nenhum
}
var
  k,kt: Integer;
  Temp: TStrings;

  { Constroi lista de nomes correspondente ao termo j }
  Function ProcessTerm(j: Integer): TStrings;
  Var
    i,k,kk  : Integer;
    Col     : TwsDataSetCol;
    Names   : TList;
    N       : TStrings;
  Begin
    Names := TList.Create;
    For i := 1 To FIncMat.NRows Do
      Begin
      Col := DSToAnalysis.Struct.ColByName(FIncMat.RowName[i]);
      If Col.ColType <> dtNumeric Then
         kk := TwsFactor(Col).Levels
      Else
         kk := 1;

      If Trunc(FIncMat[i,j]) <> 0 Then
        Begin
        N := TStringList.Create;
        Case Trunc(FIncMat[i,j]) Of
          1: for k := 1 to kk-1 do
               N.Add(Copy(FIncMat.RowName[i],1,4) + IntToStr(k));
          2: for k := 1 to kk do
               N.Add(Copy(FIncMat.RowName[i],1,4) + IntToStr(k));
          3: N.Add(FIncMat.RowName[i]);
        End; { Case IncMat }
        Names.Add(N);
        End; { If IncMat <> 0 }

      End; { For i }
    Result := NameTermKron(Names);
    Names.Free;
  End;{ ProcessTerm }

begin
  Result := TStringList.Create;
  Result.Add('Itcpt');
  for kt := 1 to FIncMat.NCols do
    begin
    Temp := ProcessTerm(kt);
    for k := 0 to Temp.Count-1 do Result.Add(Temp[k]);
    Temp.Free;
    end;
end;

{============================= TwsBAnova ==============================}

Constructor TwsBAnova.Create(Manager: TwsLMManager);
{ Objetivo
    Participa da criacao de uma instancia de uma classe para analise de modelos lineares
    balancceados. Nao pode ser criada individualmente.
 Par�metros
   Manager: Gerenciador de modelos lineares
 Campos modificados
   NReps
   xMat
   Atualiza coeficientes dos termos
 M�todos utilizados
   Create herdado
   GetXMat
   GetOrthEstimate
   Fit
}
var
  i: Integer;
  T: TwsTerm;
Begin
  Inherited Create(Manager);
  NReps  := Trunc(DSToAnalysis[1, DSToAnalysis.NCols]);
  GetXMat;
  GetOrthEstimate;
  For i := 0 to Terms.Count - 3 do
    Begin
    T := Terms.Term[i];
    T.Coef := T.Coef * NReps;
    End;
  Fit
End; {TwsBAnova.Create}


Destructor  TwsBAnova.Destroy;
{ Objetivo
    Libera conte�dos de mem�ria
  Campos liberados
    U
    SSColX
  M�todos chamados
    Destroy herdado
}
Begin
  U.Free;
  SSColX.Free;
  Inherited Destroy;
End; {TwsBAnova.Destroy}

procedure TwsBAnova.Coefficients;
{  Objetivos
     Constr�i um conjunto de dados, que armazena os testes para o contraste correspondente
     a cada coluna da matriz do modelo. Dependendo das op��es, o conjunto de dados gerado
     poder� ser impresso ou salvo na tabela de s�mbolos.
   Campos modificados
     Nenhum
   M�todos chamados
     Nenhum
}
var
  CoefTable: TwsDataSet;
  i,j,k    : Integer;
  v        : TwsVec;
  Erro     : Word;
  T        : TwsTerm;
begin
  T:=Terms.Term[Terms.Count-2]; // residuo

  for k := 1 to NResp do
    begin
    CoefTable := TwsDataSet.Create('Coef',0,0);
    CoefTable.MLab := 'Coeficientes do modelo - Resposta: '+Manager.LResp[k-1];
    CoefTable.ColIdentName := 'Efeitos';
    CoefTable.Struct.AddNumeric('Estimativa', 'Estimativa do coeficiente');       {1}
    CoefTable.Struct.AddNumeric('GL', 'Graus de liberdade',6,5);                  {2}
    CoefTable.Struct.AddNumeric('SQ', 'Soma de quadrados');                       {3}
    CoefTable.Struct.AddNumeric('F', 'Valor da estat�stica F',12,7);              {4}
    CoefTable.Struct.AddNumeric('Valor_p', 'N�vel m�nimo de signific�ncia',11,4); {5}
    CoefTable.Struct.AddQualitative('Sig','Signific�ncia do teste F',5);          {6}
    TwsQualitative(CoefTable.Struct.Col[6]).AddLevel('1%');
    TwsQualitative(CoefTable.Struct.Col[6]).AddLevel('5%');
    TwsQualitative(CoefTable.Struct.Col[6]).AddLevel('NS');
    TwsQualitative(CoefTable.Struct.Col[6]).AddLevel('-');

    v := TwsDFVec.Create(6);
    v.Name := 'Intcp';
    v[1] := Teta[1,k];
    v[2] := wscMissValue;
    v[3] := wscMissValue;
    v[4] := wscMissValue;
    v[5] := wscMissValue;
    v[6] := 3;
    CoefTable.MAdd(v);

    for i := 2 to Teta.NRows do
      begin
      v := TwsDFVec.Create(6);
      v.Name := U.RowName[i-1];
      v[1] := Teta[i,k];
      v[2] := 1;
      v[3] := sqr(U[i-1,k]);
      v[4] := v[3]/T.MSq[nresp];
      v[5] := FInt(v[4],1,DFRes,True,Erro);
      if v[5] < 0.01 then
        v[6] := 0
      else
        if v[5] < 0.05 then
          v[6] := 1
        else
          v[6] := 2;
      CoefTable.MAdd(v)
      end; { for i }
    CoefList.Add(CoefTable);
    //Salva ou imprime coeficientes

{<<<<<
    if Manager.Options[ord(cmVar_PrintCoef)] Then
      TwsOutPut(gOutPut).WriteTable(CoefTable);
}
    if Manager.Options[ord(cmVar_SaveCoef)] Then
      begin
      Manager.TabVar.AddDataSet(CoefTable.Name,CoefTable);
      //SendMessageToObjects(WSPM_UPDATE_STATUS,[0]);
      end
    end; // for k
end; { TwsBAnova.Coefficients }

(*
procedure TwsBAnova.GetXMat;
{ Objetivo
    Cria matriz X para todos os modelos de an�lise da varia��o balanceados
  Campos modificados
    xMat
    SSColX
    U
  M�todos chamados
    GetDF
}
var
  XCols : double;
  i,j,k,
  j1,j2 : Integer;
  x     : TwsVec;
begin
  GetDF(xCols);
  xMat   := TwsGeneral.Create(0, Trunc(xCols) + 1);
  xMat.Name := 'Mat_X';
  xMat.MLab := 'Matriz do modelo';
  xMat.cName.Assign(XNameCols);
  SSColX := VecConst(0, XMat.nCols-1);
  U := TwsGeneral(Jota(xMat.nCols-1,RIdx.Len,mtGeneral,0));
  U.MLab:='Matriz das estimativas ortogonais';
  for i := 1 to U.NRows do
    U.RowName[i] := xMat.ColName[i+1];
  for i := 1 to RIdx.Len do
    U.ColName[i] := DSToAnalysis.Struct.Col[RIdx[i]].Name;
  k:=RIdx.Len;
  U.Name := 'Efeitos';
  For i := 1 to DSToAnalysis.NRows do
    Begin
    x := xLine(i);
    xMat.MAdd(x);
    For k := 2 to x.Len do
      Begin
      j1 := k - 1;
      SSColX[j1] := SSColX[j1] + x[k] * x[k]; // Somas de quadrados das colunas
      End;
    // Obtem a matriz das estimativas ortogonais
    For k := FacIDX.Len + 1 to DSToAnalysis.NCols-1 do
      Begin
      j1 := k-FacIDX.Len;
      For j := 2 to x.Len do
        Begin
        j2 := j - 1;
        U[j2,j1] := U[j2,j1] + x[j] * DSToAnalysis[i,k];
        End; {for j}
      End; {For k}
    End; {For i}
  // Opcao - Matriz do modelo
  if Manager.Options[ord(cmVar_PrintMatMod] then
    TwsOutPut(gOutPut).WriteTable(XMat);

  if Manager.Options[ord(cmVar_EstOrt] then
    TwsOutPut(gOutPut).WriteTable(U);

  if Manager.Options[ord(cmVar_SaveMatMod] then
     begin
     gTabVar.AddMatrix(XMat.Name, XMat);
     SendMessageToObjects(WSPM_UPDATE_STATUS_OBJETOS, [0]);
     end;

  For i := 1 to U.NRows do
    U.Row[i].ByScalar(Sqrt(NReps/SSColX[i]),opProd,False,False);
end;
*)

procedure TwsBAnova.GetOrthEstimate;
{ Objetivo
    Obter a matriz das estimativas ortogonais (U), somas de quadrados das colunas dos termos
    e matriz das estimativas dos par�metros
  Campos modificados
    U
    SSColX
    Teta
}
var
  i,j,
  nw,k: Integer;
  x   : TwsVec;
begin
  // Matriz U para as estimativas ortogonais
  if CovIdx <> nil then nw:=CovIdx.Len else nw:=0;
  U := TwsGeneral(Jota(TermInd.Len,NResp+nw,mtGeneral,0));
  U.Name:='Efeitos';
  U.MLab:='Matriz das estimativas ortogonais';
  for i := 1 to U.NRows do
    U.RowName[i] := xMat.ColName[TermInd[i]];
  for i := 1 to nw do
    U.ColName[i] := DSToAnalysis.Struct.Col[CovIdx[i]].Name;
  j:=nw;
  for i := 1 to NResp do
    U.ColName[j+i] := DSToAnalysis.Struct.Col[RIdx[i]].Name;

  // Vetor para as somas de quadrados das colunas
  SSColX := VecConst(0,TermInd.Len);
  for i:=1 to xMat.NRows do
    begin
    x:=xMat.Row[i];
    // Obtem a matriz U
    for j:=1 to TermInd.Len do
      begin
      SSColX[j]:=SSColX[j]+Sqr(x[TermInd[j]]);// acumula sq de colunas de termos
      for k:=1 to NumInd.Len do
        U[j,k]:=U[j,k]+x[TermInd[j]]*x[NumInd[k]]
      end;
    end; // for i

  U.Copy(mtGeneral, TwsMatrix(Teta));
  Teta.MLab:='Estimativas dos par�metros';
  for i:=1 to U.NRows do
    Teta.Row[i].ByScalar(SSColX[i],opDiv,False,False);

  // Insere a estimativa do intercepto
  x:=Manager.GeneralMean.Copy(1,Manager.GeneralMean.Len);
  x.Name:='Intcp';
  Teta.MInsert(1,x);

  For i := 1 to U.NRows do
    U.Row[i].ByScalar(Sqrt(NReps/SSColX[i]),opProd,False,False);

{<<<<<
  if Manager.Options[ord(cmVar_EstOrt)] then
     TwsOutPut(gOutPut).WriteTable(U)
}
end; // GetOrthEstim

procedure TwsBAnova.GetIncMat;
{ Objetivo
    Este procedimento (virtual)
    * Constroi a matriz de incidencia considerando cada variavel definida e cada termo
      segundo metodologia proposta proposta por Chambers et all (Statistical models in S).
    * Baseado na matriz de incidencia constroi um array com as matrizes de codificacao
      para construcao da matriz de delineamento. A codificacao e feita baseada nos
      contrastes definidos pelo usuario, contrastes pre-definidos (Helmert, polinomios
      ortogonais, definidos pelo usuario adequados ao caso balanceado e variaveis binarias
      para o caso de fatores aninhados
  Campos modificados
    FIncMat
    FIncEnder
  M�todos chamados
    Nenhum
}
var
  i,j,k,ninc: Integer;
  Contido   : Boolean;
  tl        : string;
  P         : TwsGeneral;
  Col       : TwsDataSetCol;

  Function ExisteMatriz(i:Integer;Var P:TwsGeneral):Boolean;
  var
    j: Integer;
  { Como as matrizes de codifica��o se repetem em varias situa��es, esta rotina verifica se
    a matriz P j� existe no endere�o FIncEnder. Se exisitr, P retorna o enedere�o }

    Function ContToStr(ContType: TwsEnumContrType):String;
    Begin
      Case ContType Of
        ctHelm       :Result := 'HELMERT';
        ctPolOrt     :Result := 'POLORT';
        ctUser       :Result := 'USER';
        ctDev        :Result := 'DEVIATION';
        ctExper      :Result := 'EXPERIM';
        ctBin        :Result := 'BIN';
        End;
    End; { ContToStr }

  begin { ExisteMatriz }
   Result := FALSE;
   j:=0;
   while (j <= nInc-1) and (not Result) do
     begin
     With TwsFactor(Col) Do
       If  (FIncEnder^[posi(i,j,FIncMat.NCols)] <> Nil) And
           (LevelNames.Count = TwsGeneral(FIncEnder^[posi(i,j,FIncMat.NCols)]).NRows) Then
           Begin
           Result := TRUE;
           P := FIncEnder^[posi(i,j,FIncMat.NCols)];
           End;
     Inc(j);
    end;
  end; { ExisteMatriz }

begin
  nInc:=Terms.Count - 2;
  FIncMat:=TwsGeneral.Create(RightIdx.Len, nInc);
  FIncMat.MLab:= 'Matriz de incid�ncia das vari�veis nos termos';
  FIncMat.Name:= 'Mat_Incid';
  For i:=0 To nInc-1 Do
    FIncMat.ColName[i+1] := Terms[i];
  // IncEnder armazena os enderecos das matrizes de codificacao
  j:=RightIdx.Len * nInc;
  GetMem(FIncEnder, SizeOf(Pointer)*j);
  For i:=0 To j-1 Do FIncEnder^[i] := Nil;

  // Para cada variavel do lado direito do modelo
  For i := 1 To RightIdx.Len Do
    Begin
    Col := DSToAnalysis.Struct.Col[RightIdx[i]]; {Rochedo, 24/07/1998}
    FIncMat.RowName[i] := UpperCase(Col.Name);
    // Variavel intervalar
    If Col.ColType = dtNumeric Then
       For j := 0 to ninc - 1 do
         Begin
         If Pertence(Col.Name, Terms[j]) Then
            Begin
            FIncMat[i,j+1] := 3;
            FIncEnder^[Posi(i,j,FIncMat.NCols)] := Nil;
            End
         Else
            Begin
            FIncMat[i,j+1] := 0;
            FIncEnder^[Posi(i,j,FIncMat.NCols)] := Nil;
            End
         End
    Else  // Variavel do tipo fator
      For j := 0 to nInc-1 Do
        If Pertence(Col.Name,Terms[j]) Then // Variavel pertence ao termo j?
           Begin
           tl := Del(Terms[j],Col.Name); // t1 e o termo retirado o fator j
           k := 0;
           Contido := FALSE;
           If j = 0 Then
              Begin
              If Length(tl) = 0 Then Contido := TRUE;
              End
           Else
              If Length(tl) = 0 Then
                 Contido := TRUE
              Else // Termo (sem fator j) esta contido no termo k?
                 While (k<=j-1) And (Not Contido) Do
                   Begin
                   Contido := EstaContido(tl,Terms[k]);
                   Inc(k);
                   End;
           // Se estiver
           If Contido Then
               Case TwsFactor(Col).ContrType Of // Estrutura codificacao
                 ctHelm, ctDev, ctExper: // Para qualquer destes contrastes, utiliza Helmert
                   If ExisteMatriz(i,P) Then // Verifica se a matriz j� n�o foi definida
                     Begin
                     FIncMat[i,j+1]:=1;
                     FIncEnder^[Posi(i,j,FIncMat.NCols)]:=P;
                     End
                   Else  // Senao cria uma nova matriz
                     Begin
                     FIncMat[i,j+1]:=1;
                     P:=Helmert(TwsFactor(Col).LevelNames.Count);
                     FIncEnder^[Posi(i,j,FIncMat.NCols)]:=P;
                     End;

                 ctPolOrt: // Polinomios ortogonais
                   If ExisteMatriz(i,P) Then
                     Begin
                     FIncMat[i,j+1]:=1;
                     FIncEnder^[Posi(i,j,FIncMat.NCols)]:=P;
                     End
                   Else
                     Begin
                     FIncMat[i,j+1]:=1;
                     P := PolOrth(TwsQuantitative(Col).LevelValues,
                       TwsQuantitative(Col).LevelValues.Len-1);
                     FIncEnder^[Posi(i,j,FIncMat.NCols)] := P;
                     End;

                 ctUser: // Contrastes definidos pelo usuario
                   If ExisteMatriz(i,P) Then
                     Begin
                     FIncMat[i,j+1]:=1;
                     FIncEnder^[Posi(i,j,FIncMat.NCols)]:=P;
                     End
                   Else
                     Begin
                     FIncMat[i,j+1]:=1;
                     P:=TwsFactor(Col).Contr;
                     FIncEnder^[Posi(i,j,FIncMat.NCols)]:=P;
                     End;
                 End { Case }
           Else { Contido }
             Begin // Faz codificacao por variavel binaria
             FIncMat[i,j+1]:=2;
             P:=Identity(TwsFactor(Col).LevelNames.Count);
             FIncEnder^[Posi(i,j,FIncMat.NCols)]:=P;
             End;
          End // If Pertence
      Else // Se n�o pertence
        Begin
        FIncMat[i,j+1] := 0;
        FIncEnder^[Posi(i,j,FIncMat.NCols)] := Nil;
        End
    End; // For i
end; { GetIncMat }

{=============================== TwsUBAnova ====================================}
Procedure TwsUBAnova.Fit;
{  Objetivo
     Ajusta o modelo de an�lise de varia��o balanceado univariado
   Campos modificados
     TermList
   M�todos chamados
     Coeficients
}
var i       : Integer;
    ErrCode : Word;
    ResSq   : TwsVec;
    T       : TwsTerm;

    Function TermSSq(ic, lc: Integer): TwsVec;
    { Obt�m a soma de quadrados univariadas para o termo cujas colunas em xMat v�o de ic a lc }
    Var i, k: Integer;
        x   : TwsVec;
    Begin
      Result := VecConst(0,U.NCols);
      For i := ic to lc Do
        Begin
        x := U.Row[i];
        For k := 1 to U.NCols Do Result[k]:=Result[k]+x[k]*x[k];
        End;
    End;

Begin
  With Terms do
    Begin
    ResSq  := Term[Count-1].AsVec.Copy(1,U.NCols);
    For i  := 0 to Count - 3 do
      Begin
      T := Term[i];
      T.SetSSq(TermSSq(T.ICol,T.LCol));
      T.MSq := T.AsVec.ByScalar(T.DF,opDiv,True,False);
      // ResSq := ResSq - T.SSq
      ResSq.ByElement(T.AsVec,opSub,False,ErrCode);
      End;
    T     := Term[Count - 2];
    T.SetSSq(ResSq);
    T.Msq := T.AsVec.Copy(1, U.NCols);
    T.MSq := T.MSq.ByScalar(T.DF,opDiv,False,False);
    End;

  Coefficients;
end;

{=============================== TwsMBAnova ====================================}
Destructor TwsMBAnova.Destroy;
{  Objetivo
     Destr�i objeto para an�lise multivariada balanceada
   Campos destru�dos
     ResAdj
   M�todos chamados
     Destroy herdado
}
begin
  ResAdj.Free;
  inherited Destroy
end;

Procedure TwsMBAnova.Fit;
{  Objetivo
     Ajusta o modelo de an�lise de varia��o multivariado de classificacoes balanceadas
   Campos modificados
     Term
   M�todos chamados
     Nenhum
}
var i,k    : Integer;
    ErrCode: Word;
    ResSq,S: TwsSymmetric;
    T      : TwsTerm;
Begin
  With Terms do
    Begin
    // Copia matriz de sq&p total
    Term[Count-1].AsSSP.Copy(mtSymmetric,TwsMatrix(ResSq));
    S:=Term[Count-1].AsSSP;
    S.MLab:='Matriz das SQ&P Total';
    for k:=1 to U.NCols do
      begin
      S.ColName[k]:=U.ColName[k];
      S.RowName[k]:=U.ColName[k];
      end;

    For i  := 0 to Count - 3 do
      Begin
      T := Term[i];
      // matriz de sq&p para o termo i
      T.SetSSq(U.TranspMul7(T.ICol,T.LCol));
      S := T.AsSSP;
      S.MLab:='Matriz de SQ&P para '+Terms[i];
      // reduz matriz encontrada para obter residuo
      ResSq.ByElement(S,opSub,False,ErrCode);
      for k:=1 to U.NCols do
        begin
        S.ColName[k]:=U.ColName[k];
        S.RowName[k]:=U.ColName[k];
        end
      End;

    ResSq.MLab := 'Matriz de SQ&P Res�duo';
    for k:=1 to U.NCols do
      begin
      ResSq.ColName[k]:=U.ColName[k];
      ResSq.RowName[k]:=U.ColName[k];
      end;
    T:=Term[Count-2];
    T.SetSSq(ResSq);
    end;
end;

{====================== TwsUBAncova ===========================}
procedure TwsUBAncova.Fit;
{ Objetivo
    Obt�m as SQ relativas ao ajustamento de um modelo de covari�ncia balanceado univariado.
    A partir da matriz de SQ&P (obtidas como se a an�lise fosse multivariada) ajusta as SQ
    para covari�veis atrav�s do operador Sweep
  Campos modificados
    Terms: s�o retirados os campos relativos �s covari�veis, substitu�dos pelo termo REGRESS�O
  M�todos chamados
    Fit herdado
    TwsSymmetric.RevSweep
    Coefficients
}
var
  T         : TwsTerm;
  i,j,k,
  Old,nc    : integer;
  S,TermAdj : TwsSymmetric;
  ColOp     : TwsLIVec;
  Sq        : TwsVec;
  Err       : Word;
begin
  // Retira os termos correspondentes a cada covariavel
  for i:=1 to CovIdx.Len do
    begin
    k:=Terms.IndexOf(DSToAnalysis.ColName[CovIdx[i]]);
    if k<>-1 then
      begin
      for j:=k+1 to Terms.Count-3 do
        begin
        T:=Terms.Term[j];
        T.ICol:=T.ICol-1;
        T.LCol:=T.LCol-1;
        end;
      Terms.Delete(k)
      end
    end;
  // prepara as matrizes de somas de quadrados e produtos
  inherited Fit;
  // ajustar as matrizes pelo sweep para as covariaveis
  // substituir as matrizes por vetores de SQ ajustadas
  // nao esquecer de incluir o termo de regressao
  ColOp:=TwsLIVec(VecConst(1,CovIdx.Len+NResp,False));

  // S eh a matriz das SQ&P do residuo sem ajustamento para as covariaveis
  S:=Terms.Term[Terms.Count-2].AsSSP;
  nc:=S.NCols;
  S.Copy(mtSymmetric,TwsMatrix(ResAdj));

  // Ajusta para as covariaveis. ResAdj � a matriz das SQ&P do res�duo ajustada
  for i := 1 to CovIdx.Len do
    begin
    Old:=ColOp[i];
    ResAdj.RevSweep(i,1e-9,ColOp);
    // Se retorna o mesmo valor a coluna nao foi operada
{<<<<<
    if Old = ColOp[i] then
      gOutPut.Warning('Coluna '+IntToStr(i)+' nao pode ser operada');
}
    end;

  // Cria termo para regressao
  T:=TwsTerm.Create;
  T.DF:=CovIdx.Len;
  T.Coef:=1;

  T.Effect:=TBits.Create;       // Auxiliar na determinacao do valor esperado
  T.Effect.Size:=FIncMat.NRows;
  T.Fact:=TBits.Create;         // Auxiliar na determinacao do valor esperado
  T.Fact.Size:=FIncMat.NRows;
  T.CTRL:=TBits.Create;         // Auxiliar na determinacao do valor esperado
  T.CTRL.Size:=FIncMat.NRows;
  for i:=0 to FIncMat.NRows-1 do
    begin
    T.Fact[i]:=False;
    T.Effect[i]:=False;
    T.Ctrl[i]:=False;
    end;

  Sq:=TwsDFVec.Create(NResp);
  for i:=1 to NResp do
    begin
    k:=nc-i+1;
    Sq[i]:=S[k,k]-ResAdj[k,k]
    end;
  T.SetSSQ(Sq);
  T.MSq := T.AsVec.ByScalar(T.DF,opDiv,True,False);
  // Insere o termo regressao na lista de termos
  Terms.InsertTerm(Terms.Count-1,'REGRESS�O',T);

  // Ajusta todos os termos para regressao
  for i:=0 to Terms.Count-4 do
    begin
    for k:=1 to ColOp.Len do ColOp[k]:=1;
    T:=Terms.Term[i];                          // termo i
    T.AsSSP.Copy(mtSymmetric,TwsMatrix(TermAdj));
    TermAdj.ByElement(S,opSum,False,Err);      // TermAdj eh o termo i + residuo

    // Ajusta o termo para as covariaveis
    for k:=1 to CovIdx.Len do
      begin
      Old:=ColOp[k];
      TermAdj.RevSweep(k,1e-9,ColOp);
      // se retorna o mesmo valor a coluna nao foi operada
{<<<<<
      if Old=ColOp[k] then
         gOutPut.Warning('Coluna '+IntToStr(k)+' n�o pode ser operada')
}
      end;

    // sq vai armazenar as sq ajustadas para o termo i
    Sq:=TwsDFVec.Create(NResp);
    for j:=1 to NResp do
      begin
      k:=nc-j+1;
      Sq[j]:=TermAdj[k,k]-ResAdj[k,k]
      end;
    TermAdj.Free;
    T.SetSSQ(Sq);
    T.MSq := sq.ByScalar(T.DF,opDiv,True,False);
    end;

  // Residuo
  T:=Terms.Term[Terms.Count-2];
  Sq:=TwsDFVec.Create(NResp);
  for j:=1 to NResp do
    begin
    k:=nc-j+1;
    Sq[j]:=ResAdj[k,k]
    end;
  T.SetSSQ(Sq);
  T.MSq := sq.ByScalar(T.DF,opDiv,True,False);

  // Total nao ajustado
  T:=Terms.Term[Terms.Count-1];
  S:=T.AsSSP;
  Sq:=TwsDFVec.Create(NResp);
  for j:=1 to NResp do
    begin
    k:=nc-j+1;
    Sq[j]:=S[k,k]
    end;
  T.SetSSQ(Sq);
  ColOp.Free;
  Coefficients;
end;

procedure TwsUBAncova.Coefficients;
{  Objetivos
     Constr�i um conjunto de dados, que armazena os testes para o coeficiente correspondente
     a cada covariavel do modelo. Dependendo das op��es, o conjunto de dados gerado
     poder� ser impresso ou salvo na tabela de s�mbolos. As colunas de Teta correspondentes
     �s respostas s�o ajustadas para as covari�veis e a matriz de covari�ncias (sem multiplicar
     pelas vari�ncias) � constru�da.
   Campos modificados
     Teta
     VTeta
   M�todos chamados
     Nenhum
   Sa�da
     No conjunto de dados para sa�da dos coeficientes, o intercepto � colocado em primeiro lugar,
     seguindo os demais coeficientes do modelo e, por �ltimo, aparecem os coeficientes das
     covari�veis. Dependendo das op��es, o conjunto de dados com os coeficientes poder� ser
}
var
  CoefTable: TwsDataSet;
  i,j,k,
  ny,nk    : Integer;
  aux      : double;
  v        : TwsVec;
  Erro     : Word;
  T        : TwsTerm;
begin
  ny:=NResp;    // numero de respostas

  T:=Terms.Term[Terms.Count-2];  // residuo

  { Ao final dos lacos, estimativas dos coeficientes dos termos estarao ajustadas
    para regressao }
  for k:= 1 to ny do           // para cada variavel resposta
    begin
    nk:=nCov+k;
    v:=ResAdj.Row[nk];         // linha com os coeficientes de regressao
    for i:=1 to Teta.NRows do
      begin
      aux:=0;
      for j:=1 to nCov do        // ajusta as estimativas
        aux:=aux+Teta[i,j]*v[j];
      Teta[i,nk]:=Teta[i,nk]-aux
      end; // for i
    end; // for k

  VTeta:= TwsSymmetric.Create(Teta.NRows);
  VTeta[1,1]:=1/NumObs;
  v:=TwsDFVec.Create(nCov);
  for i:=2 to Teta.NRows do
    begin
    for j:=1 to nCov do
      begin
      aux:=0;
      for k:=1 to nCov do
        aux:=aux+Teta[i,k]*ResAdj[k,j];
      v[j]:=aux
      end; // for j
      for j:=2 to i do
        begin
        aux:=0;
        for k:=1 to nCov do
          aux:=aux+v[k]*Teta[j,k];
        VTeta[i,j]:=aux
        end
    end; // for i
  v.Free;

  // para obter as covariancias, deve ser multiplicada pela QMRes da resposta k
  for i:=2 to VTeta.NRows do
    VTeta[i,i]:=VTeta[i,i]+1/(NReps*SSColX[i-1]);

  // Saida dos demais coeficientes do modelo
  for k := 1 to ny do
    begin
    nk:=nCov+k;
    CoefTable := TwsDataSet.Create('Coef',0,0);
    CoefTable.MLab := 'Coeficientes do modelo - Resposta: '+Manager.LResp[k-1];
    CoefTable.ColIdentName := 'Efeitos';
    CoefTable.Struct.AddNumeric(Teta.ColName[nk], 'Valor das estimativas');       {1}
    CoefTable.Struct.AddNumeric('ErrPad', 'Erro padr�o da estimativa');           {2}
    CoefTable.Struct.AddNumeric('t', 'Valor da estat�stica t',12,7);              {3}
    CoefTable.Struct.AddNumeric('Valor_p', 'N�vel m�nimo de signific�ncia',11,4); {4}
    CoefTable.Struct.AddQualitative('Sig','Signific�ncia do teste F',5);          {5}
    TwsQualitative(CoefTable.Struct.Col[5]).AddLevel('1%');
    TwsQualitative(CoefTable.Struct.Col[5]).AddLevel('5%');
    TwsQualitative(CoefTable.Struct.Col[5]).AddLevel('NS');
    TwsQualitative(CoefTable.Struct.Col[5]).AddLevel('-');

    v := TwsDFVec.Create(5);
    v.Name := 'Intcp';
    v[1] := Teta[1,nk];
    v[2] := wscMissValue;
    v[3] := wscMissValue;
    v[4] := wscMissValue;
    v[5] := 3;
    CoefTable.MAdd(v);

    for i := 2 to Teta.NRows do
      begin
      v := TwsDFVec.Create(5);
      v.Name := Teta.RowName[i];
      v[1] := Teta[i,nk];
      v[2] := Sqrt(VTeta[i,i]*T.MSq[k]);
      v[3] := v[1]/v[2];
      v[4] := TInt(v[3],DFRes,False,True,Erro);
      if v[4] < 0.01 then
        v[5] := 0
      else
        if v[4] < 0.05 then
          v[5] := 1
        else
          v[5] := 2;
      CoefTable.MAdd(v)
      end; { for i }
    // para cada covariavel
    for i:=1 to nCov do
      begin
      v := TwsDFVec.Create(5);
      v.Name := DSToAnalysis.Struct.Col[CovIdx[i]].Name;
      v[1] := ResAdj[nk,i];
      v[2] := Sqrt(ResAdj[i,i]*T.MSq[k]);
      v[3] := v[1]/v[2];
      v[4] := TInt(v[3],DFRes,False,True,Erro);
      if v[4] < 0.01 then
        v[5] := 0
      else
        if v[4] < 0.05 then
          v[5] := 1
        else
          v[5] := 2;
      CoefTable.MAdd(v)
      end; { for i }

    CoefList.Add(CoefTable);
{<<<<<
    if Manager.Options[ord(cmVar_PrintCoef)] Then
       TwsOutPut(gOutPut).WriteTable(CoefTable);
}
    if Manager.Options[ord(cmVar_SaveCoef)] Then
      begin
      Manager.TabVar.AddDataSet(CoefTable.Name,CoefTable);
      //SendMessageToObjects(WSPM_UPDATE_STATUS,[0]);
      end
    end; // for k
end; { TwsUBAncova.Coefficients }

{  ========================= TwsNBAnova = Class(TwsAnova) ========================== }

Constructor TwsNBAnova.Create(Manager: TwsLMManager);
{ Objetivo
    Participa da criacao de uma instancia de uma classe para analise de modelos lineares
    nao balancceados. Nao deve ser criada individualmente.
 Par�metros
   Manager: Gerenciador de modelos lineares
 Campos modificados
   ColOp
   Toler
 M�todos chamados
   Create herdado
   GetXMat
   WCrossProduct
   Fit
}
var
  i,j: Integer;
  Col: TwsLIVec;
Begin
  Inherited Create(Manager);
  //Matriz do modelo (X)
  GetXMat;

  // Matriz dos produtos cruzados ajustados para a media. Ultima coluna e o peso (repeticoes)
  Col:=Index(1,xMat.NCols-1);
  wInd:=xMat.NCols;                       // Seta coluna para variavel peso
  WCrossProduct(Col);                     // Matriz de SQ&P ponderados

  Col.Free;
  // Colunas operadas pelo sweep
  ColOp  := TwsLIVec(VecConst(1,FXY.NCols,False));
  ColOp[1]:=-1;
  // Tolerancias para aplicacao do Sweep
  j:=FXY.NCols-NResp;
  Toler:= TwsDFVec.Create(j);
  for i:=1 to j do
    Toler[i]:=FXY[i,i]*1.0e-8;
  // Ajustamento. Obtem graus de liberdade, somas de quadrados, etc., para os termos
  Fit
End; {TwsNBAnova.Create}

procedure TwsNBAnova.GetXMat;
{ Objetivo
    Obt�m a matriz do modelo para os modelos lineares n�o balanceados. A matriz
    ter� uma coluna para:
      Intercepto
      cada termo do modelo
      resposta em an�lise
      n�mero de observa��es de cada combina��o (diferenca em rela��o ao caso balanceado)
   Campos modificados
     xMat
   M�todos chamados
     GetDF
     xLine
   Observa��es
     Dependendo das op��es, a matriz do modelo poder� ser impressa ou salva na tabela de
     s�mbolos
}
var
  xCols: double;
  i,j  : Integer;
  x,aux: TwsVec;
begin
  GetDF(xCols);
  xMat   := TwsGeneral.Create(0, Trunc(xCols)+1);
  xMat.Name := 'Mat_X';
  xMat.MLab := 'Matriz do modelo';
  xMat.cName.Assign(XNameCols);// Obtem nomes para as colunas de X de acordo com os termos
  for j:=1 to RIdx.Len do
    xMat.CName.Add(DSToAnalysis.ColName[RIdx[j]]);
  xMat.CName.Add('_n');
  xMat.NCols:=xMat.NCols+RIdx.Len+1;
  For i := 1 to DSToAnalysis.NRows do
    Begin
    aux := xLine(i);
    // Para copiar a linha de X e as respostas
    x := TwsDFVec.Create(aux.Len+RIdx.Len+1);
    for j:=1 to aux.Len do
      x[j]:=aux[j];                               // Linha de X
    for j:=1 to RIdx.Len do
      x[aux.Len+j]:=DSToAnalysis[i,RIdx[j]];      // Valores das medias das respostas

    x[x.len]:=DSToAnalysis[i,DSToAnalysis.NCols]; // Numero de observacoes

    aux.Free;
//    x.Name:='L'+IntToStr(i);
    xMat.MAdd(x);
    End; {For i}

  // Opcao - Matriz do modelo
{<<<<<
  if Manager.Options[ord(cmVar_PrintMatMod)] then
     TwsOutPut(gOutPut).WriteTable(XMat);
}
  if Manager.Options[ord(cmVar_SaveMatMod)] then
     begin
     Manager.TabVar.AddMatrix(XMat.Name, XMat);
     //SendMessageToObjects(WSPM_UPDATE_STATUS_OBJETOS, [0]);
     end;
end;


procedure TwsNBAnova.GetIncMat;
{ Objetivo
    Este procedimento
    * Constroi a matriz de incidencia considerando cada variavel definida e cada termo
      segundo metodologia proposta proposta por Chambers et all (Statistical models in S).
    * Baseado na matriz de incidencia constroi um array com as matrizes de codificacao
      para construcao da matriz de delineamento. A codificacao e feita baseada nos
      contrastes definidos pelo usuario, contrastes pre-definidos (Helmert, polinomios
      ortogonais, definidos pelo usuario adequados ao caso balanceado e variaveis binarias
      para o caso de fatores aninhados
  Campos modificados
    FIncMat
    FIncEnder
  M�todos chamados
    Nenhum
}
var
  i,j,k,ninc: Integer;
  Contido: Boolean;
  tl: string;
  P: TwsGeneral;
  Col: TwsDataSetCol;

  Function ExisteMatriz(i:Integer;Var P:TwsGeneral):Boolean;
  var j: Integer;

    Function ContToStr(ContType: TwsEnumContrType):String;
    Begin
      Case ContType Of
        ctHelm       :Result := 'HELMERT';
        ctPolOrt     :Result := 'POLORT';
        ctUser       :Result := 'USER';
        ctDev        :Result := 'DEVIATION';
        ctExper      :Result := 'EXPERIM';
        ctBin        :Result := 'BIN';
        End;
    End; { ContToStr }

  begin { ExisteMatriz }
   Result := FALSE;
   j:=0;
   while (j <= nInc-1) and (not Result) do
     begin
     With TwsFactor(Col) Do
       If (FIncEnder^[posi(i,j,FIncMat.NCols)] <> Nil) And
         (LevelNames.Count = TwsGeneral(FIncEnder^[posi(i,j,FIncMat.NCols)]).NRows) Then
         Begin
         Result := TRUE;
         P := FIncEnder^[posi(i,j,FIncMat.NCols)];
         End;
     Inc(j);
    end;
  end; { ExisteMatriz }

begin
  nInc:=Terms.Count - 2;
  FIncMat:=TwsGeneral.Create(RightIdx.Len, nInc);
  FIncMat.MLab:= 'Matriz de incid�ncia das vari�veis nos termos';
  FIncMat.Name:= 'Mat_Incid';
  For i:=0 To nInc-1 Do
    FIncMat.ColName[i+1] := Terms[i];

  // IncEnder armazena os enderecos das matrizes de codificacao
  j:=RightIdx.Len * nInc;
  GetMem(FIncEnder, SizeOf(Pointer)*j);
  For i:=0 To j-1 Do FIncEnder^[i] := Nil;

  // Para cada variavel do lado direito do modelo
  For i := 1 To RightIdx.Len Do
    Begin
    Col := DSToAnalysis.Struct.Col[RightIdx[i]];
    FIncMat.RowName[i] := UpperCase(Col.Name);
    // Variavel num�rica
    If Col.ColType = dtNumeric Then
       For j := 0 to ninc - 1 do
         Begin
         If Pertence(Col.Name, Terms[j]) Then
            Begin
            FIncMat[i,j+1] := 3;
            FIncEnder^[Posi(i,j,FIncMat.NCols)] := Nil;
            End
         Else
            Begin
            FIncMat[i,j+1] := 0;
            FIncEnder^[Posi(i,j,FIncMat.NCols)] := Nil;
            End
         End
    Else  // Variavel do tipo fator
      For j := 0 to nInc-1 Do
        If Pertence(Col.Name,Terms[j]) Then // Variavel pertence ao termo j?
           Begin
           tl := Del(Terms[j],Col.Name); // t1 e o termo retirado o fator j
           k := 0;
           Contido := FALSE;
           If j = 0 Then
              Begin
              If Length(tl) = 0 Then Contido := TRUE;
              End
           Else
              If Length(tl) = 0 Then
                 Contido := TRUE
              Else // Termo (sem fator j) esta contido no termo k?
                 While (k<=j-1) And (Not Contido) Do
                   Begin
                   Contido := EstaContido(tl,Terms[k]);
                   Inc(k);
                   End;
           // Se estiver
           If Contido Then
             begin
             If ExisteMatriz(i,P) Then // Verifica se a matriz j� n�o foi definida
               Begin
               FIncMat[i,j+1]:=1;
               FIncEnder^[Posi(i,j,FIncMat.NCols)]:=P;
               End
             Else  // Senao cria uma nova matriz
               Begin
               FIncMat[i,j+1]:=1;
               P := GetCodeMat(TwsFactor(Col));
               FIncEnder^[Posi(i,j,FIncMat.NCols)]:=P;
               End
             end
           Else { Contido }
             Begin // Faz codificacao por variavel binaria
             FIncMat[i,j+1]:=2;
             P:=Identity(TwsFactor(Col).LevelNames.Count);
             FIncEnder^[Posi(i,j,FIncMat.NCols)]:=P;
             End;
          End // If Pertence
      Else // Se n�o pertence
        Begin
        FIncMat[i,j+1] := 0;
        FIncEnder^[Posi(i,j,FIncMat.NCols)] := Nil;
        End
    End; // For i
end; { GetIncMat }

Procedure TwsNBAnova.GetDF(out glTrat: double);
{ Objetivo
    Obt�m, para cada termo do modelo, os graus de liberdade, coluna inicial e final da
    correspondentes na matriz X, coeficiente do componente de vari�ncia e quantidades
    auxiliares para determina��o dos valores esperados dos quadrados m�dios
  Par�metro
    glTrat: Retorna graus de liberdade de tratamentos
  Campos modificados
    DFRes
    Terms
  M�todos chamados
    Nenhum
  Observa��o
    Esta rotina � a mesma do caso balanceado, com excecao da coluna inicial para operacao
    pelo Sweep. Necessita ser modificada na obtencao dos coeficientes para os valores
    esperados, etc. As instrucoes que tratam dessas questoes estao como comentarios
}
var T,T1      : TwsTerm;   // Termos
    Niveis    : TwsLIVec;  // Numero de n�veis de cada vari�vel
    k,i,i1    : Integer;
    glTotal   : double;     // Graus de liberdade total
    x         : Integer;
    ic        : Word;
    Col       : TwsDataSetCol;
Begin
  glTotal := NumObs - 1;
  DFRes := glTotal;
  glTrat := 0;

  Niveis := TwsLIVec.Create(RightIDX.Len);
  For k := 1 to RightIDX.Len do
    Begin
    Col := DSToAnalysis.Struct.ColByName(FIncMat.Row[k].Name);
    If Col.ColType <> dtNumeric Then
       Niveis[k] := TwsFactor(Col).Levels
    Else
       Niveis[k] := 1;   // Num�rica sao consideradas como tendo um nivel
    end;

  ic := 2;
  // Colunas da matriz de incidencia armazenam os termos definidos
  For k := 1 to FIncMat.NCols do
    Begin
    T        := Terms.Term[k-1];
    T.DF     := 1;                    // Graus de liberdade
    T.ICol   := ic;                   // Coluna inicial em X'X
(*    T.Coef   := 1;                    // Coeficiente do componente de variancia
    T.Effect := TBits.Create;         // Auxiliar na determinacao do valor esperado
    T.Effect.Size := FIncMat.NRows;
    T.Fact   := TBits.Create;         // Auxiliar na determinacao do valor esperado
    T.Fact.Size   := FIncMat.NRows;
    T.CTRL   := TBits.Create;         // Auxiliar na determinacao do valor esperado
    T.CTRL.Size   := FIncMat.NRows;
*)
    For i := 0 to FIncMat.NRows - 1 do
      Begin
      i1 := i+1;
 (*
      T.Fact[i] := False;
      T.Effect[i] := False;
*)
      x := Trunc(FincMat[i1,k]);
(*
      If x <> 0 Then
         Begin
         T.Fact[i] := True;
         If x = 1 Then T.Effect[i] := True;
         End
      Else
        T.Coef := T.Coef * Niveis[i1];

      T.Ctrl[i] := FRand[i] or T.Effect[i];
*)
      If (x <> 0) and (x < 3) Then
         If x = 1 Then
            T.DF := T.DF * (Niveis[i1] - 1)
         Else
            T.DF := T.DF *  Niveis[i1];
      End; {For}
    DFRes  := DFRes  - T.DF;
    glTrat := glTrat + T.DF;
    T.LCol := T.ICol + Trunc(T.DF) - 1;   // Coluna final do termo
    ic     := T.LCol + 1;
    End; {For k}

  T1 := Terms.Term[Terms.Count - 2];     // Residuo
  T1.DF := DFRes;

  T1 := Terms.Term[Terms.Count - 1];     // Total
  T1.DF  := glTotal;
  Niveis.Free;
End;

procedure TwsNBAnova.Coefficients;
{ Objetivo
    Constr�i um conjunto de dados com os coeficientes do modelo e estatisticas relacionadas,
    referentes a cada coluna da matriz do modelo e a todas as vari�veis respostas.
    Dependendo das op��es, imprime e/ou salva o conjunto . Devera ser chamado somente
    depois que o metodo Fit tiver sido executado.
  Campos modificados
    Nenhum
  M�todos chamados
    Nenhum
}
var
   CoefTable: TwsDataSet;
   Col      : TwsDataSetCol;
   v        : TwsVec;
   i,j,p,k  : Integer;
   s2,d     : double;
   Erro     : Word;
begin
  for k:=1 to NResp do
    begin
    p:=FXY.NRows-NResp+k;
    s2:=FXY[p,p]/DFRes;
    CoefTable :=TwsDataSet.Create(FXY.RowName[p]+'_Coef',0,0);
    with CoefTable do
      begin
      MLab:='Coeficientes do modelo: Resposta: '+FXY.RowName[p];
      ColIdentName:='Efeitos';
      Struct.AddColEx(TwsNumeric.Create('Estimativa','Valor estimado dos par�metros',12,7));//1

      Struct.AddColEx(TwsNumeric.Create('E_Padrao','Erro padr�o da estimativa',12,7)); //2

      Struct.AddColEx(TwsNumeric.Create('T','Estat�stica T para hip�tese',10,5));      //3

      Struct.AddColEx(TwsNumeric.Create('Valor_p','Valor p para hip�tese Ho',10,5));   //4

      Col:=TwsQualitative.Create('Sig','Signific�ncia do teste ao n�vel 5% ou 1%',6);  //5
      With TwsQualitative(Col) Do
        Begin
        AddLevel('NS');
        AddLevel('5%');
        AddLevel('1%');
        End; { With Col }
      Struct.AddColEx(Col);

      for j:=1 to ColOp.Len-NResp do
        begin
        v := TwsDFVec.Create(5);
        v.Name := FXY.ColName[j];
        d:=FXY[j,j];
        // Se a variavel j foi incluida insere informacao sobre coeficiente
        if d>1.0e-9 then                    // Se o valor da diagonal nao e muito pequeno
          Begin
          v[1]:=FXY[p,j];                          // Coeficiente de regressao
          v[2]:=SQRT(d*s2);                        // Erro padrao do coeficiente
          v[3]:=v[1]/v[2];                         // Valor de T calculado
          v[4]:=TInt(v[3],DFRes,False,True,Erro);  // Prob. associada
          If v[4]>=0.05 Then                       // Verifica significancia
            v[5]:=0
          Else
            If v[4]>=0.01 Then
              v[5]:=1
            Else
              v[5]:=2;
          End { if ColOp }
          else
            begin
            v[1]:=0;
            for i:=2 to v.Len do
              v[i]:=wscMissValue
            end;
          MAdd(v);
          End; { if ColOp }
      CoefList.Add(CoefTable);
{<<<<<
      if Manager.Options[ord(cmPrintCoef)] Then
         TwsOutPut(gOutPut).WriteTable(CoefTable);
}
      if Manager.Options[ord(cmSaveCoef)] Then
        begin
        Manager.TabVar.AddDataSet(CoefTable.Name,CoefTable);
        //SendMessageToObjects(WSPM_UPDATE_STATUS, [0]);
        end
      end { with CoefTable }
    end
end; { TNBAnova.Coefficients }

{-------------  Analise da variacao nao balanceada completa univariada ---------------}

Procedure TwsUNBAnova3.Fit;
{ Objetivo
    Obtem somas de quadrados do tipo III para os termos do modelo, incluindo o residuo.
    Apropriado para classficacoes nao balanceadas mas completas, isto e, possui observacoes
    para todas as combinacoes mas o numero de repeticoes e diferente
  Campos modificados
    Terms
    FXY
  M�todos chamados
    RevSweep
    Coefficientes (op��es)
}
var i,p     : Integer;
    ResSq   : TwsVec;
    T       : TwsTerm;

    Function TermSSq(ic, lc: Integer): TwsVec;
    var
      i, Old: Integer;
    begin
      Result:=TwsDFVec.Create(NResp);
      // Obtem o residuo condicional
      for i:=ic to lc do
        begin
        Old := ColOp[i];
        FXY.RevSweep(i,Toler[i],ColOp);
{<<<<<
        if Old = ColOp[i] then
          gOutPut.Warning('Coluna '+IntToStr(i)+' nao pode ser operada');
}
        end;
      // Obtem a soma de quadrados do termo por diferenca
      for i:=1 to NResp do
        Result[i]:=FXY[p+i,p+i]-ResSq[i];

      // Retorna a matriz original
      for i:=ic to lc do
        FXY.RevSweep(i,Toler[i],ColOp);
    end; // TermSSq
Begin
  p := FXY.NCols - NResp;
  for i:= 1 to NResp do
    FXY[p+i,p+i]:=Terms.Term[Terms.Count-1].AsSSq[i];

  // Muda o sinal da primeira coluna para compensar o inicio como SQ&P ajustada
  for i := 2 to FXY.NRows do FXY[i,1] := -FXY[i,1];

  // Opera para obter soma de quadrados do residuo
  for i := 2 to p do
    FXY.RevSweep(i,Toler[i],ColOp);
  ResSq  := TwsDFVec.Create(NResp);

(*  if Manager.Options[ord(cmVar_PrintCoef)] or Manager.Options[ord(cmVar_SaveCoef)] Then*)
  Coefficients;

  for i:=1 to NResp do           // Obtem SQ Res da diagonal da submatriz
    ResSq[i]:=FXY[p+i,p+i];

  With Terms do
    Begin
    // Somas de quadrados para cada termo
    For i  := 0 to Count - 3 do
      Begin
      T := Term[i];
      T.SetSSq(TermSSq(T.ICol, T.LCol));
      T.MSq := T.AsVec.ByScalar(T.DF,opDiv,True,False);
      End;
    // Residuo
    T     := Term[Count - 2];
    T.SetSSq(ResSq);
    T.Msq := ResSq.Copy(1, NResp);
    T.MSq := T.MSq.ByScalar(T.DF,opDiv,False,False);
    End;

  // Compensa troca de sinal da 1a. coluna das preditoras
  for i := 2 to p do FXY[i,1] := ColOp[i]*FXY[i,1];
end;

function TwsUNBAnova3.GetCodeMat(C: TwsFactor): TwsGeneral;
{ Objetivo
    Obt�m a matriz de codifica��o adrequada para modelos n�o balanceados. Neste caso
    estabelece como matriz de codifica��o, o resultado da fun��o Deviation(), ou seja,
    se n � o n�mero de n�veis em quest�o, retorna uma matriz de dimens�o n x (n-1) onde na
    coluna i (i=1, ..., n-1), a posi��o i vale 1 e a �ltima vale -1
  Campos modificados
    Nenhum
  M�todos chamados
    Nenhum
}
begin
  Result:=Deviation(C.LevelNames.Count);
end;

{-------------  Analise da variacao nao balanceada incompleta univariada ---------------}

Procedure TwsUNBAnova1.Fit;
{ Objetivo
    Obtem somas de quadrados do tipo I (sequencial) para os termos do modelo, incluindo o
    residuo. Apropriado para classificacoes nao balanceadas incompletas, isto e, que nao
    possui observacoes para todas as combinacoes
  Campos modificados
    FXY
    ColOp
    Terms
  M�todos chamados
    Coefficients
    RevSweep
}
var i,p     : Integer;
    ResSq   : TwsVec;
    T       : TwsTerm;

    Function TermSSq(ic, lc: Integer): TwsVec;
    var
      i, Old: Integer;
    begin
      Result:=TwsDFVec.Create(NResp);
      // Obtem o residuo condicional
      for i:=ic to lc do
        begin
        Old := ColOp[i];
        FXY.RevSweep(i,Toler[i],ColOp);
        if Old = ColOp[i] then
          begin
          //gOutPut.Warning('Coluna '+IntToStr(i)+' nao pode ser operada');<<<<<
          ColOp[i]:=-ColOp[i] // Troca o sinal
          end
        end;
      // Obtem a soma de quadrados do termo por diferenca
      for i:=1 to NResp do
        Result[i]:=FXY[p+i,p+i]-ResSq[i];
      // Retorna a matriz original
      for i:=1 to ResSq.Len do
        ResSq[i]:=FXY[p+i,p+i];
    end; // TermSSq

Begin
  p := FXY.NCols - NResp;

  for i:= 1 to NResp do
    FXY[p+i,p+i]:=Terms.Term[Terms.Count-1].AsSSq[i];

  // Muda o sinal da primeira coluna para compensar o inicio como SQ&P ajustada
  for i := 2 to FXY.NRows do FXY[i,1] := -FXY[i,1];

  // Opera para obter soma de quadrados do residuo
  for i := 2 to p do
    FXY.RevSweep(i,Toler[i],ColOp);

(*  if Manager.Options[ord(cmVar_PrintCoef)] or Manager.Options[ord(cmVar_SaveCoef)] Then*)
  Coefficients;

  // Vai armazenar as somas de quadrados do residuo condicional, isto e, quando o termo e
  // retirado do modelo
  ResSq  := TwsDFVec.Create(NResp);
  for i:=1 to NResp do                   // Obtem SQ Res da diagonal da submatriz
    ResSq[i]:=FXY[p+i,p+i];
  With Terms do
    begin
    T     := Term[Count - 2];
    T.SetSSq(ResSq.Copy(1, NResp));
    T.Msq := T.AsVec.Copy(1, NResp);
    T.MSq := T.MSq.ByScalar(T.DF,opDiv,False,False);
    // Somas de quadrados para cada termo, comecando pelo ultimo
    For i  := Count - 3 downto 0 do
      Begin
      T := Term[i];
      T.SetSSq(TermSSq(T.ICol, T.LCol));
      T.MSq := T.AsVec.ByScalar(T.DF,opDiv,True,False);
      End;
    end; // with

  // Compensa troca de sinal da 1a. coluna das preditoras
  for i := 2 to p do FXY[i,1] := ColOp[i]*FXY[i,1];
end;

function TwsUNBAnova1.GetCodeMat(C: TwsFactor): TwsGeneral;
{ Objetivo
    Obt�m a matriz de codifica��o adrequada para modelos n�o balanceados incompletos. Neste
    caso estabelece como matriz de codifica��o, o resultado da fun��o Experim(), ou seja,
    se n � o n�mero de n�veis em quest�o, retorna uma matriz de dimens�o n x (n-1) onde na
    coluna i (i=1, ..., n-1), a posi��o i vale 1 e nas demias vale 0.
  Campos modificados
    Nenhum
  M�todos chamados
    Nenhum
}
begin
  Result:=Experim(C.LevelNames.Count);
end;

{========================== TwsLMManager Class ===============================}

Constructor TwsLMManager.Create(aDataSet: TwsDataSet; Const Model, Group, Condition, WV: String);
{ Objetivo
    Cria um objeto gerenciador de modelos lineares
  Par�metros
    aDataSet : Conjunto de dados para an�lise
    Model    : Express�o do modelo linear
    Group    : Vari�veis de grupo
    Condition: Condi��o (filtro) para sele��o de observa��es
    WV       : Vari�vel peso
  Campos modificados
    Error
    Options
    FOutPut
    FDataSet
    FFormula
    FWeightVar
    FGroups
    FCondition
    Graf_Reg
  M�todos chamados
    Create herdado
    gOutPut.InitDoc
}
Begin
  Inherited Create;
  Error := 0;
  Options      := TBits.Create;
  Options.Size := 80;            // O vetor se dimensionaliza automaticamente
  FDataSet     := aDataSet;
  FFormula     := Model;
  FWeightVar   := WV;
  FGroups      := Group;
  FCondition   := Condition;
  Graf_Reg     := TGraf_ModeloDeReg.Create(nil);
  //gOutPut.InitDoc;       // Inicializa arquivo de saida <<<<<
End;

Procedure TwsLMManager.ClearStage1;
Begin
(*  If Assigned(FDataSet) Then
//    FDataSet.Free
    FFDataSet.DestroyTempDataSet; {Rochedo, 02/06/1998} *)
End;

Procedure TwsLMManager.ClearStage2;
{ Objetivo
    Libera mem�ria ocupada durante o est�gio 2 da defini��o dos modelos
  Estruturas liberadas
    FFacIdx
    FCovIdx
    FRightIdx
    FNumIdx
    FRIdx
    LResp
    GMeanList
    SSList
    FListFrame
    FTerms
}
var i: integer;
Begin
  If Assigned (FFacIdx) Then
     Begin
     FacIdx.Free;
     FacIdx := nil;
     End;

  If Assigned (FCovIdx) Then
     begin
     CovIdx.Free;
     CovIdx := nil;
     end;

  If Assigned (FRightIdx) Then
     begin
     FRightIdx.Free;
     FRightIdx := nil;
     end;

  If Assigned (FNumIdx) Then
     Begin
     FNumIdx.Free;
     FNumIdx := nil;
     End;

  If Assigned (FRIdx  ) Then
     Begin
     FRIdx.Free;
     FRIdx := nil;
     End;

  If Assigned (LResp  ) Then
     Begin
     LResp.Free;
     LResp := nil;
     End;

  If Assigned (GMeanList) Then { <===== Ver }
     Begin
     if not Options[ord(cmVar_SaveMedias)] then
       For i := 0 To GMeanList.Count - 1 Do
         TwsDataSet(GMeanList.Items[i]).Free;
     GMeanList.Free;
     GMeanList := nil;
     End;

  If Assigned (SSList) Then { <===== Ver }
     Begin  // univariado sem covariancia
     If FUnivariado and (FCovIdx=nil) Then
       for i:=0 to SSList.Count-1 do
         TwsVec(SSList.Items[i]).Free
     else
       for i:=0 to SSList.Count-1 do
         TwsSymmetric(SSList.Items[i]).Free;
     SSList.Free;
     SSList := nil;
     End;

  If Assigned (FListFrame) Then
     Begin
     if not Options[ord(cmVar_SaveDadosMod)] then
       For i := 0 To FListFrame.Count - 1 Do
         FListFrame[i].Free;
     FListFrame.Free;
     FListFrame := nil;
     End;

  If Assigned (FTerms) Then
     Begin
     FTerms.Free;
     FTerms := nil;
     End;
End; { TwsLinearModel.ClearStage2 }

Procedure TwsLMManager.ClearStage3;
{ Objetivo
    Libera mem�ria ocupada no terceiro est�gio da defini��o do modelo linear
  Estruturas liberadas
    FModel
}
Begin
  If Assigned (FModel) Then
     Begin
     FModel.Free;
     FModel := nil;
     End;
End;

Destructor TwsLMManager.Destroy;
{ Objetivo
    Libera mem�ria ocupada oelos campos de TwsLMManager
  Estruturas liberadas
    LVar
    Options
    Graf_Reg
  M�todos chamados
    ClearStage1
    ClearStage2
    ClearStage3
    Destroy herdado
}
Begin
  ClearStage3;
  ClearStage2;
  ClearStage1;
  LVar.Free; //estava em EvalModel 17/11/99

  Options.Free;
  Graf_Reg.Free;
  Inherited Destroy;
End; { TwsLMManager.Destroy }

Function  TwsLMManager.GetModel: TwsLinearModel;
{ Objetivo
    Retorna modelo linear em utiliza��o. Gera uma exce��o se ainda n�o foi definido
  Campos modificados
    Nenhum
  M�todos chamados
    Nenhum
}
Begin
  If Assigned(FModel) Then
     Result := FModel
  Else
     Raise Exception.Create('Modelo ainda n�o dispon�vel');
End;

Function  TwsLMManager.GetGMean: TwsDFVec;
{ Objetivo
    Retorna o vetor de m�dias gerais das vari�veis num�ricas para o conjunto de dados atual.
  Campos modificados
    Nenhum
  M�todos chamados
    Nenhum
}
Begin
  Result:=TwsDFVec(GMeanList[DataIndex])
end; // GetGMean

{  Objetivo

   Par�metros

   Campos modificados

   M�todos chamados

}


Function TwsLMManager.GetNData: Word;
{  Objetivo
     Retorna o n�mero de conjuntos de dados em an�lise, ou seja, o n�mero de conjuntos de
     dados resultantes na lista FListFrame gerada pelo m�todo ModelFrame
   Campos modificados
     Nenhum
   M�todos chamados
     Nenhum
}
Begin
  Result := FListFrame.Count;
End; { TwsLMManager.GetNData }

Function TwsLMManager.GetMFrame(i: Integer): TwsDataSet;
{  Objetivo
     Endere�o do conjunto de dados da lista FListFrame
   Par�metros
     �ndice da lista. Baseado em zero
   Campos modificados
     Nenhum
   M�todos chamados
     Nenhum
   Exce��o
     Gera uma exce��o se o �ndice for mal definido
}
Begin
  Try
    Result := FListFrame[i];
  Except
    Raise Exception.CreateFmt(
      'Classe: TwsLMManager.' + #10#13 +
      'Propriedade: MFrame (�ndice: %d).' + #10#13 +
      'Erro: �ndice inv�lido.', [i]);
  End;
End; { TwsLMManager.GetMFrame }

(*Function TwsLMManager.GetMM(I: Integer): TwsDataSet;
Begin
  Try
    Result := TwsDataSet(FMeanFrame[i]);
  Except
    Raise Exception.CreateFmt(
      'Classe: TwsLMManager.' + #10#13 +
      'Propriedade: MeanFrame (�ndice: %d).' + #10#13 +
      'Erro: �ndice inv�lido.', [i]);
  End;
End; {TwsLMManager.GetMM}
*)

{ Objetivo
 }
Procedure TwsLMManager.EvalModel;
{  Objetivo
     Avalia a f�rmula do modelo e obt�m quantidades necess�rias aos objetos de todas as
     classez de modelos lineares. As a��es principais s�o:
      * Avalia o modelo atrav�s da rotina EvalModel,
      * Constr�i �ndices de vari�veis respostas, covari�veis e fatores,
      * Atrav�s da rotina ModelFrame quebra conjuntos de dados estabelecidos pelas vari�veis
        de grupo, gera novas vari�veis definidas no modelo e aplica filtro �s observa��es
      * obt�m o tipo basico de analise (regress ou anova),
      * Para modelos de an�lise da varia��o
         * obt�m os conjuntos de dados com as m�dias de subclasses
         * obt�m m�dias gerais das respostas (anova)
   Campos modificados
     FTerms
     LResp
     FListFrame
     LVar
     FRIdx
     FNumIdx
     FCovIdx
     FFacIdx
     FRightIdx
     DSMeanList
     SSList
     GMeanList
     AnalysisType
   M�todos chamados
     GetRespVars
     FTerms.DefineAlias
     AuxDS.SortRows
     AuxDS.KeyGroupMeans
}

Var ListVar,                 // Lista de todas as vari�veis
    G       : TStrings;
    i,J     : Integer;
    XRows   : Integer;
    SSq     : TObject;
    T       : TwsTerm;       // Termos vazios a adicionar
    VecOrdem: TwsLIVec;      // Vetor auxilar de ordena��o
    M,                       // Auxiliar para obtencao das medias
    GMean:    TwsVec;        // Media geral
    DSMean,
    AuxDS   : TwsDataSet;
    st      : String;
Begin
  FillChar(T, SizeOf(TwsTerm), 0);
  Self.Error := 0;

  G      := nil;
  FTerms := nil;
  LResp   := Self.GetRespVars;

  Try
    // <<<<< verificar repeticao ou indefinicao de nomes nas variaveis tipo expressao
    wsAvaliadorDeModelosLineares.EvalModel(FFormula, TStrings(FTerms), LVar, DataSet, Options[ord(cmOrder)], FTV);
  Except
    On E: Exception do
       begin
       If Assigned(FTerms) Then
          begin
          FTerms.Free;
          FTerms := nil;
          end;
       MessageDLG('Erro na avalia��o do Modelo'#13 +E.Message,mtError,[mbOk],0);
       Error:= 1;
       Exit; //Abort;
       end;
  End;

  ListVar := TStringList.Create; {Local}

  Try
    // define por default o Teste F
    for i := 0 to FTerms.Count-1 do
      FTerms.Term[i].FTest := True;
    FTerms.AddTerm('Residuo', TwsTerm.Create);
    FTerms.AddTerm('Total', TwsTerm.Create);

    // Redefine os Alias
    FTerms.DefineAlias;

    For i := 0 To LVar.Count-1 Do
      ListVar.Add(LVar[i]);

    For i := 0 To LResp.Count-1 Do ListVar.Add(LResp[i]);
    If FWeightVar <> '' Then
      begin
      ListVar.Add(FWeightVar);
      FWeightVar:=PegaNome(FWeightVar)
      end;
    G := StrToStrList(FGroups);

    try
      FListFrame := ModelFrame(ListVar, G, FCondition, FDataSet);
    except
      on e: Exception do
         begin
         ShowMessage(e.Message);
         Error := 1;
         exit;
         end;
    end;

    for i := 0 to LVar.Count-1 do
      LVar[i] := PegaNome(LVar[i]);

    st:='';
    for i := 0 to LResp.Count-1 do
      begin
      LResp[i] := PegaNome(LResp[i]);
      st := st+' '+LResp[i]
      end;
    st:=st+' ~ ';

    for i := 0 to FListFrame.Count-1 do
      TwsDataSet(FListFrame[i]).Name := FDataSet.Name+'_'+IntToStr(i+1);

    For i := 0 To ListVar.Count-1 Do
      ListVar[i] := PegaNome(ListVar[i]);

    FRIdx     := GetIndexResp(LResp);              { Indices das variaveis respostas }
    FFacIdx   := GetIndexByType(LVar, dtFactors);    { Indices dos fatores             }
    FCovIdx   := GetIndexByType(LVar, dtNumeric);    { Indices das covariaveis         }
    FRightIdx := GetIndex(LVar);                   { Indices das variaveis a direita }
    FNumIdx   := GetIndexByType(ListVar, dtNumeric); { Indices das variaveis numericas }

    if (FFacIdx<>nil) then
      begin
      AnalysisType := mtAnova;
      SSList := TList.Create;
      GMeanList := TList.Create;
      DSMeanList := TList.Create;
      //Se possui algum conjunto ordena-os em relacao as vari�veis Fatores (FFacIdx)
      if FListFrame.Count > 0 then
         begin
         VecOrdem := TwsLIVec(VecConst(1,FFacIdx.Len,False));
         for i := 0 to FListFrame.Count-1 do
           begin
           AuxDS := TwsDataSet(FListFrame[i]);
           AuxDS.SortRows(FFacIdx, VecOrdem); {Ordena cada conjunto}
           end;
         VecOrdem.Free;
         end;

      // Para cada conjunto de dados criado por MFrame
      for i := 0 to FListFrame.Count-1 do
        Begin
        AuxDS := TwsDataSet(FListFrame[i]);
        {Teste para ver se � vetor ou matriz.
         Se for multivariada ou an�lise de covari�ncia ent�o � matriz. }
        If FUnivariado and (FCovIdx=nil) Then
           SSq := VecConst(0,FNumIDx.Len)    // vetor de somas de quadrados (univariado)
        Else
           SSq := Jota(FNumIdx.Len,FNumIdx.Len,mtSymmetric,0);  // matriz de sq&p (multivariado)

        GMean := VecConst(0,FNumIDx.Len);
        DSMean := TwsDataSet.Create('M_'+AuxDS.Name, 0, 0);
        DSMean.MLab := 'M�dias de Caselas. Modelo: '+st+FFormula;
        {Cria��o dos descritores}
        For j := 1 to FFacIDx.Len do
          DSMean.Struct.AddCopyCol(AuxDS.Struct.Col[FFacIdx[j]]);

        For j := 1 to FNumIDx.Len do
          DSMean.Struct.AddCopyCol(AuxDS.Struct.Col[FNumIDx[j]]);

        DSMean.Struct.AddNumeric('_n', 'N�mero de observa��es para m�dia',7,0);

        XRows := 1; ValidObs := 0;

        M := AuxDS.KeyGroupMeans(FFacIDx,FNumIDx,GMean,SSq,XRows,ValidObs,
          FCovIdx <> nil,FUnivariado);
        // M[M.Len] guarda o numero de repeticoes
        If (M <> Nil) and (M[M.Len]>0) Then DSMean.MAdd(M);
        While M <> Nil Do
          Begin
          M := AuxDS.KeyGroupMeans(FFacIDx,FNumIDx,GMean,SSq,XRows,ValidObs,
            FCovIdx<>nil,FUnivariado);
          If (M <> Nil) and (M[M.Len]>0) Then DSMean.MAdd(M);
          End; { While }
        DSMeanList.Add(DSMean);
        // Multivariado ou covariancia, SSList eh uma lista de matrizes simetricas
        SSList.Add(SSq);
        GMeanList.Add(GMean);
        end; { for i }
      end { if FFacIdx }
    else
      AnalysisType := mtRegression;

  Finally
    // Revisado em 06/01/1999 by Rochedo
    If Assigned(G) Then
       G.Free;
    ListVar.Free;
  End;
End; {TwsLMManager.EvalModel}

procedure TwsLMManager.SetAnovaClass;
{ Objetivo
    Dado um modelo de an�lise da varia��o (modelo com pelo menos um fator), determina a
    classe mais adequada para an�lise desse modelo e cria uma inst�ncia dessa classe.
  Campos modificados
    FClassType
    FModel
  M�todos chamados
    GetClassType
    Create para o modelo apropriado
}
begin
  FClassType := GetClassType(DSMeanList[DataIndex]);

  Case FClassType of
     mtUnivBalAnova:     { Anova univariada balanceada }
       begin
       FModel := TwsUBAnova.Create(Self);
       stClass := 'An�lise da varia��o univariada balanceada'
       end;

     mtUnivCompAnova:    { Anova univariada completa }
       Begin
       FModel := TwsUNBAnova3.Create(Self);
       stClass := 'An�lise da varia��o univariada n�o balanceada completa'
       End;

     mtUnivBalAncova:    { Ancova univariada balanceada }
       Begin
       stClass:='An�lise da covaria��o univariada balanceada';
       FModel:=TwsUBAncova.Create(Self);
       if Options[ord(cmVar_MMQ)] then
          TwsUBAncova(FModel).LSMeans;
       End;

     mtUnivCompAncova:  { Ancova univariada completa }
       Begin
       End;

     mtUnivIncompAnova:  { Anova univariada incompleta }
       Begin
       FModel := TwsUNBAnova1.Create(Self);
       stClass := 'An�lise da varia��o univariada n�o balanceada incompleta'
       End;

     mtUnivIncompAncova: { Ancova univariada incompleta }
       Begin
       End;

     mtMultBalAnova:     { Anova multivariada balanceada }
       Begin
       FModel:=TwsMBAnova.Create(Self);
       stClass:='An�lise da varia��o multivariada balanceada';
       End;

     mtMultBalAncova:    { Ancova multivariada balanceada }
       Begin
       FModel := nil;
       End;

     mtMultCompAnova:   { Anova multivariada nao balanceada }
       Begin
       FModel := nil;
       End;

     mtMultCompAncova:  { Ancova multivariada nao balanceada }
       Begin
       FModel := nil;
       End;

     mtMultIncompAnova:  { Anova multivariada incompleta }
       Begin
       FModel := nil;
       End;

     mtMultIncompAncova: { Ancova multivariada incompleta }
       Begin
       FModel := nil;
       End;
  end { case }
end; { SetAnovaClass }

{**}
Procedure TwsLMManager.DiscrimVarTreat(mrTab, coTab, poTab: TdrStringAlignGrid);
{ Objetivo
    Executa os procedimentos de discrimina��o da varia��o de tratamentos
  Par�metros
    mrTab: Cont�m a informa��o necess�ria aos procedimentos de compara��o m�ltipla
    coTab: Cont�m a informa��o necess�ria aos procedimentos de an�lise de contrastes
    poTab: Cont�m a informa��o necess�ria aos procedimentos para an�lise de regress�o
           polinomial
  Campos modificados
    Nenhum
  M�todos chamados
    MultRange
    Contrasts
    Polinomial
}
var
  i: Integer;
Begin
  If mrTab.Cells[0, 1] <> '' Then { Se foi definido algum teste de comparacoes multiplas }
     For i := 1 to mrTab.RowCount - 1 do
       With mrTab do
         TwsAnova(FModel).MultRange
           (
           Cells[0, i], {Teste}
           Cells[1, i], {Fator}
           Cells[2, i], {Grupos}
           Cells[3, i], {Taxa}
           Cells[4, i]  {Vari�ncia}
           );

  If coTab.Cells[0, 1] <> '' Then  { Se foi definido algum teste para contrastes }
     For i := 1 to coTab.RowCount - 1 do
       With coTab do
         TwsAnova(FModel).Contrasts
           (
           Cells[0, i], {Teste}
           Cells[1, i], {Fator}
           Cells[2, i], {Grupo}
           Cells[3, i]  {Vari�ncia}
           );

  If poTab.Cells[0, 1] <> '' Then { Se foi definida regressao polinomial }
     For i := 1 to poTab.RowCount - 1 do
       With poTab do
         TwsAnova(FModel).Polinomial
           (
           Cells[0, i], {Fator}
           Cells[1, i], {Grupo}
           Cells[2, i], {Vari�ncia}
           Cells[3, i]  {Grau M�ximo}
           );
End;

Procedure TwsLMManager.ExecMReg(C,W: TwsGeneral; Desc: String; Alpha: double= 0.05);
{ Objetivo
    Executa a an�lise de regress�o multivariada
  Par�metros
    C: Matriz que define a combina��o dos par�metros para o teste da hip�tese linear
       multivariada e/ou para os intervalos de confian�a univariados
    W: Matriz que define a combina��o das vari�veis respostas para o teste da hip�tese
       linear multivariada e/ou para os intervalos de confian�a univariados
    Desc: Descri��o da hip�tese
    Alpha: Taxa de erro do tipo I para os testes. Default = 0,05
  Campos modificados
    Error
    FModel
  M�todos chamados
    HTest
    SConfInterv
}
Begin
  Error:= 0;
  FModel:= TwsMReg.Create(Self);
  if Options[ord(cmHipGeralCBWImp)] or Options[ord(cmHipGeralCBWSal)] then
    TwsMReg(FModel).HTest(C,W,Error);
  if Error = 0 then
    if Options[ord(cmInterConfiancaImp)] or Options[ord(cmInterConfiancaSal)] then
      TwsMReg(FModel).SConfInterv(C,W,Alpha,Error)
  else
    Begin
    //pensar como tratar
    End;
End;

procedure TwsLMManager.ExecReg(MSelect: Integer; ialpha, oalpha: double; LinHyp: TwsGeneral);
{ Objetivo
    Executa a an�lise de regress�o univariada. Cria inst�ncia, ajusta modelo, faz an�lise
    de res�duos e influ�ncia e possibilita os gr�ficos mais comuns na an�lise de regress�o.
    Os modelos dispon�veis s�o
      * Regress�o univariada modelo completo
      * Regress�o univariada passo a passo, m�todo stepwise
      * Regress�o univariada passo a passo, backward elimination
      * Regress�o univariada todas as regress�es poss�veis
      * Regress�o univariada passo a passo, m�todo forward selection
      * Regressao univariada ponderada
  Par�metros
    MSelect: M�todos de sele��o de vari�veis
    ialpha,
    oalpha : Probabilidades de entrada e sa�da do modelo, respectivamente. Relativas
             aos m�todos de sele��o de vari�veis.
    LinHyp : Matriz, cujo n�mero de colunas deve coincidir com o n�mero de par�metros,
             cont�m em cada linha coeficientes para uma combina��o linear de par�metros a
             ser testada. Se LinHyp for nil o m�todo LinearHypothesis n�o � chamado.
  Campos modificados
    FModel
    DataIndex
    FClassType
  M�todos chamados
    GetClassType
    Create (para o modelo adequado
    Coefficients
    AnovaTable
    Influence
    Predict
    LinearHypothesis
}
var
  i, k: Integer;
begin
  FModel := nil;
  for i := 0 to FListFrame.Count-1 do
    begin
    DataIndex := i;
    FClassType := GetClassType(FListFrame[i],MSelect);
    Case FClassType of
      mtUnivRegComp:     { Regress�o univariada modelo completo }
        Begin
        TGraf_ModeloDeReg(Graf_Reg).Clear;
        FModel := TwsUReg.Create(Self);
        with TwsUReg(FModel) do
          begin
          //gOutPut.WriteTitle('Regress�o Univariada: Modelo Completo',clBlack); <<<<<
          For k := 1 to NResp do
            Begin
            RespVar := k;
{<<<<<
            gOutPut.WriteTitle('Vari�vel Resposta: '+
                            TwsDataSet(FListFrame[DataIndex]).ColName[RIdx[k]],clBlack);
            gOutPut.WriteLine;
}
            Coefficients;             // Coeficientes do modelo

            AnovaTable(True);         // Quadro da analise da variacao

            // Medidas de influencia, componente + residuo e residuos parciais
            if Options[ord(cmPrintInfluence)] or Options[ord(cmSaveInfluence)] Then
               begin
               Influence;
               TGraf_ModeloDeReg(Graf_Reg).Model := FModel;
               Graf_Reg.ShowModal;
               end;

            if Options[ord(cmPrintPredic)] or Options[ord(cmSavePredic)] Then
               Predict;

            if LinHyp <> nil then
               LinearHypothesis(LinHyp);
            End; {For K}
          end; { with }
        end;

      mtStepwiseSelect:   { Regress�o univariada passo a passo, m�todo stepwise }
        Begin
{<<<<<
        gOutPut.WriteTitle('Sele��o de modelos - Stepwise Selection',clBlack);
}
        { ialpha, oalpha sao valores de alfa para inclusao ou exclusao de variaveis }
        FModel := TwsStepwiseSelection.Create(Self, ialpha, oalpha);
        for k := 1 to RIdx.Len do
          begin
          TwsStepwiseSelection(FModel).RespVar := k;
          TwsStepwiseSelection(FModel).Stepwise
          end;
{<<<<<
        if gLink then
          gOutPut.WriteLink('Veja mais detalhes sobre o m�todo Stepwise Selection',
          ExtractFilePath(Application.ExeName)+'Ajuda\Modelos_Lineares\Selecao de Variaveis.htm#stepwise');
}
        End;

      mtBackwardEliminat: { Regress�o univariada passo a passo, backward elimination }
        Begin
{<<<<<
        gOutPut.WriteTitle('Sele��o de modelos - Backward Elimination',clBlack);
}
        FModel := TwsBackwardElimination.Create(Self, oalpha);
        for k := 1 to RIdx.Len do
          begin
          TwsBackwardElimination(FModel).RespVar := k;
          TwsBackwardElimination(FModel).Backward
          end;
{<<<<<
        if gLink then
          gOutPut.WriteLink('Veja mais detalhes sobre o m�todo Backward Elimination',
          ExtractFilePath(Application.ExeName)+'Ajuda\Modelos_Lineares\Selecao de Variaveis.htm#backward');
}
        End;

      mtAllReg:           { Regress�o univariada todas as regress�es }
        Begin
{<<<<<
        gOutPut.WriteTitle('Sele��o de modelos - Todas as Regress�es Poss�veis',clBlack);
}
        if ialpha < 1 then ialpha := 1;
        FModel := TwsAllReg.Create(Self, Trunc(ialpha), Trunc(oalpha)+1);
        for k := 1 to RIdx.Len do
          begin
          TwsAllReg(FModel).RespVar := k;
          TwsAllReg(FModel).AllRegressions
          end;
{<<<<<
        if gLink then
          gOutPut.WriteLink('Veja mais detalhes sobre o m�todo Todas as Regress�es',
          ExtractFilePath(Application.ExeName)+'Ajuda\Modelos_Lineares\Selecao de Variaveis.htm#todas');
}
        End;

      mtForwardSelect:    { Regress�o univariada passo a passo, m�todo forward }
        Begin
{<<<<<
        gOutPut.WriteTitle('Sele��o de modelos - Forward Selection',clBlack);
}
        FModel := TwsForwardSelection.Create(Self, ialpha);
        for k := 1 to RIdx.Len do
          begin
          TwsForwardSelection(FModel).RespVar := k;
          TwsForwardSelection(FModel).ForwardSel;
          end;
{<<<<<
        if gLink then
          gOutPut.WriteLink('Veja mais detalhes sobre o m�todo Forward Selection',
          ExtractFilePath(Application.ExeName)+'Ajuda\Modelos_Lineares\Selecao de Variaveis.htm#forward');
}
        End;
      End; {Case}
    If FModel <> nil then FModel.Free;
    FModel := nil;
    end; { for i }
End;

procedure TwsLMManager.ExecAnova(FUser: Boolean);
{ Objetivo
    Constr�i o quadro da an�lise da varia��o. Opcionalmente imprime e/ou salva o quadro da
    an�lise.
  Par�metros
    FUser: True, se o teste F for realizado com numeradores e denominadores dos termos
           definidos pelo usu�rio. False para realiza��o do teste F de modo autom�tico,
           atrav�s da obten��o dos valores esperados e componentes de vari�ncia
  Campos modificados
    Nenhum
  M�todos chamados
    FModel.Terms.UserFTest
    FModel.Terms.AutoFTest
    FModel.Terms.AnovaTable
}
var
  NBal: set of TwsModelType;
  nx  : Integer;
begin
  NBal:=[mtUnivCompAnova,mtUnivCompAncova,mtUnivIncompAnova,mtUnivIncompAncova,
    mtMultCompAnova,mtMultCompAncova,mtMultIncompAnova,mtMultIncompAncova];

  If FUser or (FClassType in NBal) Then
    TwsANova(FModel).Terms.UserFTest
  else
    TwsANova(FModel).Terms.AutoFTest(LVar.Count,RIdx);
  // as respostas veem sempre depois das covariaveis
  nx:=0;
  if FCovIdx<>nil then nx:=FCovIdx.Len;
  if (Options[ord(cmVar_PrintQuadAnalysis)]) or (Options[ord(cmVar_SaveQuadAnalysis)]) then
    TwsANova(FModel).Terms.AnovaTable(LResp,nx,GeneralMean,Options);
end; { ExecAnova }

Function TwsLMManager.GetIndexResp(SL: TStrings): TwsLIVec;
{ Objetivo
    Retorna um vetor com os indices das variaveis respostas no conjunto de dados em an�lise
  Par�metros
    SL: Lista de strings especificando as vari�veis
  Campos modificados
    Nenhum
  M�todos chamados
    Nenhum
}
Var i: Word;
    j: LongInt;
Begin
  Result := TwsLIVec.Create(SL.Count);
  j := -1;
  For i := 0 To SL.Count-1 Do
    begin
    j := MFrame[0].Struct.IndexOf(SL[i]);
    if j > 0 then
      Result[i + 1] := j
    else
      Break;
    end;

  If j <= 0 Then
     Begin
     Result.Free;
     Result := nil;
     End;
End; { TwsLinearModel.Ind }

Function TwsLMManager.GetIndexByType(SL: TStrings; T: TwsEnumDataType): TwsLIVec;
{ Objetivo
    Retorna um vetor com os �ndices das vari�veis do tipo especificado no conjunto de
    dados em an�lise.
  SL: Lista com os nomes das vari�veis
  T: Tipo de vari�vel para obten��o dos �ndices. Os valores poss�veis s�o:
     dtNumeric  : Vari�veis num�ricas
     dtQuant    : Fatores quantitativos
     Qualit   : Fatores qualitativos
     QualitOrd: Fatores qualitativos ordenados
     Factors  : Fatores de qualquer tipo
  Campos modificados
    Nenhum
  M�todos chamados
    Nenhum
}
Var i,j,jj: Integer;
    Tipo: TwsEnumDataType;
    v: TwsLIVec;
Begin
  v := TwsLIVec.Create(SL.Count);
  for i := 1 to SL.Count do
    v[i] := -1;
  j := -1;
  jj := 0;
  For i := 0 To SL.Count-1 Do
    Begin
    Tipo := MFrame[0].Struct.ColByName(SL[i]).ColType;
    If Tipo in [dtQuant, dtQualit, dtQualitOrd] Then Tipo := dtFactors;
    If Tipo = T Then
      begin
      inc(jj);
      j := MFrame[0].Struct.IndexOf(SL[i]);
      if j > 0 then v[i + 1] := j else Break;
      End;
    end;

  If jj > 0 Then
     Begin
     Result := TwsLIVec.Create(jj);
     jj := 0;
     For i := 1 to v.Len do
       If v[i] <> -1 Then
          begin
          inc(jj);
          result[jj] := v[i];
          end;
     End
  Else
     Result := nil;

  v.Free;
End; { TwsLinearModel.Ind }

Function TwsLMManager.GetIndex(SL: TStrings): TwsLIVec;
{ Objetivo
    Retorna um vetor com os �ndices das vari�veis da lista especificada, presentes no
    conjunto de dados em an�lise
  Par�metros
    SL: Lista com os nomes das vari�veis
  Campos modificados
    Nenhum
  M�todos chamados
    nenhum
}
Var i: Word;
    j: LongInt;
Begin
  Result := TwsLIVec.Create(SL.Count);
  j := -1;
  For i := 0 To SL.Count-1 Do
    begin
    j := MFrame[0].Struct.IndexOf(SL[i]);
    if j <> -1 then Result[i + 1] := j else Break;
    end;

  If j <= 0 Then
    Begin
    Result.Free;
    Result := nil;
    End;
End; { TwsLinearModel.GetIndex }

Function TwsLMManager.GetRespVars: TStrings;
{ Objetivo
    Retorna a lista das vari�veis respostas presentes na f�rmula especificada no modelo.
    Essa seq��ncia de caracteres � retirada da express�o do modelo. As variaveis respostas
    poderao estar separadas por virgulas e/ou espacos em branco. Respostas definidas como
    expressoes deverao estar entre colchetes ([]).
  Campos modificados
    FFormula
}
Var P        : Byte;
    s,stoken : String;
    ch       : TCharSet;
    j        : Cardinal;
Begin
  Result := TStringList.Create;
  P := System.Pos('~', FFormula);
  s := SysUtilsEx.AllTrim(Copy(FFormula, 1, P-1));
  Delete(FFormula, 1, P);

  ch := [ '[', ']',',',' '];
  j := 1;

  while j <= length(s) do
    begin
    stoken := StrToken(s, j, ch);
    stoken := SysUtilsEx.AllTrim(stoken);
    if stoken <> '' then
       Result.Add(stoken);
    end;

End; { TwsLMManager.GetRespVars }

Function TwsLMManager.HaveFactor(SL: TStrings): Boolean;
{Objetivo
   Verifica se na lista especificada existe algum fator
 Par�metros
   SL: Lista om os nomes das vari�veis
}
Var
  i: Integer;
  C: TwsDataSetCol;
Begin
  Result := False;
  For i := 0 to SL.Count - 1 do
    Begin
      Try
        C := MFrame[0].Struct.ColByName(SL[i]);
      Except
        C := nil;
      End;

    If (C <> nil) and (C.ColType <> dtNumeric) Then
       Begin
       Result := True;
       Break;
       End;
    End;
End; { TwsLMManager.HaveFactor }

Function TwsLMManager.HaveNumeric(SL: TStrings): Boolean;
{Objetivo
   Verifica se na lista especificada existe alguma vari�vel num�rica
 Par�metros
   SL: Lista om os nomes das vari�veis
 Campos modificados
   Nenhum
 M�todos chamados
   Nenhum
}
Var
  i: Integer;
  C: TwsDataSetCol;
Begin
  Result := False;
  For i := 0 to SL.Count - 1 do
    Begin
      Try
        C := MFrame[0].Struct.ColByName(SL[i]);
      Except
        C := nil;
      End;

    If (C <> nil) and (C.ColType = dtNumeric) Then
       Begin
       Result := True;
       Break;
       End;
    End;
End; { TwsLMManager.HaveNumeric }

function TwsLMManager.GetClassType(Data: TwsDataSet; VarSelect: Integer = 0): TwsModelType;
{ Objetivo
    Obt�m a classe de modelo apropriada para an�lise considerando o modelo adotado e o
    comjunto de dados dispon�vel
  Par�metros
    Data: Conjunto de dados para an�lise. No caso de modelos de an�lise da varia��o � o
      conjunto referente �s m�dias das combina��es
    VarSelect: M�todo de sele��o de vari�veis. O valor default � zero. Os valores
      considerados s�o:
        0: Regress�o univariada modelo completo
        1: Regress�o univariada passo a passo, m�todo backward elimination
        2: Regress�o univariada passo a passo, m�todo forward selection
        3: Regress�o univariada passo a passo, m�todo stepwise
        4: Regress�o univariada todas as regress�es poss�veis
      � um par�metro considerado somente quanndo o modelo � de regress�o
  Campos modificados
    Nenhum
  M�todos chamados
    Nenhum
  Retorno
    O retorno e os modelos considerados s�o:
      mtUnivBalAnova      // Anova univariada balanceada
      mtUnivBalAncova,    // Ancova univariada balanceada

      mtUnivCompAnova,    // Anova univariada completa
      mtUnivCompAncova,   // Ancova univariada completa
      mtUnivIncompAnova,  // Anova univariada incompleta
      mtUnivIncompAncova, // Ancova univariada incompleta

      mtMultBalAnova,     // Anova multivariada balanceada
      mtMultBalAncova,    // Ancova multivariada balanceada
      mtMultCompAnova,    // Anova multivariada nao balanceada
      mtMultCompAncova,   // Ancova multivariada nao balanceada
      mtMultIncompAnova,  // Anova multivariada incompleta
      mtMultIncompAncova, // Ancova multivariada incompleta
      mtUnivRegComp,      // Regress�o univariada modelo completo
      mtUnivWReg,         // Regressao univariada ponderada
      mtAllReg,           // Regress�o univariada todas as regress�es
      mtStepwiseSelect,   // Reg. univariada passo a passo, stepwise
      mtForwardSelect,    // Reg. univariada passo a passo, forward
      mtBackwardEliminat, // Reg. univariada passo a passo, stepwise
      mtMultReg,          // Regress�o multivariada
      mtRegression,       // Analise de regressao
      mtAnova,            // Analise da variacao

}
var
  Compl,Balanc: Boolean;
  i,n         : Integer;
begin
  if FFacIdx <> nil then
    begin                                 { Se possui algum fator, entao analise da variacao }
    n := 1;
    for i:=1 to FFacIdx.Len do            { N�mero de subclasses ou combina��es }
      n := n*TwsFactor(Data.Struct.Col[FFacIdx[i]]).Levels;
    Compl := n = Data.NRows;                { Possui todas as combinacoes? }
    Balanc := Compl and                     { Estrutura balanceada? }
      Data.ColConst(Data.NCols);

    if FCovIdx = nil then                   { Se nao possui covariavel, analise da variacao }
      if (FRIdx.Len=1) or FUnivariado then  { Analise da variacao univariada }
        if Balanc then
          Result := mtUnivBalAnova        { Analise da variacao univariada balanceada }
        else
          if Compl then
            Result := mtUnivCompAnova     { Analise da variacao univariada completa }
          else
            Result := mtUnivIncompAnova   { Analise da variacao univariada incompleta }
      else                                { An�lise da varia��o multivariada }
        if Balanc then
          Result := mtMultBalAnova        { An�lise da varia��o multivariada balanceada }
        else
          if Compl then
            Result := mtMultCompAnova     { An�lise da varia��o multivariada completa }
          else
            Result := mtMultIncompAnova   { An�lise da varia��o multivariada incompleta }
    else                                  { Analise da covariacao }
      if (FRIdx.Len=1) or FUnivariado then { Analise da covariacao univariada }
        if Balanc then
          Result := mtUnivBalAncova       { Analise da covariacao univariada balanceada }
        else
          if Compl then
            Result := mtUnivCompAncova    { Analise da covariacao univariada completa }
          else
            Result := mtUnivIncompAncova  { Analise da covariacao univariada incompleta }
      else { ancova multivariada }
        if Balanc then
          Result := mtMultBalAncova       { An�lise da covaria��o multivariada balanceada }
        else
          if Compl then
            Result := mtMultCompAncova    { An�lise da covaria��o multivariada completa }
          else
            Result := mtMultIncompAncova  { An�lise da covaria��o multivariada incompleta }
    end
  else
    begin                                 { An�lise de regress�o }
    if (FRIdx.Len=1) or FUnivariado then  { Analise de regress�o univariada }
      case VarSelect of
        0: Result := mtUnivRegComp;       { Regress�o univariada modelo completo }
        1: Result := mtBackwardEliminat;  { Regress�o univariada passo a passo, stepwise}
        2: Result := mtForwardSelect;     { Regress�o univariada passo a passo, forward }
        3: Result := mtStepwiseSelect;    { Regress�o univariada passo a passo, stepwise }
        4: Result := mtAllReg;            { Regress�o univ.todas as regress�es poss�veis }
      end { case }
    else                                  { Analise de regress�o multivariada }
      Result := mtMultReg
    end;
end;


procedure TwsLMManager.MultRangeTest(Const Test: String; Y: TwsGeneral; YCol: Integer;
  Level,CVar,df: double);
{ Objetivo:
    Aplicar testes de compara��es m�ltiplas a um grupo de m�dias
  Par�metros:
    Test :  Teste de compara��o m�ltipla desejado
      'Duncan'     - Teste de Duncan
      'Tukey'      - Teste de Tukey
      'SNK'        - Teste de Student-Newman-Kewls
      'Tukey B'    - Teste que utiliza como valor critico a media entre os valores cr�ticos
                     de Tukey e Newman-Kewls
      'DMS'        - Teste DMS de Fisher
      'Scheffe'    - Teste de Scheffe
      'Bonferroni' - Teste de Bonferroni
    Y    :  Conjunto de dados com as medias
    YCol :  Coluna sobre a qual ser� executado o teste
    Level:  Nivel de significancia
    CVar :  Variancia para o contraste
    df   :  Graus de liberdade associados
  Observa��es
    Se Level nao for 0.01, 0.05, ou 0.10, o teste sera feito a 0.05
  Campos modificados
    Nenhum
  M�todos chamados
    Nenhum
  Sa�das
    Matriz das m�dias                               (  I)
    Matriz com os resultados do teste               (  I)
    Matriz com os valores cr�ticos do teste         (  I)
    Conjunto de dados com as compara��es realizadas (I/S)
}
var
  i,j,q,k,Last,
  RCol,nt       : Integer;
  Sig,Equal     : Boolean;
  Groups,                      { Groups armazena os grupos homog�nos }
  RValue, Tab   : TwsGeneral;    { RValue armazena os valores cr�ticos }
  Comp          : TwsDataSet;    { Conjunto para armazenar as compara��es realizadas }
  Lin           : TwsVec;
  Asc, CS       : TwsLIVec;
  aux,nrep,aux1 : double;
  Erro          : Word;
  MRTest        : TMeantest;
  Tit           : string;

    procedure SetComp;
    begin
      Lin[1] := i-1; Lin[2] := q-1;
      Lin[3]:=Y[i,YCol];Lin[4]:=Y[q,YCol];Lin[5]:=Lin[3]-Lin[4];
      Lin[6]:=RValue[1,q-i];
      Sig := (Lin[3]-Lin[4]) > Lin[6];
      Lin[7] := LongInt(Sig);
      Comp.MAdd(Lin);
    end;

begin
  { Ordenar Y em ordem descendente pela coluna das medias }
  nt := Y.NRows;
  Asc := TwsLIVec.Create(1);
  Asc[1] := 0;
  CS  := TwsLiVec.Create(1);
  CS[1] := YCol;
  Y.SortRows(CS, Asc);
  Asc.Free;
  CS.Free;
  if not FEquals(Level,0.05) then
    if not FEquals(Level,0.01) then
      if not FEquals(Level,0.1) then
        Level := 0.05;
  RCol := YCol+1;
  Equal := True;
  Aux:=Y[1,RCol];
  for i:=2 to Y.NRows do   // Verifica se o numero de repeticoes e constante
    begin
    Equal := FEquals(aux,Y[i,RCol]);
    if not Equal then Break
    end;
  if Equal then      // Se for trabalha com o valor constante
    nrep:=aux
  else               // se nao, estima atraves da media harmonica das repeticoes
    begin
    nrep:=0;
    for i:=1 to Y.NRows do
      nrep := nrep+1/Y[i,RCol];
    nrep:=Y.NRows/nrep
    end;
  aux1:=CVar/nrep;
  RValue := TwsGeneral.Create(1,4);
  with RValue do
    begin
    Name:='Inf_Teste';
    MLab := 'Compara��es m�ltiplas de m�dias - Vari�vel: '+ Y.ColName[YCol];
    RowName[1]:='Valor';
    ColName[1] := 'Variancia'; ColName[2] := 'Gr_Lib';
    ColName[3] := 'Nivel_Sig'; ColName[4] := 'n';
    Data[1,1] := CVar; Data[1,2] := DF; Data[1,3] := Level; Data[1,4]:=nrep;
{<<<<<
    TwsOutPut(gOutPut).WriteTable(RValue);
}
    Free;
    end;

  RValue := TwsGeneral.Create(1,nt-1);
  RValue.RowName[1] := 'Valores';
  RValue.PrintOptions.ColPrecision := 5;
  RValue.PrintOptions.ColWidth := 8;

  Comp := TwsDataSet.Create('Compara',0,0);
  Comp.MLab := 'Compara��es realizadas';
  Comp.Struct.AddQualitative('Nivel_I','Nivel I do fator',10);    //1
  for i := 1 to nt do
    TwsFactor(Comp.Struct.Col[1]).AddLevel(Y.RowName[i]);

  Comp.Struct.AddQualitative('Nivel_J','Nivel J do fator',10);
  for i := 1 to nt do
    TwsFactor(Comp.Struct.Col[2]).AddLevel(Y.RowName[i]);

  Comp.Struct.AddNumeric('Media_I','Valor da m�dia I',12,8);

  Comp.Struct.AddNumeric('Media_J','Valor da m�dia J',12,8);

  Comp.Struct.AddNumeric('Diferenca','Diferen�a entre as m�dias I e J',12,8);

  Comp.Struct.AddNumeric('DMS','Diferen�a m�nima significativa',12,8);

  Comp.Struct.AddQualitative('Sig','Signific�ncia da diferen�a no n�vel estabelecido',5);
  with TwsFactor(Comp.Struct.Col[7]) do
    begin
    AddLevel('NS');
    AddLevel('Sig');
    end;

  Groups := TwsGeneral.Create(0,nt);
  for i := 1 to nt do
    Groups.ColName[i] := 'm' + Y.RowName[i];
  Groups.MLab := 'Grupos homog�neos. M�dias com o mesmo n�vel n�o diferem entre si';
  Groups.Name := 'Sequenc';
  Groups.PrintOptions.ColWidth := 10;

  Tit:= 'Diferen�as M�nimas Significativas (DMS) ao n�vel ' + FloatToStr(100*Level) + '%';
  MRTest := StrToMeantest(Test);
  case MRTest of
    mtDuncan:
      begin
      RValue.Name := 'Duncan';
      RValue.MLab := Tit + ' - Teste de Duncan';
      for i := 1 to nt-1 do
        begin
        RValue[1,i] := FDuncan(Level, i+1, df)*Sqrt(aux1);
        RValue.ColName[i] := 'Amp_'+IntToStr(i+1)
        end;
      end;

    mtTukey:
      begin
      { Abrir matrtiz com os valores da tabela para o nivel desejado }
      If FEquals(Level,0.05) Then
        Tab := TwsGeneral(TwsMatrix.LoadFromFile(ExtractFilePath(Application.ExeName)+'Tabelas\Tukey5.mat'))
      else
        if FEquals(Level,0.01) then
          Tab := TwsGeneral(TwsMatrix.LoadFromFile(ExtractFilePath(Application.ExeName)+'Tabelas\Tukey1.mat'))
        else
          Tab := TwsGeneral(TwsMatrix.LoadFromFile(ExtractFilePath(Application.ExeName)+'Tabelas\Tukey10.mat'));
      RValue.Name := 'Tukey';
      RValue.MLab := Tit + ' - Teste de Tukey';
      aux := Tab.GetTableValue(nt,DF);
      for i := 1 to nt-1 do
        begin
        RValue[1,i] := aux*Sqrt(aux1);
        RValue.ColName[i] := 'Amp_'+IntToStr(i+1)
        end;
      Tab.Free
      end;

    mtSNK:
      begin
      { Abrir matriz com os valores da tabela para o nivel desejado }
      If FEquals(Level,0.05) Then
        Tab := TwsGeneral(TwsMatrix.LoadFromFile(ExtractFilePath(Application.ExeName)+
          'Tabelas\Tukey5.mat'))
      else
        if FEquals(Level,0.01) then
          Tab := TwsGeneral(TwsMatrix.LoadFromFile(ExtractFilePath(Application.ExeName)+
            'Tabelas\Tukey1.mat'))
        else
          Tab := TwsGeneral(TwsMatrix.LoadFromFile(ExtractFilePath(Application.ExeName)+
            'Tabelas\Tukey10.mat'));
      RValue.Name := 'SNK';
      RValue.MLab:=Tit + ' - Teste de Student-Newman-Keuls (SNK)';
      for i := 1 to nt-1 do
        begin
        RValue[1,i] := Tab.GetTableValue(i+1,df)*Sqrt(aux1);
        RValue.ColName[i] := 'Amp_'+IntToStr(i+1)
        end;
      Tab.Free
      end;

    mtModTukey:
      begin
      { Abrir matriz com os valores da tabela para o nivel desejado }
      If FEquals(Level,0.05) Then
        Tab:=TwsGeneral(TwsMatrix.LoadFromFile(ExtractFilePath(Application.ExeName)+'Tabelas\Tukey5.mat'))
      else
        if FEquals(Level,0.01) then
          Tab:=TwsGeneral(TwsMatrix.LoadFromFile(ExtractFilePath(Application.ExeName)+'Tabelas\Tukey1.mat'))
        else
          Tab:=TwsGeneral(TwsMatrix.LoadFromFile(ExtractFilePath(Application.ExeName)+'Tabelas\Tukey10.mat'));
      RValue.Name := 'SNK';
      aux := Tab.GetTableValue(nt,DF);
      RValue.MLab:=Tit + ' - Teste de TukeyB';
      for i := 1 to nt-1 do
        begin
        RValue[1,i] := ((Tab.GetTableValue(i+1, df)+aux)/2)*Sqrt(aux1);
        RValue.ColName[i] := 'Amp_'+IntToStr(i+1)
        end;
      Tab.Free
      end;

    mtDMS:
      begin
      RValue.Name := 'DMS';
      RValue.MLab := Tit + ' - Teste LSD';
      aux := Sqrt(2*FInv(Level,1,df,1.0e-10, True, Erro)*aux1);
      for i := 1 to nt-1 do
        begin
        RValue[1,i] := aux;
        RValue.ColName[i] := 'Amp_'+IntToStr(i+1)
        end;
      end;

    mtModDMS:
      begin
      RValue.Name := 'Bonf';
      RValue.MLab := Tit + ' - Teste de Bonferroni';
      aux := Sqrt(2*FInv(2*Level/(nt*(nt-1)),1,df,1.0e-10, True, Erro)*aux1);
      for i := 1 to nt-1 do
        begin
        RValue[1,i] := aux;
        RValue.ColName[i] := 'Amp_'+IntToStr(i+1)
        end;
      end;

    mtScheffe:
      begin
      RValue.Name := 'Scheffe';
      RValue.MLab:=Tit + ' - Teste de Sch�ff�';
      aux := Sqrt(2*(nt-1)*FInv(Level,nt-1,df,1.0e-10,True,Erro)*aux1);
      for i := 1 to nt-1 do
        begin
        RValue[1,i] := aux;
        RValue.ColName[i] := 'Amp_'+IntToStr(i+1)
        end;
      end;
  end; { case }
  { Colocar nomes dos niveis em Groups - linhas e colunas }
  k:=0;
  i := 0;
  Last := nt;
  Repeat
    Inc(i);
    q := nt;
    Lin := TwsDFVec.Create(7);
    SetComp;
    if Sig then Dec(q);
    { Determina os indices do grupo homogeneo }
    while Sig and (q <> Last) and (i<q) do
      begin
      Lin := TwsDFVec.Create(7);
      SetComp;
      if Sig then Dec(q);
      end; { while }
    if q<>Last then
      begin
      Inc(k);
      Lin := VecConst(wscMissValue,nt);
      Lin.Name:='Seq_'+IntToStr(k);
      for j := q downto i do Lin[j] := Level;
      Groups.MAdd(Lin)
      end;
    Last := q;
  Until (i = nt-1) or (q = nt);

  { Insere uma linha para o caso de nao haver nehuma diferen�a significativa }
  if Groups.NRows = 0 then
    begin
    Lin := VecConst(Level,Y.NRows);
    Lin.Name:='Seq_1';
    Groups.MAdd(Lin)
    end;

{<<<<<
  TwsOutPut(gOutPut).WriteTable(Y);
  TwsOutPut(gOutPut).WriteTable(RValue);
  TwsOutPut(gOutPut).WriteTable(Groups);
}
{<<<<<
  if Options[ord(cmVar_PrintTesteCM)] then
    TwsOutPut(gOutPut).WriteTable(Comp);
}
  if Options[ord(cmVar_SaveTesteCM)] then
    begin
    FTV.AddDataSet(Comp.Name,Comp);
    //SendMessageToObjects(WSPM_UPDATE_STATUS, [0]);
    end
  else
    Comp.Free;

  RValue.Free;
  Groups.Free;
end; { MultRangeTest }

Procedure TwsLMManager.SetDataSet(Value: TwsDataSet);
Begin
(*  If (FFDataSet <> Nil) Then
     FFDataSet.DestroyTempDataSet;

  FFDataSet := Value;

  FDataSet := FFDataSet.AsDataSet; *)
  FDataSet:=Value
End;

Procedure TwsLMManager.Undo;
{ Objetivo
    Libera conte�do de mem�ria alocados na an�lise
  M�todos chamados
    ClearStage1
    ClearStage2
    ClearStage3
}
Begin
  Case State of
    1: ClearStage1;
    2: ClearStage2;
    3: ClearStage3;
    4: {Nada};
    5: {Nada};
    End;
End;

function GetSignif(const x: double): Integer;
begin
  if x > 0.05 then
    Result := 0
   else
     if x > 0.01 then
       Result := 1
     else
       Result := 2
end;

procedure TwsLMManager.PolRegTest(Means:TwsGeneral; YCol:Integer; x:TwsVec; CVar,DF:double;
  DMax: Integer);
{ Objetivo
    Executa a an�lise de regress�o polinomial pelo m�todo dos polin�mios orrtogonais.
    Possibilita gr�ficos dos polin�mios ajustados juntamente com as medias observadas.
  Par�metros
    Means: Matriz de m�dias.
    YCol:  Coluna onde se encontram as m�dias para an�lise. Sup�e-se que na coluna YCol+1
           est�o as repeti��es.
    x:     Vetor com os valores dos n�veis.
    CVar:  Valor da vari�ncia para os testes
    DMax:  Grau m�ximo para o polin�mio
  Campos modificados
    Nenhum
  M�todos chamados
    Nenhum
  Sa�das
    Quadro da an�lise da varia��o                          (I)
    Janelas gr�ficas com polin�mios e m�dias observadas    (I/S)
    Valores preditos pelas equa��es e outras informa��es   (I/S)
    Valores dos coeficientes dos polin�mios ortogonais     (I)
    Valores dos coeficientes
}
const
  nValPol          = 300;
var
  w,y,sq,v,d       : TwsVec;
  Idx              : TwsLIVec;
  i,j,nniv,k       : Integer;
  P,
  Pred{, MCoef}      : TwsGeneral;
  AnovaTable       : TwsDataSet;
  Bet              : TwsTriangular;
  sqt,aux,m,w0,wa  : double;
  Erro             : Word;
  Graf             : TgrGrafico;
begin
{<<<<<
  gOutPut.WriteTitle('An�lise de Regress�o Polinomial - Vari�vel: '+Means.ColName[YCol],clBlack);
  gOutPut.WriteLine;
}
  Idx:=x.QuickIndx(True);
  Means.SortRowAs(Idx);

  nniv:= Means.NRows;
  If DMax > nNiv - 1 Then DMax := nNiv - 1;

  { Tabela com a an�lise da varia��o }
  AnovaTable := TwsDataSet.Create('Quadro',0,0);
  AnovaTable.MLab := 'Quadro da an�lise da varia��o';
  AnovaTable.Struct.AddNumeric('GL','Graus de liberdade',6,5);
  AnovaTable.Struct.AddNumeric('SQ','Quadrado m�dio'); { Coluna 2 }
  AnovaTable.Struct.AddNumeric('F','Valor da estat�stica F',12,7);
  AnovaTable.Struct.AddNumeric('Valor_p','Valor p para a hip�tese Ho',11,4);
  AnovaTable.Struct.AddQualitative('Sig','Signific�ncia do teste ao n�vel 5% ou 1%',6);
  with TwsQualitative(AnovaTable.Struct.Col[5]) do
    begin
    AddLevel('NS');
    AddLevel('5%');
    AddLevel('1%');
    end;
  AnovaTable.Struct.AddNumeric('R2','Coeficiente de determina��o acumulado',9,4);

  Pred := TwsGeneral.Create(0,nniv);
  Pred.MLab := 'Valores dos n�veis, m�dias e m�dias estimadas';

  y := TwsDFVec.Create(nniv); y.Name := Means.ColName[YCol];

  sqt := 0; m := 0; wa := 0; { sqt � soma de quadrados associada �s medias }
  v := TwsDFVec.Create(nniv);
  w := TwsDFVec.Create(nniv);
  { ===> verfificar esquema de pondera��o }
  for i := 1 to nniv do
    begin
    Pred.ColName[i] := Means.RowName[i];
    y[i] := Means[i,YCol];                // Coluna das medias
    w[i] := Means[i,YCol+1];              // Coluna das repeticoes
    v[i] := x[i];                         // valores dos niveis
    w0 := wa;                             // Guarda pesos acumulados ate passo anterior
    wa := wa+w[i];                        // Acumula os pesos ate passo i
    aux := (w[i]/wa)*(y[i]-m);
    sqt := sqt + (aux/w[i])*aux*wa*w0;    // 2o momento
    m := m + aux                          // media
    end;

  v.Name := Means.Name;
  Pred.MAdd(v); // Insere valores dos n�veis na primeira linha
  Pred.MAdd(y); // Insere os valores das m�dias na segunda linha

  P := WPolinFit(Bet,x,w,y,sq,DMax);

  w.Name:=y.Name+'_n';
  Pred.MAdd(w);
  Bet.MLab := 'Coeficientes dos termos polinomiais';
  Bet.Name := 'Coeficientes';
  Bet.PrintOptions.ColPrecision := 8;
  Bet.PrintOptions.ColWidth := 15;

  for i := 0 to DMax do
    begin
    k:=i+1;
    Bet.ColName[k] := 'X_'+IntToStr(i);
    Bet.RowName[k] := 'Grau_'+IntToStr(i);
    end;

  AnovaTable.ColIdentName:='Fontes';
  v := VecConst(wscMissValue,6);
  v.Name:=Means.Name;
  v[1] := y.len-1;
  v[2] := sqt;
  AnovaTable.MAdd(v);

  aux := 0; w0:=0; k:=0;
  // Obtencao dos valores estimados pelos polinomios e quadro da analise
  for i := 2 to Bet.NRows do // do efeito linear em diante
    begin
    v := TwsDFVec.Create(nniv);
    v.Name := 'Grau '+IntToStr(i-1);
    d := TwsDFVec.Create(nniv);           // d armazenara desvios em relacao a cada grau
    d.Name := 'Desvio '+IntToStr(i-1);
    for j := 1 to nniv do
      begin
      v[j] := Bet.Row[i].PolValue(x[j]);  // media estimada pelo polinomio
      d[j] := Pred[2,j]-v[j];             // media observada menos estimada
      end;
    Pred.MAdd(v);
    Pred.MAdd(d);
    v := TwsDFVec.Create(6);
    v.Name := 'Grau '+IntToStr(i-1);
    Inc(k);
    v[1] :=  1;                     // grau de liberdade
    v[2] := sq[i];                  // soma de quadrados
    v[3] := sq[i]/CVar;             // valor da estatistica F
    v[4] := FProb(1,DF,v[3],Erro);  // probabilidade p
    v[5] := GetSignif(v[4]);        // significancia
    aux := aux + sq[i]/sqt*100;
    v[6] := Aux;                    // r2 acumulado
    w0:=w0+sq[i];                   // acumula somas de quadrados
    AnovaTable.MAdd(v);
    end;

  if (y.len-k)>1 then               // se teve falta de ajustamento
    begin
    v:=TwsDFVec.Create(6);
    v.Name:='Desvio';
    v[1]:=y.Len-1-k;
    v[2]:=(sqt-w0)/v[1];
    v[3]:=v[2]/CVar;
    v[4]:=FProb(v[1],DF,v[3],Erro);
    v[5]:=GetSignif(v[4]);
    v[6]:=100-Aux;
    AnovaTable.MAdd(v)
    end;

  v := VecConst(wscMissValue,6);
  v.Name := 'Res�duo';
  v[1] := DF;
  v[2] := DF*CVar;
  AnovaTable.MAdd(v);

  // Valores dos polinomios para os graficos
  sq.free;
  if Options[ord(cmVar_GrafRP)] then
    Begin
    w := TwsDFVec.Create(nValPol);        // w armazena os valores de X
    x.Name := Means.ColName[YCol];
    v := TwsDFVec.Create(nValPol);        // v armazena os valores dos polinomios
    aux := (x[nniv]-x[1])/nValPol;
    for i := 2 to Bet.NRows do
      begin
      w0 := x[1];
      sq := Bet.Row[i];
      w.Name := Bet.RowName[i];
      for j := 1 to nValPol do
        begin
        // Calcula o valor do polinomio com coeficientes em sq no ponto w0
        v[j] := sq.PolValue(w0);
        w[j] := w0;
        w0   := w0 + aux;
        end;

      // Plota v contra w (Funcao) e y contra x (Pontos) no mesmo gr�fico.
      Graf := ws_VectorsPlot([w, x], [v, y], [stLine, stPoint], k);
      if k = 0 then
         begin
         Graf.Grafico.View3D:= False;
         Graf.Top:= i * 10;
         Graf.Left:= i * 10;
         Graf.Caption:= 'Gr�ficos das Equa��es Polinomiais';
         Graf.Grafico.Title.Text.Add('Equa��o de Grau '+IntToStr(i-1)+' '+Means.MLab);
         Graf.Grafico.LeftAxis.Title.Caption := Means.ColName[YCol];
         Graf.Grafico.BottomAxis.Title.Caption := Means.Name;
{<<<<<
         TwsOutPut(gOutPut).OutGraph(Graf);
}
         end
      else
         begin
         {$IFDEF DEBUG}
         ShowMessage('Erro na gera��o dos gr�ficos: C�digo ' + intToStr(k));
         {$ENDIF}
         end;
      End; // for i
    v.free; w.free;
    Application.ProcessMessages;
    end;

{<<<<<
  if Options[ord(cmVar_PrintTesteRP)] then
    TwsOutPut(gOutPut).WriteTable(Pred);
}
  if Options[ord(cmVar_SaveTesteRP)] then
     begin
     FTV.AddMatrix(Pred.Name,Pred);
     //SendMessageToObjects(WSPM_UPDATE_STATUS_OBJETOS, [0]);
     end
  else
     Pred.Free;
{<<<<<
  TwsOutPut(gOutPut).WriteTable(AnovaTable);
  TwsOutPut(gOutPut).WriteTable(Bet);
}
  Bet.free; AnovaTable.Free; P.Free;
end;

procedure TwsLMManager.ContrastTest(Const Test: String;
                       Y,
                       C: TwsGeneral;
                       YCol: Integer;
                       CVar, df: double);
{ Objetivo
    Analisar contrastes entre m�dias de n�veis de fatores
  Par�metros
    Test: Tipo de teste a ser realizado
      'F'          - Teste F
      'Scheffe'    - Teste de Scheffe
      'Bonferroni' - Teste de Bonferroni
    Y   : Matriz das m�dias e repeti��es
    C   : Matriz dos contrastes
    YCol: Coluna onde estar�o os valores das m�dias. As repeti��es estar�o na coluna seguinte
    CVar: Vari�ncia associada ao contraste
    df  : Graus de liberdade associado � vari�ncia
}
var
  i,j,RCol     : Integer;
  sqc,VCont,aux: double;
  v            : TwsVec;
  ContrType    : TMeanContr;
  ContrName    : string;
  Erro         : Word;
  mAux         : TwsGeneral;
  mContr       : TwsDataSet;
  Col          : TwsDataSetCol;
begin
  ContrType := StrToMeanContr(Test);
  case ContrType of
    mcF:
      begin
      aux := TInv(0.05,df,1.0e-10, True,False,Erro);
      ContrName := 'F';
      end;

    mcBonferroni:
      begin
      aux := TInv(0.05/C.NRows,df,1.0e-10,True,False,Erro);
      ContrName := 'Bonfer';
      end;

    mcScheffe:
      begin
      aux := Sqrt(C.NRows*FInv(0.05,C.NRows,df,1.0e-10,True,Erro));
      ContrName := 'Scheffe';
      end;
  end; { case }

  RCol := YCol+1;

  mContr := TwsDataSet.Create('Contraste',0,0);
  with mContr do
    begin
    MLab := 'Estatisticas para contrastes: '+ContrName;
    ColIdentName:='Fontes';

    Struct.AddColEx(TwsNumeric.Create('Val_Contr','Valor do contraste',11,7));     //1

    Struct.AddColEx(TwsNumeric.Create('SQ','Soma de Quadrados do Contraste'));     //2

    Struct.AddColEx(TwsNumeric.Create('GL','Graus de Liberdade',6,5));            //3

    Struct.AddColEx(TwsNumeric.Create('F','Valor da Estat�stica F',12,7));          //4

    Struct.AddColEx(TwsNumeric.Create('Valor_p','N�vel M�nimo de Signific�ncia',11,4)); //5

    Col:=TwsQualitative.Create('Sig','Signific�ncia do teste ao n�vel 5% ou 1%',6);  //6
    With TwsQualitative(Col) Do
      Begin
      AddLevel('NS');
      AddLevel('5%');
      AddLevel('1%');
      End; { With Col }
    Struct.AddColEx(Col);

    Struct.AddColEx(TwsNumeric.Create('Lim_Inf','Limite Inferior Int. Confian�a')); //7
    Struct.AddColEx(TwsNumeric.Create('Lim_Sup','Limite Superior Int. Confian�a')); //8
    end;

  Erro := 0;
  for j := 1 to C.NCols do
    begin
    sqc := 0;
    VCont := 0;
    v := TwsDFVec.Create(8);
    v.Name := C.ColName[j];
    for i := 1 to C.NRows do
      begin
      sqc := sqc + (C[i,j]*C[i,j])/Y[i,RCol];// Somas de quadrados dos contrastes
      VCont := VCont+C[i,j]*Y[i,YCol];       // Valor do contraste
      end;
    v[1] := VCont;                           // Valor do contraste
    v[2] := (VCont*VCont)/sqc;               // Soma de quadrados
    v[3] := 1;                               // Graus de liberdade
    v[4] := v[2]/CVar;                       // Valor F
    v[5] := FInt(v[4], 1, df, True, Erro);   // Probabilidade
    If v[5]>=0.05                            // Significancia da estatistica
      Then
        v[6]:=0
      Else
        If v[5]>=0.01
          Then
            v[6]:=1
          Else
            v[6]:=2;
    v[7] := v[1] - aux*sqrt(CVar*sqc);       // Extremo inferior do intervalo de confian�a
    v[8] := v[1] + aux*sqrt(CVar*sqc);       // Extremo superior do intervalo de confian�a
    mContr.MAdd(v)
    end;
  mAux := Y.ColConcat(C,Erro);
  MAux.MLab := 'M�dias, repeti��es e coeficientes dos contrastes';

{<<<<<
  if Options[ord(cmVar_PrintTesteC)] then
    TwsOutPut(gOutPut).WriteTable(mContr);
}
  if Options[ord(cmVar_SaveTesteC)] then
     begin
     FTV.AddDataSet(mContr.Name,mContr);
     //SendMessageToObjects(WSPM_UPDATE_STATUS, [0]);
     end
  else
     mContr.Free;

{<<<<<
  TwsOutPut(gOutPut).WriteTable(MAux);
}
  MAux.Free;
end;

function StrToMeantest(Const Test: String): TMeanTest;
begin
  If CompareText(Test, 'Duncan' ) = 0 Then
    Result := mtDuncan
  else
    If CompareText(Test, 'Tukey'  ) = 0 Then
      Result := mtTukey
    else
      If CompareText(Test, 'SNK'    ) = 0 Then
        Result := mtSNK
      else
        If CompareText(Test, 'Tukey B') = 0 Then
          Result := mtModTukey
        else
          If CompareText(Test, 'DMS'    ) = 0 Then
            Result := mtDMS
          else
            If CompareText(Test, 'Scheffe') = 0 Then
              Result := mtScheffe
            else
              If CompareText(Test, 'Bonferroni') = 0 Then
                Result := mtModDMS;
end;

function StrToMeanContr(Const Test: String): TMeanContr;
begin
  If CompareText(Test, 'F' ) = 0 Then
    Result := mcF
  else
    If CompareText(Test, 'Scheffe') = 0 Then
      Result := mcScheffe
    else
      If CompareText(Test, 'Bonferroni') = 0 Then
        Result := mcBonferroni
end;

end.
