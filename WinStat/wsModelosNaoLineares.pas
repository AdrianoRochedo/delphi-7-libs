unit wsModelosNaoLineares;

(*
  IMPLEMENTA��O:
    Alex Dias Gonsales - 10/1998

  MODIFICACOES:

  AUTOR E DATA: ........................
  ESCOPO: ..............................
  OBS.: ................................

  Obs.:
  ok1 - vari�veis internas est�o sendo liberadas corretamente.
  ver - ver com Amauri.
  ok  - est� tudo ok.

  --------------------------------------------------------------------------

  Diretivas de Compila��o.

  TESTE - 21/01/99 Impress�es no gEditor para teste.


==============================================================================

  FUN��O:

  Estimativa de par�metros em modelos n�o lineares.

  FUNCIONAMENTO E UTILIZA��O:

  - Declarar um objeto do tipo TwsNLinMod.
  - Criar uma inst�ncia atrav�s de TwsNLinDeriv se for utilizar os m�todos que
    necessitam de derivadas ou TwsDUD se n�o for especificar as derivadas.
  - N�o ser� necess�rio mandar mensagem alguma para o objeto, no momento da
    cria��o do objeto, par�metros ser�o passados ao mesmo, e logo ap�s o objeto
    ser criado a avalia��o do modelo ser� realizada.

  Obs.:

  - S� pode ter uma vari�vel independente(preditora) e uma vari�vel dependente no modelo.

  - Parametro tem que ser um identificador valido e nao pode ser nem nome de
    fun��o nem um nome de alguma coluna do conjunto de entrada (DataSet).

  - Valor inicial para o parametro deve ser um numero real(float) ou inteiro.

  - Epressao da derivada pode conter fun��es, constantes,
    nomes de parametros e/ou o nome da vari�vel preditora.

  - Vari�veis de grupo: s� � permitido se a vari�vel for do tipo fator.

  - Segundo a observa��o acima, automaticamente n�o � permitido que uma
    vari�vel de grupo fa�a parte da express�o do modelo. Por�m a seguinte
    exce��o pode ocorrer:
    - Est� aceitando variavel de grupo no lado esquerdo do modelo. Ex.:
      Raiz(dose) ~ altura * b1
      onde dose � uma vari�vel de grupo (e do tipo fator consequentemente).
      Neste caso est� sendo usado o valor do �ndice do n�vel do valor
      da vari�vel no c�lculo (embora isso n�o tenha significado algum).

------------------- Operadores Reconhecidos -------------------------------

  soma               +
  subtra��o          -
  multiplica��o   * ou #
  divis�o            /
  potencia��o     ** ou ##

  Obs.: Todos os caracteres '*' (asterisco) entrados na express�o do
        modelo ou nas express�es das derivadas s�o trocados por '#' antes
        de passar para o avaliador. Isto � necess�rio porque os operadores
        '*' e '**' dever�o ser tratados como operadores elemento a elemento,
        e para o Avaliador, eles n�o s�o operadores elemento a elemento.
        Para o Avaliador, os operadores elemento a elemento correspondentes
        s�o o '#' e o '##', por isso � realizada essa troca.

----------------------- Fun��es Reconhecidas --------------------------------

  Reconhece todas as fun��es da unit uEval.


===============================================================================
=========================== Mensagens de Erro =================================
===============================================================================

------------------------------- Modelo ----------------------------------
Pop ~ raiz *ano * b1
Erro na avaliacao do modelo <Pop ~ raiz *ano * b1>.
Falta de "(".

Erro na avaliacao do modelo <Pop ~ ano * b1 +>.
Falta de identificador.

------------------------------- Derivadas -------------------------------

Raiz
Erro na avaliacao da derivada<raiz>.
Falta de "(".

Erro na avaliacao da derivada <altura2>.
Vari�vel <ALTURA2> desconhecida.

*)

{ DEFINE TESTE }

interface
Uses Classes,
     wsConstTypes,
     wsMatrix,
     wsVec,
     wsAvaliadorDeExpressoes,
     wsTabelaDeSimbolos,
     wsBXML_Output,
     wsGLib;

Const
  {Constantes utilizadas na Janela ModelNL3, servem como indice para
   a variavel bitsOpcoes(TBits)}
  cIDSFrame = 1;
  cADSFrame = 2;
  cIMatCov = 3;
  cAMatCov = 4;
  cIEstPar = 5;
  cAEstPar = 6;
  cIIter = 7;
  cAIter = 8;
  cICorr = 9;
  cACorr = 10;

  cGauss = 0;
  cHart  = 1;
  cMarq  = 2;
  cDUD   = 3;

  cNomesMetodos : Array [0..3] of string = ('Gauss-Newton',
                                            'Modificado de Hartley',
                                            'Marquardt',
                                            'DUD - Livre de Derivada');

Type

  {---------------------------------------------------------------------------}

 { Heran�a
     TwsNLinMod --> TObject
   Objetivo
     An�lise de modelos n�o lineares. Dentre suas caracter�sticas, pode-se citar:
       - Indica��o de derivadas como express�es, valores iniciais e precis�o de converg�ncia
       - Possui varios m�todos de ajustamento
       - Express�es das derivadas como fun��es gerais
       - Os operadores reconhecidos s�o:
         +    soma
         -    subtra��o
         #    produto
         ##   pot�ncia
 }


  TwsNLinMod = Class
  Private
    bitsOpcoes :TBits;      // Opcoes gerias para saidas (N�o liberar)
    CondConverg, { Resultado do processo iterativo. Condicao de convergencia
                  0 - Saiu do processo interativo e converg�ncia foi alcan�ada.
                  1 - N�o houve melhoras nas subiteracoes
                  2 - Saiu do processo interativo e converg�ncia n�o foi alcan�ada,
                      esgotou o no. de itera��es.}
    iMaxIter : Integer;     // N�mero m�ximo de itera��es permitidas
    fEps     : Double;       // Precis�o do processo iterativo
    DataSet                 // DataSet de onde ser�o escolhidos os dados e as duas colunas
                            // referentes �s vari�veis preditora e dependente
             : TwsDataSet;
    sModel,                 // Express�o do modelo
    sExprModel,          // Cont�m somente a parte da direita do modelo. Ap�s o sinal '~'
    sCondicao               // Condicao para filtro de observacoes
               : string;
    stsVarsGrupo            // Vari�veis de grupo
                 : TStrings;
    sDepName,               // Nome da vari�vel dependente. Extra�da do modelo
    sPredName               // Nome da vari�vel preditora. Extra�da do modelo
                    : String;
    vValoresIniciais        // Cont�m os valores iniciais dos parametros. N�o � liberado.
                            // � s� uma refer�ncia para o objeto passado como par�metro
                      : TwsVec;
    mBeta    {Criada no Create com 4 linhas e iNPar colunas e destru�da no Destroy.
              Matriz com 4 linhas cada uma contendo a seguinte informa��o:
              1 - Estimativas. Valores dos parametros. S�o atualizados a cada itera��o.
              2 - Erro padr�o.    (calculado em EstimateTable)
              3 - Extr. Inferior. (calculado em EstimateTable)
              4 - Extr. Superior. (calculado em EstimateTable)
              - Cada coluna cont�m o nome do parametro.
              - TwsNLinMod.EvalModel atualiza os valores dos par�metros na TabVar
                utilizando os valores contidos na linha 1 de mBeta.
              - EstimateTable calcula as linhas 2, 3 e 4 e imprime ela (sempre).
              - Para cada DSFrame, a linha 1 � reinicializada com os valores de
                vValoresIniciais.
              }
             : TwsGeneral;
    iNPar    // N�mero de parametros. � igual a mBeta.NCols.
             : Integer;
    Eval     {Objeto que ir� avaliar o modelo (sExprModel).
              Criado no Create e destru�do no Destroy. O seu atributo TabVar ir� conter os
              par�metros bem como o Vetor com os valores da vari�vel preditora
              (sPredName -> vPred). Para cada DSFrame, � deletado o vPred anterior que est�
              na sua TabVar e adicionado um novo}
             : TAvaliator;
    lstDataSets // Lista com os DataSets gerados por ModelFrame.
                // Criado no Create e destru�do no Destroy
             : TwsDataSets;
    DSFrame  {Somente uma refer�ncia para um dos DataSets gerados por ModelFrame.
              A Coluna 1 cont�m os dados da vari�vel Dependente e a
              coluna 2 cont�m os dados da vari�vel Preditora. No final do
              processo interativo o m�todo interno GeraSaidas adiciona duas
              colunas a esta vari�vel, uma com os valores de vDep e outra
              com os valores de vResid. Esta vari�vel n�o � diretamente liberada
              pois ela � apenas um ponteiro tempor�rio para um dos objetos (DataSet)
              de lstDataSets.}
             : TwsDataSet;
    iNObs    {N�mero de observa��es do conjunto sendo analisado (DSFrame).
              Setado em LoopDSFrame.}
             : integer;
    fSSRes,  {Soma dos quadrados dos res�duos.}
    fSSTot   {Soma de quadrados total n�o corrigido. Calculado em LoopDSFrame.}
             : Double;
    vPred,   {- Vetor que armazena os valores da vari�vel preditora.
              - Criado no create e adicionado em Eval.TabVar.
              - Em TwsNLinDeriv.LoopDSFrame, para cada DSFrame,
                deleta vPred anterior de Eval.TabVar, cria um novo vPred fazendo uma
                c�pia da coluna 2 de DSFrame e insere vPred em Eval.TabVar.
                N�o � liberado diretamente pois o pr�prio Eval.TabVar destr�i ele.
              Obs.: Os valores de vPred n�o s�o modificados durante o processo de itera��o.}
    vDep,    {Vetor que armazena os valores da avalia��o do modelo.
              Os valores de vDep ser�o modificados a cada itera��o,
              quando for chamado EvalModel.
              Destruido e Criado em EvalModel. Em EvalModel � destruido o
              anterior e criado um novo.
              O �ltimo vDep criado em EvalModel � destru�do em TwsNLinMod.Destroy.}
    vResid   {Vetor dos res�duos.
              Criado e calculado em EvalModel e atribu�do � �ltima linha de mX.
              N�o deve ser destru�do diretamente pois a pr�pria matriz mX j� o faz.}
             : TwsVec;
    mX,      {Matriz das derivadas. Cont�m iNPar+1 linhas e iNObs Colunas.
              A �ltima linha � um ponteiro para vResid.
              Criada e destru�da em LoopDSFrame para cada DSFrame.
              Em EvalDeriv as linhas 1 at� iNPar s�o calculadas.
              Em EvalModel, a �ltima linha de mX recebe vResid. N�o devo
              liberar a linha anterior porque isto � feito automaticamente pelo
              pr�rio objeto Matrix.}
    mIter    {Armazena os resultados do processo iterativo.
              Criada no in�cio de LoopDSFrame (com 0 linhas e iNPar + aux colunas) para cada DSFrame.
              Em cada m�todo Fit � adicionada uma linha para cada itera��o.
              Destru�da em gera_saidas se n�o foi escolhida a op��o para armazenagem.}
             : TwsGeneral;
    mS ,     {Matriz do sistema de equacoes. A ultima linha contem Delta.
              Criada no create (com iNPar+1) e destruida no destroy.}
    mCov,     {Matriz das covariancias.
              Para cada DSFrame, � criada em GetCov e destruida em GeraSaidas.
              Obs.: GetCov � chamada a partir de GeraSaidas.}
    mCorr    {Matriz de correla��es.
              Para cada DSFrame, � criada em GetCorr e destruida em GeraSaidas.
              Obs.: GetCorr � chamada a partir de GeraSaidas.}
             :TwsSymmetric;

    // Mecanismo para gera��o de relat�rios
    FOutput: TwsBXML_Output;

    // Evento notificador de cria��o de objetos
    FOnCreatedObject: TwsCreatedObject_Event;

    Procedure EvalModel;
    Procedure AnovaTable;               // Cria a matriz 'Fontes', imprime e libera em seguida.
    Procedure TranspMul(A: TwsGeneral);
    Procedure GetCov; virtual;          // Cria e calcula mCov.
    Procedure GetCorr;                  // Cria e calcula mCorr.
    Procedure EstimateTable;            // Obt�m estimativas e imprime.
    Procedure GeraSaidas(n_saida : integer);
    Procedure LoopDSFrame; virtual; abstract;
    Destructor Destroy; Override;
  Public
    Constructor Create(aDataSet:             TwsDataSet;
                       astsVarsGrupo:        TStrings;
                       asts_parametros:      TStrings;
                       av_valores_iniciais:  TwsVec;
                       const asModel:        String;
                       const asCondicao:     string;
                       aiMaxIter:            integer;
                       afEPS:                Double;
                       abitsOpcoes:          TBits;
                       aOutput:              TwsBXML_Output;
                       aCreatedObject_Event: TwsCreatedObject_Event);
  End; {TwsNLinMod}

  {---------------------------------------------------------------------------}

 { Heran�a
     TwsNLinMod --> TwsNLinDeriv --> TObject
   Objetivo
     Agrupa m�todos de ajustamento que utilizam derivadas: Gauss, modificado de Hartley e
     Marquardt
 }

  TwsNLinDeriv = Class(TwsNLinMod)
  Private
    iMetodo  {M�todo a ser utilizado para avaliar o modelo.
               cGauss - Gauss-Newton
               cHart  - Hartley
               cMarq  - Marquardt}
             : Integer;
    stsDeriv {Express�es das derivadas parciais em rela��o aos respectivos Parametros.
             Cada uma das propriedades Object da lista cont�m um objeto uEval
             que ir� avaliar a express�o da derivada. O atributo TabVar destes
             objetos uEval apontam para a TabVar de Eval.
             Criada em TwsNLinDeriv.Create e destru�da em TwsNLinDeriv.Destroy.}
             : TStrings;
    Procedure EvalDeriv;             {Avalia todas derivadas e preenche a matriz mX.}
    Procedure LoopDSFrame; override; {Para cada DSFrame faz uma chamada a um dos m�todos Fit.}
    Destructor Destroy; Override;
  Public
    // Cria objeto
    Constructor Create(aDataSet:             TwsDataSet;
                       astsVarsGrupo:        Tstrings;
                       asts_parametros:      TStrings;
                       av_valores_iniciais:  TwsVec;
                       astsDeriv:            TStrings;
                       const asModel:        String;
                       const asCondicao:     String;
                       aiMaxIter:            integer;
                       afEPS:                Double;
                       aiMetodo:             integer;
                       abitsOpcoes:          TBits;
                       aOutput:              TwsBXML_Output;
                       aCreatedObject_Event: TwsCreatedObject_Event);

    // Ajustamento pelo m�todo de Gauss-Newton
    Procedure GaussNewtonFit;
    // Ajustamento pelo m�todo modificado de Hartley
    Procedure HartleyFit;
    // Ajustamento pelo m�todo de Marquardt
    procedure MarquardtFit;
  End; {TwsNLinDeriv}

 { Heran�a
     TwsNLinMod --> TwsDUD --> TObject
   Objetivo
     Ajustamento de modelos lineares que n�o utiliza derivadas informadas explicitamente. Na
     lingua inglesa � denominado DUD - Doesn't Use Derivatives
 }

  TwsDUD = Class(TwsNLinMod)
  Private
    DP : TwsGeneral;
    Procedure LoopDSFrame; override;
    Procedure GetCov; override;
  Public
    // Cria objeto
    Constructor Create(aDataSet:             TwsDataSet;
                       astsVarsGrupo:        TStrings;
                       asts_parametros:      TStrings;
                       av_valores_iniciais:  TwsVec;
                       const asModel:        String;
                       const asCondicao:     String;
                       aiMaxIter:            integer;
                       afEPS:                double;
                       abitsOpcoes:          TBits;
                       aOutput:              TwsBXML_Output;
                       aCreatedObject_Event: TwsCreatedObject_Event);

    // Destroi objeto da mem�ria
    Destructor Destroy; override;
    // Ajustamento pelo m�todd DUD
    Procedure DUDFit;
  End;{TwsDUD}

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
implementation
Uses sysUtils,
     SysUtilsEx,
     wsFuncoesDeProbabilidade,
     wsParser,
     wsFuncoesDeDataSets,
     wsProbabilidade;


(****************************************************************************
ok1*)
Constructor TwsNLinMod.Create(aDataSet:             TwsDataSet;
                              astsVarsGrupo:        TStrings;
                              asts_parametros:      TStrings;
                              av_valores_iniciais:  TwsVec;
                              const asModel:        String;
                              const asCondicao:     string;
                              aiMaxIter:            integer;
                              afEPS:                Double;
                              abitsOpcoes:          TBits;
                              aOutput:              TwsBXML_Output;
                              aCreatedObject_Event: TwsCreatedObject_Event);
{ Objetivo
    Participa da cria��o do objeto para an�lise de modelos n�o lineares. Analisa as express�es das
    derivadas segundo a sintaxe pr�-estabelecida.
  Par�metros
    aDataSet> conjunto de dados

}


var sts_Vars  : TStrings; {Conter� os nomes das duas vari�veis que ser�o passadas para ModelFrame.
                Vari�veis 'dependente' e 'preditora', nesta ordem respectivamente.}
    parser    : TAbstractParser;
    i, c      : integer;
    s_aux     : string;

  procedure cria_parser(var parser : TAbstractParser);
  var i : integer;
  begin
    {Adiciona os operadores que o parser reconhecer�.}
    parser := TExpressionParser.Create();
    With parser.AritOpers do
      begin
      Add('+');
      Add('-');
{      Add('*');}
      Add('/');
      Add('#');
{      Add('**');}
      Add('##');
      end;
    {Adiciona as fun��es que o parser reconhecer�.}
    for i := 0 to wsAvaliadorDeExpressoes.TamTab_Fun-1 do
        Parser.Functions.Add(wsAvaliadorDeExpressoes.Tab_Fun[i]);
  end; {cria_parser}

  procedure cria_objetos;
  begin
    {locais}
    sts_Vars    := TStringList.Create;
    cria_parser(parser);
  end;

  procedure destroi_objetos;
  begin
    {locais}
    sts_vars.Free;
    parser.Free;
  end;

Begin {TwsNLinMod.Create}
  cria_objetos;

  FOutput := aOutput;
  FOnCreatedObject := aCreatedObject_Event;  

  DataSet := aDataSet;
  {Troca cada '*' por '#'. Ver coment�rios no cabe�alho desta Unit.}
  sModel := ChangeChar(asModel, '*', '#');
  sCondicao := asCondicao;
  iMaxIter := aiMaxIter;
  fEPS := afEPS;
  stsVarsGrupo := astsVarsGrupo;
  iNPar := asts_parametros.Count;
  vValoresIniciais := av_valores_iniciais;
  bitsOpcoes := abitsOpcoes;

  {Cria mS}
  mS := TwsSymmetric.Create(iNPar+1);

  {Cria a matriz mBeta}
  mBeta := TwsGeneral.Create(4, iNPar);
  mBeta.Name := 'Estatisticas';
  mBeta.MLab := 'Estimativas, erros padr�es e intervalo de confian�a (5%) assint�ticos';
  mBeta.RowName[1] := 'Estimativas';
  mBeta.RowName[2] := 'Erro padr�o';
  mBeta.RowName[3] := 'Extr. Inferior';
  mBeta.RowName[4] := 'Extr. Superior';
  for i := 1 to iNPar do
      mBeta.ColName[i] := asts_parametros[i-1];

  {criar, definir Avaliador !!!!!!!!!!!!!!!!!!!!!!!!!!!!}
  Eval := TAvaliator.Create;
  Eval.Reference:=False; {Sempre cria uma copia dos objetos retornados pelo avaliador.}
  Eval.Print:=True;

  {Inserir os parametros na tabela de simbolos.}
  for i := 1 to iNPar do
      Eval.TabVar.AddFloat(mBeta.ColName[i], 0);

  {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

  {------------------ An�lise do Modelo entrado em edModelo -------------------}
  {Separar o modelo em duas partes...}
  sDepName := sysUtilsEx.AllTrim(System.Copy(sModel, 1, System.Pos('~', sModel)-1)); {obs.: Maximo de 256 bytes.}
  sExprModel := sysUtilsEx.AllTrim(System.Copy(sModel, System.Pos('~', sModel)+1,Length(sModel)));
  sts_Vars.Add(sDepName);

  If sDepName = '' Then
  begin
    destroi_objetos;
    Raise Exception.Create('Vari�vel dependente n�o definida no modelo');
  end;

  {Se a vari�vel dependente for do tipo Factor ent�o d� erro.}
  If DataSet.IsFactor(sDepName) Then
  begin
    destroi_objetos;
    Raise Exception.Create('Vari�vel dependente do tipo Fator');
  end;
  {Obs.: Se for entrada uma express�o na vari�vel dependente, por exemplo,
         'Raiz(y) ~ X + b0' n�o acusa erro nenhum, mesmo que y seja do tipo Fator.
         Neste caso e' usado o indice do nivel para achar a raiz.}

  parser.Text.Clear;
  parser.Text.Add(sExprModel);
  parser.Scan;

  {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

  {----------------------------- Analizar o modelo ----------------------------}

  {Verifica se todos os parametros foram usados no Modelo.}
  For i := 0 to asts_parametros.Count - 1 do
    begin
      {Se o parametro nao foi usado entao d� mensagem de erro.}
      If parser.Variables.IndexOf(asts_parametros[i]) = -1 Then
      begin
        destroi_objetos;
        Raise Exception.Create('Parametro ' + asts_parametros[i] +
                               ' n�o utilizado no modelo');
      end;
    end;

  {Deleta os parametros de parser.Variables}
  For i := 0 to asts_parametros.Count - 1 do
  begin
    parser.Variables.Delete(parser.Variables.IndexOf(asts_parametros[i]));
  end;

  {------------------------ Tratamento das variaveis --------------------------}

  c := 0;
  For i := 0 to parser.Variables.Count-1 do
  begin
    inc(c);
    {Se a vari�vel n�o existir no DataSet...}
    If DataSet.Struct.IndexOf(parser.Variables[i]) = -1 Then
    begin
      s_aux := parser.Variables[i];
      destroi_objetos;
      Raise Exception.Create('Vari�vel '+ s_aux +
                             ' n�o existe no conjunto de dados');
    end;
    {Se tiver mais de uma vari�vel no modelo ent�o d� erro.}
    If c > 1 Then
    begin
      destroi_objetos;
      Raise Exception.Create('Existe mais de uma vari�vel no modelo');
    end;
    {Se a vari�vel independente for igual � vari�vel dependente ent�o d� mensagem de erro.}
    If parser.Variables[i] = sDepName Then
    begin
      destroi_objetos;
      Raise Exception.Create('Vari�vel independente igual � vari�vel dependente');
    end;
    {Se a vari�vel n�o for num�rica ent�o d� uma mensagem de erro.}
    If DataSet.isFactor(parser.Variables[i]) Then
    begin
      s_aux := parser.Variables[i];
      destroi_objetos;
      Raise Exception.Create('A vari�vel ' + s_aux +
                             ' n�o � do tipo num�rica');
    end;
  end;

  {Se n�o tiver nenhuma vari�vel no modelo ent�o d� erro.}
  If c = 0 Then
  begin
    destroi_objetos;
    Raise Exception.Create('Aus�ncia da vari�vel independente no modelo');
  end;

  sPredName := parser.Variables[0];
  {Tudo Ok, ent�o adiciona a vari�vel preditora � sts_Vars.}
  sts_Vars.Add(sPredName);

  {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

  {------------------- Gera conjuntos de dados (ModelFrame) -------------------}

  {Gera os conjuntos (lstDataSets) dos quais ser�o feitas as analises.}
  try
    lstDataSets := ModelFrame(sts_Vars, stsVarsGrupo, sCondicao, DataSet);
  except
     on e: Exception do
        begin
        destroi_objetos;
        raise;
        end;
  end;
  {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

  {Cria e inicializa vPred, e adiciona o mesmo � Tabela De Simbolos(TabVar).
   Se eu n�o fizer isso, ent�o 'Eval.Epression := sExprModel' vai dizer que a vari�vel
   preditora n�o existe.}
  vPred := TwsDFVec.Create(0);
  Eval.TabVar.AddVector(sPredName, vPred);

  Try
    Eval.Expression := sExprModel;
  Except
    On E : Exception Do
       begin
       destroi_objetos;
       {N�o preciso destruir vPred pois o pr�prio Eval j� destr�i ele.}
       Raise Exception.Create('Problemas na avalia��o do modelo '+
                              '<' + sExprModel + '>.'#13 + E.Message);
       end;
  End;

End; {TwsNLinMod.Create}

(****************************************************************************
ok1*)
Destructor TwsNLinMod.Destroy;
Begin
  mBeta.Free;
  Eval.Free;
  mS.Free;
  vDep.Free;
  lstDataSets.free; // <<<<<
End; {TwsNLinMod.Destroy}

(****************************************************************************
Atualiza os parametros na TabVar.
Libera vDep anterior e cria um novo. Calcula o modelo (vDep).
Calcula os residuos (cria vResid) e fSSRes.
Atribui vResid � �ltima linha de mX.
ok
*)
Procedure TwsNLinMod.EvalModel;
var i : integer;
    res : TEvalResult;
Begin
  {Atualiza parametros na TabVar}
  for i := 1 to mBeta.NCols do
      Eval.TabVar.SetFloatValue(mBeta.ColName[i], mBeta[1,i]);

  vDep.Free;
  vDep := Nil;

  {Calcula Modelo.}
  res := Eval.Evaluate;
  vDep := res.AsVector;

  {Calcula res�duo e soma dos quadrados dos res�duos.}
  vResid := TwsDFVec.Create(iNObs);
  fSSRes := 0;
  for i := 1 to iNObs do
      begin
      vResid[i] := DSFrame[i,1] - vDep[i];
      fSSRes := fSSRes + Sqr(vResid[i]);
      end;
  mX.Row[mX.NRows] := vResid;

End; {TwsNLinMod.EvalModel}

(*****************************************************************************)
{ faz o produto de uma matriz pela sua transposta.
ver ???}
Procedure TwsNLinMod.TranspMul(A: TwsGeneral);
var
  i,j,k: Integer;
  v: double;
begin
  for i := 1 to A.NRows do
    begin
    for j := 1 to i do
      begin
      v := 0;
      for k := 1 to A.NCols do
        v := v + A[i, k]*A[j, k];
      mS[i,j] := v;
      end
    end;
end; { TwsNLinMod.TranspMul }

(*****************************************************************************)
{ver ???}
Procedure TwsNLinMod.EstimateTable;
var v : TwsVec;
    i : integer;
    f : double;
    erro : word;
Begin
  f := FInv(0.05, 1, iNObs-iNPar, 1.0E-10, True, Erro);
  for i := 1 to iNPar do
    begin
    mBeta[2,i] := sqrt(mCov[i,i]);
    mBeta[3,i] := mBeta[1,i] - sqrt(f * mCov[i,i]);
    mBeta[4,i] := mBeta[1,i] + sqrt(f * mCov[i,i]);
    end;

  FOutput.Add(mBeta); 
End;

(*****************************************************************************)
{ver ???}
Procedure TwsNLinMod.AnovaTable;
var m : TwsGeneral;
Begin
  m := TwsGeneral.Create(3,3);
  m.Name := 'Fontes';
  m.MLab := 'Quadro da an�lise da varia��o';
  m.RowName[1] := 'Regress�o';
  m.RowName[2] := 'Res�duo';
  m.RowName[3] := 'Total';
  m.ColName[1] := 'GL';
  m.ColName[2] := 'SQ';
  m.ColName[3] := 'QM';
  m[1,1] := iNPar;
  m[1,2] := fSSTot - fSSRes;
  m[1,3] := m[1,2] / iNPar;
  m[2,1] := iNObs - iNPar;
  m[2,2] := fSSRes;
  m[2,3] := fSSRes / m[2,1];
  m[3,1] := iNObs;
  m[3,2] := fSSTot;
  m[3,3] := wscMissValue;
  FOutput.Add(m); 
  m.Free;
End; {TwsNLinMod.AnovaTable}

(****************************************************************************
ver ???*)
procedure TwsNLinMod.GetCov;
var j : integer;
Begin
  {Cria mCov.}
  mCov := TwsSymmetric(mS.SubSymMat(1, iNPar));
  mCov.Name := 'Covar';
  mCov.MLab := 'Covari�ncias assint�ticas entre as estimativas';
  for j := 1 to iNPar do
      begin
      mCov.RowName[j] := mBeta.ColName[j];
      mCov.ColName[j] := mBeta.ColName[j];
      end;
  mCov.ByScalar(fSSRes/(iNObs-iNPar),opProd,False,False);
End; {TwsNLinMod.GetCov}

(****************************************************************************
ver ???*)
Procedure TwsNLinMod.GetCorr;
var i, j : integer;
Begin
  mCorr := TwsSymmetric.Create(mCov.NCols);
  mCorr.Name := 'Correl';
  mCorr.MLab := 'Correla��es assint�ticas entre as estimativas';
  for j := 1 to iNPar do
    begin
    mCorr.RowName[j] := mBeta.ColName[j];
    mCorr.ColName[j] := mBeta.ColName[j];
    end;

  for i := 1 to mCov.NCols do
    for j := 1 to i do
      if j <> i then
        mCorr[j,i] := mCov[i,j] / Sqrt( mCov[i,i] * mCov[j,j] );
  for i := 1 to mCorr.NCols do
    mCorr[i,i] := 1;
End; {TwsNLinMod.GetCorr}

(****************************************************************************
ok
Entrada:
n_saida - N�mero que ser� adicionado ao final do nome dos conjuntos gerados.
          � o �ndice de lstDataSets para o qual DSFrame est� apontando.

Fun��o:
   Adiciona as colunas 'Ajustado' e 'Residuo' em DSFrame.
   Chama AnovaTable; GetCov; GetCorr e EstimateTable;
   Imprime, Armazena ou Libera: DSFrame, mIter, mCov e mCorr.
*)
Procedure TwsNLinMod.GeraSaidas(n_saida : integer);
var
  j : integer;
Begin
  {Adiciona duas colunas em DSFRame.}
  DSFrame.Struct.AddCol(TwsNumeric.Create('Ajustado', 'Valores ajustados'));
  DSFrame.Struct.AddCol(TwsNumeric.Create('Residuo', 'Res�duos do ajustamento'));
  for j := 1 to DSFrame.NRows do
    begin
    DSFRame[j,3] := vDep[j];
    DSFRame[j,4] := vResid[j];
    end;

  {================ Impress�es, Armazenamentos/libera��es ================}

  {--------------------- Conjunto de Dados (DSFrame) ---------------------}

  if BitsOpcoes[cIDSFrame] then
     FOutput.Add(DSFrame);

  if BitsOpcoes[cADSFrame] then
     FOnCreatedObject(self, DSFrame.Copy);

  {Obt�m quadro da analise da variacao, matriz de Covariancias e correlacoes.}
  AnovaTable;
  GetCov;
  GetCorr;
  EstimateTable;

  {------------------------ Matriz das Itera��es ------------------------}

  if BitsOpcoes[cIIter] then
     FOutput.Add(mIter);

  if BitsOpcoes[cAIter] then
     begin
     mIter.Name := mIter.Name + intToStr(n_saida);
     FOnCreatedObject(self, mIter);
     end
  else
    begin
    mIter.Free;
    mIter := Nil;
    end;

  {----------------------- Matriz das Covariancias ----------------------}

  if BitsOpcoes[cIMatCov] then
     FOutput.Add(mCov);

  if BitsOpcoes[cAMatCov] then
     begin
     mCov.Name := mCov.Name + intToStr(n_saida);
     FOnCreatedObject(self, mCov);
     end
  else
    begin
    mCov.Free;
    mCov := Nil;
    end;

  {--------- Matriz das Correla��es entre estimativas dos parametros ----}

  if BitsOpcoes[cICorr] then
     FOutput.Add(mCorr);

  if BitsOpcoes[cACorr] then
     begin
     mCorr.Name := mCorr.Name + intToStr(n_saida);
     FOnCreatedObject(self, mCorr);
     end
  else
    begin
    mCorr.Free;
    mCorr := Nil;
    end;

  FOutput.BeginText;
  Case CondConverg of
    0 : FOutput.Warning('Converg�ncia alcan�ada');
    1 : FOutput.Warning('N�o houve melhoria ap�s 10 sub-itera��es');
    2 : FOutput.Warning('N�mero de itera��es insuficientes para converg�ncia');
  End;
  FOutput.EndText;

End; {TwsNLinMod.GeraSaidas}

(****************************************************************************
*)
Constructor TwsNLinDeriv.Create(aDataSet:             TwsDataSet;
                                astsVarsGrupo:        Tstrings;
                                asts_parametros:      TStrings;
                                av_valores_iniciais:  TwsVec;
                                astsDeriv:            TStrings;
                                const asModel:        String;
                                const asCondicao:     String;
                                aiMaxIter:            integer;
                                afEPS:                Double;
                                aiMetodo:             integer;
                                abitsOpcoes:          TBits;
                                aOutput:              TwsBXML_Output;
                                aCreatedObject_Event: TwsCreatedObject_Event);

var
  eval_temp : TAvaliator;
  i, j      : Integer;
  st        : string;
Begin
  {Inicializa��es.}
  eval_temp := Nil;

  Inherited Create(aDataSet, astsVarsGrupo, asts_parametros, av_valores_iniciais,
                   asModel, asCondicao, aiMaxIter, afEPS, abitsOpcoes, aOutput,
                   aCreatedObject_Event);

  iMetodo := aiMetodo;
  stsDeriv := TStringList.Create;
  for i := 0 to astsDeriv.Count - 1 do     {Adiciona � stsDeriv, trocando cada '*' por '#'}
      stsDeriv.Add(ChangeChar(astsDeriv[i], '*', '#'));

  {Criar uma Lista de Uevals para cada expressao de derivada.}
  For i := 0 to stsDeriv.Count - 1 do
      begin
      eval_temp := TAvaliator.Create;
      eval_temp.Reference := False;
      eval_temp.Print := True;
      eval_temp.TabVar := Eval.TabVar;
      Try
        eval_temp.Expression := stsDeriv[i];
      Except
        On E : Exception Do
           begin
           {libera os objetos da lstEvalDeriv}
           {ver com adriano ...}
           Raise Exception.Create('Problemas na avalia��o da derivada '+
                                  '<' + stsDeriv[i] + '>.'#13 + E.Message);
           end;
      End;
      stsDeriv.Objects[i] := eval_temp;
      end; {for}

  {-------------------- Imprime cabe�alho da an�lise --------------------------}

  FOutput.BeginText;
  if DataSet.MLab <> '' then
    FOutput.WritePropValue('Conjunto de Dados:', DataSet.Name+' - '+DataSet.MLab)
  else
    FOutput.WritePropValue('Conjunto de Dados:', DataSet.Name);
  if asCondicao <> '' then
    FOutput.WritePropValue('Filtro:', asCondicao);
  if astsVarsGrupo.Count>0 then
    begin
    st:='';
    for i:=0 to astsVarsGrupo.Count-1 do
      st:=st+astsVarsGrupo[i]+', ';
    Delete(st,Length(st)-1,2);
    FOutput.WritePropValue('Vari�veis de Grupo:', st);
    end;
  FOutput.WritePropValue('Modelo:', asModel);
  for j := 0 to astsDeriv.Count - 1 do
    FOutput.WritePropValue('Der('+mBeta.ColName[j+1]+'): ', astsDeriv[j]);
  FOutput.WritePropValue('M�todo de ajustamento:',cNomesMetodos[iMetodo]);
  FOutput.WritePropValue('N�mero M�ximo de Itera��es:', IntToStr(aiMaxIter));
  FOutput.WritePropValue('Precis�o Para Converg�ncia:', FloatToStr(afEPS));
  FOutput.EndText;

  {Faz a an�lise para cada DataSet(DSFrame) gerado por ModelFrame.}
  LoopDSFrame;
End; {TwsNLinDeriv.Create}

(****************************************************************************
ok1*)
Destructor TwsNLinDeriv.Destroy;
Begin
  Inherited Destroy;

  {Libera stsDeriv e seus objetos associados.}
  if stsDeriv <> Nil Then
     begin
     While stsDeriv.Count <> 0 do
           begin
           TAvaliator(stsDeriv.objects[0]).free;
           stsDeriv.delete(0);
           end;
     stsDeriv.free;
     end;

End; {TwsNLinDeriv.Destroy}

(**************************************************************************
ok
Para cada DataSet(DSFrame) em lstDataSets faz:
   Faz DSFrame apontar para o DataSet correto.
   Seta iNObs.
   Inicializa mBeta com os valores iniciais.
   Deleta vPred anterior de Eval.TabVar, cria um novo vPred e adiciona em Eval.Tabvar
   Cria mX (a matriz das derivadas).
   Chama EvalModel.
   Chama o m�todo Fit correspondente a iMetodo.
   Chama GeraSaidas(i).
   Libera mX.
*)
Procedure TwsNLinDeriv.LoopDSFrame;
var
  i,j : integer;
begin
  {---------------------- La�o para cada lstDataSets --------------------------}
  for i := 0 to lstDataSets.Count - 1 do
    begin
    DSFrame := lstDataSets[i];
    iNObs := DSFrame.NRows;

    {Inicializa mBeta com os valores iniciais.}
    for j := 1 to iNPar do
      mBeta[1,j] := vValoresIniciais[j];
    mX := TwsGeneral.Create(iNPar + 1, iNObs);

    {Deleta vPred anterior e cria um novo.}
    Eval.TabVar.DeleteVar(sPredName);
    fSSTot := 0;
    vPred := TwsDFVec.Create(iNObs);
    for j := 1 to iNObs do
      begin
      vPred[j] := DSFrame[j, 2];
      fSSTot := fSSTot + sqr(DSFrame[j, 1]);
      end;

    {Inserir vPred na tabela de s�mbolos com nome sPredName.}
    Eval.TabVar.AddVector(sPredName, vPred);

    // Inicializa matriz de informacoes dos passos e chama o metodo adequado
    Case iMetodo of
      cGauss:
        begin
        mIter := TwsGeneral.Create(0,iNPar+1);
        with mIter do
          begin
          Name := 'GaussNewt';
          MLab := 'Resultados do Processo Iterativo';
          ColName[1] := 'SQRes';
          for j := 1 to iNPar do
            ColName[j+1] := mBeta.ColName[j];
          end;
        GaussNewtonFit;
        end;
      cHart :
        begin
        mIter := TwsGeneral.Create(0,iNPar+1);
        with mIter do
          begin
          Name := 'Hartley';
          MLab := 'Resultados do Processo Iterativo';
          ColName[1] := 'SQRes';
          for j := 1 to iNPar do
            ColName[j+1] := mBeta.ColName[j];
          end;
        HartleyFit;
        end;
      cMarq :
        begin
        mIter := TwsGeneral.Create(0,iNPar+2);
        with mIter do
          begin
          Name := 'Marquardt';
          MLab := 'Resultados do Processo Iterativo';
          ColName[1] := 'Lambda';
          ColName[2] := 'SQRes';
          for j := 1 to iNPar do
            ColName[j+2] := mBeta.ColName[j];
          end;
        MarquardtFit;
        end;
      end; //case

    GeraSaidas(i);
    mX.Free;
    mX := Nil;
    end; {for}
end; {TwsNLinDeriv.LoopDSFrame}


(****************************************************************************
ok
Calcula as express�es das derivadas e atualiza as linhas das derivadas
na matriz mX.
*)
Procedure TwsNLinDeriv.EvalDeriv;
var i, j : integer;
    res  : TEvalResult;
    v    : TwsVec;
Begin
  for i := 0 to stsDeriv.Count - 1 do
      Begin
      res := TAvaliator(stsDeriv.Objects[i]).Evaluate;
      case res.ResultType of
           {Se o resultado da avalia��o da derivada for um vetor ent�o eu
            simplesmente atribuo esse vetor � linha i+1 de mX.}
           vtVector:
             {Obs.: O vetor anterior � automaticamente liberado quando eu fa�o a atribui��o.}
             mX.Row[i+1] := res.AsVector;

           {Se o resultado da avalia��o da derivada for um n�mero ent�o eu crio
            um vetor com tamanho mX.NCols e inicializo todas as posi��es dele com
            esse valor que foi retornado e a� atribuo esse vetor � linha i+1 de mX.}
           vtNumeric:
             begin
             v := TwsDFVec.Create(mX.NCols);
             for j := 1 to v.Len do
                 v[j] := res.AsFloat;
             mX.Row[i+1] := v; {mX.Row[i+1] anterior � automaticamente liberada.}
             end;
      end; {case}
      End; {for}
End; {TwsNLinDeriv.EvalDeriv}

(****************************************************************************
ok1
*)
Procedure TwsNLinDeriv.GaussNewtonFit;
var iter,                                 // N�mero de itera��es
    j      : Integer;
    EPS    : double;
    v_iter,                               // Informa��es sobre uma itera��o
    v_tol  : TwsVec;                      // Tolerancia para o sweep
    v      : TwsLIVec;                    // Controla colunas operadas pelo sweep
Begin

  {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
  atribui vResid � �ltima linha de mX.}
  EvalModel;

  {Inicializa��es, cria��es.}
  v_iter:=Nil; v_tol:=Nil; v:=Nil;
  v_tol := TwsDFVec.Create(mX.NRows);
  v := TwsLIVec.Create(mX.NRows);

  {Adiciona uma linha em mIter.}
  v_iter := TwsDFVec.Create(iNPar+1);
  v_Iter.Name := 'Passo_0';
  v_iter[1] := fSSRes;
  for j := 1 to iNPar do
      v_iter[j+1] := mBeta[1,j];
  mIter.MAdd(v_iter);

  {----------------------------- Processo Iterativo ---------------------------}
  EPS := 1;
  CondConverg := 0;
  for iter:=1 to iMaxIter do
    begin
    if EPS <= fEPS then Break;       // Se convergiu cai fora do processo iterativo
    {Calcula as derivadas.}
    EvalDeriv;
    {Atualiza a matriz mS do sistema de equa��es.}
    TranspMul(mX);{XX'}
    for j := 1 to v_tol.Len do
      begin
      v_tol[j] := 1.0E-9;         //mS[j,j] * 1.0E-9;
      v[j] := 1;
      end;
    for j := 1 to mS.NCols-1 do
      mS.RevSweep(j, v_tol[j], v);
    for j := 1 to iNPar do
      mBeta[1,j] := mBeta[1,j] + mS[mS.NRows,j];

    {determina o maior valor absoluto da ultima linha de mS.}
    EPS := Abs(mS[mS.NRows, 1]);
    for j := 2 to iNPar do
      if EPS < Abs(mS[mS.NRows, j]) then
        EPS := Abs(mS[mS.NRows, j]);

    {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
    atribui vResid � �ltima linha de mX.}
    EvalModel;

    {Adiciona uma linha em mIter.}
    v_iter := TwsDFVec.Create(iNPar+1);
    v_Iter.Name := 'Passo_'+IntToStr(iter);
    v_iter[1] := fSSRes;
    for j := 1 to iNPar do
      v_iter[j+1] := mBeta[1,j];
    mIter.MAdd(v_iter);
  end;
  if (iter > iMaxIter) then
    CondConverg := 2; // Numero de iteracoes insuficientes

  {Libera��es.}
  v_tol.Free;
  v.Free;
End; {GaussNewtomFit}

(****************************************************************************
ok1
*)
Procedure TwsNLinDeriv.HartleyFit;
var
  iter,                                       // Contador para o n�mero de itera��es
  subit,                                      // Contador para o n�mero de sub-itera��es
  j : Integer;
  erro : word;
  last_sse, EPS, lambda : double;              // Auxiliares para o processo iterativo
  v: TwsLIVec;                                // Controla colunas operadas pelo sweep
  v_iter,                                     // Linha com informa��es sobre uma itera��o
  v_tol: TwsVec;                              // Tolerancias para aplicacao do sweep
  m_old_beta : TwsGeneral;                    // Valores dos parametros no passo anterior
Begin
  {Inicializa��es, cria��es.}
  v_iter:=Nil; v:=Nil; v_tol:=Nil; m_old_beta:=Nil;
  v := TwsLIVec.Create(mX.NRows);
  v_tol := TwsDFVec.Create(mX.NRows);
  lambda := 1e-8;

  {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
  atribui vResid � �ltima linha de mX.}
  EvalModel;

  {Adiciona uma linha em mIter.}
  v_iter := TwsDFVec.Create(iNPar+1);
  v_Iter.Name:='Passo_0_0';
  v_iter[1] := fSSRes;
  for j := 1 to iNPar do
      v_iter[j+1] := mBeta[1,j];
  mIter.MAdd(v_iter);

  {----------------------------- Processo Iterativo ---------------------------}
  EPS := 1;
  CondConverg := 0;
  for iter:=1 to iMaxIter do
    begin
    if EPS <= fEPS then Break;            // Se convergiu cai fora do processo iterativo
    {Calcula as derivadas.}
    EvalDeriv;
    last_sse := fSSRes;
    {Atualiza a matriz mS do sistema de equa��es.}
    TranspMul(mX);{XX'}
    for j := 1 to iNPar do
      mS[j,j] := mS[j,j] + lambda * mS[j,j];
    for j := 1 to v_tol.Len do
      begin
      v_tol[j] := 1.0E-9;//mS[j,j] * 1.0E-9;
      v[j] := 1;
      end;
    for j := 1 to mS.NCols-1 do
        mS.RevSweep(j, v_tol[j], v);
    mBeta.Copy(mtGeneral,TwsMatrix(m_old_beta));
    for j := 1 to iNPar do
      mBeta[1,j] := mBeta[1,j] + mS[mS.NRows,j];
    {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
    atribui vResid � �ltima linha de mX.}
    EvalModel;
    EPS := Abs(last_sse - fSSRes)/(fSSRes + 1E-6);
    {Adiciona uma linha em mIter.}
    v_iter := TwsDFVec.Create(iNPar+1);
    v_Iter.Name:='Passo_'+IntToStr(Iter)+'_0';
    v_iter[1] := fSSRes;
    for j := 1 to iNPar do
      v_iter[j+1] := mBeta[1,j];
    mIter.MAdd(v_iter);
    {Hartley subiterations.}
    for subit:=1 to 10 do
      begin
      if (fSSRes <= last_sse) then Break;                // Se convergiu, cai fora
      for j := 1 to mS.NCols do
        mS[mS.NRows,j] := mS[mS.NRows,j] * 0.5;
      for j := 1 to iNPar do
        mBeta[1,j] := m_old_beta[1,j] + mS[mS.NRows,j];

      {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
      atribui vResid � �ltima linha de mX.}
      EvalModel;

      {Adiciona uma linha em mIter. Referente � sub-itera��o.}
      v_iter := TwsDFVec.Create(iNPar+1);
      v_iter[1] := fSSRes;
      for j := 1 to iNPar do
        v_iter[j+1] := mBeta[1,j];
      v_Iter.Name:='Passo_'+IntToStr(Iter)+'_'+IntToStr(Subit);
      mIter.MAdd(v_iter);
      end; {subit}

    {Se esgotou a �ltima sub-itera��o e ainda n�o convergiu ent�o � erro.}
    if (subit > 10) then
      begin
      Eps:=0;
      CondConverg := 1                    // Subiteracoes nao melhoraram o proceso
      end;
    m_old_beta.Free;
    m_old_beta := Nil;
    end;  // for iter
  if (iter > iMaxIter) then CondConverg := 2;           // Iteracoes insuficientes
  {Libera��es.}
  v_tol.Free;
  v.Free;
End; {HartleyFit}

(****************************************************************************
ok1
*)
Procedure TwsNLinDeriv.MarquardtFit;
var iter,                            // Contador para o n�mero de itera��es
    subit,                           // Contador para o n�mero de sub-itera��es
    j : Integer;
    erro : word;
    last_sse, EPS, lambda : double;  // valores intermediarios para o processo iterativo
    v: TwsLIVec;                    // Indicadores de linhas operadas pelo sweep
    v_iter,                         // Uma linha com informa��es sobre uma itera��o
    v_tol: TwsVec;                  // Tolerancias para o sweep
    m_old_beta : TwsGeneral;        // Mant�m uma c�pia de mBeta
Begin
  {Inicializa��es, cria��es.}
  v_iter:=Nil; v:=Nil; v_tol:=Nil; m_old_beta:=Nil;
  v := TwsLIVec.Create(mX.NRows);
  v_tol := TwsDFVec.Create(mX.NRows);

  {Avalia o modelo, atualiza parametros, obtem residuos, etc.}
  EvalModel;

  lambda := 1e-8;
  // Insere linha correspondente ao passo 0 do processo iterativo
  v_iter := TwsDFVec.Create(iNPar+2);
  v_Iter.Name:='Passo_0_0';
  v_iter[1] := lambda;
  v_iter[2] := fSSRes;
  for j := 1 to iNPar do
      v_iter[j+2] := mBeta[1,j];
  mIter.MAdd(v_iter);

  {----------------------------- Processo Iterativo ---------------------------}
  EPS := 1;
  CondConverg:=0;
  for iter:=1 to iMaxIter do
    begin
    if eps<=FEps then Break;              // Se convergiu, cai fora

    EvalDeriv;                             // Calcula as derivadas
    last_sse := fSSRes;                    // Ultimo valor da SQResiduo
    TranspMul(mX);{XX'}
    for j := 1 to iNPar do                 // X'X com a diagonal inflada
      mS[j,j] := mS[j,j] + lambda*mS[j,j];

    for j := 1 to v_tol.Len do            // Prepara para aplicar o sweep
      begin
      v_tol[j] := 1e-9;        //mS[j,j] * 1.0E-9;
      v[j] := 1;
      end;
    for j := 1 to mS.NCols-1 do           // Aplica o sweep. Solucao na ultima linha
      mS.RevSweep(j, v_tol[j], v);

    mBeta.Copy(mtGeneral,TwsMatrix(m_old_beta));  // Copia estimativas antigas
    for j := 1 to iNPar do                      // Aplica correcao
      mBeta[1,j] := mBeta[1,j] + mS[mS.NRows,j];

    {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
    atribui vResid � �ltima linha de mX.}
    EvalModel;

    if fSSRes < last_sse then lambda := lambda / 10;
    EPS := Abs(last_sse - fSSRes)/(fSSRes + 1E-6);  // Criterio de convergencia

    {Adiciona uma linha em mIter.}
    v_iter := TwsDFVec.Create(iNPar+2);
    v_Iter.Name:='Passo_'+IntToStr(Iter)+'_0';
    v_iter[1] := lambda; v_iter[2] := fSSRes;
    for j := 1 to iNPar do
      v_iter[j+2] := mBeta[1,j];
    mIter.MAdd(v_iter);

    for subit := 1 to 10 do
      begin
      if (fSSRes <= last_sse) then Break;
      lambda := lambda*10;
      TranspMul(mX);{XX'}
      for j := 1 to iNPar do
        mS[j,j] := mS[j,j] + lambda*mS[j,j];

      for j := 1 to v_tol.Len do
        begin
        v_tol[j] := 1.0E-9; //mS[j,j] * 1.0e-9
        v[j] := 1;
        end;
      for j := 1 to mS.NCols-1 do
        mS.RevSweep(j, v_tol[j], v);

      for j := 1 to iNPar do
        mBeta[1,j] := m_old_beta[1,j] + mS[mS.NRows,j];

      {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
      atribui vResid � �ltima linha de mX.}
      EvalModel;

      {Adiciona uma linha em mIter. Referente � sub-itera��o.}
      v_iter := TwsDFVec.Create(iNPar+2);
      v_Iter.Name:='Passo_'+IntToStr(Iter)+'_'+IntToStr(Subit);
      v_iter[1] := lambda;
      v_iter[2] := fSSRes;
      for j := 1 to iNPar do
        v_iter[j+2] := mBeta[1,j];
      mIter.MAdd(v_iter);
      end; // for para cada sub-iteracao

    if subit>10 then
      begin
      CondConverg:=1;
      Eps:=0;
      end;
    m_old_beta.Free;
    m_old_beta := Nil;
    end; // for para cada iteracao

  if iter > iMaxIter then CondConverg:=2;
  v_tol.Free;
  v.Free;
End; {MarquardtFit}


(****************************************************************************
ok.*)
Constructor TwsDUD.Create(aDataSet:             TwsDataSet;
                          astsVarsGrupo:        TStrings;
                          asts_parametros:      TStrings;
                          av_valores_iniciais:  TwsVec;
                          const asModel:        String;
                          const asCondicao:     String;
                          aiMaxIter:            integer;
                          afEPS:                double;
                          abitsOpcoes:          TBits;
                          aOutput:              TwsBXML_Output;
                          aCreatedObject_Event: TwsCreatedObject_Event);

var
  st: string;
  i : Integer; 
Begin
  Inherited Create(aDataSet,astsVarsGrupo,asts_parametros,av_valores_iniciais,
                   asModel,asCondicao,aiMaxIter,afEPS,abitsOpcoes,aOutput,
                   aCreatedObject_Event);

  {-------------------- Imprime cabe�alho da an�lise --------------------------}
  FOutput.BeginText;
  if DataSet.MLab <> '' then
    FOutput.WritePropValue('Conjunto de Dados:', DataSet.Name+' - '+DataSet.MLab)
  else
    FOutput.WritePropValue('Conjunto de Dados:', DataSet.Name);
  if asCondicao <> '' then
    FOutput.WritePropValue('Filtro:', asCondicao);
  if astsVarsGrupo.Count>0 then
    begin
    st:='';
    for i:=0 to astsVarsGrupo.Count-1 do
      st:=st+astsVarsGrupo[i]+', ';
    Delete(st,Length(st)-1,2);
    FOutput.WritePropValue('Vari�veis de Grupo:', st);
    end;
  FOutput.WritePropValue('Modelo:', asModel);
  FOutput.WritePropValue('M�todo de ajustamento:', cNomesMetodos[cDUD]);
  FOutput.WritePropValue('N�mero M�ximo de Itera��es:', IntToStr(aiMaxIter));
  FOutput.WritePropValue('Precis�o Para Converg�ncia:', FloatToStr(afEPS));
  FOutput.EndText;
  LoopDSFrame;
End;{TwsDUD.Create}


Destructor TwsDUD.Destroy;
begin
  DP.Free;
  inherited Destroy
end;

procedure TwsDUD.GetCov;
var
  j : integer;
  Err: Word;
  mAux: TwsSymmetric;
Begin
  {Cria mCov.}
  mAux := TwsSymmetric(mS.SubSymMat(1, iNPar));
  mCov:=TwsSymmetric(mAux.TranspMul10(DP,Err));
  mAux.Free;
  mCov.Name := 'Covar';
  mCov.MLab := 'Matriz de Covari�ncias das estimativas';
  for j := 1 to iNPar do
      begin
      mCov.RowName[j] := mBeta.ColName[j];
      mCov.ColName[j] := mBeta.ColName[j];
      end;
  mCov.ByScalar(fSSRes/(iNObs-iNPar),opProd,False,False);
End; {TwsNLinMod.GetCov}

(****************************************************************************
ok1
*)
Procedure TwsDUD.DUDFit;
var m_old_beta, BB, FF: TwsGeneral;
    i, j, iter, subit : integer;
    erro : word;
    eps, old_fSSRes : double;
    v_iter, {Uma linha com informa��es sobre uma itera��o. N�o dever� ser destru�do.}
    v_sss, v_delta, old_vDep, v_tol: TwsVec;
    v_ind, v: TwsLIVec;
Begin
  // Cria estruturas de armazenamento temporario
  BB := TwsGeneral.Create(iNPar, iNPar);
  BB.Name := 'BB';
  DP := TwsGeneral.Create(iNPar, iNPar);
  DP.Name := 'DP';
  FF :=  TwsGeneral.Create(iNObs, iNPar);
  FF.Name := 'FF';
  v_delta := TwsDFVec.Create(iNPar);
  v_delta.Name := 'v_delta';
  v_sss := TwsDFVec.Create(iNPar);
  v_sss.Name := 'v_sss';
  v_tol := TwsDFVec.Create(mX.NRows);
  v_tol.Name := 'v_tol';
  v := TwsLIVec.Create(mX.NRows);
  v.Name := 'v';

  {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
  atribui vResid � �ltima linha de mX.}
  EvalModel;


  for i := 1 to iNPar do
    begin
    for j := 1 to iNPar do
        mBeta[1,j] := vValoresIniciais[j];
    mBeta[1,i] := vValoresIniciais[i] * 1.01;

    for j := 1 to iNPar do
      BB[j,i] := mBeta[1,j];

    {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
    atribui vResid � �ltima linha de mX.}
    EvalModel;

    v_sss[i] := fSSRes;
    for j := 1 to iNObs do
        FF[j,i] := vDep[j];
    end;

  v_ind := v_sss.QuickIndx(false);
  v_ind.Name := 'v_ind';

  FF.SortCol(v_ind);
  BB.SortCol(v_ind);

  {----- Prepare for iterations -----}
  {inclu� este for.}
  for j := 1 to iNPar do
    mBeta[1,j] := vValoresIniciais[j];

  {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
  atribui vResid � �ltima linha de mX.}
  EvalModel;

  eps := 1;

  {Cria a primeira linha de mIter.}
  v_iter := TwsDFVec.Create(iNPar+2);
  v_Iter.name:='Passo_0_0';
  v_iter[1] := 0;
  v_iter[2] := fSSRes;
  for j := 1 to iNPar do
      v_iter[j+2] := mBeta[1,j];
  mIter.MAdd(v_iter);

  // Inicia o processo iterativo
  for iter := 1 to iMaxIter do
    begin
    if Eps <= fEps then Break;

    mBeta.Copy(mtGeneral,TwsMatrix(m_old_beta));

    old_fSSRes := fSSRes;
    old_vDep := vDep.Copy(1, vDep.Len);

    for i := 1 to mX.NRows-1 do
      for j := 1 to mX.NCols do
        mX[i,j] := FF[j,i] - vDep[j];

    TranspMul(mX);{XX'}

    for j := 1 to v_tol.Len do
      begin
      v_tol[j] := 1.0E-9;                //mS[j,j] * 1.0E-9;
      v[j] := 1;
      end;

    for j := 1 to mS.NCols-1 do
        mS.RevSweep(j, v_tol[j], v);

    {DP := B - mBeta}
    for i := 1 to iNPar do
      for j := 1 to iNPar do
        DP[i,j] := BB[i,j] - mBeta[1,i];

    {Encontra o vetor de corre��o. Delta = DP * Alpha}
    for i := 1 to iNPar do
      begin
      v_delta[i] := 0;
      for j := 1 to iNPar do
        v_delta[i] := v_delta[i] + DP[i,j] * mS[mS.NRows,j];
      end;

    {B = B + DELTA}
    for j := 1 to iNPar do
      mBeta[1,j] := mBeta[1,j] + v_delta[j];

    eps:=-1;
    for j:=1 to iNPar do
      if eps<Abs(v_Delta[j]) then eps:=Abs(v_Delta[j]);

    {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
    atribui vResid � �ltima linha de mX.}
    EvalModel;

    {-------------------------- Sub-Itera��o -----------------------------}
    {Adiciona uma linha em mIter. Referente � itera��o.}
    v_iter := TwsDFVec.Create(iNPar+2);
    v_Iter.Name:='Passo_'+IntToStr(Iter)+'_0';
    v_iter[1] := EPS;
    v_iter[2] := fSSRes;
    for j := 1 to iNPar do
      v_iter[j+2] := mBeta[1,j];
    mIter.MAdd(v_iter);

    for subit:=1 to 10 do
      begin
      if (fSSRes <= old_fSSRes) then Break;
      for j := 1 to iNPar do
        begin
        v_delta[j] := v_delta[j] * 0.5;
        mBeta[1,j] := m_old_beta[1,j] + v_delta[j];
        end;

      {Atualiza parametros na TabVar, calcula (modelo, vResid e fSSRes) e
      atribui vResid � �ltima linha de mX.}
      EvalModel;

      {Adiciona uma linha em mIter. Referente � itera��o.}
      v_iter := TwsDFVec.Create(iNPar+2);
      v_Iter.Name:='Passo_'+IntToStr(Iter)+'_'+IntToStr(Subit);
      v_iter[1] := EPS;
      v_iter[2] := fSSRes;
      for j := 1 to iNPar do
        v_iter[j+2] := mBeta[1,j];
      mIter.MAdd(v_iter);
      end; // for subit

    {BB}
    for i := 1 to iNPar do
      begin
      for j := 2 to iNPar do
        BB[i, j-1] := BB[i,j];
      BB[i,iNPar] := m_old_beta[1,i];
      end;

    {FF}
    for i := 1 to iNObs do
      begin
      for j := 2 to iNPar do
        FF[i, j-1] := FF[i,j];
      FF[i,iNPar] := old_vDep[i];
      end;

    {v_sss}
    for i := 2 to iNPar do
        v_sss[i-1] := v_sss[i];
    v_sss[iNPar] := old_fSSRes;

    {Libera��es.}
    m_old_beta.Free;
    m_old_beta := Nil;
    old_vDep.Free;
    old_vDep := Nil;
  end; // for iter

  BB.Free;
  FF.Free;
  v_delta.Free;
  v_sss.Free;
  v_tol.Free;
  v.Free;

  v_ind.Free;

End;{TwsDUD.DUDFit}

(****************************************************************************
ok1
*)
Procedure TwsDUD.LoopDSFrame;
var i, j, aux : integer;
begin

  {---------------------- La�o para cada lstDataSets --------------------------}
  for i := 0 to lstDataSets.Count - 1 do
      begin

      DSFrame := lstDataSets[i];
      iNObs := DSFrame.NRows;

      {Inicializa mBeta com os valores iniciais.}
      for j := 1 to iNPar do
        mBeta[1,j] := vValoresIniciais[j];

      mX := TwsGeneral.Create(iNPar+1, iNObs);

      {Libera vPred anterior e cria um novo.}
      Eval.TabVar.DeleteVar(sPredName);
      fSSTot := 0;
      vPred := TwsDFVec.Create(iNObs);
      for j := 1 to iNObs do
        begin
        vPred[j] := DSFrame[j, 2];
        fSSTot := fSSTot + sqr(DSFrame[j, 1]);
        end;

      {Inserir vPred na tabela de s�mbolos com nome sPredName.}
      Eval.TabVar.AddVector(sPredName, vPred);

      {Cria mIter com zero linhas.}
      mIter := TwsGeneral.Create(0,iNPar + 2);
      with mIter do
        begin
        Name := 'DUD';
        MLab := 'Resultados do Processo Iterativo';
        ColName[1] := 'EPS';
        ColName[2] := 'SQRes';
        for j := 1 to iNPar do
          ColName[j+2] := mBeta.ColName[j];
        end;

      DUDFit;

      GeraSaidas(i);

      mX.Free;
      mX := Nil;
      end; {for}
end; {TwsDUD.LoopDSFrame}


End.



(*
Observa��es:

FDataSet ---> Parte em mem�ria e parte em disco.
Trabalhar com FDataSet quando o DataSet tiver que sofrer alteracoes.
FDataSet.AsDataSet.MAdd;
NovoDS := DS.AsDataSet;
.
.
.
DS := DS.UpDate;


  Obs.:
  Nome de coluna de matriz deve ser um ID v�lido.
  Nome de coluna (vari�vel) de DataSet deve ser um ID v�lido.
  Nome de n�vel de fator n�o precisa ser um ID v�lido.


*)

