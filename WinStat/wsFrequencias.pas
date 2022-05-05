unit wsFrequencias;

(*
  ============================================================================
  ================================== FUN��O ==================================
  ============================================================================

  - Esta Unidade implementa a Classe TwsFreq que encapsula m�todos para se
    realizar uma an�lise de freq��ncias sobre um conjunto de dados (DataSet).
  - Possui tamb�m algumas procedures e functions relacionadas com an�lise
    de freq��ncias.

  ============================================================================
     TERMOS(express�es) UTILIZADOS E FORMATO DAS TABELAS E MATRIZES GERADAS
  ============================================================================

  Nos exemplos abaixo, considerar as seguintes vari�veis com os seguintes n�veis:
  Var1 : Nivel_1, Nivel_2 e Nivel_3
  Var2:  Nivel_A, Nivel_B e Nivel_C


  ============================================================================
  ================ TABELA DE FREQUENCIAS SIMPLES (DataSet) ===================
  ============================================================================
  Gerada por SimpleTable.
  Ex.: Frequencia da vari�vel Var1.
  DataSet.Name = 'S_Var1'
  DataSet.MLab = 'Tabela de Frequencias para Var1'


  ------------------ Para Vari�vel num�rica (Numeric) -----------------------

  Obs.: A coluna (variavel) Var1 era do tipo Numeric e foi transformada
        para tipo Factor (Quantitative).

  Observ.        Var1    L_Inf    L_Sup    Freq    Fr_Rel    Fq_Acum    FR_Acum
        1     Nivel_1
        2     Nivel_2
        3     Nivel_3                                                         1


  ------------------- Para Vari�vel Fator (Factor) ----------------------------

  Observ.        Var1     Freq     Fr_Rel     Fq_Acum     FR_Acum
        1     Nivel_1
        2     Nivel_2
        3     Nivel_3                                           1


  =============================================================================
  ================ TABELA DE FREQUENCIAS CRUZADA (DataSet) ====================
  =============================================================================
  Gerado por CrossTable.
  Ex.: Frequencia para cruzamento de Var1 x Var2.
  DataSet.Name = 'C_Var1_Var2'
  DataSet.MLab = 'Tabela de Frequencias para Var1 x Var2'

  Observ.        Var1       Var2    Freq    F_R_Lin    F_R_Col    F_R_Tot
        1     Nivel_1    Nivel_A
        2     Nivel_1    Nivel_B
        3     Nivel_1    Nivel_C
        4     Nivel_2    Nivel_A
        5     Nivel_2    Nivel_B
        6     Nivel_2    Nivel_C
        7     Nivel_3    Nivel_A
        8     Nivel_3    Nivel_B
        9     Nivel_3    Nivel_C

  =============================================================================
  ================ MATRIZ DE FREQUENCIAS SIMPLES (TwsGeneral) =================
  =============================================================================
  Gerada a partir de uma Tabela de Freq��ncias Cruzada (DataSet).
  Gerada pela fun��o DSFreqToMatFreqSimples.
  Matriz.Name = 'Freq_Assoc'
  Matriz.Lab  = 'Matriz de Frequencias para Var1 X Var2'

  MATRIZ DE FREQUENCIAS SIMPLES

            Nivel_A  Nivel_B  Nivel_C    ...   Total
  Nivel_1
  Nivel_2
  Nivel_3
     .
     .
     .
  Total

  =============================================================================
  =================MATRIZ DE FREQUENCIAS COMPLETA (TwsGeneral) ================
  =============================================================================
  Gerada a partir de uma Tabela de Freq��ncias Cruzada (DataSet).
  Gerada pela fun��o DSFreqToMatFreqCompleta.
  Matriz.Name = 'Freq_Assoc'
  Matriz.MLab = 'Matriz de Frequencias para Var1 X Var2'


            Nivel_A  Nivel_B  Nivel_C    Total
  Nivel_1
    Lin %                                    .
    Col %'                                   .
    Tot %'
  Nivel_2
    Lin %                                    .
    Col %'                                   .
    Tot %'
  Nivel_3
    Lin %                                    .
    Col %'                                   .
    Tot %'
  Total
  Total %                                  100

*)

interface
Uses Classes,
     Math,
     SysUtils,
     SysUtilsEx,
     wsBXML_Output,
     wsVec,
     wsFuncoesDeProbabilidade,
     wsFuncoesDeEscalares,
     wsGLib,
     wsMatrix,
     wsConstTypes;

Const

  {Constantes utilizadas na Janela Frequencias, servem como indice para
   a variavel bitsOpcoes(TBits)}
  cIDSFrame  = 1;
  cADSFrame  = 2;
  cITabFreq  = 3;
  cATabFreq  = 4;
  cIMatFreq  = 5;
  cAMatFreq  = 6;
  cIQuiQuad  = 7;
  cAQuiQuad  = 8;
  cIEsperado = 9;
  cAEsperado = 10;
  cIResiduos = 11;
  cAResiduos = 12;
  cIResPadr  = 13;
  cAResPadr  = 14;
  cIEstAss   = 15;
  cAEstAss   = 16;
  cGeraHistograma = 17;
  cIFreqByValue = 18;
  cAFreqByValue = 19;

  csVALOR_PERDIDO = 'Valor_Perdido'; {Nome para o n�vel Valor_Perdido quando
                     usando ChangeToFactor e ChangeToFactorEx.}
  csOUTROS        = 'Outros'; {Nome default para outros valores quando este
                     n�o for passado para ChangeToFactorEx.}

Type

  TTabelType = (ttSimples, ttCruzada); {Tabela simples ou cruzada)

  {Usada em ChangeToFactorEx}
  TCodVar = record
              Tipo : byte;{Tipo de intervalo.
                           0 - Igualdade.
                           1 - Intervalo.
                           2 - Menor.
                           3 - Maior.
                           4 - Outros valores.}
              s_novo_nome : string; {Nome para o n�vel.}
              Case byte Of
                1: ( f_valor : double);
                   {Utilizado quando tipo = 0, 2 ou 3. }
                2: ( f_inicio, f_fim : double);
                   {Utilizado somente quando tipo = 1.}
            end;

  {-------------------------------- TwsFreq ----------------------------------}
{ Heran�a
    TwsFreq --> TObject
  Objetivo
    Realiza a an�lise de frequ�ncia de vari�veis do tipo fator. Constr�i tabela de frqu�ncias
    simples e cruzadas, incluindo frequ�ncias absolutas, relativas e acumuladas, realiza v�rios
    testes em classifica��es duplas e constr�i histogramnas para represena��o gr�fica das
    distribui��es
}
  TwsFreq = Class
  Private
    // array de op��es para an�lise de frequ�ncia. Tipicamente essas op��es s�o atribu�das
    // atrav�s da interface
    bitsOpcoes :TBits;

    // Mecanismo para gera��o de relat�rios
    FOutput: TwsBXML_Output;

    // Evento notificador de cria��o de objetos
    FOnCreatedObject: TwsCreatedObject_Event;

    // Conjunto de dados original para constru��o das tabelas
    DataSet,
    // Conjunto de dados de onde, de fato, ser�o retiradas as informa��es para an�lise.
    // � uma refer�ncia a um dos conjuntos de dados gerados por ModelFrame
    DSFrame    :TwsDataSet;
    // Vari�veis de grupo
    stsVarsGrupo: TStrings;
    // Condi��o para filtro das observa��es
    sCondicao,
    // Vari�vel (express�o) utilizada como peso
    sPeso,
    // define as tabelas que ser�o criadas
    sTabelas  : String;

    // Faz uma an�lise das especifica��es para constru��o das tabelas
    Function iParserCruz(strToken : String; var expr1, expr2 : String): Integer;
    // Coordena o processo de sa�das
    Procedure GeraSaidas(tipo:TTabelType; ds_freq : TwsDataSet; v_TotLin, v_TotCol: TwsVec;
      f_freq_tot: double);
  Public
    // Cria objeto para constru��o das tabelas
    Constructor Create(aDataSet: TwsDataSet;
                       astsVarsGrupo: TStrings;
                       asCondicao: String;
                       asPeso: String;
                       asTabelas: String;
                       abitsOpcoes: TBits;
                       aOutput: TwsBXML_Output;
                       aCreatedObject_Event: TwsCreatedObject_Event);

    // Libera espa�o utilizado pelo objeto
    Destructor Destroy; override;
  End;{TwsFreq}


{============================ ROTINAS AUXILIARES =============================}

Function i_retira_espacos_extras(var s : string): integer;

{$ifdef CHUVAZ}
// Transforma uma vari�vel num�rica numa do tipo fator
function ChangeToFactor(DataSet: TWSDataSet;
                        ind_var: longint;
                        var LimSupSerie: double;
                        const Intervalos: Word = 0;
                        DivLog: Boolean = false): Word;
{$else}
// Transforma uma vari�vel num�rica numa do tipo fator
Procedure ChangeToFactor(DataSet: TwsDataSet; ind_var: Integer; var f_amplitude: double);
{$endif}

// Constroi coluna do tipo fator com base em vari�vel num�rica
Procedure ChangeToFactorEx(DataSet: TwsDataSet; ind_var: Integer; tipo_ordenado: boolean;
  s_nome, s_Lab:string; lst_niveis: TList);

{$ifdef CHUVAZ}
Function SimpleTable(ind_var:longint; ind_peso: longint; DataSet: TWSDataSet;
                    lGeraIntervalos: boolean; LimSupSerie: double): TwsDataSet;
{$else}
// Constr�i tabela de frequ�ncias para uma vari�vel
Function SimpleTable(ind_var:Integer; ind_peso: Integer; DataSet: TwsDataSet;
                     lGeraIntervalos:boolean; f_amplitude: double): TwsDataSet;
{$endif}

// Constr�i tabela de frequ�ncias para uma vari�vel
Function SimpleValueTable(ind_var:Integer; DataSet: TwsDataSet): TwsDataSet;

// constroi uma distribuicao de frequencias a partir de um vetor
function VecToFreq(v: TwsVec): TwsDataSet;

// Constr�i tabela de frequ�ncias para duas vari�veis
Function CrossTable(ind_var1, ind_var2, ind_peso:Integer; DataSet: TwsDataSet; var v_TotLin,
  v_TotCol: TwsVec; var f_freq_tot: double):TwsDataSet;

// Transforma uma tabela de frequ�ncias num conjunto de dados para matriz
Procedure DSFreqToMatFreqSimples(DataSet: TwsDataSet; v_TotLin, v_TotCol : TwsVec;
  f_freq_tot : double; Var F : TwsGeneral);

// Transforma uma tabela de frequ�ncias num conjunto de dados para matriz
Procedure DSFreqToMatFreqCompleta(DataSet: TwsDataSet; v_TotLin, v_TotCol : TwsVec;
                                 f_freq_tot : double; Var F : TwsGeneral);

// Obt�m estat�sticas baseadas no qui-quadrado
Function Qui_Quad(DataSet: TwsDataSet; var E,R,SR: TwsGeneral; var GL: Integer;
  v_TotLin, v_TotCol : TwsVec; f_freq_tot: double; var Erro: Word): TwsGeneral;

// Obt�m estat�sticas de associa��o
procedure FreqAssociation(DtSet: TwsDataSet; v_TotLin, v_TotCol: TwsVec; wTot: double;
  F : TwsGeneral; var Stat : TwsGeneral);

implementation
Uses wsFuncoesDeDataSets,
     wsGraficos,
     Form_Chart;

{==============================================================================}
{===================== IMPLEMENTA��O DAS ROTINAS DA CLASSE ====================}
{==============================================================================}

{******************************************************************************}
(*
Fun��o:
Gerar as tabelas de frequencias.
Exs.:
A     - Gera uma tabela simples apartir da variavel A.
A*B   - Gera uma tabela simples do produto das variaveis A e B.
Fun(A)- Gera uma tabela de frequencias de Fun(A),
        onde 'Fun' � uma fun��o (ex.: Sen(A), Cos(A), Raiz(A), etc.)
Cruz(A;B) - Tabela cruzada de A e B.

Obs.:
Cada DSFrame conter� somente as linhas que passaram no teste de condi��o.
Todos os DSFrame possuem a mesma estrutura de colunas, e cont�m todas as colunas
necess�rias para gerar todas as tabelas requisitadas na interface. Por�m,
durante a gera��o de uma tabela, apenas uma ou duas colunas s�o utilizadas,
dependendo se for an�lise simples ou cruzada respectivamente.
*)

{ Objetivo
    Cria objeto para obten��o de tabelas de frequ�ncias simples e cruzadas, testes de associa��o
    e outras estat�sticas.
  Par�metros
    ADataSet: Conjunto de dados onde est�o as vari�veis
    astsVarsGrupo: lista com as vari�veis de grupo
    asCondicao: condi��o para sele��o de observa��es ('' indica aus�ncia de condi��o)
    asPeso: vari�vel peso ('' indica aus�ncia de vari�vel peso)
    asTabelas: indica��o de quais tabelas ser�o constru�das. A indica��o de uma tabela simples
      deve ser por meio do nome da vari�vel (Var1 Var2) enquanto que tabelas cruzadas devem ser
      indicadas como Cruz(Var1; Var2)
    aBitsOpcoes: op��es de sa�da e/ou armazenamento. As posi��es consideradas s�o:
                 cIDSFrame  = 1 - Imprime conjunto de dados para an�lise
                 cADSFrame  = 2 - Salva conjunto de dados para an�lise
                 cITabFreq  = 3 - Imprime tabela (conjunto de dados) com frequ�ncias
                 cATabFreq  = 4 - Salva tabela (conjunto de dados) com frequ�ncias
                 cIMatFreq  = 5 - Imprime matriz de frequ�ncias
                 cAMatFreq  = 6 - Salva matriz de frequ�ncias
                 cIQuiQuad  = 7 - Imprime valores de qui-quadrado
                 cAQuiQuad  = 8 - Salva valores de qui-quadrado
                 cIEsperado = 9 - Imprime frequ�ncias esperadas
                 cAEsperado = 10 - Salva frequ�ncias esperadas
                 cIResiduos = 11 - Imprime res�duos
                 cAResiduos = 12 - Salva res�duos
                 cIResPadr  = 13 - Imprime res�duos padronizados
                 cAResPadr  = 14 - Salva res�duos padronizados
                 cIEstAss   = 15 - Imprime estat�sticas de associa��o
                 cAEstAss   = 16 - Salva estat�sticas de associa��o
                 cGeraHistograma = 17 - Constr�i histograma
}
Constructor TwsFreq.Create(aDataSet: TwsDataSet;
                           astsVarsGrupo: TStrings;
                           asCondicao: String;
                           asPeso: String;
                           asTabelas: String;
                           abitsOpcoes: TBits;
                           aOutput: TwsBXML_Output;
                           aCreatedObject_Event: TwsCreatedObject_Event);
Var
  ds_Freq        : TwsDataSet; //DataSet com o resultado da an�lise de Frequencias
  sts_TokensTabs,              // Lista com os Tokens contidos na caixa de entrada edTabelas
  sts_Tokens,                  //Lista com os Tokens contidos nas caixas de entrada edTabelas e edPeso
  sts_Vars,                    //Lista com as vari�veis que far�o parte dos conjuntos DSFrame
  sts_IsNumeric,               {Informa��o sobre cada Variavel dos DataSets que serao analizados.
                                'SIM' - Variavel � do tipo Numerica,
                                'NAO' - Variavel � do tipo Fator.}
  sts_aux        : TStrings;
  lstDataSets    : TwsDataSets;{Lista com os DataSets que ser�o usados para gerar as tabelas de
                                frequencias. DataSets gerados por ModelFrame.}
  tipo           : TTabelType; //Tipo da tabela a ser gerada (simples ou cruzada)
  s_var1,                      //Vari�vel da qual ser� feita a analise de Frequencia
  s_var2,                      //Segunda Variavel, caso seja analise cruzada
  s_peso,                      //Nome da Vari�vel Peso, � uma c�pia de edPeso
  s_NovoNome     : string;     //Nome que ser� atribuido ao novo conjunto (resultante da analise) gerado
  v_TotLin,                  {Vetor que conter� a soma das frequencias de cada linha.}
  v_TotCol       : TwsVec;   {Vetor que conter� a soma das frequencias de cada coluna.}
                             {Somente utilizadas quando for Tabela Cruzada.}
  i,n_grupo,ind_aux,           {vari�veis auxiliares}
  ind_var1,ind_var2,           {indices de s_var1, s_var2 e s_peso}
  ind_peso,
  ind_col_freq   : Integer;  {indice da coluna onde ficou o resultado com as frequencias,
                              usado somente para saber qual coluna passar para a funcao que
                              desenha o histograma.}
  lGeraIntervalos: Boolean;  {Quando true, significa que a vari�vel da qual est� sendo
                              feita a an�lise era do tipo num�rica, e foi transformada
                              em Fator, entao dever� gerar as colunas Limite_Inferior
                              e Limite_Superior ao fazer a analise de frequencias.
                              Esta vari�vel s� � utilizada quando for gerar uma tabela
                              simples, para cruzamentos, n�o s�o gerada as colunas
                              Limite_Inferior e Limite_Superior.}
  aux, j         : integer;
  f_ampl,                    {Conter� a amplitude de cada intervalo quando
                              transformar uma variavel Numeric em Factor.}
  f_freq_tot     : double;   {Frequencia total retornada por CrossTable.}
  HistGraf       : TfoChart;
begin
  DataSet          := aDataSet;
  stsVarsGrupo     := astsVarsGrupo;
  sCondicao        := asCondicao;
  sPeso            := asPeso;
  sTabelas         := asTabelas;
  bitsOpcoes       := abitsOpcoes;
  FOutput          := aOutput;
  FOnCreatedObject := aCreatedObject_Event;

  sts_Tokens     := TStringList.Create;
  sts_TokensTabs := TStringList.Create;
  sts_Vars       := TStringList.Create;
  sts_isNumeric  := TStringList.Create;
  v_TotLin       := TwsDFVec.Create(1);
  v_TotCol       := TwsDFVec.Create(1);


  Try
    If sTabelas = '' Then
       Raise Exception.Create('Nenhuma tabela foi definida. Utilize bot�o Definir Tabelas')
    else
       Begin
       sPeso := sysUtilsEx.AllTrim(sPeso);
       s_Var1:=DataSet.Name;
       if DataSet.MLab<>'' then
         s_Var1:=s_Var1+ ' - '+DataSet.MLab;
       FOutput.BeginText;
         FOutput.WritePropValue('Conjunto de Dados:', s_Var1);

         if sCondicao <> '' then
           FOutput.WritePropValue('Filtro:', sCondicao);

         s_Var1:='';
         if stsVarsGrupo.Count>0 then
           begin
           for i:=0 to stsVarsGrupo.Count-1 do
             s_Var1:=s_Var1+stsVarsGrupo[i]+', ';
           System.Delete(s_Var1,Length(s_Var1)-1,2);
           FOutput.WritePropValue('Vari�veis de grupo:', s_Var1);
           end;

         FOutput.WritePropValue('Tabelas definidas:', sTabelas);

         if sPeso <> '' then
           FOutput.WritePropValue('Vari�vel Peso:', sPeso);
         FOutPut.WriteText('');
         FOutPut.WriteText('');
       FOutput.EndText;

       {Pega tokens de sTabelas e sPeso.}

       Split(sysUtilsEx.AllTrim(sTabelas), sts_TokensTabs, [' ']);
       Split(sysUtilsEx.AllTrim(sTabelas + ' ' + sPeso), sts_Tokens, [' ']);

       {Verifica quais as vari�veis que far�o parte dos DSFrame.}
       For i:= 0 To sts_Tokens.Count-1 Do
         {Se o token for: uma variavel, express�o ou fun��o entao...}
         If iParserCruz(sts_Tokens[i], s_var1, s_var2) = 0 Then
            {Adiciona a vari�vel.}
            sts_Vars.Add(sts_Tokens[i])
         Else {Token pode representar uma tabela cruzada. Cruz(expr1;expr2).}
            {adiciona 'expr1' e 'expr2' como vari�veis, por enquanto n�o
             verifica erros.}
            Begin
            sts_Vars.Add(s_var1);
            sts_Vars.Add(s_var2);
            End;

       {Elimina as vari�veis repetidas.}
       sts_aux := sysUtilsEx.RemoveDuplicates(sts_Vars);
       sts_Vars.Free;
       sts_Vars := sts_aux;
       sts_aux := Nil;

       lstDataSets := ModelFrame(sts_Vars, stsVarsGrupo, sCondicao, DataSet);

       {Pega o indice onde ficou a variavel peso.}
       If (sPeso <> '') Then
          ind_peso := GetIndexVar(lstDataSets[0], NomeFormula(sPeso))
       else
          ind_peso := -1;

       {Verifica quais colunas s�o do tipo num�rica e guarda
        esta informa��o. Analiza somente o primeiro DataSet de lstDataSets
        pois todos possuem a mesma estrutura de colunas.}
        For i := 0 To lstDataSets[0].NCols-1 Do
            If lstDataSets[0].Struct.Col[i+1].ColType = dtNumeric Then
               sts_IsNumeric.Add('SIM') {Na verdade est� fazendo sts_IsNumeric[i]:='SIM'}
            Else
               sts_IsNumeric.Add('NAO');{Na verdade est� fazendo sts_IsNumeric[i]:='NAO'}

       {********************* Gera as analises de Frequencia ****************}

       {La�o para cada DataSet gerado por ModelFrame.}
       For n_grupo := 0 To lstDataSets.Count - 1 Do
         Begin
         DSFrame := lstDataSets[n_grupo];

{--------------------- Imprime/Armazena o DSFrame ---------------------}

         if BitsOpcoes[cIDSFrame] then
            FOutput.Add(DSFrame);

         if BitsOpcoes[cADSFrame] and Assigned(FOnCreatedObject) then
            FOnCreatedObject(self, DSFrame); {Obs.: Nao liberar DSFrame.}

         {La�o para cada token contido na entrada.
         Cada la�o ir� gerar uma Tabela de Frequencias.}
         For i := 0 To sts_TokensTabs.Count-1 Do
           Begin
           {Verifica se � para gerar uma tabela simples ou cruzada.}
           {Nao est� fazendo nenhum tratamento quanto a nomes de fun��es v�lidas.}
           If iParserCruz(sts_TokensTabs[i], s_var1, s_var2) = 0 Then
              Begin
              tipo := ttSimples;
              s_var1 := sts_TokensTabs[i];
              End
           Else
              {N�o est� fazendo nenhum tratamento quanto a nomes de fun��es
               ou nomes de vari�veis v�lidas.}
              tipo := ttCruzada;

           If tipo = ttSimples then
              Begin
              ind_var1 := GetIndexVar(DSFrame, NomeFormula(s_var1));
              if BitsOpcoes[cIFreqByValue] or BitsOpcoes[cAFreqByValue] then
                begin
                ds_Freq := SimpleValueTable(ind_var1, DSFrame);
                ind_col_freq := 3;
                end
              else
                begin
                {Se a coluna da qual ser� feita a an�lise for do tipo dtNumeric...}
                If Not DSFrame.IsFactor(DSFrame.Struct.Col[ind_var1].Name) Then
                   ChangeToFactor(DSFrame, ind_var1, f_ampl);
                {Se a coluna era do tipo dtNumeric...}
                If sts_IsNumeric[ind_var1-1]='SIM' Then
                   begin
                   lGeraIntervalos := True;
                   ind_col_freq := 5;
                   end
                Else
                   begin
                   lGeraIntervalos := False;
                   ind_col_freq := 3;
                   end;
                ds_Freq := SimpleTable(ind_var1,ind_peso,DSFrame,lGeraIntervalos,f_ampl)
                end;
              if DSFrame.MLab <> '' then
                ds_Freq.MLab := ds_Freq.MLab+' - '+DSFrame.MLab;
              sts_aux := TStringList.Create;
              sts_aux.Add(TwsFactor(ds_Freq.Struct.Col[ind_col_freq]).Name);
              End {if tipo = simples}
           Else {tipo = cruzada}
              Begin
              ind_var1 := GetIndexVar(DSFrame, NomeFormula(s_var1));
              ind_var2 := GetIndexVar(DSFrame, NomeFormula(s_var2));
              ind_col_freq := 6;
              {Se a coluna1, que ser� utilizada para fazer a an�lise for dtNumeric...}
              If Not DSFrame.IsFactor(DSFrame.Struct.Col[ind_var1].Name) Then
                 ChangeToFactor(DSFrame, ind_var1, f_ampl);

              {Se a coluna2, que ser� utilizada para fazer a an�lise for dtNumeric...}
              If Not DSFrame.IsFactor(DSFrame.Struct.Col[ind_var2].Name) Then
                 ChangeToFactor(DSFrame, ind_var2, f_ampl);

              ds_Freq := CrossTable(ind_var1,ind_var2,ind_peso,DSFrame,v_TotLin,
                v_TotCol,f_freq_tot);

              if DSFrame.MLab <> '' then
                ds_Freq.MLab := ds_Freq.MLab + ' - '+DSFrame.MLab;
              sts_aux := TStringList.Create;
              sts_aux.Add(TwsFactor(ds_Freq.Struct.Col[ind_col_freq]).Name);
              End;{tipo = cruzada}

           {----- Gera Histograma se foi escolhida esta op��o -----}
           if bitsOpcoes[cGeraHistograma] then
              begin
              HistGraf := ws_HistoGramPlot(ds_Freq,ds_Freq.Struct.Col[1].Name,sts_aux);
              FOutput.Add(HistGraf);
              if Assigned(FOnCreatedObject) then
                 FOnCreatedObject(self, HistGraf)
              else
                 HistGraf.Release;
              end;

           GeraSaidas(tipo,ds_Freq,v_TotLin,v_TotCol,f_freq_tot);

           sts_aux.free;
           sts_aux := nil;
           End; {for i}
         End; {for n_grupo}

       lstDataSets.FreeDataSets := not bitsOpcoes[cADSFrame];
       lstDataSets.free;
       End; {if edTabelas <> ''}

  Finally
    sts_Tokens.Free;
    sts_TokensTabs.Free;
    sts_Vars.Free;
    sts_IsNumeric.Free;
    v_TotLin.Free;
    v_TotCol.Free;
  End; {Try}
end; {TwsFreq.Create}

Destructor TwsFreq.Destroy;
begin
end;

Function TwsFreq.iParserCruz(strToken : String; var expr1, expr2 : String): Integer;
{  Objeto
     Analisa um Token e verifica se ele � do tipo 'Cruz(expr1;expr2)'.
   Par�metros
     strToken - token a ser analisado
     expr1: retorna a primeira vari�vel (express�o)
     expr2 retorna a segunda vari�vel (express�o)
   Retorno
     0 - Nao encontrou a palavra Cruz.
     1 - Ok, encontrou o token 'Cruz' e o ';'. Entao retorna as express�es em expr1 e expr2.
     2 - Erro: Nao achou o ';'.
     3 - Erro: Token 'Cruz' nao estava no inicio.
}
var strToken1 : string;
Begin
   strToken1 := UpperCase(strToken);
  {Ve se acha 'CRUZ'}
   Case System.Pos('CRUZ', strToken1) Of
     0 :{Nao achou a palavra CRUZ}
       Result:=0;
     1 :{Achou a palavra CRUZ no inicio.}
       Begin
       Delete(strToken1, 1, 4);
       {Separa o ';' }
       If sysUtilsEx.SubStrings(';', expr1, expr2, strToken1) <> 0 Then
          Begin
          {Ok}
          Delete(expr1, 1, 1);
          Delete(expr2, Length(expr2), 1);
          Result := 1
          End
       Else
          {Erro: Nao achou o ';'.}
          Result := 2
       End
     Else {Case}
       {Erro: Palavra Cruz nao estava no inicio. Verificar ?????????}
       Result := 3
   End; {Case}
End; {iParserCruz}

Procedure TwsFreq.GeraSaidas(tipo: TTabelType;
                             ds_freq: TwsDataSet;
                             v_TotLin: TwsVec;
                             v_TotCol: TwsVec;
                             f_freq_tot: double);
{  Objetivo
     Rotina chamada logo ap�s ser gerada uma tabela de frequencias. Ela ir� imprimir ou armazenar os
     resultados conforme as op��es.
   Par�metros
     tipo       - Tipo de tabela contida em ds_freq (ttSimples ou ttCruzada).
     ds_freq    - DataSet com o resultado de uma analise de frequencia (tabela simples ou cruzada).
     v_TotLin   - Vetor contendo a soma das frequencias de cada linha (somente v�lido se tipo=ttCruzada).
     v_TotCol   - Vetor contendo a soma das frequencias de cada coluna (somente v�lido se tipo=ttCruzada).
     f_freq_tot - Soma total das frequencias (somente v�lido se tipo=ttCruzada).
}
var
  m_Qui_Quad,Est_Ass,
  E,R,SR,A,D,F        : TwsGeneral;
  Qp,Stat,AsE         : TwsVec;
  GL                  : Integer;
  Erro_qui_quad       : Word;
Begin
  {Imprime o DataSet com o resultado da an�lise de frequencias}
  if bitsOpcoes[cITabFreq] or bitsOpcoes[cIFreqByValue] then
     FOutput.Add(ds_freq);

  {Se for uma Tabela simples ent�o n�o tem que imprimir mais nada}
  If tipo = ttSimples Then
     begin
     {----- Armazena e/ou Libera a Tabela de Frequencias(DataSet) -----}
     If (bitsOpcoes[cATabFreq] or bitsOpcoes[cAFreqByValue]) and Assigned(FOnCreatedObject) Then
        FOnCreatedObject(self, ds_freq)
     Else
        ds_freq.Free;

     Exit;
     end;

  {=============================== CALCULOS =================================}

  {Se chegou aqui � porque est� trabalhando com uma tabela cruzada}

  {----- Gera Qui-Quadrado -----}
   If bitsOpcoes[cIQuiQuad] Or bitsOpcoes[cAQuiQuad] Then
      m_Qui_Quad := Qui_Quad(ds_freq, E, R, SR, GL, v_TotLin, v_TotCol, f_freq_tot, Erro_qui_quad);

   if Erro_qui_quad > 0 then
      begin
      FOutput.BeginText;
      Case Erro_qui_quad of
         {se houver totais de linha ou coluna nula}
         1: FOutput.Warning('Ocorr�ncia de algum total de linha <= 0');
         2: FOutput.Warning('Ocorr�ncia de algum total de coluna <= 0');
         {Se tiver algum valor observado < 5 em ds_freq}
         3: FOutput.Warning('Ocorr�ncia de freq��ncia(s) observada(s) < 5');
         {Se tiver algum valor esperdo < 1 em E}
         4: FOutput.Warning('Obs.: Ocorrencia de freq��ncia esperada < 1');
         5:
           begin
           FOutput.Warning('Ocorr�ncia de freq��ncia(s) observada(s) < 5');
           FOutput.Warning('Ocorr�ncia de freq��ncia(s) esperada(s) < 1');
           end;
         End; {case}
      FOutput.EndText;
      end;

   {----- Gera matriz de frequencias simples -----}
   DSFreqToMatFreqSimples(ds_freq, v_TotLin, v_TotCol, f_freq_tot, F);

   {----- Gera Estatisticas de Associa��o -----}
   if bitsOpcoes[cIEstAss] Or bitsOpcoes[cAEstAss] then
      FreqAssociation(ds_freq, v_TotLin, v_TotCol, f_freq_tot, F, Est_Ass);

   {----- Gera Matriz de Frequencias completa -----}
   {Tenho que chamar ap�s ter chamado FreqAssociation.}
   if bitsOpcoes[cIMatFreq] Or bitsOpcoes[cAMatFreq] then
       DSFreqToMatFreqCompleta(ds_freq, v_TotLin, v_TotCol, f_freq_tot, F);

   {=========== IMPRESS�ES, ARMAZENAMENTOS E LIBERA��ES DE OBJETOS ==========}
   {Neste ponto eu nao preciso mais de ds_freq}
   If bitsOpcoes[cATabFreq] and Assigned(FOnCreatedObject) Then
      FOnCreatedObject(self, ds_freq)
   Else
      ds_freq.Free;

   {----- Matriz de Frequencias Completa -----}
   if bitsOpcoes[cIMatFreq] Or bitsOpcoes[cAMatFreq] then
      begin

      if bitsOpcoes[cIMatFreq] then
         FOutPut.Add(F);

      if bitsOpcoes[cAMatFreq] and Assigned(FOnCreatedObject) then
         FOnCreatedObject(self, F)
      else
         F.Free;
      end;

   {----- Estatisticas de Associa��o -----}
   if bitsOpcoes[cIEstAss] Or bitsOpcoes[cAEstAss] then
      begin
      if bitsOpcoes[cIEstAss] Then
         FOutPut.Add(Est_Ass);

      if bitsOpcoes[cAEstAss] and Assigned(FOnCreatedObject) then
         FOnCreatedObject(self, Est_Ass)
      else
         Est_Ass.Free;
      end;

   {----- Qui-Quadrado, Valor Esperado, Residuos e Res.Padr. -----}
   If (bitsOpcoes[cIQuiQuad] Or bitsOpcoes[cAQuiQuad]) and
      ( (Erro_qui_quad = 0) or (Erro_qui_quad >= 3) ) Then
      Begin
      if bitsOpcoes[cIEsperado] then
         FOutPut.Add(E);

      if bitsOpcoes[cAEsperado] and Assigned(FOnCreatedObject) then
         FOnCreatedObject(self, E)
      else
         E.Free;

      if bitsOpcoes[cIResiduos] then
         FOutPut.Add(R);

      if bitsOpcoes[cAResiduos] and Assigned(FOnCreatedObject) then
         FOnCreatedObject(self, R)
      else
         R.Free;

      if bitsOpcoes[cIResPadr] then
         FOutPut.Add(SR);

      if bitsOpcoes[cAResPadr] and Assigned(FOnCreatedObject) then
         FOnCreatedObject(self, SR)
      else
         SR.Free;

      if bitsOpcoes[cIQuiQuad] then
         FOutPut.Add(m_Qui_Quad);

      if bitsOpcoes[cAQuiQuad] and Assigned(FOnCreatedObject) then
         FOnCreatedObject(self, m_Qui_Quad)
      else
         m_Qui_Quad.Free;
      End; {Valor Esperado, Residuos, Res.Padr. e Qui-Quadrado}
end; {GeraSaidas}

{==============================================================================}
{===================== IMPLEMENTA��O DAS ROTINAS AUXILIARES ===================}
{==============================================================================}

{******************************************************************************}
(*
Alexdg 08/07/1998
Fun�ao:
Retira todos expa�os consecutivos da string s, ou seja, ap�s chamada a fun��o,
a string n�o possuir� dois caracteres de espa�o consecutivos.
Ex.:
Se s = 'QT   QL QA   Raiz(X)'
   ap�s chamar a fun��o, s resultar� s = 'QT QL QA Raiz(X)'
rev1.
*)
Function i_retira_espacos_extras(var s : string): integer;
var i : integer;
    l_achou_espaco : boolean;
Begin
  l_achou_espaco := False;
  i := 1;
  While i < length(s) Do
    Begin
    If s[i] = ' ' Then
       If l_achou_espaco Then
          begin
          Delete(s, i, 1);
          dec(i);
          end
       Else
          l_achou_espaco := True
    Else
       l_achou_espaco := False;
    inc(i);
    End;
End; {i_retira_espacos_extras}

{$ifdef CHUVAZ}
function ChangeToFactor(DataSet: TWSDataSet;
                        ind_var: longint;
                        var LimSupSerie: double;
                        const Intervalos: Word = 0;
                        DivLog: Boolean = false): Word;

var Linfs, LSups: TwsDFVec;
    k: Longint; {k � o n�mero de classes}

  function ClassificaValor(const x: double): double;
  var i: Integer;
  begin
    Result := k;
    for i := 1 to k do
      if (x >= LInfs[i]) and (x < LSups[i]) then
         begin
         Result := i;
         Break;
         end;
  end;

var
  i , j: longint;
  Maior, Menor, classe,
  LimInf, LimSup, amp, x    : double;
  ColunaNova, ColunaAntiga  : TWSDataSetCol;
  HasMissValue: boolean;
begin
  ColunaAntiga := DataSet.Struct.Col[ind_var];
  ColunaNova := TwsQuantitative.Create(NONAME, '');

  ColunaNova.Name := ColunaAntiga.Name;
  ColunaNova.Lab := ColunaAntiga.Lab;

  Maior := wsMatrixMax(DataSet, ind_var, 1, DataSet.NRows, HasMissValue);
  Menor := wsMatrixMin(DataSet, ind_var, 1, DataSet.NRows, HasMissValue);

  if Intervalos = 0 then
     k := Trunc(3.322*log10(DataSet.NRows)+1)+1
  else
     k := Intervalos;

  Result := k;

  if DivLog then
     begin
     amp := (Ln(Maior)-Ln(Menor)) / k;
     LInfs := TwsDFVec.Create(k);
     LSups := TwsDFVec.Create(k);
     end
  else
     amp := (Maior-Menor) / k;

  LimSupSerie := Maior;

  {Cria os n�veis (classes).}
  if amp<>0 then
     begin
     if DivLog then LimInf := Ln(menor) else LimInf := menor;
     For i := 1 to k Do
         begin
         LimSup := LimInf + amp;

         TWSFactor(ColunaNova).AddLevel('Classe_' + IntToStr(i-1));

         if DivLog then
            begin
            TwsQuantitative(ColunaNova).LevelValues[i] := Exp(LimInf);
            LInfs[i] := Exp(LimInf);
            LSups[i] := Exp(LimSup);
            end
         else
            TwsQuantitative(ColunaNova).LevelValues[i] := LimInf;

         LimInf := LimSup;
         end;
     end
  else
     {Cria apenas um n�vel.}
     begin
     TWSFactor(ColunaNova).AddLevel('Classe_0');
     TwsQuantitative(ColunaNova).LevelValues[1] := Menor;
     end;

  {------------------- Atualiza a coluna do DataSet -------------------------}

  {Inicializa j que ser� utilizado para verificar se h� valores perdidos na
  coluna sendo codificada.
  Se no final do la�o j=0 ent�o n�o havia nenhum valor perdido na coluna.
  Se j=n (n>0) ent�o haviam n valores perdidos na coluna.}
  j := 0;
  if amp <> 0 then
     For i := 1 to DataSet.NRows do
         If not DataSet.IsMissValue(i, ind_var, x) Then
            begin
            if DivLog then
               classe := ClassificaValor(x) - 1
            else
               classe := (x - menor) / amp;

            {Necess�rio apenas para o caso quando DataSet[i,ind_var]=maior.}
            If Classe >= k Then Classe := Classe - 1;

            DataSet[i,ind_var] := trunc(classe);
            end
         else
            begin
            DataSet[i, ind_var] := k; {o �ltimo n�vel ser� o n�vel csVALOR_PERDIDO}
            inc(j);
            end
  else
     For i := 1 to DataSet.Nrows do
         If not IsMissValue(DataSet[i,ind_var]) Then
            DataSet[i,ind_var] := 0
         else
            begin
            DataSet[i,ind_var] := 1;
            inc(j);
            end;

  {Se houve algum valor perdido no DataSet ent�o adiciona o n�vel csVALOR_PERDIDO.}
  if j > 0 then
     begin
     TWSFactor(ColunaNova).AddLevel(csVALOR_PERDIDO);
     TwsQuantitative(ColunaNova).LevelValues[k+1] := wscMissValue;
     end;

  {Atribui a nova coluna ao DataSet e libera a coluna antiga.
   ind_var-1 porque a propriedade Objects come�a em zero.}
  DataSet.Struct.Cols.Objects[ind_var-1] := ColunaNova;
  ColunaAntiga.Free;

  if DivLog then
     begin
     LInfs.Free;
     LSups.Free;
     end
end; {ChangeToFactor}
{$else}
Procedure ChangeToFactor(DataSet: TwsDataSet; ind_var: Integer; var f_amplitude: double);
{  Objetivo
     Transforma uma vari�vel do tipo num�rica para tipo fator quantitativo. A aplica��o b�sica
     desta fun��o destina-se � constru��o de tabelas de frequ�ncias. O n�mero de classes �
     definido como k=Trunc(1+3,32Log(n))+1, sendo n o n�mero de linhas do conjunto de dados
     e Log o logaritmo decimal. Com base no n�mero de classes e na amplitude total � definida
     a amplitude (ou intervalo) de cada classe. A primeira classe inicia com o menor valor e a
     �ltima classe necessariamente cont�m o maior valor. Como o fator � criado como do tipo
     quantitativo, como valores dos seus n�veis s�o atribu�do os extremos inferiores de cada
     classe.
   Par�metros
     DataSet    : conjunto de dados que cont�m a vari�vel
     ind_var    : �ndice da vari�vel num�rica a ser transformada
     f_amplitude: Retorna a amplitude dos intervalos. Juntamente com os exrtemos inferiores de
       cada classe, possibilita a constru��o das classes.
   Observa��es
   - Cada valor da vari�vel num�rica � substitu�do pelo �ndice da sua classe
   - Se houver algum valor perdido, ent�o ser� criado um n�vel com o nome definido pela constante
      csVALOR_PERDIDO.
   - Se n�o houver nenhum valor perdido na coluna que est� sendo codificada, ent�o n�o � criado o
     n�vel para valores perdidos.
   - Cada n�vel cont�m um valor double que � igual ao limite inferior ao qual o n�vel se refere.
   - O valor para o n�vel csVALOR_PERDIDO � igual a MissValue.
}
var
  i,j,k                  : Integer;
  Maior,Menor,classe,
  LimInf,LimSup,amp      : double;
  ColunaNova,ColunaAntiga: TwsDataSetCol;
  HasMissValue: boolean;
begin

  ColunaAntiga := DataSet.Struct.Col[ind_var];
  ColunaNova := TwsQuantitative.Create(NONAME, '');

  ColunaNova.Name := ColunaAntiga.Name;
  ColunaNova.Lab := ColunaAntiga.Lab;

  Maior := wsMatrixMax(DataSet, ind_var, 1, DataSet.NRows, HasMissValue);
  Menor := wsMatrixMin(DataSet, ind_var, 1, DataSet.NRows, HasMissValue);
  // N�mero de classes
  k := Trunc(1 + 3.32*Log10(DataSet.NRows)) + 1;
  // amplitude comum as classes
  amp := (Maior-Menor) / k;
  f_amplitude := amp;

  {Cria os n�veis (classes).}
  if amp<>0 then
     begin
     LimInf := menor;
     For i := 1 to k Do
         begin
         LimSup := LimInf + amp;
         TwsFactor(ColunaNova).AddLevel('Classe_' + IntToStr(i));
         TwsQuantitative(ColunaNova).LevelValues[i] := LimInf;
         LimInf := LimSup;
         end;
     end
  else
     {Cria apenas um n�vel.}
     begin
     TwsFactor(ColunaNova).AddLevel('Classe_1');
     TwsQuantitative(ColunaNova).LevelValues[1] := Menor;
     end;

  {------------------- Atualiza a coluna do DataSet -------------------------}

  {Inicializa j que ser� utilizado para verificar se h� valores perdidos na
  coluna sendo codificada.
  Se no final do la�o j=0 ent�o n�o havia nenhum valor perdido na coluna.
  Se j=n (n>0) ent�o haviam n valores perdidos na coluna.}
  j := 0;
  if amp<>0 then
     For i:=1 to DataSet.NRows do
     begin
         If not wsGLib.IsMissValue(DataSet[i,ind_var]) Then
            begin
            classe:=(DataSet[i,ind_var]-menor)/amp;
            {Necess�rio apenas para o caso quando DataSet[i,ind_var]=maior.}
            If Classe >= k Then
               Classe := Classe - 1 ;
            DataSet[i,ind_var]:= trunc(classe);
            end
         else
            begin
            DataSet[i,ind_var]:= k; {o �ltimo n�vel ser� o n�vel csVALOR_PERDIDO}
            inc(j);
            end
     end
  else
     For i:=1 to DataSet.Nrows do
         If not wsGLib.IsMissValue(DataSet[i,ind_var]) Then
            DataSet[i,ind_var] := 0
         else
            begin
            DataSet[i,ind_var] := 1;
            inc(j);
            end;

  {Se houve algum valor perdido no DataSet ent�o adiciona o n�vel csVALOR_PERDIDO.}
  if j > 0 then
     begin
     TwsFactor(ColunaNova).AddLevel(csVALOR_PERDIDO);
     TwsQuantitative(ColunaNova).LevelValues[k+1] := wscMissValue;
     end;

  {Atribui a nova coluna ao DataSet e libera a coluna antiga.
   ind_var-1 porque a propriedade Objects come�a em zero.}
  DataSet.Struct.Cols.Objects[ind_var-1] := ColunaNova;
  ColunaAntiga.Free;

end; {ChangeToFactor}
{$endif CHUVAZ}

Procedure ChangeToFactorEx(DataSet: TwsDataSet; ind_var: Integer; tipo_ordenado: boolean;
  s_nome,s_Lab:string; lst_niveis: TList);
{  Objetivo
     Recodifica uma vari�vel do tipo num�rica para tipo Fator Qualitativo ou Qualitativo Ordenado. A
     vari�vel num�rica n�o � alterada e a vari�vel do tipo fator passa a constituir uma coluna
     adicional do conjunto de dados. A cria��o de cada n�vel do fator � feita a partir de
     especifica��es sobre a vari�vel num�rica: igualdade, intervalos, valoers perdidos, falta de
     especifica��o, etc. s�o consideradas para a defini��o. O funcionamento se d� do seguinte modo
      - Cada valor do conjunto de dados � verificado para ver se ele se encontra dentro de uma das
        faixas definidas nos records de lst_niveis.
      - Ao encontrar um record que fa�a com que o valor do dado esteja dentro da faixa, ent�o aquele
        dado do conjunto pertencer� ao n�vel s_novo_nome que est� definido no pr�prio record.
      - Caso o valor do dado n�o esteja dentro de nenhuma das faixas definidas em lst_niveis e nenhum
        dos records de list_niveis tenha o tipo = 4 ent�o ser� criado um n�vel chamado csOUTROS.
      - Caso haja um record com tipo = 4 e o dado n�o esteja dentro de nenhuma das faixas ent�o o nome
        do n�vel ser� pego de s_novo_nome desse mesmo record.
      - Se o dado for valor perdido ent�o ser� criado o n�vel csVALOR_PERDIDO.
  Par�metros
    DataSet       - DataSet que possui a vari�vel.
    ind_var       - Indice da vari�vel.
    tipo_ordenado - Define que tipo de Fator ser� criado:
                    True  - Qualitativo (TwsQualitative)
                    False - Qualitativo Ordenado (TwsOrdered)
    s_nome        - Nome para a vari�vel a ser criada.
    s_Lab         - Label para a vari�vel a ser criada.
    lst_niveis    - Lista com informa��es sobre como criar os niveis.
}
var
  i,j,ind_nova_col: Integer;   {indice da nova coluna criada.}
  rec             : TCodVar;
  coluna          : TwsFactor; {ponteiro para a coluna que ser� criada.}
  l_achou         : boolean;   {se encontrou uma condi��o que combina.}
begin

  {Cria uma coluna do tipo Qualitativo ou Qualitativo Ordenado.}
  If tipo_ordenado then
     DataSet.Struct.AddOrdered(s_nome, s_Lab)
  else
     DataSet.Struct.AddQualitative(s_nome, s_Lab);

  ind_nova_col := DataSet.NCols;
  coluna := TwsFactor(DataSet.Struct.Col[ind_nova_col]);

  {Cria os n�veis (classes) e atualiza o DataSet.}
  For i:= 1 to DataSet.NRows Do

      Begin {for i}
        // Ajusta tamanho do vetor para incluir nova coluna
        DataSet.Row[i].Len:=DataSet.NCols;
        If wsGLib.IsMissValue(DataSet[i, ind_var]) Then
           begin
           coluna.AddLevel(csVALOR_PERDIDO);
           DataSet[i, ind_nova_col] := coluna.LevelNames.IndexOf(csVALOR_PERDIDO);
           end
        Else
           Begin
           For j := 0 to lst_niveis.Count - 1 Do
               Begin
               rec := TCodVar(lst_niveis[j]^);
               l_achou := false;
               Case rec.Tipo Of
               {Igualdade.}
               0: If DataSet[i, ind_var] = rec.f_Valor Then
                     begin
                     l_achou := true;
                     break;
                     end;
               {Intervalo.}
               1: If (DataSet[i, ind_var] >= rec.f_inicio) and (DataSet[i, ind_var] <= rec.f_fim) Then
                     begin
                     l_achou := true;
                     break;
                     end;
               {Maior que.}
               2: If DataSet[i, ind_var] < rec.f_valor Then
                     begin
                     l_achou := true;
                     break;
                     end;
               {Menor que.}
               3: If DataSet[i, ind_var] > rec.f_valor Then
                     begin
                     l_achou := true;
                     break;
                     end;
               {Outros valores.}
               4: begin
                     l_achou := true;
                     break;
                  end;
               End; {case}
               End; {for J}
           if l_achou then
              begin
              coluna.AddLevel(rec.s_novo_nome);
              DataSet[i, ind_nova_col] := coluna.LevelNames.IndexOf(rec.s_novo_nome);
              end
           else
              begin
              coluna.AddLevel(csOUTROS);
              DataSet[i, ind_nova_col] := coluna.LevelNames.IndexOf(csOUTROS);
              end;

           End;

      End; {for i}

end; {ChangeToFactorEx}

{$ifdef CHUVAZ}                                            
Function SimpleTable(ind_var:longint; ind_peso: longint; DataSet: TWSDataSet;
                    lGeraIntervalos:boolean; LimSupSerie: double): TWSDataSet;
var
  i,
  j,        {Indice para a coluna Freq.}
  j1,j2,j3, {Utilizadas somente para otimiza��o - cont�m os valores j+1, j+2 e j+3.}
  j4, j5,
  n_linhas, {N�mero de linhas que ter� o conjunto de sa�da.}
  aux       : longint;
  f_peso    : double;
  obj       : TWSDataSetCol;
  v_linha   : TwsDFVec;
  s_var     : string;
  Col       : TwsQuantitative;
  LI, LS    : double;
begin
  if DataSet.NRows < 1 then
     raise Exception.Create('Erro: N�o existe observa��es no conjunto de dados');

  s_var := DataSet.Struct.Col[ind_var].Name;

  {numero de linhas que ter� o conj resultante.}
  n_linhas := TWSFactor(DataSet.Struct.Col[ind_var]).LevelNames.count;

  {cria um dataset vazio com n_linhas linhas e 0 colunas}
  Result := TWSDataSet.Create('S_'+s_var);
  Result.MLab := 'Tabela de Frequencias para ' + s_var;

  {Cria as estruturas para as colunas.}
  Result.Struct.AddColEx(CopyDescCol(DataSet.Struct.Col[ind_var]));
  If lGeraIntervalos Then
     Begin
     Result.Struct.AddNumeric('L_Inf'  ,'Limite Inferior');
     Result.Struct.AddNumeric('L_Sup'  ,'Limite Superior');
     j := 4; {Indice para a coluna Freq.}
     End
  Else
     j := 2; {Indice para a coluna Freq.}

  {Otimiza��es para ficar mais r�pido.}
  j1 := j + 1;
  j2 := j + 2;
  j3 := j + 3;
  j4 := j + 4;
  j5 := j + 5;
  Result.Struct.AddNumeric('Freq'       , 'Frequencia Absoluta');                            // j
  Result.Struct.AddNumeric('Fr_Rel'     , 'Frequencia Relativa');                            // j1
  Result.Struct.AddNumeric('Fq_Acum'    , 'Frequencia Absoluta Acumulada');                  // j2
  Result.Struct.AddNumeric('FR_Acum'    , 'Frequencia Relativa Acumulada');                  // j3
  Result.Struct.AddNumeric('Fq_Acum Dec', 'Valores >= ao limite inferior');                  // j4
  Result.Struct.AddNumeric('FR_Acum Dec', '% dos Valores que s�o >= ao limite inferior');    // j5

  {Adiciona linhas vazias e inicializa a coluna Freq com zero.}
  For i := 1 to n_Linhas do
    begin
    v_linha := TwsDFVec.Create(Result.NCols);
    Result.MAdd(v_linha);
    Result[i, j] := 0;
    end;

  {Calcula os valores da coluna Freq.}
  If ind_peso <> -1 Then
     {Calcula a freq��ncia utilizando Peso.}
     For i := 1 To DataSet.NRows Do
       Begin
       f_peso := DataSet[i,ind_peso];
       aux := Trunc(DataSet[i,ind_var]) + 1; {Fa�o o Trunc para retornar um valor
         inteiro e somo +1 pq os valores contidos em DataSet[i,ind_var]
         come�am em zero e a vari�vel aux � utilizada como �ndice
         para as linhas do conjunto de sa�da logo abaixo, e os �ndices come�am em 1.}
       Result[aux,j] := Result[aux,j] + f_peso
       End
  Else {ind_peso=-1}
     {Calcula a freq��ncia sem utilizar Peso.}
     For i := 1 To DataSet.NRows Do
       Begin
       aux := trunc(DataSet[i,ind_var]) + 1; {idem anterior...}
       Result[aux,j]:= Result[aux,j] + 1; {sem vari�vel peso, logo somar +1}
       End;

  {Calcula as outras frequencias.}

  {x1 e x2}
  Result[1, j4] := DataSet.NRows;
  Result[1, j5] := 100;

  Col := TwsQuantitative(Result.Struct.Col[1]);
  For i := 1 to Result.NRows do
    begin
    {A variavel em questao.}
    Result[i, 1] := i - 1;
    If lGeraIntervalos Then {L_Inf e L_Sup}
       Begin
       LI := Col.LevelValues[i];

       if i < Result.NRows then
          LS := Col.LevelValues[i + 1]
       else
          LS := LimSupSerie;

       Result[i, 2] := LI;

       if not IsMissValue(Col.LevelValues[i]) then
          Result[i, 3] := LS
       else
          Result[i, 3] := wscMissValue
       End;

    {Fr_Rel}
    Result[i,j1] := Result[i,j] / DataSet.NRows;

    {Fr_Acum}
    if i = 1 then
       Result[i,j2] := Result[i,j]
    else
       Result[i,j2] := Result[i,j] + Result[i-1,j2];

    {FR_Acum}
    Result[i,j3] := Result[i,j2] / DataSet.NRows;

    if i > 1 then
       begin
       {x1}
       Result[i, j4] :=  Result[i-1, j4] - Result[i-1, j];

       {x2}
       Result[i, j5] := (Result[i, j4] / DataSet.NRows) * 100;
       end;
    end;{for}

end; {SimpleTable}
{$else}
Function SimpleTable(ind_var:Integer; ind_peso:Integer; DataSet:TwsDataSet;
  lGeraIntervalos:boolean; f_amplitude: double):TwsDataSet;
{  Objetivo
     Obt�m a tabela de frequ�ncias para uma vari�vel do tipo fator
   Par�metros
     ind_var  - Indice da vari�vel a ser analisada. Deve ser do tipo Factor.
     ind_peso - Indice da variavel que ser� utilizada como peso. Dever� conter (-1) caso n�o se
       deseje utilizar nenhuma vari�vel como peso.
     DataSet  - conjunto de dados a ser analisado.
     lGeraIntervalos - Se True entao a vari�vel sendo analisada era do tipo num�rica. Cria duas
       colunas contendo o limite inferior e superior do intervalo. O limite inferior � igual ao valor
       do n�vel (LevelValues) e o limite superior � igual ao valor do n�vel + f_amplitude. Se o valor
       do n�vel for igual a wscMissValue, entao o limite superior tamb�m ser� igual a wscMissValue. Se
       lGeraIntervalos=True ent�o a vari�vel analisada deve obrigatoriamente ser do tipo Quantitativo.
     f_amplitude - Amplitude do intervalo. Deve ter sido calculado por ChangeToFactor.
  Retorno:
    Retorna um conjunto de dados que cont�m a tabela constru�da. O formato do resultado � o seguinte:

  ------------------ Para Vari�vel num�rica (dtNumeric) -----------------------

  Obs.: A coluna (variavel) Var1 era do tipo dtNumeric e foi transformada para tipo Factor (Quantitative).

  Observ.        Var1    L_Inf    L_Sup    Freq    Fr_Rel    Fq_Acum    FR_Acum
        1     Nivel_1
        2     Nivel_2
        3     Nivel_3                                                         1

  ------------------- Para Vari�vel Fator (Factor) ----------------------------

  Observ.        Var1     Freq     Fr_Rel     Fq_Acum     FR_Acum
        1     Nivel_1
        2     Nivel_2
        3     Nivel_3                                           1
}
var
  i,
  j,        {Indice para a coluna Freq.}
  j1,j2,j3, {Utilizadas somente para otimiza��o - cont�m os valores j+1, j+2 e j+3.}
  n_linhas, {N�mero de linhas que ter� o conjunto de sa�da.}
  aux       : Integer;
  f_peso    : double;
  obj       : TwsDataSetCol;
  v_linha   : TwsDFVec;
  s_var     : string;
begin
  if DataSet.NRows < 1 then
     Raise Exception.Create('Erro: Conjunto de dados sem observa��es');

  // nome da variavel para analise
  s_var := DataSet.Struct.Col[ind_var].Name;

  {numero de linhas que ter� o conj resultante.}
  n_linhas := TwsFactor(DataSet.Struct.Col[ind_var]).LevelNames.Count;

  {cria um dataset vazio com 0 linhas e 0 colunas}
  Result := TwsDataSet.Create('S_'+s_var);
  Result.MLab := 'Tabela de Frequencias para ' + s_var;

  {Cria as estruturas para as colunas.}
  Result.Struct.AddColEx(CopyDescCol(DataSet.Struct.Col[ind_var]));  // Coluna 1
  If lGeraIntervalos Then
     Begin
     Result.Struct.AddNumeric('L_Inf', 'Limite Inferior');           // Coluna 2
     Result.Struct.AddNumeric('L_Sup', 'Limite Superior');           // Coluna 3
     j := 4; {Indice para a coluna Freq.}
     End
  Else
     j := 2; {Indice para a coluna Freq.}
  {Otimiza��es para ficar mais r�pido.}
  j1 := j+1;
  j2 := j+2;
  j3 := j+3;
  Result.Struct.AddNumeric('Freq','Frequencia Absoluta',13,7);         // Coluna 4
  Result.Struct.AddNumeric('Fq_Rel' ,'Frequencia Relativa',10,5);         // Coluna 5
  Result.Struct.AddNumeric('Fq_Acum','Frequencia Absoluta Acumulada',13,7);  // Coluna 6
  Result.Struct.AddNumeric('FR_Acum','Frequencia Relativa Acumulada',10,5);  // Coluna 7

  {Adiciona linhas vazias e inicializa a coluna Freq com zero.}
  For i:=1 to n_Linhas do
    begin
    v_linha := TwsDFVec.Create(Result.NCols);
    Result.MAdd(v_linha);
    Result[i,j]:=0;
    end;

  {Calcula os valores da coluna Freq.}
  If ind_peso <> -1 Then
     {Calcula a freq��ncia utilizando Peso.}
     For i := 1 To DataSet.NRows Do
       Begin
       f_peso := DataSet[i,ind_peso];
       aux := Trunc(DataSet[i,ind_var]) + 1; {Fa�o o Trunc para retornar um valor
         inteiro e somo +1 pq os valores contidos em DataSet[i,ind_var]
         come�am em zero e a vari�vel aux � utilizada como �ndice
         para as linhas do conjunto de sa�da logo abaixo, e os �ndices come�am em 1.}
       Result[aux,j] := Result[aux,j] + f_peso
       End
  Else {ind_peso=-1}
     {Calcula a freq��ncia sem utilizar Peso.}
     For i := 1 To DataSet.NRows Do
       Begin
       aux := trunc(DataSet[i,ind_var]) + 1; {idem anterior...}
       Result[aux,j]:= Result[aux,j] + 1; {sem vari�vel peso, logo somar +1}
       End;

  {Calcula as outras frequencias.}
  For i := 1 to Result.NRows do
    begin
    {A variavel em questao.}
    Result[i, 1] := i - 1;
    If lGeraIntervalos Then {L_Inf e L_Sup}
       Begin
       Result[i,2] := TwsQuantitative(Result.Struct.Col[1]).LevelValues[i];
       if not wsGLib.IsMissValue(TwsQuantitative(Result.Struct.Col[1]).LevelValues[i]) then
          Result[i,3] := Result[i,2] + f_amplitude
       else
          Result[i,3] := wscMissValue
       End;

    {Fr_Rel}
    Result[i,j1] := Result[i,j] / DataSet.NRows;

    {Fr_Acum}
    if i = 1 then
       Result[i,j2] := Result[i,j]
    else
       Result[i,j2] := Result[i,j] + Result[i-1,j2];

    {FR_Acum}
    Result[i,j3] := Result[i,j2] / DataSet.NRows;

    end;{for}

end; {SimpleTable}
{$endif CHUVAZ}

Function SimpleValueTable(ind_var:Integer; DataSet: TwsDataSet): TwsDataSet;
{ Objetivo
    Cria conjunto de dados com as frequ�ncias de valores uma vari�vel num�rica por valor
    e n�o por intervalo
  Par�metros
    ind_var: �ndice da vari�vel
    DataSet: Conjunto de dados
}
var
  s_var : string;
  i,f,af: Integer;
  x     : Double;
  v     : TwsVec;
  s1,s2 : TwsLIVec;
begin
  if DataSet.NRows < 1 then
     Raise Exception.Create('Erro: Conjunto de dados sem observa��es');
  // nome da variavel para analise
  s_var := DataSet.Struct.Col[ind_var].Name;

  // Ordena coluna em ordem ascendente
  s1:=TwsLIVec(VecConst(ind_var,1,False));  // quem vai ser ordenado
  s2:=TwsLIVec(VecConst(1,1,False));        // em ordem ascendente
  DataSet.SortRows(s1,s2);
  s1.Free; s2.Free;

  // Conjunto de dados com os resultados
  Result := TwsDataSet.Create('FreqVal');
  Result.MLab := 'Tabela de frequencias por valor para ' + s_var;

  {Cria as estruturas para as colunas.}
  Result.Struct.AddNumeric(s_var,'Valores distintos da vari�vel');         // Coluna 1
  Result.Struct.AddNumeric('Freq','Frequ�ncia do valor',8,7);              // Coluna 2
  Result.Struct.AddNumeric('F_Relat','Frequ�ncia relativa do valor',8,5);  // Coluna 3
  Result.Struct.AddNumeric('F_Acum','Frequ�ncia acumulada',8,5);           // Coluna 4
  Result.Struct.AddNumeric('FR_Acum','Frequ�ncia relativa acumulada',8,5); // Coluna 5
  f:=1;  // Numero de valores iguais ao valor considerado
  af:=0;     // Frequencia acumulada
  i:=1;
  repeat
    x:=DataSet[i,ind_var];
    while (i<DataSet.nRows) do
      if FEquals(x,DataSet[i+1,ind_var]) then
        begin
        Inc(f);
        Inc(i)
        end
      else
        Break;
    Inc(af,f);
    v:=TwsDFVec.Create(5);
    v[1]:= x;
    v[2]:= f;
    v[3]:= f/DataSet.NRows;
    v[4]:= af;
    v[5]:= af/DataSet.NRows;
    Result.MAdd(v);
    Inc(i);
    f:=1;
  until (af=DataSet.nRows);
end; // SimpleValueTable

function VecToFreq(v: TwsVec): TwsDataSet;
{ Objetivo
    Obt�m uma tabela de frequ�ncias a partir de dados em um vetor
  Par�metros
    v: Vetor com os dados
  Valores perdidos
    N�o trata
}
var
  i,j,k,
  n,ii,i1: Integer;
  amp    : Double;
  lst    : TList;
  L      : TwsVec;
begin
  Result := TwsDataSet.Create('Dist_Freq');
  with Result do
    begin
    MLab := 'Distribui��o de Frequ�ncias '+v.Name;
    Struct.AddNumeric('L_Inf', 'Limite Inferior',10,5);                 // 1
    Struct.AddNumeric('L_Sup', 'Limite Superior',10,5);                 // 2
    Struct.AddNumeric('Freq','Frequencia Absoluta',11,8);               // 3
    Struct.AddNumeric('Fq_Rel' ,'Frequencia Relativa',10,5);            // 4
    Struct.AddNumeric('Fq_Acum','Frequencia Absoluta Acumulada',11,8);  // 5
    Struct.AddNumeric('FR_Acum','Frequencia Relativa Acumulada',10,5);  // 6
    end;
  n:=v.Len;
  v.QuickSort(true);
  k:=Trunc(3.322*Log10(n)+1)+1;  // numero de classes
  amp:=(v[v.Len]-v[1])/k;
  L:=TwsDFVec.Create(6);
  L[1]:=v[1];
  L[2]:=v[1]+amp;
  L[3]:=0;
  L[5]:=0;
  L[6]:=0;
  Result.MAdd(L);
  for i:=2 to k do
    begin
    L:=TwsDFVec.Create(6);
    L[1]:=Result[i-1,1]+amp;
    L[2]:=L[1]+amp;
    L[5]:=0;
    L[6]:=0;
    Result.MAdd(L)
    end;
  i:=1; i1:=0;
  for j:=1 to k-1 do
    begin
    ii:=0;
    while (v[i]<Result[j,2]) and (i<=n) do
      begin
      Inc(ii);
      Inc(i1);
      Inc(i)
      end;
    Result[j,3]:=ii;
    Result[j,4]:=ii/n;
    Result[j,5]:=i1;
    Result[j,6]:=i1/n
    end;
  // Completa a ultima classe
  Result[k,3]:=n-Result[k-1,5];
  Result[k,4]:=Result[k,3]/n;
  Result[k,5]:=Result[k-1,5]+Result[k,3];
  Result[k,6]:=Result[k-1,6]+Result[k,4]

end; // VecToFreq

Function CrossTable(ind_var1, ind_var2, ind_peso:Integer; DataSet: TwsDataSet; var v_TotLin,
  v_TotCol: TwsVec; var f_freq_tot: double):TwsDataSet;
{  Objetivo
     Constr�i uma tabela de frequ�ncias cruzada. Ambas as vari�veis devem ser do tipo fator
   Par�metros:
     ind_var1   - indice da variavel 1 no conjunto de dados.
     ind_var1   - indice da variavel 2 no conjunto de dados.
     ind_peso   - indice da variavel que ser� utilizada como peso. Dever� conter (-1) caso n�o
       se deseje utilizar nenhuma vari�vel como peso.
     DataSet    - Conjunto de dados do qual ser� feita a an�lise.
     v_TotLin   - Retorna um vetor contendo a soma das frequencias de cada linha.
     v_TotCol   - Retorna um vetor contendo a soma das frequencias de cada coluna.
     f_freq_tot - Retorna a soma das frequencias (Frequencia total).
  Retorno:
    Retorna um conjunto de dados com o resultado da analise de frequencias. O formato do
    resultado �:

  Tabela de Frequencias para Var1 x Var2

  Observ.        Var1       Var2    Freq    F_R_Lin    F_R_Col    F_R_Tot
        1     Nivel_1    Nivel_A
        2     Nivel_1    Nivel_B
        3     Nivel_1    Nivel_C
        4     Nivel_2    Nivel_A
        5     Nivel_2    Nivel_B
        6     Nivel_2    Nivel_C
        7     Nivel_3    Nivel_A
        8     Nivel_3    Nivel_B
        9     Nivel_3    Nivel_C
}
var
  i,j,k,l,m,
  aux,aux1,aux2: Integer;
  f_valpeso    : double;
  Obj          : TwsDataSetCol;
  s_var1,s_var2: string;
  v              : TwsDFVec;
begin

  s_var1 := DataSet.Struct.Col[ind_var1].Name;
  s_var2 := DataSet.Struct.Col[ind_var2].Name;

{  v_TotLin:=VecCreate(TwsFactor(DataSet.Struct.Col[ind_var1]).LevelNames.Count);}
  v_TotLin := VecConst(0,TwsFactor(DataSet.Struct.Col[ind_var1]).LevelNames.Count);
  v_TotCol := VecConst(0,TwsFactor(DataSet.Struct.Col[ind_var2]).LevelNames.Count);

  {Cria o DataSet que conter� a an�lise de frequencias.}
  aux:=v_TotLin.len*v_TotCol.len;
  Result:=TwsDataSet.Create('C_'+s_var1+'_'+s_var2);
  with Result do
    begin
    MLab := 'Tabela de Frequencias para ' + s_var1 + ' x ' + s_var2;
    Struct.AddCopyCol(DataSet.Struct.Col[ind_var1]);                     // Coluna 1
    Struct.AddCopyCol(DataSet.Struct.Col[ind_var2]);                     // Coluna 2
    Struct.AddNumeric('Freq','Frequencia Absoluta',13,7);                // Coluna 3
    Struct.AddNumeric('FR_Lin','Frequencia Relativa para '+s_var1,10,4); // Coluna 4
    Struct.AddNumeric('FR_Col','Frequencia Relativa para '+s_var2,10,4); // Coluna 5
    Struct.AddNumeric('FR_Tot','Frequencia Relativa Total',10,4);        // Coluna 6
    end;
  {Zera a coluna Frequencia.}
  for i:=1 to aux do
    begin
    v:=TwsDFVec.Create(6);
    v[3]:=0;
    Result.MAdd(v);
    end;
  f_freq_tot := 0;

  {Calcula: Frequencias (coluna[3]), v_TotLin, v_TotCol e f_freq_tot.}
  if ind_peso = -1 then
     for i := 1 to DataSet.NRows do
       begin
       aux := trunc(DataSet[i,ind_var1] * v_TotCol.Len + Dataset[i,ind_var2]) + 1;
       Result[aux,3] := Result[aux,3] + 1;
       f_freq_tot := f_freq_tot + 1;
       aux1 := DataSet.AsInteger[i,ind_var1] + 1 ;
       aux2 := DataSet.AsInteger[i,ind_var2] + 1;
       v_TotLin[aux1] := v_TotLin[aux1] + 1;
       v_TotCol[aux2] := v_TotCol[aux2] + 1;
       end
  else {ind_peso <> -1}
     for i := 1 to DataSet.NRows do
       begin
       f_valpeso := DataSet[i,ind_peso];
       aux := trunc(DataSet[i,ind_var1] * v_TotCol.Len + Dataset[i,ind_var2]) + 1;
       Result[aux,3] := Result[aux,3] + f_valpeso;
       f_freq_tot := f_freq_tot + f_valpeso;
       aux1 := DataSet.AsInteger[i,ind_var1] + 1 ;
       aux2 := DataSet.AsInteger[i,ind_var2] + 1;
       v_TotLin[aux1] := v_TotLin[aux1] + f_valpeso;
       v_TotCol[aux2] := v_TotCol[aux2] + f_valpeso;
       end;

  {Seta os dados do DataSet, colunas 1, 2, 4, 5, 6}
  j:=1;
  for i:=1 to v_TotLin.Len do
    for m:= 1 to v_TotCol.Len do
      begin
      Result.AsString[j,1] := TwsFactor(DataSet.Struct.Col[ind_var1]).LevelNames[i-1];
      Result.AsString[j,2] := TwsFactor(DataSet.Struct.Col[ind_var2]).LevelNames[m-1];
      Result[j,4] := ScalarDiv(Result[j,3], v_TotLin[i]);
      Result[j,5] := ScalarDiv(Result[j,3], v_TotCol[m]);
      Result[j,6] := ScalarDiv(Result[j,3], f_freq_tot);
      inc(j);
      end;

end; {CrossTable}

Procedure DSFreqToMatFreqSimples(DataSet: TwsDataSet; v_TotLin,v_TotCol: TwsVec;
  f_freq_tot: double; Var F: TwsGeneral);
{  Objetivo
     Transforma um conjunto de dados com uma tabela de frequ�ncias cruzadas numa matriz onde
     nas linhas est�o os n�veis do primeiro fator e nas colunas est�o os n�veis do segundo fator.
   Par�metros
     DataSet    - Conjunto de dados com o resultado da analise de frequencias do cruzamento de duas
                  vari�veis.
     v_TotLin   - Vetor contendo os totais de frequ�ncias de cada linha.
     v_TotCol   - Vetor contendo os totais de frequ�ncias de cada coluna.
     f_freq_tot - Total geral ou soma de todas as frequ�ncias
     F          - Matriz com as frequ�ncias. O formato do resultado �:

  Matriz de Frequencias para Var1 X Var2

            Nivel_A  Nivel_B  Nivel_C    ...   Total
  Nivel_1
  Nivel_2
  Nivel_3
     .
     .
     .
  Total
}
var
  i,j,k,l,p,
  lin,Erro  : Integer;
  s_nome    : string;
begin

  k := TwsFactor(DataSet.Struct.Col[1]).LevelNames.Count;
  l := TwsFactor(DataSet.Struct.Col[2]).LevelNames.Count;

  F := TwsGeneral.Create(k+1, l+1);

  {Coloca as frequencias na matriz. Linha para o primeiro fator, coluna para o segundo}
  for i := 1 to DataSet.NRows do
    begin
    k := DataSet.AsInteger[i,1] + 1;
    l := DataSet.AsInteger[i,2] + 1;
    F[k,l] := DataSet[i,3];
    end;

  F.Name := 'Freq_Assoc'; {ou Mat_Freq}
  F.MLab := 'Matriz de Frequencias para ' + DataSet.Struct.Col[1].Name + ' x ' +
            DataSet.Struct.Col[2].Name;

  {Seta nome das linhas e
   prenche a coluna 'Total' com a soma das frequencias das linhas.}
  for i:= 1 to F.NRows-1 do
      begin
      F.RowName[i] := TwsFactor(DataSet.Struct.Col[1]).LevelNames[i-1];
      F[i,F.NCols] := v_TotLin[i];
      end;
  F.RowName[F.NRows] := 'Total';

  {Seta nome das colunas e
   prenche a linha 'Total' com a soma das frequencias das colunas.}
  for i:= 1 to F.NCols-1 do
      begin
      F.ColName[i] := GetValidId(TwsFactor(DataSet.Struct.Col[2]).LevelNames[i-1]);
      F[F.NRows,i] := v_TotCol[i];
      end;
  F.ColName[F.NCols] := 'Total';

  F[F.NRows, F.NCols] := f_freq_tot;

end; {DSFreqToMatFreqSimples}

Procedure DSFreqToMatFreqCompleta(DataSet: TwsDataSet; v_TotLin, v_TotCol: TwsVec; f_freq_tot: double;
  Var F : TwsGeneral);
{  Objetivo
     Transforma um conjunto de dados com uma tabela de frequ�ncias cruzada numa matriz onde
     nas linhas est�o os n�veis do primeiro fator. Inclui outras quantidades como totais e
     percentuais de linhas, colunas, etc. e nas colunas est�o os n�veis do segundo fator
   Par�metros
     DataSet    - Conjunto de dados com o resultado da analise de frequencias do cruzamento de duas
                  vari�veis.
     v_TotLin   - Vetor contendo os totais de frequ�ncias de cada linha.
     v_TotCol   - Vetor contendo os totais de frequ�ncias de cada coluna.
     f_freq_tot - Total geral ou soma de todas as frequ�ncias
     F          - Matriz com as frequ�ncias. O formato do resultado �:

  Matriz de Frequencias para Var1 X Var2
            Nivel_A  Nivel_B  Nivel_C    Total
  Nivel_1
    Lin %                                    .
    Col %'                                   .
    Tot %'
  Nivel_2
    Lin %                                    .
    Col %'                                   .
    Tot %'
  Nivel_3
    Lin %                                    .
    Col %'                                   .
    Tot %'
  Total
  Total %                                  100
}
var
  i,j,k,l,p,lin: Integer;
  v1,v2,v3     : TwsVec;
begin

  k := TwsFactor(DataSet.Struct.Col[1]).LevelNames.Count;
  l := TwsFactor(DataSet.Struct.Col[2]).LevelNames.Count + 1;

  p:=2;
  lin := 0;
  For i := 1 to k do
    begin
    v1 := TwsDFVec.Create(l);
    v2 := TwsDFVec.Create(l);
    v3 := TwsDFVec.Create(l);
    v1.Name := 'Lin %';
    v2.Name := 'Col %';
    v3.Name := 'Tot %';
    v3[l] := 0;
    For j := 1 to l-1 do
      begin
      inc(lin);
      v1[j] := ScalarProd(DataSet[Lin,4],100);
      v2[j] := ScalarProd(DataSet[Lin,5],100);
      v3[j] := ScalarProd(DataSet[Lin,6],100);
      end;
    v1[l] := wscMissValue;
    v2[l] := wscMissValue;

    v3[l] := ScalarDiv(v_TotLin[i], f_freq_tot);
    v3[l]:=ScalarProd(v3[l],100);

    F.MInsert(p, v1);
    F.MInsert(p+1, v2);
    F.MInsert(p+2, v3);
    inc(p, 4);
    end;

  {Insere a ultima linha da matriz ('Total %') e calcula
   os valores para a mesma.}
  v1 := TwsDFVec.Create(l);
  v1.Name := 'Total %';
  F.MInsert(F.NRows + 1, v1);
  for j:= 1 to l-1 do
    begin
    F[F.NRows, j] := ScalarDiv(v_TotCol[j],f_freq_tot);
    F[F.NRows, j] := ScalarProd(F[F.NRows, j],100);
    end;
  F[F.NRows, F.NCols] := 100;

end; {DSFreqToMatFreqCompleta}

Function Qui_Quad(DataSet: TwsDataSet; var E,R,SR: TwsGeneral; var GL: Integer; v_TotLin,
  v_TotCol: TwsVec; f_freq_tot: double; var Erro: Word): TwsGeneral;
{  Objetivo
     Dada uma tabela de classifica��o cruzada e ourtas quantidades, aplica diversos testes
     de associados � estat�stica qui-quadrado. As estat�siticas calculadas s�o:
     [1] - Qui-quadrado
     [2] - Qui-quadrado Ajustado  se GL = 1; wscMissValue caso contrario .
     [3] - Raz�o Verossimilhan�a
     [4] - Coeficiente Phi
     [5] - Coeficiente de Contingencia
     [6] - Coeficiente V de Cramer
   Par�metros
     DataSet - Conjunto de dados com o resultado da analise de frequencia do cruzamento de duas
       vari�veis. As colunas 1 e 2 contem os Fatores dos quais foram feitas as an�lises, a coluna
       3 (num�rica) cont�m as frequencias.
     E  - Retorna matriz com o valor esperado.
     R  - Retorna matriz com os res�duos (valor observado - esperado)
     SR - Retorna matriz com os res�duos padronizados
     GL - graus de Liberdade.
     f_freq_tot - Frequencia total geral.
     v_TotLin - Vetor contendo a soma das frequencias de cada linha.
     v_TotCol - Vetor contendo a soma das frequencias de cada coluna.
     Erro - Indica algum tipo de erro ou observa��o a considerar conforme o valor retornado. No caso
       ocorr�ncia de erro, o c�lculo do Qui-Quadrado n�o � realizado. No caso de observa��o, os
       c�lculos s�o feitos sem problema.
       (0) - Nao ocorreu nenhum erro.
       (1) - Erro, algum total de linha menor ou igual a zero.
       (2) - Erro, algum total de coluna menor ou igual a zero.
       (3) - Obs.: Ocorrencia de frequencia observada < 5
       (4) - Obs.: Ocorrencia de frequencia esperada < 1
       (5) - Obs.: Ocorrencia de (3) e (4).
     Retorno:
       Retorna uma matriz com os resultados.
}
var
  i,j,k,lx,cx : Integer;
  QP          : TwsVec;
{Conte�do do vetor auxiliar QP:
[1] - Qui-quadrado
[2] - Qui-quadrado Ajustado  se GL = 1; wscMissValue caso contrario .
[3] - Raz�o Verossimilhan�a
[4] - Coeficiente Phi
[5] - Coeficiente de Contingencia
[6] - Coeficiente V de Cramer
}
  erro_aux    : Word;
  aux         : double;
begin
 {Verifica��o de ERRO: se h� algum total de linha ou de coluna igual a zero}
  for i := 1 to v_TotLin.Len do
     if v_TotLin[i] <= 0 Then
        begin
        Erro := 1;
        exit;
        end;
  for i := 1 to v_TotCol.Len do
     if v_TotCol[i] <= 0 Then
        begin
        Erro := 2;
        exit;
        end;
  Result := TwsGeneral.Create(6,3);
  Result.Name := 'Estatisticas';
  Result.MLab := 'Estat�sticas Qui Quadrado e associadas';
  Result.RowName[1] := 'Qui Quadrado';
  Result.RowName[2] := 'Qui Quadr Ajust';
  Result.RowName[3] := 'Razao Verossim';
  Result.RowName[4] := 'Coeficiente Phi';
  Result.RowName[5] := 'Coefic. Conting';
  Result.RowName[6] := 'V de Cramer';
  Result.ColName[1] := 'GL';
  Result.ColName[2] := 'Valor';
  Result.ColName[3] := 'Prob';

  {Cria as matrizes E, R e SR.}
  E := TwsGeneral.Create(v_TotLin.len, v_TotCol.len);
  E.Name := 'Val_Esp';
  E.MLab := 'Valores Esperados Para Tabela ' + DataSet.Name;
  R := TwsGeneral.Create(v_TotLin.len, v_TotCol.len);
  R.Name := 'Residuos';
  R.MLab := 'Residuos Para Tabela ' + DataSet.Name;;
  SR := TwsGeneral.Create(v_TotLin.len, v_TotCol.len);
  SR.Name:= 'Res_Pad';
  SR.MLab := 'Residuos Padronizados Para Tabela ' + DataSet.Name;;;

  {Seta os nomes das colunas.}
  For i := 1 To E.NCols Do
    begin
    E.ColName[i] := TwsFactor(DataSet.Struct.Col[2]).LevelNames[i-1];
    R.ColName[i] := TwsFactor(DataSet.Struct.Col[2]).LevelNames[i-1];
    SR.ColName[i] := TwsFactor(DataSet.Struct.Col[2]).LevelNames[i-1];
    end;
  {Seta os nomes das linhas.}
  For i := 1 To E.NRows Do
    begin
    E.RowName[i] := TwsFactor(DataSet.Struct.Col[1]).LevelNames[i-1];
    R.RowName[i] := TwsFactor(DataSet.Struct.Col[1]).LevelNames[i-1];
    SR.RowName[i] := TwsFactor(DataSet.Struct.Col[1]).LevelNames[i-1];
    end;

  GL:= (v_TotLin.len - 1) * (v_TotCol.len - 1);
  QP := VecConst(0,6);  // Armazena as estatisticas
  QP[2]:=wscMissValue;     // Permanece como valor perdido se GL>1
  k := 1;

  {C�lculos e verifica��o de ERROS(observa��o).
  Se no final do la�o, Erro <> 0, ent�o significa que existe algum
  valor observado < 5 na coluna Frequencias do DataSet.
  Se erro_aux <> 0, ent�o significa que existe algum valor esperado < 1.}
  Erro := 0;
  erro_aux := 0;
  for i := 1 to v_TotLin.len do
      for j := 1 to v_TotCol.len do
          begin
          lx := DataSet.AsInteger[k, 1] + 1; {adg 22/07/1998}
          cx := DataSet.AsInteger[k, 2] + 1; {adg 22/07/1998}
          E[lx, cx] := ScalarDiv((ScalarProd(v_TotCol[cx],v_TotLin[lx])),f_freq_tot);
          R[lx, cx] := ScalarSub(Dataset[k,3],E[lx, cx]);
          SR[lx, cx] := ScalarDiv(R[lx,cx],Sqrt(E[lx,cx]));
          // Frequencia observada menor que 5?
          if (DataSet[k,3]<5) then
            Inc(Erro);
          // Frequencia esperada menor que 1?
          if (E[lx, cx] < 1) then
            Inc(erro_aux);
          {Qui-Quadrado nao ajustado}
          QP[1] := QP[1] + Sqr(SR[lx,cx]);
          {Raz�o-Verossimilhan�a}
          If DataSet[k,3] <> 0 Then
             QP[3] := ScalarSum(QP[3],
               ScalarProd(DataSet[k,3],ScalarLn(ScalarDiv(DataSet[k,3],E[lx,cx]))));
          inc(k);
        end;
  Qp[3]:=ScalarProd(2,QP[3]);

  {Atribui o valor adequado a Erro.}
  if Erro <> 0 Then
     if Erro_aux <> 0 then
        Erro := 5
     else
        Erro := 3
  else
     if Erro_aux <> 0 then
        Erro := 4;

  if GL = 1 then
     begin
     aux:=ScalarProd(DataSet[1,3],DataSet[4,3]);
     aux:=ScalarSub(aux,ScalarProd(DataSet[2,3],DataSet[3,3]));
     aux:=Abs(aux);
     // X2 ajustado para 1 GL
     if aux > f_freq_tot/2 then
       QP[2]:=ScalarDiv(f_freq_tot*ScalarSqr(aux-f_freq_tot/2),
         v_TotLin[1]*v_TotLin[2]*v_TotCol[1]*v_TotCol[2])
     else
       QP[2]:=0;

     //Coeficiente Phi para o caso 2x2
     QP[4]:=ScalarSqrt(ScalarDiv(QP[1],f_freq_tot));

     // Coeficiente V de Cramer para o caso 2x2
     QP[6]:= QP[4];
     end
  else
     begin
     {Coeficiente Phi}
     QP[4]:= ScalarSqrt(ScalarDiv(QP[1],f_freq_tot));

     {Coeficiente V de Cramer}
     QP[6]:= ScalarSqrt(ScalarDiv(ScalarDiv(QP[1], f_freq_tot),
                  Math.Min(v_TotLin.len-1, v_TotCol.len-1)));
     end;

  {Coeficiente de Contingencia}
  QP[5]:= Sqrt(QP[1]/(QP[1]+f_freq_tot));

  Result[1,1] := GL;
  if GL = 1 then
    Result[2,1] := GL
  else
    Result[2,1] := wscMissValue;
  Result[3,1] := GL;
  Result[4,1] := wscMissValue;
  Result[5,1] := wscMissValue;
  Result[6,1] := wscMissValue;

  Result[1,2] := Qp[1];  // X2
  Result[2,2] := Qp[2];  // X2 ajustado 2x2
  Result[3,2] := Qp[3];  // Razao de verossimilhanca
  Result[4,2] := Qp[4];  // Coeficiente Phi
  Result[5,2] := Qp[5];  // Coeficiente de contingencia
  Result[6,2] := Qp[6];  // V de Cramer

  Result[1,3] := X2Int(Qp[1], GL, True, Erro);
  if GL=1 then
    Result[2,3] := X2Int(Qp[2], GL, True, Erro)
  else
    Result[2,3] := wscMissValue;
  Result[3,3] := X2Int(Qp[3], GL, True, Erro);
  Result[4,3] := wscMissValue;
  Result[5,3] := wscMissValue;
  Result[6,3] := wscMissValue;

end; {Qui_Quad}

procedure FreqAssociation(DtSet: TwsDataSet; v_TotLin,v_TotCol: TwsVec; wTot: double;
  F : TwsGeneral; var Stat : TwsGeneral);
{Objetivo
    Obt�m medidas de associa��o em tabelas de dupla entrada. As estat�sticas (e respectivos erros
    padr�es assint�ticos) obtidas s�o Gamma, Tau-b de Kendall, Tau-c de Stuart e D(C\R) de Sommers
 Par�metros
   DtSet    - Conjunto de dados com as frequencias
   wTot     - Soma das frequencias
   v_TotLin - Totais de linhas
   v_TotCol - Totais de colunas
   F        - Matriz de Frequencias Simples
   Stat     - Retorna a Matriz de Estat�sticas de Associa��o.
}
var
  i,j,k,l,m,nr1,nc1          : Integer;
  Fij,w,wr,wc,s1,s2,s3,s4,P,Q: double;
  A,D                        : TwsGeneral;
  v_stat,v_AsE,v_aux         : TwsVec;

  { Vetores para estatisticas e erros padroes assint�ticos:
      v_stat[1]: Gamma
      v_stat[2]: Tau-b de Kendall
      v_stat[3]: Tau-c de Stuart
      v_stat[4]: D(C\R) de Sommers
  }
begin
  k := TwsFactor(DtSet.Struct.Col[1]).LevelNames.Count;
  l := TwsFactor(DtSet.Struct.Col[2]).LevelNames.Count;
  A := TwsGeneral.Create(k, l); { Para obter o numero de concordancias }
  D := TwsGeneral.Create(k, l); { Para obter o numero de discordancias }

  nr1 := F.NRows - 1;
  nc1 := F.NCols - 1;
  P := 0; Q := 0;
  wr := wTot*WTot; { wTot - Frequencia total }
  wc := wr;
  for i := 1 to nr1 do
    begin
    wr := wr - v_TotLin[i]*v_TotLin[i];
    for j := 1 to nc1 do
      begin
      s1 := 0;
      for k := i+1 to nr1 do
        for l := j+1 to nc1 do s1 := s1+F[k,l];
      s2 := 0;
      for k := i-1 downto 1 do
        for l := j-1 downto 1 do s2 := s2+F[k,l];
      A[i,j] := s1+s2;
      P := P + F[i,j]*A[i,j];
      s1 := 0; s2 := 0;
      for k := i+1 to nr1 do
        for l := j-1 downto 1 do s1 := s1+F[k,l];
      for k := i-1 downto 1 do
        for l := j+1 to nc1 do s2 := s2+F[k,l];
      D[i,j] := s1+s2;
      Q := Q + F[i,j]*D[i,j];
      end; { For j }
    end; { for i }

  for j := 1 to v_TotCol.Len do wc := wc - v_TotCol[j]*v_TotCol[j];

  { v_AsE armazenara os desvios padroes assintoticos }
  v_AsE := VecConst(0,4); { Dimensao 4 com todos valores iniciais nulos }
  v_stat := TwsDFVec.Create(4);
  v_AsE.Name :=  'Erros_Padroes';
  v_stat.Name := 'Estatisticas';

  w := Sqrt(wr*wc);
  { Obtem os valores das estat�sticas }
  s4 := P-Q;
  v_stat[1] := ScalarDiv(s4, P+Q); { Gamma }
  v_stat[2] := ScalarDiv(s4, w);     { Tau-b de Kendall }
  if nr1 < nc1 then m := nr1 else m := nc1;
  v_stat[3] := s4/(wTot*wTot*(m-1)/m);  { Tau-c de Stuart }
  v_stat[4] := ScalarDiv(s4, wr);                     { D de Sommers }

  for i := 1 to nr1 do
    begin
    s3 := s4*(wTot-v_TotLin[i]);
    for j := 1 to nc1 do
      begin
      Fij := F[i,j];
      v_AsE[1] := v_AsE[1]+Fij*Power(Q*A[i,j]-P*D[i,j],2);
      s1 := v_TotLin[i]*wc+v_TotCol[j]*wr;
      s2 := A[i,j]-D[i,j];
      v_AsE[2] := v_AsE[2]+Fij*Power(2*w*s2+v_stat[2]*s1,2);
      v_AsE[3] := v_AsE[3]+Fij*s2*s2;
      v_AsE[4] := v_AsE[4]+Fij*Power(wr*s2 - s3, 2);
      end; { for j }
    end;

  v_AsE[1] := Sqrt(16*v_AsE[1]/Power(P+Q,4));{ Gamma }
  s1 :=(wr+wc);
  v_AsE[2] := Sqrt((v_AsE[2] - wTot*wTot*wTot*v_stat[2]*v_stat[2]*s1*s1)/Power(w,4));{ Tau-b de Kendall }
  v_AsE[3] := Sqrt(4*m*m*(v_AsE[3]-s4*s4/wTot)/Power((m-1)*wTot*wTot,2));{ Tau-c de Stuart }
  v_AsE[4] := Sqrt(4*v_AsE[4]/Power(wr,4)); { D de Sommers }


  Stat := TwsGeneral.Create(0,v_stat.Len);
  Stat.MAdd(v_stat);
  Stat.MAdd(v_AsE);

  Stat.Name := 'Est_Ass';
  Stat.MLab := 'Estat�sticas de Associa��o para ' + DtSet.Struct.Col[1].Name +
                ' x ' + DtSet.Struct.Col[2].Name;

  Stat.ColName[1] := 'Gama';
  Stat.ColName[2] := 'Tau_Kenndall';
  Stat.ColName[3] := 'Tau_Stuart';
  Stat.ColName[4] := 'D_Sommers';

end; { FreqAssociation }

end.
