unit wsTTest;

interface
uses Classes,
     wsConstTypes,
     wsBXML_Output,
     wsMatrix,
     wsVec,
     wsGLib;

type
  TwsOneSample = class
  private
    // armazenam apenas as referencias
    Options: TBits;
    LData: TwsDataSets;
    RVar: TwsLIVec;
    WInd: Integer;
    FOutput: TwsBXML_Output;
    FOnCreatedObject: TwsCreatedObject_Event;
    
    Procedure SetSaveTest(SaveTest: Boolean);
    Procedure SetListTest(ListTest: Boolean);
    Procedure SetSaveData(SaveData: Boolean);
    Procedure SetListData(ListData: Boolean);
    function GetSaveTest: Boolean;
    function GetListTest: Boolean;
    function GetSaveData: Boolean;
    function GetListData: Boolean;
  public
    constructor Create(DataList: TwsDataSets;
                       RespVar: TwsLIVec;
                       WIndex: Integer;
                       Output: TwsBXML_Output;
                       CreatedObjectEvent: TwsCreatedObject_Event);

    procedure TTest(var HValue: TwsVec; Alpha: Double; OneSided: Boolean); virtual;

    property SaveTest : Boolean Read GetSaveTest Write SetSaveTest;
    property ListTest : Boolean Read GetListTest Write SetListTest;
    property SaveData : Boolean Read GetSaveData Write SetSaveData;
    property ListData : Boolean Read GetListData Write SetListData;
  end;

  TwsTwoSample = class(TwsOneSample)
  private
    // armazena apenas referencia
    FVar: TwsLIVec;
  public
    constructor Create(DataList: TwsDataSets;
                       Fac: TwsLIVec;
                       RespVar: TwsLIVec;
                       WIndex: Integer;
                       Output: TwsBXML_Output;
                       CreatedObjectEvent: TwsCreatedObject_Event);

    procedure TTest(var HValue: TwsVec; Alpha: Double; OneSided: Boolean); override;
  end;

implementation
uses wsFuncoesDeProbabilidade;

constructor TwsOneSample.Create(DataList: TwsDataSets;
                                RespVar: TwsLIVec;
                                WIndex: Integer;
                                Output: TwsBXML_Output;
                                CreatedObjectEvent: TwsCreatedObject_Event);
{  Objetivo
     Cria objeto para tratamento de m�todos estat�sticos aplic�veis a uma amostra
   Par�metros
     DataList: Lista com os conjuntos de dados. Usualmente ser� a lista resultante da fun��o
               ModelFrame
     RespVar : �ndices das vari�veis respostas que ser�o tratadas
     WIndex  : �ndice da vari�vel peso. Um valor nulo ou negativo (default) indica a todos
               os m�todos que n�o existe defini��o de vari�vel peso.
}
begin
  inherited Create;
  LData := DataList;
  RVar := RespVar;
  WInd := WIndex;
  Options := TBits.Create;
  FOutput := Output;
  FOnCreatedObject := CreatedObjectEvent;
end;
{====================  M�todos privados =================}

Procedure TwsOneSample.SetSaveTest(SaveTest: Boolean);
{Atribui True ou False para a posi��o 0 de TBits, que controla o armazenamento
do resultado do teste}
Begin
  Options.Bits[0]:= SaveTest;
End;

function TwsOneSample.GetSaveTest: Boolean;
Begin
  Result:=Options.Bits[0];
End;

Procedure TwsOneSample.SetListTest(ListTest: Boolean);
{Atribui True ou False para a posi��o 1 de TBits, que controla a impress�o da
matriz de estat�sticas e do resultado do teste}
Begin
  Options.Bits[1]:= ListTest;
End;

function TwsOneSample.GetListTest: Boolean;
Begin
  Result:=Options.Bits[1];
End;

Procedure TwsOneSample.SetSaveData(SaveData: Boolean);
{Atribui True ou False para a posi��o 2 de TBits, que controla o armazenamento
do conjunto de dados que cont�m os dados para a an�lise (retorno de modelframe)}
Begin
  Options.Bits[2]:= SaveData;
End;

function TwsOneSample.GetSaveData: Boolean;
Begin
  Result:=Options.Bits[2];
End;

Procedure TwsOneSample.SetListData(ListData: Boolean);
{Atribui True ou False para a posi��o 3 de TBits, que controla a impress�o
do conjunto de dados que cont�m os dados para a an�lise (retorno de modelframe)}
Begin
  Options.Bits[3]:= ListData;
End;

function TwsOneSample.GetListData: Boolean;
Begin
  Result:=Options.Bits[3];
End;

{============================================================}

procedure TwsOneSample.TTest(var HValue: TwsVec; Alpha: double; OneSided: Boolean);
{  Objetivo
     Executa as compara��es entre a m�dia e um valor padr�o atrav�s do teste t
   Par�metros
     HValue: Vetor com os valores da hip�tese
     Alpha: Valor entre 0 e 1 que indica o nivel de signific�ncia desejado para o teste
     OneSided: True se o teste ser� unilateral; false para teste bilateral
}
var
  i,j,k: Integer;
  Stat : TwsGeneral;    // matriz com as estatisticas
  IStat: TwsLIVec;     // indices para as estat�sticas
  TData: TwsDataSet;   // conjunto de dados para sa�da dos resultados
  v    : TwsVec;
  Col  : TwsDataSetCol;
  aux  : double;
  Erro : Word;
begin
  // 0-Media 2-Desvio padrao 4-Minimo 5-Maximo 7-Valores validos 8-Erro padrao da media
  if WInd<=0 then
    IStat:=TwsLIVec.Create([0,2,4,5,7,8])
  else
    IStat:=TwsLIVec.Create([0,2,4,5,7,8,15]);   // 15-soma dos pesos

  if HValue = nil then
    HValue:=VecConst(0,RVar.Len);

  TData:=TwsDataSet.Create('Teste_t');  // Conjunto de dados para saida dos resultados
  with TData do
    begin
    MLab:='Teste t para m�dia de uma amostra';
    if not OneSided then
      MLab:=MLab+' - Bilateral'
    else
      MLab:=MLab+' - Unilateral';
    ColIdentName:='Variavel';
    Col:=TwsNumeric.Create('Media','M�dia amostral');                   { 1 }
    Struct.AddColEx(Col);
    Col:=TWSNumeric.Create('Val_Hip','Valor da hip�tese');              { 2 }
    Struct.AddColEx(Col);
    Col:=TWSNumeric.Create('Diferenca','Diferen�a Media-Val_Hip');      { 3 }
    Struct.AddColEx(Col);
    Col:=TWSNumeric.Create('T','Valor da estat�stica T para hip�tese H0'); { 4 }
    TwsNumeric(Col).Size:=10; TwsNumeric(Col).Precision:=5;
    Struct.AddColEx(Col);
    Col:=TWSNumeric.Create('p',
      'Probabilidade de ocorrer um valor maior que T observado');        { 5 }
    TwsNumeric(Col).Size:=10; TwsNumeric(Col).Precision:=5;
    Struct.AddColEx(Col);
    Col:=TWSNumeric.Create('Alfa','Taxa de erro Tipo I');               { 6 }
    TwsNumeric(Col).Size:=8; TwsNumeric(Col).Precision:=4;
    Struct.AddColEx(Col);
    Col:=TWSQualitative.Create('Decisao','Decis�o ao n�vel alfa');        { 7 }
    With TWSQualitative(Col) Do
      Begin
      Size := 6;
      AddLevel('Nao_Rejeita');
      AddLevel('Rejeita');
      End; { With Col }
    Struct.AddColEx(Col);
                                                                        { 8 }
    Col:=TWSNumeric.Create('Extr_Inf','EI do IC para m�dia ao n�vel 100(1-alfa)%');
    Struct.AddColEx(Col);
                                                                        { 9 }
    Col:=TWSNumeric.Create('Extr_Sup','ES do IC para m�dia ao n�vel 100(1-alfa)%');
    Struct.AddColEx(Col);
    end;

  k:=0;
  for i := 0 to LData.Count-1 do     // para cada conjunto de dados da lista
    begin
    if WInd<=0  then               // Obtem matriz das estatisticas.
      Stat := TwsDataSet(LData[i]).DescStat(RVar,IStat)
    else
      Stat := TwsDataSet(LData[i]).wDescStat(RVar,IStat,WInd);

    Stat.MLab := 'Estat�sticas Descritivas';

    if ListData then
       FOutPut.Add(LData[i]);

    if ListTest then
       FOutPut.Add(Stat);

    for j:=1 to RVar.Len do  // para cada variavel resposta
      begin
      Inc(k);
      v:=TwsDFVec.Create(9);
      v.Name:=Stat.RowName[k];
      v[1]:=Stat[k,1];                                  // Media
      v[2]:=HValue[j];                                  // Valor da hip�tese
      v[3]:=v[1]-HValue[j];                             // Diferenca
      v[4]:=v[3]/Stat[k,6];                             // Estatistica t
      v[5]:=TInt(v[4],Stat[k,5]-1,OneSided,True,Erro);  // Probabilidade
      if OneSided and (v[4]<0) then v[5]:=1-v[5];
      v[6]:=Alpha;
      if v[5]<alpha then
        v[7]:=1                                         // significativo
      else
        v[7]:=0;                                        // nao significativo
      if WInd<=0 then                                   // n-1
        aux:=TInv(Alpha,Stat[k,5]-1,1.0e-9,True,False,Erro)
      else                                              // soma dos pesos - 1
        aux:=TInv(Alpha,Stat[k,7]-1,1.0e-9,True,False,Erro);
      aux:=aux*Stat[k,6];
      v[8]:=v[1]-aux;                                   // Extremo inferior
      v[9]:=v[1]+aux;                                   // Extremo superior
      TData.MAdd(v)
      end;
    k:=0;
    Stat.Free;

    if SaveData and Assigned(FOnCreatedObject) then
       begin
       FOnCreatedObject(self, LData[i]);
       LData[i] := nil; // Isto evitar� a futura destrui��o
       end;
    end; // for i

  if ListTest then
     FOutPut.Add(TData);

  if SaveTest and Assigned(FOnCreatedObject) then
     FOnCreatedObject(self, TData)
  else
     TData.Free;

  IStat.Free
end; // TTest

constructor TwsTwoSample.Create(DataList: TwsDataSets;
                                Fac, RespVar: TwsLIVec;
                                WIndex: Integer;
                                Output: TwsBXML_Output;
                                CreatedObjectEvent: TwsCreatedObject_Event);
{ Objetivo
    Cria objeto para compara��o entre duas m�dias
  Par�metros
    DataList: Lista de conjuntos de dados
}
begin
  inherited Create(DataList, RespVar, WIndex, Output, CreatedObjectEvent);
  FVar := Fac
end;

procedure TwsTwoSample.TTest(var HValue: TwsVec; Alpha: double; OneSided: Boolean);
{  Objetivo
     Executa as compara��es entre duas m�dias de amostras independentes atrav�s do teste t.
     As amostras correspondem aos dois (e somente dois) n�veis do fator de �ndice FVar.
     Atrav�s de um teste F bilateral (5%) verifica se as vari�ncias s�o heterog�neas. Se
     forem, inclui a aproxima��o para vari�ncias heterog�neas, utilizando a aproxima��o de
     Satterthwaite para os graus de liberdade.
   Par�metros
     HValue  : Vetor com os valores da hip�tese. Se a entrada for nil o vetor � contru�do
               e preenchido com zeros em todas as posi��es.
     Alpha   : Valor entre 0 e 1 que indica o nivel de signific�ncia desejado para o teste
     OneSided: True se o teste ser� unilaterla; false para teste bilateral
}
var
  i,j,k,kf,
  kv,kp1,kv1,kv2: Integer;
  FMat,Stat     : TwsGeneral;    // matriz com as estatisticas
  TData         : TwsDataSet;    // conjunto de dados para sa�da dos resultados
  v             : TwsVec;
  Col           : TwsDataSetCol;
  aux,ndf,ddf,
  ndf1,ndf2,pf  : double;
  Erro          : Word;
begin
  TData:=TwsDataSet.Create('Teste_t');
  with TData do
    begin
    MLab:='Teste t para compara��o entre m�dias de duas amostras independentes';
    if not OneSided then
      MLab:=MLab+' - Bilateral'
    else
      MLab:=MLab+' - Unilateral';
    ColIdentName:='Fator';

    Col:=TwsNumeric.Create('Diferenca','Diferen�a entre as m�dias');           { 1 }
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('Val_Hip','Valor da hip�tese');                     { 2 }
    TwsNumeric(Col).Size:=9; TwsNumeric(Col).Precision:=5;
    Struct.AddColEx(Col);
                                                                               { 3 }
    Col:=TWSNumeric.Create('Dif_Hip','Diferen�a entre as m�dias e o valor da hip�tese');
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('Variancia','Vari�ncia da compara��o');                 { 4 }
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('G_Lib','Graus de liberdade da vari�ncia combinada');{ 5 }
    TwsNumeric(Col).Size:=9; TwsNumeric(Col).Precision:=6;
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('T','Valor da estat�stica T para hip�tese');         { 6 }
    TwsNumeric(Col).Size:=10; TwsNumeric(Col).Precision:=5;
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('p',
      'Probabilidade de ocorrer um valor maior que T observado');       { 7 }
    TwsNumeric(Col).Size:=10; TwsNumeric(Col).Precision:=4;
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('Alfa','Taxa de erro Tipo I');                       { 8 }
    TwsNumeric(Col).Size:=8; TwsNumeric(Col).Precision:=4;
    Struct.AddColEx(Col);

    Col:=TWSQualitative.Create('Decisao','Decis�o ao n�vel Alfa');           { 9 }
    With TWSQualitative(Col) Do
      Begin
      Size := 7;
      AddLevel('Nao_Rejeita');
      AddLevel('Rejeita');
      End; { With Col }
    Struct.AddColEx(Col);

    Col:=TWSNumeric.Create('Extr_Inf','EI do IC para diferen�a ao n�vel 100(1-alfa)%');{ 10 }
    Struct.AddColEx(Col);
    Col:=TWSNumeric.Create('Extr_Sup','ES do IC para diferen�a ao n�vel 100(1-alfa)%');{ 11 }
    Struct.AddColEx(Col);
    end;

  // Se nenhum valor foi atribuido para hipotese, atribui zero
  if HValue = nil then
     HValue := VecConst(0, RVar.Len);

  // Executa o teste: para cada conjunto, cada fator e cada vari�vel resposta
  for i:=0 to LData.Count-1 do     // para cada conjunto de dados da lista
    begin
    if (WInd < 0) then
       Stat := LData[i].FacVarMean(FVar, 1, LData[i].NRows, RVar)
    else
       Stat := LData[i].wFacVarMean(FVar, 1, LData[i].NRows, WInd, RVar);

    Stat.MLab := 'Estat�sticas Descritivas';

    if ListData then
       FOutPut.Add(LData[i]);

    if ListTest then
       FOutPut.Add(Stat);

    k:=0;
    for kf := 1 to FVar.Len do       // Para cada fator
      begin
      Col := TwsDataSet(LData[i]).Struct.Col[FVar[kf]];
      if TwsFactor(Col).Levels = 2 then  // Se fator nao tiver 2 niveis, nao faz
        begin
        k:=k+(kf-1)*TwsFactor(Col).Levels;          // k eh a linha da estatistica
        for j:=1 to RVar.Len do                     // para cada variavel resposta
          begin
          if WInd<0 then kv:=3*j-2 else kv:=4*j-3;  // Se existe peso
          kv1:=kv+1;
          if WInd<0 then kv2:=kv+2 else kv2:=kv+3;  // Se existe peso
          Inc(k);
          kp1:=k+1;
          v:=TwsDFVec.Create(11);
          v.Name:=Col.Name;
          v[1]:=Stat[k,kv]-Stat[kp1,kv];            // Diferenca entre as medias
          v[2]:=HValue[j];                          // Valor da hip�tese
          v[3]:=v[1]-HValue[j];                     // Diferenca
          ndf:=Stat[k,kv2]-1; ddf:=Stat[kp1,kv2]-1; // gl numerador e denominador
          aux:=(ndf*Stat[k,kv1]+ddf*Stat[kp1,kv1]);
          v[5]:=ndf+ddf;                            // Graus de liberdade combinad
          v[4]:=aux/v[5];                           // Variancia combinada
                                                    // Estatistica T
          v[6]:=v[3]/Sqrt(v[4]*(1/Stat[k,kv2]+1/Stat[kp1,kv2]));

          v[7]:=TInt(v[6],v[5],OneSided,True,Erro); // Probabilidade p
          if OneSided and (v[6]<0) then v[7]:=1-v[7];
          v[8]:=Alpha;
          if v[7]>=alpha then
            v[9]:=0                                 // nao signifiactivo
          else
            v[9]:=1;                                // significativo
          aux:=TInv(Alpha,v[5],1.0e-9,True,False,Erro);
          aux:=aux*Sqrt(v[4]*(1/Stat[k,kv2]+1/Stat[kp1,kv2]));
          v[10]:=v[1]-aux;                          // Extremo inferior
          v[11]:=v[1]+aux;                          // Extremo superior
          TData.MAdd(v);

          // executa o teste F. F eh a razao entre a maior e a menor vari�ncia
          aux:=wsGLib.MaxF(Stat[k,kv1],Stat[kp1,kv1])/wsGLib.MinF(Stat[k,kv1],Stat[kp1,kv1]);
          if Stat[k,kv1]>Stat[kp1,kv1] then // troca os graus de liberdade, se necessario
            begin
            ndf1:=ndf;
            ndf2:=ddf
            end
          else
            begin
            ndf1:=ddf;
            ndf2:=ndf
            end;
          pf:=2*FInt(aux,ndf1,ndf2,True,Erro);      // Duplica para teste bilateral
          if pf<0.05 then                           // Teste F bilateral a 5%
            begin
            FMat:= TwsGeneral.Create(1,6);
            FMat.Name:='Teste_F';
            FMat.PrintOptions.ColPrecision := 8;
            FMat.PrintOptions.ColWidth := 14;
            FMat.MLab :='Teste F bilateral (5%) para as vari�ncias';
            FMat.RowName[1]:='Valores';
            FMat.ColName[1]:='Var_1';FMat.ColName[2]:='Var_2';
            FMat.ColName[3]:='GL_1';FMat.ColName[4]:='GL_2';
            FMat.ColName[5]:='F';FMat.ColName[6]:='p';
            FMat[1,1]:=Stat[k,kv1];FMat[1,2]:=Stat[kp1,kv1];
            FMat[1,3]:=ndf;FMat[1,4]:=ddf;FMat[1,5]:=aux;FMat[1,6]:=pf;

            FOutPut.Add(FMat);
            FMat.Free;

            // Cria uma nova linha com teste para vari�ncias heterog�neas
            v:=TwsDFVec.Create(11);
            v.Name:=Col.Name+'_Het';
            v[1]:=Stat[k,kv]-Stat[kp1,kv];          // Diferenca entre as medias
            v[2]:=HValue[j];                        // Valor da hip�tese
            v[3]:=v[1]-HValue[j];                   // Diferenca
            // ndf (ddf) agora e a variancia 1 (2) dividida pelas respectivas repeticoes
            ndf:=Stat[k,kv1]/Stat[k,kv2]; ddf:=Stat[kp1,kv1]/Stat[kp1,kv2];
                                                    // Graus de liberdade Satterthwaithe
            v[5]:=Sqr(ndf+ddf)/(Sqr(ndf)/(Stat[k,kv2]-1)+Sqr(ddf)/(Stat[kp1,kv2]-1));
            v[4]:=ndf+ddf;                          // Variancia conjunta
            v[6]:=v[3]/Sqrt(v[4]);                  // Estatistica T
            v[7]:=TInt(v[6],v[5],OneSided,True,Erro);// Probabilidade p
            if OneSided and (v[6]<0) then v[7]:=1-v[7];
            v[8]:=Alpha;
            if v[7]>=alpha then
              v[9]:=0                                // nao signifiactivo
            else
              v[9]:=1;                               // significativo
            aux:=TInv(Alpha,v[5],1.0e-9,True,False,Erro);
            aux:=aux*Sqrt(v[4]);
            v[10]:=v[1]-aux;                         // Extremo inferior
            v[11]:=v[1]+aux;                         // Extremo superior
            TData.MAdd(v)
            end
          end; // for j
        end //if TwsFactor(Col).Levels = 2
      end;   // for kf
    Stat.Free;

    if SaveData and Assigned(FOnCreatedObject) then
       begin
       FOnCreatedObject(self, LData[i]);
       LData[i] := nil; // Isto evitar� a futura destrui��o
       end;
    end; // for i

  if ListTest then
     FOutPut.Add(TData);

  if SaveTest and Assigned(FOnCreatedObject) then
     FOnCreatedObject(self, TData)
  else
     TData.Free;
end; // TTest

end.
