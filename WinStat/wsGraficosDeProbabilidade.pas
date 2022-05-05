unit wsGraficosDeProbabilidade;

interface
uses SysUtils,
     Math,
     wsConstTypes,
     drGraficosBase,
     drGraficos,
     wsVec,
     wsMatrix;
(*
  // Gr�fico de quantis
  function GraphQuantil(DS: TwsDataSet; const YStr: String): TgrGrafico;
  // Gr�fico de simetria
  function SymmetryPlot(DS: TwsDataSet; const YStr: String): TgrGrafico;
  // Gr�fico quantil-quantil
  function QQGraph(DS: TwsDataSet; const XStr, YStr: String): TgrGrafico;

  // Gr�fico de probabilidade
  function ProbabilityPlot(vData: TwsVec; Dist: TDistType; out V: TwsSymmetric; Null,Env: Boolean;
    ParType: byte; var Par: TwsVec; const c: Double; out S: TwsGeneral): TgrGrafico;
*)
implementation
uses wsGraficos,
     wsProbabilidade,
     wsDistribuicoesRandomicas,
     wsGLib;

{ =========================== Gr�ficos =========================== }
(*
function GraphQuantil(DS: TwsDataSet; const YStr: String): TgrGrafico;
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
       Result.Grafico.LeftAxis.Title.Caption:= YStr;
       Result.Grafico.BottomAxis.Title.Caption:= 'Quantis';
       Result.Grafico.Title.Text.Add('Gr�fico de Quantis');
       end
    else
       raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo ' + intToStr(Graf_Erro));
  finally
    pData.Free; vData.Free;
  end;
end; { GraphQuantil }

function SymmetryPlot(DS: TwsDataSet; const YStr: String): TgrGrafico;
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
         Result.Grafico.LeftAxis.Title.Caption:= 'Mediana - Y';
         Result.Grafico.BottomAxis.Title.Caption:= 'Y - Mediana';
         Result.Grafico.Title.Text.Add('Gr�fico de Simetria para '+YStr)
         end
      else
         raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo '+intToStr(Graf_Erro));
    finally
      vData.Free;
      xData.Free;
      yData.Free;
    end;
end; { SymmetryPlot }

function QQGraph(DS: TwsDataSet; const XStr, YStr: String): TgrGrafico;
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
     Result.Grafico.Title.Text.Add('Gr�fico de Quantis Emp�ricos');
     Result.Grafico.LeftAxis.Title.Caption:= XStr;
     Result.Grafico.BottomAxis.Title.Caption:= YStr;
     end;
  vXData.Free;
  vYData.Free
end; { QQGraph }


{ ================== Nova vers�o graficos de probabilidade ================= }

function GetPlotDist(x: TwsVec; D: TDistType; PT: byte; var V: TwsSymmetric; var P: TwsVec): TwsProbCont;
{ Objetivo
    Instancia classe para constru��o de gr�fico de probabilidade de acordo com o tipo desejado
  Par�metros
    x: Vetor com os valores da vari�vel ou dados observados
    D: Distribui��o desejada
    PT: Modo de obten��o dos valores dos par�metros
      1: N�o especificar valor. Distribui��o pode ser especificada com um valor qualquer.
      2: Estima��o dos par�metros pelo m�todo dos momentos
      3: Estima��o dos par�metros por m�xima verossimilhan�a
      4: Os valores dos par�metros foram especificados
}
begin
  case D of
    dbExpon:      // Exponencial com 1 parametro
      begin
      Result:=TwsProbExponential.Create;
      case PT of
        1:   TwsProbExponential(Result).Lambda:=1;      // um valor qualquer
        2,3: P:=Result.MLE(x,V);                          // MV ou momentos
        4  : TwsProbExponential(Result).Lambda:=P[1];   // Especifica valor
        end; // case PT
      end; // Exponencial
    dbExpon2:
      begin      // Exponencial com dois parametros
      Result:=TwsProbExponential2.Create;
      case PT of
        1:   begin                                     // Inicializa com valores quaisquer
             TwsProbExponential2(Result).Theta:=0;
             TwsProbExponential2(Result).Lambda:=1;
             end;
        2,3: P:=Result.MLE(x,V);                         // MV ou momentos
        4  : begin
             TwsProbExponential2(Result).Theta:=P[1];  // Especifica valor
             TwsProbExponential(Result).Lambda:=P[2];  // Especifica valor
             end;
        end; // case PT
      end;
    dbStdNorm: Result:=TwsProbStdNormal.Create;
    dbNormal:
      begin      // Normal
      Result:=TwsProbNormal.Create;
      case PT of
        1:   begin
             TwsProbNormal(Result).Mean:=0;   // Especifica valor
             TwsProbNormal(Result).Sigma:=1;  // Especifica valor
             end;
        2,3: P:=Result.MLE(x,V);   // MV ou momentos
        4  : begin
             TwsProbNormal(Result).Mean:=P[1];   // Especifica valor
             TwsProbNormal(Result).Sigma:=P[2];  // Especifica valor
             end;
        end; // case PT
      end;
    dbHalfNorm:
      begin      // Semi-normal
      Result:=TwsProbHalfNormal.Create;
      case PT of              // Inicializa com um valor qualquer
        1: TwsProbHalfNormal(Result).Lambda:=1;
        2,3: P:=Result.MLE(x,V);   // MV ou momentos
        4  : TwsProbHalfNormal(Result).Lambda:=P[1];  // Especifica valor
        end; // case PT
      end;
    dbLognormal:
      begin      // Lognormal
      Result:=TwsProbLognormal.Create;
      case PT of
        1:   begin
             TwsProbLognormal(Result).Mean:=0;   // Especifica valor
             TwsProbLognormal(Result).Sigma:=1;  // Especifica valor
             end;
        2: P:=Result.MomentEst(x);   // Momentos
        3: P:=Result.MLE(x,V);         // MV
        4  : begin
             TwsProbLognormal(Result).Mean:=P[1];   // Especifica valor
             TwsProbLognormal(Result).Sigma:=P[2];  // Especifica valor
             end;
        end; // case PT
      end;
    dbLognormal3:
      begin      // Lognormal
      Result:=TwsProbLognormal3.Create;
      case PT of
        1:   begin
             TwsProbLognormal3(Result).Theta:=0;   // Especifica valor
             TwsProbLognormal3(Result).Mean:=1;    // Especifica valor
             TwsProbLognormal3(Result).Sigma:=1;   // Especifica valor
             end;
        2: P:=Result.MomentEst(x);   // Momentos
        3: P:=Result.MLE(x,V);         // MV
        4  : begin
             TwsProbLognormal3(Result).Theta:=P[1];   // Especifica valor
             TwsProbLognormal3(Result).Mean:=P[2];   // Especifica valor
             TwsProbLognormal3(Result).Sigma:=P[3];  // Especifica valor
             end;
        end; // case PT
      end;
    dbGamma:
      begin      // Gama com 1 parametro
      Result:=TwsProbGamma.Create;
      case PT of
        2  : P:=Result.MomentEst(x);              // O parametro de formato eh obrigatorio
        1,3: P:=Result.MLE(x,V);                    // MV
        4  : TwsProbGamma2(Result).Alpha:=P[1];   // Especifica valor
        end; // case PT
      end; // Gamma
    dbGamma2:
      begin      // Gama com 2 parametros
      Result:=TwsProbGamma2.Create;
      case PT of
        2  : P:=Result.MomentEst(x);              // O parametro de formato eh obrigatorio
        1,3: P:=Result.MLE(x,V);                    // MV
        4  : begin
             TwsProbGamma2(Result).Lambda:=P[1];  // Especifica valor dispersao
             TwsProbGamma2(Result).Alpha:=P[2];   // Especifica valor
             end
        end; // case PT
      end; // Gamma
    dbGamma3:
      begin      // Gama com 3 parametros
      Result:=TwsProbGamma3.Create;
      case PT of
        2  : P:=Result.MomentEst(x);              // O parametro de formato eh obrigatorio
        1,3: P:=Result.MLE(x,V);                  // MV
        4  : begin
             TwsProbGamma3(Result).Theta:=P[1];   // Especifica valor dispersao
             TwsProbGamma3(Result).Lambda:=P[2];  // Especifica valor dispersao
             TwsProbGamma3(Result).Alpha:=P[3];   // Especifica valor
             end
        end; // case PT
      end; // Gamma3
    dbLPearson3:
      begin      // Log-Pearson tipo III
      Result:=TwsProbLogPearson3.Create;
      case PT of
        2  : P:=Result.MomentEst(x);              // O parametro de formato eh obrigatorio
        1,3: P:=Result.MLE(x,V);                  // MV
        4  : begin
             TwsProbLogPearson3(Result).Theta:=P[1];   // Especifica valor dispersao
             TwsProbLogPearson3(Result).Lambda:=P[2];  // Especifica valor dispersao
             TwsProbLogPearson3(Result).Alpha:=P[3];   // Especifica valor
             end
        end; // case PT
      end; // LPearson3
    dbUnif:
      begin      // uniforme
      Result:=TwsProbUniform.Create;
      case PT of
        2  : P:=Result.MomentEst(x);              // O parametro de formato eh obrigatorio
        1,3: P:=Result.MLE(x,V);                    // MV
        4  : begin
             TwsProbUniform(Result).Alpha:=P[1];  // Especifica extremo inferior
             TwsProbUniform(Result).Beta:=P[2];   // Especifica extremo superior
             end
        end; // case PT
      end; // Uniforme
  end; // case D
end;

procedure GetLimits(var xInf,xSup: TwsVec; Dist: TwsProb; n: integer);
{ Objetivo
    Obt�m, via simula��o, valores para os limites dos envelopes inferior e superior utilizados no
    gr�fico de probabilidades
  Par�metros
    xInf: Vetor com os limites inferiores
    xSup: Vetor com os limites superiores
    Dist: Distribui��o para a gera��o dos dados
    n   : N�mero de observa��es
}
var
  S    : TwsGeneral;
  i,k  : Integer;
  xi,xm: double;
begin
  S:=TwsGeneral.Create(0,n);
  // Gera a tabela 19 x n (S)
  for i:=1 to 19 do
    begin
    // Gera a amostra, ordena e insere na tabela
    xSup:=TwsDFVec.Create(n);
    for k:=1 to n do
      xSup[k]:=Dist.RandValue;
    xSup.QuickSort(True);
    S.MAdd(xSup)
    end;
  // xSup contera os maximos de cada coluna
  xSup:=TwsDFVec.Create(n);
  xSup.Name:='LS_Env';
  // xInf contera os minimos de cada coluna
  xInf:=TwsDFVec.Create(n);
  xInf.Name:='LI_Env';
  for i:=1 to S.NCols do
    begin
    S.ColExtrems(i,xi,xm);
    xInf[i]:=xi;
    xSup[i]:=xm
    end;
  S.Free;
end; // GetLimits

procedure GetYCoord(D: TDistType; var yMin,yMax: Double; P: TwsVec);
{ Objetivo
    Obtem as ordenadas para a reta que indica o formato nulo do gr�fico de probabilidades
  Par�metros
    D: Distribui��o desejada
    yMin: Na entrada traz o menor valor da abscissa; na sa�da leva o ponto que define o
          extremo inferior da reta
    yMax: Na entrada traz o maior valor da abscissa; na sa�da leva o ponto que define o
          extremo superior da reta
    P: Valores para os par�metros. Quando necess�ria, a ordem considerada sempre ser�:
       Deslocamento: primeira posi��o
       Dispers�o   : segunda posi��o
       Formato     : terceira posi��o
}
begin
  case D of
    dbExpon:      // Exponencial com 1 parametro
      begin
      yMin:=yMin/p[1];
      yMax:=yMax/p[1]
      end;
    dbExpon2:    // Exponencial com 2 parametros
      begin
      yMin:=p[1]+yMin/p[2];
      yMax:=p[1]+yMax/p[2]
      end;
    dbStdNorm:;
    dbNormal:;
    dbLognormal:;
    dbLognormal3:;
    dbLPearson3:;
(*
      begin
      yMin:=p[1]+p[2]*yMin;
      yMax:=p[1]+p[2]*yMax
      end;

    dbHalfNorm:
      begin
      yMin:= {p[1]+}yMin/p[1];
      yMax:= {p[1]+}yMax/p[1]
      end;
    dbGamma,dbGamma2,dbGamma3,dbUnif:;

      begin
      yMin:= {p[1]+}Sqrt(p[1])*yMin;
      yMax:= {p[1]+}Sqrt(p[1])*yMax
      end;
    dbGamma2:
      begin
      yMin:= {p[1]*p[2]+}Sqrt(p[1]*Sqr(p[2]))*yMin;
      yMax:= {p[1]*p[2]+}Sqrt(p[1]*Sqr(p[2]))*yMax
      end;

    dbUnif:
      begin
      yMin:= (p[1]+p[2])/2+Sqrt(Sqr(p[2]-p[1])/12)*yMin;
      yMax:= (p[1]+p[2])/2+Sqrt(Sqr(p[2]-p[1])/12)*yMax
      end;

  end; // case D

end; // GetYCoord

function ProbabilityPlot(vData: TwsVec; Dist: TDistType; out V: TwsSymmetric; Null,Env: Boolean;
  ParType: byte; var Par: TwsVec; const c: Double; out S: TwsGeneral): TgrGrafico;
{ Objetivo
    Construir gr�fico dos quantis observados (vari�vel com valores ordenados) contra os quantis
    te�ricos de uma distribui��o de probabilidades. Os par�metros de origem e dispers�o n�o necessitam
    ser informados. Par�metros de formatos necessitar�o de valores para a obten��o dos quantis te�ricos.
    Assim, se n�o forem estimados, necessitar�o ser especificados. A constru��o de um formato nulo e de
    um envelope por simula��o tamb�m poder� ser feita.
  Par�metros
    vData  : Valores das observa��es da vari�vel
    Null   : True se formato nulo vai ser construido
    Env    : True para constru��o do envelope para o gr�fico.
    ParType: Modo de obten��o dos valores dos par�metros
             1: Par�metros n�o ser�o informados
             2: Par�metros ser�o estimados pelo m�todo dos momentos
             3: Par�metros ser�o estimados por m�xima verossimilhan�a
             4: Valores dos par�metros ser�o fornecidos
    Par    : Valores dos par�metros, se ParType for 4
    c      : Valor da constante para obten��o dos quantis te�ricos
    S      : Matriz geral que retorna nas linhas as quantidades utilizadas na constru��o do gr�fico
}
var
  xData,
  xSup,xInf: TwsVec;
  xi,xm,xl,
  yi,ym    : Double;
  i,k,n,Err: Integer;
  PDist    : TwsProbCont;  // calcular quantis
//  GDist    : TwsRanDist;      // gerar valores para envelope
begin
  // Obtem a distribuicao necessaria
  PDist:=GetPlotDist(vData,Dist,ParType,V,Par);
  // Cria xData. vData volta ordenado. Na realidade, QQData nao utiliza o valor do parametro
  xData:=PDist.QQData(vData,c);

  // Constroi envelope de simulacao?
  if Env then
    GetLimits(xInf,xSup,PDist,vData.Len); // Cria limites para o envelope
  PDist.Free;

  S:=TwsGeneral.Create(0,vData.Len);
  S.MAdd(vData);

  if Env then
    begin
    S.MAdd(xInf);
    S.MAdd(xSup)
    end;
  xData.Name := 'Quantis';

  // Se o formato nulo vai ser incluido
  if Null then
    begin
    xData.MinMax(xi,xm);
    // xi e xm sao as abscissas
    yi:=xi;
    ym:=xm;
    // yi, ym retornam as ordenadas
    GetYCoord(Dist,yi,ym,Par);
    Result := ws_MatrixPlotTendency(xData,S,stPoint,xi,yi,xm,ym,Err)
    end
  else
    Result := ws_MatrixPlot(xData,S,stPoint,Err);

  S.MAdd(xData);
  if Err = 0 then
    Result.Grafico.LeftAxis.Title.Caption:= vData.Name
  else
    raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo ' + intToStr(Err));
end; { ProbabilityPlot }
*)
  // gr�fico de probabilidade exponencial - 2 par�metros
function ExponencialPlot2(vData: TwsVec; Null,Env: Boolean; ThType,LType: byte;Theta,Lam: Double): TgrGrafico;
{ Objetivo
    Construir gr�fico dos quantis observados (vari�vel com valores ordenados) contra os quantis te�ricos
    da distribui��o exponencial com dois par�metros. Os par�metros de origem e dispers�o n�o necessitam ser
    informados. Se for desejada a constru��o do envelope ou do formato nulo, os par�metros ser�o estimados se
    n�o houver informa��o sobre como obt�-los (veja descri��o dos par�metros ThType e LType)
  Par�metros
    DS  : Conjunto de dados
    YStr: Vari�vel (coluna) para a qual se deseja fazer o gr�fico
    Null: True se formato nulo vai ser construido
    Env : True para constru��o do envelope para o gr�fico.
    ThType (LType): Modo de tratamento do par�metro de origem (dispers�o)
      1: Par�metro n�o ser� informado
      2: Par�metro ser� estimado pelo m�todo dos momentos
      3: Par�metro ser� estimado por m�xima verossimilhan�a
      4: Valor do par�metro ser� fornecido
    ThType, Lam: Valor do par�metro de origem (dispers�o), se ThType, LType for 4
}
var
  xData,xSup,
  xInf        : TwsVec;
  xi,xm,xl,xt,
  yi,ym       : Double;
  S           : TwsGeneral;
  i,k,n,Err   : Integer;
  Expo        : TwsProbExponential; // calcular quantis
  GExpo       : TwsExponential;     // gerar valores para envelope
begin
  xData:=Expo.QQData(vData{,FcValue});

  Expo.Free;
  // Constroi envelope de simulacao?
  if Env then
    begin
    S:=TwsGeneral.Create(0,vData.Len);
    GExpo:=TwsExponential2.Create(xt,xl);
    // Gera a tabela 19 x n (S)
    for i:=1 to 19 do
      begin
      // Gera a amostra, ordena e insere na tabela
      xSup:=TwsDFVec.Create(vdata.Len);
      for k:=1 to vData.Len do
        xSup[k]:=GExpo.Generate;
      xSup.QuickSort(True);
      S.MAdd(xSup)
      end;
    // xSup contera os maximos de cada coluna
    xSup:=TwsDFVec.Create(vData.Len);
    xSup.Name:='LS_Env';
    // xInf contera os minimos de cada coluna
    xInf:=TwsDFVec.Create(vData.Len);
    xInf.Name:='LI_Env';
    for i:=1 to S.NCols do
      begin
      S.ColExtrems(i,xi,xm);
      xInf[i]:=xi;
      xSup[i]:=xm
      end;
    end;
  GExpo.Free;
  S.Free;

  S:=TwsGeneral.Create(0,vData.Len);
  S.MAdd(vData);
  if Env then
    begin
    S.MAdd(xInf);
    S.MAdd(xSup)
    end;
  try
    xData.Name:= vData.Name;
    if Null then
      begin
      xData.MinMax(xi,xm);
      yi:=xt+xi/xl;
      ym:=xt+xm/xl;
      Result := ws_MatrixPlotTendency(xData,S,stPoint,xi,yi,xm,ym,Err)
      end
    else
      Result := ws_MatrixPlot(xData,S,stPoint,Err);
    if Err = 0 then
      begin
      Result.Grafico.LeftAxis.Title.Caption:= vdata.Name;
      Result.Grafico.BottomAxis.Title.Caption:= 'Quantis Exponenciais';
      Result.Grafico.Title.Text.Add('Gr�fico Quantil-Quantil Exponencial');
      end
    else
      raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo ' + intToStr(Err));
  finally
    xData.Free;
    S.Free
  end;
end; { ExponentialPlot2 }


function WeibullPlot(DS: TwsDataSet; const YStr: String): TgrGrafico;
{ Objetivo
    Construir gr�fico dos quantis observados (vari�vel com valores ordenados) contra os
    quantis te�ricos da distribui��o de Weibull relativos �s propor��es de dados que esses
    valores representam.
  Par�metros
    DS         : Conjunto de dados
    YStr: Vari�vel (coluna) para a qual se deseja fazer o gr�fico
}
var
  xData,vData: TwsVec;
  k,n        : Integer;
  Graf_Erro  : Integer;
begin
  vData := DS.CopyCol(DS.Struct.IndexOf(YStr));
  n := vData.Len;
  vData.QuickSort(True);            // Esta ordem deve ser ascendente
  xData := TwsDFVec.Create(n);
  try
    for k := 1 to n do
      begin
      xData[k] := Log10(-Log10(1-(k-0.5)/n)); // Encontra os quantis teoricos
      vData[k] := Log10(vData[k]);
      end;
    xData.Name:= YStr;
    Result := ws_VectorsPlot([xData], [vData], [stPoint], Graf_Erro);
    if Graf_Erro = 0 then
       begin
       Result.Grafico.LeftAxis.Title.Caption:=YStr;
       Result.Grafico.BottomAxis.Title.Caption:='Quantis Weibull';
       Result.Grafico.Title.Text.Add('Gr�fico Quantil-Quantil Weibull');
       end
    else
       raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo ' + intToStr(Graf_Erro));
  finally
    vdata.Free;
    xData.Free;
  end;  
end; { WeibullPlot }

end.

(*
  // Gr�fico de probabilidade normal
  function NormalPlot(DS: TwsDataSet; const YStr: String): TgrGrafico; overload;
  // gr�fico de probabilidade normal
  function NormalPlot(DS: TwsDataSet; const YStr: String; const Theta: Double): TgrGrafico; overload;
  // gr�fico de probabilidade meio-normal
//  function HalfNormalPlot(DS: TwsDataSet; const YStr: String): TgrGrafico;
  // gr�fico de probabilidade uniforme
  function UniformPlot(DS: TwsDataSet; const YStr: String): TgrGrafico;
  // gr�fico de probabilidade qui-quadrado
  function ChiSquarePlot(DS: TwsDataSet; const YStr: String; const DF: Double; Trf: Boolean): TgrGrafico;
  // grp�fico de probabilidade gama
  function GammaPlot(DS: TwsDataSet; const YStr: String; const Alpha: Double; Trf: Boolean): TgrGrafico;
  // gr�fico de probabilidade exponencial - 2 par�metros
  function ExponentialPlot2(vData: TwsVec; Null,Env: Boolean; ThType,LType: byte;Theta,Lam: Double): TgrGrafico;
  // gr�fico de probabilidade Weibull
  function WeibullPlot(DS: TwsDataSet; const YStr: String): TgrGrafico;
*)

(*
function NormalPlot(DS: TwsDataSet; const YStr: String): TgrGrafico;
{ Objetivo
    Construir gr�fico dos quantis observados (vari�vel com valores ordenados) contra os
    quantis te�ricos da distribui��o normal relativos �s propor��es de dados que esses
    valores representam.
  Par�metros
    DS  : Conjunto de dados
    YStr: Nome da coluna (vari�vel)
}
var
  xData,vData: TwsVec;
  i,k,n      : Integer;
  NP         : TwsProbStdNormal;
  Graf_Erro  : Integer;
begin
  NP := TwsProbStdNormal.Create;
  vData := DS.CopyCol(DS.Struct.IndexOf(YStr));
  n := vData.Len;
  vData.QuickSort(True);            // Esta ordem deve ser ascendente
  xData := TwsDFVec.Create(n);
  try
    for k := 1 to n do
      xData[k] := NP.Inv((k-0.5)/n);  // Obtem os valores da normal padrao
    xData.Name:= YStr;
    Result := ws_VectorsPlot([xData],[vData],[stPoint],Graf_Erro);
    if Graf_Erro = 0 then
       begin
       Result.Top := 10;
       Result.Left := 10;
       Result.Grafico.LeftAxis.Title.Caption:= YStr;
       Result.Grafico.BottomAxis.Title.Caption:= 'Quantis Normais';
       Result.Grafico.Title.Text.Add('Gr�fico Quantil-Quantil Normal');
       end
    else
       raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo ' + intToStr(Graf_Erro));
  finally
    vdata.Free;
    xData.Free;
    NP.Free;
  end;
end; { NormalPlot }

function HalfNormalPlot(DS: TwsDataSet; const YStr: String): TgrGrafico;
{ Objetivo
    Construir gr�fico dos quantis observados (vari�vel com valores ordenados) contra os
    quantis te�ricos da distribui��o meio-normal relativos �s propor��es de dados que esses
    valores representam.
  Par�metros
    DS  : Conjunto de dados
    YStr: Vari�vel (coluna) para as quais se deseja fazer o gr�fico.
}
var
  xData,vData: TwsVec;
  k,n        : Integer;
  NP         : TwsProbStdNormal;
  Graf_Erro  : Integer;
begin
  NP := TwsProbStdNormal.Create;
  vData := DS.CopyCol(DS.Struct.IndexOf(YStr));
  n := vData.Len;
  vData.QuickSort(True);                // Esta ordem deve ser ascendente
  xData := TwsDFVec.Create(n);
  try
    for k := 1 to n do
      xData[k] := NP.HalfInv((k-0.5)/n);  // Obtem os valores da meio normal
    xData.Name:= YStr;
    Result := ws_VectorsPlot([xData],[vData],[stPoint],Graf_Erro);
    if Graf_Erro = 0 then
       begin
       Result.Top := 10;
       Result.Left := 10;
       Result.Grafico.LeftAxis.Title.Caption:= YStr;
       Result.Grafico.BottomAxis.Title.Caption:= 'Quantis Normais';
       Result.Grafico.Title.Text.Add('Gr�fico Quantil-Quantil Meio Normal');
       end
    else
       raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo ' + intToStr(Graf_Erro));
  finally
    vdata.Free;
    xdata.Free;
    NP.Free;
  end;
end; { HalfNormalPlot }

function UniformPlot(DS: TwsDataSet; const YStr: String): TgrGrafico;
{ Objetivo
    Construir gr�fico dos quantis observados (vari�vel com valores ordenados) contra os
    quantis te�ricos da distribui��o uniforme relativos �s propor��es de dados que esses
    valores representam.
  Par�metros
    DS  : Conjunto de dados
    YStr: Vari�vel (coluna) para as quais se deseja fazer o gr�fico.
}
var
  xData,vData: TwsVec;
  k,n        : Integer;
  Graf_Erro  : Integer;
begin
  vData := DS.CopyCol(DS.Struct.IndexOf(YStr));
  n := vData.Len;
  vData.QuickSort(True);                // Esta ordem deve ser ascendente
  xData := TwsDFVec.Create(n);
  try
    for k := 1 to n do
      xData[k] := (k-0.5)/n;  // Obtem os valores dos quantis emp�ricos
    xData.Name:= YStr;
    Result := ws_VectorsPlot([xData],[vData],[stPoint],Graf_Erro);
    if Graf_Erro = 0 then
       begin
       Result.Top := 10;
       Result.Left := 10;
       Result.Grafico.LeftAxis.Title.Caption:= YStr;
       Result.Grafico.BottomAxis.Title.Caption:= 'Quantis Uniformes';
       Result.Grafico.Title.Text.Add('Gr�fico Quantil-Quantil Uniforme');
       end
    else
       raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo ' + intToStr(Graf_Erro));
  finally
    vdata.Free;
    xdata.Free;
  end;
end; { UniformPlot }

function NormalPlot(DS:TwsDataSet; const YStr:String; const Theta:Double): TgrGrafico;
{ Objetivo
    Construir gr�fico dos quantis observados (vari�vel com valores ordenados) contra os
    quantis te�ricos da distribui��o normal relativos �s propor��es de dados que esses
    valores representam.
  Par�metros
    DS         : Conjunto de dados
    Col        : Colunas para as quais se deseja fazer o gr�fico. A sa�da ter� uma janela
                 gr�fica para cada vari�vel
    Theta      : Pot�ncia a elevar os dados. Se Teta>0, valores s�o elevados a pot�ncia;
                 se Teta < 0 o sinal � trocado para manter a mesma ordem; se Teta=0 a
                 transforma��o utilizada � a logaritmica
}
var
  xData,vData: TwsVec;
  k,n        : Integer;
  NP         : TwsProbStdNormal;
  Graf_Erro  : Integer;
begin
  NP := TwsProbStdNormal.Create;
  vData := DS.CopyCol(DS.Struct.IndexOf(YStr));
  n := vData.Len;
  if Theta > 0 then
    for k := 1 to n do
      vData[k] := Power(vData[k], Theta)  // Potencia usual
  else
    if Theta < 0 then
      for k := 1 to n do// Expoente negativo, troca o sinal para manter a ordem
        vData[k] := -Power(vData[k], Theta)
    else
      for k := 1 to n do
        vData[k] := Log10(vData[k]);         // Logaritmica para Teta nulo
  vData.QuickSort(True);            // Esta ordem deve ser ascendente
  xData := TwsDFVec.Create(n);
  try
    for k := 1 to n do
      xData[k] := NP.Inv((k-0.5)/n);  // Obtem os valores da normal padrao
    xData.Name:= YStr;
    Result := ws_VectorsPlot([xData],[vData],[stPoint],Graf_Erro);
    if Graf_Erro = 0 then
       begin
       Result.Top := 10;
       Result.Left := 10;
       Result.Grafico.LeftAxis.Title.Caption:= YStr;
       Result.Grafico.BottomAxis.Title.Caption:= 'Quantis Normais';
       Result.Grafico.Title.Text.Add('Gr�fico Quantil-Quantil Normal (Pot�ncia)');
       end
    else
       raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo ' + intToStr(Graf_Erro));
  finally
    vdata.Free;
    xData.Free;
    NP.Free;
  end;
end; { NormalPowerPlot }

function ChiSquarePlot(DS: TwsDataSet; const YStr: String; const DF: Double;
  Trf: Boolean): TgrGrafico;
{ Objetivo
    Construir gr�fico dos quantis observados (vari�vel com valores ordenados) contra os
    quantis te�ricos da distribui��o qui-quadrado relativos �s propor��es de dados que
    esses valores representam.
  Par�metros
    DS  : Conjunto de dados
    YStr: Nome da vari�vel (coluna) para a qual se deseja fazer o gr�fico
    DF  : Par�metro da distribuicao qui-quadrado
    Trf : True para efetuar a transforma��o raiz c�bica; False para manter valores
          originais
}
var
  xData,vData: TwsVec;
  i,k,n      : Integer;
  QP         : TwsProbChiSqr;
  Graf_Erro  : Integer;
begin
  QP := TwsProbChiSqr.Create(DF);
  vData := DS.CopyCol(DS.Struct.IndexOf(YStr));
  n := vData.Len;
  vData.QuickSort(True);            // Esta ordem deve ser ascendente
  xData := TwsDFVec.Create(n);
  try
    for k := 1 to n do
      xData[k] := QP.Inv((k-0.5)/n);  // Obtem os valores da qui-quadrado
    if Trf then                       // Se faz a transformacao
      for k := 1 to n do
        vData[k] := Power(vData[k],0.333333); // Extrai a raiz cubica
    xData.Name:= YStr;
    Result := ws_VectorsPlot([xData],[vData],[stPoint],Graf_Erro);
    if Graf_Erro = 0 then
       begin
       Result.Top := 10;
       Result.Left := 10;
       Result.Grafico.LeftAxis.Title.Caption:= YStr;
       Result.Grafico.BottomAxis.Title.Caption:= 'Quantis Qui-Quadrado';
       Result.Grafico.Title.Text.Add('Gr�fico Quantil-Quantil Qui-Quadrado');
       end
    else
       raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo ' + intToStr(Graf_Erro));
  finally
    vdata.Free;
    xData.Free;
    QP.Free;
  end;
end; { ChiSquarePlot }

function GammaPlot(DS: TwsDataSet; const YStr: String; const Alpha: Double;
  Trf: Boolean): TgrGrafico;
{ Objetivo
    Construir gr�fico dos quantis observados (vari�vel com valores ordenados) contra os
    quantis te�ricos da distribui��o Gama relativos �s propor��es de dados que esses
    valores representam.
  Par�metros
    DS   : Conjunto de dados
    YStr : Vari�vel (coluna) para a qual se deseja fazer o gr�fico
    Alpha: Par�metro da distribui��o gama
    Trf  : True para efetuar a transforma��o raiz c�bica, False para manter os valores
           originais
}
var
  xData,vData: TwsVec;
  k,n        : Integer;
  GP         : TwsProbGamma;
  Graf_Erro  : Integer;
begin
  GP := TwsProbGamma.Create(Alpha);
  vData := DS.CopyCol(DS.Struct.IndexOf(YStr));
  n := vData.Len;
  vData.QuickSort(True);            // Esta ordem deve ser ascendente
  xData := TwsDFVec.Create(n);
  try
    if Trf then
      for k := 1 to n do
        begin
        vData[k] := Power(vData[k],0.333333);
        xData[k]:= GP.CubInv((k-0.5)/n)
        end
    else
      for k := 1 to n do
        xData[k] := GP.Inv((k-0.5)/n);  // Obtem os valores da gama

    xData.Name:= YStr;
    Result := ws_VectorsPlot([xData], [vData], [stPoint], Graf_Erro);
    if Graf_Erro = 0 then
       begin
       Result.Top := 10;
       Result.Left := 10;
       Result.Grafico.LeftAxis.Title.Caption:= YStr;
       Result.Grafico.BottomAxis.Title.Caption:= 'Quantis Gama';
       Result.Grafico.Title.Text.Add('Gr�fico Quantil-Quantil Gama');
       end
    else
       raise Exception.Create('Erro na gera��o dos gr�ficos: C�digo ' + intToStr(Graf_Erro));
  finally
    vdata.Free;
    xData.Free;
    GP.Free;
  end;
end; { GammaPlot }

*)

